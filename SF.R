# References: Predictability of the daily high and low of the S&P 500 index by Clive Jones
# Method is called EVPA for Extreme Value Prediction Algorithm
# Predicts the high and low price for the upcoming week for SPY
# Suggested to run time for this script is Monday - 9:50 AM EST 

# Load libraries from github
library(devtools)
devtools::install_github('drewgriffith15/griffun')
library(griffun)
load.packages('forecast,quantmod,TTR')

# NEVER put credentials or api keys in script bodies!!
# Get API key from file location and set it
setwd("/Users/wgriffith2/Dropbox/R/API")
wd <- getwd()
api.key <- readLines(paste0(wd, "/alphavantage.json"))

# My list
symbols <- c("SPY") #"SPXS", "SPXL",

getSymbols(
  symbols,
  src = 'av',
  adjusted = TRUE,
  output.size = 'full',
  api.key = api.key
)
prices.import <- list()
for (i in 1:length(symbols)) {
  prices.import[[i]] <- Cl(get(symbols[i]))
}
prices <- do.call(cbind, prices.import)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
# tail(prices)

rm(list = setdiff(ls(),
                  c("SPY", "prices")))

## BEGIN BACKTESTING AND FORECAST--->

## Forecast will include last n days plus the current day
# SPY <- Cl(SPY)
weekly <- to.weekly(SPY)
weekly <- weekly[-nrow(weekly),]
indx <- round(nrow(weekly)*0.8) # grab the last n% of the dataset
bt <- 20 # how many periods are OOS?

## Create independent variables
HiLag1 <- mlag(Hi(weekly),1)
HiLag2 <- mlag(Hi(weekly),2)
LoLag1 <- mlag(Lo(weekly),1)
LoLag2 <- mlag(Lo(weekly),2)

weekly$GrowthH <- (-1+(Hi(weekly)/HiLag1))
weekly$GrowthL <- (-1+(Lo(weekly)/LoLag1))
weekly$OPH <- (-1+Op(weekly)/HiLag1)
weekly$OPL <- (-1+Op(weekly)/LoLag1)
weekly$OPPH <- (-1+(Op(weekly)/HiLag2))
weekly$OPPL <- (-1+(Op(weekly)/LoLag2))
weekly[is.na(weekly)] <- 0

weekly <- last(weekly,indx)
bt.weekly <- weekly[1:(NROW(weekly)-bt)]

bt.Hi.weekly <- (bt.weekly[,c(7,9:12)])
bt.Lo.weekly <- (bt.weekly[,c(8:12)])

bt.high.reg.data <- data.frame(bt.Hi.weekly)
colnames(bt.high.reg.data) <- c("Y","X1","X2","X3","X4")
bt.low.reg.data <- data.frame(bt.Lo.weekly)
colnames(bt.low.reg.data) <-  c("Y","X1","X2","X3","X4")

## Calculate regression models for high and low
bt.high.fit <- lm(Y~. , data=bt.high.reg.data); summary(bt.high.fit)
bt.low.fit <- lm(Y~. , data=bt.low.reg.data); summary(bt.low.fit)

## Building forecast model
bt.high.fcast.data <- tail(weekly[,9:12],bt)
colnames(bt.high.fcast.data) <- c("X1","X2","X3","X4")

bt.low.fcast.data <- tail(weekly[,9:12],bt)
colnames(bt.low.fcast.data) <- c("X1","X2","X3","X4")

bt.high.fcast <- predict(bt.high.fit, newdata=bt.high.fcast.data)
bt.low.fcast <- predict(bt.low.fit, newdata=bt.low.fcast.data)

# Forecast Results
bt.high.fcast <- as.xts(cbind(data.frame(tail(Hi(weekly),bt)),data.frame(tail(HiLag1,bt)),data.frame(bt.high.fcast)))
colnames(bt.high.fcast) <- c("Hi","PH","HighGrowth")
bt.high.fcast$Forecast.High <- (1+bt.high.fcast$HighGrowth) * bt.high.fcast$PH
bt.high.fcast
bt.high.acc <- round(acc(bt.high.fcast$Hi,bt.high.fcast$Forecast.High),3) * 100

bt.low.fcast <- as.xts(cbind(data.frame(tail(Lo(weekly),bt)),data.frame(tail(LoLag1,bt)),data.frame(bt.low.fcast)))
colnames(bt.low.fcast) <- c("Lo","PL","LowGrowth")
bt.low.fcast$Forecast.low <- (1+bt.low.fcast$LowGrowth) * bt.low.fcast$PL
bt.low.fcast
bt.low.acc <- round(acc(bt.low.fcast$Lo,bt.low.fcast$Forecast.low),3) * 100

current.price <- round(last(weekly[,1]),2)
forecast.high <- round(last(bt.high.fcast[,4]),2)
forecast.low <- round(last(bt.low.fcast[,4]),2)
movement <- iif(forecast.high-current.price > current.price-forecast.low, "(higher)","(lower)")

# Send to Pushbullet
library(httr)
setwd("/Users/wgriffith2/Dropbox/R/API")
wd <- getwd()

# NEVER put credentials or api keys in script bodies!!
# Get API key from file location
wag.api.key<-readLines(paste0(wd,"/wag.rpushbullet.json"))
# jdr.api.key<-readLines(paste0(wd,"/jdr.rpushbullet.json"))

push.text <- paste0("Currently: $",current.price,"    Forecast: $",
                    forecast.high," - $",forecast.low,"    Accuracy (", bt,"w): ",
                    bt.high.acc, "% - ", bt.low.acc, "%"); push.text

# Send note through Pushbullet
api <- "https://api.pushbullet.com/api/pushes"
note <- list(type='note', title='SPY Weekly Forecast', body=push.text)
invisible(POST(api, authenticate(wag.api.key, ""), body=note))
# invisible(POST(api, authenticate(jdr.api.key, ""), body=note))

## <--- END
