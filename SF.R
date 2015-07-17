# References: Predictability of the daily high and low of the S&P 500 index by Clive Jones
# Method is called EVPA for Extreme Value Prediction Algorithm
# Predicts the high and low price for the upcoming week for SPY
# Suggested to run time for this script is Monday - 9:50 AM EST 

library(griffun)
load.packages('forecast,quantmod,svDialogs,lmtest,TTR')

# Disable Sci Notation and supress warnings
options(scipen=999)
options(warn=-1)

tickers <- spl('SPY')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
quotes <- getQuote(tickers)
for(i in ls(data))
  if( last(index(data[[i]])) < as.Date(quotes[i, 'Trade Time']) ) {
    data[[i]] <- rbind( data[[i]], make.xts(quotes[i, spl('Open,High,Low,Last,Volume,Last')],
                                            as.Date(quotes[i, 'Trade Time'])))
  }
bt.prep(data)
summary(data$prices)

## BEGIN BACKTESTING AND FORECAST--->

## Forecast will include last n days plus the current day
SPY <- data$SPY
SPY <- to.weekly(data$SPY)
indx <- round(nrow(SPY)*0.8) # grab the last n% of the dataset
bt <- 20 # how many periods are OOS?

## Create independent variables
HiLag1 <- mlag(Hi(SPY),1)
HiLag2 <- mlag(Hi(SPY),2)
LoLag1 <- mlag(Lo(SPY),1)
LoLag2 <- mlag(Lo(SPY),2)

SPY$GrowthH <- (-1+(Hi(SPY)/HiLag1))
SPY$GrowthL <- (-1+(Lo(SPY)/LoLag1))
SPY$OPH <- (-1+Op(SPY)/HiLag1)
SPY$OPL <- (-1+Op(SPY)/LoLag1)
SPY$OPPH <- (-1+(Op(SPY)/HiLag2))
SPY$OPPL <- (-1+(Op(SPY)/LoLag2))
SPY[is.na(SPY)] <- 0

SPY <- last(SPY,indx)
bt.SPY <- SPY[1:(NROW(SPY)-bt)]

bt.Hi.SPY <- (bt.SPY[,c(7,9:12)])
bt.Lo.SPY <- (bt.SPY[,c(8:12)])

bt.high.reg.data <- data.frame(bt.Hi.SPY)
colnames(bt.high.reg.data) <- c("Y","X1","X2","X3","X4")
bt.low.reg.data <- data.frame(bt.Lo.SPY)
colnames(bt.low.reg.data) <-  c("Y","X1","X2","X3","X4")

## Calculate regression models for high and low
bt.high.fit <- lm(Y~. , data=bt.high.reg.data); summary(bt.high.fit)
bt.low.fit <- lm(Y~. , data=bt.low.reg.data); summary(bt.low.fit)

## Building forecast model
bt.high.fcast.data <- tail(SPY[,9:12],bt)
colnames(bt.high.fcast.data) <- c("X1","X2","X3","X4")

bt.low.fcast.data <- tail(SPY[,9:12],bt)
colnames(bt.low.fcast.data) <- c("X1","X2","X3","X4")

bt.high.fcast <- forecast.lm(bt.high.fit, newdata=bt.high.fcast.data)
bt.low.fcast <- forecast.lm(bt.low.fit, newdata=bt.low.fcast.data)

# Forecast Results
bt.high.fcast <- as.xts(cbind(data.frame(tail(Hi(SPY),bt)),data.frame(tail(HiLag1,bt)),data.frame(bt.high.fcast$mean)))
colnames(bt.high.fcast) <- c("Hi","PH","HighGrowth")
bt.high.fcast$Forecast.High <- (1+bt.high.fcast$HighGrowth) * bt.high.fcast$PH
bt.high.fcast
bt.high.acc <- round(acc(bt.high.fcast$Hi,bt.high.fcast$Forecast.High),3) * 100

bt.low.fcast <- as.xts(cbind(data.frame(tail(Lo(SPY),bt)),data.frame(tail(LoLag1,bt)),data.frame(bt.low.fcast$mean)))
colnames(bt.low.fcast) <- c("Lo","PL","LowGrowth")
bt.low.fcast$Forecast.low <- (1+bt.low.fcast$LowGrowth) * bt.low.fcast$PL
bt.low.fcast
bt.low.acc <- round(acc(bt.low.fcast$Lo,bt.low.fcast$Forecast.low),3) * 100

current.price <- round(last(SPY[,1]),2)
forecast.high <- round(last(bt.high.fcast[,4]),2)
forecast.low <- round(last(bt.low.fcast[,4]),2)

# Send to Pushbullet
library(httr)
setwd("/Users/wgriffith2/Dropbox/R/API")
wd <- getwd()

# NEVER put credentials or api keys in script bodies!!
# Get API key from file location
wag.api.key<-readLines(paste0(wd,"/wag.rpushbullet.json"))
jdr.api.key<-readLines(paste0(wd,"/jdr.rpushbullet.json"))

push.text <- paste0("Currently: $",current.price,"    Forecast: $",
                    forecast.high," - $",forecast.low,"    Accuracy (20w): ",
                    bt.high.acc, "% - ", bt.low.acc, "%")

# Send note through Pushbullet
api <- "https://api.pushbullet.com/api/pushes"
note <- list(type='note', title='SPY Weekly Forecast', body=push.text)
invisible(POST(api, authenticate(wag.api.key, ""), body=note))
invisible(POST(api, authenticate(jdr.api.key, ""), body=note))

## <--- END
