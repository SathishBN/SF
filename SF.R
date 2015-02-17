###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con <- gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

library(griffun)

load.packages('forecast,quantmod,svDialogs,lmtest,TTR')

# Disable Sci Notation and supress warnings
options(scipen=999)
options(warn=-1)

# ## INPUT BOX
# sym <- dlgInput("Enter Symbol: ", default="SPY")$res
# if (!length(sym)) { # The user clicked the cancel button
#   cat("OK, if you'd rather not, that is fine too...\n")
# } else {
#   cat("Gathing data for forecast on ", toupper(spl(sym)), "\n")
# }
# tickers <- toupper(spl(sym))

tickers <- spl('SPY')

## ONLY SUPPORTS ONE STOCK CURRENTLY

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


## BEGIN DAILY BACKTESTING AND FORECAST--->

n.hist <- 35; n.fore <- 5 ## <- daily
SPY <- data$SPY

# Get Calculate Open/High and Open/Lo for X variables
SPY$OpHi <-  -1 + (Op(SPY)/rollapply(mlag(Hi(SPY),1), n.hist, max))
SPY$OpLo <-  -1 + (Op(SPY)/rollapply(mlag(Lo(SPY),1), n.hist, max))
SPY[is.na(SPY)] <- 0

high <- Hi(SPY)
bt.high <- high[1:(NROW(high)-n.fore)]
bt.high <- last(bt.high,n.hist)
bt.high.xvar <- SPY$OpHi[1:(NROW(high)-n.fore)]
bt.high.xvar <- last(bt.high.xvar,n.hist)

low <- Lo(SPY)
bt.low <- low[1:(NROW(low)-n.fore)]
bt.low <- last(bt.low,n.hist)
bt.low.xvar <- SPY$OpLo[1:(NROW(low)-n.fore)]
bt.low.xvar <- last(bt.low.xvar,n.hist)

bt.high.reg.data <- data.frame(cbind(bt.high,bt.high.xvar)); colnames(bt.high.reg.data) <- c("Y","X1")
bt.low.reg.data <- data.frame(cbind(bt.low,bt.low.xvar)); colnames(bt.low.reg.data) <- c("Y","X1")

## Calculate regression models for high and low
bt.high.fit <- lm(Y~. , data=bt.high.reg.data)
bt.low.fit <- lm(Y~. , data=bt.low.reg.data)

## Building forecast model

bt.high.fcast.data <- last(SPY$OpHi,n.fore); colnames(bt.high.fcast.data) <- c("X1")
bt.low.fcast.data <- last(SPY$OpLo,n.fore); colnames(bt.low.fcast.data) <- c("X1")

bt.high.fcast <- forecast.lm(bt.high.fit, newdata=bt.high.fcast.data)
bt.low.fcast <- forecast.lm(bt.low.fit, newdata=bt.low.fcast.data)

bt.high.fcast <- as.xts(bt.high.fcast$mean); colnames(bt.high.fcast) <- "Forecast.High"
bt.low.fcast <- as.xts(bt.low.fcast$mean); colnames(bt.low.fcast) <- "Forecast.Low"

## Combine fits and forecast
daily.fitted.High <- rbind(round(as.xts(bt.high.fit$fitted.values),2),bt.high.fcast)
daily.fitted.Low <- rbind(round(as.xts(bt.low.fit$fitted.values),2),bt.low.fcast)

## Quick comparison of what really happened...
thm <- chart_theme()
# thm$col$line.col <- 'gray'
chart_Series(last(OHLC(SPY),n.hist+n.fore), theme=thm,name="SPY vs Hi/Lo Forecast")
add_Series(daily.fitted.High,on=1)
add_Series(daily.fitted.Low,on=1)

## Check Forecast Accuracy
bt.high.acc <- acc(last(Hi(SPY),n.fore), last(daily.fitted.High,n.fore))
bt.low.acc <- acc(last(Lo(SPY),n.fore), last(daily.fitted.Low,n.fore))

# Check Forecast Correlation
bt.high.cor <- cor(last(Hi(SPY),n.fore), last(daily.fitted.High,n.fore))
bt.low.cor <- cor(last(Lo(SPY),n.fore), last(daily.fitted.Low,n.fore))

daily.output <- cbind(last(Hi(SPY),n.fore),bt.high.fcast,last(Lo(SPY),n.fore),bt.low.fcast)
daily.bt.acc <- cbind(bt.high.acc,bt.low.acc)

## <--- END DAILY BACKTESTING AND FORECAST

## BEGIN WEEKLY BACKTESTING AND FORECAST--->

n.hist = 13; n.fore = 3 ## <- Weekly
SPY = to.weekly(data$SPY)

# SPY prices
SPY$OpHi <-  -1 + (Op(SPY)/rollapply(mlag(Hi(SPY),1), 2, max))
SPY$OpLo <-  -1 + (Op(SPY)/rollapply(mlag(Lo(SPY),1), 5, max))
SPY[is.na(SPY)] <- 0

high <- Hi(SPY)
bt.high <- high[1:(NROW(high)-n.fore)]
bt.high <- last(bt.high,n.hist)
bt.high.xvar <- SPY$OpHi[1:(NROW(high)-n.fore)]
bt.high.xvar <- last(bt.high.xvar,n.hist)

low <- Lo(SPY)
bt.low <- low[1:(NROW(low)-n.fore)]
bt.low <- last(bt.low,n.hist)
bt.low.xvar <- SPY$OpLo[1:(NROW(low)-n.fore)]
bt.low.xvar <- last(bt.low.xvar,n.hist)

bt.high.reg.data <- data.frame(cbind(bt.high,bt.high.xvar)); colnames(bt.high.reg.data) <- c("Y","X1")
bt.low.reg.data <- data.frame(cbind(bt.low,bt.low.xvar)); colnames(bt.low.reg.data) <- c("Y","X1")

## Calculate regression models for high and low
bt.high.fit <- lm(Y~. , data=bt.high.reg.data)
bt.low.fit <- lm(Y~. , data=bt.low.reg.data)

## Building forecast model

bt.high.fcast.data <- last(SPY$OpHi,n.fore); colnames(bt.high.fcast.data) <- c("X1")
bt.low.fcast.data <- last(SPY$OpLo,n.fore); colnames(bt.low.fcast.data) <- c("X1")

bt.high.fcast <- forecast.lm(bt.high.fit, newdata=bt.high.fcast.data)
bt.low.fcast <- forecast.lm(bt.low.fit, newdata=bt.low.fcast.data)

bt.high.fcast <- as.xts(bt.high.fcast$mean); colnames(bt.high.fcast) <- "Forecast.High"
bt.low.fcast <- as.xts(bt.low.fcast$mean); colnames(bt.low.fcast) <- "Forecast.Low"

## Combine fits and forecast
daily.fitted.High <- rbind(round(as.xts(bt.high.fit$fitted.values),2),bt.high.fcast)
daily.fitted.Low <- rbind(round(as.xts(bt.low.fit$fitted.values),2),bt.low.fcast)

## Quick comparison of what really happened...
# thm <- chart_theme()
# # thm$col$line.col <- 'gray'
# chart_Series(last(OHLC(SPY),n.hist+n.fore), theme=thm,name="SPY vs Hi/Lo Forecast")
# add_Series(daily.fitted.High,on=1)
# add_Series(daily.fitted.Low,on=1)

## Check Forecast Accuracy
bt.high.acc <- acc(last(Hi(SPY),n.fore), last(daily.fitted.High,n.fore))
bt.low.acc <- acc(last(Lo(SPY),n.fore), last(daily.fitted.Low,n.fore))

# Check Forecast Correlation
bt.high.cor <- cor(last(Hi(SPY),n.fore), last(daily.fitted.High,n.fore))
bt.low.cor <- cor(last(Lo(SPY),n.fore), last(daily.fitted.Low,n.fore))

# Weekly Output
weekly.output <- cbind(last(Hi(SPY),n.fore),bt.high.fcast,last(Lo(SPY),n.fore),bt.low.fcast)
weekly.bt.acc <- cbind(bt.high.acc,bt.low.acc)

## <--- END WEEKLY BACKTESTING AND FORECAST

weekly.output; weekly.bt.acc
daily.output; daily.bt.acc