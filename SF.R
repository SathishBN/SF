## LOAD PACKAGES
# install_github("griffun", "drewgriffith15")
require(griffun)
load.packages('forecast,quantmod,svDialogs,lmtest,TTR')

## INPUT BOX
sym <- dlgInput("Enter Symbol: ")$res
if (!length(sym)) { # The user clicked the cancel button
  cat("OK, if you'd rather not, that is fine too...\n")
} else {
  cat("Gathing data for forecast on ", toupper(spl(sym)), "\n")
}

## ONLY SUPPORTS ONE STOCK CURRENTLY
tickers = toupper(spl(sym))
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
quotes = getQuote(tickers)
for(i in ls(data))
  if( last(index(data[[i]])) < as.Date(quotes[i, 'Trade Time']) ) {
    data[[i]] = rbind( data[[i]], make.xts(quotes[i, spl('Open,High,Low,Last,Volume,Last')],
                                           as.Date(quotes[i, 'Trade Time'])))
  }
bt.prep(data)
summary(data$prices)

adj.close = data$prices 
fm.dates = data$dates
n.hist=35; n.fore=20

## BEGIN BACK TESTING --->

bt.adj.close = adj.close[1:(NROW(adj.close)-n.fore)]
bt.dates = data$dates[1:(NROW(adj.close)-n.fore)]

## Find matches to create regression models
bt.ves = find.matches(bt.adj.close,n.hist,n.fore,model="ves", use.cd=FALSE)
bt.ces = find.matches(bt.adj.close,n.hist,n.fore,model="ces", use.cd=FALSE)
bt.lin = find.matches(bt.adj.close,n.hist,n.fore,model="linear", use.cd=FALSE)

bt.ves.cd = find.matches(bt.adj.close,n.hist,n.fore,model="ves", use.cd=TRUE)
bt.ces.cd = find.matches(bt.adj.close,n.hist,n.fore,model="ces", use.cd=TRUE)
bt.lin.cd = find.matches(bt.adj.close,n.hist,n.fore,model="linear", use.cd=TRUE)

## Calculate regression models
bt.ves.fit = lm(Y~. , data=bt.ves$rmodel)
bt.ces.fit = lm(Y~. , data=bt.ces$rmodel)
bt.lin.fit = lm(Y~. , data=bt.lin$rmodel)

bt.ves.cd.fit = lm(Y~. , data=bt.ves.cd$rmodel)
bt.ces.cd.fit = lm(Y~. , data=bt.ces.cd$rmodel)
bt.lin.cd.fit = lm(Y~. , data=bt.lin.cd$rmodel)

## Building forecast models
bt.ves.fcast = forecast.lm(bt.ves.fit, newdata=bt.ves$fmodel)
bt.ves.fcast = bt.ves.fcast$mean
bt.ves.forecast = extendForecast(bt.dates, round(bt.ves.fcast,4))
colnames(bt.ves.forecast) = "bt.ves.FORECAST"

bt.ces.fcast = forecast.lm(bt.ces.fit, newdata=bt.ces$fmodel)
bt.ces.fcast = bt.ces.fcast$mean
bt.ces.forecast = extendForecast(bt.dates, round(bt.ces.fcast,4))
bt.ces.forecast = exp(bt.ces.forecast[,1]) ## converting from log to exp
colnames(bt.ces.forecast) = "bt.ces.FORECAST"

bt.lin.fcast = forecast.lm(bt.lin.fit, newdata=bt.lin$fmodel)
bt.lin.fcast = bt.lin.fcast$mean
bt.lin.forecast = extendForecast(bt.dates, round(bt.lin.fcast,4))
colnames(bt.lin.forecast) = "bt.lin.FORECAST"

bt.ves.cd.fcast = forecast.lm(bt.ves.cd.fit, newdata=bt.ves.cd$fmodel)
bt.ves.cd.fcast = bt.ves.cd.fcast$mean
bt.ves.cd.forecast = extendForecast(bt.dates, round(bt.ves.cd.fcast,4))
colnames(bt.ves.cd.forecast) = "bt.ves.cd.FORECAST"

bt.ces.cd.fcast = forecast.lm(bt.ces.cd.fit, newdata=bt.ces.cd$fmodel)
bt.ces.cd.fcast = bt.ces.cd.fcast$mean
bt.ces.cd.forecast = extendForecast(bt.dates, round(bt.ces.cd.fcast,4))
bt.ces.cd.forecast = exp(bt.ces.cd.forecast[,1]) ## converting from log to exp
colnames(bt.ces.cd.forecast) = "bt.ces.cd.FORECAST"

bt.lin.cd.fcast = forecast.lm(bt.lin.cd.fit, newdata=bt.lin.cd$fmodel)
bt.lin.cd.fcast = bt.lin.cd.fcast$mean
bt.lin.cd.forecast = extendForecast(bt.dates, round(bt.lin.cd.fcast,4))
colnames(bt.lin.cd.forecast) = "bt.lin.cd.FORECAST"

bt.ag.forecast = extendForecast(bt.dates,rowMeans(cbind(bt.ves.forecast,bt.ces.forecast,bt.lin.forecast,bt.ves.cd.forecast,bt.ces.cd.forecast,bt.lin.cd.forecast)))
colnames(bt.ag.forecast) = "FORECAST"

## What really happened...
hist.adj.close = adj.close[(NROW(adj.close)-(n.fore-1)):NROW(adj.close)]
colnames(hist.adj.close) = "HISTORY"

## Quick comparison
thm = chart_theme()
thm$col$line.col = 'gray'
chart_Series(last(bt.ag.forecast,(n.fore+1)), theme=thm,name=tickers)
add_Series(hist.adj.close,on=1)
## GRAY - FORECAST; RED - HISTORICAL

## Model specs
bt.profit = sum(buy.sell(bt.ag.forecast)$Buy.Sell*(-hist.adj.close))
bt.out = cbind(hist.adj.close, bt.ag.forecast, buy.sell(bt.ag.forecast)$Buy.Sell)
names(bt.out)[3] = "Buy.Sell"
bt.model.acc = acc(bt.out$FORECAST, bt.out$HIST) ## Real Accuracy

## <--- END BACK TESTING

## BEGIN FORECAST MODEL --->

## Find matches to create regression models
fm.ves = find.matches(adj.close,n.hist,n.fore,model="ves", use.cd=FALSE)
fm.ces = find.matches(adj.close,n.hist,n.fore,model="ces", use.cd=FALSE)
fm.lin = find.matches(adj.close,n.hist,n.fore,model="linear", use.cd=FALSE)

fm.ves.cd = find.matches(adj.close,n.hist,n.fore,model="ves", use.cd=TRUE)
fm.ces.cd = find.matches(adj.close,n.hist,n.fore,model="ces", use.cd=TRUE)
fm.lin.cd = find.matches(adj.close,n.hist,n.fore,model="linear", use.cd=TRUE)

## Calculate regression models
fm.ves.fit = lm(Y~. , data=fm.ves$rmodel)
fm.ces.fit = lm(Y~. , data=fm.ces$rmodel)
fm.lin.fit = lm(Y~. , data=fm.lin$rmodel)

fm.ves.cd.fit = lm(Y~. , data=fm.ves.cd$rmodel)
fm.ces.cd.fit = lm(Y~. , data=fm.ces.cd$rmodel)
fm.lin.cd.fit = lm(Y~. , data=fm.lin.cd$rmodel)

# dwtest(fm.lin.cd.fit, alt="two.sided")

## Building forecast models
fm.ves.fcast = forecast.lm(fm.ves.fit, newdata=fm.ves$fmodel)
fm.ves.fcast = fm.ves.fcast$mean
fm.ves.forecast = extendForecast(fm.dates, round(fm.ves.fcast,4))
colnames(fm.ves.forecast) = "fm.ves.FORECAST"

fm.ces.fcast = forecast.lm(fm.ces.fit, newdata=fm.ces$fmodel)
fm.ces.fcast = fm.ces.fcast$mean
fm.ces.forecast = extendForecast(fm.dates, round(fm.ces.fcast,4))
fm.ces.forecast = exp(fm.ces.forecast[,1]) ## converting from log to exp
colnames(fm.ces.forecast) = "fm.ces.FORECAST"

fm.lin.fcast = forecast.lm(fm.lin.fit, newdata=fm.lin$fmodel)
fm.lin.fcast = fm.lin.fcast$mean
fm.lin.forecast = extendForecast(fm.dates, round(fm.lin.fcast,4))
colnames(fm.lin.forecast) = "fm.lin.FORECAST"

fm.ves.cd.fcast = forecast.lm(fm.ves.cd.fit, newdata=fm.ves.cd$fmodel)
fm.ves.cd.fcast = fm.ves.cd.fcast$mean
fm.ves.cd.forecast = extendForecast(fm.dates, round(fm.ves.cd.fcast,4))
colnames(fm.ves.cd.forecast) = "fm.ves.cd.FORECAST"

fm.ces.cd.fcast = forecast.lm(fm.ces.cd.fit, newdata=fm.ces.cd$fmodel)
fm.ces.cd.fcast = fm.ces.cd.fcast$mean
fm.ces.cd.forecast = extendForecast(fm.dates, round(fm.ces.cd.fcast,4))
fm.ces.cd.forecast = exp(fm.ces.cd.forecast[,1]) ## converting from log to exp
colnames(fm.ces.cd.forecast) = "fm.ces.cd.FORECAST"

fm.lin.cd.fcast = forecast.lm(fm.lin.cd.fit, newdata=fm.lin.cd$fmodel)
fm.lin.cd.fcast = fm.lin.cd.fcast$mean
fm.lin.cd.forecast = extendForecast(fm.dates, round(fm.lin.cd.fcast,4))
colnames(fm.lin.cd.forecast) = "fm.lin.cd.FORECAST"

## Plot matches
n.match = NROW(fm.lin$matchindx)
max.index = fm.lin$matchindx
d.matches = index(fm.dates)[1:NROW(fm.dates)]
plota(adj.close, type='l', col='gray', main=tickers)
plota.lines(last(adj.close,n.hist), col='blue')
for(i in 1:n.match) {
  plota.lines(adj.close[max.index[i]:(max.index[i]+n.hist)], col='red')
}

## Putting it all together
y = as.xts(last(adj.close,180),
           index(fm.dates)[(NROW(fm.dates)-(n.hist-1)):NROW(fm.dates)])
z = extendForecast(fm.dates,rowMeans(cbind(fm.ves.forecast,fm.ces.forecast,fm.lin.forecast,fm.ves.cd.forecast,fm.ces.cd.forecast,fm.lin.cd.forecast)))
fm.ag.forecast = rbind(y,z)
colnames(fm.ag.forecast) = "Adj.Close"

## Quick technical analysis
thm = chart_theme()
thm$col$line.col = 'blue'
chart_Series(fm.ag.forecast, theme=thm,name=tickers)
add_Series(last(fm.ag.forecast,(n.fore+1)),on=1)
add_RSI(n=14)
add_BBands()

## Market Price
mark = round(last(adj.close),2); colnames(mark) = "Adj.Close"
mark$Buy.Sell = 0

## Future Prices
future = tail(fm.ag.forecast, n.fore)
future$Buy.Sell = buy.sell(future$Adj.Close)$Buy.Sell
future = rbind(mark, future)

## Model specs
profit = sum(buy.sell(future$Adj.Close)$Buy.Sell*(-future$Adj.Close))
model.acc = acc(fm.lin$rmodel$Y,fm.lin.fit$fitted.values) ## Model Accuracy
ur2 = unadj.rsquared(fm.lin.fit)$unadj.rsquared

## <--- END FORECAST MODEL

## OUTPUT --->

bt.report = list(bt.out, bt.profit, bt.model.acc)
names(bt.report)[1] = "Back.Tested.Forecast"
names(bt.report)[2] = "Back.Tested.Profit"
names(bt.report)[3] = "Back.Tested.Model.acc"

forecast.report = list(future, profit, model.acc)
names(forecast.report)[1] = "Forecast"
names(forecast.report)[2] = "Profit"
names(forecast.report)[3] = "Model.acc"

bt.report; forecast.report

## <--- OUTPUT

##########################################################################