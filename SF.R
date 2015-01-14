###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

library(griffun)
load.packages('forecast,quantmod,svDialogs,lmtest,TTR')

## INPUT BOX
sym <- dlgInput("Enter Symbol: ", default="SPY")$res
if (!length(sym)) { # The user clicked the cancel button
  cat("OK, if you'd rather not, that is fine too...\n")
} else {
  cat("Gathing data for forecast on ", toupper(spl(sym)), "\n")
}

## ONLY SUPPORTS ONE STOCK CURRENTLY
tickers = toupper(spl(sym))
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1950-01-01', env = data, auto.assign = T)
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

## Find matches in historical data
bt.ves.cd = find.matches(bt.adj.close,n.hist,n.fore,model="ves", use.cd=TRUE)

## Calculate regression model
bt.ves.cd.fit = lm(Y~. , data=bt.ves.cd$rmodel)

## Building forecast model
bt.ves.cd.fcast = forecast.lm(bt.ves.cd.fit, newdata=bt.ves.cd$fmodel)
bt.ves.cd.fcast = bt.ves.cd.fcast$mean
bt.ves.cd.forecast = extendForecast(bt.dates, round(bt.ves.cd.fcast,4))
colnames(bt.ves.cd.forecast) = "bt.ves.cd.FORECAST"

## Combine forecast models
bt.ag.forecast = extendForecast(bt.dates,bt.ves.cd.forecast)
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
bt.model.acc = acc(bt.out$FORECAST, bt.out$HIST) ## Overall Accuracy
bt.model.cor = cor(bt.out$FORECAST,bt.out$HIST) ## Correlation
bt.model.acc;bt.model.cor

## <--- END BACK TESTING

## BEGIN FORECAST MODEL --->

## Find matches to create regression model
fm.ves.cd = find.matches(adj.close,n.hist,n.fore,model="ves", use.cd=TRUE)

## Calculate regression model
fm.ves.cd.fit = lm(Y~. , data=fm.ves.cd$rmodel)

## Building forecast model
fm.ves.cd.fcast = forecast.lm(fm.ves.cd.fit, newdata=fm.ves.cd$fmodel)
fm.ves.cd.fcast = fm.ves.cd.fcast$mean
fm.ves.cd.forecast = extendForecast(fm.dates, round(fm.ves.cd.fcast,4))
colnames(fm.ves.cd.forecast) = "fm.ves.cd.FORECAST"

## Plot matches
n.match = NROW(fm.ves.cd$matchindx)
max.index = fm.ves.cd$matchindx
d.matches = index(fm.dates)[1:NROW(fm.dates)]
plota(adj.close, type='l', col='gray', main=tickers)
plota.lines(last(adj.close,n.hist), col='blue')
for(i in 1:n.match) {
  plota.lines(adj.close[max.index[i]:(max.index[i]+n.hist)], col='red')
}

## Putting it all together
y = as.xts(last(adj.close,180),
           index(fm.dates)[(NROW(fm.dates)-(n.hist-1)):NROW(fm.dates)])
z = extendForecast(fm.dates,fm.ves.cd.forecast)
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
# mark$Buy.Sell = 0

## Future Prices
future = tail(fm.ag.forecast, n.fore)
future = rbind(mark, future)
future$Buy.Sell = buy.sell(future$Adj.Close)$Buy.Sell

## Model specs
profit = sum(buy.sell(future$Adj.Close)$Buy.Sell*(-future$Adj.Close))
model.acc = acc(fm.ves.cd$rmodel$Y,fm.ves.cd.fit$fitted.values) ## Model Accuracy
ur2 = unadj.rsquared(fm.ves.cd.fit)$unadj.rsquared
yr.return = last(yearlyReturn(adj.close),1)

## <--- END FORECAST MODEL

## OUTPUT --->

report = list(bt.out, bt.profit, bt.model.acc, bt.model.cor, future, profit, yr.return)
names(report)[1] = "Back.Tested.Forecast"
names(report)[2] = "Back.Tested.Profit"
names(report)[3] = "Back.Tested.Model.acc"
names(report)[4] = "Back.Tested.Model.cor"
names(report)[5] = "Forecast"
names(report)[6] = "Est.Profit"
names(report)[7] = "Yearly.Return"
report

## <--- OUTPUT

##########################################################################