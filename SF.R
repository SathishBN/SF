## LOAD PACKAGES
# install_github("griffun", "drewgriffith15")
require(griffun)
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
getSymbols(tickers, src = 'yahoo', from = '2000-01-01', env = data, auto.assign = T)
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
rsi.values = RSI(adj.close)
n.hist=35; n.fore=15

## BEGIN BACK TESTING --->

bt.adj.close = adj.close[1:(NROW(adj.close)-n.fore)]
bt.dates = data$dates[1:(NROW(adj.close)-n.fore)]
bt.rsi.values = RSI(bt.adj.close)

## Find matches to create regression models
bt.raw = find.matches(bt.adj.close,n.hist,n.fore,n.match=floor(n.hist*.25), model="linear", use.cd=FALSE)
bt.rsi = find.matches(bt.rsi.values,n.hist,n.fore,n.match=floor(n.hist*.15),model="linear", use.cd=FALSE)

## Adding RSI to model
bt.n.match = NROW(bt.rsi$matchindx)
bt.match.index = bt.rsi$matchindx
bt.d.matches = index(bt.dates)[1:NROW(bt.dates)]

# RSI regression model
bt.rsi.rmod = matrix(NA, nr=(n.hist), nc=(bt.n.match))
for(i in 1:bt.n.match) {
  bt.rsi.rmod[,i] = bt.adj.close[bt.match.index[i]:(bt.match.index[i]+(n.hist-1))]
}
# Rename columns
bt.rsi.rmod = data.frame(bt.rsi.rmod)
names(bt.rsi.rmod) = gsub("X", "R", names(bt.rsi.rmod))

# Combine real values with RSI
bt.reg.model = cbind(bt.raw$rmodel,bt.rsi.rmod)

## Calculate regression models
bt.reg.fit = lm(Y~. , data=bt.reg.model)

## Building forecast models

# RSI forecast model
bt.rsi.fmod = matrix(NA, nr=(n.fore), nc=(bt.n.match))
for(i in 1:bt.n.match) {
  bt.rsi.fmod[,i] = bt.adj.close[(bt.match.index[i]+n.hist):(bt.match.index[i]+(n.hist+n.fore)-1)]
}
# Rename columns
bt.rsi.fmod = data.frame(bt.rsi.fmod)
names(bt.rsi.fmod) = gsub("X", "R", names(bt.rsi.fmod))

# Combine real values with RSI
bt.fcast.model = cbind(bt.raw$fmodel,bt.rsi.fmod)

bt.fcast = forecast.lm(bt.reg.fit, newdata=bt.fcast.model)
bt.fcast = bt.fcast$mean
bt.forecast = extendForecast(bt.dates, round(bt.fcast,4))
colnames(bt.forecast) = "FORECAST"

## What really happened...
hist.adj.close = adj.close[(NROW(adj.close)-(n.fore-1)):NROW(adj.close)]
colnames(hist.adj.close) = "HISTORY"

## Quick comparison
thm = chart_theme()
thm$col$line.col = 'gray'
chart_Series(last(bt.forecast,(n.fore+1)), theme=thm,name=tickers)
add_Series(hist.adj.close,on=1)
## GRAY - FORECAST; RED - HISTORICAL

## Model specs
bt.profit = sum(buy.sell(bt.forecast)$Buy.Sell*(-hist.adj.close))
bt.out = cbind(hist.adj.close, bt.forecast, buy.sell(bt.forecast)$Buy.Sell)
names(bt.out)[3] = "Buy.Sell"
bt.model.reg = summary(bt.reg.fit) ## Reg model summary
bt.model.acc = acc(bt.out$FORECAST, bt.out$HIST) ## Overall Accuracy
bt.model.cor = cor(bt.out$FORECAST,bt.out$HIST) ## Correlation

## <--- END BACK TESTING

## BEGIN FORECAST MODEL --->

## Find matches to create regression models
fm.raw = find.matches(adj.close,n.hist,n.fore,n.match=floor(n.hist*.25), model="linear", use.cd=FALSE)
fm.rsi = find.matches(rsi.values,n.hist,n.fore,n.match=floor(n.hist*.15),model="linear", use.cd=FALSE)

## Adding RSI to model
fm.n.match = NROW(fm.rsi$matchindx)
fm.match.index = fm.rsi$matchindx
fm.d.matches = index(fm.dates)[1:NROW(fm.dates)]

# RSI regression model
fm.rsi.rmod = matrix(NA, nr=(n.hist), nc=(fm.n.match))
for(i in 1:fm.n.match) {
  fm.rsi.rmod[,i] = adj.close[fm.match.index[i]:(fm.match.index[i]+(n.hist-1))]
}
# Rename columns
fm.rsi.rmod = data.frame(fm.rsi.rmod)
names(fm.rsi.rmod) = gsub("X", "R", names(fm.rsi.rmod))

# Combine real values with RSI
fm.reg.model = cbind(fm.raw$rmodel,fm.rsi.rmod)

## Calculate regression models
fm.reg.fit = lm(Y~. , data=fm.reg.model)

## Building forecast models

# RSI forecast model
fm.rsi.fmod = matrix(NA, nr=(n.fore), nc=(fm.n.match))
for(i in 1:fm.n.match) {
  fm.rsi.fmod[,i] = adj.close[(fm.match.index[i]+n.hist):(fm.match.index[i]+(n.hist+n.fore)-1)]
}
# Rename columns
fm.rsi.fmod = data.frame(fm.rsi.fmod)
names(fm.rsi.fmod) = gsub("X", "R", names(fm.rsi.fmod))

# Combine real values with RSI
fm.fcast.model = cbind(fm.raw$fmodel,fm.rsi.fmod)

fm.fcast = forecast.lm(fm.reg.fit, newdata=fm.fcast.model)
fm.fcast = fm.fcast$mean
fm.forecast = extendForecast(fm.dates, round(fm.fcast,4))
colnames(fm.forecast) = "FORECAST"

## Plot matches
n.match = NROW(fm.raw$matchindx)
match.index = fm.raw$matchindx
d.matches = index(fm.dates)[1:NROW(fm.dates)]
plota(adj.close, type='l', col='gray', main=tickers)
plota.lines(last(adj.close,n.hist), col='blue')
for(i in 1:n.match) {
  plota.lines(adj.close[match.index[i]:(match.index[i]+n.hist)], col='red')
}

## Putting it all together
y = as.xts(last(adj.close,180),
           index(fm.dates)[(NROW(fm.dates)-(n.hist-1)):NROW(fm.dates)])
z = extendForecast(fm.dates,fm.forecast)
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
model.acc = acc(fm.raw$rmodel$Y,fm.reg.fit$fitted.values) ## Model Accuracy
ur2 = unadj.rsquared(fm.reg.fit)$unadj.rsquared

## <--- END FORECAST MODEL

## OUTPUT --->

report = list(bt.out, bt.profit, bt.model.acc, bt.model.cor, future, profit)
names(report)[1] = "Back.Tested.Forecast"
names(report)[2] = "Back.Tested.Profit"
names(report)[3] = "Back.Tested.Model.acc"
names(report)[4] = "Back.Tested.Model.cor"
names(report)[5] = "Forecast"
names(report)[6] = "Est.Profit"
report

## <--- OUTPUT

##########################################################################