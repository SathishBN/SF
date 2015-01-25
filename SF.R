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

tickers = spl('SPY,^VIX')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in data$symbolnames) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='remove.na', fill.gaps = T)

# Closing VIX prices
VIX = Cl(data$VIX)
bt.prep.remove.symbols(data, 'VIX')

# Closing SPY prices
SPY = Cl(data$SPY)

prices = SPY
fm.dates = data$dates
n.hist=35; n.fore=15

## BEGIN BACK TESTING --->

bt.adj.close = SPY[1:(NROW(SPY)-n.fore)]
bt.dates = data$dates[1:(NROW(SPY)-n.fore)]

## Find matches in historical data
bt.ves.cd = find.matches(bt.adj.close,n.hist,n.fore,model="ves", use.cd=TRUE,n.match=round(n.hist*.3)-4)
# cor(bt.ves.cd$rmodel$Y,bt.ves.cd$rmodel$X1)

# Using top matches, get VIX data and add it to the regression model
RVX1 = data.frame(VIX[bt.ves.cd$matchindx[1]:(bt.ves.cd$matchindx[1]+n.hist-1)]); colnames(RVX1) = "VX1"
lnVX1 = log(RVX1); colnames(lnVX1) = "lnVX1"

RVX2 = data.frame(VIX[bt.ves.cd$matchindx[2]:(bt.ves.cd$matchindx[2]+n.hist-1)]); colnames(RVX2) = "VX2"
lnVX2 = log(RVX2); colnames(lnVX2) = "lnVX2"

bt.reg.data = cbind(bt.ves.cd$rmodel,RVX1,RVX2,lnVX1,lnVX2)

## Calculate regression model
bt.ves.cd.fit = lm(Y~. , data=bt.reg.data)
summary(bt.ves.cd.fit)

## Building forecast model
FVX1 = data.frame(VIX[bt.ves.cd$matchindx[1]:(bt.ves.cd$matchindx[1]+n.fore-1)]); colnames(FVX1) = "VX1"
lnFVX1 = log(FVX1); colnames(lnFVX1) = "lnVX1"

FVX2 = data.frame(VIX[bt.ves.cd$matchindx[2]:(bt.ves.cd$matchindx[2]+n.fore-1)]); colnames(FVX2) = "VX2"
lnFVX2 = log(FVX2); colnames(lnFVX2) = "lnVX2"

bt.fcast.data = cbind(bt.ves.cd$fmodel,FVX1,FVX2,lnFVX1,lnFVX2)

bt.ves.cd.fcast = forecast.lm(bt.ves.cd.fit, newdata=bt.fcast.data)
bt.ves.cd.fcast = bt.ves.cd.fcast$mean
bt.ves.cd.forecast = extendForecast(bt.dates, round(bt.ves.cd.fcast,4))
colnames(bt.ves.cd.forecast) = "FORECAST"

## Combine forecast models
bt.ag.forecast = extendForecast(bt.dates,bt.ves.cd.forecast)
colnames(bt.ag.forecast) = "FORECAST"

## What really happened...
hist.adj.close = prices[(NROW(prices)-(n.fore-1)):NROW(prices)]
colnames(hist.adj.close) = "HISTORY"

## Quick comparison
thm = chart_theme()
thm$col$line.col = 'gray'
chart_Series(last(bt.ag.forecast,(n.fore+1)), theme=thm,name="SPY")
add_Series(hist.adj.close,on=1)
## GRAY - FORECAST; RED - HISTORICAL

## Model specs
bt.profit = sum(buy.sell(bt.ag.forecast)$Buy.Sell*(-hist.adj.close))
bt.out = cbind(hist.adj.close, bt.ag.forecast, buy.sell(bt.ag.forecast)$Buy.Sell)
names(bt.out)[3] = "Buy.Sell"
bt.model.acc = acc(bt.out$FORECAST, bt.out$HIST) ## Overall Accuracy
bt.model.cor = cor(bt.out$FORECAST,bt.out$HIST) ## Correlation
bt.model.acc;bt.model.cor

## BEGIN FORECAST MODEL --->

## Find matches to create regression model
fm.ves.cd = find.matches(prices,n.hist,n.fore,model="ves", use.cd=TRUE, n.match=round(n.hist*.3)-4)

# Using top matches, get VIX data and add it to the regression model
RVX1 = data.frame(VIX[fm.ves.cd$matchindx[1]:(fm.ves.cd$matchindx[1]+n.hist-1)]); colnames(RVX1) = "VX1"
lnVX1 = log(RVX1); colnames(lnVX1) = "lnVX1"

RVX2 = data.frame(VIX[fm.ves.cd$matchindx[2]:(fm.ves.cd$matchindx[2]+n.hist-1)]); colnames(RVX2) = "VX2"
lnVX2 = log(RVX2); colnames(lnVX2) = "lnVX2"

fm.reg.data = cbind(fm.ves.cd$rmodel,RVX1,RVX2,lnVX1,lnVX2)

## Calculate regression model
fm.ves.cd.fit = lm(Y~. , fm.reg.data)
summary(fm.ves.cd.fit)

## Building forecast model
FVX1 = data.frame(VIX[fm.ves.cd$matchindx[1]:(fm.ves.cd$matchindx[1]+n.fore-1)]); colnames(FVX1) = "VX1"
lnFVX1 = log(FVX1); colnames(lnFVX1) = "lnVX1"

FVX2 = data.frame(VIX[fm.ves.cd$matchindx[2]:(fm.ves.cd$matchindx[2]+n.fore-1)]); colnames(FVX2) = "VX2"
lnFVX2 = log(FVX2); colnames(lnFVX2) = "lnVX2"

fm.fcast.data = cbind(fm.ves.cd$fmodel,FVX1,FVX2,lnFVX1,lnFVX2)

fm.ves.cd.fcast = forecast.lm(fm.ves.cd.fit, newdata=fm.fcast.data )
fm.ves.cd.fcast = fm.ves.cd.fcast$mean
fm.ves.cd.forecast = extendForecast(fm.dates, round(fm.ves.cd.fcast,4))
colnames(fm.ves.cd.forecast) = "FORECAST"

## Plot matches
n.match = NROW(fm.ves.cd$matchindx)
max.index = fm.ves.cd$matchindx
d.matches = index(fm.dates)[1:NROW(fm.dates)]
plota(prices, type='l', col='gray', main="SPY")
plota.lines(last(prices,n.hist), col='blue')
for(i in 1:n.match) {
  plota.lines(prices[max.index[i]:(max.index[i]+n.hist)], col='red')
}

## Putting it all together
y = as.xts(last(prices,180),
           index(fm.dates)[(NROW(fm.dates)-(n.hist-1)):NROW(fm.dates)])
z = extendForecast(fm.dates,fm.ves.cd.forecast)
fm.ag.forecast = rbind(y,z)
colnames(fm.ag.forecast) = "Close"

## Quick technical analysis
thm = chart_theme()
thm$col$line.col = 'blue'
chart_Series(fm.ag.forecast, theme=thm,name="SPY")
add_Series(last(fm.ag.forecast,(n.fore+1)),on=1)
add_RSI(n=14)
add_BBands()

## Future Prices
future = tail(fm.ag.forecast, n.fore)
future$Buy.Sell = buy.sell(future$Close)$Buy.Sell

## Model specs
profit = sum(buy.sell(future$Close)$Buy.Sell*(-future$Close))
model.acc = acc(fm.ves.cd$rmodel$Y,fm.ves.cd.fit$fitted.values) ## Model Accuracy
ur2 = unadj.rsquared(fm.ves.cd.fit)$unadj.rsquared
yr.return = last(yearlyReturn(prices),1)

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