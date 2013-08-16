# LOAD PACKAGES
install_github("griffun", "drewgriffith15")
require(griffun)
load.packages('forecast,quantmod,svDialogs,lmtest')

# INPUT BOX
sym <- dlgInput("Enter Symbol: ")$res
if (!length(sym)) { # The user clicked the cancel button
  cat("OK, if you'd rather not!\n")
} else {
  cat("Gathing data for forecast on ", toupper(spl(sym)), "\n")
}

# ONLY SUPPORTS ONE STOCK CURRENTLY
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
dates = data$dates
n.hist=35; n.fore=15

# BEGIN BACK TESTING --->

bt.adj.close = adj.close[1:(NROW(adj.close)-n.fore)]
bt.dates = data$dates[1:(NROW(adj.close)-n.fore)]

bt.fm = find.matches(bt.adj.close,n.hist,n.fore,model="ves", use.cd=TRUE)
bt.fit = lm( Y~. , data=bt.fm$rmodel ) #http://bit.ly/WCiGpw
# summary(bt.fit)
dwtest(bt.fit)

# Building forecast model
bt.newdf = bt.fm$fmodel
bt.forecast = forecast.lm(bt.fit, newdata=bt.newdf)
bt.forecast = bt.forecast$mean
bt.frcst = extendForecast(bt.dates, round(bt.forecast,2))
colnames(bt.frcst) = "FORECAST"

# What really happened...
hist.adj.close = adj.close[(NROW(adj.close)-(n.fore-1)):NROW(adj.close)]
colnames(hist.adj.close) = "HIST"

# Quick comparison
thm = chart_theme()
thm$col$line.col = 'gray'
chart_Series(last(bt.frcst,(n.fore+1)), theme=thm,name=tickers)
add_Series(hist.adj.close,on=1)
# GRAY - FORECAST; RED - HISTORICAL

# Model specs
bt.profit = sum(buy.sell(bt.frcst)$Buy.Sell*(-hist.adj.close))
bt.out = cbind(bt.frcst, hist.adj.close, buy.sell(bt.frcst)$Buy.Sell)
names(bt.out)[3] = "Buy.Sell"
bt.model.acc = acc(bt.out$FORECAST, bt.out$HIST) # Real Accuracy
bt.max.cd = max(bt.fm$matchcd) # CD
bt.ur2 = unadj.rsquared(bt.fit)$unadj.rsquared

# <--- END BACK TESTING

# BEGIN FORECAST MODEL --->

# FIND MATCHES
fm = find.matches(adj.close,n.hist,n.fore,model="ves", use.cd=TRUE)
fit = lm( Y~. , data=fm$rmodel ) #http://bit.ly/WCiGpw
summary(fit)
dwtest(fit)

# if (dwtest(fit) < 2) {
# } 
# else {data[i,1] = ceiling(data[i,1] + (data[i,1] *.5))}

# Plot matches
n.match = NROW(fm$matchindx)
max.index = fm$matchindx
d.matches = index(dates)[1:NROW(dates)]
plota(adj.close, type='l', col='gray', main=tickers)
plota.lines(last(adj.close,n.hist), col='blue')
for(i in 1:n.match) {
  plota.lines(adj.close[(max.index[i]-n.hist+1):max.index[i]], col='red')
}

# Building forecast model
newdf = fm$fmodel
forecast = forecast.lm(fit, newdata=newdf)
forecast = forecast$mean
y = as.xts(last(adj.close,180),           
           index(dates)[(NROW(dates)-(n.hist-1)):NROW(dates)])
z = extendForecast(dates, round(forecast,2))
frcst = rbind(y,z); colnames(frcst) = "Adj.Close"

# Quick technical analysis
thm = chart_theme()
thm$col$line.col = 'blue'
chart_Series(frcst, theme=thm,name=tickers)
add_Series(last(frcst,(n.fore+1)),on=1)
add_SMA(n=50, col = "gray")
add_RSI(n=14)
add_BBands()

# Market Price
mark = round(last(adj.close),2); colnames(mark) = "Adj.Close"
mark$Buy.Sell = 0

# Future Prices
future = tail(frcst, n.fore)
future$Buy.Sell = buy.sell(future$Adj.Close)$Buy.Sell
future = rbind(mark, future)

# Model specs
profit = sum(buy.sell(future$Adj.Close)$Buy.Sell*(-future$Adj.Close))
model.acc = acc(fm$rmodel$Y,fit$fitted.values) # Model Accuracy
max.cd = max(fm$matchcd) # CD
ur2 = unadj.rsquared(fit)$unadj.rsquared

# <--- END FORECAST MODEL

# OUTPUT --->

forecast.report = list(future, profit, model.acc)
names(forecast.report)[1] = "Forecast"
names(forecast.report)[2] = "Profit"
names(forecast.report)[3] = "Model.acc"

bt.report = list(bt.out, bt.profit, bt.model.acc)
names(bt.report)[1] = "Back.Tested.Forecast"
names(bt.report)[2] = "Back.Tested.Profit"
names(bt.report)[3] = "Back.Tested.Model.acc"

bt.report; forecast.report

# <--- OUTPUT

##########################################################################