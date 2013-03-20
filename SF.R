# LOAD PACKAGES
# install_github("toolbox", "drewgriffith15")
require(toolbox)
load.packages('forecast,quantmod,toolbox,svDialogs')

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
data = getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = TRUE)
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

# FIND MATCHES
fm = find.matches(adj.close,n.hist,n.fore,model="ves", use.cd=TRUE)
fit = lm( Y~. , data=fm$rmodel ) #http://bit.ly/WCiGpw
summary(fit)
dwtest(fit)

# PLOT MATCHES
n.match = NROW(fm$matchindx)
max.index = fm$matchindx
d.matches = index(dates)[1:NROW(dates)]
plota(adj.close, type='l', col='gray', main=tickers)
plota.lines(last(adj.close,n.hist), col='blue')
for(i in 1:n.match) {
  plota.lines(adj.close[(max.index[i]-n.hist+1):max.index[i]], col='red')
}

# BUILD FORECAST
newdf = fm$fmodel
forecast = forecast.lm(fit, newdata=newdf)
forecast = forecast$mean
y = as.xts(last(adj.close,180),           
           index(dates)[(NROW(dates)-(n.hist-1)):NROW(dates)])
z = extendForecast(dates, round(forecast,2))
frcst = rbind(y,z); colnames(frcst) = "Adj.Close"

# QUICK TECHNICAL REVIEW
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
profit = sum(buy.sell(future$Adj.Close)$Buy.Sell*(-future$Adj.Close));
model.acc = acc(fm$rmodel$Y,fit$fitted.values) # Model Accuracy
max.cd = max(fm$matchcd) # CD
ur2 = unadj.rsquared(fit)$unadj.rsquared

report = list(future, profit, model.acc)
names(report)[1] = "Forecast"
names(report)[2] = "Profit"
names(report)[3] = "Model.acc"

report

##########################################################################