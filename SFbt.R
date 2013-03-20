##########################################################################

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

# Back testing...
bt.adj.close = adj.close[1:(NROW(adj.close)-n.fore)]
bt.dates = data$dates[1:(NROW(adj.close)-n.fore)]

# FIND MATCHES
fm = find.matches(bt.adj.close,n.hist,n.fore,model="ves", use.cd=TRUE)
fit = lm( Y~. , data=fm$rmodel ) #http://bit.ly/WCiGpw
summary(fit)

dwtest(fit)

# PLOT MATCHES
n.match = NROW(fm$matchindx)
max.index = fm$matchindx
d.matches = index(bt.dates)[1:NROW(bt.dates)]
plota(bt.adj.close, type='l', col='gray', main=tickers)
plota.lines(last(bt.adj.close,n.hist), col='blue')
for(i in 1:n.match) {
  plota.lines(bt.adj.close[(max.index[i]-n.hist+1):max.index[i]], col='red')
}

# BUILD FORECAST
newdf = fm$fmodel
forecast = forecast.lm(fit, newdata=newdf)
forecast = forecast$mean
frcst = extendForecast(bt.dates, round(forecast,2))
colnames(frcst) = "FORECAST"

# What really happened...
hist.adj.close = adj.close[(NROW(adj.close)-(n.fore-1)):NROW(adj.close)]
colnames(hist.adj.close) = "HIST"

# Quick comparison
thm = chart_theme()
thm$col$line.col = 'gray'
chart_Series(last(frcst,(n.fore+1)), theme=thm,name=tickers)
add_Series(hist.adj.close,on=1)
# GRAY - FORECAST; RED - HISTORICAL

# Report
out = cbind(frcst, hist.adj.close); real.acc = acc(out$FORECAST, out$HIST) # Real Accuracy
profit = sum(buy.sell(frcst)$Buy.Sell*(-hist.adj.close));

# Model specs
model.acc = acc(fm$rmodel$Y,fit$fitted.values) # Model Accuracy
max.cd = max(fm$matchcd) # CD
ur2 = unadj.rsquared(fit)$unadj.rsquared

report = list(profit, real.acc, model.acc)
names(report)[1] = "Real.Profit"
names(report)[2] = "Real.acc"
names(report)[3] = "Model.acc"

report
##########################################################################