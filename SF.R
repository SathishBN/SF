##########################################################################

# LOAD PACKAGES
install_github("toolbox", "drewgriffith15")
require(toolbox)
load.packages('forecast,quantmod,toolbox')

# LOAD DATA USING CURRENT QUOTES LOGIC
# ONLY SUPPORTS ONE STOCK CURRENTLY
tickers = toupper(spl('SPY'))
data <- new.env()
data = getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
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
n.hist=45; n.fore=15

# FIND MATCHES
fm = find.matches(adj.close,n.hist,n.fore,model="linear", use.cd=FALSE)
fit = lm( Y~. , data=fm$rmodel ) #http://bit.ly/WCiGpw
summary(fit)
mean(1-abs((fm$rmodel$Y-fit$fitted.values)/fm$rmodel$Y))

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
out = rbind(y,z); colnames(out) = "Adj.Close"

# QUICK TECHNICAL REVIEW
thm = chart_theme()
thm$col$line.col = 'blue'
chart_Series(out, theme=thm,name=tickers)
add_Series(last(out,(n.fore+1)),on=1)
add_SMA(n=50, col = "gray")
add_RSI(n=14)
add_BBands()

# OUTPUT = CURRENT PRICE + FORECAST
tail(out,n.fore+1)

##########################################################################