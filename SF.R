#install_github("toolbox", "drewgriffith15")
#require(toolbox)

# load packages
load.packages('forecast,quantmod,TTR,PerformanceAnalytics,toolbox')

#####################################################################################

# LOAD DATA
ticker = toupper('SPY')
data = getSymbols(ticker, src = 'yahoo', from = '1980-01-01', auto.assign = F)
adj.close = coredata(Ad(data))
dates = index(data)
n.hist=45; n.fore=15

fm = find.matches(adj.close,n.hist,n.fore,model="linear", use.cd=FALSE)
fit = lm( Y~. , data=fm$rmodel ) #http://bit.ly/WCiGpw
summary(fit)
mean(1-abs((fm$rmodel$Y-fit$fitted.values)/fm$rmodel$Y))

newdf = fm$fmodel
forecast = forecast.lm(fit, newdata=newdf)
forecast = forecast$mean

y = as.xts(last(data[,6],180), 
           index(data)[(NROW(data)-(n.hist-1)):NROW(data)])
z = extendForecast(dates, round(forecast,2))
out = rbind(y,z)
colnames(out) = "Adj.Close"

# QUICK TECHNICAL REVIEW
thm = chart_theme()
thm$col$line.col = 'blue'
chart_Series(out, theme=thm,name=ticker) 
add_Series(last(out,(n.fore+1)),on=1)
add_SMA(n=50, col = "grey")
add_RSI(n=14)
add_BBands()

# OUTPUT = PREVIOUS CLOSING + FORECAST
tail(out,n.fore+1)