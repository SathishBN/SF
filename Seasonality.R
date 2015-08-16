# References: Original code from seasonality report by Systematic Investor - http://systematicinvestor.github.io/
# and has been modified to suit my needs.
# Suggested to run time for this script is first day of each month - 9:50 AM EST

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

# Disable Sci Notation and supress warnings
options(scipen=999)
options(warn=-1)

# Main ETF sectors
tickers = c("DIA","GDX","IWM","IYR","IYT","QQQ","SPY","UNG","USO","XLB","XLE","XLF","XLI","XLK","XLP","XLU","XLV","XLY")

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1990-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
bt.prep(data, align='keep.all')

#*****************************************************************
# Compute monthly returns
#******************************************************************
prices = data$prices
n = ncol(prices)

# find month ends
month.ends = endpoints(prices, 'months')

prices = prices[month.ends,]
ret = prices / mlag(prices) - 1

# keep only specified month
retCurr = ret[date.month(index(ret)) == as.numeric(format(Sys.time(), "%m")), ] # GET CURRENT MONTH
retNext = ret[date.month(index(ret)) == as.numeric(format(Sys.time(), "%m"))+1, ] # GET NEXT MONTH

# keep last 15 years
retCurr = last(retCurr,15)
retNext = last(retNext,15)

#*****************************************************************
# Compute stats
#*****************************************************************
# Current Month...
statsCurr = matrix(rep(NA,3*n), nc=n)
colnames(statsCurr) = colnames(prices)
rownames(statsCurr) = spl('Pos,Ret,N')

# Calculate the Positive 
for(i in 1:n) {
statsCurr['Pos',i] = round(sum(retCurr[,i]>0, na.rm=T) / sum(!is.na(retCurr[,i])),2)
statsCurr['Ret',i] = round(mean(retCurr[,i],na.rm=TRUE)*100,2)
statsCurr['N',i] = sum(!is.na(retCurr[,i]))
}
statsCurr<-as.data.frame(t(statsCurr))
statsCurr <- statsCurr[order(-statsCurr[,1], -statsCurr[,2]),]

# Next month...
statsNext = matrix(rep(NA,3*n), nc=n)
colnames(statsNext) = colnames(prices)
rownames(statsNext) = spl('Pos,Ret,N')

# Calculate the Positive 
for(i in 1:n) {
  statsNext['Pos',i] = round(sum(retNext[,i]>0, na.rm=T) / sum(!is.na(retNext[,i])),2)
  statsNext['Ret',i] = round(mean(retNext[,i],na.rm=TRUE)*100,2)
  statsNext['N',i] = sum(!is.na(retNext[,i]))
}
statsNext<-as.data.frame(t(statsNext))
statsNext <- statsNext[order(-statsNext[,1], -statsNext[,2]),]

# CURRENT MONTH
statsCurr.OUT <- first(statsNext[,1:2],5)
# NEXT MONTH
statsNext.OUT <- first(statsNext[,1:2],5)

# Send to Pushbullet
library(httr)
setwd("/Users/wgriffith2/Dropbox/R/API")
wd <- getwd()

# NEVER put credentials or api keys in script bodies!!
# Get API key from file location
wag.api.key<-readLines(paste0(wd,"/wag.rpushbullet.json"))
jdr.api.key<-readLines(paste0(wd,"/jdr.rpushbullet.json"))

push.text <- paste0(statsCurr[,1:2])

# Send note through Pushbullet
api <- "https://api.pushbullet.com/api/pushes"
note <- list(type='note', title='Seasonality Report', body=push.text)
invisible(POST(api, authenticate(wag.api.key, ""), body=note))
invisible(POST(api, authenticate(jdr.api.key, ""), body=note))

## <--- END


###############################################################################