library(griffun)
load.packages('forecast,quantmod,svDialogs,lmtest,TTR')

# Disable Sci Notation
options(scipen=999)

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
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
quotes = getQuote(tickers)
for(i in ls(data))
  if( last(index(data[[i]])) < as.Date(quotes[i, 'Trade Time']) ) {
    data[[i]] = rbind( data[[i]], make.xts(quotes[i, spl('Open,High,Low,Last,Volume,Last')],
                                           as.Date(quotes[i, 'Trade Time'])))
  }
bt.prep(data)
# summary(data$prices)

## Calculate Fibonacci Retracements over last 90 periods
hi <- last(Hi(data$SPY),90)
lo <- last(Lo(data$SPY),90)

FR100 <- max(hi)
FR0 <- min(lo)

last90 <- last(data$SPY,90)
last90$FR100 <- FR100
last90$FR0 <- FR0
last90$FR79 <- FR100 - (FR100 - FR0) * 0.786;
last90$FR62 <- FR100 - (FR100 - FR0) * 0.618;
last90$FR50 <- FR100 - (FR100 - FR0) * 0.500;
last90$FR38 <- FR100 - (FR100 - FR0) * 0.382;
last90$FR24 <- FR100 - (FR100 - FR0) * 0.236;
# last90$FR124 <- FR100 + (FR100 - FR0) * 0.236;

## Plot
thm <- chart_theme()
#thm$col$line.col <- 'black'
chart_Series(last90, theme=thm,name="SPY (OHLC)")
add_Series(last90[,7],on=1)
add_Series(last90[,8],on=1)
add_Series(last90[,9],on=1)
add_Series(last90[,10],on=1)
add_Series(last90[,11],on=1)
add_Series(last90[,12],on=1)
add_Series(last90[,13],on=1)
# add_Series(last90[,14],on=1)
last(last90)

