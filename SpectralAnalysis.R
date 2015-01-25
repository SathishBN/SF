#SpectralAnalysis.R

# REMOVE ALL OBJECTS
rm(list = ls())

# LOAD SYSTEMATIC INVESTOR TOOLBOX (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

load.packages('forecast,quantmod')

########################################################################################

# LOAD DATA
ticker = toupper('SPY')
data = getSymbols(ticker, src = 'yahoo', from = '1980-01-01', auto.assign = F)
data = adjustOHLC(data, use.Adjusted=T)
data = last(data, 252*10) # pull last n years
summary(data)

# PARAMETERS
n.query = 45 # periods of history
n.fore = 20 # forecast this many periods
n.match = 2 # how many sine/cosine waves each
n.spec = 100 # how many spectral waves

reference = coredata(Ad(data))
n.data = NROW(reference)
query = reference[(n.data-n.query+1):n.data]

########################################################################################

# SETUP SEARCH FOR SINE WAVES
sinwaves = c()
for (i in 1:n.spec){
  h = 1:(n.query+n.fore)
  l = i * 0.1
  s = sin(l*(360/(360/i)*2*pi/360*h))
  sinwaves = c(sinwaves,s)
}
n.waves = NROW(sinwaves)
plot(sinwaves)

# COMPUTE CORRELATION
matches = rep(NA, n.waves) # filling a matrix with NAs
for(i in seq(n.query, n.spec*(n.query+n.fore), by = (n.query+n.fore)) ) {
  window = sinwaves[(i - n.query + 1):i]
  matches[i] = cor(query, window)
}
cd = matches^2 # correlation determination
summary(cd)

# FIND BEST MATCH(ES)
max.index = c()
temp = cd

for(i in 1:n.match) {
  if(any(!is.na(temp))) {
    index = which.max(temp)
    max.index[i] = index
    temp[max(0,index-(n.fore+n.query)):min(n.waves,(index+n.query))] = NA
  }
}
n.match = NROW(max.index) # finding at which point the match occurred

# PULL MATCHES OUT FOR REGRESSION & FORECAST
x = matrix(NA, nr=(n.match), nc=(n.query))
temp = c(sinwaves,query)
for(i in 1:n.match) {
  x[i,] = temp[((max.index[i] - n.query) + 1):(max.index[i])]
}

newx = matrix(NA, nr=(n.match), nc=(n.fore))
temp = sinwaves
for(i in 1:n.match) {
  newx[i,] = temp[(max.index[i]+1):(max.index[i]+n.fore)]
}

########################################################################################

# SETUP SEARCH FOR COSINE WAVES
coswaves = c()
for (i in 1:n.spec){
  h = 1:(n.query+n.fore)
  l = i * 0.1
  c = cos(l*(360/(360/i)*2*pi/360*h))
  coswaves = c(coswaves,c)
}

# COMPUTE CORRELATION
matches = rep(NA, n.waves) # filling a matrix with NAs
for(i in seq(n.query, n.spec*(n.query+n.fore), by = (n.query+n.fore)) ) {
  window = coswaves[(i - n.query + 1):i]
  matches[i] = cor(query, window)
}
cd = matches^2 # correlation determination
summary(cd)

# FIND BEST MATCH(ES)
max.index = c()
temp = cd

for(i in 1:n.match) {
  if(any(!is.na(temp))) {
    index = which.max(temp)
    max.index[i] = index
    temp[max(0,index-(n.fore+n.query)):min(n.waves,(index+n.query))] = NA
  }
}
n.match = NROW(max.index) # finding at which point the match occurred

# PULL MATCHES OUT FOR REGRESSION & FORECAST
z = matrix(NA, nr=(n.match), nc=(n.query))
temp = c(coswaves,query)
for(i in 1:n.match) {
  z[i,] = temp[((max.index[i] - n.query) + 1):(max.index[i])]
}

newz = matrix(NA, nr=(n.match), nc=(n.fore))
temp = coswaves
for(i in 1:n.match) {
  newz[i,] = temp[(max.index[i]+1):(max.index[i]+n.fore)]
}

########################################################################################

# REGRESSION
X = as.data.frame(cbind(t(x),t(z))) #combine sin & cos
Y = reference[(n.data-(n.query-1)):n.data]
df = cbind(data.frame(Y=Y),as.data.frame(X))
fit = lm( Y~. , data=df ) # http://bit.ly/WCiGpw
summary(fit)
mean(1-abs((Y-fit$fitted.values)/Y))  # model accuracy

# COMPARING SPECTRAL MATCH(ES) WITH REAL DATA
par(mfrow=c(3,1))
plot(tail(reference,n.query),type="l",main=ticker,xlab="x", ylab="y")
plot(df$V1,type="l",main="Sin",xlab="x", ylab="y", ylim=c(-6,6))
plot(df$V2,type="l",main="Cos",xlab="x", ylab="y", ylim=c(-6,6))

# FORECAST
newdf = as.data.frame(cbind(t(newx),t(newz)))
forecast = forecast.lm(fit, newdata=newdf)

dates = as.Date(index(last(data[,6],90)))
x = as.xts(round(last(fit$fitted.values,90),2),dates)
y = as.xts(last(data[,6],90),
           index(data)[(NROW(data)-(n.query-1)):NROW(data)])
z = forecast2xts(data, as.data.frame(round(forecast$mean,2)))
out = rbind(y,z)
out2 = rbind(x,z)
colnames(out) = "Adj.Close"
colnames(out2) = "Model"

# PLOT
thm = chart_theme() 
thm$col$line.col = 'blue'
chart_Series(out,theme=thm,name=ticker)
add_Series(out2,on=1)

# OUTPUT
tail(out,n.fore+1) # LAST DAY + FORECAST

#######################################################################################
#######################################################################################