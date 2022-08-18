#MONTHLY

require(openxlsx)

 
mData <- read.xlsx("database.xlsx",sheet = "Q")

DATA_TS = ts(mData[,2:5], start = c(1954,3),frequency = 4)

 

 
# Output:
#gdp <- log(DATA$GDPC1[first.date:last.date])
#gdp.pot <- log(DATA$GDPPOT[first.date:last.date])
#y.gdp.gap <- 100*(gdp - gdp.pot)

gdp <- log(DATA_TS[,1])
gdp.pot <- log(DATA_TS[,2])
y.gdp.gap <- 100*(gdp - gdp.pot)
  


# Inflation:
#p <- log(DATA$GDPDEF)
#infl <- 400 * (p - c(NaN,p[1:(length(p)-1)]))
p <- log(DATA_TS[,3])
infl <- 400 * (p - c(NaN,p[1:(length(p)-1)]))
 

r<- DATA_TS[,4]


DATA_TS = ts(cbind(infl[-1],y.gdp.gap[-1],r[-1]) , start = c(1954,4),frequency = 4)


plot.ts(DATA_TS)
 
