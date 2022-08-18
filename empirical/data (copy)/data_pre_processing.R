#MONTHLY

require(openxlsx)

if (flag_monthly == 1){
mData <- read.xlsx("database.xlsx",sheet = "M")

DATA_TS = ts(mData[,2:7], start = 1992/1/1,frequency = 12)

if(flag_applicatin_fed_fund==1){
  # AUX = ( log(DATA_TS[,1:2]))
  # aux_r = diff( DATA_TS[,3])
  AUX = diff( log(DATA_TS[,1:2]))
  
   DATA_TS    = DATA_TS[-1,]
  # DATA_TS[,1:2]    = AUX
  DATA_TS[,1:2] = AUX
  # DATA_TS[,3] = aux_r
  DATA_TS[,1] = DATA_TS[,1]*100
  DATA_TS[,2] = DATA_TS[,2]*400
  DATA_TS[,3] = DATA_TS[,3]*1  
}else{
  DATA_TS [,4:6] = log(DATA_TS [,4:6])*100
  DATA_TS[,1] = log(DATA_TS[,1])*100
}

  
# Output:
#gdp <- log(DATA$GDPC1[first.date:last.date])
#gdp.pot <- log(DATA$GDPPOT[first.date:last.date])
#y.gdp.gap <- 100*(gdp - gdp.pot)


# Inflation:
#p <- log(DATA$GDPDEF)
#infl <- 400 * (p - c(NaN,p[1:(length(p)-1)]))

DATA_TS = ts(DATA_TS , start = 1992/1/1,frequency = 12)


plot.ts(DATA_TS)
}else{
# quiii
##########################################################
mData <- read.xlsx("database.xlsx",sheet = "Q")

DATA_TS = ts(mData[,2:7], start = 1973/1/1,frequency = 4)

 


# DATA_TS [,4:5] = log(DATA_TS [,4:5])
# AUX = diff( log(DATA_TS[,1:2]))
# 
# DATA_TS    = DATA_TS[-1,]
# DATA_TS[,1:2]    = AUX
# Output:
#gdp <- log(DATA$GDPC1[first.date:last.date])
#gdp.pot <- log(DATA$GDPPOT[first.date:last.date])
#y.gdp.gap <- 100*(gdp - gdp.pot)
# DATA_TS[,1] = DATA_TS[,1]*100
# DATA_TS[,2] = DATA_TS[,2]*400 

if(flag_applicatin_fed_fund==1){
  # AUX = ( log(DATA_TS[,1:2]))
  # aux_r = diff( DATA_TS[,3])
  AUX = diff( log(DATA_TS[,1:2]))
  
  DATA_TS    = DATA_TS[-1,]
  # DATA_TS[,1:2]    = AUX
  DATA_TS[,1:2] = AUX
  # DATA_TS[,3] = aux_r
  DATA_TS[,1] = DATA_TS[,1]*100
  DATA_TS[,2] = DATA_TS[,2]*400
  DATA_TS[,3] = DATA_TS[,3]*1  
}else{
  DATA_TS [,4:6] = log(DATA_TS [,4:6])*100
  DATA_TS[,1] = log(DATA_TS[,1])*100
}

plot.ts(DATA_TS)
}

