 
library(seasonal)
library(openxlsx)

mData <- read.xlsx("database.xlsx",sheet = "Seasonal_CO2_M")
 
vEmiss <- mData$Total.Energy.CO2.Emissions

 

tsEmiss <- ts(mData$Total.Energy.CO2.Emissions,start=1973/1/1,
              frequency=12)
mDesEmiss <- as.matrix(seas(tsEmiss)$data)
vDesEmiss <- mDesEmiss[,3]

 
plot(as.numeric(vEmiss),type="l",ylab="CO2 Emissions")
lines(as.numeric(vDesEmiss),lwd=2,col="red")

write.csv(vDesEmiss,file = "M_Deseas_CO2.csv")

###########################################################

mData <- read.xlsx("database.xlsx",sheet = "Seasonal_CO2_Q")

vEmiss <- mData$Total_CO2_Emissions



tsEmiss <- ts(mData$Total_CO2_Emissions,start=1973/1/1,
              frequency=4)
mDesEmiss <- as.matrix(seas(tsEmiss)$data)
vDesEmiss <- mDesEmiss[,3]


plot(as.numeric(vEmiss),type="l",ylab="CO2 Emissions")
lines(as.numeric(vDesEmiss),lwd=2,col="red")

write.csv(vDesEmiss,file = "Q_Deseas_CO2.csv")
 