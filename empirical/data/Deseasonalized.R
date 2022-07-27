rm(list=ls())

library(seasonal)

wd <- "/Users/mariomartinoli/Dropbox (SSSUP)/Climate-change-models-validation/Calibration_Validation/Final_version_march2022/RW_Data"
setwd(wd)

mData <- read.csv("Real_world_data.csv",header=T)
vEnDem <- mData$Energy_demand
vEmiss <- mData$Total_CO2_Emissions

tsEnDem <- ts(mData$Energy_demand,start=1973/1/1,frequency=4)
mDesEnDem <- as.matrix(seas(tsEnDem)$data)
vDesEnDem <- mDesEnDem[,3]

tsEmiss <- ts(mData$Total_CO2_Emissions,start=1973/1/1,frequency=4)
mDesEmiss <- as.matrix(seas(tsEmiss)$data)
vDesEmiss <- mDesEmiss[,3]

plot(as.numeric(vEnDem),type="l",ylab="Energy demand")
lines(as.numeric(vDesEnDem),lwd=2,col="red")
plot(as.numeric(vEmiss),type="l",ylab="CO2 Emissions")
lines(as.numeric(vDesEmiss),lwd=2,col="red")

# fcDes <- function(x) {
	# x <- ts(x)
	
	# # Centered MA (cMA): create cma time series having the same length with the original TS
	# # cMA has 2 NAs on both ends
	# cma <- filter(x,filter=c(1/8,1/4,1/4,1/4,1/8),sides=2)
	
	# # Ratios = original TS/cma
	# ratio <- x/cma
	
	# # Unadjusted 4 seasonal indexes
	# unadj4si <- ts(1:4)
	# unadj4si[1] <- mean(ratio[3+4*(0:(floor((length(x)-4)/4)-1))])
	# unadj4si[2] <- mean(ratio[4+4*(0:(floor((length(x)-4)/4)-1))])
	# unadj4si[3] <- mean(ratio[5+4*(0:(floor((length(x)-4)/4)-1))])
	# unadj4si[4] <- mean(ratio[6+4*(0:(floor((length(x)-4)/4)-1))])
	
	# # Adjusted 4 seasonal indexes
	# adj4si <- ts(1:4)
	# adj4si[1] <- unadj4si[1]/mean(c(unadj4si[1],unadj4si[2],unadj4si[3],unadj4si[4]))
	# adj4si[2] <- unadj4si[2]/mean(c(unadj4si[1],unadj4si[2],unadj4si[3],unadj4si[4]))
	# adj4si[3] <- unadj4si[3]/mean(c(unadj4si[1],unadj4si[2],unadj4si[3],unadj4si[4]))
	# adj4si[4] <- unadj4si[4]/mean(c(unadj4si[1],unadj4si[2],unadj4si[3],unadj4si[4]))
	
	# # Propagated adjusted seasonal indexes
	# propadjsi <- ts(1:length(x))
	# propadjsi[3+4*(0:(floor((length(x)-4)/4)-1))] <- adj4si[1]
	# propadjsi[4+4*(0:(floor((length(x)-4)/4)-1))] <- adj4si[2]
	# propadjsi[5+4*(0:(floor((length(x)-4)/4)-1))] <- adj4si[3]
	# propadjsi[6+4*(0:(floor((length(x)-4)/4)-1))] <- adj4si[4]
	# propadjsi[1] <- adj4si[3]
	# propadjsi[2] <- adj4si[4]
	# propadjsi[length(x)-1] <- adj4si[1]
	# propadjsi[length(x)] <- adj4si[2]
	
	# # Deseasonalized values
	# out <- x/propadjsi
	# out
# }