setwd("~/Dropbox/post-doc/non-linearity/code/simulations_comparison/empirical")

# data quarterly con GMR application without oil price exogenous

flag_monthly = 0
setwd("data/")
flag_applicatin_fed_fund = 0
source("data_pre_processing.R")
setwd("../")

# try the application where 
#[o_t  energy co2] monthly

# variable selections 
#DATA = DATA_TS[,c("RealGDP","Energy","Total_CO2_Emissions")]
DATA = DATA_TS 
plot.ts(DATA)
#demean the data
scales = apply(DATA,2,sd)
DATA = scale(DATA, center = FALSE, scale = FALSE)
 varnames = c("infl","output","r")
plot.ts(DATA)

DATA = DATA[1:262,]
#1953 Q3 : 2019 Q4
aux_plot_data = ts( DATA , start = c(1954,3),frequency = 4)

# install.packages("latex2exp")
library(latex2exp)
 
colnames(aux_plot_data) = c("pi","o","r")
plot.ts(aux_plot_data, main = "", ylabel= c(,"o","r"))
par(mfrow=c(3,1), mai=c(0.4,0.65,0.4,0.2), cex.lab = 1.5)
plot.ts(x = aux_plot_data[,1],ylab= TeX("$\\pi$"), main="", xlab="")
plot.ts(x = aux_plot_data[,2],ylab= "o", main="", xlab="",  cex.lab = 2)
plot.ts(x = aux_plot_data[,3],ylab= "r", main="",  xlab="",  cex.lab = 2)
source("main_empirical.R")

source("aux_Resit:R.R")

#considerare shock policy monetary sulle altre due
#shock con delta = 1 sd  ,, PRE_COVID
t_star = lag+1
T_horizon =t_star+ 20
k_star = 1
H = T_horizon 
delta = sd(DATA[,k_star])


source("g_irfs_for fixed_A.R")
source("plot_irfs_empirical.R")

delta =   -sd(DATA[,k_star])

source("g_irfs_for fixed_A.R")
source("plot_irfs_empirical.R")

for (k_star in 1:3){
  t_star = lag+1
  T_horizon =t_star+ 20
   
  H = T_horizon 
  
  delta = sd(DATA[,k_star])
 

 

Nboot = 200
irf_RESIT_boot = array(0,dim= c(T_horizon-t_star+1,N,N,Nboot) )
irf_sr_boot = irf_RESIT_boot
flag_boot = 1

if (flag_boot == 1){
  source("aux_boot.R")
}


  delta = -sd(DATA[,k_star])




irf_RESIT_boot = array(0,dim= c(T_horizon-t_star+1,N,N,Nboot) )
irf_sr_boot = irf_RESIT_boot
flag_boot = 1
 
if (flag_boot == 1){
  source("aux_boot.R")
}
}
#questo è ok asimmetria dello shock
# ###################################################################
# setwd("~/Dropbox/post-doc/non-linearity/code/simulations_comparison/empirical")
# setwd("data/")
# flag_applicatin_fed_fund = 1
# flag_monthly = 1
# source("data_pre_processing.R")
# setwd("../")
# 
# DATA = DATA_TS[,c("RealGDP","PGDP","FEDFUNDS")] 
# plot.ts(DATA)
# #demean the data
# scales = apply(DATA,2,sd)
# DATA = scale(DATA, center = FALSE, scale = FALSE)
# plot.ts(DATA)
# 
# varnames = colnames(DATA)
# source("main_empirical.R")
#  
# 
# 
# source("aux_Resit:R.R")
# 
# 
# t_star = lag+1
# T_horizon =t_star+ 20
# k_star = 1
# H = T_horizon 
# delta = 1
# 
# source("g_irfs_for fixed_A.R")
# source("plot_irfs_empirical.R")
# 
# delta =   -100
# 
# 
# source("g_irfs_for fixed_A.R")
# source("plot_irfs_empirical.R")
# 
# Nboot = 100
# 
# t_star = lag+1
# T_horizon =t_star+ 20
# k_star = 1
# H = T_horizon 
# delta = 1
# 
# irf_RESIT_boot = array(0,dim= c(T_horizon-t_star+1,N,N,Nboot) )
# irf_sr_boot = irf_RESIT_boot
# flag_boot = 1
# if (flag_boot == 1){
#   source("aux_boot.R")
# }
# 
# delta = -1
# 
# irf_RESIT_boot = array(0,dim= c(T_horizon-t_star+1,N,N,Nboot) )
# irf_sr_boot = irf_RESIT_boot
# flag_boot = 1
# if (flag_boot == 1){
#   source("aux_boot.R")
# }
