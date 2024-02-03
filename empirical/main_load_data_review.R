

set.seed(0)
InDir = "~/Dropbox/post-doc/non-linearity/code/simulations_comparison/empirical" # # change here your Dir
setwd(InDir)
require(tidyr)


setwd("review/")


# Assign column names to the time series object
load("data_original_application_augmented.RData")


setwd("../")

DATA = DATA_augmented

require(tseries)
DATA = na.remove(DATA)
plot.ts(DATA)
#demean the data
varnames = colnames(DATA)
plot.ts(DATA)

# restrict to few variables
DATA = DATA[, c("pi", "out", "r", "ebp", "ip")]
varnames = colnames(DATA)
plot.ts(DATA)
#1953 Q3 : 2019 Q4



# estimate the TS model, VAR or STVAR model
flag_model = "VAR"
# colnames(DATA) = colnames(aux_plot_data)
Nvar = ncol(DATA)
fix_lag = FALSE
lag = 3
source("main_empirical.R")


# residual <- scale(residual)
source("aux_Resit:R.R")
print(varnames[pi])
# "pi"  "ip"  "out" "ebp" "r"



for (k_star in 1:Nvar){
  # k_star = 1   # <--select the variable to shock, order defined by the new topological order,
  # i.e., pi
  t_star = lag+1
  T_horizon =t_star+ 20
  
  H = T_horizon 
  
  delta = sd(DATA[,varnames[pi][k_star]])*2 # choose the size of the shock
  
  
  
  
  Nboot =250
  irf_RESIT_boot = array(0,dim= c(T_horizon-t_star+1,N,N,Nboot) )
  irf_sr_boot = irf_RESIT_boot
  flag_boot = 1
  
  if (flag_boot == 1){
    source("aux_boot.R")
  }
  
  
  delta = - sd(DATA[,varnames[pi][k_star]])*2 # choose the size of the shock
  
  
  
  
  irf_RESIT_boot = array(0,dim= c(T_horizon-t_star+1,N,N,Nboot) )
  irf_sr_boot = irf_RESIT_boot
  flag_boot = 1
  
  if (flag_boot == 1){
    source("aux_boot.R")
  }
}
