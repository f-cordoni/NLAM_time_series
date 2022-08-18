# prova commento
require("tseries")
require(NonlinearTSA)
require("np")
library(dHSIC)
require(gam)
require(gptk)


library(mgcv)

source("../VAR_IRFs_test/RESIT.R")


# generate VAR- TS with nonlinear contemporaneous effects


set.seed(2021)


 
source("../VAR_IRFs_test/RESIT.R", local = TRUE)

source("../VAR_IRFs_test/condIRFs_functions.R",local = TRUE)



out_shocks_resit = get_structural_shocks_RESIT(residual)


E_resit = out_shocks_resit$Structural_shocks
phi_resit = out_shocks_resit$phi 
parents_resit = out_shocks_resit$parents


ORACLE = parents_resit


#perform a bootstrap to generate a new time series and estimate Y_t

#set parameter of IRFs
N = ncol(DATA)

require(igraph)
pi = t(as.matrix(topo_sort(graph.adjacency(ORACLE))))
  

Y = DATA[,pi]
#permute following the topological order 

model_var = lineVar(Y,lag = lag,include = "const")
model_var_true = model_var
#get the matrices 
A  = list()
A[[lag+1]] = model_var$coefficients[,1]
AUX_COEF = model_var$coefficients[,-1]
for (ii in 1:lag){
  A[[ii]] = matrix(0,nrow = 3,ncol = 3)
  
  A[[ii]]   = AUX_COEF[,((ii-1)*3+1):(ii*3)]# 4:6 # 7 : 9
  
}
residual = (model_var$residuals)



out_shocks_resit = get_structural_shocks_RESIT(residual,
                                               flag_oracle = 1, oracle = ORACLE[pi,pi])


E_resit = out_shocks_resit$Structural_shocks
phi_resit = out_shocks_resit$phi 
parents_resit = out_shocks_resit$parents


#fare delta +/- 1 che otteniamo effetti diversi
  

#estimate CONDIRFS from RESIT and Cholesky



