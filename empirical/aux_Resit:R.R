
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
Nvar = ncol(Y)

#permute following the topological order (permutation and RESIT refit made only for graphical purpose,
#example var 1 is 'gdp' , var 2 is 'r' and var 4 is 'pi')

if (flag_model == "VAR"){
  model_var = lineVar(Y,lag = lag,include = "const")
  model_var_true = model_var
  #get the matrices 
  A  = list()
  A[[lag+1]] = model_var$coefficients[,1]
  AUX_COEF = model_var$coefficients[,-1]
  for (ii in 1:lag){
    A[[ii]] = matrix(0,nrow = Nvar,ncol = Nvar)
    
    A[[ii]]   = AUX_COEF[,((ii-1)*Nvar+1):(ii*Nvar)]# 4:6 # 7 : 9
    
  }
  residual = (model_var$residuals)
  
  
  
  out_shocks_resit = get_structural_shocks_RESIT(residual,
                                                 flag_oracle = 1, oracle = ORACLE[pi,pi])
  
  
  E_resit = out_shocks_resit$Structural_shocks
  phi_resit = out_shocks_resit$phi 
  parents_resit = out_shocks_resit$parents
}

if (flag_model == "STVAR"){
  
  start_date = as.Date("1954-03-01")
  end_date <- as.Date("2019-12-01")
  
  # Create a sequence of dates at quarterly frequency
  date_sequence <- seq(start_date, end_date, by = "3 months")
  
  auxDATA =  zoo(Y, order.by = date_sequence) 
  
  y <-auxDATA[-1, ]
  #  y <- Realized[-1,1:3]
  # compute the transition variable
  #st <- auxDATA[-1, 1]
  st = get_st(given_ts = auxDATA, variable_selected = which(pi==2)) # select o

  require(parallel)
  ncores = detectCores(all.tests = FALSE, logical = TRUE)-1
  
  stvalues <- startingVLSTAR(y, p = p, n.combi = 3,
                             singlecgamma = FALSE, st = st, ncores = 1, m=m)
  
  
  fit.VLSTAR <- VLSTAR(y, p = p, singlecgamma = FALSE, starting = stvalues,
                       n.iter = 1, st = st, method ='NLS', ncores = 10, m=m)
  # a few methods for VLSTAR
  #summary(fit.VLSTAR)
  #
  
  #coef(fit.VLSTAR)
  
  residual =residuals(fit.VLSTAR)
  out_shocks_resit = get_structural_shocks_RESIT(residual,
                                                 flag_oracle = 1, oracle = ORACLE[pi,pi])
  
  
  E_resit = out_shocks_resit$Structural_shocks
  phi_resit = out_shocks_resit$phi 
  parents_resit = out_shocks_resit$parents
}



#fare delta +/- 1 che otteniamo effetti diversi
  

#estimate CONDIRFS from RESIT and Cholesky



