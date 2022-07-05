# prova commento
require("tseries")
require(NonlinearTSA)
require("np")
library(dHSIC)
require(gam)
require(gptk)
require(GAS)
require(tsDyn)

library(mgcv)

source("RESIT.R")


# generate VAR- TS with nonlinear contemporaneous effects


set.seed(2021)

par_sim=list()

#1->2, 1->3, 2->3, 
f12=function (x) x^2#1->2; 
f13=function (x) x^4# 1 ->3
f23=function (x) sin(x^2)
 


source("aux_functions.R")
source("RESIT.R")

count=matrix(0,nrow = 3,ncol=3)
irf_true_boot  = array(0,dim= c(T_horizon-t_star+1,N,N,Nsim) )
irf_RESIT_boot = irf_true_boot
irf_sr_boot = irf_true_boot





   for (nn in 1:Nsim){
PI_1=aa*matrix(c(1,0,0,
              1,1,0,
              1,1,1),N,N)
 PI_1 = t(PI_1)

 
 
 

 
#generate time series
Y_out = generate_time_series (N = 3,  T = T , sigma = 0.1,
                                 PI_1  = PI_1, f12 = f12, f13  = f13, f23 = f23)
#estimate the VAR
Y = Y_out$Y
out_var = VAR_estimation(Y = Y)
#compute IRFs

source("condIRFs_functions.R")
source("RESIT.R")
out_shocks_resit = get_structural_shocks_RESIT(out_var$residuals)

E_resit = out_shocks_resit$Structural_shocks
phi_resit = out_shocks_resit$phi 
parents_resit = out_shocks_resit$parents



phi = list()
phi[[1]] = f12; phi[[2]] = f23; phi[[3]] = f13

true_parents = matrix(c(0,0,0,
                        1,0,0,
                        1,1,0),N,N)

True_IRFs = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = 1, 
                          delta = 0.5 , Nsim = 100,
                          Omega = Y[1:(t_star-1),], E = Y_out$S_shocks,
                          PI_hat = PI_1,
                          phi = phi, Pa = true_parents , 
                          flag_resit = 3, q_alfa = 0.68)


C_IRFS_resit = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = 1, 
                          delta = 0.5 , Nsim = 100,
                          Omega = Y[1:(t_star-1),], E = E_resit,
                          PI_hat = out_var$hat_PI_1,
                          phi = phi_resit, Pa = parents_resit , 
                          flag_resit = 1, q_alfa = 0.68)

S = t(chol(cov(out_var$residuals)))
parents_SR = t(S!=0)*1-diag(N)
S_shocks_chol = t( solve(S) %*% t(out_var$residuals))

Chol_IRFs = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = 1, 
                       delta = 0.5 , Nsim = 100,
                       Omega = Y[1:(t_star-1),], E = S_shocks_chol,
                       PI_hat = out_var$hat_PI_1,
                       phi = phi, Pa = parents_SR , 
                       flag_resit = 2, q_alfa = 0.68, S = S)

# plot.ts(C_IRFS_resit$AVG[t_star:H,2],type="l")
#   lines(C_IRFS_resit$LWR[2:H,2], col="2")
#       lines(C_IRFS_resit$UPR[2:H,2], col = "2")
#       
#       plot.ts(True_IRFs$AVG[t_star:H,2],type="l")
#       lines(True_IRFs$LWR[2:H,2], col="2")
#       lines(True_IRFs$UPR[2:H,2], col = "2")
#       
#       plot.ts(Chol_IRFs$AVG[t_star:H,2],type="l")
#       lines(Chol_IRFs$LWR[2:H,2], col="2")
#       lines(Chol_IRFs$UPR[2:H,2], col = "2")
#       
#       plot.ts(True_IRFs$AVG[t_star:H,3],type="l")
#       lines(C_IRFS_resit$AVG[2:H,3], col="2")
#       lines(Chol_IRFs$AVG[2:H,3], col = "3")
#       
#       plot.ts(True_IRFs$AVG[t_star:H,2],type="l")
#       lines(C_IRFS_resit$AVG[2:H,2], col="2")
#       lines(Chol_IRFs$AVG[2:H,2], col = "3")
      
 
 
irf_true_boot[,,k_star,nn] = True_IRFs$AVG[t_star:H,]
irf_RESIT_boot[,,k_star,nn] = C_IRFS_resit$AVG[t_star:H,]
irf_sr_boot[,,k_star,nn] = Chol_IRFs$AVG[t_star:H,]
   }





# arrivato qui sistemare codice sotto

 #let's fix an a
#
irf_true = True_IRFs$AVG[t_star:H,]

irf_RESIT_avg = irf_true*0;
irf_sr_avg = irf_true*0;
irf_true_avg = irf_true*0;

irf_RESIT_upper =irf_true*0;
irf_RESIT_lower = irf_true*0;
irf_sr_upper =  irf_true*0;
  irf_sr_lower =  irf_true*0;

  for (ii in k_star){
  irf_RESIT_avg  = rowMeans(irf_RESIT_boot[,,],dims = 2)
  irf_sr_avg  = rowMeans(irf_sr_boot[,,],dims=2)
  irf_true_avg  = rowMeans(irf_true_boot[,,],dims = 2)

  #CI of 1 shocks
  irf_RESIT_upper[,ii,1] = apply(irf_RESIT_boot[,ii,1,],1 , 
                               quantile , probs = c(0.68)  )
  
  irf_RESIT_lower[,ii,1] = apply(irf_RESIT_boot[,ii,1,],1 , 
                               quantile , probs = c(1-0.68)  )
  
  irf_sr_upper[,ii,1] = apply(irf_sr_boot[,ii,1,],1 , 
                                 quantile , probs = c(0.68)  )
  
  irf_sr_lower[,ii,1] = apply(irf_sr_boot[,ii,1,],1 , 
                                 quantile , probs = c(1-0.68)  )
  }


 source("irf_plots.R")

write.csv(cbind(irf_RESIT_avg[,2,1],irf_RESIT_lower[,2,1],
                irf_RESIT_upper[,2,1]),file = "IRF_2_NL_sim.csv")

write.csv(cbind(irf_true[,2,1]),file = "IRF_2_true_sim.csv")

write.csv(cbind(irf_RESIT_avg[,3,1],irf_RESIT_lower[,3,1],
                irf_RESIT_upper[,3,1]),file = "IRF_3_NL_sim.csv")

write.csv(cbind(irf_sr_avg[,2,1],irf_sr_lower[,2,1],
                irf_sr_upper[,2,1]),file = "IRF_2_LIN_sim.csv")

write.csv(cbind(irf_sr_avg[,3,1],irf_sr_lower[,3,1],
                irf_sr_upper[,3,1]),file = "IRF_3_LIN_sim.csv")

write.csv(cbind(irf_true[,3,1]),file = "IRF_3_true_sim.csv")


############################# testing
#consider the first 5 points (Short-Run horizon) of reactions 
#to shocks 1 to the others

aux_e_resit = matrix(0,5,Nsim)
aux_e_sr = matrix(0,5,Nsim)

aux_e_sr_3 = aux_e_sr
aux_e_resit_3 = aux_e_sr
for (nn in 1:Nsim){
    aux_e_resit[,nn] = (irf_RESIT_boot[1:5,2,1,nn]-irf_true[1:5,2,1])^2
    aux_e_sr[,nn] = (irf_sr_boot[1:5,2,1,nn]-irf_true[1:5,2,1])^2
    
    aux_e_resit_3[,nn] = (irf_RESIT_boot[1:5,3,1,nn]-irf_true[1:5,3,1])^2
    aux_e_sr_3[,nn] = (irf_sr_boot[1:5,3,1,nn]-irf_true[1:5,3,1])^2
  }  

aux_e_resit_all = cbind(aux_e_resit, aux_e_resit_3)
aux_e_sr_all = cbind(aux_e_sr, aux_e_sr_3)

require(pracma)
e_RESIT = Reshape(aux_e_resit_all,5*Nsim*2)
e_sr = Reshape(aux_e_sr_all,5*Nsim*2)


d=e_RESIT-e_sr


mean(e_RESIT)
mean(e_sr)
t.test(d)


#to shocks 2 to 3 contemporaneous shock

aux_e_resit = matrix(0,1,Nsim)
aux_e_sr = matrix(0,1,Nsim)

aux_e_sr_3 = aux_e_sr
aux_e_resit_3 = aux_e_sr
for (nn in 1:Nsim){
  aux_e_resit[,nn] = (irf_RESIT_boot[1,3,2,nn]-irf_true[1,3,2])^2
  aux_e_sr[,nn] = (irf_sr_boot[1,3,2,nn]-irf_true[1,3,2])^2
  
 
}  

 

require(pracma)
e_RESIT = Reshape(aux_e_resit,1*Nsim)
e_sr = Reshape(aux_e_sr,1*Nsim)


d=e_RESIT-e_sr


mean(e_RESIT)
mean(e_sr)
t.test(d)
#calcolare MSE

