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
N=3
Nsim=500
T=250  
set.seed(2021)

par_sim=list()

#1->2, 1->3, 2->3, 
f12=function (x) x^2#1->2; 
f13=function (x) x^4# 1 ->3
f23=function (x) sin(x^2)
 
a = c (0.1,0.5,0.9)

T_horizon = 20
source("aux_functions.R")
source("RESIT.R")

count=matrix(0,nrow = 3,ncol=3)
irf_true_boot  = array(0,dim= c(T_horizon,N,N,Nsim) )
irf_RESIT_boot = irf_true_boot
irf_sr_boot = irf_true_boot



 for (aa in a){
   for (nn in 1:Nsim){
PI_1=aa*matrix(c(1,0,0,
              1,1,0,
              1,1,1),N,N)
 PI_1 = t(PI_1)

 
 
 

 
#generate time series
Y = generate_time_series (N = 3,  T = T , sigma = 0.1,
                                 PI_1  = PI_1, f12 = f12, f13  = f13, f23 = f23)
#estimate the VAR
out_var = VAR_estimation(Y = Y)
#compute IRFs

out_shocks_resit = get_structural_shocks_RESIT(auxY)

E_resit = out_shocks_resit$Structural_shocks
phi_resit = out_shocks_resit$phi 
parents_resit = out_shocks_resit$parents

C_IRFS_resit = Cond_IRFS (t_star = 1 , H = T_horizon , k_star = 1, delta = 0.5 , Nsim = 100,
                          Omega, E = E_resit, PI_hat = out_var$hat_PI_1,
                          phi = phi_resit, Pa = parents_resit , flag_resit = 1)

irf_RESIT = array(0,dim= c(T_horizon,N,N))

# f42=function (x) cos(x+x^2)
out_irfs = get_IRFS ( auxY = out_var$residuals ,   f12 = f12, f13 = f13, f23 = f23, 
                     size_shock = 0.5,
                     PI_1 = PI_1, hat_PI_1 = out_var$hat_PI_1, T_horizon = T_horizon)
 
irf_true_boot[,,,nn] = out_irfs$irf_true
irf_RESIT_boot[,,,nn] = out_irfs$irf_RESIT
irf_sr_boot[,,,nn] = out_irfs$irf_sr
   }
 }






 
#
irf_true = out_irfs$irf_true

irf_RESIT_avg = irf_true*0;
irf_sr_avg = irf_true*0;

irf_RESIT_upper =irf_true*0;
irf_RESIT_lower = irf_true*0;
irf_sr_upper =  irf_true*0;
  irf_sr_lower =  irf_true*0;
  for (ii in 1:N){
  irf_RESIT_avg[,,ii] = rowMeans(irf_RESIT_boot[,,ii,],dims = 2)
  irf_sr_avg[,,ii] = rowMeans(irf_sr_boot[,,ii,],dims=2)
  

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

