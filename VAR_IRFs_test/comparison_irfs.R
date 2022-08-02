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


 


source("aux_functions.R")
source("RESIT.R")

count=matrix(0,nrow = 3,ncol=3)
irf_true_boot  = array(0,dim= c(T_horizon-t_star+1,N,N,Nsim) )
irf_RESIT_boot = irf_true_boot
irf_sr_boot = irf_true_boot
irf_RESIT_oracle = irf_sr_boot


parents_resit_all = diag(N)*0
parents_SR_all = diag(N)*0

   for (nn in 1:Nsim){
PI_1=aa*matrix(c(1,0,0,
              1,1,0,
              1,1,1),N,N)
 PI_1 = t(PI_1)

 
 #1->2, 1->3, 2->3, 
 if (flag_irf_plots == 1){
   f12=function (x) x^2#1->2; 
   f13=function (x) sin(x^2)#sqrt(abs(x))# 1 ->3
   #f23=function (x) sin(x^2)
 }else{
   
   alfa = runif(1,min = 1, max = 4)
   beta = runif(1,min = 1, max = 4)
   
   f12<-function(x, a = alfa){ 
     aux = abs(x)^a 
   return(aux)}#1->2; 
   f13<-function (x, b = beta ){ 
     aux = sin(abs(x)^b)
     return(aux) }
 }
 

 
#generate time series
Y_out = generate_time_series (N = 3,  T = T , sigma = 0.1,
                                 PI_1  = PI_1, f12 = f12, f13  = f13)
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

parents_resit_all = parents_resit_all +parents_resit

phi = list()
phi[[1]] = f12; phi[[2]] = f13; #phi[[3]] = f23

true_parents = matrix(c(0,0,0,
                        1,0,0,
                        1,0,0),N,N)

True_IRFs = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = k_star, 
                          delta = delta , Nsim = 100,
                          Omega = Y[1:(t_star-1),], E = Y_out$S_shocks,
                          PI_hat = PI_1,
                          phi = phi, Pa = true_parents , 
                          flag_resit = 3, q_alfa = 0.68)


 

C_IRFS_resit = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = k_star, 
                          delta = delta , Nsim = 100,
                          Omega = Y[1:(t_star-1),], E = E_resit,
                          PI_hat = out_var$hat_PI_1,
                          phi = phi_resit, Pa = parents_resit , 
                          flag_resit = 1, q_alfa = 0.68)


out_shocks_oracle = get_structural_shocks_RESIT(out_var$residuals,flag_oracle = 1,oracle = true_parents)

E_oracle = out_shocks_oracle$Structural_shocks
phi_resit_oracle = out_shocks_oracle$phi 


C_IRFS_true_oracle = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = k_star, 
                                delta = delta , Nsim = 100,
                                Omega = Y[1:(t_star-1),], E = E_oracle,
                                PI_hat = out_var$hat_PI_1,
                                phi = phi_resit_oracle, Pa = true_parents , 
                                flag_resit = 4, q_alfa = 0.68)

#S = t(chol(cov(out_var$residuals)))
#Sigma_u<-cov(ures) ## this is not a consistent estimator! t( t(x)-colMeans(x) )
ures <- out_var$residuals
ures <- t( t(ures)-colMeans(ures) )# demean data
Lag = 1
Sigma_u<-(t(ures)%*%ures) / (nrow(ures)-1-ncol(ures)*Lag) ## denominator: (T - kp -1). This is a consistent estimator
S<-t(chol(Sigma_u)) 

parents_SR = t(S!=0)*1-diag(N)
S_shocks_chol = t( solve(S) %*% t(out_var$residuals))

parents_SR_all =  parents_SR_all + parents_SR
Chol_IRFs = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = k_star, 
                       delta = delta , Nsim = 100,
                       Omega = Y[1:(t_star-1),], E = S_shocks_chol,
                       PI_hat = out_var$hat_PI_1,
                       phi = phi, Pa = parents_SR , 
                       flag_resit = 2, q_alfa = 0.68, S = S)

 
 
 
irf_true_boot[,,k_star,nn] = True_IRFs$AVG[t_star:H,]
irf_RESIT_boot[,,k_star,nn] = C_IRFS_resit$AVG[t_star:H,]
irf_sr_boot[,,k_star,nn] = Chol_IRFs$AVG[t_star:H,]
irf_RESIT_oracle[,,k_star,nn] = C_IRFS_true_oracle$AVG[t_star:H,]
   }





# arrivato qui sistemare codice sotto

# arrivato qui sistemare codice sotto

#let's fix an a
#
irf_true = True_IRFs$AVG[t_star:H,]

irf_RESIT_avg = irf_true*0;
irf_sr_avg = irf_true*0;
irf_true_avg = irf_true*0;
irf_RESIT_orcl_avg =irf_true*0

irf_RESIT_upper =irf_true*0;
irf_RESIT_lower = irf_true*0;
irf_sr_upper =  irf_true*0;
irf_sr_lower =  irf_true*0;
irf_RESIT_orcl_upper = irf_true*0;
  irf_RESIT_orcl_lower= irf_true*0;

for (ii in k_star){
  irf_RESIT_avg  = rowMeans(irf_RESIT_boot[,,ii,],dims = 2)
  irf_sr_avg  = rowMeans(irf_sr_boot[,,ii,],dims=2)
  irf_true_avg  = rowMeans(irf_true_boot[,,ii,],dims = 2)
  irf_RESIT_orcl_avg = rowMeans(irf_RESIT_oracle[,,ii,],dims = 2)
    
  #CI of 1 shocks
  for (jj in 1:3){
  irf_RESIT_upper[,jj] = apply(irf_RESIT_boot[,jj,1,],1 , 
                                 quantile , probs = c(0.68)  )
  
  irf_RESIT_lower[,jj] = apply(irf_RESIT_boot[,jj,1,],1 , 
                                 quantile , probs = c(1-0.68)  )
  
  irf_sr_upper[,jj] = apply(irf_sr_boot[,jj,1,],1 , 
                              quantile , probs = c(0.68)  )
  
  irf_sr_lower[,jj] = apply(irf_sr_boot[,jj,1,],1 , 
                              quantile , probs = c(1-0.68)  )
  
  irf_RESIT_orcl_upper[,jj] =   apply(irf_RESIT_oracle[,jj,1,],1 , 
                                      quantile , probs = c(0.68)  )

  irf_RESIT_orcl_lower[,jj] = apply(irf_RESIT_oracle[,jj,1,],1 , 
                                   quantile , probs = c(1-0.68)  )
  }
}

if (flag_irf_plots == 1){
#source("irf_plots.R")

# qui salvare with a and T
aux_s = sprintf("_a=%g_T=%g.csv", aa,T)
write.csv(cbind(irf_RESIT_avg[,2],irf_RESIT_lower[,2],
                irf_RESIT_upper[,2]),file = paste("IRF_2_NL_sim",aux_s ))

write.csv(cbind(irf_true[,2]),file = paste("IRF_2_true_sim",aux_s ))

write.csv(cbind(irf_RESIT_avg[,3],irf_RESIT_lower[,3],
                irf_RESIT_upper[,3]),file = paste("IRF_3_NL_sim",aux_s ))

write.csv(cbind(irf_sr_avg[,2],irf_sr_lower[,2],
                irf_sr_upper[,2]),file = paste("IRF_2_LIN_sim",aux_s ))

write.csv(cbind(irf_sr_avg[,3],irf_sr_lower[,3],
                irf_sr_upper[,3]),file = paste("IRF_3_LIN_sim",aux_s ))

write.csv(cbind(irf_true[,3]),file = paste("IRF_3_true_sim",aux_s ))


write.csv(cbind(irf_RESIT_orcl_avg[,2],irf_RESIT_orcl_lower[,2],
                irf_RESIT_orcl_upper[,2]),file = paste("IRF_2_orcl_sim",aux_s ))

write.csv(cbind(irf_RESIT_orcl_avg[,3],irf_RESIT_orcl_lower[,3],
                irf_RESIT_orcl_upper[,3]),file = paste("IRF_3_orcl_sim",aux_s ))

}
source("testing_irfs.R")

