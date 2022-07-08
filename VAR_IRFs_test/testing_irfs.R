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