############################# testing
#consider the first 5 points (Short-Run horizon) of reactions 
#to shocks 1 to the others

aux_e_resit = matrix(0,5,Nsim)
aux_e_sr = matrix(0,5,Nsim)

aux_e_sr_3 = aux_e_sr
aux_e_resit_3 = aux_e_sr
for (nn in 1:Nsim){
  aux_e_resit[,nn] = (irf_RESIT_boot[1:5,2,1,nn]-irf_true[1:5,2])^2
  aux_e_sr[,nn] = (irf_sr_boot[1:5,2,1,nn]-irf_true[1:5,2])^2
  
  aux_e_resit_3[,nn] = (irf_RESIT_boot[1:5,3,1,nn]-irf_true[1:5,3])^2
  aux_e_sr_3[,nn] = (irf_sr_boot[1:5,3,1,nn]-irf_true[1:5,3])^2
}  

aux_e_resit_all = cbind(aux_e_resit, aux_e_resit_3)
aux_e_sr_all = cbind(aux_e_sr, aux_e_sr_3)

require(pracma)
e_RESIT = Reshape(aux_e_resit_all,5*Nsim*2)
e_sr = Reshape(aux_e_sr_all,5*Nsim*2)


d=e_RESIT-e_sr

MSE_resit[ia,iT] = mean(e_RESIT)
MSE_sr[ia,iT] = mean(e_sr)

SE_resit[ia,iT] = sd(e_RESIT)/length(e_RESIT)
SE_sr[ia,iT] = sd(e_sr)/length(e_sr)

TEST_diff_IRFs[[ia]][[iT]] = t.test(d)



