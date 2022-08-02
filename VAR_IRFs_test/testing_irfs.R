############################# testing
#consider the first 5 points (Short-Run horizon) of reactions 
#to shocks 1 to the others

aux_e_resit = matrix(0,5,Nsim)
aux_e_sr = matrix(0,5,Nsim)
aux_e_resit_orcl = matrix(0,5,Nsim)

aux_e_sr_3 = aux_e_sr
aux_e_resit_3 = aux_e_sr
aux_e_resit_orcl_3 = aux_e_sr

for (nn in 1:Nsim){
  aux_e_resit[,nn] = (irf_RESIT_boot[1:5,2,1,nn]-irf_true[1:5,2])^2
  aux_e_sr[,nn] = (irf_sr_boot[1:5,2,1,nn]-irf_true[1:5,2])^2
  
  aux_e_resit_3[,nn] = (irf_RESIT_boot[1:5,3,1,nn]-irf_true[1:5,3])^2
  aux_e_sr_3[,nn] = (irf_sr_boot[1:5,3,1,nn]-irf_true[1:5,3])^2
  
  aux_e_resit_orcl[,nn] = (irf_RESIT_oracle[1:5,2,1,nn]-irf_true[1:5,2])^2
  aux_e_resit_orcl_3[,nn] = (irf_RESIT_oracle[1:5,3,1,nn]-irf_true[1:5,3])^2
}  

aux_e_resit_all = cbind(aux_e_resit, aux_e_resit_3)
aux_e_sr_all = cbind(aux_e_sr, aux_e_sr_3)
aux_e_resit_all_orcl = cbind(aux_e_resit_orcl, aux_e_resit_orcl_3)

require(pracma)
e_RESIT = Reshape(aux_e_resit_all,5*Nsim*2)
e_sr = Reshape(aux_e_sr_all,5*Nsim*2)
e_orcl = Reshape(aux_e_resit_all_orcl,5*Nsim*2)

d=e_RESIT-e_sr
d_oracle_sr = e_orcl-e_sr
d_oracle_RESIT = e_orcl-e_RESIT

MSE_resit[ia,iT] = mean(e_RESIT)
MSE_sr[ia,iT] = mean(e_sr)
MSE_resit_orcl[ia,iT] = mean(e_orcl)


SE_resit[ia,iT] = sd(e_RESIT)/sqrt(length(e_RESIT))
SE_sr[ia,iT] = sd(e_sr)/sqrt(length(e_sr))
SE_resit_orcl[ia,iT] = sd(e_orcl)/sqrt(length(e_orcl))

TEST_diff_IRFs[[ia]][[iT]] = t.test(d)
TEST_diff_IRFs_orcl_sr[[ia]][[iT]] = t.test(d_oracle_sr)
TEST_diff_IRFs_orcl_resit[[ia]][[iT]] = t.test(d_oracle_RESIT)



