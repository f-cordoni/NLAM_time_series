############################# testing
#consider the first 5 points (Short-Run horizon) of reactions 
#to shocks 1 to the others

aux_e_resit = matrix(0,5,Nsim)
aux_e_sr = matrix(0,5,Nsim)
aux_e_resit_orcl = matrix(0,5,Nsim)
aux_e_resit_true_topo = matrix(0,5,Nsim)

aux_e_sr_3 = aux_e_sr
aux_e_resit_3 = aux_e_sr
aux_e_resit_orcl_3 = aux_e_sr
aux_e_resit_true_topo_3 = aux_e_sr


for (nn in 1:Nsim){
  aux_e_resit[,nn] = (irf_RESIT_boot[1:5,2,k_star,nn]-irf_true[1:5,2])^2
  aux_e_sr[,nn] = (irf_sr_boot[1:5,2,k_star,nn]-irf_true[1:5,2])^2
  
  aux_e_resit_3[,nn] = (irf_RESIT_boot[1:5,3,k_star,nn]-irf_true[1:5,3])^2
  aux_e_sr_3[,nn] = (irf_sr_boot[1:5,3,k_star,nn]-irf_true[1:5,3])^2
  
  aux_e_resit_orcl[,nn] = (irf_true_dag[1:5,2,k_star,nn]-irf_true[1:5,2])^2
  aux_e_resit_orcl_3[,nn] = (irf_true_dag[1:5,3,k_star,nn]-irf_true[1:5,3])^2
  
  aux_e_resit_true_topo[,nn] = (irf_true_top[1:5,2,k_star,nn]-irf_true[1:5,2])^2
  aux_e_resit_true_topo_3[,nn] = (irf_true_top[1:5,3,k_star,nn]-irf_true[1:5,3])^2
}  

aux_e_resit_all = cbind(aux_e_resit, aux_e_resit_3)
aux_e_sr_all = cbind(aux_e_sr, aux_e_sr_3)
aux_e_resit_all_orcl = cbind(aux_e_resit_orcl, aux_e_resit_orcl_3)
aux_e_resit_all_true_top = cbind(aux_e_resit_true_topo, aux_e_resit_true_topo_3)

require(pracma)
e_RESIT = Reshape(aux_e_resit_all,5*Nsim*2)
e_sr = Reshape(aux_e_sr_all,5*Nsim*2)
e_orcl = Reshape(aux_e_resit_all_orcl,5*Nsim*2)
e_true_top = Reshape(aux_e_resit_all_true_top,5*Nsim*2)

d=e_RESIT-e_sr
d_oracle_sr = e_orcl-e_sr
d_oracle_RESIT = e_orcl-e_RESIT

d_true_top_RESIT = e_true_top-e_RESIT
d_true_top_sr = e_true_top-e_sr
d_true_top_oracle = e_true_top-e_orcl

MSE_resit[ia,iT] = mean(e_RESIT)
MSE_sr[ia,iT] = mean(e_sr)
MSE_true_dag[ia,iT] = mean(e_orcl)
MSE_true_top[ia,iT] = mean(e_true_top)


SE_resit[ia,iT] = sd(e_RESIT)/sqrt(length(e_RESIT))
SE_sr[ia,iT] = sd(e_sr)/sqrt(length(e_sr))
SE_true_dag[ia,iT] = sd(e_orcl)/sqrt(length(e_orcl))
SE_true_top[ia,iT] = sd(e_true_top)/sqrt(length(e_true_top))

TEST_diff_IRFs[[ia]][[iT]] = t.test(d)
TEST_diff_IRFs_orcl_sr[[ia]][[iT]] = t.test(d_oracle_sr)
TEST_diff_IRFs_orcl_resit[[ia]][[iT]] = t.test(d_oracle_RESIT)

TEST_diff_IRFs_true_top_resit[[ia]][[iT]] = t.test(d_true_top_RESIT)
TEST_diff_IRFs_true_top_sr[[ia]][[iT]] = t.test(d_true_top_sr)
TEST_diff_IRFs_true_top_dag[[ia]][[iT]] = t.test(d_true_top_oracle)
 


