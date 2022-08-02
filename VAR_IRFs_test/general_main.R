

a = c (0.1,0.5,0.9)
TT = c(250,500,1e3)
 
irf_true_boot_a = list()
irf_RESIT_boot_a = list()
irf_sr_boot_a = list()

parents_SR_across_all = array(0, dim = c(3,3,length(a),length(TT)))
parents_resit_across_all = array(0, dim = c(3,3,length(a),length(TT)))

MSE_resit = matrix(0,length(a),length(TT))
MSE_sr = matrix(0,length(a),length(TT))
MSE_resit_orcl = MSE_resit

SE_resit = MSE_resit
SE_sr = MSE_sr
SE_resit_orcl = MSE_resit

TEST_diff_IRFs = list(list(1,2,3),list(1,2,3),list(1,2,3))  # t-test of mse of irfs1->2 1->3 on the first 5 points
TEST_diff_IRFs_orcl_sr = list(list(1,2,3),list(1,2,3),list(1,2,3))  # t-test of mse of irfs1->2 1->3 on the first 5 points
TEST_diff_IRFs_orcl_resit = list(list(1,2,3),list(1,2,3),list(1,2,3))  # t-test of mse of irfs1->2 1->3 on the first 5 points



names(TEST_diff_IRFs) = c(a[1],a[2],a[3])
for (ii in 1:length(a)){
names(TEST_diff_IRFs[[ii]]) = c(TT[1],TT[2],TT[3])
}

for (iT in 1:length(TT)){
for (ia in 1:length(a) ){
  aa  = a[ia]
  N=3
  Nsim= 10
  T=TT[iT]  
  T_horizon = 20
  t_star = 2
  k_star = 1
  H = T_horizon
  delta = 1
  
source("comparison_irfs.R")
  parents_SR_across_all[,,ia,iT] =  parents_SR_all
  parents_resit_across_all[,,ia,iT] = parents_resit_all
}
}

MEAN_RESIT_parents = array(0,dim =c(3,3,3))
SE_RESIT_parents =array(0,dim =c(3,3,3))
for ( ii in 1:3){
  for(jj in 1:3){
    MEAN_RESIT_parents[ii,jj,]  = apply(parents_resit_across_all[ii,jj,,],MARGIN = 2,mean)
    SE_RESIT_parents[ii,jj,] =  apply(parents_resit_across_all[ii,jj,,],
                                     MARGIN = 2,sd)/sqrt(3)
  }
  }
rowSums(parents_resit_across_all[,,,1],dims=2)/3
rowSums(parents_resit_across_all[,,,2],dims=2)/3
rowSums(parents_resit_across_all[,,,3],dims=2)/3

