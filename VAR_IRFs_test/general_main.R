

a = c (0.1,0.5,0.9)
TT = c(250,500,1e3)

irf_true_boot_a = list()
irf_RESIT_boot_a = list()
irf_sr_boot_a = list()

parents_SR_across_all = array(0, dim = c(3,3,length(a),length(TT)))
parents_resit_across_all = array(0, dim = c(3,3,length(a),length(TT)))

for (iT in 1:length(TT)){
for (ia in 1:length(a) ){
  aa  = a[ia]
  N=3
  Nsim= 100
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