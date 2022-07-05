

a = c (0.5)
TT = c(250,500,1e3)

irf_true_boot_a = list()
irf_RESIT_boot_a = list()
irf_sr_boot_a = list()

for (ia in 1:length(a) ){
  aa  = a[ia]
  N=3
  Nsim=500
  T=1e3  
  T_horizon = 20
  t_star = 2
  
source("comparison_irfs.R")
irf_true_boot_a[[ia]] = irf_true_boot
irf_RESIT_boot_a[[ia]] = irf_RESIT_boot
irf_sr_boot_a[[ia]] = irf_sr_boot
}