setwd("~/Desktop/SIMULATIONS_RESIT/VAR_IRFs_test_to_send")

setwd("~/Dropbox/post-doc/non-linearity/code/simulations_comparison/VAR_IRFs_test")


flag_irf_plots =0
JJ_causal = 1 # chain
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

flag_irf_plots =0
JJ_causal = 1 # chain
k_star = 2
source("general_main.R")
#save environment
aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())



flag_irf_plots =0
JJ_causal = 2 # cause
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

flag_irf_plots =0
JJ_causal = 2 # cause
k_star = 2
source("general_main.R")
#save environment
aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())



flag_irf_plots =0
JJ_causal = 3 # v-structure
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

flag_irf_plots =0
JJ_causal = 3 # v-structure
k_star = 2
source("general_main.R")
#save environment
aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

############################################### fix the phi
#for plot
flag_irf_plots =1
JJ_causal = 1 # chain
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("PLOT_200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

flag_irf_plots =1
JJ_causal = 1 # chain
k_star = 2
source("general_main.R")
#save environment
aux_s_env = sprintf("PLOT_200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())



flag_irf_plots =1
JJ_causal = 2 # cause
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("PLOT_200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

flag_irf_plots =1
JJ_causal = 2 # cause
k_star = 2
source("general_main.R")
#save environment
aux_s_env = sprintf("PLOT_200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())



flag_irf_plots =1
JJ_causal = 3 # v-structure
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("PLOT_200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

flag_irf_plots =1
JJ_causal = 3 # v-structure
k_star = 2
source("general_main.R")
#save environment
aux_s_env = sprintf("PLOT_200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())