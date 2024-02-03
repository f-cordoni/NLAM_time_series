#setwd("~/Desktop/SIMULATIONS_RESIT/VAR_IRFs_test_to_send")
# Jan 2 , 2024
# consider only a = 0.5 and k_star =1,
# no plot (at the moment) consider VAR = Choleski structure and VAR  + non Gaussian

# change the delta shock to 1
# re-run the simulations with 
# sigma = 1 / 0.1 
# sign(x) and No-sign(x)

###########
# sigma2       1   0.1 
# sign(x)     x     x
# no-sign(x) (x)     x
tic()
setwd("~/Dropbox/post-doc/non-linearity/code/simulations_comparison/VAR_IRFs_test")
tic = tic
model =  "var_nonGauss" # c("var_linear", "var_nonlinear", "var_nonGauss")


flag_irf_plots =1
JJ_causal = 1 # chain
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("%s_500_sim_%s_kstar_%s_negative_sign.RData",model, flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
toc()
rm(list = ls())

tic()
model =  "var_linear" # c("var_linear", "var_nonlinear", "var_nonGauss")


flag_irf_plots =1
JJ_causal = 1 # chain
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("%s_500_sim_%s_kstar_%s_negative_sign.RData",model, flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())
toc()
# flag_irf_plots =0
# JJ_causal = 1 # chain
# k_star = 2
# source("general_main.R")
# #save environment
# aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
# save.image(file=aux_s_env)
# #and delete all
# rm(list = ls())


model =  "var_nonGauss" # c("var_linear", "var_nonlinear", "var_nonGauss")

flag_irf_plots =1
JJ_causal = 2 # cause
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("%s_500_sim_%s_kstar_%s.RData", model, flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())


model =  "var_linear" # c("var_linear", "var_nonlinear", "var_nonGauss")

flag_irf_plots =1
JJ_causal = 2 # cause
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("%s_500_sim_%s_kstar_%s.RData", model, flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

# flag_irf_plots =0
# JJ_causal = 2 # cause
# k_star = 2
# source("general_main.R")
# #save environment
# aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
# save.image(file=aux_s_env)
# #and delete all
# rm(list = ls())


model =  "var_nonGauss" # c("var_linear", "var_nonlinear", "var_nonGauss")

flag_irf_plots =1
JJ_causal = 3 # v-structure
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("%s_500_sim_%s_kstar_%s.RData",model, flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())


model =  "var_linear" # c("var_linear", "var_nonlinear", "var_nonGauss")

flag_irf_plots =1
JJ_causal = 3 # v-structure
k_star = 1
source("general_main.R")
#save environment
aux_s_env = sprintf("%s_500_sim_%s_kstar_%s.RData",model, flag_causal_structure,k_star)
save.image(file=aux_s_env)
#and delete all
rm(list = ls())

# flag_irf_plots =0
# JJ_causal = 3 # v-structure
# k_star = 2
# source("general_main.R")
# #save environment
# aux_s_env = sprintf("200_sim_%s_kstar_%s.RData", flag_causal_structure,k_star)
# save.image(file=aux_s_env)
# #and delete all
# rm(list = ls())

 

# # rm(list = ls())
