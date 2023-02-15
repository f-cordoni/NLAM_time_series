
# laod data 


# get the 
# load("~/Desktop/non_linear_IRF/SIMULATIONS/200_sim_chain_kstar_1.RData")
# load("~/Desktop/non_linear_IRF/SIMULATIONS/200_sim_chain_kstar_2.RData")
# 
# load("~/Desktop/non_linear_IRF/SIMULATIONS/200_sim_common_cause_kstar_1.RData")
# load("~/Desktop/non_linear_IRF/SIMULATIONS/200_sim_common_cause_kstar_2.RData")
# 
# load("~/Desktop/non_linear_IRF/SIMULATIONS/200_sim_v_structure_kstar_1.RData")
# load("~/Desktop/non_linear_IRF/SIMULATIONS/200_sim_v_structure_kstar_2.RData")

load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/sigma_1/200_sim_chain_kstar_1.RData")
load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/sigma_1/200_sim_common_cause_kstar_1.RData")
load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/sigma_1/200_sim_v_structure_kstar_1.RData")

load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/sigma_0.1/200_sim_chain_kstar_1.RData")
load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/sigma_0.1/200_sim_common_cause_kstar_1.RData")
load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/sigma_0.1/200_sim_v_structure_kstar_1.RData")

load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/no_sign_sigma_1/200_sim_chain_kstar_1.RData")
load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/no_sign_sigma_1/200_sim_common_cause_kstar_1.RData")
load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/no_sign_sigma_1/200_sim_v_structure_kstar_1.RData")

load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/no_sign_sigma_0.1/200_sim_chain_kstar_1.RData")
load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/no_sign_sigma_0.1/200_sim_common_cause_kstar_1.RData")
load("/media/fcordoni/d8a1cc9c-6ed3-4913-92f0-f8c21492668f/non_linear_IRF/no_sign_sigma_0.1/200_sim_v_structure_kstar_1.RData")

# MSE on the variables 2 and 3
require(xtable)
# table_aux = matrix()
# print(xtable(MSE_sr, type='latex'))
# mettere le parentesi e #print only 3 decimal
for (ii in 1:3){
print(round(c(MSE_sr[ii,],MSE_resit[ii,]),3))
print(round(c(SE_sr[ii,],SE_resit[ii,]),3))
print('')
}

for (ii in 1:3){
print(round(c(MSE_true_dag[ii,],MSE_true_top[ii,]),3))
print(round(c(SE_true_dag[ii,],SE_true_top[ii,]),3))
print('')
}


# t.test
TEST_diff_IRFs  # RESIT vs SR
TEST_diff_IRFs_orcl_sr  # ORACLE (true DAG) vs SR
TEST_diff_IRFs_orcl_resit # true DAG vs RESIT

TEST_diff_IRFs_true_top_resit   # true top vs RESIT
TEST_diff_IRFs_true_top_sr # # true top vs SR
TEST_diff_IRFs_true_top_dag #true top vs true DAG

 

 

# accuracy of RESIT to get the topological order


how_many_times_RESIT_topological_order


