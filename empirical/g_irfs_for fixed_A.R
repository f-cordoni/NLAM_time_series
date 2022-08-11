
C_IRFS_resit = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = k_star, 
                          delta = delta , Nsim = 100,
                          Omega = Y[1:(t_star-1),] , E = E_resit,
                          PI_hat = A,
                          phi = phi_resit, Pa = parents_resit , 
                          flag_resit = 1, q_alfa = 0.99, lag = lag) 

ures <- residual
ures <- t( t(ures)-colMeans(ures) )# demean data
Lag = lag
Sigma_u<-(t(ures)%*%ures) / (nrow(ures)-1-ncol(ures)*Lag) ## denominator: (T - kp -1). This is a consistent estimator
S<-t(chol(Sigma_u)) 

# S = (I+H0 D^{-1}) D
D = diag(diag(S))
S_scaled = S%*% solve(D)

parents_SR = t(S!=0)*1-diag(N)
S_shocks_chol = t( solve(S_scaled) %*% t(ures))


Chol_IRFs = Cond_IRFS (t_star = t_star , H = T_horizon , k_star = k_star, 
                       delta = delta , Nsim = 100,
                       Omega = Y[1:(t_star-1),], E = S_shocks_chol,
                       PI_hat = A,
                       phi = NULL, Pa = parents_SR , 
                       flag_resit = 2, q_alfa = 0.99, S = S_scaled, lag = lag)


