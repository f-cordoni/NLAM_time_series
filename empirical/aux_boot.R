for (kk in 1:Nboot){
  out_boot = VAR.boot(model_var_true,boot.scheme = "resample" )

  Y = out_boot
  model_var = lineVar(out_boot,lag = lag,include = "const")
  Nvar = ncol(Y)
  #get the matrices
  A = list()
  A[[lag+1]] = model_var$coefficients[,1]
  model_var$coefficients  = model_var$coefficients[,-1]
  for (ii in 1:lag){
    A[[ii]] = matrix(0,nrow = Nvar,ncol = Nvar)
    
    A[[ii]]   = model_var$coefficients[,((ii-1)*Nvar+1):(ii*Nvar)]# 4:6 # 7 : 9
    
  }
  
  
   residual = (model_var$residuals)
   
  # aux = residual_orig
  # ix = seq(1,nrow(aux))
  # residual = aux[sample(ix,replace = TRUE),]
  
  Omega = cov(residual)
  out_shocks_resit = get_structural_shocks_RESIT(residual,
                                                 flag_oracle = 1, oracle = ORACLE[pi,pi])
  
  E_resit = out_shocks_resit$Structural_shocks
  phi_resit = out_shocks_resit$phi 
  parents_resit = out_shocks_resit$parents
  
  #estimate CONDIRFS from RESIT and Cholesky
 source("g_irfs_for fixed_A.R")
  
  
  irf_RESIT_boot[,,k_star,kk] = C_IRFS_resit$AVG[t_star:H,]
  irf_sr_boot[,,k_star,kk] = Chol_IRFs$AVG[t_star:H,]
  print(kk)
}



irf_true = Chol_IRFs$AVG[t_star:H,]*0

irf_RESIT_avg = irf_true*0;
irf_sr_avg = irf_true*0;
 
irf_RESIT_upper =irf_true*0;
irf_RESIT_lower = irf_true*0;
irf_sr_upper =  irf_true*0;
irf_sr_lower =  irf_true*0;
 

for (ii in k_star){
  irf_RESIT_avg  = rowMeans(irf_RESIT_boot[,,ii,],dims = 2)
  irf_sr_avg  = rowMeans(irf_sr_boot[,,ii,],dims=2)
 
 
  
  #CI of 1 shocks
  for (jj in 1:Nvar){
    irf_RESIT_upper[,jj] = apply(irf_RESIT_boot[,jj,k_star,],1 , 
                                 quantile , probs = c(0.84)  )
    
    irf_RESIT_lower[,jj] = apply(irf_RESIT_boot[,jj,k_star,],1 , 
                                 quantile , probs = c(1-0.84)  )
    
    irf_sr_upper[,jj] = apply(irf_sr_boot[,jj,k_star,],1 , 
                              quantile , probs = c(0.84)  )
    
    irf_sr_lower[,jj] = apply(irf_sr_boot[,jj,k_star,],1 , 
                              quantile , probs = c(1-0.84)  )
    
 
  }
}



require(R.matlab)
aux_s = sprintf("IRFs_delta=%s_kstar=%s_%s.mat",sign(delta),k_star,varnames[pi][k_star])
writeMat(aux_s,irfs_boot = irf_sr_boot, irf_sr_avg = irf_sr_avg,
         irf_RESIT_avg = irf_RESIT_avg, irf_RESIT_boot= irf_RESIT_boot
         )

for (ii in 1:Nvar){
  aux_min = min(c(irf_RESIT_lower[,ii],irf_sr_lower[,ii]))
aux_max = max(c(irf_RESIT_upper[,ii],irf_sr_upper[,ii]))
plot.ts(irf_RESIT_avg[,ii] ,ylim=c(aux_min,aux_max) )
lines( irf_RESIT_upper[,ii],col=2,lty=2)
lines( irf_RESIT_lower[,ii],col=2,lty=2)
lines(irf_sr_avg[,ii],col=3)
lines( irf_sr_upper[,ii],col=3,lty=2)
lines( irf_sr_lower[,ii],col=3,lty=2)
title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[ii]])  )   )

}

# 
# aux_min = min(c(irf_RESIT_lower[,2],irf_sr_lower[,2]))
# aux_max = max(c(irf_RESIT_upper[,2],irf_sr_upper[,2]))
# plot.ts(irf_RESIT_avg[,2] ,ylim=c(aux_min,aux_max) )
# lines( irf_RESIT_upper[,2],col=2,lty=2)
# lines( irf_RESIT_lower[,2],col=2,lty=2)
# lines(irf_sr_avg[,2],col=3)
# lines( irf_sr_upper[,2],col=3,lty=2)
# lines( irf_sr_lower[,2],col=3,lty=2)
# title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[2]])  )   )
# 
# aux_min = min(c(irf_RESIT_lower[,3],irf_sr_lower[,3]))
# aux_max = max(c(irf_RESIT_upper[,3],irf_sr_upper[,3]))
# plot.ts(irf_RESIT_avg[,3] ,ylim=c(aux_min,aux_max) )
# lines( irf_RESIT_upper[,3],col=2,lty=2)
# lines( irf_RESIT_lower[,3],col=2,lty=2)
# lines(irf_sr_avg[,3],col=3)
# lines( irf_sr_upper[,3],col=3,lty=2)
# lines( irf_sr_lower[,3],col=3,lty=2)
# title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[3]])  )   )
# 
# if (Nvar == 4){
#   aux_min = min(c(irf_RESIT_lower[,4],irf_sr_lower[,4]))
#   aux_max = max(c(irf_RESIT_upper[,4],irf_sr_upper[,4]))
#   plot.ts(irf_RESIT_avg[,4] ,ylim=c(aux_min,aux_max) )
#   lines( irf_RESIT_upper[,4],col=2,lty=2)
#   lines( irf_RESIT_lower[,4],col=2,lty=2)
#   lines(irf_sr_avg[,4],col=3)
#   lines( irf_sr_upper[,4],col=3,lty=2)
#   lines( irf_sr_lower[,4],col=3,lty=2)
#   title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[4]])  )   )
#   
# }
