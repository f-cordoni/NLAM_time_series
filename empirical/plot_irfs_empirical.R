
for (ii in 1:Nvar){
  plot.ts(C_IRFS_resit$AVG[t_star:H,ii] ,ylim=c(min(C_IRFS_resit$LWR[,ii]),
                                             max(C_IRFS_resit$UPR[,ii])))
  lines( C_IRFS_resit$LWR[t_star:H,ii],col=2,lty=2)
  lines( C_IRFS_resit$UPR[t_star:H,ii],col=2,lty=2)
  title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[ii]])  )   )
  plot.ts(Chol_IRFs$AVG[t_star:H,ii],col=3,ylim=c(min(Chol_IRFs$LWR[,ii]),
                                                 max(Chol_IRFs$UPR[,ii])))
  lines(Chol_IRFs$LWR[t_star:H,ii],col=3,lty=2)
  lines( Chol_IRFs$UPR[t_star:H,ii],col=3,lty=2)
  title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[ii]])  )   )

}

# 
# plot.ts(C_IRFS_resit$AVG[t_star:H,2] ,ylim=c(min(C_IRFS_resit$LWR[,2]),
#                                              max(C_IRFS_resit$UPR[,2])))
# lines( C_IRFS_resit$LWR[t_star:H,2],col=2,lty=2)
# lines( C_IRFS_resit$UPR[t_star:H,2],col=2,lty=2)
# title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[2]])  )   )
# plot.ts(Chol_IRFs$AVG[t_star:H,2],col=3,ylim=c(min(Chol_IRFs$LWR[,2]),
#                                                max(Chol_IRFs$UPR[,2])))
# lines(Chol_IRFs$LWR[t_star:H,2],col=3,lty=2)
# lines( Chol_IRFs$UPR[t_star:H,2],col=3,lty=2)
# title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[2]])  )   )
# 
# plot.ts(C_IRFS_resit$AVG[t_star:H,3] ,ylim=c(min(C_IRFS_resit$LWR[,3]),
#                                              max(C_IRFS_resit$UPR[,3])))
# lines( C_IRFS_resit$LWR[t_star:H,3],col=2,lty=2)
# lines( C_IRFS_resit$UPR[t_star:H,3],col=2,lty=2)
# 
# title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[3]])  )   )
# plot.ts(Chol_IRFs$AVG[t_star:H,3],col=3,ylim=c(min(Chol_IRFs$LWR[,3]),
#                                                max(Chol_IRFs$UPR[,3])))
# lines(Chol_IRFs$LWR[t_star:H,3],col=3,lty=2)
# lines( Chol_IRFs$UPR[t_star:H,3],col=3,lty=2)
# title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[3]])  )   )
# 
# if (Nvar ==4){
# title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[4]])  )   )
# plot.ts(Chol_IRFs$AVG[t_star:H,4],col=3,ylim=c(min(Chol_IRFs$LWR[,4]),
#                                                max(Chol_IRFs$UPR[,4])))
# lines(Chol_IRFs$LWR[t_star:H,4],col=3,lty=2)
# lines( Chol_IRFs$UPR[t_star:H,4],col=3,lty=2)
# title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[4]])  )   )
# }