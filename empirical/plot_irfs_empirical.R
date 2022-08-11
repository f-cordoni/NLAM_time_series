
 
plot.ts(C_IRFS_resit$AVG[t_star:H,1] ,ylim=c(min(C_IRFS_resit$LWR[,1]),
                                             max(C_IRFS_resit$UPR[,1])))
lines( C_IRFS_resit$LWR[t_star:H,1],col=2,lty=2)
lines( C_IRFS_resit$UPR[t_star:H,1],col=2,lty=2)
title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[1]])  )   )
plot.ts(Chol_IRFs$AVG[t_star:H,1],col=3,ylim=c(min(Chol_IRFs$LWR[,1]),
                                               max(Chol_IRFs$UPR[,1])))
lines(Chol_IRFs$LWR[t_star:H,1],col=3,lty=2)
lines( Chol_IRFs$UPR[t_star:H,1],col=3,lty=2)
title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[1]])  )   )


plot.ts(C_IRFS_resit$AVG[t_star:H,2] ,ylim=c(min(C_IRFS_resit$LWR[,2]),
                                             max(C_IRFS_resit$UPR[,2])))
lines( C_IRFS_resit$LWR[t_star:H,2],col=2,lty=2)
lines( C_IRFS_resit$UPR[t_star:H,2],col=2,lty=2)
title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[2]])  )   )
plot.ts(Chol_IRFs$AVG[t_star:H,2],col=3,ylim=c(min(Chol_IRFs$LWR[,2]),
                                               max(Chol_IRFs$UPR[,2])))
lines(Chol_IRFs$LWR[t_star:H,2],col=3,lty=2)
lines( Chol_IRFs$UPR[t_star:H,2],col=3,lty=2)
title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[2]])  )   )

plot.ts(C_IRFS_resit$AVG[t_star:H,3] ,ylim=c(min(C_IRFS_resit$LWR[,3]),
                                             max(C_IRFS_resit$UPR[,3])))
lines( C_IRFS_resit$LWR[t_star:H,3],col=2,lty=2)
lines( C_IRFS_resit$UPR[t_star:H,3],col=2,lty=2)
title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[3]])  )   )
plot.ts(Chol_IRFs$AVG[t_star:H,3],col=3,ylim=c(min(Chol_IRFs$LWR[,3]),
                                               max(Chol_IRFs$UPR[,3])))
lines(Chol_IRFs$LWR[t_star:H,3],col=3,lty=2)
lines( Chol_IRFs$UPR[t_star:H,3],col=3,lty=2)
title(main=paste(sprintf("IRF %s -> %s",varnames[pi[k_star]],varnames[pi[3]])  )   )