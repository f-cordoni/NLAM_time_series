  # 
  # for (jj in 1:N){
  #   plot.ts(irf_true[,jj],xlim = c(1,20),ylim=c(-0.1,0.6))
  #   lines(irf_RESIT_avg[,jj],col=2)
  #   lines(irf_sr_avg[,jj],col=3)
  #   title(main=sprintf("IRF %d -> %d",1,jj))
  # }
 
aux_s = sprintf(" a = %g, T = %g", a,T)
plot.ts(irf_true[,2],xlim = c(1,20),ylim=c(0,max(irf_true[,2])+0.05) )
lines(irf_RESIT_avg[,2],col=2)
lines( irf_RESIT_upper[,2],col=2,lty=2)
lines( irf_RESIT_lower[,2],col=2,lty=2)
lines(irf_sr_avg[,2],col=3)
lines( irf_sr_upper[,2],col=3,lty=2)
lines( irf_sr_lower[,2],col=3,lty=2)
title(main=paste(sprintf("IRF %d -> %d",1,2),aux_s )   )

plot.ts(irf_true[,3],xlim = c(1,20),ylim=c(-0.1, max(irf_true[,3])+0.05) )
lines(irf_RESIT_avg[,3],col=2)
lines( irf_RESIT_upper[,3],col=2,lty=2)
lines( irf_RESIT_lower[,3],col=2,lty=2)
lines(irf_sr_avg[,3],col=3)
lines( irf_sr_upper[,3],col=3,lty=2)
lines( irf_sr_lower[,3],col=3,lty=2)
title(main=paste(sprintf("IRF %d -> %d",1,3),aux_s )   )