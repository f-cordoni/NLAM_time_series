
#show all plots
for (jj in 1:N){
  for (ii in 1:N){
    plot.ts(irf_true[,ii,jj],xlim = c(1,20),ylim=c(-0.1,0.6))
    lines(irf_RESIT_avg[,ii,jj],col=2)
    lines(irf_sr_avg[,ii,jj],col=3)
    title(main=sprintf("IRF %d -> %d",jj,ii))
  }
}

plot.ts(irf_true[,2,1],xlim = c(1,20),ylim=c(0,0.6))
lines(irf_RESIT_avg[,2,1],col=2)
lines( irf_RESIT_upper[,2,1],col=2,lty=2)
lines( irf_RESIT_lower[,2,1],col=2,lty=2)
lines(irf_sr_avg[,2,1],col=3)
lines( irf_sr_upper[,2,1],col=3,lty=2)
lines( irf_sr_lower[,2,1],col=3,lty=2)
title(main=sprintf("IRF %d -> %d",1,2))

plot.ts(irf_true[,3,1],xlim = c(1,20),ylim=c(-0.1,0.15))
lines(irf_RESIT_avg[,3,1],col=2)
lines( irf_RESIT_upper[,3,1],col=2,lty=2)
lines( irf_RESIT_lower[,3,1],col=2,lty=2)
lines(irf_sr_avg[,3,1],col=3)
lines( irf_sr_upper[,3,1],col=3,lty=2)
lines( irf_sr_lower[,3,1],col=3,lty=2)
title(main=sprintf("IRF %d -> %d",1,3))