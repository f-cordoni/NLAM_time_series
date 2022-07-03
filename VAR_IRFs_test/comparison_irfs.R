# prova commento
require("tseries")
require(NonlinearTSA)
require("np")
library(dHSIC)
require(gam)
require(gptk)
require(GAS)
require(tsDyn)

library(mgcv)

source("RESIT.R")


# generate VAR- TS with nonlinear contemporaneous effects
N=3
Nsim=500
T=250+1
set.seed(2021)

par_sim=list()
PI_1=matrix(c(0.5,0.2,0.5,
              0.2,0.7,0.6,
              0.2,-0.3,0.25),N,N)
PI_1=t(PI_1)

all(Mod(eigen(PI_1)$values)<1)
#1->2, 1->3, 2->3, 

f12=function (x) x^2#1->2; 
f13=function (x) x^4# 1 ->3
f23=function (x) sin(x^2)
 
# f42=function (x) cos(x+x^2)



count=matrix(0,nrow = 3,ncol=3)
irf_true_boot  = array(0,dim= c(20,N,N,Nsim) )
irf_RESIT_boot = irf_true_boot
irf_sr_boot = irf_true_boot

source("RESIT.R")
aux_graph=matrix(0,N,N)
require(tictoc)
tic()
for (nn in 1:Nsim){
  T=250+1
  eps=MASS::mvrnorm(T, mu = matrix(0,1,N), Sigma = 0.1* diag(N))
  Y=matrix(NA,T,N)
  u=eps*0
  Y_0=c(0.5, 0, -0.5); Y[1,]=Y_0;
  #the contemporaneous effect are computed separately from Y ( in order to make less confusion due co-founder)
  for (tt in 2:T){
    
    u[tt,1]=eps[tt,1];
    u[tt,2]=eps[tt,2]+f12(u[tt,1])
    u[tt,3]=eps[tt,3]+f13(u[tt,1])+f23(u[tt,2])
    
    
    past=PI_1%*%Y[tt-1,]
    Y[tt,1]=past[1]+u[tt,1] 
    Y[tt,2]=past[2]+u[tt,2];  
    Y[tt,3]=past[3]+u[tt,3];  
  }
  Y=Y[-1,]
  T=T-1
  # plot(as.ts(Y) ) 
  
  #estimate VAR with intercept
  hat_PI_1=matrix(c(0),N,N)
  
  #for simplicity I estimate the model with the correct specification
  reg1=lm(Y[2:T,1]~Y[1:(T-1),1]+Y[1:(T-1),2]+Y[1:(T-1),3]+0 )
  reg2=lm(Y[2:T,2]~Y[1:(T-1),1]+Y[1:(T-1),2]+Y[1:(T-1),3]+0 )
  reg3=lm(Y[2:T,3]~Y[1:(T-1),1]+Y[1:(T-1),2]+Y[1:(T-1),3]+0 )
  
  
  hat_PI_1[1, ]=reg1$coefficients[1:3]
  hat_PI_1[2,]=reg2$coefficients[1:3]
  hat_PI_1[3, ]=reg3$coefficients[1:3]
 
  
  residual=matrix(NA,nrow = T-1,ncol=N)
  residual[,1]=reg1$residuals
  residual[,2]=reg2$residuals
  residual[,3]=reg3$residuals

  
  auxY=residual
  
  graph_resit <- ICML(as.matrix(auxY), alpha = 0.05, model = "GP", parsModel = list(), 
              indtest = dhsic.test, 
              parsIndtest = list(method = "ExactFastTrace"), confounder_check = 0, output = FALSE)
  aux_graph=aux_graph+graph_resit
  #true irfs


  irf_true = array(0,dim= c(20,N,N))
  
  
  irf_true[1,,1] =c(0.5,f12(0.5),f13(0.5)+f23(f12(0.5)) )
  irf_true[1,,2] =c(0,0.5,f23(0.5))
  irf_true[1,,3] =c(0,0,0.5)
  
  for (tt in 2:20){
    irf_true[tt,,1] =  PI_1 %*% irf_true[tt-1,,1]
    irf_true[tt,,2] =  PI_1 %*% irf_true[tt-1,,2]
    irf_true[tt,,3] =  PI_1 %*% irf_true[tt-1,,3]
  }
  # for (ii in 1:3){
  #   plot.ts(irf_true[,,ii])
  # }
  
  #irfs linear
  Sigma = cov(residual)
  
  S = t(chol(Sigma))
  irf_sr = irf_true*0
  
  irf_sr[1,,1] =t(S %*% c(0.5,0,0)/S[1,1])
  irf_sr[1,,2] =t(S %*% c(0,0.5,0)/S[2,2])
  irf_sr[1,,3] =t(S %*% c(0,0,0.5)/S[3,3])
  
  for (tt in 2:20){
    irf_sr[tt,,1] =  hat_PI_1 %*% irf_sr[tt-1,,1]
    irf_sr[tt,,2] =  hat_PI_1 %*% irf_sr[tt-1,,2]
    irf_sr[tt,,3] =  hat_PI_1 %*% irf_sr[tt-1,,3]
  }
  # for (ii in 1:3){
  #   plot.ts(irf_sr[,,ii])
  # }

  
  #irfs nonlinear
  irf_RESIT = array(0,dim= c(20,N,N))
  
  # we known that RESIT will be provide the order 
  # 1->2; 1->3; 2->3
  shocks = residual
  shocks[,1] = residual[,1]
     aux_pa = shocks[,1]
    
    options=gpOptions("ftc")
    options$kern$comp=list("rbf","white")
    #options$learnScales=TRUE
    model_12<-gpCreate(1,1,as.matrix(aux_pa),
                    as.matrix(residual[,2]),options)
    yhat<-gpOut(model_12,as.matrix(aux_pa))
    shocks[,2]<-as.matrix(residual[,2])-yhat
    
    aux_pa = shocks[,1:2] # chiedere qui, secondo me è gisuta perché shocks sono indipendenti altrimenti ho problemi di cofounder
    model_3<-gpCreate(2,1,as.matrix(aux_pa),
                       as.matrix(residual[,3]),options)
    yhat<-gpOut(model_3,as.matrix(aux_pa))
    shocks[,3]<-as.matrix(residual[,3])-yhat
    
    
      
    # cond_distr_ss_y <- ecdf(ss_y)
    # 
    # y_shocked = y2[1]+cond_distr_ss_y(runif(100,0,1))
  
# impact of unitary shock (1) of x on y
   y12<-gpOut(model_12,0.5+0*as.matrix(shocks[,1]))
   # y12<-gpOut(model_12,seq(from=-1,to=1,by=1/124)+0*as.matrix(aux_pa))
   # plot(y12)
   
   aux = matrix(0,T-1,2); aux[,1]=0; aux[,2]=y12[1]
   y23<-gpOut(model_3,aux)
   aux = matrix(0,T-1,2); aux[,1]=0.5; aux[,2]=0
   y13<-gpOut(model_3,aux)
   
  irf_RESIT[1,,1] =c(0.5,y12[1],y13[1]+y23[1])
  aux = matrix(0,T-1,2); aux[,1]=0; aux[,2]=0.5
  y23<-gpOut(model_3,aux)
  irf_RESIT[1,,2] =c(0,0.5,y23[1])
  irf_RESIT[1,,3] =c(0,0,0.5)
  
  for (tt in 2:20){
    irf_RESIT[tt,,1] =  hat_PI_1 %*% irf_RESIT[tt-1,,1]
    irf_RESIT[tt,,2] =  hat_PI_1 %*% irf_RESIT[tt-1,,2]
    irf_RESIT[tt,,3] =  hat_PI_1 %*% irf_RESIT[tt-1,,3]
  }
#   for (ii in 1:3){
# #    plot.ts(irf_RESIT[,,ii])
#   }
  
  irf_true_boot[,,,nn] = irf_true
  irf_RESIT_boot[,,,nn] = irf_RESIT
  irf_sr_boot[,,,nn] = irf_sr
}
toc()
#
irf_RESIT_avg = irf_true*0;
irf_sr_avg = irf_true*0;

irf_RESIT_upper =irf_true*0;
irf_RESIT_lower = irf_true*0;
irf_sr_upper =  irf_true*0;
  irf_sr_lower =  irf_true*0;
  for (ii in 1:N){
  irf_RESIT_avg[,,ii] = rowMeans(irf_RESIT_boot[,,ii,],dims = 2)
  irf_sr_avg[,,ii] = rowMeans(irf_sr_boot[,,ii,],dims=2)
  

  #CI of 1 shocks
  irf_RESIT_upper[,ii,1] = apply(irf_RESIT_boot[,ii,1,],1 , 
                               quantile , probs = c(0.68)  )
  
  irf_RESIT_lower[,ii,1] = apply(irf_RESIT_boot[,ii,1,],1 , 
                               quantile , probs = c(1-0.68)  )
  
  irf_sr_upper[,ii,1] = apply(irf_sr_boot[,ii,1,],1 , 
                                 quantile , probs = c(0.68)  )
  
  irf_sr_lower[,ii,1] = apply(irf_sr_boot[,ii,1,],1 , 
                                 quantile , probs = c(1-0.68)  )
  }


 
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

write.csv(cbind(irf_RESIT_avg[,2,1],irf_RESIT_lower[,2,1],
                irf_RESIT_upper[,2,1]),file = "IRF_2_NL_sim.csv")

write.csv(cbind(irf_true[,2,1]),file = "IRF_2_true_sim.csv")

write.csv(cbind(irf_RESIT_avg[,3,1],irf_RESIT_lower[,3,1],
                irf_RESIT_upper[,3,1]),file = "IRF_3_NL_sim.csv")

write.csv(cbind(irf_sr_avg[,2,1],irf_sr_lower[,2,1],
                irf_sr_upper[,2,1]),file = "IRF_2_LIN_sim.csv")

write.csv(cbind(irf_sr_avg[,3,1],irf_sr_lower[,3,1],
                irf_sr_upper[,3,1]),file = "IRF_3_LIN_sim.csv")

write.csv(cbind(irf_true[,3,1]),file = "IRF_3_true_sim.csv")

#consider the first 5 points (Short-Run horizon) of reactions 
#to shocks 1 to the others

aux_e_resit = matrix(0,5,Nsim)
aux_e_sr = matrix(0,5,Nsim)

aux_e_sr_3 = aux_e_sr
aux_e_resit_3 = aux_e_sr
for (nn in 1:Nsim){
    aux_e_resit[,nn] = (irf_RESIT_boot[1:5,2,1,nn]-irf_true[1:5,2,1])^2
    aux_e_sr[,nn] = (irf_sr_boot[1:5,2,1,nn]-irf_true[1:5,2,1])^2
    
    aux_e_resit_3[,nn] = (irf_RESIT_boot[1:5,3,1,nn]-irf_true[1:5,3,1])^2
    aux_e_sr_3[,nn] = (irf_sr_boot[1:5,3,1,nn]-irf_true[1:5,3,1])^2
  }  

aux_e_resit_all = cbind(aux_e_resit, aux_e_resit_3)
aux_e_sr_all = cbind(aux_e_sr, aux_e_sr_3)

require(pracma)
e_RESIT = Reshape(aux_e_resit_all,5*Nsim*2)
e_sr = Reshape(aux_e_sr_all,5*Nsim*2)


d=e_RESIT-e_sr


mean(e_RESIT)
mean(e_sr)
t.test(d)

#calcolare MSE

