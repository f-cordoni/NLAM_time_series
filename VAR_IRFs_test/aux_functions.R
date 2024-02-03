
generate_time_series <- function(N = 3,    T = 250 , sigma = 1,
                                 PI_1, f12, f13, f23 ,
                                 flag_causal_structure,
                                 model="var_nonlinear",
                                 scale_laplace = c(1, 2, 4)){
  
  # model  =c("var_linear", "var_nonlinear", "var_nonGauss")
  
  
    T = T +1
    if (model == "var_nonGauss"){
      # Generating Laplace distribution with parameters (mu = 0, b = 1)
      eps <- matrix(0, nrow = T, ncol = N)
    
      # Generating Laplace random variables for each scale
      for (i in seq_along(scale_laplace)) {
        eps[, i] <- VGAM::rlaplace(T, location = 0, scale = scale_laplace[i])
      }
      
    }else{
      eps = MASS::mvrnorm(T, mu = matrix(0,1,N), Sigma = sigma* diag(N))
    }
    
    Y=matrix(NA,T,N)
    u=eps*0
    Y_0=c(0.5, 0, -0.5); Y[1,]=Y_0;
    #the contemporaneous effect are computed separately from Y ( in order to make less confusion due co-founder)
    for (tt in 2:T){
     
        
      switch(flag_causal_structure,
             chain={
               # 1->2->3
               u[tt,1]=eps[tt,1];
               u[tt,2]=eps[tt,2]+f12(u[tt,1])
               u[tt,3]=eps[tt,3]+f23(u[tt,2]) 
               # print("chain")
             },
             common_cause={
               # 1->2;   1->3
               u[tt,1]=eps[tt,1];
               u[tt,2]=eps[tt,2]+f12(u[tt,1])
               u[tt,3]=eps[tt,3]+f13(u[tt,1])  
               # print("common-cause")
             },
             v_structure={
               # 1->3; 2->3
               u[tt,1]=eps[tt,1];
               u[tt,2]=eps[tt,2]
               u[tt,3]=eps[tt,3]+f13(u[tt,1]) +f23(u[tt,2])
               # print("v-structure")
             })
      
 
      past=PI_1%*%Y[tt-1,]
      Y[tt,1]=past[1]+u[tt,1] 
      Y[tt,2]=past[2]+u[tt,2];  
      Y[tt,3]=past[3]+u[tt,3];  
    }
    Y=Y[-1,]
    T=T-1
    # plot(as.ts(Y) ) 
  
  return(list(Y = Y, u = u[-1,] , S_shocks = eps[-1,]) )
}

VAR_estimation <- function(Y){
    #estimate VAR with intercept
  N = ncol(Y)
  T = nrow(Y)
    hat_PI_1=matrix(c(0),N,N)
    
    #for simplicity I estimate the model with the correct specification of lag
    reg1=lm(Y[2:T,1]~Y[1:(T-1),1]+Y[1:(T-1),2]+Y[1:(T-1),3] +0 )
    reg2=lm(Y[2:T,2]~Y[1:(T-1),1]+Y[1:(T-1),2]+Y[1:(T-1),3] + 0 )
    reg3=lm(Y[2:T,3]~Y[1:(T-1),1]+Y[1:(T-1),2]+Y[1:(T-1),3]+0 )
    
    
    hat_PI_1[1, ]=reg1$coefficients[1:3]
    hat_PI_1[2,]=reg2$coefficients[1:3]
    hat_PI_1[3, ]=reg3$coefficients[1:3]
    
    
    residual=matrix(NA,nrow = T-1,ncol=N)
    residual[,1]=reg1$residuals
    residual[,2]=reg2$residuals
    residual[,3]=reg3$residuals
    
    
    auxY=residual

    return(list(residuals = auxY, hat_PI_1 = hat_PI_1 ))    
}
# 
# get_IRFS <-function( auxY ,   f12, f13, f23, size_shock = 0.5,
#                      PI_1, hat_PI_1, T_horizon = 20,                     ){
#   
#   #conditional IRFS
#   residual = auxY
#   N = ncol(auxY)
#   aux_graph = matrix(0,N,N)
#   
#     graph_resit <- ICML(as.matrix(auxY), alpha = 0.05, model = "GP", parsModel = list(), 
#                         indtest = dhsic.test, 
#                         parsIndtest = list(method = "ExactFastTrace"), confounder_check = 0, output = FALSE)
#     aux_graph=aux_graph+graph_resit
#      
#     #true irfs
#     
#     
#     irf_true = array(0,dim= c(20,N,N))
#     
#     
#     irf_true[1,,1] =c(size_shock,f12(size_shock),f13(size_shock)+f23(f12(size_shock)) )
#     irf_true[1,,2] =c(0,size_shock,f23(size_shock))
#     irf_true[1,,3] =c(0,0,size_shock)
#     
#     for (tt in 2:T_horizon){
#       irf_true[tt,,1] =  PI_1 %*% irf_true[tt-1,,1]
#       irf_true[tt,,2] =  PI_1 %*% irf_true[tt-1,,2]
#       irf_true[tt,,3] =  PI_1 %*% irf_true[tt-1,,3]
#     }
#     # for (ii in 1:3){
#     #   plot.ts(irf_true[,,ii])
#     # }
#     
#     #irfs linear
#     Sigma = cov(auxY)
#     
#     S = t(chol(Sigma))
#     irf_sr = irf_true*0
#     
#     irf_sr[1,,1] =t(S %*% c(0.5,0,0)/S[1,1])
#     irf_sr[1,,2] =t(S %*% c(0,0.5,0)/S[2,2])
#     irf_sr[1,,3] =t(S %*% c(0,0,0.5)/S[3,3])
#     
#     for (tt in 2:T_horizon){
#       irf_sr[tt,,1] =  hat_PI_1 %*% irf_sr[tt-1,,1]
#       irf_sr[tt,,2] =  hat_PI_1 %*% irf_sr[tt-1,,2]
#       irf_sr[tt,,3] =  hat_PI_1 %*% irf_sr[tt-1,,3]
#     }
#     # for (ii in 1:3){
#     #   plot.ts(irf_sr[,,ii])
#     # }
#     
#     
#     #irfs nonlinear
#     
# 
#     
#     # we known that RESIT will be provide the order 
#     # 1->2; 1->3; 2->3
#     shocks = residual
#     shocks[,1] = residual[,1]
#     aux_pa = shocks[,1]
#     
#     options=gpOptions("ftc")
#     options$kern$comp=list("rbf","white")
#     #options$learnScales=TRUE
#     model_12<-gpCreate(1,1,as.matrix(aux_pa),
#                        as.matrix(residual[,2]),options)
#     yhat<-gpOut(model_12,as.matrix(aux_pa))
#     shocks[,2]<-as.matrix(residual[,2])-yhat
#     
#     aux_pa = shocks[,1:2] # chiedere qui, secondo me è gisuta perché shocks sono indipendenti altrimenti ho problemi di cofounder
#     model_3<-gpCreate(2,1,as.matrix(aux_pa),
#                       as.matrix(residual[,3]),options)
#     yhat<-gpOut(model_3,as.matrix(aux_pa))
#     shocks[,3]<-as.matrix(residual[,3])-yhat
#     
#     
#     
#     # cond_distr_ss_y <- ecdf(ss_y)
#     # 
#     # y_shocked = y2[1]+cond_distr_ss_y(runif(100,0,1))
#     
#     # impact of unitary shock (1) of x on y
#     y12<-gpOut(model_12,0.5+0*as.matrix(shocks[,1]))
#     # y12<-gpOut(model_12,seq(from=-1,to=1,by=1/124)+0*as.matrix(aux_pa))
#     # plot(y12)
#     
#     aux = matrix(0,T-1,2); aux[,1]=0; aux[,2]=y12[1]
#     y23<-gpOut(model_3,aux)
#     aux = matrix(0,T-1,2); aux[,1]=0.5; aux[,2]=0
#     y13<-gpOut(model_3,aux)
#     
#     irf_RESIT[1,,1] =c(0.5,y12[1],y13[1]+y23[1])
#     aux = matrix(0,T-1,2); aux[,1]=0; aux[,2]=0.5
#     y23<-gpOut(model_3,aux)
#     irf_RESIT[1,,2] =c(0,0.5,y23[1])
#     irf_RESIT[1,,3] =c(0,0,0.5)
#     
#     for (tt in 2:T_horizon){
#       irf_RESIT[tt,,1] =  hat_PI_1 %*% irf_RESIT[tt-1,,1]
#       irf_RESIT[tt,,2] =  hat_PI_1 %*% irf_RESIT[tt-1,,2]
#       irf_RESIT[tt,,3] =  hat_PI_1 %*% irf_RESIT[tt-1,,3]
#     }
#     #   for (ii in 1:3){
#     # #    plot.ts(irf_RESIT[,,ii])
#     #   }
#     
#     return(list(irf_true = irf_true,
#                 irf_sr = irf_sr,
#                 irf_RESIT = irf_RESIT))
#     
#     
# }


 