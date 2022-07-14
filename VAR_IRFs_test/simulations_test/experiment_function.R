experiment2parralel <- function(p,T,pCon,linear,pars,expCounter)
{
    res <- rep(NA,14)
    cat("\r p =",p,", n =",T, ", exp = ", expCounter, "\r")

    # Generate data
    trueG <- as.matrix(randomDAG(p,pCon))

    if(linear=="VAR+NonGauss")
    {
        trueB <- randomB(trueG,0.1,2,TRUE)
        X <- sampleDataFromG(T+1,trueG,funcType="linear", 
                             parsFuncType=list(B=trueB,kap=1,sigmax=1,sigmay=1,output=FALSE), 
                             noiseType="MixedGauss", parsNoise=list(varMin=0.1,varMax=0.5,noiseExpVarMin=2,noiseExpVarMax=4))
        X <- as.matrix(X)
        #is the contemporaneous effect noise
        
        
        Y=matrix(NA,T+1,p)
        u= X
        Y_0=c(0.5, 0, -0.5,0.25); Y[1,]=Y_0;
        PI_1=diag(runif(p))
        for (tt in 2:(T+1)){

            past=PI_1%*%Y[tt-1,]
            Y[tt,1]=past[1]+u[tt,1] 
            Y[tt,2]=past[2]+u[tt,2];  
            Y[tt,3]=past[3]+u[tt,3];  
            Y[tt,4]=past[4]+u[tt,4]; 
        }
        Y=Y[-1,]
        
        reg1=lm(Y[2:T,1]~Y[1:(T-1),1])
        reg2=lm(Y[2:T,2]~Y[1:(T-1),2])
        reg3=lm(Y[2:T,3]~Y[1:(T-1),3])
        reg4=lm(Y[2:T,4]~Y[1:(T-1),4])
  
        
        residual=matrix(NA,nrow = T-1,ncol=4)
        residual[,1]=reg1$residuals
        residual[,2]=reg2$residuals
        residual[,3]=reg3$residuals
        residual[,4]=reg4$residuals
        
        X=residual
        
        
        
         
        
        # STVAR+Nonlinear
        
        #and so on
    } 
    
    if(linear=="VAR+Nonlinear"){
      trueB <- randomB(trueG,0.1,2,TRUE)
      X <- sampleDataFromG(T+1,trueG,funcType="GAM", parsFuncType=list(kap=1,sigmax=1,sigmay=1,output=FALSE), 
                           noiseType="normalRandomVariances", parsNoise=list(noiseExp=1,varMin=1,varMax=2))
      X <- as.matrix(X)
      #is the contemporaneous effect noise
      
      
      Y=matrix(NA,T+1,p)
      u= X
      Y_0=c(0.5, 0, -0.5,0.25); Y[1,]=Y_0;
      PI_1=diag(runif(p))
      
      for (tt in 2:(T+1)){
        
        past=PI_1%*%Y[tt-1,]
        Y[tt,1]=past[1]+u[tt,1] 
        Y[tt,2]=past[2]+u[tt,2];  
        Y[tt,3]=past[3]+u[tt,3];  
        Y[tt,4]=past[4]+u[tt,4]; 
      }
      Y=Y[-1,]
      
      reg1=lm(Y[2:T,1]~Y[1:(T-1),1])
      reg2=lm(Y[2:T,2]~Y[1:(T-1),2])
      reg3=lm(Y[2:T,3]~Y[1:(T-1),3])
      reg4=lm(Y[2:T,4]~Y[1:(T-1),4])
      
      
      residual=matrix(NA,nrow = T-1,ncol=4)
      residual[,1]=reg1$residuals
      residual[,2]=reg2$residuals
      residual[,3]=reg3$residuals
      residual[,4]=reg4$residuals
      
      X=residual

    }
    #Non linear temporal past effects
    #STVAR -- with Kalmanu filter
    #time-varying
    #time-varying with Structural Time series model with linear Gaussian signal and nonlinear shocks
    #GAS 
    if(linear=="NL+NL"){
      trueB <- randomB(trueG,0.1,2,TRUE)
      X <- sampleDataFromG(T+1,trueG,funcType="GAM", parsFuncType=list(kap=1,sigmax=1,sigmay=1,output=FALSE), 
                           noiseType="normalRandomVariances", parsNoise=list(noiseExp=1,varMin=1,varMax=2))
      X <- as.matrix(X)
      #is the contemporaneous effect noise
      
      
      Y=matrix(NA,T+1,p)
      u= X
      Y_0=c(0.5, 0, -0.5,0.25); Y[1,]=Y_0;
      PI_1=diag(runif(p))
      past=u*0
      
      f_past1=function (x) sqrt(abs(x))
      f_past2=function (x) cos(x^3)
      f_past3=function (x) sin(x)
      f_past4=function (x) sin(x^2)
      
      for (tt in 2:(T+1)){
        
 
        
        past[tt,1]=f_past1(Y[tt-1,1])
        past[tt,2]=f_past2(Y[tt-1,2])
        past[tt,3]=f_past3(Y[tt-1,3])
        past[tt,4]=f_past4(Y[tt-1,4])
        
        Y[tt,1]=past[tt,1]+u[tt,1] 
        Y[tt,2]=past[tt,2]+u[tt,2];  
        Y[tt,3]=past[tt,3]+u[tt,3];  
        Y[tt,4]=past[tt,4]+u[tt,4];  
      }
      Y=Y[-1,]
    
      residual=matrix(NA,nrow = T-1,ncol=4)
      auxY=Y
      
      for (ii in 1:4){
        X=auxY[1:(T-1),ii]
        Y=auxY[2:T,ii]
        X=as.matrix(X)
        Y=as.matrix(Y)
        
        
        # if (flag_gam==1){
          GAM<-gam(Y~s(X))
          u_fit=residuals(GAM)
          residual[,ii]=u_fit
        # }else{
        #   my_options=gpOptions("ftc")
        #   my_options$kern$comp=list("rbf","white")
        #   my_options$learnScales=TRUE
        #   model_GP=gpCreate(dim(X)[2],1,X,Y,my_options)
        #   y2<-gpOut(model_GP,X)
        #   model_GP$Yfit<-y2
        #   model_GP$residuals<-Y-y2
        #   
        #   residual[,ii]=model_GP$residuals
        # }
      }
      
 
      
      X=residual
      
    }
    
    if(linear=="STVAR"){
    
     
      trueB <- randomB(trueG,0.1,2,TRUE)
      X <- sampleDataFromG(T+1,trueG,funcType="GAM", parsFuncType=list(kap=1,sigmax=1,sigmay=1,output=FALSE), 
                          noiseType="normalRandomVariances", parsNoise=list(noiseExp=1,varMin=1,varMax=2))
      X <- as.matrix(X)
      #is the contemporaneous effect noise
      
      #X = cbind(rnorm(T+1),0.75*rnorm(T+1),0.25*rnorm(T+1),0.5*rnorm(T+1))
      
      Y=matrix(NA,T+1,p)
      u = X
      Y_0=c(0.5, 0, -0.5,0.25); Y[1,]=Y_0;
      PI_1=diag(runif(p))
      past=u*0
      
 
      
      theta=-0.5; c=-0.5*T#-1.5;#try 0
      
      if (pars$flag_st_function=='exp') {
        F<-function(z) 1-exp(-theta*( (z-c)^2)  )
      } else {
        F<-function(z) 1/(1+exp( -theta*(z-c) )  )
      }
      
      B_1=diag(runif(p,min=-1,max=1))
      B_2=diag(runif(p,min=-1,max=1))
      
 
      z=1:T;
      for (tt in 2:(T+1)){
        
        
        
        past=(1-F(z[tt-1]))*B_1%*%Y[tt-1,]+F(z[tt-1])*B_2%*%Y[tt-1,]
        
         
        
        Y[tt,1]=past[1]+u[tt,1] 
        Y[tt,2]=past[2]+u[tt,2];  
        Y[tt,3]=past[3]+u[tt,3];  
        Y[tt,4]=past[4]+u[tt,4]; 
      }
      Y=Y[-1,]
       
      residual=matrix(NA,nrow = T-1,ncol=4)
      auxY=Y
      N=p
      model_init=SSModel(auxY[ 2:T,]~  SSMregression(~auxY[ 1:(T-1),],
                                                     Q = diag(N)+diag(NA,N),type="common",remove.intercept=TRUE),
                         H = diag(N)+diag(NA,N))
      for (nn in 1:(T-1)){
        model_init$Z[,5,nn]=   c(model_init$Z[1,5,nn],0,0,0)
        model_init$Z[,6,nn]=   c(0,model_init$Z[2,6,nn],0,0)
        model_init$Z[,7,nn]=   c(0,0,model_init$Z[3,7,nn],0)
        model_init$Z[,8,nn]=   c(0,0,0,model_init$Z[4,8,nn])
      }
      
      model_init$T[5:8,5:8,1]=diag(NA,N)
      
      
      ownupdatefn <- function(pars,model){
        model$T[5:8,5:8,1]=diag(      1/(1+exp(- pars[1:4] ) ),N); 
        model$Q[,,1] <-diag( exp(pars[5:8]),N)
        model$H[,,1] <-diag( exp(pars[9:12]))
        model
      }
      
      model_exa <- fitSSM(model_init, inits = c(0.5,0.5,0.5,0.5, 
                                                log(c(0.25,0.25,0.25,0.25,rep(0.5/10,N)))),
                          updatefn = ownupdatefn, method='BFGS')$model
      out_model=KFS(model_exa)
      
      residual=out_model$v
      
      
      
      
      X=residual
      
    }
    
    if(linear=="TV-VAR"){
      trueB <- randomB(trueG,0.1,2,TRUE)
      X <- sampleDataFromG(T+1,trueG,funcType="GAM", parsFuncType=list(kap=1,sigmax=1,sigmay=1,output=FALSE), 
                           noiseType="normalRandomVariances", parsNoise=list(noiseExp=1,varMin=1,varMax=2))
      X <- as.matrix(X)
      #is the contemporaneous effect noise
      
      
      Y=matrix(NA,T+1,p)
      u= X
      Y_0=c(0.5, 0, -0.5,0.25); Y[1,]=Y_0;
      PI_1=diag(runif(p))
      past=u*0
      
      N=p
      Z=diag(N)
      TT=0.2*diag(N)
      
      
      
      eta=MASS::mvrnorm(T+1, mu = matrix(0,1,N), Sigma =0.005*diag(N))
      alfa=eta*0
      aux_alfa=c(0.5,0.4,0.3,0.2)
      for (tt in 2:(T+1)){
        alfa[tt,]=TT%*%alfa[tt-1,]+eta[tt-1,]+aux_alfa
        
        theta=Z%*%alfa[tt,]
        #
        A_TV_VAR=diag(c(theta))
        
        
        Y[tt,]=A_TV_VAR%*%Y[tt-1,]+u[tt,]
      }
      Y=Y[-1,]
      
      residual=matrix(NA,nrow = T-1,ncol=4)
      auxY=Y
  
      model_init=SSModel(auxY[ 2:T,]~  SSMregression(~auxY[ 1:(T-1),],
                                                     Q = diag(N)+diag(NA,N),type="common",remove.intercept=TRUE),
                         H = diag(N)+diag(NA,N))
      for (nn in 1:(T-1)){
        model_init$Z[,5,nn]=   c(model_init$Z[1,5,nn],0,0,0)
        model_init$Z[,6,nn]=   c(0,model_init$Z[2,6,nn],0,0)
        model_init$Z[,7,nn]=   c(0,0,model_init$Z[3,7,nn],0)
        model_init$Z[,8,nn]=   c(0,0,0,model_init$Z[4,8,nn])
      }
      
      model_init$T[5:8,5:8,1]=diag(NA,N)
      
      
      ownupdatefn <- function(pars,model){
        model$T[5:8,5:8,1]=diag(     pars[1],N); 
        model$Q[,,1] <-diag( exp(pars[2:5]),N)
        model$H[,,1] <-diag( exp(pars[6:9]))
        model
      }
      
      model_exa <- fitSSM(model_init, inits = c(0.5, 
                                                log(c(0.25,0.25,0.25,0.25,rep(0.5/10,N)))),
                          updatefn = ownupdatefn, method='BFGS')$model
      out_model=KFS(model_exa)
      
      residual=out_model$v
      
      
      
      
      X=residual
      
    }
    
    if(linear=="GR-TV-VAR"){
      trueB <- randomB(trueG,0.1,2,TRUE)
      X <- sampleDataFromG(T+1,trueG,funcType="GAM", parsFuncType=list(kap=1,sigmax=1,sigmay=1,output=FALSE), 
                           noiseType="normalRandomVariances", parsNoise=list(noiseExp=1,varMin=1,varMax=2))
      X <- as.matrix(X)
      #is the contemporaneous effect noise
      
      A_VAR=diag(c(runif(p,min=0,max=0.9)))
      
      Y=matrix(NA,T+1,p)
      u= X
      Y_0=c(0.5, 0, -0.5,0.25); Y[1,]=Y_0;
      PI_1=diag(runif(p))
      past=u*0
      
      N=p
      Z=diag(N)
      TT=0.8*diag(N)
      
      
     
      eta=MASS::mvrnorm(T+1, mu = matrix(0,1,N), Sigma =0.05*diag(N))
      alfa=eta*0
      EPS=eta*0
      
      for (tt in 2:(T+1)){
        alfa[tt,]=TT%*%alfa[tt-1,]+eta[tt-1,]
        theta=Z%*%alfa[tt,]
        #
        
        EPS[tt,]=theta+u[tt,] 
        
        
        Y[tt,]=A_VAR%*%Y[tt-1,]+EPS[tt,]
      }
      Y=Y[-1,]
      
      residual=matrix(NA,nrow = T-1,ncol=4)
      
      reg1=lm(Y[2:T,1]~Y[1:(T-1),1])
      reg2=lm(Y[2:T,2]~Y[1:(T-1),2])
      reg3=lm(Y[2:T,3]~Y[1:(T-1),3])
      reg4=lm(Y[2:T,4]~Y[1:(T-1),4])
      
      auxY=residual;
      
      auxY[,1]=reg1$residuals
      auxY[,2]=reg2$residuals
      auxY[,3]=reg3$residuals
      auxY[,4]=reg4$residuals
      
      model_init=SSModel(auxY ~  SSMtrend(1, Q = diag(N)+diag(NA,N)),H = diag(N)+diag(NA,N))
      
      model_init$T[,,1]=diag(N)+diag(NA,N)
      
      ownupdatefn <- function(pars,model){
        model$T[1]=pars[1]; model$T[6]=pars[2]; model$T[11]=pars[3];  model$T[16]=pars[4]
        model$Q[,,1] <-diag( exp(pars[5:8]),N)
        model$H[,,1] <-diag( exp(pars[9:12]))
        model
      }
      
      model_exa <- fitSSM(model_init, inits = c(0.5,0.5,0.5,0.5,log(c(0.5,0.5,0.5,0.5,rep(0.5/10,N)))),
                          updatefn = ownupdatefn, method='BFGS')$model
      out_model=KFS(model_exa)
      
      residual=out_model$v
      
      
      
      
      X=residual
      
    }
    
    if(linear=="GR-TV-VAR-GAS"){
      trueB <- randomB(trueG,0.1,2,TRUE)
      X <- sampleDataFromG(T+1,trueG,funcType="GAM", parsFuncType=list(kap=1,sigmax=1,sigmay=1,output=FALSE), 
                           noiseType="normalRandomVariances", parsNoise=list(noiseExp=1,varMin=1,varMax=2))
      X <- as.matrix(X)
      #is the contemporaneous effect noise
      
      A_VAR=diag(c(runif(p,min=0,max=0.9)))
      
      Y=matrix(NA,T+1,p)
      u= X
      Y_0=c(0.5, 0, -0.5,0.25); Y[1,]=Y_0;
      PI_1=diag(runif(p))
      past=u*0
      
      N=p
      Z=diag(N)
      TT=0.25*diag(N)
      
      
      Q_level=c(0.25,0.5,0.1,0.2)*diag(N)/10
      eta=MASS::mvrnorm(T+1, mu = matrix(0,1,N), Sigma =Q_level)
      alfa=eta*0
      EPS=eta*0
      
      for (tt in 2:(T+1)){
        alfa[tt,]=TT%*%alfa[tt-1,]+eta[tt-1,]
        theta=Z%*%alfa[tt,]
        #
        
        EPS[tt,]=theta+u[tt,] 
        
        
        Y[tt,]=A_VAR%*%Y[tt-1,]+EPS[tt,]
      }
      Y=Y[-1,]
      
      residual=matrix(NA,nrow = T-1,ncol=4)
      
      reg1=lm(Y[2:T,1]~Y[1:(T-1),1])
      reg2=lm(Y[2:T,2]~Y[1:(T-1),2])
      reg3=lm(Y[2:T,3]~Y[1:(T-1),3])
      reg4=lm(Y[2:T,4]~Y[1:(T-1),4])
      
      auxY=residual;
      
      auxY[,1]=reg1$residuals
      auxY[,2]=reg2$residuals
      auxY[,3]=reg3$residuals
      auxY[,4]=reg4$residuals
      
      GASSpec = MultiGASSpec(Dist = "mvnorm", ScalingType = "Identity", 
                             GASPar = list(location    = TRUE, scale = FALSE,
                                           correlation = FALSE,  shape = FALSE),
                             ScalarParameters = TRUE)
      
      
      
      Fit_FULL = MultiGASFit(GASSpec, auxY)
      
      
      
      mY = Fit_FULL@Data$mY
      iT = ncol(mY)
      
      lMoments = getMoments(Fit_FULL)
      mMean = lMoments$mean[1:iT, ]
      
      mRes     = t(mY) - mMean
      residual=mRes
      
      
      
      
      X=residual
      
    }
    SigmaHat <- cov(X)
    
    
    
    
    # RESIT (in the code also called ICML)
    if(linear=="VAR+NonGauss")
    {
      resICML <-ICML(X, alpha = 0.05, model = "linear", parsModel = list(), 
                     indtest = dhsic.test, 
                     parsIndtest = list(method = "ExactFastTrace"), confounder_check = 0, output = FALSE)
    }else{
      resICML <-ICML(X, alpha = 0.05, model = "GP", parsModel = list(), 
                     indtest = dhsic.test, 
                     parsIndtest = list(method = "ExactFastTrace"), confounder_check = 0, output = FALSE)
    }

    
     
    
    res[1] <- structIntervDist(trueG, resICML)$sid
    res[2] <- hammingDistance(trueG, resICML)
    
    
    
    # LINGAM
    resLINGAM <- lingamWrap(X)$Adj
    res[3] <- structIntervDist(trueG, resLINGAM*1)$sid
    res[4] <- hammingDistance(trueG, resLINGAM*1)

    # PC
    resPC <- pcWrap(X, 0.01, mmax = Inf)
    res[5] <- structIntervDist(trueG, resPC)$sid
    res[6] <- hammingDistance(trueG, resPC)


    # cPC
    rescPC <- cpcWrap(X, 0.01, mmax = Inf)
    res[7] <- structIntervDist(trueG, rescPC)$sid
    res[8] <- hammingDistance(trueG, rescPC)
     
    # BF
    #  setwd("../../codeANM/code/experiments/ANM/")
    # tic()
    # pars <- list(regr.method = train_linear, 
    #          regr.pars = list(), 
    #          indtest.method = dhsic.test, indtest.pars = list())
    # resBFtmp <- BruteForce(X , "SEMIND", pars , output = FALSE)
    # toc()
    # setwd("../../../../code/simulations_comparison/")
    resBF <- rescPC*0
    res[9] <- structIntervDist(trueG, resBF*1)$sid
    res[10] <- hammingDistance(trueG, resBF*1)


   #CAM
   resCAM <- CAM(X, scoreName = "SEMGAM", 
              parsScore = list(numBasisFcts=10),   
              maxNumParents = min(dim(X)[2] - 1, round(dim(X)[1]/20)),
              variableSel = TRUE, 
              variableSelMethodPars = list(atLeastThatMuchSelected = 0.02, atMostThatManyNeighbors = 10),
              pruning = TRUE) 
     resCAM = resCAM$Adj
    res[11] <- structIntervDist(trueG, resCAM)$sid
    res[12] <- hammingDistance(trueG, resCAM)

       


    #RANDOM
    resRand <- as.matrix(randomDAG(p,runif(1)))
    res[13] <- structIntervDist(trueG, resRand)$sid
    res[14] <- hammingDistance(trueG, resRand)      
    
    

    
    return(res)
}
