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
        Y_0=rnorm(p)*0; Y[1,]=Y_0;
        PI_1=diag(runif(p))
        for (tt in 2:(T+1)){
          
          past=PI_1%*%Y[tt-1,]
          for (pp in 1:p){
            Y[tt,pp]=past[pp]+u[tt,pp] 
          }
          
        }
        Y=Y[-1,]
        reg  = list()
        residual=matrix(NA,nrow = T-1,ncol=p)
        
        for (pp in 1:p){
          reg = lm(Y[2:T,pp]~Y[1:(T-1),pp])
          residual[,pp]=reg$residuals
        }
        
        
 
        
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
      Y_0=rnorm(p)*0; Y[1,]=Y_0;
      PI_1=diag(runif(p))
      for (tt in 2:(T+1)){
        
        past=PI_1%*%Y[tt-1,]
        for (pp in 1:p){
          Y[tt,pp]=past[pp]+u[tt,pp] 
        }
        
      }
      Y=Y[-1,]
      reg  = list()
      residual=matrix(NA,nrow = T-1,ncol=p)
      
      for (pp in 1:p){
        reg = lm(Y[2:T,pp]~Y[1:(T-1),pp])
        residual[,pp]=reg$residuals
      }
      
      
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
