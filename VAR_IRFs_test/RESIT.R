indtestAll <- function(f,x,y,alpha,pars = list())
{
result<-dhsic.test(x,y,pairwise=FALSE,method = "gamma",kernel = "gaussian")

}

train_model <- function(flag,X,y,pars = list())
{
switch(flag, 
linear={
    mod <- lm(y ~ X)
    result <- list()
    result$Yfit = as.matrix(mod$fitted.values)
    result$residuals = as.matrix(mod$residuals)
    result$model = mod
     return(result)
},
GP={
  options=gpOptions("ftc")
    options$kern$comp=list("rbf","white")
    #options$learnScales=TRUE
    model<-gpCreate(dim(X)[2],1,X,y,options)
    y2<-gpOut(model,X)
    model$Yfit<-y2
    model$residuals<-y-y2
    return(model)
},
GAM={
library(mgcv)
numBasisFcts = 1

  
        pars$numBasisFcts = 1
 
    p <- dim(as.matrix(X))
    if(p[1]/p[2] < 3*pars$numBasisFcts)
    {
        pars$numBasisFcts <- ceiling(p[1]/(3*p[2]))
        cat("changed number of basis functions to    ", pars$numBasisFcts, "    in order to have enough samples per basis function\n")
    }
    dat <- data.frame(as.matrix(y),as.matrix(X))
    coln <- rep("null",p[2]+1)
    for(i in 1:(p[2]+1))
    {
     
        coln[i] <- paste("var",i,sep="")
    }
    colnames(dat) <- coln
    labs<-"var1 ~ "
    if(p[2] > 1)
    {
        for(i in 2:p[2])
        {
            #labs<-paste(labs,"s(var",i,",k = ",pars$numBasisFcts,") + ",sep="")
                   labs<-paste(labs,"s(var",i,") + ",sep="")
            # labs<-paste(labs,"lo(var",i,") + ",sep="")
        }
    }
    #labs<-paste(labs,"s(var",p[2]+1,",k = ",pars$numBasisFcts,")",sep="")
     labs<-paste(labs,"s(var",p[2]+1,")",sep="")
    # labs<-paste(labs,"s(var",p[2]+1,", bs = "cc")",sep="") #factor 2 faster
    # labs<-paste(labs,"s(var",p[2]+1,", bs = "cr")",sep="") # factor 2 + eps faster
    # labs<-paste(labs,"lo(var",p[2]+1,")",sep="")
    mod_gam <- FALSE
    try(mod_gam <- gam(formula=formula(labs), data=dat),silent = TRUE)
    if(typeof(mod_gam) == "logical")
    {
        cat("There was some error with gam. The smoothing parameter is set to zero.\n")
        labs<-"var1 ~ "
        if(p[2] > 1)
        {
            for(i in 2:p[2])
            {
                labs<-paste(labs,"s(var",i,",k = ",pars$numBasisFcts,",sp=0) + ",sep="")
            }
        }
        labs<-paste(labs,"s(var",p[2]+1,",k = ",pars$numBasisFcts,",sp=0)",sep="")
        mod_gam <- gam(formula=formula(labs), data=dat)
    }
    result <- list()
    result$Yfit <- as.matrix(mod_gam$fitted.values)
    result$residuals <- as.matrix(mod_gam$residuals)
    result$model <- mod_gam 
    result$df <- mod_gam$df.residual     
    result$edf <- mod_gam$edf     
    result$edf1 <- mod_gam$edf1     
    
    # for degree of freedom see mod_gam$df.residual
    # for aic see mod_gam$aic
    return(result)
},
print('Error 42')
)
    
}

fit_and_test_independence <- function(x,y,z,alpha,model,parsModel = list(),indtest, parsIndtest)
    # fits x using y and tests against z
{
    y <- as.matrix(y)
    z <- as.matrix(z)
    x<-as.matrix(x)
    ####
    # fit x using y
    ####
    mod_fit <- train_model(model,y,x,parsModel)
    r2 <- mod_fit$residuals
    
    Tquan <- indtestAll(indtest,z,r2,alpha,parsIndtest)
    return(Tquan)
}


ICML <- function(M, alpha = 0.05, model = train_linear, parsModel = list(), indtest = indtestHsic, parsIndtest = list(method = "ExactFastTrace"), confounder_check = 0, output = FALSE)
{
    #M contains the data (each col one component)
    #confounder_check indicates subsets of which size the method tries to omit if it doesn't find any possible sink node
    stopping <- 1
    p <- dim(M)[2]
    C <- matrix(0,p,p)
    err <- matrix(0,p,1)
    S <- 1:p
    par <- matrix(0,p-1,p-1)
    parlen <- rep(0,p-1)
    variable <- rep(0,p-1)
    indtest_at_end <- rep(0,p-1)
    d <- 0
    while(length(S)>1)
    {
        #show(variable)
        d <- d+1
        # check is a vector. the k-th entry < 0 says that making the k-th variable to a sink node leads to independent residuals. 
        check <- rep(0,length(S))
        #show(S)
        for(k in 1:length(S))
        {
            i <- S[k]
            S_new <- S
            S_new <- S_new[-c(k)]
            if(output)
            {
                print(paste("fit",i,"with the help of", paste(S_new, collapse=" "),"..."))  
            }
            Fc <- fit_and_test_independence(M[,i],M[,S_new],M[,S_new],alpha,model,parsModel,indtest, parsIndtest)
            #check[k] <- Fc$statistic - Fc$crit.value
            check[k] <- -Fc$p.value
            if(output)
            {
                if(check[k]>-alpha)
                {
                #    print(paste("Independence rejected: test statistic - critical value =",check[k]))
                    print(paste("Independence rejected: p-value =",-check[k]))
                }
                else
                {
                #    print(paste("Independence not rejected: test statistic - critical value =",check[k]))
                    print(paste("Independence not rejected: p-value =",-check[k]))
                }
            }
        }
 
        bb <- which.min(check)
        variable[d] <- S[bb]
        S <- S[-c(bb)]
        parlen[d] <- length(S)
        par[d,1:length(S)] <- S
        if(output)
        {
            print(paste("Possible sink node found:",variable[d]))
            print(paste("causal order (beginning at sink):",paste(variable,collapse=" ")))
        }	
        #         }
        rm(check)
    }
    # show(variable)
    if(d<p)
    {
        variable[p]<-S[1]
    }
    if(output)
    {   
        print(paste("causal order (beginning at sink):",paste(variable,collapse=" ")))
        print(paste("removing unnecessary edges..."))
    }
    rm(S)
    #todo: here, we take the first possible parent away (not the best one). in theory it probably doesn't make a difference. experiments in paper done correctly. but maybe finding all dags is better.
    for(d in 1:(p-1))
    {
        if(err[d] != 1)
        {
            S<-par[d,1:parlen[d]]
            for(i in 1:length(S))
            {
                S_new<-S
                S_new<-S_new[-c(1)]
                if(length(S)==1)
                {
                    tsx <- M[,variable[d]]
                    # todo: test against par[d,1:parlen[d]] or S????
                    Fc <- indtestAll(indtest,M[,par[d,1:parlen[d]]],tsx,alpha,parsIndtest)
                }
                else
                {
                    # todo: test against par[d,1:parlen[d]] or S????
                    Fc <- fit_and_test_independence(M[,variable[d]],M[,S_new],M[,par[d,1:parlen[d]]],alpha,model,parsModel,indtest, parsIndtest)
                }
                if(Fc$statistic < Fc$crit.value)
                {
                    S <- S_new
                }
                else
                {
                    if(length(S)>1)
                    {
                        tmp <- S[1]
                        S[1:(length(S)-1)] <- S[2:length(S)]
                        S[length(S)] <- tmp
                    }
                }
            }
            
            #todo: always hsic?
            #todo: change code...
            if(1==0)
            {     
            ####### not RUN            
                print(paste("...and performing final independence test using HSIC..."))
                if(length(S)>0)
                    #if(indtest != indtestts_hsic & length(S)>0)
                {
                    print(paste("fitting ", variable[d], " with the help of ", paste(S,collapse=" "), " and testing independence against ", paste(par[d,1:parlen[d]],collapse=" ")))
                    Fc <- fit_and_test_independence(M[1:p[1],variable[d]],M[1:p[1],S],M[1:p[1],par[d,1:parlen[d]]],alpha/(p-1),max_lag,model,indtest,instant)
                }
                else
                {
                    print(paste("fitting ", variable[d], " with the help of NOTHING and testing independence against ", paste(par[d,1:parlen[d]],collapse=" ")))
                    tsx <- M[1:p[1],variable[d]]
                    pars <- list(maxOrder = max_lag)
                    modx <- traints_model(model,tsx,list(),pars)
                    resx <- modx$residuals[(modx$model$order+1):length(tsx)]
                    Fc <- indtest_model(indtest,M[1:p[1],par[d,1:parlen[d]]],resx,alpha,max_lag,FALSE)
                }
                show(sprintf("Test statistic: %.3f and critical value: %.3f and p-value %.2e", Fc[1],Fc[2],Fc[3]))        
                indtest_at_end[d] <- sign(Fc[1]-Fc[2])
             ####### not RUN            
            }
            else
            {
                #-1: ind., +1 dep.
                indtest_at_end[d] <- -1
            }
            parlen[d] <- length(S)
            C[S,variable[d]] <- rep(1,length(S))
        }
        else
        {
            #-1: ind., +1 dep.
            indtest_at_end[d] <- -1
        }
    }
    if(max(indtest_at_end)<0)
    {
        if(output)
        {
            print(paste("all correct..."))
        }
        return(C)
    }
    else
    {
        print(paste("final ind. test failed. No solution."))
        return(NULL)
    }
}
