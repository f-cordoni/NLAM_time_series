
simul.distri <- function(distri,nb.sim){
  # Simulation of independent shocks
  eps <- NULL
  for(i in 1:length(distri$type)){
    if(distri$type[i]=="gaussian"){
      eps.i <- rnorm(nb.sim)
    }else if(distri$type[i]=="mixt.gaussian"){
      p <- distri$p[i]
      mu.1 <- distri$mu[i]
      sigma.1 <- distri$sigma[i]
      mu.2 <- - p/(1-p)*mu.1
      sigma.2 <- sqrt( 1/(1-p) * (1 - p*sigma.1^2 - p/(1-p)*mu.1^2) )
      B <- (runif(nb.sim)<p)*1
      eps.i <- B*rnorm(nb.sim,mean = mu.1,sd = sigma.1) +
        (1-B)*rnorm(nb.sim,mean = mu.2,sd = sigma.2)
    }else if(distri$type[i]=="student"){
      nu <- distri$df[i]
      eps.i <- rt(nb.sim,df = nu)/sqrt(nu/(nu-2))
    }else if(distri$type[i]=="laplace"){
      U <- runif(nb.sim) - .5
      b <- 1/sqrt(2)
      eps.i <- - b * sign(U) * log(1 - 2 * abs(U))
    }else if(distri$type[i]=="hyper.sec"){
      U <- runif(nb.sim)
      eps.i <- 2/pi * log(tan(pi/2*U))
    }
    eps <- cbind(eps,eps.i)
  }
  return(eps)
}
