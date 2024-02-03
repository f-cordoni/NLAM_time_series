
# change here your Dir
InDir = "~/Dropbox/post-doc/non-linearity/code/simulations_comparison/empirical"
setwd(InDir)

# data quarterly con GMR application without oil price exogenous

flag_monthly = 0
setwd("data/")
flag_applicatin_fed_fund = 0
source("data_pre_processing.R")
setwd("../")


DATA = DATA_TS 
plot.ts(DATA)
#demean the data
scales = apply(DATA,2,sd)
DATA = scale(DATA, center = FALSE, scale = FALSE)
varnames = c("infl","output","r")
plot.ts(DATA)

DATA = DATA[1:262,]
#1953 Q3 : 2019 Q4
aux_plot_data = ts( DATA , start = c(1954,3),frequency = 4)

# install.packages("latex2exp")
library(latex2exp)

colnames(aux_plot_data) = c("pi","o","r")
plot.ts(aux_plot_data, main = "", ylabel= c("pi","o","r"))
par(mfrow=c(3,1), mai=c(0.4,0.65,0.4,0.2), cex.lab = 1.5)
plot.ts(x = aux_plot_data[,1],ylab= TeX("$\\pi$"), main="", xlab="")
plot.ts(x = aux_plot_data[,2],ylab= "o", main="", xlab="",  cex.lab = 2)
plot.ts(x = aux_plot_data[,3],ylab= "r", main="",  xlab="",  cex.lab = 2)

# estimate the TS model, VAR or STVAR model
# flag_model = "VAR"
flag_model = "STVAR"
colnames(DATA) = colnames(aux_plot_data)
DATA = ts( DATA , start = c(1954,3),frequency = 4)

require(vars)
require(tsDyn)
require(starvars)

get_st <- function(given_ts){
  # given the time series of the data at time t get the st time-series
  st <- given_ts[-nrow(given_ts), 2] # select the second variable lagged of 1 period
  # st <- st*0+1:nrow(st)
  return(st)
}


start_date = as.Date("1954-03-01")
end_date <- as.Date("2019-12-01")

# Create a sequence of dates at quarterly frequency
date_sequence <- seq(start_date, end_date, by = "3 months")

auxDATA =  zoo(DATA, order.by = date_sequence) 

y <-auxDATA[-1, ]
#  y <- Realized[-1,1:3]
# compute the transition variable
#st <- auxDATA[-1, 1]
st = get_st(given_ts = auxDATA)

# number of regimes 
m = 2

# number of lags
p = 1

stvalues <- startingVLSTAR(y, p = p, n.combi = 3,
                           singlecgamma = FALSE, st = st, ncores = 5, m=m)


fit.VLSTAR <- VLSTAR(y, p = p, singlecgamma = FALSE, starting = stvalues,
                     n.iter = 1, st = st, method ='NLS', ncores = 10, m=m)



plot(fit.VLSTAR)
summary(fit.VLSTAR)

simulate_stvar_model <- function(fit.VLSTAR, T, p, y0){
  # then we have to simulate this model from the residual
  # T observation to simulate
  # p number of lags
  
  # y0 initial vector
  # the model is the following
  #
  # Y-t = mu_0 + Sum_{i=1}^p  Phi_i,0 Y_{t-i}  +  G_t (mu_1 + Sum_{i=1}^p  Phi_1,0 Y_{t-i}) + eps_t
  
  # however the VLSTAR estimate the matrices of 
  #regime =0 when G_t == 0 and regime =1 when G_t == 1 and
  # so it estimates 
  #for regime =0  mu_0,  Phi_i,0
  #for regime =1  mu_0+mu_1,  Phi_i,0+Phi_i,1  
  
  # so to recover Phi_1 we need to takes the difference Regime_1 -Regime_0
  
  mu_regime <- fit.VLSTAR$Bhat[grep("const", rownames(fit.VLSTAR$Bhat)), ]
  
  Phi_regime0 = fit.VLSTAR$Bhat[grep("m_1", rownames(fit.VLSTAR$Bhat)), ]
  Phi_regime0 = Phi_regime0[-1,] # discard the constant
  Phi_regime1 = fit.VLSTAR$Bhat[grep("m_2", rownames(fit.VLSTAR$Bhat)), ]
  Phi_regime1 = Phi_regime1[-1,] # discard the constant
  
  Phi1 = Phi_regime1-Phi_regime0
  Phi0 = Phi_regime0
  Phi0 = t(Phi0) # for multiplication
  Phi1 = t(Phi1)
  mu_0 = mu_regime[1,]
  mu_1 = mu_regime[2,]-mu_regime[1,]
  
  # collect the location and the inverse of scale (called gamma)
  Gamma_c = fit.VLSTAR$Gammac
  
  Nvar = ncol(Phi_regime0)
  y_fitted = matrix(0, nrow = T, ncol = Nvar) 
  y_fitted[1,] = c(y0) # first observation
  for (tt in (p+1):T){
    aux_lag0 = 0 # mu_0 + Sum_{i=1}^p  Phi_i,0 Y_{t-i}
    aux_lag1 = 0 # mu_1 + Sum_{i=1}^p  Phi_i,1 Y_{t-i}
    for(ii_ in 1:p){
      
      
      auxPhi0 = Phi0[,(1+(ii_-1)*Nvar): (ii_*Nvar)] 
      auxPhi1 = Phi1[,(1+(ii_-1)*Nvar): (ii_*Nvar)] 
      
      # try this if you want to replicate the fitted(fit.VLSTAR)
      # aux_lag0 = aux_lag0 + auxPhi0 %*% t(y[tt-ii_,])
      # aux_lag1 = aux_lag1 + auxPhi1 %*% t(y[tt-ii_,])

      aux_lag0 = aux_lag0 + auxPhi0 %*% (y_fitted[tt-ii_,])
      aux_lag1 = aux_lag1 + auxPhi1 %*% (y_fitted[tt-ii_,])
    }
    aux_lag0 = aux_lag0+mu_0
    aux_lag1 = aux_lag1+mu_1
    # get s_t
    # get s_t using the information at time t
    # please note in get_ts we considering the lag of the argument givent_ts
    
    # try this if you want to replicate the fitted(fit.VLSTAR)
    # s_t_fitted = get_st(given_ts = y[1:tt,])#
    s_t_fitted = get_st(given_ts = y_fitted[1:tt,]) 
    s_t_fitted = tail(s_t_fitted,1) # considering the last value
    
    # compute G_matrix
    G_ = matrix(0, nrow=Nvar, ncol = 1)
    for (i_var in 1:Nvar){
      G_[i_var] =  (1+exp(-Gamma_c[i_var, 1]*(s_t_fitted- Gamma_c[i_var, 2])))^-1
    }
    
    y_fitted[tt,] = aux_lag0 + diag(c(G_)) %*% aux_lag1
  }
  
  return(y_fitted)
}

y_fitted = simulate_stvar_model(fit.VLSTAR = fit.VLSTAR, 
                                T=nrow(y),
                                y0 = y[1,], 
                                p=p)

plot.ts(y_fitted)
plot(y_fitted)
y_fitted = as.matrix(y_fitted)
y_fitted[-c(1:p), ]-fitted(fit.VLSTAR)

aux_plot = as.data.frame(y_fitted[-c(1:p),])
par(mfrow=c(3,1))
for (ii in 1:3){
  plot(aux_plot[,ii])
  lines(fitted(fit.VLSTAR)[,ii])
}















############################################################################à

# OLD CODE obsolete do not run


############################################################################à

simulate_multivariate_st_model <- function(N, n, p, gamma, c_value, mean_eps, phi0, phi1) {

  # Generate random white noise errors (εt)
  epsilon <- mvrnorm(n, mu = mean_eps, Sigma = diag(N))
  yt = matrix(0, nrow = n, ncol=N)
  
  # Simulate the rest of the series
  
  for (t in (p + 1):n) {
    # the transition variable is the first variable
    st <- 1 / (1 + exp(-gamma * yt[t - 1, 1] - c_value)) 
    yt[t, ] <- mean_eps + phi0 %*% yt[t-1, ] + 
      st * (0.1+mean_eps + phi1 %*% yt[t-1, ] )+epsilon[t, ]
  }
  
  # Create a data frame with simulated values
  simulated_data <- data.frame(yt = yt)
  
  return(simulated_data)
}

# Example usage:
set.seed(123)  # Set a random seed for reproducibility
n <- 500  # Number of observations
p <- 1    # Order of autoregressive terms

N = 3 # number of ts
random_matrix <- 
gamma <- c(0.5, 0.2, 0.1)  # Gamma parameter
c_value <- c(0.2, 0.1, 0)  # c parameter


mean_eps <- c(0, 0, 0 )  # Mean of epsilon (error term)
phi0 <- matrix(rnorm(N^2), nrow = N, ncol = N)  # Autoregressive coefficients for yt
phi1 <- matrix(rnorm(N^2), nrow = N, ncol = N)  # Autoregressive coefficients for yt in the transition part

eigenvalues <- eigen(phi0)$values
phi0 <- phi0 / max(abs(eigenvalues)+0.5)
eigenvalues <- eigen(phi1)$values
phi1 <- phi0 / max(abs(eigenvalues)+0.5)
 

simulated_series <- simulate_multivariate_st_model(N, n, p, gamma, c_value, mean_eps, phi0, phi1)

# View the simulated series
head(simulated_series)

y <- simulated_series[-p, ]
st <- simulated_series[-nrow(simulated_series), 1]

m = 2

# number of lags
p = 1

stvalues <- startingVLSTAR(y, p = p, n.combi = 3,
                           singlecgamma = FALSE, st = st, ncores = 1, m=m)


fit.VLSTAR <- VLSTAR(y, p = p, singlecgamma = FALSE, starting = stvalues,
                     n.iter = 1, st = st, method ='ML', ncores = 1, m=m)
# a few methods for VLSTAR
print(fit.VLSTAR)
summary(fit.VLSTAR)









y_fitted = y*0
y_fitted[1,] = y[1,]
for (tt in (p+1):nrow(y)){
  aux_lag0 = 0
  aux_lag1 = 0
  for(ii in 1:p){
    aux_lag0 = aux_lag0 + Phi_regime0[(1+(p-1)*Nvar): (p*Nvar),] %*% t(y[tt-ii,])
    aux_lag1 = aux_lag1 + Phi_regime1[(1+(p-1)*Nvar): (p*Nvar),] %*% t(y[tt-ii,])
  }
  aux_lag0 = aux_lag0+constant_rows[1,]
  aux_lag1 = aux_lag1+constant_rows[2,]
  # get s_t
  s_t_fitted = get_st(given_ts = y[1:tt,])
  s_t_fitted = tail(s_t_fitted, 1)
  G_ = matrix(0, nrow=Nvar, ncol = 1)
  for (i_var in 1:Nvar){
    G_[i_var] =  (1+exp(-Gamma_c[i_var, 1]*(s_t_fitted- Gamma_c[i_var, 2])))^-1
  }
  y_fitted[tt,] = aux_lag0 + diag(c(G_)) %*% aux_lag1+fit.VLSTAR$residuals[tt-1,]
}


