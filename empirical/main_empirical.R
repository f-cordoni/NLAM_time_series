require(vars)
require(tsDyn)
require(starvars)

get_st <- function(given_ts, variable_selected=2){
  # given the time series of the data at time t get the st time-series
  st <- given_ts[-nrow(given_ts), variable_selected] # select the second variable lagged of 1 period
  # st <- st*0+1:nrow(st)
  return(st)
}

if (flag_model == "VAR"){
  print("VAR")
  #estimate the VAR model
  #select VAR order

  
  print(VARselect(DATA)$selection)
  # looking at AIC (BIC)  lag  = 6
  #
  if (!fix_lag){
    lag = VARselect(DATA)$selection[1]
  }
  print("lag")
  print(lag)
  

  model_var = lineVar(DATA,lag = lag,include = "const")
  
  #get the matrices 
  A = list()
  A[[lag+1]] = model_var$coefficients[,1]
  model_var$coefficients  = model_var$coefficients[,-1]
  for (ii in 1:lag){
    A[[ii]] = matrix(0,nrow = 3,ncol = 3)
    
    A[[ii]]   = model_var$coefficients[,((ii-1)*3+1):(ii*3)]# 4:6 # 7 : 9
    
  }
  #interecept is the last entry in A list
  
  residual =(model_var$residuals)
  Omega = cov(residual)
  residual_orig = residual
  
  #launch RESIT
  
}

if (flag_model == "STVAR"){
  print("STVAR")
  # auxiliart function for generating stvar
  simulate_stvar_model <- function(fit.VLSTAR, T, p, y0, variable_selected=2){
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
      s_t_fitted = get_st(given_ts = y_fitted[1:tt,], variable_selected=variable_selected) 
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
  
  #estimate the stvar model
  
  
  start_date = as.Date("1954-03-01")
  end_date <- as.Date("2019-12-01")
  
  # Create a sequence of dates at quarterly frequency
  date_sequence <- seq(start_date, end_date, by = "3 months")
  
  auxDATA =  zoo(DATA, order.by = date_sequence) 
  
  y <-auxDATA[-1, ]
  #  y <- Realized[-1,1:3]
  # compute the transition variable
  #st <- auxDATA[-1, 1]
  st = get_st(given_ts = auxDATA, variable_selected=2) # selection of o

  # number of regimes 
  m = 2
  
  # number of lags
  p = 1
  require(parallel)
  ncores = detectCores(all.tests = FALSE, logical = TRUE)-1
  
  stvalues <- startingVLSTAR(y, p = p, n.combi = 3,
                             singlecgamma = FALSE, st = st, ncores = 1, m=m)

  
  fit.VLSTAR <- VLSTAR(y, p = p, singlecgamma = FALSE, starting = stvalues,
                       n.iter = 1, st = st, method ='NLS', ncores = 10, m=m)
  # a few methods for VLSTAR
  #summary(fit.VLSTAR)
  #
  
  #coef(fit.VLSTAR)
  G_over_time = matrix(0, nrow = nrow(st), ncol=ncol(y))
  Gamma_c = fit.VLSTAR$Gammac
  for (tt in 1:nrow(st)){
    # s_t_fitted = get_st(given_ts = auxDATA[1:tt,]) 
    # s_t_fitted = tail(s_t_fitted,1) # considering the last value
  
    # compute G_matrix
    G_ = matrix(0, nrow=ncol(st), ncol = 1)
    for (i_var in 1:ncol(y)){
      G_[i_var] =  (1+exp(-Gamma_c[i_var, 1]*(st[tt]- Gamma_c[i_var, 2])))^-1
    }
    G_over_time[tt, ] = G_
  }
  

  
  
  residual =residuals(fit.VLSTAR)
  Omega = cov(residual)
  residual_orig = residual
  
  plot.ts(G_over_time)
  plot(fit.VLSTAR)
  
  # plot the G over time
  
  fit.VLSTAR$Gtilde
}

