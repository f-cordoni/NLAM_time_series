Cond_IRFS <-function(t_star , H , k_star, delta, Nsim,
                     Omega, E, PI_hat, phi, Pa , flag_resit = 1,
                     q_alfa = 0.68, S = diag(ncol(E) ) ){
   
    
    #phase 1
    I_delta_nn = array(0,dim = c(H+1,ncol(E),Nsim))
    for (nn in 1:Nsim){
      Y_nn  = matrix(0,t_star+H, ncol(E))
      Y_nn [1:(t_star-1),] = Omega# [1:(t_star-1),]
      Y_delta_nn = Y_nn
      #
      
      E_tilde = matrix(0,H+1,ncol(E))
      for (i_col in 1:ncol(E)){
      ix_sample = sample(1:nrow(E),H+1, replace = TRUE)
      # non per row?
      E_tilde[,i_col] = E[ix_sample,i_col]
       
      }
      E_delta_tilde = E_tilde
      E_delta_tilde[t_star,k_star] = delta
      
      U = matrix(0,H+1,ncol(E))
      U_delta = U
      I_delta = U
    
      K = ncol(E)
      for (hh in 1:(H+1)){
        for (k in 1:K){
          #parents of node k
          i_parents = which(Pa[,k]==1)
          if ( length( i_parents )==0 ) {
            
             U_delta[hh,k] = E_delta_tilde[hh,k] 
             
             U[hh,k]  = E_tilde[hh,k] 

          }else{
            
            aux_pa = U_delta[hh,i_parents]
            
            if (flag_resit == 1){
            options=gpOptions("ftc")
            options$kern$comp=list("rbf","white")
            
            model_phi <- phi[[k]] 
            
            if (length(i_parents)>1){
              phi_hat<-gpOut(model_phi, t(as.matrix(aux_pa))  )
            }else{
              phi_hat<-gpOut(model_phi, as.matrix(aux_pa) )
            }
            }
            
            if (flag_resit == 2){
              #Choleski
              phi_hat  = S[k,] %*% U_delta[hh,]
            }
            
            if (flag_resit == 3){
              # true CIRFS
              if (k ==2){
                phi_hat = phi[[1]](aux_pa)
              }
              if (k ==3){
                phi_hat = phi[[2]](aux_pa[1])+ phi[[3]](aux_pa[2])
              }
            }
            
            U_delta[hh,k]  = phi_hat + E_delta_tilde[hh,k] 

            
            aux_pa = U [hh,i_parents]
            
            if (flag_resit == 1){
            if (length(i_parents)>1){
              phi_hat<-gpOut(model_phi, t(as.matrix(aux_pa))  )
            }else{
              phi_hat<-gpOut(model_phi, as.matrix(aux_pa) )
            }
            }
            
            if (flag_resit == 2){
              
              #Choleski
              phi_hat  = S[k,] %*% U[hh,]
            }
            
            if (flag_resit == 3){
              # true CIRFS
              if (k ==2){
                phi_hat = phi[[1]](aux_pa)
              }
              if (k ==3){
                phi_hat = phi[[2]](aux_pa[1])+ phi[[3]](aux_pa[2])
              }
            }
            # phi_hat <- gpOut(model_phi,as.matrix(aux_pa))
            U [hh,k] = phi_hat + E_tilde[hh,k] 

          }
          # generate time series 
            #CONTROLLARE QUI il ciclo almeno su kk deve terminare
        }
           #difference
          Y_delta_nn[t_star+hh-1, ] = PI_hat %*% Y_delta_nn[t_star+hh-2 ,] + U_delta[hh, ] 
          
          Y_nn[t_star+hh-1, ] = PI_hat %*% Y_nn[t_star+hh-2 ,] + U[hh, ] 
          I_delta [hh , ]  = Y_delta_nn[t_star+hh -1, ] - Y_nn[t_star+hh -1, ] 
      }
      I_delta_nn [,,nn] = I_delta
    }
    
    I_delta_nn_avg = I_delta*0
    I_delta_nn_up = I_delta*0
    I_delta_nn_lwr = I_delta*0
    #phase 2
    
    aux = matrix(0,Nsim, H+1)
    
      for (ii in 1:K){
        for (nn in 1:Nsim){
      aux [nn,] = I_delta_nn [,ii,nn]
        }
        I_delta_nn_avg[,ii] = colMeans(aux)
        I_delta_nn_up [,ii] = apply(aux,2,quantile,probs = q_alfa)
        I_delta_nn_lwr[,ii] = apply(aux,2,quantile,probs = 1-q_alfa)
    }
   
    # fare per SR
    # fare con true specification
   
  
  return( list(AVG = I_delta_nn_avg, LWR = I_delta_nn_lwr, UPR = I_delta_nn_up  ) )
}

get_structural_shocks_RESIT <- function(residual){
  auxY = residual
  N = ncol(auxY)
  aux_graph = matrix(0,N,N)
  
  graph_resit <- ICML(as.matrix(auxY), alpha = 0.05, model = "GP", parsModel = list(), 
                      indtest = dhsic.test, 
                      parsIndtest = list(method = "ExactFastTrace"), confounder_check = 0, output = FALSE)
  aux_graph=aux_graph+graph_resit
  
  shocks = auxY
  parents = aux_graph
   
  model_np = list(NULL,NULL,NULL)
  for (ii in 1:N){
    i_parents = which(parents[,ii]==1)
    if ( length( i_parents )==0 ) {
      shocks[,ii] = auxY[,ii]
    }else{
      aux_pa = auxY[,i_parents]
      
      options=gpOptions("ftc")
      options$kern$comp=list("rbf","white")
      
      model_np[[ii]]<-gpCreate(length( i_parents ),1,as.matrix(aux_pa),
                         as.matrix(auxY[,ii]),options)
      
      yhat<-gpOut(model_np[[ii]],as.matrix(aux_pa))
      shocks[,ii]<-as.matrix(auxY[,ii])-yhat
      
    }
  }
  
   
  
  return(list(Structural_shocks = shocks, phi = model_np, parents = parents))
}