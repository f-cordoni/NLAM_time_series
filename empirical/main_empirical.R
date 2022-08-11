

#select VAR order
require(vars)

print(VARselect(DATA)$selection)
# looking at AIC (BIC)  lag  = 6
#estimate VAR withouth interecpt since we have demeaned data
lag = VARselect(DATA)$selection[1]

require(tsDyn)
model_var = lineVar(DATA,lag = lag,include = "const")

#get the matrices 
A = list()
A[[lag+1]] = model_var$coefficients[,1]
model_var$coefficients  = model_var$coefficients[,-1]
for (ii in 1:lag){
  A[[ii]] = matrix(0,nrow = 3,ncol = 3)
  
    A[[ii]]   = model_var$coefficients[,((ii-1)*3+1):(ii*3)]# 4:6 # 7 : 9
  
}
#interecpt

residual =(model_var$residuals)
Omega = cov(residual)
residual_orig = residual

#launch RESIT


