require("tseries")
require(NonlinearTSA)
require("np")
library(dHSIC)
require(gam)
require(gptk)
require(GAS)
require(tsDyn)

library(mgcv)
library(ks)

 
setwd("CAM_R/")
 files.sources = list.files()

sapply(files.sources, source)
setwd("../")


source("utils/startupLINGAM.R", chdir = TRUE)
source("utils/startupGDS.R", chdir = TRUE)
source("utils/startupPC.R", chdir = TRUE)

#example faithful violated
T= 2e2
X = 0.8*rnorm(T)
Y = 3/2*X+0.5*rnorm(T)
Z = 2*Y -3* X + 0.25*rnorm(T)
library(dHSIC) 
#The null hypothesis (H_0) is that all variables are jointly independent.
dhsic.test(X,Z)$p.value
dhsic.test(X,Y)$p.value
dhsic.test(Y,Z)$p.value
DATA = cbind(X,Y,Z)
resPC <- pcWrap(DATA, 0.05, mmax = Inf)

source("utils/startupGES.R", chdir = TRUE)
source("utils/startupScoreSEMIND.R", chdir = TRUE)
source("utils/startupScoreSEMSEV.R", chdir = TRUE)
source("utils/startupBF.R", chdir = TRUE)
source("../RESIT.R")

require(Matrix)
source("utils/randomB.R")
# source("../../codeANM/code/util_DAGs/randomDAG.R")
 
source("utils/hammingDistance.R")
# source("../../codeANM/code/util_DAGs/structIntervDist.R")
# source("../../codeANM/code/util_DAGs/computePathMatrix.R")
# source("../../codeANM/code/util_DAGs/dSepAdji.R")
# source("../../codeANM/code/util_DAGs/allDagsJonas.R")
library(igraph)
source("help_aux_functions.R")
source("my_sampleDataFromG.R")
library(MASS)
 
 
# BiocManager::install("graph")
# BiocManager::install("RBGL")
library(graph)
library(RBGL)
library(ggm)
library(pcalg)
library(Matrix)
library(SID)
require(KFAS)

library(dHSIC)  
source("experiment_function.R")





P = c(4,6,8)
T=c(250,500,1e3)

table_SID_all = list(NA,NA,NA)
names(table_SID_all) = c(P)

for (ii in 1:3){
  table_SID_all[[ii]] = list(NA,NA,NA)
  names( table_SID_all[[ii]] ) = c(T)
}

table_HD_all = table_SID_all


Nsim=500

for (pp in 1:length(P)){
 for(tt in 1:length(T)){  


N=P[pp]# try 3,4 8
p=N
TT = T[tt]

pCon <- 2/(p-1)
noiseVar <- rep(1,p)




library(parallel)
#VAR +linear
 

#Var+Non GAuss
pars <- list( regr.pars = list(),indtest.pars = list())
#experiment2parralel (4,T = TT,pCon,linear="VAR+NonGauss",pars,1)

respar_VARNG<-mcmapply(experiment2parralel,
                 MoreArgs=list(p=p,T=TT,pCon=pCon,
            linear="VAR+NonGauss", pars = pars),1:Nsim,  mc.cores = 8)

#Var+Non Linear
pars <- list( regr.pars = list(),indtest.pars = list())
respar_VARNL<-mcmapply(experiment2parralel,
               MoreArgs=list(p=p,T=TT,pCon=pCon,
                             linear="VAR+Nonlinear",
                             pars = pars),1:Nsim, mc.cores= 8)

 



 
 
table_SID = matrix(NA,7,4)
row.names(table_SID) = c("ICML","LINGAM",
                         "PC","CPC","BF",
                         "CAM","RANDOM")
table_HD = matrix(NA,7,4)
row.names(table_HD) = c("ICML","LINGAM",
                         "PC","CPC","BF",
                         "CAM","RANDOM")
colnames(table_SID)=rep(c("mean","se"),2)
colnames(table_SID)[c(1,3)] = c("NL-avg","NG-avg")
colnames(table_HD)=rep(c("mean","se"),2)
colnames(table_HD)[c(1,3)] = c("NL-avg","NG-avg")

for (ii in 1:7){
table_SID[ii,] = c(round(mean(respar_VARNL[2*ii-1,]),2),
                  round(sd(respar_VARNL[2*ii-1,]),2)/sqrt(Nsim),
                  round(mean(respar_VARNG[2*ii-1,]),2),
                  round(sd(respar_VARNG[2*ii-1,]),2)/sqrt(Nsim) 
                  )

table_HD[ii,] = c(round(mean(respar_VARNL[2*ii,]),2),
                   round(sd(respar_VARNL[2*ii,]),2)/sqrt(Nsim),
                   round(mean(respar_VARNG[2*ii,]),2),
                   round(sd(respar_VARNG[2*ii,]),2)/sqrt(Nsim) )
}

table_SID_all[[pp]][[tt]] = table_SID
  table_HD_all[[pp]][[tt]] = table_HD
 }
  }

