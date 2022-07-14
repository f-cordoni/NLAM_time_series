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


N=3# try 3,4 8
p=N
Nsim=500

T=500

pCon <- 2/(p-1)
noiseVar <- rep(1,p)

#Var+Non GAuss
library(parallel)
pars <- list( regr.pars = list(),indtest.pars = list())
respar_VARNG<-mcmapply(experiment2parralel,
                 MoreArgs=list(p=p,T=TT,pCon=pCon,
            linear="VAR+NonGauss", pars = pars),1:Nsim,  mc.cores = 6)

#Var+Non Linear
pars <- list( regr.pars = list(),indtest.pars = list())
respar_VARNL<-mcmapply(experiment2parralel,
               MoreArgs=list(p=p,T=TT,pCon=pCon,
                             linear="VAR+Nonlinear",
                             pars = pars),1:Nsim, mc.cores= 8)

#Nonlinear+Non Linear
pars <- list( regr.pars = list(),indtest.pars = list())
respar_NLNL<-mcmapply(experiment2parralel,
                    MoreArgs=list(p=p,T=TT,pCon=pCon,
                                  linear="NL+NL", pars = pars),
                    1:Nsim,mc.cores= 8)

pars <- list( regr.pars = list(),indtest.pars = list())
pars=list(flag_st_function='logi')
respar_STVAR<-mcmapply(experiment2parralel,
                      MoreArgs=list(p=p,T=TT,
                                    pCon=pCon,linear="STVAR", 
                                    pars = pars),1:Nsim,
                      mc.cores = 8)

pars <- list( regr.pars = list(),indtest.pars = list())
respar_TVVAR<-mcmapply(experiment2parralel,
               MoreArgs=list(p=p,T=TT,pCon=pCon,
                             linear="TV-VAR", pars = pars),
               1:Nsim,mc.cores = 8)
if (p==3){
save(respar_VARNL,
     respar_VARNG,respar_NLNL,
     respar_STVAR,respar_TVVAR,file = "respar_p_3.RData")
} 
if (p==4){
  save(respar_VARNL,
       respar_VARNG,respar_NLNL,
       respar_STVAR,respar_TVVAR,file = "respar_p_4.RData")
}
if (p==10){
  save(respar_VARNL,
       respar_VARNG,respar_NLNL,
       respar_STVAR,respar_TVVAR,file = "respar_p_10.RData")
}



respar = respar_VARNL


sidicmlDAG <- round(mean(respar[1,]),2)
sidicmlDAGsd <- round(sd(respar[1,]),2)
hdicmlDAG <- round(mean(respar[2,]),2)
hdicmlDAGsd<- round(sd(respar[2,]),2)

sidlingamDAG <- round(mean(respar[3,]),2)
sidlingamDAGsd <- round(sd(respar[3,]),2)
hdlingamDAG<- round(mean(respar[4,]),2)
hdlingamDAGsd <- round(sd(respar[4,]),2)

sidPCDAG <- round(mean(respar[5,]),2)
sidPCDAGsd <- round(sd(respar[5,]),2)
hdPCDAG<- round(mean(respar[6,]),2)
hdPCDAGsd <- round(sd(respar[6,]),2)

sidCPCDAG <- round(mean(respar[7,]),2)
sidCPCDAGsd <- round(sd(respar[7,]),2)
hdCPCDAG<- round(mean(respar[8,]),2)
hdCPCDAGsd <- round(sd(respar[8,]),2)

sidBFDAG <- round(mean(respar[9,]),2)
sidBFDAGsd <- round(sd(respar[9,]),2)
hdBFDAG<- round(mean(respar[10,]),2)
hdBFDAGsd <- round(sd(respar[10,]),2)

sidCAMDAG <- round(mean(respar[11,]),2)
sidCAMDAGsd <- round(sd(respar[11,]),2)
hdCAMDAG<- round(mean(respar[12,]),2)
hdCAMDAGsd <- round(sd(respar[12,]),2)

sidrandDAG <- round(mean(respar[13,]),2)
sidrandDAGsd <- round(sd(respar[13,]),2)
hdrandDAG <- round(mean(respar[14,]),2)
hdrandDAGsd <- round(sd(respar[14,]),2)
 
 
table_SID = matrix(NA,7,10)
row.names(table_SID) = c("ICML","LINGAM",
                         "PC","CPC","BF",
                         "CAM","RANDOM")
table_HD = matrix(NA,7,10)
row.names(table_HD) = c("ICML","LINGAM",
                         "PC","CPC","BF",
                         "CAM","RANDOM")
colnames(table_SID)=rep(c("mean","sd"),5)
colnames(table_HD)=rep(c("mean","sd"),5)

for (ii in 1:7){
table_SID[ii,] = c(round(mean(respar_VARNL[2*ii-1,]),2),
                  round(sd(respar_VARNL[2*ii-1,]),2),
                  round(mean(respar_VARNG[2*ii-1,]),2),
                  round(sd(respar_VARNG[2*ii-1,]),2),
                  round(mean(respar_NLNL[2*ii-1,]),2),
                  round(sd(respar_NLNL[2*ii-1,]),2),
                  round(mean(respar_STVAR[2*ii-1,]),2),
                  round(sd(respar_STVAR[2*ii-1,]),2),
                    round(mean(respar_TVVAR[2*ii-1,]),2),
                  round(sd(respar_TVVAR[2*ii-1,]),2)
                  )

table_HD[ii,] = c(round(mean(respar_VARNL[2*ii,]),2),
                   round(sd(respar_VARNL[2*ii,]),2),
                   round(mean(respar_VARNG[2*ii,]),2),
                   round(sd(respar_VARNG[2*ii,]),2),
                   round(mean(respar_NLNL[2*ii,]),2),
                   round(sd(respar_NLNL[2*ii,]),2),
                   round(mean(respar_STVAR[2*ii,]),2),
                   round(sd(respar_STVAR[2*ii,]),2),
                   round(mean(respar_TVVAR[2*ii,]),2),
                   round(sd(respar_TVVAR[2*ii,]),2)
)
}

 

