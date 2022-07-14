require("tseries")
require(NonlinearTSA)
require("np")
library(dHSIC)
require(gam)
require(gptk)
require(GAS)
require(tsDyn)

library(mgcv)

source("../RESIT.R")
source("../../codeANM/code/startups/startupLINGAM.R", chdir = TRUE)
 
require(Matrix)
source("../../codeANM/code/util_DAGs/randomB.R")
# source("../../codeANM/code/util_DAGs/randomDAG.R")
 
source("../../codeANM/code/util_DAGs/hammingDistance.R")
# source("../../codeANM/code/util_DAGs/structIntervDist.R")
# source("../../codeANM/code/util_DAGs/computePathMatrix.R")
# source("../../codeANM/code/util_DAGs/dSepAdji.R")
# source("../../codeANM/code/util_DAGs/allDagsJonas.R")
library(igraph)
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

 
source("help_aux_functions.R")
source("experiment_function.R")


N=4
p=N
Nsim=100

TT=500

pCon <- 2/(p-1)
noiseVar <- rep(1,p)

#Var+Non GAuss
pars <- list( regr.pars = list(),indtest.pars = list())
respar<-mapply(experiment2parralel,MoreArgs=list(p=p,T=TT,pCon=pCon,linear="VAR+NonGauss", pars = pars),1:Nsim)

#Var+Non Linear
pars <- list( regr.pars = list(),indtest.pars = list())
respar<-mapply(experiment2parralel,MoreArgs=list(p=p,T=TT,pCon=pCon,linear="VAR+Nonlinear", pars = pars),1:Nsim)

#Nonlinear+Non Linear
pars <- list( regr.pars = list(),indtest.pars = list())
respar<-mapply(experiment2parralel,MoreArgs=list(p=p,T=TT,pCon=pCon,linear="NL+NL", pars = pars),1:Nsim)

pars <- list( regr.pars = list(),indtest.pars = list())
pars=list(flag_st_function='logi')
respar<-mapply(experiment2parralel,MoreArgs=list(p=p,T=TT,pCon=pCon,linear="STVAR", pars = pars),1:Nsim)

pars <- list( regr.pars = list(),indtest.pars = list())
respar<-mapply(experiment2parralel,MoreArgs=list(p=p,T=TT,pCon=pCon,linear="TV-VAR", pars = pars),1:Nsim)

pars <- list( regr.pars = list(),indtest.pars = list())
respar<-mapply(experiment2parralel,MoreArgs=list(p=p,T=TT,pCon=pCon,linear="GR-TV-VAR", pars = pars),1:Nsim)

pars <- list( regr.pars = list(),indtest.pars = list())
respar<-mapply(experiment2parralel,MoreArgs=list(p=p,T=TT,pCon=pCon,linear="GR-TV-VAR-GAS", pars = pars),1:Nsim)

sidicmlDAG <- round(mean(respar[1,]),2)
sidicmlDAGsd <- round(sd(respar[1,]),2)
hdicmlDAG <- round(mean(respar[2,]),2)
hdicmlDAGsd<- round(sd(respar[2,]),2)

sidlingamDAG <- round(mean(respar[3,]),2)
sidlingamDAGsd <- round(sd(respar[3,]),2)
hdlingamDAG<- round(mean(respar[4,]),2)
hdlingamDAGsd <- round(sd(respar[4,]),2)

sidrandDAG <- round(mean(respar[5,]),2)
sidrandDAGsd <- round(sd(respar[5,]),2)
hdrandDAG <- round(mean(respar[6,]),2)
hdrandDAGsd <- round(sd(respar[6,]),2)
 
 
cat("======== \n SID results \n ======== \n ICML\n ", sidicmlDAG,
    "   +- ", sidicmlDAGsd, "\n LINGAM\n",
    sidlingamDAG, "   +- ", sidlingamDAGsd, "\n \n random\n",
    sidrandDAG, "   +- ", sidrandDAGsd)

cat("\n\n ======== \n Hamming Dist results DAG
    ======== \n ICML\n", hdicmlDAG, "    +- ", hdicmlDAGsd, "\n \n LINGAM\n",
    hdlingamDAG, "    +- ", hdlingamDAGsd, "\n \n random\n", hdrandDAG, 
    "    +- ", hdrandDAGsd, "\n\n")

 

