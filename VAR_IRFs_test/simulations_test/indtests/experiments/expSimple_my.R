source("indtestHsic.R")

x<-rnorm(400,1)
y<-rnorm(400,1)
T<-indtestHsic(x,y,pars = list(method = "ExactFastTrace"))
dHSIC::dhsic.test(x,y,method = "gamma")

x<-rnorm(400,1)
y<-0.5*x+rnorm(400,1)
T<-indtestHsic(x,y,pars = list(method = "ExactFastTrace"))

dHSIC::dhsic.test(x,y)

source("indtestAll.R")

source("indtestMutualHsic.R")
sampleSize <- 500
x <- matrix(0,sampleSize,3)

for(i in 1:sampleSize)
{
  a <- sample(4,1)
  x[i,1:3] <-  switch(as.character(a),
                      "1"= c(0,0,0),
                      "2"= c(0,1,1),
                      "3"= c(1,0,1),
                      "4"= c(1,1,0))
}
show(indtestMutualHsic(x[,1],x[,2],x[,3]))

show(indtestMutualHsic(x[,2],x[,3]))
show(indtestHsic(x[,1],x[,3]))
show(indtestHsic(x[,2],x[,1]))


z=rnorm(400,1)
indtestHsic(x,y)
X=cbind(x,y,z)
X=list(x,y,z)

y<-0.5*x+rnorm(400,1)
X=cbind(x,y)
indtestHsic(X,z,pars = list(method = "ExactFastTrace"))
indtestMutualHsic(cbind(x,y,z))

############################################# qui debug
x=rnorm(10,1)
y=rnorm(10,1)
z=rnorm(10,1)
y=x+1
x=cbind(x,z)

indtestHsic(x,y,pars = list(method = "ExactFastTrace"))
dHSIC::dhsic.test(x,y,pairwise=FALSE,method = "gamma")

x=rnorm(10,1)
y=rnorm(10,1)
z=rnorm(10,1)+x
y=x+1
x=cbind(x,z)

indtestHsic(x,y,pars = list(method = "ExactFastTrace"))
indtestHsic(x,y,pars = list(method = "Exact"))
dHSIC::dhsic.test(x,y,pairwise=FALSE,method = "gamma",kernel = "gaussian")
#different gamma approximaton same statistic


dHSIC::dhsic.test(list(x,y,z),pairwise=TRUE,method = "gamma")

x=rnorm(400,1)
y=rnorm(400,1)
z=runif(400,1)
X=list(x,y,z)
dHSIC::dhsic.test(X,pairwise=FALSE,method = "gamma")

