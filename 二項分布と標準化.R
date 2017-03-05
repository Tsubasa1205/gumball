#(4)-1
n<-10000
Y=rbinom(n,3,.3)
hist(Y,probability=T,main="Y=X1+X2+X3",breaks=0:4,right=F)
mean(Y)
var(Y)

#(4)-2
W=rbinom(n,100,.3)
hist(W,probability=T,main="W=X1+X2+c+X100",breaks=10:50)
mean(W)
var(W)

rm(list=ls(all=TRUE))

#(5)-1
X1<-rnorm(n,1,2)
X2<-rnorm(n,1,2)
X3<-rnorm(n,1,2)
X4<-rnorm(n,1,2)
X5<-rnorm(n,1,2)
X6<-rnorm(n,1,2)
X7<-rnorm(n,1,2)
X8<-rnorm(n,1,2)
X9<-rnorm(n,1,2)
X10<-rnorm(n,1,2)
Y=X1+X2+X3+X4+X5+X6+X7+X8+X9+X10
hist(Y,probability=T,main="Y=X1+X2+c+X10",breaks=50)
mean(Y)
var(Y)

#(5)-2
Z=(Y-10)/sqrt(40)
hist(Z,probability=T,xlim=c(-4,4),ylim=c(0,.4),main="Z=(Y-10)/sqrt(40)",breaks=50)
par(new=T)
x<-seq(-4,4,0.1)
curve(dnorm(x,0,1),xlim=c(-4,4),ylim=c(0,.4),type="l")