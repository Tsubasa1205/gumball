#(1)
T=10000
n=100
Y<-numeric(T)
for(i in 1:T){
Xi<-rbinom(n,1,.5)
Y[i]<-mean(Xi)
}
hist(Y,freq=F,breaks=50,xlim=c(.3,.7),main="sample means")

#(2)
mu=0.5
sigma2=0.5*0.5
Z<-(Y-mu)/(sqrt(sigma2/n))

#(3)
hist(Z,freq=F,breaks=50,xlim=c(-4,4),ylim=c(0,.4))

#(4)
std.normal.density <-function(x){dnorm(x,0,sqrt(1))}
par(new=T)
plot(std.normal.density,xlim=c(-4,4),ylim=c(0,.4),main="Histogram of Z")
mean(Z)
var(Z)
qqnorm(Z)
qqline(Z, col=5)
