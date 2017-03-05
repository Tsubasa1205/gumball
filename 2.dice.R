#(1)
n<-10
X10<-floor(runif(n,1,7))
X10

#(2)
n<-100
X100<-floor(runif(n,1,7))
hist(X100,breaks=seq(0,6,by=1),freq=F)

#(3)
mean(X100)
var(X100)

#(4)
fraction=6
for (i in 1:6){
fraction[i]<-length(X100[X100==i])/n
}
fraction
