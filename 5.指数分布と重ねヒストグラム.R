#(1)
#λ=1をパラメータとする指数分布から100個のデータを無作為抽出し、その結果のヒストグラムを表示
N<-100
s<-rexp(N,1)
hist(s,breaks=15,freq=F)

#(2)
#(1)のような無作為抽出を10000回シミュレートし、標本分散を10000個計算する。その標本分散の標本平均を表示
T<-10000
sample_vars<-numeric(T)
for (i in 1:T) {
  s<-rexp(N,1)
  sample_vars[i]<-var(s)
  }
hist(sample_vars,breaks=50,freq=F)
mean(sample_vars)

#(3)
#(2)で生成された標本分散10000個を標準化し、その結果のヒストグラムを表示し、正規標準分布の密度関数の曲線も重ねて表示
sigma2<-1
sigma4<-sigma2^2
m4<-9
W=sqrt(N)*((sample_vars-sigma2)/sqrt(m4-(sigma4*(N-3))/(N-1)))
par(mfrow=c(1,2))
hist(W,xlim=c(-2,6),ylim=c(0,.5),breaks=50,freq=F,col="#ff00ff40")
hist(W,xlim=c(-5,5),ylim=c(0,.5),breaks=50,freq=F,col="#ff00ff40")
std.normal.density <- function(x) dnorm(x,0,1)
par(new=T)
plot(std.normal.density,xlim=c(-5,5),ylim=c(0,.5),xlab="W",ylab="Density")

#(4)
#(3)までで扱った標本分散は100個のデータを利用していたが、これを1000個のデータに増やし同様に標準化まで行い、ヒストグラムを表示。その結果がどのように推移したかわかりやすくするために透過色で色付けし、ヒストグラムを重ねた。また、標準正規分布に近づいたのかわかりやすくするために標準正規分布の密度関数の曲線と重ねた。
N_2<-1000
sample_vars_2<-numeric(T)
for (i in 1:T) {
  s<-rexp(N_2,1)
  sample_vars_2[i]<-var(s)
  }
W_2=sqrt(N_2)*((sample_vars_2-sigma2)/sqrt(m4-(sigma4*(N_2-3))/(N_2-1)))
par(mfrow=c(1,1))
hist(W_2,xlim=c(-4,4),ylim=c(0,.5),main="std_var(N=1000)",breaks=50,freq=F,col="#0000ff40")

par(mfrow=c(1,2))
hist(W,xlim=c(-4,6),ylim=c(0,.5),main="std_var(N=100)",breaks=50,freq=F,col="#ff00ff40")
par(new=T)
plot(std.normal.density,xlim=c(-4,6),ylim=c(0,.5),xlab="W",ylab="Density",main="std_var(N=100)")
hist(W_2,xlim=c(-4,6),ylim=c(0,.5),main="std_var(N=1000)",breaks=50,freq=F,col="#0000ff40")
par(new=T)
plot(std.normal.density,xlim=c(-4,6),ylim=c(0,.5),xlab="W_2",ylab="Density",main="std_var(N=1000)")

par(mfrow=c(1,1))
hist(W,xlim=c(-4,6),ylim=c(0,.5),xlab="W",main="std_var",breaks=50,freq=F,col="#ff00ff40")
par(new=T)
hist(W_2,xlim=c(-4,6),ylim=c(0,.5),xlab="W",main="std_var",breaks=50,freq=F,col="#0000ff40")
