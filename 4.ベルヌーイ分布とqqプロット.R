#(1)
#1となる確率が0.5のベルヌーイ分布から100個のデータを無作為抽出し、その標本平均を計算するセットを10000回繰り返し、そのヒストグラムを表示。
T=10000
n=100
Y<-numeric(T)
for(i in 1:T){
Xi<-rbinom(n,1,.5)
Y[i]<-mean(Xi)
}
hist(Y,freq=F,breaks=50,xlim=c(.3,.7),main="sample means")

#(2)
#(1)で生成したデータを利用して標準化した標本平均を10000個シミュレート
mu=0.5
sigma2=0.5*0.5
Z<-(Y-mu)/(sqrt(sigma2/n))

#(3)
#(2)で生成した標準化された標本平均のヒストグラムを表示
hist(Z,freq=F,breaks=50,xlim=c(-4,4),ylim=c(0,.4))

#(4)
#(3)で表示したヒストグラムに正規標準分布の密度関数の曲線を重ねて表示。また、qqプロットの表示
std.normal.density <-function(x){dnorm(x,0,sqrt(1))}
par(new=T)
plot(std.normal.density,xlim=c(-4,4),ylim=c(0,.4),main="Histogram of Z")
mean(Z)
var(Z)
qqnorm(Z)
qqline(Z, col=5)
