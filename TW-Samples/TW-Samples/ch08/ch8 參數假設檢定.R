#例1
attach(mtcars)
hist(mpg,prob=T,nclass=10,col="light blue")	#繪製長條圖
xfit<-seq(min(mpg),max(mpg),length=40)
yfit<-dnorm(xfit,mean(mpg),sd(mpg))   		#你的經驗認為隨機變數的密度分布
lines(xfit,yfit,col="red",lwd=3)		#畫出密度函數，與長條圖對比，直觀判斷


x=rnorm(1000,170,8)                   		#假設隨機選取1000男同學的身高。
hist(x,prob=T,nclass=50,col="light blue")
xfit<-seq(min(x),max(x),length=100)
yfit1<-dnorm(xfit,mean(x),sd(x))		#常態分布
yfit2<-dchisq(xfit,mean(x))			#卡方分布
yfit3<-dunif(xfit,min(x),max(x))		#均勻分布
lines(xfit,yfit1,type="p",col="blue",lwd=1)
lines(xfit,yfit2,type="l",col="red",lwd=2)
lines(xfit,yfit3,type="b",col="green",lwd=2)

stem(mpg)

attach(mtcars)
qqnorm(mpg)
qqline(mpg)


a=runif(200)
qqnorm(a)
qqline(a)

b=rbinom(200,50,.25)
qqnorm(b)
qqline(b)

x=seq(0,20,0.1)
curve(dchisq(x,2),0,20,ylab="p（x）")
curve(dchisq(x,4),add=T,lty=2)
curve(dchisq(x,6),add=T,lty=3)
curve(dchisq(x,8),add=T,lty=4)
curve(dchisq(x,10),add=T,lty=5)
legend(13,0.4,c("df=2","df=4","df=6","df=8","df=10"), lty=1:5, bty="n")

X5=rchisq(100,5)
EDA(X5)

#例2
freq =c(22,21,22,27,22,36)
probs = c(1,1,1,1,1,1)/6        # 指定理論機率（均勻分布）
chisq.test(freq,p=probs)

#例3

freq = c(100,110,80,55,14)
probs = c(29, 21, 17, 17, 16)/100
chisq.test(freq,p=probs)


#例4
#檢驗100人成人男性是否服從N(170,8^2)
x=rnorm(100,170,10)
fn=table(cut(x,breaks=c(min(x),160,170,180,190,max(x))))
F=pnorm(c(min(x),160,170,180,190,max(x)),mean=170,sd=8)
P=c(F[1],F[2]-F[1],F[3]-F[2],F[4]-F[3],1-F[4])
chisq.test(fn,p=P)


#例5
yesbelt = c(12813,647,359,42)
nobelt = c(65963,4000,2642,303)
chisq.test(rbind(yesbelt,nobelt))

#例6
die.fair = sample(1:6,100,p=c(1,1,1,1,1,1)/6,rep=T)    # 均勻骰子
die.bias = sample(1:6,100,p=c(.5,.5,1,1,1,2)/6,rep=T)  # 不均勻骰子
res.fair = table(die.fair)
res.bias = table(die.bias)
chisq.test(count)
ks.test(die.fair,die.bias) 

#例7
 x = c(21240,4632, 22836,5484,5052,5064,6972,7596,14760,15012, 18720, 9480, 4728, 67200,52788)
ri = rank(x)


#例8
x = c(21240,4632, 22836,5484,5052,5064,6972,7596,14760,15012, 18720, 9480, 4728, 67200,52788)
ri = rank(x)
wilcox.test(x,mu=5080)

median.test <- function (x, median = NA){
 x <- as.vector(x)
 n <- length(x)
 bigger <- sum(x > median)
 equal <- sum(x == median)
 count <- bigger + equal/2
 count <- min(count, n - count)
 p <- 2 * pbinom(count, n, 0.5)
 c(Positive=bigger,Negative=count,P=p)
}

median.test(x,median=5080)


#例9
kruskal.test(weight ~ group, data=PlantGrowth)

#例10
scores = c(4,3,4,5,2,3,4,5,4,4,5,5,4,5,4,4,3,4,2,4,5,5,4,4)
person = c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3)
boxplot(scores~person)
kruskal.test(scores ~ person)
oneway.test(scores ~ person)

#例11
X=matrix(c(20.3,21.2,18.2,18.6,18.5,25.6,24.7,19.3,19.3,20.7, 24.0, 23.1,20.6,19.8,21.4),5)
friedman.test(X)


#例12
x=rnorm(50)
y=runif(50,0,1)
ks.test(x, "pnorm", mean=0, sd=1)
ks.test(y, "punif", 0,1 )
ks.test(x,"pexp",0.5)
x1=c(48,47,44,45,46,47,43,47,42,48)
x2=c(36,45,47,38,39,42,36,42,46,35)
boxplot(x1,x2,horizontal=T,names=c("x1","x2"))
ks.test(x1,x2)

#例13
#install.packages("tseries")
#mplibrary(tseries)
attach(mtcars)
jarque.bera.test(mpg)


x <- rnorm(100) 
jarque.bera.test(x)

x <- runif(100) 
jarque.bera.test(x)

#例14
attach(mtcars)
shapiro.test(mpg)

#例15
x= rnorm(40, mean = 5, sd = 3)
y= runif(40, min = 2, max = 4)
shapiro.test(x); shapiro.test(y);
#也可以用命令
#install.packages("nortest")
#library(nortest)
lillie.test(x)
lillie.test(y)


#例16

#install.packages("mvnormtest")
library(mvnormtest)
data(EuStockMarkets)
C <- t(EuStockMarkets[15:29,1:4])
mshapiro.test(C)
C <- t(EuStockMarkets[14:29,1:4])
mshapiro.test(C)
R <- t(diff(t(log(C))))
mshapiro.test(R)
dR <- t(diff(t(R)))
mshapiro.test(dR)




