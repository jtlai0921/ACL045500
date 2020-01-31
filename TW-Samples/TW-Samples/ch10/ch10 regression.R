install.packages("maxLik")
######################################
##consumeption
consume=c(594,638,1122,1155,1408,1595,1969,2078,2585,2530)
income=c(800,1100,1400,1700,2000,2300,2600,2900,3200,3500)
cor(income,consume)
plot(income,consume)
abline(lm(consume~income))

### heart
x = c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37) 
y=c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)
cor(x,y)
cor.test(x,y)  #correlation test
plot(x,y)
abline(lm(y~x))
#####################################################
data=read.table("consume_p.txt")
x=data$V1
y=data$V2
plot(x,y,xlab="每月可支配收入",ylab="每月消費支出")
for x in (c(800,1100,1400,1700,2000,2300,2600,2900,3200,3500)){
 ey<-mean(subset(data,x
 }
abline(lm())

#########################
 #OLS
 lm1=lm(consume~income)
 coef(lm1)
 coef(lm(consume~-1+income))

 lm2=lm(y~x)
 coef(lm2)
###########################################
##MLE
 library(maxLik)
loglik=function (para){
       N=length(consume)
       e=consume-para[1]-para[2]*income
       ll=-0.5*N*log(2*pi)-0.5*N*log(para[3]^2)-0.5*sum(e^2/para[3]^2)
       return(ll)
       }
mle1=maxLik(loglik,start=c(0.1,1,1))
coef(mle1)

loglik2=function (para){
       N=length(consume)
       e=consume-para[1]-para[2]*income
       #ll=-0.5*N*log(2*pi)-0.5*N*log(para[3]^2)-0.5*sum(e^2/para[3]^2)
       ll=sum(e^2)
       return(ll)
       }
optim(c(100,1),loglik2)

loglik=function (para){
       N=length(x)
       e=y-para[1]-para[2]*x
       ll=-0.5*N*log(2*pi)-0.5*N*log(para[3]^2)-0.5*sum(e^2/para[3]^2)
       return(ll)
       }
mle2=maxLik(loglik,start=c(0.1,1,1))
coef(mle2)
#############################
 # sigma standard error
 slm<-summary(lm1)
 slm$sigma
 slm$coef
 slm$coef[,2]
 
 slm2<-summary(lm2)
 slm2$sigma
 slm2$coef
 slm2$coef[,2]
###############################
### R^2
slm$r.squared
slm2$r.squared
#############
##t test
slm<-summary(lm1)
slm
slm$coef[,3]
slm$coef[,4]
########################
##forecasting
slm$coef[,1][1]+ slm$coef[,1][2]*4000
coef(lm1)[1]+coef(lm1)[2]*4000
predict(lm1,newdata=data.frame(income=4000))
round(fitted(lm1),2)
round(resid(lm1),2)
plot(lm1)
## predict intervation
predict(lm1,newdata=data.frame(income=4000),interval="confidence",level=0.95)
predict(lm1,newdata=data.frame(income=4000),interval="prediction",level=0.95)

####
 par(mfrow=c(1,1))
 sx=sort(income)
 conf = predict(lm1,data.frame(income=sx),interval="confidence")
 pred = predict(lm1,data.frame(income=sx),interval="prediction")
 plot(income,consume);
abline(lm1)
lines(sx,conf[,2]); lines(sx,conf[,3])
lines(sx,pred[,2],lty=3); lines(sx,pred[,3],lty=3)
########################################################################
###multivariate regression
dat<-read.csv(file="tax.csv")
lm3=lm(tax~GDP+expand+CPI,data=dat)
coef(lm3)

attach(dat)
loglik=function (para){
       #para[5]=sigma
       N=length(tax)
       e=tax-para[1]-para[2]*GDP-para[3]*expand-para[4]*CPI
       ll=-N*log(sqrt(2*pi)*para[5])-(1/(2*para[5]^2))*sum(e^2)
       #ll=-0.5*N*log(2*pi)-0.5*N*log(para[5]^2)-0.5*sum(e^2/para[5]^2)
       return(ll)
       }
mle3=maxLik(loglik,start=c(6616,0.04,0.6,58,100),iterlim=10000)
mle3
coef(mle3)
detach()
##R2
slm3<-summary(lm3)
slm3$r.squared
slm3$adj.r.squared
#######################
###predict
coef(lm3)[1]+coef(lm3)[2]*520000+coef(lm3)[3]*130000+coef(lm3)[4]*103
predict(lm3,newdata=data.frame(GDP=520000,expand=130000,CPI=103))
## predict intervation
predict(lm3,interval="confidence")
predict(lm3,interval="prediction")
predict(lm3,newdata=data.frame(GDP=520000,expand=130000,CPI=103),interval="confidence")
predict(lm3,newdata=data.frame(GDP=520000,expand=130000,CPI=103),interval="prediction")
################################################
####multicolinearity
revenue<-read.csv(file="multicolin.csv")
lm_re<-lm(Y~X2+X3+X4+X5+X6+X7,data=revenue)
summary(lm_re)





fm = lm(y ~ x)
summary(fm)
  abline(fm)
abline(v=50,col="red")
fm2=lm(y~-1+x)
summary(fm2)
coef(fm)+coef(fm)[2]*50
resid(fm)
fm$resid
fitted(fm)
EDA(resid(fm))
par(mfrow=c(2,2))
 plot(fm)

 n= length(x)
 res = resid(fm)
 b1 =(coef(fm))[2]
 s = sqrt(sum(res^2)/(n-2))
 SE = s/sqrt(sum((x-mean(x))^2))
 t = (b1-(-1))/SE
 pt(t,n-2,lower.tail=FALSE)*2

 bo =(coef(fm))[1]
 SEbo = s * sqrt( sum(x^2)/( n*sum((x-mean(x))^2)))
 t= (bo-220)/SE
 pt(t,n-2)*2
##point predict
coef(fm)[1]+coef(fm)[2]*50
predict(fm,data.frame(x=50))
predict(fm)

##interval predict
par(mfrow=c(1,1))
 sx=sort(x)
 conf = predict(fm,data.frame(x=sx),interval="confidence")
 pred = predict(fm,data.frame(x=sx),interval="prediction")
 plot(x,y);
abline(fm)
lines(sx,conf[,2]); lines(sx,conf[,3])
 lines(sx,pred[,2],lty=3); lines(sx,pred[,3],lty=3)

coef(summary(fm))
dim(coef(summary(fm)))
coef(summary(fm))[2,4]
attributes(coef(summary(fm)))

 x1 = 1:20
 x2 = sample(1:100,20)
 e=rnorm(20)
 y = x1+x2+e
 lm.sim=lm(y ~ -1+x1+x2)
summary(lm.sim)

#### example 
reg2=read.table("reg2.txt")
reg2
dim(reg2)
pairs(reg2)
fm=lm(y~x1+x2+x3+x4,data=reg2)
summary(fm)
cor(reg2)
cor.test(reg2)
plot(rownames(reg2),reg2$y,type="p",xlab="year")
lines(rownames(reg2),fm$fit)
#stepwise regression
step(fm) 
step(fm,direction="forward")
step(fm,direction="backward")
step(fm,direction="both")

##ridge regression
names(longley)[1] <- "y"
lm.ridge(y ~ ., lon  gley)
plot(lm.ridge(y ~ ., longley,
              lambda = seq(0,0.1,0.001)))
select(lm.ridge(y ~ ., longley,
                lambda = seq(0,0.1,0.0001)))

##quadratic regression
x = c(100,200,300,450,600,800,1000)
y = c(253, 337,395,451,495,534,574)
lm.1 = lm(y ~ x)
summary(lm.1)
lm.2 = lm(y ~ x + I(x^2))
summary(lm.2)
lm.3 = lm(y ~ x + I(x^2) + I(x^3))
summary(lm.3)
 plot(x,y)
 lines(x,fitted(lm.1),lty=1)
lines(x,fitted(lm.2),lty=2)
 lines(x,fitted(lm.3),lty=3)
 legend(600,400,c("linear","quadratic","cubic"),lty=1:3,bty="n")
title("regression")