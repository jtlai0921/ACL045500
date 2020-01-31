#5.1
x=c(1,2,3,4)
x
y=c("a","b","c")
y
x = scan()

data(package="SemiPar")
data(copper,package=”SemiPar”)

library(SemiPar)
data()
data(fossil)

s1=read.table("student.txt")
s1

s2=read.table("student.txt",header=T)
s2

S2=read.csv(file="student.csv")
S2

install.packages(“foreign”)
library(foreign)

#5.3 新建新變數
cons<-c(5000,5800,6000,10200,8500)
pop<-c(2000,3600,3500,5020,6100)
gnp<-c(6000,7200,7400,11000,9200)

pgnp<-gnp/pop
psave<-(gnp-cons)/pop

data<-data.frame(gnp,cons,pop)
transform(data,pgnp=gnp/pop,psave=(gnp-cons)/pop)
(pgnp<-with(data,gnp/pop))
(psave<-with(data,(gnp-cons)/pop))

#5.3.2 變數重編碼
data(Cars93)
head(Cars93)
dat<-data.frame(manu=Cars93$Manufacturer,price=Cars93$Price)

dat$pricegrade<-NA
dat$pricegrade[dat$price>=20]<-"expensive"
dat$pricegrade[dat$price>=12&dat$price<20]<-"okay"
dat$pricegrade[dat$price<12]<-"cheap"
head(dat)

dat<-within(dat,{
    pricegrade<-NA
    pricegrade[price>=20]<-"expensive"
    pricegrade[price>=12&dat$price<20]<-"okay"
    pricegrade[price<12]<-"cheap"
})
head(dat)

dat$pricegrade<-cut(dat$price,c(0,12,20,max(dat$price)))  
levels(dat$pricegrade)<-c("cheap","okay","expensive")
head(dat)

recode(dat$pricegrade,"'cheap'='A';'okay'='B';'expensive'='C'")

#5.3.3 變數重新命名
dat1 <- rename(dat, c(pricegrade="grade"))
head(dat1)

names(dat)
names(dat)[3] <- "grade"
head(dat)

#5.3.4 變數類型的轉換
a <- c(1,2,3);a
is.numeric(a)
is.vector(a)

a <- as.character(a);a
is.numeric(a)
is.vector(a)
is.character(a)

#5.4  缺失資料的探索與檢定
y <- c(1,2,3,NA)
is.na(y)

data(sleep, package= "VIM")
sleep[!complete.cases(sleep),]
sum(!complete.cases(sleep))
mean(complete.cases(sleep))

newsleep <- na.omit(sleep)

#  缺失資料的處理
library(mice)
data(sleep, package="VIM")
imp <- mice(sleep, seed=6666)
fit <- with(imp,lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)

#5.5  資料框的合併與拆分
data(PlantGrowth)
PlantGrowth     
data(PlantGrowth)
unPG <- unstack(PlantGrowth)
unPG
sPG=stack(unPG)
sPG

# 資料集的抽取
newdata1 <- sleep[1:20, ]  # 選擇前20個觀測

attach(sleep)
newdata2 <- sleep[which(Sleep>=3 & Sleep <6),]  # 選擇睡眠時間在3~6小時之間的觀測
detach(sleep)

newdata3 <- subset(sleep, sleep>=3 & sleep<6, select=c(BodyWgt, Dream, Sleep, Span, Pred, Exp, Danger))

newdata4<- subset(sleep, Pred=3 & sleep<6, select=BodyWgt:  Pred)


install.packages('VIM')
install.packages('mice')
data(sleep,package='VIM')
summary(sleep)
attach(sleep)
summary(NonD)
sleep$NonD[sleep$NonD==NA] <- mean(NonD,na.rm=T)