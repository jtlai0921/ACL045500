MS=read.table("mathstat.txt")
MS
head(MS)
dim(MS)
stem(MS$maths)
stem(MS$stats)

par(mfrow=c(1,2))
hist(MS$maths); hist(MS$stats)   
par(mfrow=c(1,1))

stem(MS$maths)
stem(MS$stats)

EDA <- function (x)
{ 
  par(mfrow=c(2,2))              
  hist(x)                        
  dotchart(x)                    
  boxplot(x,horizontal=T)        
  qqnorm(x);qqline(x)            
  par(mfrow=c(1,1))              
}

EDA(MS$maths)
EDA(MS$stats)

#data(exec.pay) 
pays<-c(1500,2500,5000,5200,5500,5600,5700,5800,5850,5850,5900,5950,6500,80000)
mean(pays)
summary(pays)
boxplot(pays)
mean(pays,trim=0.1)
#EDA(exec.pay)
#log.exec.pay = log(exec.pay[exec.pay >0])/log(10)
#EDA(log.exec.pay)
#pay=round(rexp(100,1/25),0)
#pay1=pay[pay>10]
pay=c(11,19,14,22,14,28,13,81,12,43,11,16,31,16,23,42,22,26,17,22,
      13,27,108,16,43,82,14,11,51,76,28,66,29,14,14,65,37,16,37,35,
      39,27,14,17,13,38,28,40,85,32,25,26,16,12,54,40,18,27,16,14,
      33,29,77,50,19,34)
EDA(pay)
log.pay =log10(pay)   
EDA(log.pay)
data()
data(ewr)
ewr
boxplot(ewr[,3:10])
names(ewr)[1]

par(mfrow=c(2,4))    
for(i in 3:10) boxplot(ewr[,i] ~ as.factor(ewr$inorout),main=names(ewr)[i])
par(mfrow=c(1,1))    
attach(InsectSprays)
boxplot(count)
boxplot(count~spray)
detach()

X=runif(100);EDA(X)
X=rnorm(100);EDA(X)
X=rt(100,10);EDA(X)
X=rf(100,10,10);EDA(X)
X=abs(rnorm(200));EDA(X)
X=rexp(200);EDA(X)

x=c("y","n","n","y","y","n","n","y","y")
table(x)

drink=c(3,4,1,1,3,4,3,3,1,3,2,1,2,1,2,3,2,3,1,1,1,1,4,3,1) 
table(drink)
par(mfrow=c(1,2))
barplot(drink)
barplot(table(drink))
par(mfrow=c(1,2))
barplot(table(drink)/length(drink),col=1:4)
barplot(table(drink),col=c("red","yellow","blue","white"))
par(mfrow=c(1,1))

drink.count=table(drink)    
par(mfrow=c(1,3))
pie(drink.count)
names(drink.count)=c("wine","wite","yellow","beer")
pie(drink.count)
pie(drink.count,col=c("purple","green","cyan","white"))
par(mfrow=c(1,1))

salary=c(2000,2100,2200,2300,2350,2450,2500 ,2700,2900,2850,3500,3800,2600,
         3000,3300,3200,4000,3100,4200)
mean(salary)
median(salary)
var(salary)
sd(salary)
fivenum(salary)
summary(salary)
salarym=c(salary,150000)
mean(salarym)
median(salarym)
mean(salarym,trim=0.2)
mean(salarym,trim=0.5)
IQR(salarym)
mad(salarym)
stem(salary)
stem(salarym)
salaryg=cut(salary,breaks=c(2000,3000,4000,max(salary)))
table(salaryg)
par(mfrow=c(1,2))
hist(salary)
rug(salary)
hist(salary,prob=T)
rug(salary)
par(mfrow=c(1,1))
par(mfrow=c(1,2))
boxplot(salary)
boxplot(salary,horizontal=T)
par(mfrow=c(1,1))

hist(faithful$eruptions,prob=T,breaks=25)  
lines(density(faithful$eruptions),col='red')

####outlier detection
boxplot(salarym)
boxplot.stats(salarym)
##Grubb test
library(outliers)
set.seed(5)
x = rnorm(10)
x
grubbs.test(x)
grubbs.test(x,type=20)
grubbs.test(x,type=11)
######Dixon Q test
set.seed(8)
x=rnorm(10)
x
dixon.test(x)
dixon.test(x,opposite=TRUE)
dixon.test(x,type=10)




###############################
### two variables
smoke=c("Y","N","N","Y","N","Y","Y","Y","N","Y")
study=c(1,2,2,3,3,1,2,1,3,2)
smoke=c("Y","N","N","Y","N","Y","Y","Y","N","Y")
study=c("<5h","5-10h","5-10h",">10h",">10h","<5h","5-10h","<5h",">10h","5-10h")
table(smoke,study)
tab=table(smoke,study)
prop.table(tab,1)
prop.table(tab,2)
prop.table(tab)
prop = function(x) x/sum(x)
apply(tab,2,prop)
t(apply(tab,1,prop))
par(mfrow=c(1,3))
barplot(table(smoke,study))     
barplot(table(study,smoke))     
barplot(table(study,smoke),beside=T,legend.text=c("<5h","5-10h",">10"))
legend()
par(mfrow=c(1,1))

x=c(5,5,13,7,11,11,9,8,9)
y=c(11,8,4,5,9,5,10,5,4,10)
boxplot(x,y)
d=c(5,5,5,13,7,11,11,9,8,9,11,8,4,5,9,5,10,5,4,10)
g=c(1,1,1, 1,1, 1, 1,1,1,1, 2,2,2,2,2,2, 2,2,2, 2)
boxplot(d~g)

data.entry(c(NA))
plot(x,y)                
abline(lm(y~x))          

yx=read.table("reg1.txt",header=T)
yx
attach(yx)
par(mfrow=c(1,2))
plot(x,y,pch=20)
plot(x,y);abline(lm(y~x))
abline(v=4500)
par(mfrow=c(1,2))
cor(x,y)
cor(y,x)
cor(yx$x,yx$y,method="spearman")
cor(rank(yx$x),rank(yx$y))
cor(x,y,method="spearman")
detach()


yx
yx[,"y"]         
yx[,1]           
yx[1:5,1:2]      
yx[1,]
yx[1,2]            
yx[,]

yx$x         
yx[['y']]      
yx[[1]]       
yx[yx$t>'1995',]
subset(yx,t>1995)

data(PlantGrowth)
attach(PlantGrowth)
par(mfrow=c(1,2))
stripchart(weight~group,pc=1)         
boxplot(weight~group, horizontal=T)   
par(mfrow=c(1,1))
detach()
unPG=unstack(PlantGrowth)
sPG=stack(unPG)
sPG


library(MASS)
data(Cars93)
dim(Cars93)
attach(Cars93)
names(Cars93)
price=cut(Price,c(0,12,20,max(Price)))    
table(price)
levels(price)=c("cheap","okay","expensive")
table(price)
mpg=cut(MPG.highway,c(0,20,30,max(MPG.highway)))
table(mpg)
levels(mpg)=c("gas guzzler","oky","miser")
table(mpg)
table(Type)
table(price,Type)
table(price,mpg)
table(price,Type,mpg)
par(mfrow=c(1,2))
barplot(table(price,Type))
barplot(table(price,Type),beside=T)

barplot(table(Type,price))
barplot(table(Type,price),beside=T)
par(mfrow=c(1,1))
boxplot(Price~Type)
r1=rnorm(1000)
f1=factor(rep(1:10,100))
boxplot(r1~f1)
stripchart(Type~price)
r2=rnorm(100)
f2=factor(rep(1:5,10))
stripchart(r2~f2)
dotchart(PlantGrowth$weight,groups=PlantGrowth$group)
data(PlantGrowth)
attach(PlantGrowth)
plot(weight,group)


iris
head(iris)
levels(iris$Species)
iris.lab = rep(c("1","2","3"),rep(50,3))

par(mfrow=c(1,2))
plot(iris[,1],iris[,3],type="n")      
text(iris[,1],iris[,3],cex=0.6)       
plot(iris[,1],iris[,3],type="n") 
text(iris[,1],iris[,3],iris.lab,cex=0.7)  
par(mfrow=c(1,1)) 

pairs(iris)
pairs(iris[,1:4])

pairs(iris[1:4],pch=21,bg=iris.lab)
pairs(iris[1:4],pch=21, bg=c("red", "green3", "blue")[unclass(iris$Species)])
