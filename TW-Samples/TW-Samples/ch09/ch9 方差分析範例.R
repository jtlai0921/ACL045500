PlantGrowth
oneway.test(weight ~ group, data=PlantGrowth, var.equal=T)
anova(lm(weight ~ group, data=PlantGrowth))
summary(lm(weight ~ group, data=PlantGrowth))
aov(weight ~ group, data=PlantGrowth)
summary(aov(weight ~ group, data=PlantGrowth))

range=c(582,562,653,491,541,516,601,709,392,758,582,487) 
A=c(1,1,1,2,2,2,3,3,3,4,4,4)
B=c(1,2,3,1,2,3,1,2,3,1,2,3)
 plot(range~A,pch=B)
  legend(1.5,750,legend=1:3,pch=B)

par(mfrow=c(1,2))
  boxplot(range~A,xlab="A"); boxplot(range~B,xlab="B") 
  par(mfrow=c(1,1))

A = factor(A) ; B = factor(B)
range.aov <- aov(range ~ A + B )
 summary(range.aov)
summary(aov(range ~ A + B ))

Y=c ( 60.7,61.1,61.5,61.3,61.6,62.0,61.7,61.1,61.5,60.8,61.7,61.2, 62.2,62.8,62.1,61.7,
      60.6,60.3,60.6,61.0,61.4,61.5,60.7,60.9)
A=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3)
B=c(1,1,2,2,3,3,4,4, 1,1,2,2,3,3,4,4, 1,1,2,2,3,3,4,4) 
A=factor(A); B=factor(B)
summary(aov(Y~A+B+A*B))
