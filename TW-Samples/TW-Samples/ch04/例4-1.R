Nsim=10^4
U=runif(Nsim)
X=-log(U)
Y=rexp(Nsim)
par(mfrow=c(1,2))
hist(X,freq=F,main="Exp from Uniform")
curve(dexp(x,1),add=T,col="red")
hist(Y,freq=F,main="Exp from R")
curve(dexp(x,1),add=T,col="red")
