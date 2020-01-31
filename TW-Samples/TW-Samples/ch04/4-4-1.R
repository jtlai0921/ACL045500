sim.clt <- function (m=100,n=10,p=0.25)
{ z = rbinom(m,n,p)               
   x = (z-n*p)/sqrt(n*p*(1-p))        
   hist(x,prob=T,breaks=20,main=paste("n =",n,"p =",p))
   curve(dnorm(x),add=T)             
}
sim.clt()          	  # 預設 m=100，n=10，p=0.25
sim.clt(1000)      	  # 取 m=1000，n=10，p=0.25
sim.clt(1000,30)   	  # 取 m=1000，n=30，p=0.25
sim.clt(1000,30,0.5)	  # 取 m=1000，n=30，p=0.5
