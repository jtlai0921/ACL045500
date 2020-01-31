sim.clt <- function (m=100,n=10,p=0.25)
{ z = rbinom(m,n,p)               
   x = (z-n*p)/sqrt(n*p*(1-p))        
   hist(x,prob=T,breaks=20,main=paste("n =",n,"p =",p))
   curve(dnorm(x),add=T)             
}
sim.clt()          	  # �w�] m=100�An=10�Ap=0.25
sim.clt(1000)      	  # �� m=1000�An=10�Ap=0.25
sim.clt(1000,30)   	  # �� m=1000�An=30�Ap=0.25
sim.clt(1000,30,0.5)	  # �� m=1000�An=30�Ap=0.5