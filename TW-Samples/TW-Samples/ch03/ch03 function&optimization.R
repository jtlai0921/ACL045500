##if/else
x<-3
if(x>2) y<-2*x else y<-3*x 
y

#ifelse
 x<-1
ifelse(x>2, y<-2*x, y<-3*x)
#switch
switch(1,2*3,sd(1:5),runif(3)) 
switch(2,2*3,sd(1:5),runif(3))  
switch(3,2*3,sd(1:5),runif(3))
x<-"meat"
switch(x, meat="chicken", fruit="apple", vegetable="potato")

##loop
#for
Fibonacci<-NULL
Fibonacci[1]<-Fibonacci[2]<-1
n=16
for (i in 3:n) Fibonacci[i]<-Fibonacci[i-2]+Fibonacci[i-1]
Fibonacci       
##while
Fibonacci<-NULL
Fibonacci[1]<-Fibonacci[2]<-1
  i<-1
  while (Fibonacci[i]+Fibonacci[i+1]<1000) 
    {
  Fibonacci[i+2]<-Fibonacci[i]+Fibonacci[i+1]  
  i<-i+1 
    } 
Fibonacci
       
#repeat
Fibonacci<-NULL
  Fibonacci[1]<-Fibonacci[2]<-1
    i<-1
    repeat {
         Fibonacci[i+2]<-Fibonacci[i]+Fibonacci[i+1]
         i<-i+1
         if (Fibonacci[i]+Fibonacci[i+1]>=1000) break
       }
  Fibonacci      

         
                  
################         
## write ourown function
std <- function(x) {
  sqrt(var(x))
}
std2<-function(x)  sqrt(var(x))
x=1:10
sd(x)
std(x)
std2(x)
##
welcome <- function() {
  print("welcome to use R")
}
welcome()
##
welcome.sb <- function(names){
  print(paste("welcome", names,"to use R"))
}
welcome.sb("Mr Fang")
welcome.sb("Mr wang")
welcome.sb()
welcome.sb <- function(names="Mr Fang"){
  print(paste("welcome", names,"to use R"))
}
welcome.sb()

sim.t=function(n){ mu=10;sigma=5;
                   x=rnorm(n,mu,sigma)
                   t<-(mean(x)-mu)/(std(x)/n) 
                  return(list(x=x,t=t))
                  }
sim.t(100)
sim.t2=function(n,mu=10,sigma=5){ 
                   x=rnorm(n,mu,sigma)
                   t<-(mean(x)-mu)/(std(x)/n) 
                   return(list(x=x,t=t))
}
sim.t2(100)
sim.t2(100,0,1)

##
plot.f <- function(f,a,b,...){
  xvals=seq(a,b,length=100)
  plot(xvals,f(xvals),type="l",...)
}
plot.f(sin,0,2*pi) 
curve(sin,0,2*pi)
plot.f(cos,0,2*pi)
plot.f(exp,-1,1)
plot.f(log,0,1)
##
my.average <- function(x) {
  sum(x)/length(x)
}
my.average(c(1,2,3))
mean(c(1,2,3))


vms=function(x){
  xx=rev(sort(x)) 
  xx=xx[1:5] 
  mm=mean(xx)
  va=var(xx)
  return(list(mm=mm,var=va))
}
y=c(5,15,32,25,26,28,65,48,3,37,45,54,23,44) 
vm=vms(y)
vm$mm
vm[[1]]
### system time
system.time(for(i in 1:100) mad(runif(1000)))
 ptm<-proc.time()
 for(i in 1:100) mad(runif(1000)) 
 proc.time()-ptm

 ptm<-proc.time()
 x<-rnorm(100000)
 y<-rnorm(100000)
 z<-c()
 for (i in 1:100000){
   z<-c(z,x[i]+y[i])
  }
 proc.time()-ptm

ptm<-proc.time()
x<-rnorm(100000)
y<-rnorm(100000)
z<-rep(NA,100000) 
 for (i in 1:100000){
   z[i]<-x[i]+y[i] 
    }
 proc.time()-ptm

ptm<-proc.time() 
z<-x+y
proc.time()-ptm

f=function(x) log(x)-x^2
curve(f,xlim=c(0,2))

optimize(f,c(0.1,10),tol=0.0001,maximum=T)

x1=x2=seq(-10,10,length=100)

fr2=function(x1,x2){
  (x1^2+x2-11)^2+(x1+x2^2-7)^2
}

gr.exp=expression((x1^2+x2-11)^2+(x1+x2^2-7)^2)
grr2=deriv(gr.exp,c("x1","x2"))

z=outer(x1,x2,fr2)     ###求外積
image(x1,x2,z)
contour(x1,x2,z,add=T)    ##畫等高線
persp(x1,x2,z)             ###畫三維圖
persp(x1,x2,z,box=T,border=T,theta=45,phi=35) 

x1=x2=seq(-10,10,length=100) 
fr2=function(x){
  x1=x[1]
  x2=x[2] 
  (x1^2+x2-11)^2+(x1+x2^2-7)^2
}
grr=function(x){ x1=x[1]
                 x2=x[2] ###寫一階導運算式（梯度運算式）
                 c(2*(x1^2+x2-11)*2*x1+2*(x1+x2^2-7),
                 2*(x1^2+x2-11)+2*(x1+x2^2-7)*2*x2)
}
z=outer(x1,x2,fr2) ###求外積
image(x1,x2,z)
contour(x1,x2,z,add=T) ##畫等高線 
persp(x1,x2,z) ###畫三維圖
persp(x1,x2,z,box=T,border=T,theta=45,phi=35)

optim(c(-5,-5),fr2,hessian=T)
optim(c(-5,-5),fr2,grr)
optim(c(2,3),fr2,grr)
optim(c(3,-2),fr2,grr)
#####
uimat=rbind(c(1,0),c(0,1))
cimat=c(0,0)
cop=constrOptim(c(0.2,0.5),fr2,grr,ui=uimat,ci=cimat)
cop
##
fr3=function(z){
      x1=exp(z[1])
      x2=exp(z[2])
     (x1^2+x2-11)^2+(x1+x2^2-7)^2
     }
grr3=function(z){
      x1=exp(z[1])
      x2=exp(z[2])
    c(2*(x1^2+x2-11)*2*x1*exp(z[1])+2*(x1+x2^2-7)*exp(z[1]),
          2*(x1^2+x2-11)*exp(z[2])+2*(x1+x2^2-7)*2*x2*exp(z[2]))
     }
   
optran=optim(c(-1.6,-0.7),fr3,grr3)
 ##log(0.2)=-1.6,log(0.5)=-0.7
   exp(optran$par)


