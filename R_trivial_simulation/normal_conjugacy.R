trials=10^6
d=c(0.01,0,1,0.5,1,5,10,15,20,50,80,100)
x=2
y=3
d=0.1
mu=0
sigma=1
t=2
for (i in trials){
  xt=x+rnorm(0,80)
  if(runif(1)<exp(-(y-xt)^2/(2*sigma^2)-(xt-mu)^2/(2*t^2))/
     exp(-(y-x)^2/(2*sigma^2)-(x-mu)^2/(2*t^2))){
       x=xt
     }
}