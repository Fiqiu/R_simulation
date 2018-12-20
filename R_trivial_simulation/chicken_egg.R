pset=c()
nset=c()
trials=10^3
p=0.5
n=800
x=7
lambda=10
a=1
b=1
for (i in trials){
  p=rbeta(1,x+a,n-x+b)
  y=rpois(1,lambda*(1-p))
  n=x+y
}