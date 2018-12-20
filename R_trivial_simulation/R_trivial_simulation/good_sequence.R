good_sequence <- function(init,n)
{k <- length(init)
total <- 0
new <- c(2, init, 2)
for (i in 1:n){
  index <- 1+sample(1:k,1)
  newbit <- 0+ !new[index]
  if (newbit == 0){
    new[index] <- 0
    total <- total + sum(new)
  }else{
    if(new[index-1]==1|new[index+1]==1){
      total <- total+sum(new)
  }
    else{new[index]<-1
    total<-total+sum(new)}}
}
total/n-4}
m=88
init<-rep(0,m)
y=c()
print(good_sequence(init,10000))
for (i in seq(1,10000,50)){
 y=c(y,good_sequence(init,i)) 
}
options(scipen=999)
library(ggplot2)
x=seq(1,50*length(y),50)
sp=spline(x,y,n=1000)
plot(sp,type='l',xlim=c(0,max(x)),ylim=c(0,max(y)),xlab="times",
ylab="expectaiton",main="converge",
col.main="black",font.main=2)