good_sequence <- function(init,n)
{k <- length(init)
total <- 0
new <- c(2, init, 2)
good<-0
bad<-0
for (i in 1:n){
  index <- 1+sample(1:k,1)
  if (new[index] == 1){
    new[index] <- 0
    good<- good+ 1
  }else{
    if(new[index-1]==1|new[index+1]==1){
      bad<-bad+1
    }
    else{new[index]<-1
    good<-good+1}}
}
return(bad/(bad+good))}
m=88
init<-rep(0,m)
print(good_sequence(init,100000))