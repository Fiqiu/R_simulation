zipf<-function(a)
{trials=10^6
simlist<- numeric(trials)
simlist[1]<-2
for(i in 2:trials){
  if(simlist[i-1]==1){
  first=sample(c(-1,1),1)
  if(first==1){
    simlist[i]=1}
  else{
   p<- (1/2)^(a)
    new<-sample(c(1,2),1,prob=c(1-p,p))
    simlist[i]<-new
  }}
  else{leftright<- sample(c(-1,1),1)
  if(leftright==-1){
    simlist[i]<- simlist[i-1]-1}else{
      p<- (simlist[i-1]/(simlist[i-1]+1))^(a)
      simlist[i]<-sample(c(simlist[i-1],
              1+simlist[i-1]),1,prob=c(1-p,p))
      return(simlist)
    }}}}

tab<-table(zipf(1))/trials
print(tab[1:8])
barplot(tab[1:8],main='the powerlaw limiting distribution',xlab='state',ylab='probability')