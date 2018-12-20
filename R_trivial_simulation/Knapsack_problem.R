gj=c(6,3,5,4,6)
wj=c(2,2,6,5,4)
m=5
w=10
chain=c(0,0,0,0,0)
trials=2000
beta=0.5
for (i in 1:trials){
chain2=chain
rand=sample(1:5,1)
chain2[rand]=1-chain[rand]
rand=sample(sequence(1:length(chain)),1)
weight=wj*chain
value1=0
value2=0
if (weight<=w){ value1=gj*chain
value2=gj*chain2
if(runif(1)<exp(beta*(value2-value1))){
  chain=chain2
}}

}
print(chain)