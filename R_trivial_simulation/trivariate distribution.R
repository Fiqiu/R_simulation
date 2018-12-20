# trivariate.R
 trials <- 5000
 sim <- matrix(rep(0,3*trials),ncol=3)
 sim[1,] <- c(1,0.5,2)
 for (i in 2:trials) {
 sim[i,1] <- rbinom(1,sim[i-1,3],sim[i-1,2])
 sim[i,2] <- rbeta(1,sim[i,1]+1,sim[i-1,3]-sim[i,1]+1)
 sim[i,3] <- rpois(1,4*(1-sim[i,2])+sim[i,1] )}