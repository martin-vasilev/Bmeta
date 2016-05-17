# Script for performing a sensitivity analysis on N+2 studies
# using the leave-one-out method

K<-NULL

for(i in 1:nrow(data1)){
  
  data<- data1[-i,]
  
  M_M <-jags.model(JModel("dunif(-50, 50)", "dunif(0, 50)", nrow(data), "N2_M1.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
  M<- coda.samples(M_M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
  sum<- summary(M); sum
  
  K[i]<- sum$statistics[1,1] # Pooled mean
}