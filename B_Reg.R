# Tentstive code for Bayesian random-effects meta-regression with 
# language/ parafoveal mask as a covariate

# Martin R. Vasilev, 2016
  rm(list=ls())

source("JReg.R")
library(rjags)  

load("Data/data6.Rda") # GD, N+1 preview

data6$cov<- NULL

for(i in 1:nrow(data6)){
  if(data6$Language[i]!="Chinese"){
    data6$cov[i]<- -1
  } else{
    data6$cov[i]<- 1
  }
}

data6<- data6[,c(3,4,6)]

# check cases per cov type:
table(data6$cov)



MR1<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(data6), "MR1.txt"),
                 data6, n.chains=3, n.adapt=3000, quiet=FALSE)
R1<- coda.samples(MR1, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum1<- summary(R1);
# sum4; save(sum4, file="Summaries/N2/sum4.Rda") # MAIN

plot(R1, trace=FALSE)
gelman.diag(R1, confidence=0.95); gelman.plot(R1, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R1, smooth=TRUE); 
autocorr.diag(R1); autocorr.plot(R1, lagmax=20); acfplot(R1) 


S<-jags.samples(MR1, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S<-c(S$beta[1,,1],S$beta[1,,2],S$beta[1,,3])
ECDF<- ecdf(S); 1- ECDF(0)


#########
# FFD:  #
#########

load("Data/data5.Rda") # FFD, N+1 preview

data5$cov<- NULL

for(i in 1:nrow(data5)){
  if(data5$Language[i]!="Chinese"){
    data5$cov[i]<- -1
  } else{
    data5$cov[i]<- 1
  }
}

data5<- data5[,c(3,4,6)]

# check cases per cov type:
table(data5$cov)

MR2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(data5), "MR2.txt"),
                 data5, n.chains=3, n.adapt=3000, quiet=FALSE)
R2<- coda.samples(MR2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum2<- summary(R2);
# sum4; save(sum4, file="Summaries/N2/sum4.Rda") # MAIN

plot(R2, trace=FALSE)
gelman.diag(R2, confidence=0.95); gelman.plot(R2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R2, smooth=TRUE); 
autocorr.diag(R2); autocorr.plot(R2, lagmax=20); acfplot(R2) 


S2<-jags.samples(MR2, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S2<-c(S2$beta[1,,1],S2$beta[1,,2],S2$beta[1,,3])
ECDF2<- ecdf(S2); 1- ECDF2(0)


#########
# SFD:  #
#########

load("Data/data7.Rda") # FFD, N+1 preview

data7$cov<- NULL

for(i in 1:nrow(data7)){
  if(data7$Language[i]!="Chinese"){
    data7$cov[i]<- -1
  } else{
    data7$cov[i]<- 1
  }
}

data7<- data7[,c(3,4,6)]

# check cases per cov type:
table(data7$cov)

MR3<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(data7), "MR3.txt"),
                 data7, n.chains=3, n.adapt=3000, quiet=FALSE)
R3<- coda.samples(MR3, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum3<- summary(R3);


S3<-jags.samples(MR3, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S3<-c(S3$beta[1,,1],S3$beta[1,,2],S3$beta[1,,3])
ECDF3<- ecdf(S3); 1- ECDF3(0)



############################
# Parafoveal masks
############################

#1.1. Alphabetical vs Chinese:

load('Data/UNREL_FFD.Rda'); load('Data/UNREL_SFD.Rda')
load('Data/UNREL_GD.Rda')

load('Data/PSEUD_FFD.Rda');load('Data/PSEUD_GD.Rda');

## FFD
UNREL_FFD$cov<- NULL

for(i in 1:nrow(UNREL_FFD)){
  if(UNREL_FFD$Language[i]!="Chinese"){
    UNREL_FFD$cov[i]<- -1
  } else{
    UNREL_FFD$cov[i]<- 1
  }
}

UNREL_FFD<- UNREL_FFD[,c(3,4,6)]
table(UNREL_FFD$cov)

MR4<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(UNREL_FFD), "MR4.txt"),
                 UNREL_FFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R4<- coda.samples(MR4, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum4<- summary(R4);


S4<-jags.samples(MR4, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S4<-c(S4$beta[1,,1],S4$beta[1,,2],S4$beta[1,,3])
ECDF4<- ecdf(S4); 1- ECDF4(0)


## SFD
UNREL_SFD$cov<- NULL

for(i in 1:nrow(UNREL_SFD)){
  if(UNREL_SFD$Language[i]!="Chinese"){
    UNREL_SFD$cov[i]<- -1
  } else{
    UNREL_SFD$cov[i]<- 1
  }
}

UNREL_SFD<- UNREL_SFD[,c(3,4,6)]
table(UNREL_SFD$cov)

MR5<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(UNREL_SFD), "MR5.txt"),
                 UNREL_SFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R5<- coda.samples(MR5, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum5<- summary(R5);


S5<-jags.samples(MR5, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S5<-c(S5$beta[1,,1],S5$beta[1,,2],S5$beta[1,,3])
ECDF5<- ecdf(S5); 1- ECDF5(0)


## GD
UNREL_GD$cov<- NULL

for(i in 1:nrow(UNREL_GD)){
  if(UNREL_GD$Language[i]!="Chinese"){
    UNREL_GD$cov[i]<- -1
  } else{
    UNREL_GD$cov[i]<- 1
  }
}

UNREL_GD<- UNREL_GD[,c(3,4,6)]
table(UNREL_GD$cov)

MR6<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(UNREL_GD), "MR6.txt"),
                 UNREL_GD, n.chains=3, n.adapt=3000, quiet=FALSE)
R6<- coda.samples(MR6, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum6<- summary(R6);


S6<-jags.samples(MR6, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S6<-c(S6$beta[1,,1],S6$beta[1,,2],S6$beta[1,,3])
ECDF6<- ecdf(S6); 1- ECDF6(0)


##########
# PSEUDO #
##########

# FFD:
PSEUD_FFD$cov<- NULL

for(i in 1:nrow(PSEUD_FFD)){
  if(PSEUD_FFD$Language[i]!="Chinese"){
    PSEUD_FFD$cov[i]<- -1
  } else{
    PSEUD_FFD$cov[i]<- 1
  }
}

PSEUD_FFD<- PSEUD_FFD[,c(3,4,6)]
table(PSEUD_FFD$cov)

MR7<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(PSEUD_FFD), "MR7.txt"),
                 PSEUD_FFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R7<- coda.samples(MR7, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum7<- summary(R7);


S7<-jags.samples(MR7, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S7<-c(S7$beta[1,,1],S7$beta[1,,2],S7$beta[1,,3])
ECDF7<- ecdf(S7); 1- ECDF7(0)


# GD:
PSEUD_GD$cov<- NULL

for(i in 1:nrow(PSEUD_GD)){
  if(PSEUD_GD$Language[i]!="Chinese"){
    PSEUD_GD$cov[i]<- -1
  } else{
    PSEUD_GD$cov[i]<- 1
  }
}

PSEUD_GD<- PSEUD_GD[,c(3,4,6)]
table(PSEUD_GD$cov)


MR8<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(PSEUD_GD), "MR8.txt"),
                 PSEUD_GD, n.chains=3, n.adapt=3000, quiet=FALSE)
R8<- coda.samples(MR8, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum8<- summary(R8);


S8<-jags.samples(MR8, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S8<-c(S8$beta[1,,1],S8$beta[1,,2],S8$beta[1,,3])
ECDF8<- ecdf(S8); 1- ECDF8(0)

