#  Code for Bayesian random-effects meta-regression with 
# language/ parafoveal mask as a covariate for N+1 studies
 
# Martin R. Vasilev, 2016
 
rm(list=ls())

source("JReg.R")
library(rjags)  

###################################################

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

table(data5$cov) # check cases per cov type

MR1<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(data5),
                "N1_R1.txt"), data5, n.chains=3, n.adapt=3000, quiet=FALSE)
R1<- coda.samples(MR1, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR1<- summary(R1); save(sumR1, file="Summaries/N1/sumR1.Rda") # MAIN

MR1_2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dnorm(0, 1.0E-4)", nrow(data5),
                      "N1_R1_2.txt"), data5, n.chains=3, n.adapt=3000, quiet=FALSE)
R1_2<- coda.samples(MR1_2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR1_2<- summary(R1_2); save(sumR1_2, file="Summaries/N1/sumR1_2.Rda") 

# Diagnostics:
plot(R1, trace=FALSE)
gelman.diag(R1, confidence=0.95); gelman.plot(R1, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R1, smooth=TRUE); 
autocorr.diag(R1); autocorr.plot(R1, lagmax=20); acfplot(R1) 

plot(R1_2, trace=FALSE)
gelman.diag(R1_2, confidence=0.95); gelman.plot(R1_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R1_2, smooth=TRUE); 
autocorr.diag(R1_2); autocorr.plot(R1_2, lagmax=20); acfplot(R1_2) 

S1<-jags.samples(MR1, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S1<-c(S1$beta[1,,1],S1$beta[1,,2],S1$beta[1,,3])
ECDF1<- ecdf(S1); 1- ECDF1(0)
save(ECDF1, file= "Summaries/Probs/N1_R1.Rda")



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

MR2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(data7),
                "MR2.txt"), data7, n.chains=3, n.adapt=3000, quiet=FALSE)
R2<- coda.samples(MR2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR2<- summary(R2); save(sumR2, file="Summaries/N1/sumR2.Rda") # MAIN

MR2_2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dnorm(0, 1.0E-4)", nrow(data7),
                      "MR2_2.txt"), data7, n.chains=3, n.adapt=3000, quiet=FALSE)
R2_2<- coda.samples(MR2_2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR2_2<- summary(R2_2); save(sumR2_2, file="Summaries/N1/sumR2_2.Rda") 

# Diagnostics:
plot(R2, trace=FALSE)
gelman.diag(R2, confidence=0.95); gelman.plot(R2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R2, smooth=TRUE); 
autocorr.diag(R2); autocorr.plot(R2, lagmax=20); acfplot(R2) 

plot(R2_2, trace=FALSE)
gelman.diag(R2_2, confidence=0.95); gelman.plot(R2_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R2_2, smooth=TRUE); 
autocorr.diag(R2_2); autocorr.plot(R2_2, lagmax=20); acfplot(R2_2) 

S2<-jags.samples(MR2, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S2<-c(S2$beta[1,,1],S2$beta[1,,2],S2$beta[1,,3])
ECDF2<- ecdf(S2); 1- ECDF2(0)
save(ECDF2, file= "Summaries/Probs/N1_R2.Rda")



#########
#  GD:  #
#########

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

table(data6$cov) # check cases per cov type


MR3<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(data6),
                "MR3.txt"), data6, n.chains=3, n.adapt=3000, quiet=FALSE)
R3<- coda.samples(MR3, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR3<- summary(R3); save(sumR3, file="Summaries/N1/sumR3.Rda") # MAIN

MR3_2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dnorm(0, 1.0E-4)", nrow(data6),
                      "MR3_2.txt"), data6, n.chains=3, n.adapt=3000, quiet=FALSE)
R3_2<- coda.samples(MR3_2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR3_2<- summary(R3_2); save(sumR3_2, file="Summaries/N1/sumR3_2.Rda") 

# Diagnostics:
plot(R3, trace=FALSE)
gelman.diag(R3, confidence=0.95); gelman.plot(R3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R3, smooth=TRUE); 
autocorr.diag(R3); autocorr.plot(R3, lagmax=20); acfplot(R3) 

plot(R3_2, trace=FALSE)
gelman.diag(R3_2, confidence=0.95); gelman.plot(R3_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R3_2, smooth=TRUE); 
autocorr.diag(R3_2); autocorr.plot(R3_2, lagmax=20); acfplot(R3_2) 


S3<-jags.samples(MR3, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S3<-c(S3$beta[1,,1],S3$beta[1,,2],S3$beta[1,,3])
ECDF3<- ecdf(S3); 1- ECDF3(0)
save(ECDF3, file= "Summaries/Probs/N1_R3.Rda")


#########################################################
# Alphabetical vs Chinese: effect of parafoveal masks   #
#########################################################

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

MR4<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(UNREL_FFD),
                "MR4.txt"), UNREL_FFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R4<- coda.samples(MR4, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR4<- summary(R4); save(sumR4, file="Summaries/N1/sumR4.Rda") # MAIN

MR4_2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dnorm(0, 1.0E-4)", nrow(UNREL_FFD),
                      "MR4_2.txt"), UNREL_FFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R4_2<- coda.samples(MR4_2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR4_2<- summary(R4_2); save(sumR4_2, file="Summaries/N1/sumR4_2.Rda")

# Diagnostics:
plot(R4, trace=FALSE)
gelman.diag(R4, confidence=0.95); gelman.plot(R4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R4, smooth=TRUE); 
autocorr.diag(R4); autocorr.plot(R4, lagmax=20); acfplot(R4) 

plot(R4_2, trace=FALSE)
gelman.diag(R4_2, confidence=0.95); gelman.plot(R4_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R4_2, smooth=TRUE); 
autocorr.diag(R4_2); autocorr.plot(R4_2, lagmax=20); acfplot(R4_2) 


S4<-jags.samples(MR4, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S4<-c(S4$beta[1,,1],S4$beta[1,,2],S4$beta[1,,3])
ECDF4<- ecdf(S4); 1- ECDF4(0)
save(ECDF4, file= "Summaries/Probs/N1_R4.Rda")


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

MR5<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(UNREL_SFD),
                "MR5.txt"), UNREL_SFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R5<- coda.samples(MR5, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR5<- summary(R5); save(sumR5, file="Summaries/N1/sumR5.Rda") # MAIN

MR5_2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dnorm(0, 1.0E-4)", nrow(UNREL_SFD),
                      "MR5_2.txt"), UNREL_SFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R5_2<- coda.samples(MR5_2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR5_2<- summary(R5_2); save(sumR5_2, file="Summaries/N1/sumR5_2.Rda") 

# Diagnostics:
plot(R5, trace=FALSE)
gelman.diag(R5, confidence=0.95); gelman.plot(R5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R5, smooth=TRUE); 
autocorr.diag(R5); autocorr.plot(R5, lagmax=20); acfplot(R5) 

plot(R5_2, trace=FALSE)
gelman.diag(R5_2, confidence=0.95); gelman.plot(R5_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R5_2, smooth=TRUE); 
autocorr.diag(R5_2); autocorr.plot(R5_2, lagmax=20); acfplot(R5_2) 

S5<-jags.samples(MR5, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S5<-c(S5$beta[1,,1],S5$beta[1,,2],S5$beta[1,,3])
ECDF5<- ecdf(S5); 1- ECDF5(0)
save(ECDF5, file= "Summaries/Probs/N1_R5.Rda")


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

MR6<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(UNREL_GD),
                 "MR6.txt"), UNREL_GD, n.chains=3, n.adapt=3000, quiet=FALSE)
R6<- coda.samples(MR6, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR6<- summary(R6); save(sumR6, file="Summaries/N1/sumR6.Rda") # MAIN

MR6_2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dnorm(0, 1.0E-4)", nrow(UNREL_GD),
                      "MR6_2.txt"), UNREL_GD, n.chains=3, n.adapt=3000, quiet=FALSE)
R6_2<- coda.samples(MR6_2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR6_2<- summary(R6_2); save(sumR6_2, file="Summaries/N1/sumR6_2.Rda")

# Diagnostics:
plot(R6, trace=FALSE)
gelman.diag(R6, confidence=0.95); gelman.plot(R6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R6, smooth=TRUE); 
autocorr.diag(R6); autocorr.plot(R6, lagmax=20); acfplot(R6) 

plot(R6_2, trace=FALSE)
gelman.diag(R6_2, confidence=0.95); gelman.plot(R6_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R6_2, smooth=TRUE); 
autocorr.diag(R6_2); autocorr.plot(R6_2, lagmax=20); acfplot(R6_2) 

S6<-jags.samples(MR6, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S6<-c(S6$beta[1,,1],S6$beta[1,,2],S6$beta[1,,3])
ECDF6<- ecdf(S6); 1- ECDF6(0)
save(ECDF6, file= "Summaries/Probs/N1_R6.Rda")

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

MR7<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(PSEUD_FFD),
                "MR7.txt"), PSEUD_FFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R7<- coda.samples(MR7, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR7<- summary(R7); save(sumR7, file="Summaries/N1/sumR7.Rda") # MAIN

MR7_2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dnorm(0, 1.0E-4)", nrow(PSEUD_FFD),
                      "MR7_2.txt"), PSEUD_FFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R7_2<- coda.samples(MR7_2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR7_2<- summary(R7_2); save(sumR7_2, file="Summaries/N1/sumR7_2.Rda")

# Diagnostics:
plot(R7, trace=FALSE)
gelman.diag(R7, confidence=0.95); gelman.plot(R7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R7, smooth=TRUE); 
autocorr.diag(R7); autocorr.plot(R7, lagmax=20); acfplot(R7) 

plot(R7_2, trace=FALSE)
gelman.diag(R7_2, confidence=0.95); gelman.plot(R7_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R7_2, smooth=TRUE); 
autocorr.diag(R7_2); autocorr.plot(R7_2, lagmax=20); acfplot(R7_2) 

S7<-jags.samples(MR7, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S7<-c(S7$beta[1,,1],S7$beta[1,,2],S7$beta[1,,3])
ECDF7<- ecdf(S7); 1- ECDF7(0)
save(ECDF7, file= "Summaries/Probs/N1_R7.Rda")



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


MR8<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(PSEUD_GD),
                "MR8.txt"), PSEUD_GD, n.chains=3, n.adapt=3000, quiet=FALSE)
R8<- coda.samples(MR8, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR8<- summary(R8); save(sumR8, file="Summaries/N1/sumR8.Rda") # MAIN

MR8_2<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dnorm(0, 1.0E-4)", nrow(PSEUD_GD),
                      "MR8_2.txt"), PSEUD_GD, n.chains=3, n.adapt=3000, quiet=FALSE)
R8_2<- coda.samples(MR8_2, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sumR8_2<- summary(R8_2); save(sumR8_2, file="Summaries/N1/sumR8_2.Rda")

# Diagnostics:
plot(R8, trace=FALSE)
gelman.diag(R8, confidence=0.95); gelman.plot(R8, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R8, smooth=TRUE); 
autocorr.diag(R8); autocorr.plot(R8, lagmax=20); acfplot(R8) 

plot(R8_2, trace=FALSE)
gelman.diag(R8_2, confidence=0.95); gelman.plot(R8_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R8_2, smooth=TRUE); 
autocorr.diag(R8_2); autocorr.plot(R8_2, lagmax=20); acfplot(R8_2) 

S8<-jags.samples(MR8, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
S8<-c(S8$beta[1,,1],S8$beta[1,,2],S8$beta[1,,3])
ECDF8<- ecdf(S8); 1- ECDF8(0)
save(ECDF8, file= "Summaries/Probs/N1_R8.Rda")

