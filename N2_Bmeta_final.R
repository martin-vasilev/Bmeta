# Martin Vasilev, 2015

# This is the script for random-effects Bayesian meta-analysis of N+2 preview effects

# !NOTE: In this version I use uniform prior distributions for mu and tau

rm(list=ls())

library(rjags)
library(ggplot2)


ord= c(1,2,3,7,4,5,8,11,10,6,9) # order studies chronologically
ch= c(5,6,7,10) # chinese studies

#-------------
# Load data: 
#-------------

# I load the data that I have coded in separate files: 
# For info about coding of individual articles, see "All_papers.xls" 

#setwd("C:/Users/Martin Vasilev/Dropbox/Research/Statistics/R/Master thesis/Bmeta/Bmeta")
load('Data/ES_N2.Rda')
load('Data/dataN2.Rda')


#---------------------
# Visual inspection: 
#---------------------

# N+2 Preview effects on word N+2:

#Cohen's d
boxplot(ES_N2$FFD_es, ES_N2$Gaze_es, xlab= 'FFD                   GD',
        ylab= 'Effect size (Cohens d)', main="N+2 preview effects on word N+2")

# ms
boxplot(ES_N2$FFD_ms, ES_N2$Gaze_ms, xlab= 'FFD                   GD',
        ylab= 'Mean difference (in ms)', main="N+2 preview effects on word N+2")

# N+2 Preview effects on word N+1:

#Cohen's d
boxplot(ES_N2$FFD_es_N1, ES_N2$Gaze_es_N1, xlab= 'FFD                    GD',
        ylab= 'Effect size (Cohens d)', main="N+2 preview effects on word N+1")
# ms
boxplot(ES_N2$FFD_ms_N1, ES_N2$Gaze_ms_N1, xlab= 'FFD                    GD',
        ylab= 'Mean difference (in ms)', main="N+2 preview effects on word N+1")

# Violin plot
library(vioplot)
vioplot(ES_N2$FFD_ms, ES_N2$Gaze_ms)

#-------------------------
# Prepare data for rJAGS:
#-------------------------
source("get_var.R")

V<- get_var(dataN2$FFD_N2_val_SD, dataN2$FFD_N2_inval_SD, dataN2$N)
T<-ES_N2$FFD_ms

data1<-data.frame(T,V)
colnames(data1)<- c('T','S.sqr')
FFD<- data1 
FFD$cov<- dataN2$N1len
data1<- data1[ord,] # re-order data
FFD<- FFD[ord,] # re-order data

# Alphabetical-only:
data1a<-data1[-ch,]


# N+2 effects on word N+2 (GD):
V<- NULL
V<- get_var(dataN2$Gaze_N2_val_SD, dataN2$Gaze_N2_inval_SD, dataN2$N)
T<-ES_N2$Gaze_ms

data2<-data.frame(T,V[])
colnames(data2)<- c('T','S.sqr')
GD<- data2
GD$cov<- dataN2$N1len
data2<- data2[ord,] # re-order data
GD<- GD[ord,] # re-order data

# Alphabetical-only:
data2a<-data2[-ch,]

# N+2 effects on word N+1 (FFD):
V<- NULL
V<- get_var(dataN2$FFD_N1_val_SD, dataN2$FFD_N1_inval_SD, dataN2$N)
T<-ES_N2$FFD_ms_N1

data3<-data.frame(T,V[])
colnames(data3)<- c('T','S.sqr')
data3<- subset(data3, T!="NA") # to remove studies that did not report word N+1 statistics

# N+2 effects on word N+1 (GD):
V<- NULL
V<- get_var(dataN2$Gaze_N1_val_SD, dataN2$Gaze_N1_inval_SD, dataN2$N)
T<-ES_N2$Gaze_ms_N1

data4<-data.frame(T,V[])
colnames(data4)<- c('T','S.sqr')
data4<- subset(data4, T!="NA")

rm(T); rm(V)

save(data1, file="Data/data1.Rda")
save(data1a, file="Data/data1a.Rda")
save(data2, file="Data/data2.Rda")
save(data2a, file="Data/data2a.Rda")
save(data3, file="Data/data3.Rda")
save(data4, file="Data/data4.Rda")

#---------------
# rJAGS models:
#---------------
source("JModel.R")

####################################################
# M1 (N+2 Effects on word N+2, measured with FFD): #
####################################################
M1_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data1), "N2_M1.txt"),
                   data1, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M1<- coda.samples(M1_M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum1<- summary(M1); sum1; save(sum1, file="Summaries/N2/sum1.Rda")
M1_2M <-jags.model(JModel("dunif(-50, 50)", "dunif(0, 50)", nrow(data1), "N2_M1_2.txt"),
                  data1, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=1, "tau"=5)) # different starting values
M1_2<- coda.samples(M1_2M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum1_2<- summary(M1_2); 
M1_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0,200)", nrow(data1), "N2_M1_3.txt"),
                  data1, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_3<- coda.samples(M1_3M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum1_3<- summary(M1_3); save(sum1_3, file="Summaries/N2/sum1_3.Rda")
M1_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data1), "N2_M1_4.txt"),
                  data1, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_4<- coda.samples(M1_4M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum1_4<- summary(M1_4); save(sum1_4, file="Summaries/N2/sum1_4.Rda")
M1_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data1), "N2_M1_5.txt"),
                  data1, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_5<- coda.samples(M1_5M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum1_5<- summary(M1_5); save(sum1_5, file="Summaries/N2/sum1_5.Rda")
M1_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data1), "N2_M1_6.txt"),
                   data1, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_6<- coda.samples(M1_6M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum1_6<- summary(M1_6); save(sum1_6, file="Summaries/N2/sum1_6.Rda"); 
M1_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data1), "N2_M1_7.txt"),
                   data1, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_7<- coda.samples(M1_7M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum1_7<- summary(M1_7); save(sum1_7, file="Summaries/N2/sum1_7.Rda"); sum1_7

# Diagnostics
plot(M1, trace=FALSE)
gelman.diag(M1, confidence=0.95); gelman.plot(M1, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1, smooth=TRUE); 
autocorr.diag(M1); autocorr.plot(M1, lagmax=20); acfplot(M1) # auto-correlations

plot(M1_2, trace=FALSE)
gelman.diag(M1_2, confidence=0.95); gelman.plot(M1_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_2, smooth=TRUE); 
autocorr.diag(M1_2); autocorr.plot(M1_2, lagmax=20); acfplot(M1_2) # auto-correlations

plot(M1_3, trace=FALSE)
gelman.diag(M1_3, confidence=0.95); gelman.plot(M1_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_3, smooth=TRUE); 
autocorr.diag(M1_3); autocorr.plot(M1_3, lagmax=20); acfplot(M1_3) # auto-correlations

plot(M1_4, trace=FALSE)
gelman.diag(M1_4, confidence=0.95); gelman.plot(M1_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_4, smooth=TRUE); 
autocorr.diag(M1_4); autocorr.plot(M1_4, lagmax=20); acfplot(M1_4) # auto-correlations

plot(M1_5, trace=FALSE)
gelman.diag(M1_5, confidence=0.95); gelman.plot(M1_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_5, smooth=TRUE); 
autocorr.diag(M1_5); autocorr.plot(M1_5, lagmax=20); acfplot(M1_5) # auto-correlations

plot(M1_6, trace=FALSE)
gelman.diag(M1_6, confidence=0.95); gelman.plot(M1_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_6, smooth=TRUE); 
autocorr.diag(M1_6); autocorr.plot(M1_6, lagmax=20); acfplot(M1_6) # auto-correlations

plot(M1_7, trace=FALSE)
gelman.diag(M1_7, confidence=0.95); gelman.plot(M1_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_7, smooth=TRUE); 
autocorr.diag(M1_7); autocorr.plot(M1_7, lagmax=20); acfplot(M1_7) # auto-correlations


###############################################################################
# M2 (N+2 Effects on word N+2, measured with FFD, only alphabetical studies): #
###############################################################################
M2_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data1a), "N2_M2.txt"),
                  data1a, n.chains=3, n.adapt=3000, quiet=FALSE)
M2<- coda.samples(M2_M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum2<- summary(M2); sum2; save(sum2, file="Summaries/N2/sum2.Rda") # MAIN
M2_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data1a), "N2_M2_2.txt"),
                  data1a, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=1, "tau"=5))
M2_2<- coda.samples(M2_2M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum2_2<- summary(M2_2); 
M2_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data1a), "N2_M2_3.txt"),
                  data1a, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_3<- coda.samples(M2_3M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum2_3<- summary(M2_3); save(sum2_3, file="Summaries/N2/sum2_3.Rda")
M2_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data1a), "N2_M2_4.txt"),
                  data1a, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_4<- coda.samples(M2_4M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum2_4<- summary(M2_4); save(sum2_4, file="Summaries/N2/sum2_4.Rda")
M2_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data1a), "N2_M2_5.txt"),
                  data1a, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_5<- coda.samples(M2_5M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum2_5<- summary(M2_5); save(sum2_5, file="Summaries/N2/sum2_5.Rda")
M2_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data1a), "N2_M2_6.txt"),
                   data1a, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_6<- coda.samples(M2_6M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum2_6<- summary(M2_6); save(sum2_6, file="Summaries/N2/sum2_6.Rda")
M2_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data1a), "N2_M2_7.txt"),
                   data1a, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_7<- coda.samples(M2_7M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum2_7<- summary(M2_7); save(sum2_7, file="Summaries/N2/sum2_7.Rda")

# Diagnostics
plot(M2, trace=FALSE)
gelman.diag(M2, confidence=0.95); gelman.plot(M2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2, smooth=TRUE); 
autocorr.diag(M2); autocorr.plot(M2, lagmax=20); acfplot(M2) # auto-correlations

plot(M2_2, trace=FALSE)
gelman.diag(M2_2, confidence=0.95); gelman.plot(M2_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_2, smooth=TRUE); 
autocorr.diag(M2_2); autocorr.plot(M2_2, lagmax=20); acfplot(M2_2) # auto-correlations

plot(M2_3, trace=FALSE)
gelman.diag(M2_3, confidence=0.95); gelman.plot(M2_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_3, smooth=TRUE); 
autocorr.diag(M2_3); autocorr.plot(M2_3, lagmax=20); acfplot(M2_3) # auto-correlations

plot(M2_4, trace=FALSE)
gelman.diag(M2_4, confidence=0.95); gelman.plot(M2_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_4, smooth=TRUE); 
autocorr.diag(M2_4); autocorr.plot(M2_4, lagmax=20); acfplot(M2_4) # auto-correlations

plot(M2_5, trace=FALSE)
gelman.diag(M2_5, confidence=0.95); gelman.plot(M2_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_5, smooth=TRUE); 
autocorr.diag(M2_5); autocorr.plot(M2_5, lagmax=20); acfplot(M2_5) # auto-correlations

plot(M2_6, trace=FALSE)
gelman.diag(M2_6, confidence=0.95); gelman.plot(M2_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_6, smooth=TRUE); 
autocorr.diag(M2_6); autocorr.plot(M2_6, lagmax=20); acfplot(M2_6) # auto-

plot(M2_7, trace=FALSE)
gelman.diag(M2_7, confidence=0.95); gelman.plot(M2_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_7, smooth=TRUE); 
autocorr.diag(M2_7); autocorr.plot(M2_7, lagmax=20); acfplot(M2_7) # auto-correlations
#----------------


###################################################
# M3 (N+2 Effects on word N+2, measured with GD): #
###################################################
M3_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data2), "N2_M3.txt"),
                  data2, n.chains=3, n.adapt=3000, quiet=FALSE)
M3<- coda.samples(M3_M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum3<- summary(M3); sum3; save(sum3, file="Summaries/N2/sum3.Rda") # MAIN
M3_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data2), "N2_M3_2.txt"),
                  data2, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=1, "tau"=5))
M3_2<- coda.samples(M3_2M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum3_2<- summary(M3_2); save(sum3_2, file="Summaries/N2/sum3_2.Rda") 
M3_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data2), "N2_M3_3.txt"),
                  data2, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_3<- coda.samples(M3_3M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum3_3<- summary(M3_3); save(sum3_3, file="Summaries/N2/sum3_3.Rda") 
M3_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data2), "N2_M3_4.txt"),
                  data2, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_4<- coda.samples(M3_4M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum3_4<- summary(M3_4); save(sum3_4, file="Summaries/N2/sum3_4.Rda")
M3_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data2), "N2_M3_5.txt"),
                  data2, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_5<- coda.samples(M3_5M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum3_5<- summary(M3_5); save(sum3_5, file="Summaries/N2/sum3_5.Rda")
M3_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data2), "N2_M3_6.txt"),
                   data2, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_6<- coda.samples(M3_6M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum3_6<- summary(M3_6); save(sum3_6, file="Summaries/N2/sum3_6.Rda")
M3_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data2), "N2_M3_7.txt"),
                   data2, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_7<- coda.samples(M3_7M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum3_7<- summary(M3_7); save(sum3_7, file="Summaries/N2/sum3_7.Rda")

    # Diagnostics
plot(M3, trace=FALSE)
gelman.diag(M3, confidence=0.95); gelman.plot(M3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3, smooth=TRUE); 
autocorr.diag(M3); autocorr.plot(M3, lagmax=20); acfplot(M3) # auto-correlations

plot(M3_2, trace=FALSE)
gelman.diag(M3_2, confidence=0.95); gelman.plot(M3_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_2, smooth=TRUE); 
autocorr.diag(M3_2); autocorr.plot(M3_2, lagmax=20); acfplot(M3_2) # auto-correlations

plot(M3_3, trace=FALSE)
gelman.diag(M3_3, confidence=0.95); gelman.plot(M3_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_3, smooth=TRUE); 
autocorr.diag(M3_3); autocorr.plot(M3_3, lagmax=20); acfplot(M3_3) # auto-correlations

plot(M3_4, trace=FALSE)
gelman.diag(M3_4, confidence=0.95); gelman.plot(M3_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_4, smooth=TRUE); 
autocorr.diag(M3_4); autocorr.plot(M3_4, lagmax=20); acfplot(M3_4) # auto-correlations

plot(M3_5, trace=FALSE)
gelman.diag(M3_5, confidence=0.95); gelman.plot(M3_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_5, smooth=TRUE); 
autocorr.diag(M3_5); autocorr.plot(M3_5, lagmax=20); acfplot(M3_5) # auto-correlations

plot(M3_6, trace=FALSE)
gelman.diag(M3_6, confidence=0.95); gelman.plot(M3_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_6, smooth=TRUE); 
autocorr.diag(M3_6); autocorr.plot(M3_6, lagmax=20); acfplot(M3_6) # auto-correlations

plot(M3_7, trace=FALSE)
gelman.diag(M3_7, confidence=0.95); gelman.plot(M3_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_7, smooth=TRUE); 
autocorr.diag(M3_7); autocorr.plot(M3_7, lagmax=20); acfplot(M3_7) # auto-correlations

#########################################################################
# M4 (N+2 Effects on word N+2, measured with GD, alphabetical studies): #
#########################################################################
M4_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data2a), "N2_M4.txt"),
                  data2a, n.chains=3, n.adapt=3000, quiet=FALSE)
M4<- coda.samples(M4_M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum4<- summary(M4); sum4; save(sum4, file="Summaries/N2/sum4.Rda") # MAIN
M4_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data2a), "N2_M4_2.txt"),
                  data2a, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=10, "tau"=10))
M4_2<- coda.samples(M4_2M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum4_2<- summary(M4_2); save(sum4_2, file="Summaries/N2/sum4_2.Rda") 
M4_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data2a), "N2_M4_3.txt"),
                  data2a, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_3<- coda.samples(M4_3M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum4_3<- summary(M4_3); save(sum4_3, file="Summaries/N2/sum4_3.Rda")
M4_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data2a), "N2_M4_4.txt"),
                  data2a, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_4<- coda.samples(M4_4M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum4_4<- summary(M4_4); save(sum4_4, file="Summaries/N2/sum4_4.Rda")
M4_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data2a), "N2_M4_5.txt"),
                  data2a, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_5<- coda.samples(M4_5M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum4_5<- summary(M4_5); save(sum4_5, file="Summaries/N2/sum4_5.Rda")
M4_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data2a), "N2_M4_6.txt"),
                   data2a, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_6<- coda.samples(M4_6M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum4_6<- summary(M4_6); save(sum4_6, file="Summaries/N2/sum4_6.Rda")
M4_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data2a), "N2_M4_7.txt"),
                   data2a, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_7<- coda.samples(M4_7M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum4_7<- summary(M4_7); save(sum4_7, file="Summaries/N2/sum4_7.Rda")


# Diagnostics
plot(M4, trace=FALSE)
gelman.diag(M4, confidence=0.95); gelman.plot(M4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4, smooth=TRUE); 
autocorr.diag(M4); autocorr.plot(M4, lagmax=20); acfplot(M4) # auto-correlations

plot(M4_2, trace=FALSE)
gelman.diag(M4_2, confidence=0.95); gelman.plot(M4_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_2, smooth=TRUE); 
autocorr.diag(M4_2); autocorr.plot(M4_2, lagmax=20); acfplot(M4_2) # auto-correlations

plot(M4_3, trace=FALSE)
gelman.diag(M4_3, confidence=0.95); gelman.plot(M4_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_3, smooth=TRUE); 
autocorr.diag(M4_3); autocorr.plot(M4_3, lagmax=20); acfplot(M4_3) # auto-correlations

plot(M4_4, trace=FALSE)
gelman.diag(M4_4, confidence=0.95); gelman.plot(M4_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_4, smooth=TRUE); 
autocorr.diag(M4_4); autocorr.plot(M4_4, lagmax=20); acfplot(M4_4) # auto-correlations

plot(M4_5, trace=FALSE)
gelman.diag(M4_5, confidence=0.95); gelman.plot(M4_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_5, smooth=TRUE); 
autocorr.diag(M4_5); autocorr.plot(M4_5, lagmax=20); acfplot(M4_5) # auto-correlations

plot(M4_6, trace=FALSE)
gelman.diag(M4_6, confidence=0.95); gelman.plot(M4_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_6, smooth=TRUE); 
autocorr.diag(M4_6); autocorr.plot(M4_6, lagmax=20); acfplot(M4_6) # auto-correlations

plot(M4_7, trace=FALSE)
gelman.diag(M4_7, confidence=0.95); gelman.plot(M4_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_7, smooth=TRUE); 
autocorr.diag(M4_7); autocorr.plot(M4_7, lagmax=20); acfplot(M4_7) # auto-correlations


####################################################
# M5 (N+2 Effects on word N+1, measured with FFD): #
####################################################
M5_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data3), "N2_M5.txt"),
                  data3, n.chains=3, n.adapt=3000, quiet=FALSE)
M5<- coda.samples(M5_M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum5<- summary(M5); sum5; save(sum5, file="Summaries/N2/sum5.Rda") # MAIN
M5_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data3), "N2_M5_2.txt"),
                  data3, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=1, "tau"=5))
M5_2<- coda.samples(M5_2M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum5_2<- summary(M5_2); save(sum5_2, file="Summaries/N2/sum5_2.Rda")
M5_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data3), "N2_M5_3.txt"),
                  data3, n.chains=3, n.adapt=3000, quiet=FALSE)
M5_3<- coda.samples(M5_3M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum5_3<- summary(M5_3); save(sum5_3, file="Summaries/N2/sum5_3.Rda")
M5_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data3), "N2_M5_4.txt"),
                  data3, n.chains=3, n.adapt=3000, quiet=FALSE)
M5_4<- coda.samples(M5_4M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum5_4<- summary(M5_4); save(sum5_4, file="Summaries/N2/sum5_4.Rda")
M5_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data3), "N2_M5_5.txt"),
                  data3, n.chains=3, n.adapt=3000, quiet=FALSE)
M5_5<- coda.samples(M5_5M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum5_5<- summary(M5_5); save(sum5_5, file="Summaries/N2/sum5_5.Rda")
M5_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data3), "N2_M5_6.txt"),
                   data3, n.chains=3, n.adapt=3000, quiet=FALSE)
M5_6<- coda.samples(M5_6M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum5_6<- summary(M5_6); save(sum5_6, file="Summaries/N2/sum5_6.Rda")
M5_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data3), "N2_M5_7.txt"),
                   data3, n.chains=3, n.adapt=3000, quiet=FALSE)
M5_7<- coda.samples(M5_7M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum5_7<- summary(M5_7); save(sum5_7, file="Summaries/N2/sum5_7.Rda")

# Diagnostics
plot(M5, trace=FALSE)
gelman.diag(M5, confidence=0.95); gelman.plot(M5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5, smooth=TRUE); 
autocorr.diag(M5); autocorr.plot(M5, lagmax=20); acfplot(M5) # auto-correlations

plot(M5_2, trace=FALSE)
gelman.diag(M5_2, confidence=0.95); gelman.plot(M5_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5_2, smooth=TRUE); 
autocorr.diag(M5_2); autocorr.plot(M5_2, lagmax=20); acfplot(M5_2) # auto-correlations

plot(M5_3, trace=FALSE)
gelman.diag(M5_3, confidence=0.95); gelman.plot(M5_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5_3, smooth=TRUE); 
autocorr.diag(M5_3); autocorr.plot(M5_3, lagmax=20); acfplot(M5_3) # auto-correlations

plot(M5_4, trace=FALSE)
gelman.diag(M5_4, confidence=0.95); gelman.plot(M5_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5_4, smooth=TRUE); 
autocorr.diag(M5_4); autocorr.plot(M5_4, lagmax=20); acfplot(M5_4) # auto-correlations

plot(M5_5, trace=FALSE)
gelman.diag(M5_5, confidence=0.95); gelman.plot(M5_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5_5, smooth=TRUE); 
autocorr.diag(M5_5); autocorr.plot(M5_5, lagmax=20); acfplot(M5_5) # auto-correlations

plot(M5_6, trace=FALSE)
gelman.diag(M5_6, confidence=0.95); gelman.plot(M5_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5_6, smooth=TRUE); 
autocorr.diag(M5_6); autocorr.plot(M5_6, lagmax=20); acfplot(M5_6) # auto-correlations

plot(M5_7, trace=FALSE)
gelman.diag(M5_7, confidence=0.95); gelman.plot(M5_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5_7, smooth=TRUE); 
autocorr.diag(M5_7); autocorr.plot(M5_7, lagmax=20); acfplot(M5_7) # auto-correlations

###################################################
# M6 (N+2 Effects on word N+1, measured with GD): #
###################################################
M6_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data4), "N2_M6.txt"),
                  data4, n.chains=3, n.adapt=3000, quiet=FALSE)
M6<- coda.samples(M6_M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum6<- summary(M6); sum6; save(sum6, file="Summaries/N2/sum6.Rda") # MAIN
M6_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data4), "N2_M6_2.txt"),
                  data4, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=5, "tau"=10))
M6_2<- coda.samples(M6_2M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum6_2<- summary(M6_2); save(sum6_2, file="Summaries/N2/sum6_2.Rda")
M6_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data4), "N2_M6_3.txt"),
                  data4, n.chains=3, n.adapt=3000, quiet=FALSE)
M6_3<- coda.samples(M6_3M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum6_3<- summary(M6_3); save(sum6_3, file="Summaries/N2/sum6_3.Rda")
M6_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data4), "N2_M6_4.txt"),
                  data4, n.chains=3, n.adapt=3000, quiet=FALSE)
M6_4<- coda.samples(M6_4M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum6_4<- summary(M6_4); save(sum6_4, file="Summaries/N2/sum6_4.Rda")
M6_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data4), "N2_M6_5.txt"),
                  data4, n.chains=3, n.adapt=3000, quiet=FALSE)
M6_5<- coda.samples(M6_5M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum6_5<- summary(M6_5); save(sum6_5, file="Summaries/N2/sum6_5.Rda")
M6_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data4), "N2_M6_6.txt"),
                   data4, n.chains=3, n.adapt=3000, quiet=FALSE)
M6_6<- coda.samples(M6_6M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum6_6<- summary(M6_6); save(sum6_6, file="Summaries/N2/sum6_6.Rda")
M6_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data4), "N2_M6_7.txt"),
                   data4, n.chains=3, n.adapt=3000, quiet=FALSE)
M6_7<- coda.samples(M6_7M, c('mu', 'tau','theta'), n.iter=75000, thin=5)
sum6_7<- summary(M6_7); save(sum6_7, file="Summaries/N2/sum6_7.Rda")


# Diagnostics
plot(M6, trace=FALSE)
gelman.diag(M6, confidence=0.95); gelman.plot(M6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6, smooth=TRUE); 
autocorr.diag(M6); autocorr.plot(M6, lagmax=20); acfplot(M6) # auto-correlations

plot(M6_2, trace=FALSE)
gelman.diag(M6_2, confidence=0.95); gelman.plot(M6_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6_2, smooth=TRUE); 
autocorr.diag(M6_2); autocorr.plot(M6_2, lagmax=20); acfplot(M6_2) # auto-correlations

plot(M6_3, trace=FALSE)
gelman.diag(M6_3, confidence=0.95); gelman.plot(M6_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6_3, smooth=TRUE); 
autocorr.diag(M6_3); autocorr.plot(M6_3, lagmax=20); acfplot(M6_3) # auto-correlations

plot(M6_4, trace=FALSE)
gelman.diag(M6_4, confidence=0.95); gelman.plot(M6_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6_4, smooth=TRUE); 
autocorr.diag(M6_4); autocorr.plot(M6_4, lagmax=20); acfplot(M6_4) # auto-correlations

plot(M6_5, trace=FALSE)
gelman.diag(M6_5, confidence=0.95); gelman.plot(M6_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6_5, smooth=TRUE); 
autocorr.diag(M6_5); autocorr.plot(M6_5, lagmax=20); acfplot(M6_5) # auto-correlations

plot(M6_6, trace=FALSE)
gelman.diag(M6_6, confidence=0.95); gelman.plot(M6_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6_6, smooth=TRUE); 
autocorr.diag(M6_6); autocorr.plot(M6_6, lagmax=20); acfplot(M6_6) # auto-correlations

plot(M6_7, trace=FALSE)
gelman.diag(M6_7, confidence=0.95); gelman.plot(M6_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6_7, smooth=TRUE); 
autocorr.diag(M6_7); autocorr.plot(M6_7, lagmax=20); acfplot(M6_7) # auto-correlations

#----------------------
# Sensitivity analysis:
#----------------------
source('sensitivity_analysis.R')

# N+2 effects on word N+2, FFD:
Sens1<- sensitivity_analysis(data1)
save(Sens1, file= "Summaries/Sensitivity/N2_Sens1.Rda")

# N+2 effects on word N+2, GD:
Sens2<- sensitivity_analysis(data2)
save(Sens2, file= "Summaries/Sensitivity/N2_Sens2.Rda")

# N+2 effects on word N+1, FFD:
Sens3<- sensitivity_analysis(data3)
save(Sens3, file= "Summaries/Sensitivity/N2_Sens3.Rda")

# N+2 effects on word N+1, FFD:
Sens4<- sensitivity_analysis(data4)
save(Sens4, file= "Summaries/Sensitivity/N2_Sens4.Rda")

#############
# Save descriptives:

load('Summaries/N2/sum1.Rda');load('Summaries/N2/sum2.Rda');
load('Summaries/N2/sum3.Rda');load('Summaries/N2/sum4.Rda');

# M1:
M1_meanP<- sum1$statistics[1,1] # Pooled mean
M1_meanTau<-  sum1$statistics[2,1] #  Tau
M1_theta<- unname(sum1$statistics[3:13,1]) # Theta from individual studies
M1_MuCrI<- c(sum1$quantiles[1,1], sum1$quantiles[1,5])
M1_TauCrI<- c(sum1$quantiles[2,1], sum1$quantiles[2,5])
M1_thetaCrI<- matrix(list(), nrow=11, ncol=2); 
M1_thetaCrI[,1] <- sum1$quantiles[3:13,1]
M1_thetaCrI[,2] <- sum1$quantiles[3:13,5]

# M2:
M2_meanP<- sum2$statistics[1,1] # Pooled mean
M2_meanTau<-  sum2$statistics[2,1] #  Tau
M2_theta<- unname(sum2$statistics[3:9,1]) # Theta from individual studies
M2_MuCrI<- c(sum2$quantiles[1,1], sum2$quantiles[1,5])
M2_TauCrI<- c(sum2$quantiles[2,1], sum2$quantiles[2,5])
M2_thetaCrI<- matrix(list(), nrow=7, ncol=2); 
M2_thetaCrI[,1] <- sum2$quantiles[3:9,1]
M2_thetaCrI[,2] <- sum2$quantiles[3:9,5]

# M3:
M3_meanP<- sum3$statistics[1,1] # Pooled mean
M3_meanTau<-  sum3$statistics[2,1] #  Tau
M3_theta<- unname(sum3$statistics[3:13,1]) # Theta from individual studies
M3_MuCrI<- c(sum3$quantiles[1,1], sum3$quantiles[1,5])
M3_TauCrI<- c(sum3$quantiles[2,1], sum3$quantiles[2,5])
M3_thetaCrI<- matrix(list(), nrow=11, ncol=2); 
M3_thetaCrI[,1] <- sum3$quantiles[3:13,1]
M3_thetaCrI[,2] <- sum3$quantiles[3:13,5]

# M4:
M4_meanP<- sum4$statistics[1,1] # Pooled mean
M4_meanTau<-  sum4$statistics[2,1] #  Tau
M4_theta<- unname(sum4$statistics[3:9,1]) # Theta from individual studies
M4_MuCrI<- c(sum4$quantiles[1,1], sum4$quantiles[1,5])
M4_TauCrI<- c(sum4$quantiles[2,1], sum4$quantiles[2,5])
M4_thetaCrI<- matrix(list(), nrow=7, ncol=2); 
M4_thetaCrI[,1] <- sum4$quantiles[3:9,1]
M4_thetaCrI[,2] <- sum4$quantiles[3:9,5]
#############

#--------
# ECDFs:
#--------

S1<-jags.samples(M1_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S1<-c(S1$mu[1,,1],S1$mu[1,,2],S1$mu[1,,3])
ECDF1<- ecdf(S1);1- ECDF1(1)

S2<-jags.samples(M2_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S2<-c(S2$mu[1,,1],S2$mu[1,,2],S2$mu[1,,3])
ECDF2<- ecdf(S2);1- ECDF2(1)

S3<-jags.samples(M3_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S3<-c(S3$mu[1,,1],S3$mu[1,,2],S3$mu[1,,3])
ECDF3<- ecdf(S3);1- ECDF3(1)

S4<-jags.samples(M4_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S4<-c(S4$mu[1,,1],S4$mu[1,,2],S4$mu[1,,3])
ECDF4<- ecdf(S4);1- ECDF4(1)

S5<-jags.samples(M5_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S5<-c(S5$mu[1,,1],S5$mu[1,,2],S5$mu[1,,3])
ECDF5<- ecdf(S5);1- ECDF5(1)

S6<-jags.samples(M6_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S6<-c(S6$mu[1,,1],S6$mu[1,,2],S6$mu[1,,3])
ECDF6<- ecdf(S6);1- ECDF6(1)

#########
# Plots:
#########


#-------------
# Forest plot:  
#-------------

source("BforestN2.R")
Plot<- BforestN2(data1$T, data1$S.sqr, unlist(M1_thetaCrI[,1]), 
                 unlist(M1_thetaCrI[,2]),
                 M1_meanP, M2_meanP, M1_MuCrI, M2_MuCrI)

# Save Image:
png(file = 'Plots/N2forest.png', width = 600, height = 800, units = "px")

Plot

dev.off()


Plot<- BforestN2(data1$T, data1$S.sqr, data2$T, data2$S.sqr, unlist(M1_thetaCrI[,1]), 
                 unlist(M1_thetaCrI[,2]), unlist(M3_thetaCrI[,1]), unlist(M3_thetaCrI[,2]),
                 M1_meanP, M3_meanP, M2_meanP, M4_meanP, M1_MuCrI, M3_MuCrI, M2_MuCrI, M4_MuCrI)


#-------------
# ECDF plot:  
#-------------

# FFD:
prob1<- NULL; prob2<- NULL; prob3<- NULL; prob3<- NULL; seq1<- NULL

  mu<- seq(0,20,0.001) # create sequence of ESs
  seq1<- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)

  for (i in 1:length(mu)){
    prob1[i]<- 1- (ECDF1(mu[i]))
    prob2[i]<- 1- (ECDF2(mu[i]))
    prob3[i]<- 1- (ECDF5(mu[i]))
  }

  mu<- c(mu, mu, mu)
  prob<- c(prob1, prob2, prob3)
  Model= c(rep(paste("N+2 on word N+2[all]", sep=""), length(mu)/3),
         rep(paste("N+2 on word N+2[alphab.]", sep=""), length(mu)/3),
         rep(paste("N+2 on word N+1", sep=""), length(mu)/3))
  
  DB<- data.frame(mu, prob, Model)
  DB$Model <- factor(DB$Model, levels = c("N+2 on word N+2[all]", "N+2 on word N+2[alphab.]",
                                          "N+2 on word N+1"))
  
  # Create graph:
  library(ggplot2)
  
  y<- expression(paste("P(", mu, " > X | Data)"))
  #
  Plot <-ggplot(DB, aes(mu, prob, colour=Model)) + geom_line(size=2)+ theme_bw() +
    theme(panel.grid.major.y = element_line(color="#E3E3E3", size=0.2),panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab(y) + xlab("X (in ms)") + 
    scale_x_continuous(breaks=seq1) +
    scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                       labels=c("0",".1",".2",".3",".4",".5", '.6', '.7','.8', '.9', '1')) + 
    theme(axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
          axis.title.y = element_text(size=18), axis.text=element_text(size=18));

  Plot<- Plot + theme(legend.position="bottom", legend.text = element_text(size = 17),
                      legend.title = element_text(size = 16, face="bold"))
  Plot<- Plot + geom_rect(mapping=aes(ymin= .93, ymax=1.01, xmin=17, xmax=20.3),size=0.7,
                          color="black", fill="white")+
     geom_text(aes(18.7, .97, label="FFD"), colour="black", size=8)

  # Save Image:
  png(file = 'Plots/N2prob.png', width = 600, height = 800, units = "px")
  
  Plot
  
  dev.off()  
  
  
  # GD:
  prob1<- NULL; prob2<- NULL; prob3<- NULL; prob3<- NULL; seq1<- NULL
  
  mu<- seq(0,20,0.001) # create sequence of ESs
  seq1<- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,20)
  
  for (i in 1:length(mu)){
    prob1[i]<- 1- (ECDF3(mu[i]))
    prob2[i]<- 1- (ECDF4(mu[i]))
    prob3[i]<- 1- (ECDF6(mu[i]))
  }
  
  mu<- c(mu, mu, mu)
  prob<- c(prob1, prob2, prob3)
  Model= c(rep(paste("N+2 on word N+2[all]", sep=""), length(mu)/3),
           rep(paste("N+2 on word N+2[alphab.]", sep=""), length(mu)/3),
           rep(paste("N+2 on word N+1", sep=""), length(mu)/3))
  
  DB<- data.frame(mu, prob, Model)
  DB$Model <- factor(DB$Model, levels = c("N+2 on word N+2[all]", "N+2 on word N+2[alphab.]",
                                          "N+2 on word N+1"))
  
  # Create graph:
  library(ggplot2)
  
  # Create graph:
  library(ggplot2)
  
  y<- expression(paste("P(", mu, " > X | Data)"))
  #
  Plot2 <-ggplot(DB, aes(mu, prob, colour=Model)) + geom_line(size=2)+ theme_bw() +
    theme(panel.grid.major.y = element_line(color="#E3E3E3", size=0.2),panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    ylab(y) + xlab("X (in ms)") + 
    scale_x_continuous(breaks=seq1) +
    scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
                       labels=c("0",".1",".2",".3",".4",".5", '.6', '.7','.8', '.9', '1')) + 
    theme(axis.title.x = element_text(size=18),axis.title.y = element_text(size=18),
          axis.title.y = element_text(size=18), axis.text=element_text(size=18));
  
  Plot2<- Plot2 + theme(legend.position="bottom", legend.text = element_text(size = 17),
                      legend.title = element_text(size = 16, face="bold"))
  Plot2<- Plot2 + geom_rect(mapping=aes(ymin= .93, ymax=1.01, xmin=17, xmax=20.3),size=0.7,
                          color="black", fill="white")+
    geom_text(aes(18.7, .97, label="GD"), colour="black", size=8)
  
  # Save Image:
  png(file = 'Plots/N2prob2.png', width = 600, height = 800, units = "px")
  
  Plot2
  
  dev.off()  
  

  library(gridExtra)
  library(grid)
  png(file = 'Plots/N2probs.png', width = 1300, height = 600, units = "px")
  grid.arrange(Plot, Plot2, 
               top= textGrob('',
                             gp=gpar(cex=2), just="top"), ncol=2, nrow =1)
  
  dev.off() 
  
  
  # N+2 on word n+2:
#  source("ECDF_plot.R")
#  
#  Plot1<-ECDF_plot(S1, S2, S3, "FFD")
#  Plot2<-ECDF_plot(S7, S8, S9, "GD")
#  library(gridExtra)
# grid.arrange(Plot1, Plot2, main= 'Probability that Mu is bigger than some number X (word n+2)',
#               legend=grid.legend(labels=c(expression(paste(mu, " ~ N(0, 1/10" ^ "3", ")    ")),
#                                           expression(paste(mu, " ~ N(0, 1/10", ")    ")), 
#                                           expression(paste(mu, " ~ N(0, 0.5", ")    "))),
#                                  draw=TRUE, gp=gpar(fontsize=14, 
#                                                     col = c("black", "#ED6666", "lightseagreen"),lwd=3), 
#                                  vgap = unit(5, "points"),pch=15,
#                                  do.lines = TRUE))
  

  
  
  
#############################################
#  Bayesian random-effects meta-regression  #
#  with word N+1 length as a covariate      #
#############################################  

# get only alphabetical studies (due the difference of letters vs characters in Chinese) 
FFD<- subset(FFD, cov>2)  
  
table(FFD$cov) # check covariate:

plot(FFD$cov, FFD$T) # check scatterplot

source("JReg.R"); library(rjags)
#FFD$cov<-  as.factor(FFD$cov)
  
MR1<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", nrow(FFD), "N2_R1.txt"),
                 FFD, n.chains=3, n.adapt=3000, quiet=FALSE)
R1<- coda.samples(MR1, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
sum1<- summary(R1);
# sum4; save(sum4, file="Summaries/N2/sum4.Rda") # MAIN

plot(R1, trace=FALSE)
gelman.diag(R1, confidence=0.95); gelman.plot(R1, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(R1, smooth=TRUE); 
autocorr.diag(R1); autocorr.plot(R1, lagmax=20); acfplot(R1) 

################
# Plot results #
################
FFD<- FFD[order(FFD$cov, decreasing = F),]
#FFD$cov<- as.numeric(FFD$cov)
FFD$prec<- 1/FFD$S.sqr
FFD$weight<- NULL

for(i in 1:nrow(FFD)){
  
  FFD$weight[i]<- FFD$prec[i]/sum(FFD$prec)
}
FFD$weight<- 2*(FFD$weight/max(FFD$weight)) # so that the most precise study leads to 2x magnification


y<- 1:10
plot (y, type="l", col="white", lty="solid", lwd=2
      , ylim=c(-5, 20), xlim= c(0, 7)
      , axes=F, xaxs="i", yaxs="i", cex.lab=1.5) #


points(x= FFD$cov, y=FFD$T, pch = 1, cex=1.5 + FFD$weight, col="black")


axis(side=1, at=c(0, 1, 2, 3, 4, 5, 6, 7), 
     labels=c(toString(0), toString(1), toString(2), toString(3), toString(4), 
              toString(5), toString(6), toString(7)), 
     tick=T, cex.axis=1.4)

axis(side=2, at=c(-6, -4, -2, 0, 2, 4, 6, 8, 10, 12), 
     labels=c(toString(-6), toString(-4), toString(-2), toString(0), toString(2), 
              toString(4), toString(6), toString(8), toString(10), toString(12)), 
     tick=T, cex.axis=1.4)



