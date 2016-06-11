# Martin Vasilev, 2016

# This is the script for random-effects Bayesian meta-analysis of N+1 preview effects

# !NOTE: In this version I use uniform prior distributions for mu

##########################################################
# "ULTIMATE" N+1 preview effect at the end! (~line 1590) #
##########################################################


rm(list=ls())

library(rjags)
library(ggplot2)


#-------------
# Load data: 
#-------------

# I load the data that I have coded in separate files: 
# For info about coding of individual articles, see "All_papers.xls" 

#setwd("C:/Users/Martin Vasilev/Dropbox/Research/Statistics/R/Master thesis/Bmeta/Bmeta")
load('Data/ES_N1.Rda')
load('Data/dataN1.Rda')

load('Data/X_FFD.Rda')
#load('Data/X_SFD.Rda') # not enough studies for analysis
load('Data/X_GD.Rda')

load('Data/RAN_FFD.Rda'); load('Data/RAN_SFD.Rda')
load('Data/RAN_GD.Rda'); load('Data/RAN_TVT.Rda')

load('Data/UNREL_FFD.Rda'); load('Data/UNREL_SFD.Rda')
load('Data/UNREL_GD.Rda'); load('Data/UNREL_TVT.Rda')

load('Data/PSEUD_FFD.Rda'); load('Data/PSEUD_SFD.Rda')
load('Data/PSEUD_GD.Rda'); load('Data/PSEUD_TVT.Rda')

load('Data/ORTH_FFD.Rda'); load('Data/ORTH_SFD.Rda')
load('Data/ORTH_GD.Rda'); load('Data/ORTH_TVT.Rda')

load('Data/SEM_FFD.Rda'); load('Data/SEM_SFD.Rda')
load('Data/SEM_GD.Rda'); load('Data/SEM_TVT.Rda')

load('Data/PHON_FFD.Rda'); load('Data/PHON_SFD.Rda')
load('Data/PHON_GD.Rda'); load('Data/PHON_TVT.Rda')

RAN_FFD$Measure<-"FFD";RAN_SFD$Measure<-"SFD" 
RAN_GD$Measure<-"GD";RAN_TVT$Measure<-"TVT" 
RAN<- rbind(RAN_FFD, RAN_SFD, RAN_GD, RAN_TVT)
RAN$Measure<- ordered(RAN$Measure, levels = c("FFD", "SFD", "GD", "TVT"))

UNREL_FFD$Measure<-"FFD";UNREL_SFD$Measure<-"SFD" 
UNREL_GD$Measure<-"GD";UNREL_TVT$Measure<-"TVT" 
UNREL<- rbind(UNREL_FFD, UNREL_SFD, UNREL_GD, UNREL_TVT)
UNREL$Measure<- ordered(UNREL$Measure, levels = c("FFD", "SFD", "GD", "TVT"))

PSEUD_FFD$Measure<-"FFD";PSEUD_SFD$Measure<-"SFD" 
PSEUD_GD$Measure<-"GD";PSEUD_TVT$Measure<-"TVT" 
PSEUD<- rbind(PSEUD_FFD, PSEUD_SFD, PSEUD_GD, PSEUD_TVT)
PSEUD$Measure<- ordered(PSEUD$Measure, levels = c("FFD", "SFD", "GD", "TVT"))

ORTH_FFD$Measure<-"FFD";ORTH_SFD$Measure<-"SFD" 
ORTH_GD$Measure<-"GD";ORTH_TVT$Measure<-"TVT" 
ORTH<- rbind(ORTH_FFD, ORTH_SFD, ORTH_GD, ORTH_TVT)
ORTH$Measure<- ordered(ORTH$Measure, levels = c("FFD", "SFD", "GD", "TVT"))

SEM_FFD$Measure<-"FFD";SEM_SFD$Measure<-"SFD" 
SEM_GD$Measure<-"GD";SEM_TVT$Measure<-"TVT" 
SEM<- rbind(SEM_FFD, SEM_SFD, SEM_GD, SEM_TVT)
SEM$Measure<- ordered(SEM$Measure, levels = c("FFD", "SFD", "GD", "TVT"))

PHON_FFD$Measure<-"FFD";PHON_SFD$Measure<-"SFD" 
PHON_GD$Measure<-"GD";PHON_TVT$Measure<-"TVT" 
PHON<- rbind(PHON_FFD, PHON_SFD, PHON_GD, PHON_TVT)
PHON$Measure<- ordered(PHON$Measure, levels = c("FFD", "SFD", "GD", "TVT"))

#---------------------
# Visual inspection: 
#---------------------

# Violin plot (All studies)
#RAN:
P1 <- ggplot(subset(RAN), aes(x=Measure, y=T, fill=Measure)) +
  scale_y_continuous(breaks=c(10,20,30,40,50,60,
                              70,80,90,100,110,120,130,140,150))+
   theme_bw() +
  theme(panel.grid.major.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))+
  geom_violin() + geom_boxplot(width=0.1, fill="white") +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=15), plot.title = element_text(size=18),
        legend.text=element_text(size=15), legend.title=element_text(size=15))+
  ylab('Effect size (in ms)') + xlab('Type of measure')+
  ggtitle("Violin Plot for N+1 RAN Preview Effect (All studies)")+
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
png(file = 'Plots/Vio_N1_RAN.png', width = 800, height = 600, units = "px")
P1
dev.off()


# UNREL:
P2 <- ggplot(subset(UNREL), aes(x=Measure, y=T, fill=Measure)) + 
  scale_y_continuous(breaks=c(10,20,30,40,50,60,
                              70,80,90,100))+ theme_bw() +
  theme(panel.grid.major.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))+
  geom_violin() + geom_boxplot(width=0.1, fill="white") +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=15), plot.title = element_text(size=18),
        legend.text=element_text(size=15), legend.title=element_text(size=15))+
  ylab('Effect size (in ms)') + xlab('Type of measure')+
  ggtitle("Violin Plot for N+1 UNREL Preview Effect (All studies)")+
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
png(file = 'Plots/Vio_N1_UNREL.png', width = 800, height = 600, units = "px")
P2
dev.off()

# PSEUD:
P3 <- ggplot(subset(PSEUD), aes(x=Measure, y=T, fill=Measure)) + 
  scale_y_continuous(breaks=c(10,20,30,40,50,60,
                              70,80,90,100))+ theme_bw() +
  theme(panel.grid.major.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))+
  geom_violin() + geom_boxplot(width=0.1, fill="white") +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=15), plot.title = element_text(size=18),
        legend.text=element_text(size=15), legend.title=element_text(size=15))+
  ylab('Effect size (in ms)') + xlab('Type of measure')+
  ggtitle("Violin Plot for N+1 PSEUD Preview Effect (All studies)")+
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
png(file = 'Plots/Vio_N1_PSEUD.png', width = 800, height = 600, units = "px")
P3
dev.off()

# ORTH:
P4 <- ggplot(subset(ORTH), aes(x=Measure, y=T, fill=Measure)) + 
  scale_y_continuous(breaks=c(10,20,30,40,50,60,
                              70,80,90,100))+ theme_bw() +
  theme(panel.grid.major.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))+
  geom_violin() + geom_boxplot(width=0.1, fill="white") +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=15), plot.title = element_text(size=18),
        legend.text=element_text(size=15), legend.title=element_text(size=15))+
  ylab('Effect size (in ms)') + xlab('Type of measure')+
  ggtitle("Violin Plot for N+1 ORTH Preview Effect (All studies)")+
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
png(file = 'Plots/Vio_N1_ORTH.png', width = 800, height = 600, units = "px")
P4
dev.off()


# SEM:
P5 <- ggplot(subset(SEM), aes(x=Measure, y=T, fill=Measure)) + 
  scale_y_continuous(breaks=c(10,20,30,40,50,60,
                              70,80,90,100))+ theme_bw() +
  theme(panel.grid.major.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))+
  geom_violin() + geom_boxplot(width=0.1, fill="white") +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=15), plot.title = element_text(size=18),
        legend.text=element_text(size=15), legend.title=element_text(size=15))+
  ylab('Effect size (in ms)') + xlab('Type of measure')+
  ggtitle("Violin Plot for N+1 SEM Preview Effect (All studies)")+
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
png(file = 'Plots/Vio_N1_SEM.png', width = 800, height = 600, units = "px")
P5
dev.off()


# PHON:
P6 <- ggplot(subset(PHON), aes(x=Measure, y=T, fill=Measure)) + 
  scale_y_continuous(breaks=c(10,20,30,40,50,60,
                              70,80,90,100))+ theme_bw() +
  theme(panel.grid.major.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(color="#E3E3E3", size=0.2),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "gray"))+
  geom_violin() + geom_boxplot(width=0.1, fill="white") +
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=15), plot.title = element_text(size=18),
        legend.text=element_text(size=15), legend.title=element_text(size=15))+
  ylab('Effect size (in ms)') + xlab('Type of measure')+
  ggtitle("Violin Plot for N+1 PHON Preview Effect (All studies)")+
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
png(file = 'Plots/Vio_N1_PHON.png', width = 800, height = 600, units = "px")
P6
dev.off()

#---------------
# rJAGS models:
#---------------


#-----------------------------------------------------------------------------------------
#                                    ALL STUDIES:
#-----------------------------------------------------------------------------------------

source("JModel.R")

#######
# RAN #
#######

# M1 (N+1 RAN effect,  measured with FFD):
data<- RAN_FFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M1_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M1.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M1_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M1_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M1_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M1_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M1_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M1_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M1_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M1_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M1_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M1_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M1_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M1_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M1<- coda.samples(M1_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum1<- summary(M1); sum1; save(sum1, file= "Summaries/N1/sum1.Rda")

# Diagnostics
#plot(M1, trace=FALSE)
gelman.diag(M1, confidence=0.95); gelman.plot(M1, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1, smooth=TRUE); 
autocorr.diag(M1); autocorr.plot(M1, lagmax=20); acfplot(M1) # auto-correlations


# M2 (N+1 RAN effect,  measured with SFD):
data<- RAN_SFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M2_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M2_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M2_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M2_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M2_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M2_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M2_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M2_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M2_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M2_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M2_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M2_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M2_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M2<- coda.samples(M2_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum2<- summary(M2); sum2; save(sum2, file= "Summaries/N1/sum2.Rda")

# Diagnostics
#plot(M2, trace=FALSE)
gelman.diag(M2, confidence=0.95); gelman.plot(M2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2, smooth=TRUE); 
autocorr.diag(M2); autocorr.plot(M2, lagmax=20); acfplot(M2) # auto-correlations
#----------------

# M3 (N+1 RAN effect,  measured with GD):
data<- RAN_GD[,3:4]; #data<- subset(data, S.sqr!="NA")
M3_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M3_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M3_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M3_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M3_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M3_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M3_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M3_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M3_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M3_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M3_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M3_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M3_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M3<- coda.samples(M3_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum3<- summary(M3); sum3; save(sum3, file= "Summaries/N1/sum3.Rda")

# Diagnostics
#plot(M3, trace=FALSE)
gelman.diag(M3, confidence=0.95); gelman.plot(M3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3, smooth=TRUE); 
autocorr.diag(M3); autocorr.plot(M3, lagmax=20); acfplot(M3) # auto-correlations


#----------------
# M4 (N+1 RAN effect,  measured with TVT):
data<- RAN_TVT[,3:4]; #data<- subset(data, S.sqr!="NA")
M4_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M4_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M4_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M4_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M4_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M4_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M4_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M4_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M4_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M4_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M4_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M4_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M4_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M4<- coda.samples(M4_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum4<- summary(M4); sum4; save(sum4, file= "Summaries/N1/sum4.Rda")

# Diagnostics
#plot(M4, trace=FALSE)
gelman.diag(M4, confidence=0.95); gelman.plot(M4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4, smooth=TRUE); 
autocorr.diag(M4); autocorr.plot(M4, lagmax=20); acfplot(M4) # auto-correlations


#########
# UNREL #
#########

# M5 (N+1 UNREL effect,  measured with FFD):
data<- UNREL_FFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M5_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M5_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M5_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M5_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M5_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M5_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M5_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M5_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M5_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M5_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M5_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M5_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M5_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M5<- coda.samples(M5_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum5<- summary(M5); sum5; save(sum5, file= "Summaries/N1/sum5.Rda")

# Diagnostics
#plot(M5, trace=FALSE)
gelman.diag(M5, confidence=0.95); gelman.plot(M5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5, smooth=TRUE); 
autocorr.diag(M5); autocorr.plot(M5, lagmax=20); acfplot(M5) # auto-correlations


# M6 (N+1 UNREL effect,  measured with SFD):
data<- UNREL_SFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M6_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M6.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M6_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M6_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M6_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M6_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M6_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M6_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M6_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M6_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M6_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M6_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M6_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M6_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M6<- coda.samples(M6_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum6<- summary(M6); sum6; save(sum6, file= "Summaries/N1/sum6.Rda")

# Diagnostics
#plot(M6, trace=FALSE)
gelman.diag(M6, confidence=0.95); gelman.plot(M6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6, smooth=TRUE); 
autocorr.diag(M6); autocorr.plot(M6, lagmax=20); acfplot(M6) # auto-correlations
#----------------

# M7 (N+1 UNREL effect,  measured with GD):
data<- UNREL_GD[,3:4]; #data<- subset(data, S.sqr!="NA")
M7_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M7.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M7_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M7_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M7_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M7_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M7_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M7_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M7_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M7_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M7_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M7_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M7_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M7_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M7<- coda.samples(M7_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum7<- summary(M7); sum7; save(sum7, file= "Summaries/N1/sum7.Rda")

# Diagnostics
#plot(M7, trace=FALSE)
gelman.diag(M7, confidence=0.95); gelman.plot(M7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M7, smooth=TRUE); 
autocorr.diag(M7); autocorr.plot(M7, lagmax=20); acfplot(M7) # auto-correlations


#----------------
# M8 (N+1 UNREL effect,  measured with TVT):
data<- UNREL_TVT[,3:4]; #data<- subset(data, S.sqr!="NA")
M8_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M8.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M8_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M8_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M8_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M8_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M8_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M8_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M8_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M8_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M8_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M8_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M8_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M8_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M8<- coda.samples(M8_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum8<- summary(M8); sum8; save(sum8, file= "Summaries/N1/sum8.Rda")

# Diagnostics
#plot(M4, trace=FALSE)
gelman.diag(M8, confidence=0.95); gelman.plot(M8, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M8, smooth=TRUE); 
autocorr.diag(M8); autocorr.plot(M8, lagmax=20); acfplot(M8) # auto-correlations



#########
#   X   #
#########

# M9 (N+1 X effect,  measured with FFD):
data<- X_FFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M9_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M9.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M9_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M9_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M9_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M9_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M9_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M9_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M9_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M9_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M9_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M9_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M9_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M9_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M9<- coda.samples(M9_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum9<- summary(M9); sum9; save(sum9, file= "Summaries/N1/sum9.Rda")

# Diagnostics
#plot(M9, trace=FALSE)
gelman.diag(M9, confidence=0.95); gelman.plot(M9, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M9, smooth=TRUE); 
autocorr.diag(M9); autocorr.plot(M9, lagmax=20); acfplot(M9) # auto-correlations



# M10 (N+1 X effect,  measured with GD):
data<- X_GD[,3:4]; #data<- subset(data, S.sqr!="NA")
M10_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M10.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M10_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M10_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M10_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M10_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M10_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M10_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M10_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M10_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M10_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M10_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M10_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M10_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M10<- coda.samples(M10_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum10<- summary(M10); sum10; save(sum10, file= "Summaries/N1/sum10.Rda")

# Diagnostics
#plot(M7, trace=FALSE)
gelman.diag(M10, confidence=0.95); gelman.plot(M10, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M10, smooth=TRUE); 
autocorr.diag(M10); autocorr.plot(M10, lagmax=20); acfplot(M10) # auto-correlations


#########
# PSEUD #
#########

# M11 (N+1 PSEUD effect,  measured with FFD):
data<- PSEUD_FFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M11_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M11.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M11_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M11_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M11_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M11_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M11_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M11_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M11_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M11_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M11_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M11_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M11_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M11_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M11<- coda.samples(M11_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum11<- summary(M11); sum11; save(sum11, file= "Summaries/N1/sum11.Rda")

# Diagnostics
#plot(M11, trace=FALSE)
gelman.diag(M11, confidence=0.95); gelman.plot(M11, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M11, smooth=TRUE); 
autocorr.diag(M11); autocorr.plot(M11, lagmax=20); acfplot(M11) # auto-correlations


# M12 (N+1 PSEUD effect,  measured with SFD):
data<- PSEUD_SFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M12_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M12.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M12_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M12_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M12_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M12_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M12_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M12_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M12_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M12_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M12_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M12_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M12_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M12_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M12<- coda.samples(M12_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum12<- summary(M12); sum12; save(sum12, file= "Summaries/N1/sum12.Rda")

# Diagnostics
#plot(M12, trace=FALSE)
gelman.diag(M12, confidence=0.95); gelman.plot(M12, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M12, smooth=TRUE); 
autocorr.diag(M12); autocorr.plot(M12, lagmax=20); acfplot(M12) # auto-correlations
#----------------

# M13 (N+1 PSEUD effect,  measured with GD):
data<- PSEUD_GD[,3:4]; #data<- subset(data, S.sqr!="NA")
M13_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M13.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M13_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M13_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M13_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M13_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M13_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M13_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M13_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M13_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M13_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M13_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M13_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M13_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M13<- coda.samples(M13_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum13<- summary(M13); sum13; save(sum13, file= "Summaries/N1/sum13.Rda")

# Diagnostics
#plot(M13, trace=FALSE)
gelman.diag(M13, confidence=0.95); gelman.plot(M13, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M13, smooth=TRUE); 
autocorr.diag(M13); autocorr.plot(M13, lagmax=20); acfplot(M13) # auto-correlations


#----------------
# M14 (N+1 PSEUD effect,  measured with TVT):
data<- PSEUD_TVT[,3:4]; #data<- subset(data, S.sqr!="NA")
M14_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M14.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M14_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M14_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M14_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M14_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M14_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M14_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M14_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M14_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M14_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M14_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M14_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M14_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M14<- coda.samples(M14_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum14<- summary(M14); sum14; save(sum14, file= "Summaries/N1/sum14.Rda")

# Diagnostics
#plot(M14, trace=FALSE)
gelman.diag(M14, confidence=0.95); gelman.plot(M14, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M14, smooth=TRUE); 
autocorr.diag(M14); autocorr.plot(M14, lagmax=20); acfplot(M14) # auto-correlations



#########
#  ORTH #
#########

# M15 (N+1 ORTH effect,  measured with FFD):
data<- ORTH_FFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M15_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M15.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M15_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M15_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M15_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M15_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M15_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M15_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M15_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M15_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M15_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M15_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M15_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M15_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M15<- coda.samples(M15_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum15<- summary(M15); sum15; save(sum15, file= "Summaries/N1/sum15.Rda")

# Diagnostics
#plot(M15, trace=FALSE)
gelman.diag(M15, confidence=0.95); gelman.plot(M15, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M15, smooth=TRUE); 
autocorr.diag(M15); autocorr.plot(M15, lagmax=20); acfplot(M15) # auto-correlations


# M16 (N+1 ORTH effect,  measured with SFD):
data<- ORTH_SFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M16_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M16.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M16_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M16_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M16_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M16_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M16_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M16_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M16_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M16_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M16_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M16_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M16_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M16_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M16<- coda.samples(M16_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum16<- summary(M16); sum16; save(sum16, file= "Summaries/N1/sum16.Rda")

# Diagnostics
#plot(M16, trace=FALSE)
gelman.diag(M16, confidence=0.95); gelman.plot(M16, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M16, smooth=TRUE); 
autocorr.diag(M16); autocorr.plot(M16, lagmax=20); acfplot(M16) # auto-correlations
#----------------

# M17 (N+1 ORTH effect,  measured with GD):
data<- ORTH_GD[,3:4]; #data<- subset(data, S.sqr!="NA")
M17_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M17.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M17_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M17_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M17_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M17_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M17_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M17_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M17_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M17_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M17_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M17_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M17_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M17_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M17<- coda.samples(M17_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum17<- summary(M17); sum17; save(sum17, file= "Summaries/N1/sum17.Rda")

# Diagnostics
#plot(M17, trace=FALSE)
gelman.diag(M17, confidence=0.95); gelman.plot(M17, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M17, smooth=TRUE); 
autocorr.diag(M17); autocorr.plot(M17, lagmax=20); acfplot(M17) # auto-correlations

#----------------
# M18 (N+1 ORTH effect,  measured with TVT):
data<- ORTH_TVT[,3:4]; #data<- subset(data, S.sqr!="NA")
M18_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M18.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M18_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M18_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M18_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M18_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M18_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M18_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M18_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M18_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M18_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M18_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M18_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M18_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M18<- coda.samples(M18_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum18<- summary(M18); sum18; save(sum18, file= "Summaries/N1/sum18.Rda")

# Diagnostics
#plot(M18, trace=FALSE)
gelman.diag(M18, confidence=0.95); gelman.plot(M18, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M18, smooth=TRUE); 
autocorr.diag(M18); autocorr.plot(M18, lagmax=20); acfplot(M18) # auto-correlations


#########
#  SEM  #
#########

# M19 (N+1 SEM effect,  measured with FFD):
data<- SEM_FFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M19_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M19.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M19_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M19_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M19_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M19_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M19_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M19_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M19_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M19_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M19_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M19_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M19_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M19_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M19<- coda.samples(M19_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum19<- summary(M19); sum19; save(sum19, file= "Summaries/N1/sum19.Rda")

# Diagnostics
#plot(M19, trace=FALSE)
gelman.diag(M19, confidence=0.95); gelman.plot(M19, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M19, smooth=TRUE); 
autocorr.diag(M19); autocorr.plot(M19, lagmax=20); acfplot(M19) # auto-correlations


# M20 (N+1 SEM effect,  measured with SFD):
data<- SEM_SFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M20_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M20.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M20_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M20_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M20_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M20_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M20_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M20_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M20_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M20_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M20_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M20_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M20_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M20_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M20<- coda.samples(M20_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum20<- summary(M20); sum20; save(sum20, file= "Summaries/N1/sum20.Rda")

# Diagnostics
#plot(M20, trace=FALSE)
gelman.diag(M20, confidence=0.95); gelman.plot(M20, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M20, smooth=TRUE); 
autocorr.diag(M20); autocorr.plot(M20, lagmax=20); acfplot(M20) # auto-correlations
#----------------

# M21 (N+1 SEM effect,  measured with GD):
data<- SEM_GD[,3:4]; #data<- subset(data, S.sqr!="NA")
M21_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M21.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M21_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M21_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M21_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M21_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M21_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M21_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M21_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M21_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M21_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M21_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M21_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M21_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M21<- coda.samples(M21_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum21<- summary(M21); sum21; save(sum21, file= "Summaries/N1/sum21.Rda")

# Diagnostics
#plot(M21, trace=FALSE)
gelman.diag(M21, confidence=0.95); gelman.plot(M21, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M21, smooth=TRUE); 
autocorr.diag(M21); autocorr.plot(M21, lagmax=20); acfplot(M21) # auto-correlations


#########
#  PHON #
#########

# M22 (N+1 PHON effect,  measured with FFD):
data<- PHON_FFD[,3:4]; #data<- subset(data, S.sqr!="NA")
M22_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M22.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M22_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M22_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M22_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M22_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M22_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M22_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M22_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M22_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M22_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M22_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M22_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M22_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M22<- coda.samples(M22_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum22<- summary(M22); sum22; save(sum22, file= "Summaries/N1/sum22.Rda")

# Diagnostics
#plot(M22, trace=FALSE)
gelman.diag(M22, confidence=0.95); gelman.plot(M22, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M22, smooth=TRUE); 
autocorr.diag(M22); autocorr.plot(M22, lagmax=20); acfplot(M22) # auto-correlations



# M23 (N+1 PHON effect,  measured with GD):
data<- PHON_GD[,3:4]; #data<- subset(data, S.sqr!="NA")
M23_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M23.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M23_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M23_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M23_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M23_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M23_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M23_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M23_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M23_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M23_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M23_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M23_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M23_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M23<- coda.samples(M23_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum23<- summary(M23); sum23; save(sum23, file= "Summaries/N1/sum23.Rda")

# Diagnostics
#plot(M23, trace=FALSE)
gelman.diag(M23, confidence=0.95); gelman.plot(M23, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M23, smooth=TRUE); 
autocorr.diag(M23); autocorr.plot(M23, lagmax=20); acfplot(M23) # auto-correlations




#############
# Save descriptives:
#------- RAN
# M1:
M1_meanP<- sum1$statistics[1,1] # Pooled mean
M1_MuCrI<- c(sum1$quantiles[1,1], sum1$quantiles[1,5])
S1<-jags.samples(M1_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S1<-c(S1$mu[1,,1],S1$mu[1,,2],S1$mu[1,,3])
save(S1, file= "Summaries/Posterior_samples/RAN_FFD.Rda")

# M2:
M2_meanP<- sum2$statistics[1,1] # Pooled mean
M2_MuCrI<- c(sum2$quantiles[1,1], sum2$quantiles[1,5])

# M3:
M3_meanP<- sum3$statistics[1,1] # Pooled mean
M3_MuCrI<- c(sum3$quantiles[1,1], sum3$quantiles[1,5])

# M4:
M4_meanP<- sum4$statistics[1,1] # Pooled mean
M4_MuCrI<- c(sum4$quantiles[1,1], sum4$quantiles[1,5])
#------- UNREL
# M5:
M5_meanP<- sum5$statistics[1,1] # Pooled mean
M5_MuCrI<- c(sum5$quantiles[1,1], sum5$quantiles[1,5])

# M6:
M6_meanP<- sum6$statistics[1,1] # Pooled mean
M6_MuCrI<- c(sum6$quantiles[1,1], sum6$quantiles[1,5])

# M7:
M7_meanP<- sum7$statistics[1,1] # Pooled mean
M7_MuCrI<- c(sum7$quantiles[1,1], sum7$quantiles[1,5])

# M8:
M8_meanP<- sum8$statistics[1,1] # Pooled mean
M8_MuCrI<- c(sum8$quantiles[1,1], sum8$quantiles[1,5])
#------- X
# M9:
M9_meanP<- sum9$statistics[1,1] # Pooled mean
M9_MuCrI<- c(sum9$quantiles[1,1], sum9$quantiles[1,5])

# M10:
M10_meanP<- sum10$statistics[1,1] # Pooled mean
M10_MuCrI<- c(sum10$quantiles[1,1], sum10$quantiles[1,5])

#------- PSEUD
# M11:
M11_meanP<- sum11$statistics[1,1] # Pooled mean
M11_MuCrI<- c(sum11$quantiles[1,1], sum11$quantiles[1,5])

# M12:
M12_meanP<- sum12$statistics[1,1] # Pooled mean
M12_MuCrI<- c(sum12$quantiles[1,1], sum12$quantiles[1,5])

# M13:
M13_meanP<- sum13$statistics[1,1] # Pooled mean
M13_MuCrI<- c(sum13$quantiles[1,1], sum13$quantiles[1,5])

# M14:
M14_meanP<- sum14$statistics[1,1] # Pooled mean
M14_MuCrI<- c(sum14$quantiles[1,1], sum14$quantiles[1,5])
#------- ORTH
# M15:
M15_meanP<- sum15$statistics[1,1] # Pooled mean
M15_MuCrI<- c(sum15$quantiles[1,1], sum15$quantiles[1,5])

# M16:
M16_meanP<- sum16$statistics[1,1] # Pooled mean
M16_MuCrI<- c(sum16$quantiles[1,1], sum16$quantiles[1,5])

# M17:
M17_meanP<- sum17$statistics[1,1] # Pooled mean
M17_MuCrI<- c(sum17$quantiles[1,1], sum17$quantiles[1,5])

# M18:
M18_meanP<- sum18$statistics[1,1] # Pooled mean
M18_MuCrI<- c(sum18$quantiles[1,1], sum18$quantiles[1,5])
#------- SEM
# M19:
M19_meanP<- sum19$statistics[1,1] # Pooled mean
M19_MuCrI<- c(sum19$quantiles[1,1], sum19$quantiles[1,5])

# M20:
M20_meanP<- sum20$statistics[1,1] # Pooled mean
M20_MuCrI<- c(sum20$quantiles[1,1], sum20$quantiles[1,5])

# M21:
M21_meanP<- sum21$statistics[1,1] # Pooled mean
M21_MuCrI<- c(sum21$quantiles[1,1], sum21$quantiles[1,5])

#------- SEM
# M22:
M22_meanP<- sum22$statistics[1,1] # Pooled mean
M22_MuCrI<- c(sum22$quantiles[1,1], sum22$quantiles[1,5])

# M23:
M23_meanP<- sum23$statistics[1,1] # Pooled mean
M23_MuCrI<- c(sum23$quantiles[1,1], sum23$quantiles[1,5])
#############





#-----------------------------------------------------------------------------------------
#                              ALPHABETICAL STUDIES:
#-----------------------------------------------------------------------------------------

source("JModel.R")

# Results are the same for RAN because such mask is not possible in Chinese!!


#########
# UNREL #
#########

# M24 (N+1 UNREL effect,  measured with FFD):
data<- UNREL_FFD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];
M24_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M24.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M24_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M24_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M24_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M24_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M24_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M24_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M24_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M24_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M24_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M24_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M24_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M24_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M24<- coda.samples(M24_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum24<- summary(M24); sum24; save(sum24, file= "Summaries/N1/sum24.Rda")

# Diagnostics
#plot(M24, trace=FALSE)
gelman.diag(M24, confidence=0.95); gelman.plot(M24, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M24, smooth=TRUE); 
autocorr.diag(M24); autocorr.plot(M24, lagmax=20); acfplot(M24) # auto-correlations


# M25 (N+1 UNREL effect,  measured with SFD):
data<- UNREL_SFD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];
M25_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M25.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M25_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M25_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M25_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M25_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M25_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M25_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M25_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M25_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M25_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M25_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M25_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M25_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M25<- coda.samples(M25_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum25<- summary(M25); sum25; save(sum25, file= "Summaries/N1/sum25.Rda")

# Diagnostics
#plot(M25, trace=FALSE)
gelman.diag(M25, confidence=0.95); gelman.plot(M25, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M25, smooth=TRUE); 
autocorr.diag(M25); autocorr.plot(M25, lagmax=20); acfplot(M25) # auto-correlations
#----------------

# M26 (N+1 UNREL effect,  measured with GD):
data<- UNREL_GD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];
M26_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M26.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M26_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M26_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M26_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M26_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M26_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M26_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M26_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M26_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M26_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M26_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M26_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M26_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M26<- coda.samples(M26_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum26<- summary(M26); sum26; save(sum26, file= "Summaries/N1/sum26.Rda")

# Diagnostics
#plot(M26, trace=FALSE)
gelman.diag(M26, confidence=0.95); gelman.plot(M26, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M26, smooth=TRUE); 
autocorr.diag(M26); autocorr.plot(M26, lagmax=20); acfplot(M26) # auto-correlations


#----------------
# (N+1 UNREL effect,  measured with TVT):

# Same as model for all studies!



# Results are the same for X because such mask is not possible in Chinese!!


#########
# PSEUD #
#########

# M27 (N+1 PSEUD effect,  measured with FFD):
data<- PSEUD_FFD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];
M27_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M27.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M27_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M27_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M27_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M27_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M27_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M27_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M27_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M27_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M27_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M27_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M27_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M27_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M27<- coda.samples(M27_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum27<- summary(M27); sum27; save(sum27, file= "Summaries/N1/sum27.Rda")

# Diagnostics
#plot(M27, trace=FALSE)
gelman.diag(M27, confidence=0.95); gelman.plot(M27, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M27, smooth=TRUE); 
autocorr.diag(M27); autocorr.plot(M27, lagmax=20); acfplot(M27) # auto-correlations


# (N+1 PSEUD effect,  measured with SFD):
data<- PSEUD_SFD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];

# Not enough data!!
#----------------

# M28 (N+1 PSEUD effect,  measured with GD):
data<- PSEUD_GD[,2:4];  data<- subset(data, Language!="Chinese"); data<- data[,2:3];
M28_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M28.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M28_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M28_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M28_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M28_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M28_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M28_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M28_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M28_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M28_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M28_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M28_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M28_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M28<- coda.samples(M28_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum28<- summary(M28); sum28; save(sum28, file= "Summaries/N1/sum28.Rda")

# Diagnostics
#plot(M28, trace=FALSE)
gelman.diag(M28, confidence=0.95); gelman.plot(M28, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M28, smooth=TRUE); 
autocorr.diag(M28); autocorr.plot(M28, lagmax=20); acfplot(M28) # auto-correlations

#----------------
#  (N+1 PSEUD effect,  measured with TVT):

# Same as the model for all studies



#########
#  ORTH #
#########

# M29 (N+1 ORTH effect,  measured with FFD):
data<- ORTH_FFD[,2:4];  data<- subset(data, Language!="Chinese"); data<- data[,2:3];
M29_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M29.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M29_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M29_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M29_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M29_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M29_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M29_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M29_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M29_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M29_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M29_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M29_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M29_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M29<- coda.samples(M29_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum29<- summary(M29); sum29; save(sum29, file= "Summaries/N1/sum29.Rda")

# Diagnostics
#plot(M29, trace=FALSE)
gelman.diag(M29, confidence=0.95); gelman.plot(M29, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M29, smooth=TRUE); 
autocorr.diag(M29); autocorr.plot(M29, lagmax=20); acfplot(M29) # auto-correlations


# M30 (N+1 ORTH effect,  measured with SFD):
data<- ORTH_SFD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];
M30_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M30.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M30_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M30_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M30_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M30_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M30_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M30_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M30_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M30_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M30_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M30_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M30_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M30_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M30<- coda.samples(M30_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum30<- summary(M30); sum30; save(sum30, file= "Summaries/N1/sum30.Rda")

# Diagnostics
#plot(M30, trace=FALSE)
gelman.diag(M30, confidence=0.95); gelman.plot(M30, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M30, smooth=TRUE); 
autocorr.diag(M30); autocorr.plot(M30, lagmax=20); acfplot(M30) # auto-correlations
#----------------

# M31 (N+1 ORTH effect,  measured with GD):
data<- ORTH_GD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];
M31_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M31.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M31_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M31_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M31_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M31_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M31_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M31_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M31_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M31_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M31_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M31_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M31_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M31_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M31<- coda.samples(M31_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum31<- summary(M31); sum31; save(sum31, file= "Summaries/N1/sum31.Rda")

# Diagnostics
#plot(M31, trace=FALSE)
gelman.diag(M31, confidence=0.95); gelman.plot(M31, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M31, smooth=TRUE); 
autocorr.diag(M31); autocorr.plot(M31, lagmax=20); acfplot(M31) # auto-correlations

#----------------
#  (N+1 ORTH effect,  measured with TVT):

# Same as the model for all studies!

#########
#  SEM  #
#########

# (N+1 SEM effect,  measured with FFD):
data<- SEM_FFD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];

# Not enough studies for analysis!


# (N+1 SEM effect,  measured with SFD):
data<- SEM_SFD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3];

# Not enough studies for analysis!

# M32 (N+1 SEM effect,  measured with GD):
data<- SEM_GD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3]
M32_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M32.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M32_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M32_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M32_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M32_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M32_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M32_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M32_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M32_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M32_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M32_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M32_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M32_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M32<- coda.samples(M32_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum32<- summary(M32); sum32; save(sum32, file= "Summaries/N1/sum32.Rda")

# Diagnostics
#plot(M32, trace=FALSE)
gelman.diag(M32, confidence=0.95); gelman.plot(M32, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M32, smooth=TRUE); 
autocorr.diag(M32); autocorr.plot(M32, lagmax=20); acfplot(M32) # auto-correlations


#########
#  PHON #
#########

# M33 (N+1 PHON effect,  measured with FFD):
data<- PHON_FFD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3]
M33_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M33.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M33_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M33_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M33_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M33_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M33_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M33_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M33_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M33_5.txt"),
#                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M33_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M33_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M33_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M33_7.txt"),
#                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M33<- coda.samples(M33_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum33<- summary(M33); sum33; save(sum33, file= "Summaries/N1/sum33.Rda")

# Diagnostics
#plot(M33, trace=FALSE)
gelman.diag(M33, confidence=0.95); gelman.plot(M33, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M33, smooth=TRUE); 
autocorr.diag(M33); autocorr.plot(M33, lagmax=20); acfplot(M33) # auto-correlations



# M34 (N+1 PHON effect,  measured with GD):
data<- PHON_GD[,2:4]; data<- subset(data, Language!="Chinese"); data<- data[,2:3]
M34_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M34.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M34_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M34_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M34_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M34_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M34_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M34_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M34_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M34_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M34_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M34_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M34_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M34_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M34<- coda.samples(M34_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum34<- summary(M34); sum34; save(sum34, file= "Summaries/N1/sum34.Rda")

# Diagnostics
#plot(M34, trace=FALSE)
gelman.diag(M34, confidence=0.95); gelman.plot(M34, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M34, smooth=TRUE); 
autocorr.diag(M34); autocorr.plot(M34, lagmax=20); acfplot(M34) # auto-correlations




#############
# Save descriptives:
#------- UNREL
# M24:
M24_meanP<- sum24$statistics[1,1] # Pooled mean
M24_MuCrI<- c(sum24$quantiles[1,1], sum24$quantiles[1,5])

# M25:
M25_meanP<- sum25$statistics[1,1] # Pooled mean
M25_MuCrI<- c(sum25$quantiles[1,1], sum25$quantiles[1,5])

# M26:
M26_meanP<- sum26$statistics[1,1] # Pooled mean
M26_MuCrI<- c(sum26$quantiles[1,1], sum26$quantiles[1,5])
#------- PSEUD
# M27:
M27_meanP<- sum27$statistics[1,1] # Pooled mean
M27_MuCrI<- c(sum27$quantiles[1,1], sum27$quantiles[1,5])

# M28:
M28_meanP<- sum28$statistics[1,1] # Pooled mean
M28_MuCrI<- c(sum28$quantiles[1,1], sum28$quantiles[1,5])
#------- ORTH
# M29:
M29_meanP<- sum29$statistics[1,1] # Pooled mean
M29_MuCrI<- c(sum29$quantiles[1,1], sum29$quantiles[1,5])

# M30:
M30_meanP<- sum30$statistics[1,1] # Pooled mean
M30_MuCrI<- c(sum30$quantiles[1,1], sum30$quantiles[1,5])

# M31:
M31_meanP<- sum31$statistics[1,1] # Pooled mean
M31_MuCrI<- c(sum31$quantiles[1,1], sum31$quantiles[1,5])
#------- PSEUD
# M32:
M32_meanP<- sum32$statistics[1,1] # Pooled mean
M32_MuCrI<- c(sum32$quantiles[1,1], sum32$quantiles[1,5])
#------- PHON
# M33:
M33_meanP<- sum33$statistics[1,1] # Pooled mean
M33_MuCrI<- c(sum33$quantiles[1,1], sum33$quantiles[1,5])

# M34:
M34_meanP<- sum34$statistics[1,1] # Pooled mean
M34_MuCrI<- c(sum34$quantiles[1,1], sum34$quantiles[1,5])

#############





#-----------------------------------------------------------------------------------------
#                              NON-ALPHABETICAL STUDIES:
#-----------------------------------------------------------------------------------------

source("JModel.R")

# Results are the same for RAN because such mask is not possible in Chinese!!


#########
# UNREL #
#########

# M35 (N+1 UNREL effect,  measured with FFD):
data<- UNREL_FFD[,2:4]; data<- subset(data, Language=="Chinese"); data<- data[,2:3];
M35_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M35.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M35_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M35_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M35_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M35_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M35_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M35_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M35_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M35_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M35_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M35_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M35_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M35_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M35<- coda.samples(M35_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum35<- summary(M35); sum35; save(sum35, file= "Summaries/N1/sum35.Rda")

# Diagnostics
#plot(M35, trace=FALSE)
gelman.diag(M35, confidence=0.95); gelman.plot(M35, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M35, smooth=TRUE); 
autocorr.diag(M35); autocorr.plot(M35, lagmax=20); acfplot(M35) # auto-correlations


# M36 (N+1 UNREL effect,  measured with SFD):
data<- UNREL_SFD[,2:4]; data<- subset(data, Language=="Chinese"); data<- data[,2:3];
M36_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M36.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M36_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M36_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M36_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M36_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M36_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M36_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M36_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M36_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M36_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M36_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M36_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M36_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M36<- coda.samples(M36_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum36<- summary(M36); sum36; save(sum36, file= "Summaries/N1/sum36.Rda")

# Diagnostics
#plot(M36, trace=FALSE)
gelman.diag(M36, confidence=0.95); gelman.plot(M36, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M36, smooth=TRUE); 
autocorr.diag(M36); autocorr.plot(M36, lagmax=20); acfplot(M36) # auto-correlations
#----------------

# M37 (N+1 UNREL effect,  measured with GD):
data<- UNREL_GD[,2:4]; data<- subset(data, Language=="Chinese"); data<- data[,2:3];
M37_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M37.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M37_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M37_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M37_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M37_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M37_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M37_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M37_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M37_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M37_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M37_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M37_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M37_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M37<- coda.samples(M37_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum37<- summary(M37); sum37; save(sum37, file= "Summaries/N1/sum37.Rda")

# Diagnostics
#plot(M37, trace=FALSE)
gelman.diag(M37, confidence=0.95); gelman.plot(M37, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M37, smooth=TRUE); 
autocorr.diag(M37); autocorr.plot(M37, lagmax=20); acfplot(M37) # auto-correlations



#########
# PSEUD #
#########

# M38 (N+1 PSEUD effect,  measured with FFD):
data<- PSEUD_FFD[,2:4]; data<- subset(data, Language=="Chinese"); data<- data[,2:3];
M38_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M38.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M38_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M38_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M38_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M38_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M38_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M38_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M38_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M38_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M38_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M38_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M38_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M38_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M38<- coda.samples(M38_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum38<- summary(M38); sum38; save(sum38, file= "Summaries/N1/sum38.Rda")

# Diagnostics
#plot(M38, trace=FALSE)
gelman.diag(M38, confidence=0.95); gelman.plot(M38, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M38, smooth=TRUE); 
autocorr.diag(M38); autocorr.plot(M38, lagmax=20); acfplot(M38) # auto-correlations


# M39 (N+1 PSEUD effect,  measured with GD):
data<- PSEUD_GD[,2:4];  data<- subset(data, Language=="Chinese"); data<- data[,2:3];
M39_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M39.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # uniform prior on mu
#M39_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M39_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=20, "tau"=5))
#M39_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M39_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M39_M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M39_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M39_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M39_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M39_M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M39_6.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M39_M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M39_7.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M39<- coda.samples(M39_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum39<- summary(M39); sum39; save(sum39, file= "Summaries/N1/sum39.Rda")

# Diagnostics
#plot(M39, trace=FALSE)
gelman.diag(M39, confidence=0.95); gelman.plot(M39, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M39, smooth=TRUE); 
autocorr.diag(M39); autocorr.plot(M39, lagmax=20); acfplot(M39) # auto-correlations



#############
# Save descriptives:
#------- UNREL
# M35:
M35_meanP<- sum35$statistics[1,1] # Pooled mean
M35_MuCrI<- c(sum35$quantiles[1,1], sum35$quantiles[1,5])

# M36:
M36_meanP<- sum36$statistics[1,1] # Pooled mean
M36_MuCrI<- c(sum36$quantiles[1,1], sum36$quantiles[1,5])

# M37:
M37_meanP<- sum37$statistics[1,1] # Pooled mean
M37_MuCrI<- c(sum37$quantiles[1,1], sum37$quantiles[1,5])
#------- PSEUD
# M38:
M38_meanP<- sum38$statistics[1,1] # Pooled mean
M38_MuCrI<- c(sum38$quantiles[1,1], sum38$quantiles[1,5])

# M39:
M39_meanP<- sum39$statistics[1,1] # Pooled mean
M39_MuCrI<- c(sum39$quantiles[1,1], sum39$quantiles[1,5])
#############





#-----------------------------------------------------------------------------------------
#                              ULTIMATE N+1 PREVIEW EFFECT:
#-----------------------------------------------------------------------------------------
load("Data/data5.Rda") #FFD
load("Data/data6.Rda") #GD
load("Data/data7.Rda") #SFD
load("Data/data8.Rda") #TVT


###############
#             #
# ALL STUDIES #
#             #
###############

# M40 (N+1 effect,  measured with FFD):
data<- data5[,3:4]; 
M40_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M40.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M40<- coda.samples(M40_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum40<- summary(M40); sum40; save(sum40, file= "Summaries/N1/sum40.Rda")
M40_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M40_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M40_2<- coda.samples(M40_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum40_2<- summary(M40_2); save(sum40_2, file= "Summaries/N1/sum40_2.Rda")
M40_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M40_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M40_3<- coda.samples(M40_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum40_3<- summary(M40_3); save(sum40_3, file= "Summaries/N1/sum40_3.Rda")
M40_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M40_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M40_4<- coda.samples(M40_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum40_4<- summary(M40_4); save(sum40_4, file= "Summaries/N1/sum40_4.Rda")
M40_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M40_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M40_5<- coda.samples(M40_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum40_5<- summary(M40_5); save(sum40_5, file= "Summaries/N1/sum40_5.Rda")
M40_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M40_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M40_6<- coda.samples(M40_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum40_6<- summary(M40_6); save(sum40_6, file= "Summaries/N1/sum40_6.Rda")
M40_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M40_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M40_7<- coda.samples(M40_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum40_7<- summary(M40_7); save(sum40_7, file= "Summaries/N1/sum40_7.Rda")


# Diagnostics
#plot(M40, trace=FALSE)
gelman.diag(M40, confidence=0.95); gelman.plot(M40, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M40, smooth=TRUE); 
autocorr.diag(M40); autocorr.plot(M40, lagmax=20); acfplot(M40) # auto-correlations

gelman.diag(M40_2, confidence=0.95); gelman.plot(M40_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M40_2, smooth=TRUE); 
autocorr.diag(M40_2); autocorr.plot(M40_2, lagmax=20); acfplot(M40_2) # auto-correlations

gelman.diag(M40_3, confidence=0.95); gelman.plot(M40_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M40_3, smooth=TRUE); 
autocorr.diag(M40_3); autocorr.plot(M40_3, lagmax=20); acfplot(M40_3) # auto-correlations

gelman.diag(M40_4, confidence=0.95); gelman.plot(M40_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M40_4, smooth=TRUE); 
autocorr.diag(M40_4); autocorr.plot(M40_4, lagmax=20); acfplot(M40_4) # auto-correlations

gelman.diag(M40_5, confidence=0.95); gelman.plot(M40_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M40_5, smooth=TRUE); 
autocorr.diag(M40_5); autocorr.plot(M40_5, lagmax=20); acfplot(M40_5) # auto-correlations

gelman.diag(M40_6, confidence=0.95); gelman.plot(M40_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M40_6, smooth=TRUE); 
autocorr.diag(M40_6); autocorr.plot(M40_6, lagmax=20); acfplot(M40_6) # auto-correlations

gelman.diag(M40_7, confidence=0.95); gelman.plot(M40_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M40_7, smooth=TRUE); 
autocorr.diag(M40_7); autocorr.plot(M40_7, lagmax=20); acfplot(M40_7) # auto-correlations

#--------------------------------------
# M41 (N+1 effect,  measured with SFD):
data<- data7[,3:4]; 
M41_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M41.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M41<- coda.samples(M41_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum41<- summary(M41); sum41; save(sum41, file= "Summaries/N1/sum41.Rda")
M41_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M41_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M41_2<- coda.samples(M41_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum41_2<- summary(M41_2); save(sum41_2, file= "Summaries/N1/sum41_2.Rda")
M41_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M41_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M41_3<- coda.samples(M41_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum41_3<- summary(M41_3); save(sum41_3, file= "Summaries/N1/sum41_3.Rda")
M41_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M41_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M41_4<- coda.samples(M41_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum41_4<- summary(M41_4); save(sum41_4, file= "Summaries/N1/sum41_4.Rda")
M41_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M41_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M41_5<- coda.samples(M41_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum41_5<- summary(M41_5); save(sum41_5, file= "Summaries/N1/sum41_5.Rda")
M41_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M41_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M41_6<- coda.samples(M41_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum41_6<- summary(M41_6); save(sum41_6, file= "Summaries/N1/sum41_6.Rda")
M41_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M41_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M41_7<- coda.samples(M41_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum41_7<- summary(M41_7); save(sum41_7, file= "Summaries/N1/sum41_7.Rda")


# Diagnostics
#plot(M41, trace=FALSE)
gelman.diag(M41, confidence=0.95); gelman.plot(M41, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M41, smooth=TRUE); 
autocorr.diag(M41); autocorr.plot(M41, lagmax=20); acfplot(M41) # auto-correlations

gelman.diag(M41_2, confidence=0.95); gelman.plot(M41_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M41_2, smooth=TRUE); 
autocorr.diag(M41_2); autocorr.plot(M41_2, lagmax=20); acfplot(M41_2) # auto-correlations

gelman.diag(M41_3, confidence=0.95); gelman.plot(M41_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M41_3, smooth=TRUE); 
autocorr.diag(M41_3); autocorr.plot(M41_3, lagmax=20); acfplot(M41_3) # auto-correlations

gelman.diag(M41_4, confidence=0.95); gelman.plot(M41_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M41_4, smooth=TRUE); 
autocorr.diag(M41_4); autocorr.plot(M41_4, lagmax=20); acfplot(M41_4) # auto-correlations

gelman.diag(M41_5, confidence=0.95); gelman.plot(M41_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M41_5, smooth=TRUE); 
autocorr.diag(M41_5); autocorr.plot(M41_5, lagmax=20); acfplot(M41_5) # auto-correlations

gelman.diag(M41_6, confidence=0.95); gelman.plot(M41_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M41_6, smooth=TRUE); 
autocorr.diag(M41_6); autocorr.plot(M41_6, lagmax=20); acfplot(M41_6) # auto-correlations

gelman.diag(M41_7, confidence=0.95); gelman.plot(M41_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M41_7, smooth=TRUE); 
autocorr.diag(M41_7); autocorr.plot(M41_7, lagmax=20); acfplot(M41_7) # auto-correlations

#--------------------------------------
# M42 (N+1 effect,  measured with GD):
data<- data6[,3:4]; 
M42_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M42.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M42<- coda.samples(M42_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum42<- summary(M42); sum42; sum42; save(sum42, file= "Summaries/N1/sum42.Rda")
M42_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M42_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M42_2<- coda.samples(M42_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum42_2<- summary(M42_2); save(sum42_2, file= "Summaries/N1/sum42_2.Rda")
M42_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M42_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M42_3<- coda.samples(M42_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum42_3<- summary(M42_3); save(sum42_3, file= "Summaries/N1/sum42_3.Rda")
M42_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M42_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M42_4<- coda.samples(M42_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum42_4<- summary(M42_4); save(sum42_4, file= "Summaries/N1/sum42_4.Rda")
M42_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M42_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M42_5<- coda.samples(M42_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum42_5<- summary(M42_5); save(sum42_5, file= "Summaries/N1/sum42_5.Rda")
M42_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M42_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M42_6<- coda.samples(M42_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum42_6<- summary(M42_6); save(sum42_6, file= "Summaries/N1/sum42_6.Rda")
M42_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M42_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M42_7<- coda.samples(M42_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum42_7<- summary(M42_7); save(sum42_7, file= "Summaries/N1/sum42_7.Rda")

# Diagnostics
#plot(M42, trace=FALSE)
gelman.diag(M42, confidence=0.95); gelman.plot(M42, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M42, smooth=TRUE); 
autocorr.diag(M42); autocorr.plot(M42, lagmax=20); acfplot(M42) # auto-correlations

gelman.diag(M42_2, confidence=0.95); gelman.plot(M42_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M42_2, smooth=TRUE); 
autocorr.diag(M42_2); autocorr.plot(M42_2, lagmax=20); acfplot(M42_2) # auto-correlations

gelman.diag(M42_3, confidence=0.95); gelman.plot(M42_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M42_3, smooth=TRUE); 
autocorr.diag(M42_3); autocorr.plot(M42_3, lagmax=20); acfplot(M42_3) # auto-correlations

gelman.diag(M42_4, confidence=0.95); gelman.plot(M42_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M42_4, smooth=TRUE); 
autocorr.diag(M42_4); autocorr.plot(M42_4, lagmax=20); acfplot(M42_4) # auto-correlations

gelman.diag(M42_5, confidence=0.95); gelman.plot(M42_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M42_5, smooth=TRUE); 
autocorr.diag(M42_5); autocorr.plot(M42_5, lagmax=20); acfplot(M42_5) # auto-correlations

gelman.diag(M42_6, confidence=0.95); gelman.plot(M42_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M42_6, smooth=TRUE); 
autocorr.diag(M42_6); autocorr.plot(M42_6, lagmax=20); acfplot(M42_6) # auto-correlations

gelman.diag(M42_7, confidence=0.95); gelman.plot(M42_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M42_7, smooth=TRUE); 
autocorr.diag(M42_7); autocorr.plot(M42_7, lagmax=20); acfplot(M42_7) # auto-correlations

#--------------------------------------
# M43 (N+1 effect,  measured with TVT):
data<- data8[,3:4]; 
M43_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M43.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M43<- coda.samples(M43_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum43<- summary(M43); sum43; save(sum43, file= "Summaries/N1/sum43.Rda")
M43_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M43_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M43_2<- coda.samples(M43_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum43_2<- summary(M43_2); save(sum43_2, file= "Summaries/N1/sum43_2.Rda")
M43_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M43_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M43_3<- coda.samples(M43_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum43_3<- summary(M43_3); save(sum43_3, file= "Summaries/N1/sum43_3.Rda")
M43_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M43_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M43_4<- coda.samples(M43_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum43_4<- summary(M43_4); save(sum43_4, file= "Summaries/N1/sum43_4.Rda")
M43_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M43_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M43_5<- coda.samples(M43_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum43_5<- summary(M43_5); save(sum43_5, file= "Summaries/N1/sum43_5.Rda")
M43_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M43_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M43_6<- coda.samples(M43_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum43_6<- summary(M43_6); save(sum43_6, file= "Summaries/N1/sum43_6.Rda")
M43_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M43_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M43_7<- coda.samples(M43_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum43_7<- summary(M43_7); save(sum43_7, file= "Summaries/N1/sum43_7.Rda")


# Diagnostics
#plot(M43, trace=FALSE)
gelman.diag(M43, confidence=0.95); gelman.plot(M43, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M43, smooth=TRUE); 
autocorr.diag(M43); autocorr.plot(M43, lagmax=20); acfplot(M43) # auto-correlations

gelman.diag(M43_2, confidence=0.95); gelman.plot(M43_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M43_2, smooth=TRUE); 
autocorr.diag(M43_2); autocorr.plot(M43_2, lagmax=20); acfplot(M43_2) # auto-correlations

gelman.diag(M43_3, confidence=0.95); gelman.plot(M43_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M43_3, smooth=TRUE); 
autocorr.diag(M43_3); autocorr.plot(M43_3, lagmax=20); acfplot(M43_3) # auto-correlations

gelman.diag(M43_4, confidence=0.95); gelman.plot(M43_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M43_4, smooth=TRUE); 
autocorr.diag(M43_4); autocorr.plot(M43_4, lagmax=20); acfplot(M43_4) # auto-correlations

gelman.diag(M43_5, confidence=0.95); gelman.plot(M43_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M43_5, smooth=TRUE); 
autocorr.diag(M43_5); autocorr.plot(M43_5, lagmax=20); acfplot(M43_5) # auto-correlations

gelman.diag(M43_6, confidence=0.95); gelman.plot(M43_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M43_6, smooth=TRUE); 
autocorr.diag(M43_6); autocorr.plot(M43_6, lagmax=20); acfplot(M43_6) # auto-correlations

gelman.diag(M43_7, confidence=0.95); gelman.plot(M43_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M43_7, smooth=TRUE); 
autocorr.diag(M43_7); autocorr.plot(M43_7, lagmax=20); acfplot(M43_7) # auto-correlations


########################
#                      #
# ALPHABETICAL STUDIES #
#                      #
########################

# M44 (N+1 effect,  measured with FFD):
data<- data5[,2:4]; data<- subset(data, Language!= "Chinese"); data<- data[,2:3];
M44_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M44.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M44<- coda.samples(M44_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum44<- summary(M44); sum44; save(sum44, file= "Summaries/N1/sum44.Rda")
M44_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M44_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M44_2<- coda.samples(M44_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum44_2<- summary(M44_2); save(sum44_2, file= "Summaries/N1/sum44_2.Rda")
M44_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M44_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M44_3<- coda.samples(M44_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum44_3<- summary(M44_3); save(sum44_3, file= "Summaries/N1/sum44_3.Rda")
M44_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M44_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M44_4<- coda.samples(M44_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum44_4<- summary(M44_4); save(sum44_4, file= "Summaries/N1/sum44_4.Rda")
M44_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M44_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M44_5<- coda.samples(M44_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum44_5<- summary(M44_5); save(sum44_5, file= "Summaries/N1/sum44_5.Rda")
M44_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M44_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M44_6<- coda.samples(M44_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum44_6<- summary(M44_6); save(sum44_6, file= "Summaries/N1/sum44_6.Rda")
M44_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M44_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M44_7<- coda.samples(M44_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum44_7<- summary(M44_7); save(sum44_7, file= "Summaries/N1/sum44_7.Rda")



# Diagnostics
#plot(M44, trace=FALSE)
gelman.diag(M44, confidence=0.95); gelman.plot(M44, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M44, smooth=TRUE); 
autocorr.diag(M44); autocorr.plot(M44, lagmax=20); acfplot(M44) # auto-correlations

gelman.diag(M44_2, confidence=0.95); gelman.plot(M44_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M44_2, smooth=TRUE); 
autocorr.diag(M44_2); autocorr.plot(M44_2, lagmax=20); acfplot(M44_2) # auto-correlations

gelman.diag(M44_3, confidence=0.95); gelman.plot(M44_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M44_3, smooth=TRUE); 
autocorr.diag(M44_3); autocorr.plot(M44_3, lagmax=20); acfplot(M44_3) # auto-correlations

gelman.diag(M44_4, confidence=0.95); gelman.plot(M44_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M44_4, smooth=TRUE); 
autocorr.diag(M44_4); autocorr.plot(M44_4, lagmax=20); acfplot(M44_4) # auto-correlations

gelman.diag(M44_5, confidence=0.95); gelman.plot(M44_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M44_5, smooth=TRUE); 
autocorr.diag(M44_5); autocorr.plot(M44_5, lagmax=20); acfplot(M44_5) # auto-correlations

gelman.diag(M44_6, confidence=0.95); gelman.plot(M44_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M44_6, smooth=TRUE); 
autocorr.diag(M44_6); autocorr.plot(M44_6, lagmax=20); acfplot(M44_6) # auto-correlations

gelman.diag(M44_7, confidence=0.95); gelman.plot(M44_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M44_7, smooth=TRUE); 
autocorr.diag(M44_7); autocorr.plot(M44_7, lagmax=20); acfplot(M44_7) # auto-correlations

#--------------------------------------
# M45 (N+1 effect,  measured with SFD):
data<- data7[,2:4]; data<- subset(data, Language!= "Chinese"); data<- data[,2:3];
M45_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M45.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M45<- coda.samples(M45_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum45<- summary(M45); sum45; save(sum45, file= "Summaries/N1/sum45.Rda")
M45_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M45_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M45_2<- coda.samples(M45_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum45_2<- summary(M45_2); save(sum45_2, file= "Summaries/N1/sum45_2.Rda")
M45_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M45_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M45_3<- coda.samples(M45_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum45_3<- summary(M45_3); save(sum45_3, file= "Summaries/N1/sum45_3.Rda")
M45_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M45_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M45_4<- coda.samples(M45_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum45_4<- summary(M45_4); save(sum45_4, file= "Summaries/N1/sum45_4.Rda")
M45_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M45_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M45_5<- coda.samples(M45_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum45_5<- summary(M45_5); save(sum45_5, file= "Summaries/N1/sum45_5.Rda")
M45_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M45_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M45_6<- coda.samples(M45_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum45_6<- summary(M45_6); save(sum45_6, file= "Summaries/N1/sum45_6.Rda")
M45_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M45_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M45_7<- coda.samples(M45_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum45_7<- summary(M45_7); save(sum45_7, file= "Summaries/N1/sum45_7.Rda")


# Diagnostics
#plot(M45, trace=FALSE)
gelman.diag(M45, confidence=0.95); gelman.plot(M45, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M45, smooth=TRUE); 
autocorr.diag(M45); autocorr.plot(M45, lagmax=20); acfplot(M45) # auto-correlations

gelman.diag(M45_2, confidence=0.95); gelman.plot(M45_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M45_2, smooth=TRUE); 
autocorr.diag(M45_2); autocorr.plot(M45_2, lagmax=20); acfplot(M45_2) # auto-correlations

gelman.diag(M45_3, confidence=0.95); gelman.plot(M45_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M45_3, smooth=TRUE); 
autocorr.diag(M45_3); autocorr.plot(M45_3, lagmax=20); acfplot(M45_3) # auto-correlations

gelman.diag(M45_4, confidence=0.95); gelman.plot(M45_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M45_4, smooth=TRUE); 
autocorr.diag(M45_4); autocorr.plot(M45_4, lagmax=20); acfplot(M45_4) # auto-correlations

gelman.diag(M45_5, confidence=0.95); gelman.plot(M45_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M45_5, smooth=TRUE); 
autocorr.diag(M45_5); autocorr.plot(M45_5, lagmax=20); acfplot(M45_5) # auto-correlations

gelman.diag(M45_6, confidence=0.95); gelman.plot(M45_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M45_6, smooth=TRUE); 
autocorr.diag(M45_6); autocorr.plot(M45_6, lagmax=20); acfplot(M45_6) # auto-correlations

gelman.diag(M45_7, confidence=0.95); gelman.plot(M45_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M45_7, smooth=TRUE); 
autocorr.diag(M45_7); autocorr.plot(M45_7, lagmax=20); acfplot(M45_7) # auto-correlations

#--------------------------------------
# M46 (N+1 effect,  measured with GD):
data<- data6[,2:4]; data<- subset(data, Language!= "Chinese"); data<- data[,2:3];
M46_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M46.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M46<- coda.samples(M46_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum46<- summary(M46); sum46; save(sum46, file= "Summaries/N1/sum46.Rda")
M46_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M46_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M46_2<- coda.samples(M46_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum46_2<- summary(M46_2); save(sum46_2, file= "Summaries/N1/sum46_2.Rda")
M46_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M46_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M46_3<- coda.samples(M46_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum46_3<- summary(M46_3); save(sum46_3, file= "Summaries/N1/sum46_3.Rda")
M46_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M46_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M46_4<- coda.samples(M46_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum46_4<- summary(M46_4); save(sum46_4, file= "Summaries/N1/sum46_4.Rda")
M46_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M46_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M46_5<- coda.samples(M46_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum46_5<- summary(M46_5); save(sum46_5, file= "Summaries/N1/sum46_5.Rda")
M46_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M46_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M46_6<- coda.samples(M46_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum46_6<- summary(M46_6); save(sum46_6, file= "Summaries/N1/sum46_6.Rda")
M46_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M46_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M46_7<- coda.samples(M46_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum46_7<- summary(M46_7); save(sum46_7, file= "Summaries/N1/sum46_7.Rda")


# Diagnostics
#plot(M46, trace=FALSE)
gelman.diag(M46, confidence=0.95); gelman.plot(M46, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M46, smooth=TRUE); 
autocorr.diag(M46); autocorr.plot(M46, lagmax=20); acfplot(M46) # auto-correlations

gelman.diag(M46_2, confidence=0.95); gelman.plot(M46_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M46_2, smooth=TRUE); 
autocorr.diag(M46_2); autocorr.plot(M46_2, lagmax=20); acfplot(M46_2) # auto-correlations

gelman.diag(M46_3, confidence=0.95); gelman.plot(M46_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M46_3, smooth=TRUE); 
autocorr.diag(M46_3); autocorr.plot(M46_3, lagmax=20); acfplot(M46_3) # auto-correlations

gelman.diag(M46_4, confidence=0.95); gelman.plot(M46_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M46_4, smooth=TRUE); 
autocorr.diag(M46_4); autocorr.plot(M46_4, lagmax=20); acfplot(M46_4) # auto-correlations

gelman.diag(M46_5, confidence=0.95); gelman.plot(M46_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M46_5, smooth=TRUE); 
autocorr.diag(M46_5); autocorr.plot(M46_5, lagmax=20); acfplot(M46_5) # auto-correlations

gelman.diag(M46_6, confidence=0.95); gelman.plot(M46_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M46_6, smooth=TRUE); 
autocorr.diag(M46_6); autocorr.plot(M46_6, lagmax=20); acfplot(M46_6) # auto-correlations

gelman.diag(M46_7, confidence=0.95); gelman.plot(M46_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M46_7, smooth=TRUE); 
autocorr.diag(M46_7); autocorr.plot(M46_7, lagmax=20); acfplot(M46_7) # auto-correlations

#--------------------------------------
# M47 (N+1 effect,  measured with TVT): SAME AS IN PREVIOUS SUB-SECTION
data<- data8[,2:4]; data<- subset(data, Language!= "Chinese"); data<- data[,2:3];
M47_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M47.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M47<- coda.samples(M47_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum47<- summary(M47); sum47; save(sum47, file= "Summaries/N1/sum47.Rda")
M47_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M47_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M47_2<- coda.samples(M47_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum47_2<- summary(M47_2); save(sum47_2, file= "Summaries/N1/sum47_2.Rda")
M47_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M47_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M47_3<- coda.samples(M47_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum47_3<- summary(M47_3); save(sum47_3, file= "Summaries/N1/sum47_3.Rda")
M47_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M47_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M47_4<- coda.samples(M47_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum47_4<- summary(M47_4); save(sum47_4, file= "Summaries/N1/sum47_4.Rda")
M47_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M47_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M47_5<- coda.samples(M47_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum47_5<- summary(M47_5); save(sum47_5, file= "Summaries/N1/sum47_5.Rda")
M47_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M47_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M47_6<- coda.samples(M47_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum47_6<- summary(M47_6); save(sum47_6, file= "Summaries/N1/sum47_6.Rda")
M47_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M47_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M47_7<- coda.samples(M47_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum47_7<- summary(M47_7); save(sum47_7, file= "Summaries/N1/sum47_7.Rda")


# Diagnostics
#plot(M47, trace=FALSE)
gelman.diag(M47, confidence=0.95); gelman.plot(M47, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M47, smooth=TRUE); 
autocorr.diag(M47); autocorr.plot(M47, lagmax=20); acfplot(M47) # auto-correlations

gelman.diag(M47_2, confidence=0.95); gelman.plot(M47_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M47_2, smooth=TRUE); 
autocorr.diag(M47_2); autocorr.plot(M47_2, lagmax=20); acfplot(M47_2) # auto-correlations

gelman.diag(M47_3, confidence=0.95); gelman.plot(M47_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M47_3, smooth=TRUE); 
autocorr.diag(M47_3); autocorr.plot(M47_3, lagmax=20); acfplot(M47_3) # auto-correlations

gelman.diag(M47_4, confidence=0.95); gelman.plot(M47_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M47_4, smooth=TRUE); 
autocorr.diag(M47_4); autocorr.plot(M47_4, lagmax=20); acfplot(M47_4) # auto-correlations

gelman.diag(M47_5, confidence=0.95); gelman.plot(M47_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M47_5, smooth=TRUE); 
autocorr.diag(M47_5); autocorr.plot(M47_5, lagmax=20); acfplot(M47_5) # auto-correlations

gelman.diag(M47_6, confidence=0.95); gelman.plot(M47_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M47_6, smooth=TRUE); 
autocorr.diag(M47_6); autocorr.plot(M47_6, lagmax=20); acfplot(M47_6) # auto-correlations

gelman.diag(M47_7, confidence=0.95); gelman.plot(M47_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M47_7, smooth=TRUE); 
autocorr.diag(M47_7); autocorr.plot(M47_7, lagmax=20); acfplot(M47_7) # auto-correlations


############################
#                          #
# NON-ALPHABETICAL STUDIES #
#                          #
############################

# M48 (N+1 effect,  measured with FFD):
data<- data5[,2:4]; data<- subset(data, Language== "Chinese"); data<- data[,2:3];
M48_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M48.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M48<- coda.samples(M48_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum48<- summary(M48); sum48; save(sum48, file= "Summaries/N1/sum48.Rda")
M48_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M48_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M48_2<- coda.samples(M48_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum48_2<- summary(M48_2); save(sum48_2, file= "Summaries/N1/sum48_2.Rda")
M48_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M48_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M48_3<- coda.samples(M48_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum48_3<- summary(M48_3); save(sum48_3, file= "Summaries/N1/sum48_3.Rda")
M48_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M48_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M48_4<- coda.samples(M48_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum48_4<- summary(M48_4); save(sum48_4, file= "Summaries/N1/sum48_4.Rda")
M48_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M48_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M48_5<- coda.samples(M48_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum48_5<- summary(M48_5); save(sum48_5, file= "Summaries/N1/sum48_5.Rda")
M48_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M48_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M48_6<- coda.samples(M48_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum48_6<- summary(M48_6); save(sum48_6, file= "Summaries/N1/sum48_6.Rda")
M48_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M48_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M48_7<- coda.samples(M48_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum48_7<- summary(M48_7); save(sum48_7, file= "Summaries/N1/sum48_7.Rda")


# Diagnostics
#plot(M48, trace=FALSE)
gelman.diag(M48, confidence=0.95); gelman.plot(M48, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M48, smooth=TRUE); 
autocorr.diag(M48); autocorr.plot(M48, lagmax=20); acfplot(M48) # auto-correlations

gelman.diag(M48_2, confidence=0.95); gelman.plot(M48_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M48_2, smooth=TRUE); 
autocorr.diag(M48_2); autocorr.plot(M48_2, lagmax=20); acfplot(M48_2) # auto-correlations

gelman.diag(M48_3, confidence=0.95); gelman.plot(M48_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M48_3, smooth=TRUE); 
autocorr.diag(M48_3); autocorr.plot(M48_3, lagmax=20); acfplot(M48_3) # auto-correlations

gelman.diag(M48_4, confidence=0.95); gelman.plot(M48_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M48_4, smooth=TRUE); 
autocorr.diag(M48_4); autocorr.plot(M48_4, lagmax=20); acfplot(M48_4) # auto-correlations

gelman.diag(M48_5, confidence=0.95); gelman.plot(M48_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M48_5, smooth=TRUE); 
autocorr.diag(M48_5); autocorr.plot(M48_5, lagmax=20); acfplot(M48_5) # auto-correlations

gelman.diag(M48_6, confidence=0.95); gelman.plot(M48_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M48_6, smooth=TRUE); 
autocorr.diag(M48_6); autocorr.plot(M48_6, lagmax=20); acfplot(M48_6) # auto-correlations

gelman.diag(M48_7, confidence=0.95); gelman.plot(M48_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M48_7, smooth=TRUE); 
autocorr.diag(M48_7); autocorr.plot(M48_7, lagmax=20); acfplot(M48_7) # auto-correlations

#--------------------------------------
# M49 (N+1 effect,  measured with SFD):
data<- data7[,2:4]; data<- subset(data, Language== "Chinese"); data<- data[,2:3];
M49_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M49.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M49<- coda.samples(M49_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum49<- summary(M49); sum49; save(sum49, file= "Summaries/N1/sum49.Rda")
M49_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M49_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M49_2<- coda.samples(M49_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum49_2<- summary(M49_2); save(sum49_2, file= "Summaries/N1/sum49_2.Rda")
M49_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M49_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M49_3<- coda.samples(M49_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum49_3<- summary(M49_3); save(sum49_3, file= "Summaries/N1/sum49_3.Rda")
M49_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M49_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M49_4<- coda.samples(M49_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum49_4<- summary(M49_4); save(sum49_4, file= "Summaries/N1/sum49_4.Rda")
M49_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M49_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M49_5<- coda.samples(M49_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum49_5<- summary(M49_5); save(sum49_5, file= "Summaries/N1/sum49_5.Rda")
M49_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M49_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M49_6<- coda.samples(M49_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum49_6<- summary(M49_6); save(sum49_6, file= "Summaries/N1/sum49_6.Rda")
M49_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M49_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M49_7<- coda.samples(M49_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum49_7<- summary(M49_7); save(sum49_7, file= "Summaries/N1/sum49_7.Rda")


# Diagnostics
#plot(M49, trace=FALSE)
gelman.diag(M49, confidence=0.95); gelman.plot(M49, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M49, smooth=TRUE); 
autocorr.diag(M49); autocorr.plot(M49, lagmax=20); acfplot(M49) # auto-correlations

gelman.diag(M49_2, confidence=0.95); gelman.plot(M49_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M49_2, smooth=TRUE); 
autocorr.diag(M49_2); autocorr.plot(M49_2, lagmax=20); acfplot(M49_2) # auto-correlations

gelman.diag(M49_3, confidence=0.95); gelman.plot(M49_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M49_3, smooth=TRUE); 
autocorr.diag(M49_3); autocorr.plot(M49_3, lagmax=20); acfplot(M49_3) # auto-correlations

gelman.diag(M49_4, confidence=0.95); gelman.plot(M49_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M49_4, smooth=TRUE); 
autocorr.diag(M49_4); autocorr.plot(M49_4, lagmax=20); acfplot(M49_4) # auto-correlations

gelman.diag(M49_5, confidence=0.95); gelman.plot(M49_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M49_5, smooth=TRUE); 
autocorr.diag(M49_5); autocorr.plot(M49_5, lagmax=20); acfplot(M49_5) # auto-correlations

gelman.diag(M49_6, confidence=0.95); gelman.plot(M49_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M49_6, smooth=TRUE); 
autocorr.diag(M49_6); autocorr.plot(M49_6, lagmax=20); acfplot(M49_6) # auto-correlations

gelman.diag(M49_7, confidence=0.95); gelman.plot(M49_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M49_7, smooth=TRUE); 
autocorr.diag(M49_7); autocorr.plot(M49_7, lagmax=20); acfplot(M49_7) # auto-correlations

#--------------------------------------
# M50 (N+1 effect,  measured with GD):
data<- data6[,2:4]; data<- subset(data, Language== "Chinese"); data<- data[,2:3];
M50_M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M50.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE) # MAIN
M50<- coda.samples(M50_M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum50<- summary(M50); sum50; save(sum50, file= "Summaries/N1/sum50.Rda")
M50_2M <-jags.model(JModel("dunif(-200, 200)", "dunif(0, 200)", nrow(data), "N1_M50_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=20, "tau"=5))
M50_2<- coda.samples(M50_2M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum50_2<- summary(M50_2); save(sum50_2, file= "Summaries/N1/sum50_2.Rda")
M50_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 200)", nrow(data), "N1_M50_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M50_3<- coda.samples(M50_3M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum50_3<- summary(M50_3); save(sum50_3, file= "Summaries/N1/sum50_3.Rda")
M50_4M <-jags.model(JModel("dunif(-200, 200)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M50_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M50_4<- coda.samples(M50_4M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum50_4<- summary(M50_4); save(sum50_4, file= "Summaries/N1/sum50_4.Rda")
M50_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "N1_M50_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M50_5<- coda.samples(M50_5M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum50_5<- summary(M50_5); save(sum50_5, file= "Summaries/N1/sum50_5.Rda")
M50_6M <-jags.model(JModel("dunif(-200, 200)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M50_6.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M50_6<- coda.samples(M50_6M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum50_6<- summary(M50_6); save(sum50_6, file= "Summaries/N1/sum50_6.Rda")
M50_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "N1_M50_7.txt"),
                    data, n.chains=3, n.adapt=3000, quiet=FALSE)
M50_7<- coda.samples(M50_7M, c('mu', 'tau', 'theta'), n.iter=75000, thin=5)
sum50_7<- summary(M50_7); save(sum50_7, file= "Summaries/N1/sum50_7.Rda")


# Diagnostics
#plot(M50, trace=FALSE)
gelman.diag(M50, confidence=0.95); gelman.plot(M50, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M50, smooth=TRUE); 
autocorr.diag(M50); autocorr.plot(M50, lagmax=20); acfplot(M50) # auto-correlations

gelman.diag(M50_2, confidence=0.95); gelman.plot(M50_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M50_2, smooth=TRUE); 
autocorr.diag(M50_2); autocorr.plot(M50_2, lagmax=20); acfplot(M50_2) # auto-correlations

gelman.diag(M50_3, confidence=0.95); gelman.plot(M50_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M50_3, smooth=TRUE); 
autocorr.diag(M50_3); autocorr.plot(M50_3, lagmax=20); acfplot(M50_3) # auto-correlations

gelman.diag(M50_4, confidence=0.95); gelman.plot(M50_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M50_4, smooth=TRUE); 
autocorr.diag(M50_4); autocorr.plot(M50_4, lagmax=20); acfplot(M50_4) # auto-correlations

gelman.diag(M50_5, confidence=0.95); gelman.plot(M50_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M50_5, smooth=TRUE); 
autocorr.diag(M50_5); autocorr.plot(M50_5, lagmax=20); acfplot(M50_5) # auto-correlations

gelman.diag(M50_6, confidence=0.95); gelman.plot(M50_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M50_6, smooth=TRUE); 
autocorr.diag(M50_6); autocorr.plot(M50_6, lagmax=20); acfplot(M50_6) # auto-correlations

gelman.diag(M50_7, confidence=0.95); gelman.plot(M50_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M50_7, smooth=TRUE); 
autocorr.diag(M50_7); autocorr.plot(M50_7, lagmax=20); acfplot(M50_7) # auto-correlations


#----------------------
# sensitivity analysis:
#----------------------

source("sensitivity_analysis.R")

#FFD
data<- data5[,3:4]; 
Sens1<- sensitivity_analysis(data, "dunif(-200, 200)", "dunif(0, 200)") 
save(Sens1, file= "Summaries/Sensitivity/N1_FFD.Rda")

#SFD
data<- data7[,3:4]; 
Sens2<- sensitivity_analysis(data, "dunif(-200, 200)", "dunif(0, 200)") 
save(Sens2, file= "Summaries/Sensitivity/N1_SFD.Rda")

#GD
data<- data6[,3:4]; 
Sens3<- sensitivity_analysis(data, "dunif(-200, 200)", "dunif(0, 200)") 
save(Sens3, file= "Summaries/Sensitivity/N1_GD.Rda")

#TVT
data<- data8[,3:4]; 
Sens4<- sensitivity_analysis(data, "dunif(-200, 200)", "dunif(0, 200)") 
save(Sens4, file= "Summaries/Sensitivity/N1_TVT.Rda")







#ECDFs:

S40<-jags.samples(M40_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S40<-c(S40$mu[1,,1],S40$mu[1,,2],S40$mu[1,,3])
ECDF40<- ecdf(S40); ECDF40(29)

S41<-jags.samples(M41_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S41<-c(S41$mu[1,,1],S41$mu[1,,2],S41$mu[1,,3])

S42<-jags.samples(M42_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S42<-c(S42$mu[1,,1],S42$mu[1,,2],S42$mu[1,,3])
ECDF42<- ecdf(S42); ECDF42(44.5)

S43<-jags.samples(M43_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000)
S43<-c(S43$mu[1,,1],S43$mu[1,,2],S43$mu[1,,3])

########################
# Posterior histograms #
########################
# requested by Reviewer 2

png(file = 'Plots/N1_hist.png', width = 800, height = 600, units = "px")  
par(mfrow=c(2,2))

hist(S40, breaks=30, col="#828282", freq=FALSE, main="All studies (FFD)",
     xlab= "N+1 effect size (in ms)", cex.lab=1.4, cex.axis=1.2)  

hist(S41, breaks=30, col="#828282", freq=FALSE, main="All studies (SFD)",
     xlab= "N+1 effect size (in ms)", cex.lab=1.4, cex.axis=1.2)    
hist(S42, breaks=30, col="#828282", freq=FALSE, main="All studies (GD)",
     xlab= "N+1 effect size (in ms)", cex.lab=1.4, cex.axis=1.2)  

hist(S43, breaks=30, col="#828282", freq=FALSE, main="All studies (TVT)",
     xlab= "N+1 effect size (in ms)", cex.lab=1.4, cex.axis=1.2)    

dev.off()     

