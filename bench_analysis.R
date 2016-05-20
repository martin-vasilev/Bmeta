# Data analysis script for the benchmark data (Martin Vasilev, 2016)

# Here, I analyse fixation durations from n+1 stuides when the preview was valid (i.e. normal reading)

rm(list=ls())

library(rjags)
library(ggplot2)

source("get_bench.R") # load function for retrieving data



get_bench() # compile benchmark data

# load compiled data:
load('Data/bench_FFD.Rda'); colnames(bench_FFD)<- c("ID", "Language", "N", "M", "SD")
load('Data/bench_SFD.Rda'); colnames(bench_SFD)<- c("ID", "Language", "N", "M", "SD") 
load('Data/bench_GD.Rda'); colnames(bench_GD)<- c("ID", "Language", "N", "M", "SD")
load('Data/bench_TVT.Rda'); colnames(bench_TVT)<- c("ID", "Language", "N", "M", "SD")


#get counts:
table(bench_FFD$Language); nrow(bench_FFD)
table(bench_SFD$Language); nrow(bench_SFD)
table(bench_GD$Language); nrow(bench_GD)
table(bench_TVT$Language); nrow(bench_TVT)

# Descriptive plots:
data<-rbind(bench_FFD, bench_SFD, bench_GD, bench_TVT) # combine all data in one
M<- c(rep("FFD", each=nrow(bench_FFD)), rep("SFD", each=nrow(bench_SFD)), 
      rep("GD", each=nrow(bench_GD)), rep("TVT", each=nrow(bench_TVT)))
data$Measure<- M; # vector for measure type
data$Measure<- ordered(data$Measure, levels = c("FFD", "SFD", "GD", "TVT"))
table(data$Measure)

# Violin plot (all studies)
P1 <- ggplot(data, aes(x=Measure, y=M, fill=Measure)) + 
   scale_y_continuous(breaks=c(50,100,150,200,250,300,
                               350,400,450,500))+ theme_bw() +
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
        ylab('Fixation Duration (in ms)') + xlab('Type of measure')+
        ggtitle("Violin Plot for Fixation Durations During Valid Preview (all studies)")+
        guides(fill = guide_legend(override.aes = list(colour = NULL)))
                                            # + geom_jitter(shape=16, position=position_jitter(0.2))
png(file = 'Plots/BenchVio_all.png', width = 800, height = 600, units = "px")
P1
dev.off()


# Violin plot (alphabetical studies)
P2 <- ggplot(subset(data, Language!="Chinese"), aes(x=Measure, y=M, fill=Measure)) + 
  scale_y_continuous(breaks=c(50,100,150,200,250,300,
                              350,400,450,500))+ theme_bw() +
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
  ylab('Fixation Duration (in ms)') + xlab('Type of measure')+
  ggtitle("Violin Plot for Fixation Durations During Valid Preview (alphabetical studies)")+
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
# + geom_jitter(shape=16, position=position_jitter(0.2))
png(file = 'Plots/BenchVio_alpha.png', width = 800, height = 600, units = "px")
P2
dev.off()


# Violin plot (Chinese studies)
P3 <- ggplot(subset(data, Language=="Chinese"), aes(x=Measure, y=M, fill=Measure)) + 
  scale_y_continuous(breaks=c(50,100,150,200,250,300,
                              350,400,450,500))+ theme_bw() +
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
  ylab('Fixation Duration (in ms)') + xlab('Type of measure')+
  ggtitle("Violin Plot for Fixation Durations During Valid Preview (Chinese studies)")+
  guides(fill = guide_legend(override.aes = list(colour = NULL)))
# + geom_jitter(shape=16, position=position_jitter(0.2))
png(file = 'Plots/BenchVio_Chinese.png', width = 800, height = 600, units = "px")
P3
dev.off()


#------------------------------------------------------------------------------------------------
#                                       Models:
#------------------------------------------------------------------------------------------------

library(rjags)
source("JModel.R")

##############
# All studies:
##############

# FFD: 
data<- bench_FFD
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr"); #data<- subset(data, S.sqr!="NA")

M1_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 800)", nrow(data), "Bench1.txt"), data, n.chains=3,
               n.adapt=3000, quiet=TRUE)
M1<- coda.samples(M1_M, c('mu', 'tau'), n.iter=75000, thin=5)
sum1<-summary(M1); save(sum1, file="Summaries/bench/sum1.Rda")
M1_2M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 800)", nrow(data), "Bench1_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=50, "tau"=5))
M1_2<- coda.samples(M1_2M, c('mu', 'tau'), n.iter=75000, thin=5)
sum1_2<-summary(M1_2); save(sum1_2, file="Summaries/bench/sum1_2.Rda")
M1_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 800)", nrow(data), "Bench1_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_3<- coda.samples(M1_3M, c('mu', 'tau'), n.iter=75000, thin=5)
sum1_3<-summary(M1_3); save(sum1_3, file="Summaries/bench/sum1_3.Rda")
M1_4M <-jags.model(JModel("dunif(0, 800)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "Bench1_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_4<- coda.samples(M1_4M, c('mu', 'tau'), n.iter=75000, thin=5)
sum1_4<-summary(M1_4); save(sum1_4, file="Summaries/bench/sum1_4.Rda")
M1_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "Bench1_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_5<- coda.samples(M1_5M, c('mu', 'tau'), n.iter=75000, thin=5)
sum1_5<-summary(M1_5); save(sum1_5, file="Summaries/bench/sum1_5.Rda")
M1_6M <-jags.model(JModel("dunif(0, 800)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "Bench1_6.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_6<- coda.samples(M1_6M, c('mu', 'tau'), n.iter=75000, thin=5)
sum1_6<-summary(M1_6); save(sum1_6, file="Summaries/bench/sum1_6.Rda")
M1_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "Bench1_7.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M1_7<- coda.samples(M1_7M, c('mu', 'tau'), n.iter=75000, thin=5)
sum1_7<-summary(M1_7); save(sum1_7, file="Summaries/bench/sum1_7.Rda")

# Diagnostics
#plot(M1, trace=FALSE)
gelman.diag(M1, confidence=0.95); gelman.plot(M1, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1, smooth=TRUE); 
autocorr.diag(M1); autocorr.plot(M1, lagmax=20); acfplot(M1) # auto-correlations

gelman.diag(M1_2, confidence=0.95); gelman.plot(M1_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_2, smooth=TRUE); 
autocorr.diag(M1_2); autocorr.plot(M1_2, lagmax=20); acfplot(M1_2) # auto-correlations

gelman.diag(M1_3, confidence=0.95); gelman.plot(M1_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_3, smooth=TRUE); 
autocorr.diag(M1_3); autocorr.plot(M1_3, lagmax=20); acfplot(M1_3) # auto-correlations

gelman.diag(M1_4, confidence=0.95); gelman.plot(M1_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_4, smooth=TRUE); 
autocorr.diag(M1_4); autocorr.plot(M1_4, lagmax=20); acfplot(M1_4) # auto-correlations

gelman.diag(M1_5, confidence=0.95); gelman.plot(M1_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_5, smooth=TRUE); 
autocorr.diag(M1_5); autocorr.plot(M1_5, lagmax=20); acfplot(M1_5) # auto-correlations

gelman.diag(M1_6, confidence=0.95); gelman.plot(M1_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_6, smooth=TRUE); 
autocorr.diag(M1_6); autocorr.plot(M1_6, lagmax=20); acfplot(M1_6) # auto-correlations

gelman.diag(M1_7, confidence=0.95); gelman.plot(M1_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M1_7, smooth=TRUE); 
autocorr.diag(M1_7); autocorr.plot(M1_7, lagmax=20); acfplot(M1_7) # auto-correlations

# SFD: 
data<- bench_SFD
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr"); #data<- subset(data, S.sqr!="NA")

M2_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 800)", nrow(data), "Bench2.txt"), data, n.chains=3,
               n.adapt=3000, quiet=TRUE)
M2<- coda.samples(M2_M, c('mu', 'tau'), n.iter=75000, thin=5)
sum2<-summary(M2); save(sum2, file="Summaries/bench/sum2.Rda")
M2_2M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 800)", nrow(data), "Bench2_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                 inits= list("mu"=50, "tau"=5))
M2_2<- coda.samples(M2_2M, c('mu', 'tau'), n.iter=75000, thin=5)
sum2_2<-summary(M2_2); save(sum2_2, file="Summaries/bench/sum2_2.Rda")
M2_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 800)", nrow(data), "Bench2_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_3<- coda.samples(M2_3M, c('mu', 'tau'), n.iter=75000, thin=5)
sum2_3<-summary(M2_3); save(sum2_3, file="Summaries/bench/sum2_3.Rda")
M2_4M <-jags.model(JModel("dunif(0, 800)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "Bench2_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_4<- coda.samples(M2_4M, c('mu', 'tau'), n.iter=75000, thin=5)
sum2_4<-summary(M2_4); save(sum2_4, file="Summaries/bench/sum2_4.Rda")
M2_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "Bench2_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_5<- coda.samples(M2_5M, c('mu', 'tau'), n.iter=75000, thin=5)
sum2_5<-summary(M2_5); save(sum2_5, file="Summaries/bench/sum2_5.Rda")
M2_6M <-jags.model(JModel("dunif(0, 800)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "Bench2_6.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_6<- coda.samples(M2_6M, c('mu', 'tau'), n.iter=75000, thin=5)
sum2_6<-summary(M2_6); save(sum2_6, file="Summaries/bench/sum2_6.Rda")
M2_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "Bench2_7.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M2_7<- coda.samples(M2_7M, c('mu', 'tau'), n.iter=75000, thin=5)
sum2_7<-summary(M2_7); save(sum2_7, file="Summaries/bench/sum2_7.Rda")

# Diagnostics
#plot(M1, trace=FALSE)
gelman.diag(M2, confidence=0.95); gelman.plot(M2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2, smooth=TRUE); 
autocorr.diag(M2); autocorr.plot(M2, lagmax=20); acfplot(M2) # auto-correlations

gelman.diag(M2_2, confidence=0.95); gelman.plot(M2_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_2, smooth=TRUE); 
autocorr.diag(M2_2); autocorr.plot(M2_2, lagmax=20); acfplot(M2_2) # auto-correlations

gelman.diag(M2_3, confidence=0.95); gelman.plot(M2_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_3, smooth=TRUE); 
autocorr.diag(M2_3); autocorr.plot(M2_3, lagmax=20); acfplot(M2_3) # auto-correlations

gelman.diag(M2_4, confidence=0.95); gelman.plot(M2_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_4, smooth=TRUE); 
autocorr.diag(M2_4); autocorr.plot(M2_4, lagmax=20); acfplot(M2_4) # auto-correlations

gelman.diag(M2_5, confidence=0.95); gelman.plot(M2_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_5, smooth=TRUE); 
autocorr.diag(M2_5); autocorr.plot(M2_5, lagmax=20); acfplot(M2_5) # auto-correlations

gelman.diag(M2_6, confidence=0.95); gelman.plot(M2_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_6, smooth=TRUE); 
autocorr.diag(M2_6); autocorr.plot(M2_6, lagmax=20); acfplot(M2_6) # auto-correlations

gelman.diag(M2_7, confidence=0.95); gelman.plot(M2_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M2_7, smooth=TRUE); 
autocorr.diag(M2_7); autocorr.plot(M2_7, lagmax=20); acfplot(M2_7) # auto-correlations

# GD: 
data<- bench_GD
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr");  #data<- subset(data, S.sqr!="NA")

M3_M <-jags.model(JModel("dunif(0, 1000)", "dunif(0, 1000)", nrow(data), "Bench3.txt"), data, n.chains=3,
                 n.adapt=3000, quiet=TRUE)
M3<- coda.samples(M3_M, c('mu', 'tau'), n.iter=75000, thin=5)
sum3<- summary(M3); save(sum3, file="Summaries/bench/sum3.Rda")
M3_2M <-jags.model(JModel("dunif(0, 1000)", "dunif(0, 1000)", nrow(data), "Bench3_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=50, "tau"=5))
M3_2<- coda.samples(M3_2M, c('mu', 'tau'), n.iter=75000, thin=5)
sum3_2<- summary(M3_2); save(sum3_2, file="Summaries/bench/sum3_2.Rda")
M3_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 1000)", nrow(data), "Bench3_3.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_3<- coda.samples(M3_3M, c('mu', 'tau'), n.iter=75000, thin=5)
sum3_3<- summary(M3_3); save(sum3_3, file="Summaries/bench/sum3_3.Rda")
M3_4M <-jags.model(JModel("dunif(0, 1000)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "Bench3_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_4<- coda.samples(M3_4M, c('mu', 'tau'), n.iter=75000, thin=5)
sum3_4<- summary(M3_4); save(sum3_4, file="Summaries/bench/sum3_4.Rda")
M3_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "Bench3_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_5<- coda.samples(M3_5M, c('mu', 'tau'), n.iter=75000, thin=5)
sum3_5<- summary(M3_5); save(sum3_5, file="Summaries/bench/sum3_5.Rda")
M3_6M <-jags.model(JModel("dunif(0, 1000)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "Bench3_6.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_6<- coda.samples(M3_6M, c('mu', 'tau'), n.iter=75000, thin=5)
sum3_6<- summary(M3_6); save(sum3_6, file="Summaries/bench/sum3_6.Rda")
M3_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "Bench3_7.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M3_7<- coda.samples(M3_7M, c('mu', 'tau'), n.iter=75000, thin=5)
sum3_7<- summary(M3_7); save(sum3_7, file="Summaries/bench/sum3_7.Rda")


# Diagnostics
#plot(M3, trace=FALSE)
gelman.diag(M3, confidence=0.95); gelman.plot(M3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3, smooth=TRUE); 
autocorr.diag(M3); autocorr.plot(M3, lagmax=20); acfplot(M3) # auto-correlations

#plot(M3_2, trace=FALSE)
gelman.diag(M3_2, confidence=0.95); gelman.plot(M3_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_2, smooth=TRUE); 
autocorr.diag(M3_2); autocorr.plot(M3_2, lagmax=20); acfplot(M3_2) # auto-correlations

#plot(M3_3, trace=FALSE)
gelman.diag(M3_3, confidence=0.95); gelman.plot(M3_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_3, smooth=TRUE); 
autocorr.diag(M3_3); autocorr.plot(M3_3, lagmax=20); acfplot(M3_3) # auto-correlations

#plot(M3_4, trace=FALSE)
gelman.diag(M3_4, confidence=0.95); gelman.plot(M3_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_4, smooth=TRUE); 
autocorr.diag(M3_4); autocorr.plot(M3_4, lagmax=20); acfplot(M3_4) # auto-correlations

#plot(M3_5, trace=FALSE)
gelman.diag(M3_5, confidence=0.95); gelman.plot(M3_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_5, smooth=TRUE); 
autocorr.diag(M3_5); autocorr.plot(M3_5, lagmax=20); acfplot(M3_5) # auto-correlations

#plot(M3_6, trace=FALSE)
gelman.diag(M3_6, confidence=0.95); gelman.plot(M3_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_6, smooth=TRUE); 
autocorr.diag(M3_6); autocorr.plot(M3_6, lagmax=20); acfplot(M3_6) # auto-correlations

#plot(M3_7, trace=FALSE)
gelman.diag(M3_7, confidence=0.95); gelman.plot(M3_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M3_7, smooth=TRUE); 
autocorr.diag(M3_7); autocorr.plot(M3_7, lagmax=20); acfplot(M3_7) # auto-correlations


# TVT: 
data<- bench_TVT
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr"); # data<- subset(data, S.sqr!="NA")

M4_M <-jags.model(JModel("dunif(0, 1000)", "dunif(0, 1000)", nrow(data), "Bench4.txt"), data, n.chains=3,
                 n.adapt=3000, quiet=TRUE)
M4<- coda.samples(M4_M, c('mu', 'tau'), n.iter=75000, thin=5)
sum4<- summary(M4); save(sum4, file="Summaries/bench/sum4.Rda")
M4_2M <-jags.model(JModel("dunif(0, 1000)", "dunif(0, 1000)", nrow(data), "Bench4_2.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
                  inits= list("mu"=50, "tau"=5))
M4_2<- coda.samples(M4_2M, c('mu', 'tau'), n.iter=75000, thin=5)
sum4_2<- summary(M4_2); save(sum4_2, file="Summaries/bench/sum4_2.Rda")
M4_3M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dunif(0, 1000)", nrow(data), "Bench4_3.txt"), # ^-5 because there are fewer studies..
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_3<- coda.samples(M4_3M, c('mu', 'tau'), n.iter=75000, thin=5)
sum4_3<- summary(M4_3); save(sum4_3, file="Summaries/bench/sum4_3.Rda")
M4_4M <-jags.model(JModel("dunif(0, 1000)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "Bench4_4.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_4<- coda.samples(M4_4M, c('mu', 'tau'), n.iter=75000, thin=5)
sum4_4<- summary(M4_4); save(sum4_4, file="Summaries/bench/sum4_4.Rda")
M4_5M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dgamma(1.0E-3, 1.0E-3)", nrow(data), "Bench4_5.txt"),
                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_5<- coda.samples(M4_5M, c('mu', 'tau'), n.iter=75000, thin=5)
sum4_5<- summary(M4_5); save(sum4_5, file="Summaries/bench/sum4_5.Rda")
M4_6M <-jags.model(JModel("dunif(0, 1000)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "Bench4_6.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_6<- coda.samples(M4_6M, c('mu', 'tau'), n.iter=75000, thin=5)
sum4_6<- summary(M4_6); save(sum4_6, file="Summaries/bench/sum4_6.Rda")
M4_7M <-jags.model(JModel("dnorm(0, 1.0E-4)", "dnorm(0, 1/100^2)  I(0, )", nrow(data), "Bench4_7.txt"),
                   data, n.chains=3, n.adapt=3000, quiet=FALSE)
M4_7<- coda.samples(M4_7M, c('mu', 'tau'), n.iter=75000, thin=5)
sum4_7<- summary(M4_7); save(sum4_7, file="Summaries/bench/sum4_7.Rda")


# Diagnostics
#plot(M4, trace=FALSE)
gelman.diag(M4, confidence=0.95); gelman.plot(M4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4, smooth=TRUE); 
autocorr.diag(M4); autocorr.plot(M4, lagmax=20); acfplot(M4) # auto-correlations

gelman.diag(M4_2, confidence=0.95); gelman.plot(M4_2, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_2, smooth=TRUE); 
autocorr.diag(M4_2); autocorr.plot(M4_2, lagmax=20); acfplot(M4_2) # auto-correlations

gelman.diag(M4_3, confidence=0.95); gelman.plot(M4_3, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_3, smooth=TRUE); 
autocorr.diag(M4_3); autocorr.plot(M4_3, lagmax=20); acfplot(M4_3) # auto-correlations

gelman.diag(M4_4, confidence=0.95); gelman.plot(M4_4, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_4, smooth=TRUE); 
autocorr.diag(M4_4); autocorr.plot(M4_4, lagmax=20); acfplot(M4_4) # auto-correlations

gelman.diag(M4_5, confidence=0.95); gelman.plot(M4_5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_5, smooth=TRUE); 
autocorr.diag(M4_5); autocorr.plot(M4_5, lagmax=20); acfplot(M4_5) # auto-correlations

gelman.diag(M4_6, confidence=0.95); gelman.plot(M4_6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_6, smooth=TRUE); 
autocorr.diag(M4_6); autocorr.plot(M4_6, lagmax=20); acfplot(M4_6) # auto-correlations

gelman.diag(M4_7, confidence=0.95); gelman.plot(M4_7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M4_7, smooth=TRUE); 
autocorr.diag(M4_7); autocorr.plot(M4_7, lagmax=20); acfplot(M4_7) # auto-correlations

#######################
# Alphabetical studies:
#######################

# FFD: 
data<- bench_FFD; data<- subset(data, Language!="Chinese")
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr"); #data<- subset(data, S.sqr!="NA")

M5_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench5.txt"), data, n.chains=3,
               n.adapt=3000, quiet=TRUE)
#M5_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench5_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=50, "tau"=5))
#M5_M <-jags.model(JModel("dnorm(0, 1.0E-3)", "dunif(0, 400)", nrow(data), "Bench5_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M5_M <-jags.model(JModel("dunif(0, 800)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench5_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M5_M <-jags.model(JModel("dnorm(0, 1.0E-3)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench5_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M5<- coda.samples(M5_M, c('mu', 'tau'), n.iter=75000, thin=5)
summary(M5)

# Diagnostics
#plot(M1, trace=FALSE)
gelman.diag(M5, confidence=0.95); gelman.plot(M5, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M5, smooth=TRUE); 
autocorr.diag(M5); autocorr.plot(M5, lagmax=20); acfplot(M5) # auto-correlations

# SFD: 
data<- bench_SFD;  data<- subset(data, Language!="Chinese")
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr"); # data<- subset(data, S.sqr!="NA")

M6_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench6.txt"), data, n.chains=3,
                 n.adapt=3000, quiet=TRUE)
#M6_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench6_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=50, "tau"=5))
#M6_M <-jags.model(JModel("dnorm(0, 1.0E-3)", "dunif(0, 400)", nrow(data), "Bench6_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M6_M <-jags.model(JModel("dunif(0, 800)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench6_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M6_M <-jags.model(JModel("dnorm(0, 1.0E-3)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench6_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M6<- coda.samples(M6_M, c('mu', 'tau'), n.iter=75000, thin=5)
summary(M6)

# Diagnostics
#plot(M1, trace=FALSE)
gelman.diag(M6, confidence=0.95); gelman.plot(M6, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M6, smooth=TRUE); 
autocorr.diag(M6); autocorr.plot(M6, lagmax=20); acfplot(M6) # auto-correlations

# GD: 
data<- bench_GD;  data<- subset(data, Language!="Chinese")
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr");  #data<- subset(data, S.sqr!="NA")

M7_M <-jags.model(JModel("dunif(0, 1000)", "dunif(0, 400)", nrow(data), "Bench7.txt"), data, n.chains=3,
                 n.adapt=3000, quiet=TRUE)
#M7_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench7_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=50, "tau"=5))
#M7_M <-jags.model(JModel("dnorm(0, 1.0E-3)", "dunif(0, 400)", nrow(data), "Bench7_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M7_M <-jags.model(JModel("dunif(0, 800)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench7_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M7_M <-jags.model(JModel("dnorm(0, 1.0E-3)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench7_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M7<- coda.samples(M7_M, c('mu', 'tau'), n.iter=75000, thin=5)
summary(M7)

# Diagnostics
#plot(M3, trace=FALSE)
gelman.diag(M7, confidence=0.95); gelman.plot(M7, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M7, smooth=TRUE); 
autocorr.diag(M7); autocorr.plot(M7, lagmax=20); acfplot(M7) # auto-correlations


# TVT: 
data<- bench_TVT; data<- subset(data, Language!="Chinese")
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr"); #data<- subset(data, S.sqr!="NA")

M8_M <-jags.model(JModel("dunif(0, 1000)", "dunif(0, 400)", nrow(data), "Bench8.txt"), data, n.chains=3,
                 n.adapt=3000, quiet=TRUE)
#M8_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench8_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=50, "tau"=5))
#M8_M <-jags.model(JModel("dnorm(0, 1.0E-5)", "dunif(0, 400)", nrow(data), "Bench8_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M8_M <-jags.model(JModel("dunif(0, 800)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench8_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M8_M <-jags.model(JModel("dnorm(0, 1.0E-5)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench8_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M8<- coda.samples(M8_M, c('mu', 'tau'), n.iter=75000, thin=5)
summary(M8)

# Diagnostics
#plot(M4, trace=FALSE)
gelman.diag(M8, confidence=0.95); gelman.plot(M8, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M8, smooth=TRUE); 
autocorr.diag(M8); autocorr.plot(M8, lagmax=20); acfplot(M8) # auto-correlations




###########################
# Non-alphabetical studies:
###########################

# FFD: 
data<- bench_FFD; data<- subset(data, Language=="Chinese")
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr"); #data<- subset(data, S.sqr!="NA")

M9_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench9.txt"), data, n.chains=3,
                 n.adapt=3000, quiet=TRUE)
#M9_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench9_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=50, "tau"=5))
#M9_M <-jags.model(JModel("dnorm(0, 1.0E-5)", "dunif(0, 400)", nrow(data), "Bench9_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M9_M <-jags.model(JModel("dunif(0, 800)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench9_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M9_M <-jags.model(JModel("dnorm(0, 1.0E-5)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench9_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M9<- coda.samples(M9_M, c('mu', 'tau'), n.iter=75000, thin=5)
summary(M9)

# Diagnostics
#plot(M1, trace=FALSE)
gelman.diag(M9, confidence=0.95); gelman.plot(M9, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M9, smooth=TRUE); 
autocorr.diag(M9); autocorr.plot(M9, lagmax=20); acfplot(M9) # auto-correlations

# SFD: 
data<- bench_SFD;  data<- subset(data, Language=="Chinese")
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr");# data<- subset(data, S.sqr!="NA")

M10_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench10.txt"), data, n.chains=3,
                 n.adapt=3000, quiet=TRUE)
#M10_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench10_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=50, "tau"=5))
#M10_M <-jags.model(JModel("dnorm(0, 1.0E-5)", "dunif(0, 400)", nrow(data), "Bench10_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M10_M <-jags.model(JModel("dunif(0, 800)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench10_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M10_M <-jags.model(JModel("dnorm(0, 1.0E-5)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench10_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M10<- coda.samples(M10_M, c('mu', 'tau'), n.iter=75000, thin=5)
summary(M10)

# Diagnostics
#plot(M1, trace=FALSE)
gelman.diag(M10, confidence=0.95); gelman.plot(M10, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M10, smooth=TRUE); 
autocorr.diag(M10); autocorr.plot(M10, lagmax=20); acfplot(M10) # auto-correlations

# GD: 
data<- bench_GD;  data<- subset(data, Language=="Chinese")
data$ID<- NULL; data$Language<-NULL; data$N<- NULL
colnames(data)<- c("T", "S.sqr");  #data<- subset(data, S.sqr!="NA")

M11_M <-jags.model(JModel("dunif(0, 1000)", "dunif(0, 400)", nrow(data), "Bench11.txt"), data, n.chains=3,
                 n.adapt=3000, quiet=TRUE)
#M11_M <-jags.model(JModel("dunif(0, 800)", "dunif(0, 400)", nrow(data), "Bench11_2.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE,
#                  inits= list("mu"=50, "tau"=5))
#M11_M <-jags.model(JModel("dnorm(0, 1.0E-5)", "dunif(0, 400)", nrow(data), "Bench11_3.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M11_M <-jags.model(JModel("dunif(0, 800)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench11_4.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
#M11_M <-jags.model(JModel("dnorm(0, 1.0E-5)", "dgamma(5.0E-1, 5.0E-3)", nrow(data), "Bench11_5.txt"),
#                  data, n.chains=3, n.adapt=3000, quiet=FALSE)
M11<- coda.samples(M11_M, c('mu', 'tau'), n.iter=75000, thin=5)
summary(M11)

# Diagnostics
#plot(M3, trace=FALSE)
gelman.diag(M11, confidence=0.95); gelman.plot(M11, confidence=0.95) # Gelman and Rubin’s convergence diagnostic
traceplot(M11, smooth=TRUE); 
autocorr.diag(M11); autocorr.plot(M11, lagmax=20); acfplot(M11) # auto-correlations


