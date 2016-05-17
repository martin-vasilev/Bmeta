
#  Script for analysing the data for publication bias
# Martin Vasilev, 2016

rm(list=ls())


library(ggplot2)
library(meta)

# Load N2 data:
load("Data/dataN2.Rda")
load("Data/ES_N2.Rda")
load("Data/data1.Rda")
load("Data/data2.Rda")
load("Data/data3.Rda")
load("Data/data4.Rda")

#---------------------------------------------------------------------------------------
#                                      N+2 data
#---------------------------------------------------------------------------------------


#Plot effect sizes against sample size
ES_N2$N<- dataN2$N

# FFD:
P1 <- ggplot(ES_N2, aes(x=FFD_ms, y=N)) + geom_point(size=3) +
        theme_bw() + theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
        ylab('Sample size')+ xlab('Effect size (in ms)')+
        ggtitle("FFD (all studies)"); P1

# GD:
P2 <- ggplot(ES_N2, aes(x=Gaze_ms, y=N)) + geom_point(size=3) +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("GD (all studies)"); P2


# FFD(alphabetical studies):
P3 <- ggplot(ES_N2[-c(5,6,7,10),], aes(x=FFD_ms, y=N)) + geom_point(size=3) +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("FFD (alphabetical studies)"); P3

# GD (alphabetical studies):
P4 <- ggplot(ES_N2[-c(5,6,7,10),], aes(x=Gaze_ms, y=N)) + geom_point(size=3) +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("GD (alphabetical studies)"); P4


library(gridExtra)
library(grid)
png(file = 'Plots/N2_ES_x_N.png', width = 800, height = 600, units = "px")

grid.arrange(P1, P2, P3, P4, 
             top= textGrob('Effect size as a function of sample size: N+2 preview effects',
                           gp=gpar(cex=2), just="top"))
dev.off()

## OVERALL, there is no substantial evidence to suggest studies with larger effect sizes
# also had larger effect sizes




# Funnel plots:
png(file = 'Plots/N2_funnel.png', width = 850, height = 300, units = "px")
attach(mtcars)
par(mfrow=c(1,4))

funnel(data1$T, data1$S.sqr, xlab="Mean difference",main="N+2 FFD (all)",col="black", 
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(data2$T, data2$S.sqr, xlab="Mean difference",main="N+2 GD (all)",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(data1a$T, data1a$S.sqr, xlab="Mean difference",main="N+2 FFD (alphabetical)",col="black", 
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(data2a$T, data2a$S.sqr, xlab="Mean difference",main="N+2 GD (alphabetical)",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

#funnel(data3$T, data3$S.sqr, xlab="Mean difference",main="Funnel plot: N+1 (FFD)",col="black", 
#       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95)
#
#funnel(data4$T, data4$S.sqr, xlab="Mean difference",main="Funnel plot: N+1 (GD)",col="black",
#       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95)
dev.off()


trimfill(data1$T, data1$S.sqr)
trimfill(data2$T, data2$S.sqr)


#---------------------------------------------------------------------------------------
#                                      N+1 data
#---------------------------------------------------------------------------------------

# I analyse only FFD data


#Plot effect sizes against sample size
library(ggplot2)

# Random mask:
load("Data/RAN_FFD.Rda"); 
RAN_FFD$S.sqr<- sqrt(RAN_FFD$S.sqr) # to get SE needed for the funnel plot
load("Data/RAN_GD.Rda"); 
RAN_GD$S.sqr<- sqrt(RAN_GD$S.sqr) # to get SE needed for the funnel plot

P5 <- ggplot(RAN_FFD, aes(x=T, y=N)) + geom_point(size=3, colour="#828282") +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("RAN mask (all studies)"); P5

# Unrelated word mask:
load("Data/UNREL_FFD.Rda")
UNREL_FFD$S.sqr<- sqrt(UNREL_FFD$S.sqr) # to get SE needed for the funnel plot
load("Data/UNREL_GD.Rda")
UNREL_GD$S.sqr<- sqrt(UNREL_GD$S.sqr) # to get SE needed for the funnel plot

P6 <- ggplot(UNREL_FFD, aes(x=T, y=N)) + geom_point(size=3, colour="#828282") +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("UNREL mask (all studies)"); P6

# X mask:
load("Data/X_FFD.Rda")
X_FFD$S.sqr<- sqrt(X_FFD$S.sqr) # to get SE needed for the funnel plot
load("Data/X_GD.Rda")
X_GD$S.sqr<- sqrt(X_GD$S.sqr) # to get SE needed for the funnel plot

P7 <- ggplot(X_FFD, aes(x=T, y=N)) + geom_point(size=3, colour="#828282") +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("X mask (all studies)"); P7

# Pseudo word mask:
load("Data/PSEUD_FFD.Rda")
PSEUD_FFD$S.sqr<- sqrt(PSEUD_FFD$S.sqr) # to get SE needed for the funnel plot
load("Data/PSEUD_GD.Rda")
PSEUD_GD$S.sqr<- sqrt(PSEUD_GD$S.sqr) # to get SE needed for the funnel plot

P8 <- ggplot(PSEUD_FFD, aes(x=T, y=N)) + geom_point(size=3, colour="#828282") +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("PSEUD mask (all studies)"); P8


# Orthographical mask:
load("Data/ORTH_FFD.Rda")
ORTH_FFD$S.sqr<- sqrt(ORTH_FFD$S.sqr) # to get SE needed for the funnel plot
load("Data/ORTH_GD.Rda")
ORTH_GD$S.sqr<- sqrt(ORTH_GD$S.sqr) # to get SE needed for the funnel plot

P9 <- ggplot(ORTH_FFD, aes(x=T, y=N)) + geom_point(size=3, colour="#828282") +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("ORTH mask (all studies)"); P9


# Semantic mask:
load("Data/SEM_FFD.Rda")
SEM_FFD$S.sqr<- sqrt(SEM_FFD$S.sqr) # to get SE needed for the funnel plot
load("Data/SEM_GD.Rda")
SEM_GD$S.sqr<- sqrt(SEM_GD$S.sqr) # to get SE needed for the funnel plot

P10 <- ggplot(SEM_FFD, aes(x=T, y=N)) + geom_point(size=3, colour="#828282") +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("SEM mask (all studies)"); P10


# Phonological mask:
load("Data/PHON_FFD.Rda")
PHON_FFD$S.sqr<- sqrt(PHON_FFD$S.sqr) # to get SE needed for the funnel plot
load("Data/PHON_GD.Rda")
PHON_GD$S.sqr<- sqrt(PHON_GD$S.sqr) # to get SE needed for the funnel plot

P11 <- ggplot(PHON_FFD, aes(x=T, y=N)) + geom_point(size=3, colour="#828282") +
  theme_bw() + theme(panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18), axis.title.y = element_text(size=18),
        axis.text=element_text(size=16), plot.title = element_text(size=18))+
  ylab('Sample size')+ xlab('Effect size (in ms)')+
  ggtitle("PHON mask (all studies)"); P11

library(gridExtra)
library(grid)
png(file = 'Plots/N1_ES_x_N.png', width = 800, height = 600, units = "px")

grid.arrange(P5, P6, P7, P8, P9, P10, P11, 
             top= textGrob('Effect size as a function of sample size: N+1 preview effects [FFD]',
                           gp=gpar(cex=2), just="top"))
dev.off()



# Funnel plots:
png(file = 'Plots/N1_funnel_FFD.png', width = 850, height = 600, units = "px")
attach(mtcars)
par(mfrow=c(2,3))
library(meta)
# RAN:
funnel(RAN_FFD$T, RAN_FFD$S.sqr, xlab="Mean difference",main="N+1: RAN mask",col="black", 
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.9, cex.axis=1.7, cex.main=1.7, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(UNREL_FFD$T, UNREL_FFD$S.sqr, xlab="Mean difference",main="N+1: UNREL mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.9, cex.axis=1.7, cex.main=1.7, cex=2)#,contour=0.95, col.contour="lightgray")

#funnel(X_FFD$T, X_FFD$S.sqr, xlab="Mean difference",main="X mask",col="black", 
#       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
#       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2,contour=0.95, col.contour="lightgray")

funnel(PSEUD_FFD$T, PSEUD_FFD$S.sqr, xlab="Mean difference",main="N+1: PSEUD mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.9, cex.axis=1.7, cex.main=1.7, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(ORTH_FFD$T, ORTH_FFD$S.sqr, xlab="Mean difference",main="N+1: ORTH mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.9, cex.axis=1.7, cex.main=1.7, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(SEM_FFD$T, SEM_FFD$S.sqr, xlab="Mean difference",main="N+1: SEM mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.9, cex.axis=1.7, cex.main=1.7, cex=2) #,contour=0.95, col.contour="lightgray")

funnel(PHON_FFD$T, PHON_FFD$S.sqr, xlab="Mean difference",main="N+1: PHON mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.9, cex.axis=1.7, cex.main=1.7, cex=2)#,contour=0.95, col.contour="lightgray")
dev.off()


# Funnel plots:
png(file = 'Plots/N1_funnel_GD.png', width = 850, height = 600, units = "px")
attach(mtcars)
par(mfrow=c(2,3))
library(meta)
# GD:
funnel(RAN_GD$T, RAN_GD$S.sqr, xlab="Mean difference",main="RAN mask",col="black", 
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(UNREL_GD$T, UNREL_GD$S.sqr, xlab="Mean difference",main="UNREL mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

#funnel(X_FFD$T, X_FFD$S.sqr, xlab="Mean difference",main="X mask",col="black", 
#       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
#       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2,contour=0.95, col.contour="lightgray")

funnel(PSEUD_GD$T, PSEUD_GD$S.sqr, xlab="Mean difference",main="PSEUD mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(ORTH_GD$T, ORTH_GD$S.sqr, xlab="Mean difference",main="ORTH mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(SEM_GD$T, SEM_GD$S.sqr, xlab="Mean difference",main="SEM mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(PHON_GD$T, PHON_GD$S.sqr, xlab="Mean difference",main="PHON mask",col="black",
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")
dev.off()



# "Ultimate" N+1 preview effect:

png(file = 'Plots/N1_Prev_funnel_FFD.png', width = 850, height = 300, units = "px")
attach(mtcars)
par(mfrow=c(1,4))
library(meta)

load("Data/data5.Rda")
data5$S.sqr<- sqrt(data5$S.sqr)
load("Data/data6.Rda")
data6$S.sqr<- sqrt(data6$S.sqr)
data5a<- subset(data5, Language!="Chinese")
data6a<- subset(data6, Language!="Chinese")


funnel(data5$T, data5$S.sqr, xlab="Mean difference",main="N+1 FFD (all)",col="black", 
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(data5a$T, data5a$S.sqr,
       xlab="Mean difference",main="N+1 FFD (alphabetical)",col="black", 
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(data6$T, data6$S.sqr, xlab="Mean difference",main="N+1 GD (all)",col="black", 
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

funnel(data6a$T, data6a$S.sqr,
       xlab="Mean difference",main="N+1 GD (alphabetical)",col="black", 
       bg="darkgray", col.random="black",comb.random=TRUE, yaxis="invse",sm="MD",level=0.95, 
       cex.lab=1.7, cex.axis=1.5, cex.main=1.5, cex=2)#,contour=0.95, col.contour="lightgray")

dev.off()

