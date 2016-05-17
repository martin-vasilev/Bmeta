
# Script for N+1 preview probabilities graph

# Martin R. Vasilev, 2016

# To create the graph, first run the N+1 Measures for all studies from "N1_Bmeta_final.R"


#-------------
# ECDF plot:  
#-------------

# Create ECDFs:

S1<-jags.samples(M40_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000) # FFD
S1<-c(S1$mu[1,,1],S1$mu[1,,2],S1$mu[1,,3])
ECDF1<- ecdf(S1);

S2<-jags.samples(M41_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000) # SFD
S2<-c(S2$mu[1,,1],S2$mu[1,,2],S2$mu[1,,3])
ECDF2<- ecdf(S2);

S3<-jags.samples(M42_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000) # GD
S3<-c(S3$mu[1,,1],S3$mu[1,,2],S3$mu[1,,3])
ECDF3<- ecdf(S3);

S4<-jags.samples(M43_M, variable.names='mu', n.iter=75000, thin=5, n.adapt=3000) # TVT
S4<-c(S4$mu[1,,1],S4$mu[1,,2],S4$mu[1,,3])
ECDF4<- ecdf(S4);

# FFD:
prob1<- NULL; prob2<- NULL; prob3<- NULL; prob3<- NULL; prob4<- NULL; seq1<- NULL

mu<- seq(20,70,0.001) # create sequence of ESs
seq1<- seq(20, 70, 5)

for (i in 1:length(mu)){
  prob1[i]<- 1- (ECDF1(mu[i])) # FFD
  prob2[i]<- 1- (ECDF2(mu[i])) # SFD
  prob3[i]<- 1- (ECDF3(mu[i])) # GD
  prob4[i]<- 1- (ECDF4(mu[i])) # TVT
}

mu<- c(mu, mu, mu, mu)
prob<- c(prob1, prob2, prob3, prob4)
Measure= c(rep(paste("FFD", sep=""), length(mu)/4),
         rep(paste("SFD", sep=""), length(mu)/4),
         rep(paste("GD", sep=""), length(mu)/4),
         rep(paste("TVT", sep=""), length(mu)/4)
         )

DB<- data.frame(mu, prob, Measure)
DB$Measure <- factor(DB$Measure, levels = c("FFD", "SFD",
                                        "GD", "TVT"))

# Create graph:
library(ggplot2)

y<- expression(paste("P(", mu, " > X | Data)"))
#
Plot <-ggplot(DB, aes(mu, prob, colour=Measure)) + geom_line(size=2)+ theme_bw() +
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

# Save Image:
png(file = 'Plots/N1prob.png', width = 800, height = 600, units = "px")

Plot

dev.off()  


