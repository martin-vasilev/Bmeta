# Martin Vasilev, 2015

ECDF_plot <- function(JAGS_model1=S1, JAGS_model2=S6, JAGS_model3=S7, type="FFD"){
  
  
  # Generate samples for all models:
  Samples1<-c(JAGS_model1$mu[1,,1],JAGS_model1$mu[1,,2],JAGS_model1$mu[1,,3])
  ECDF1<- ecdf(Samples1);
  
  Samples2<-c(JAGS_model2$mu[1,,1],JAGS_model2$mu[1,,2],JAGS_model2$mu[1,,3])
  ECDF2<- ecdf(Samples2); 
  
  Samples3<-c(JAGS_model3$mu[1,,1],JAGS_model3$mu[1,,2],JAGS_model3$mu[1,,3])
  ECDF3<- ecdf(Samples3);
  
  prob1<- NULL; prob2<- NULL; prob3<- NULL; seq1<- NULL
  
  if(type=="FFD"){
    mu<- seq(0,8,0.001)
    seq1<- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
  } else {
    mu<- seq(0,12,0.001)
    seq1<- c(0, 1, 2, 3, 4, 5, 6, 7,8, 9, 10, 11, 12)
  }

  
  for (i in 1:length(mu)){
  prob1[i]<- 1- (ECDF1(mu[i]))
  prob2[i]<- 1- (ECDF2(mu[i]))
  prob3[i]<- 1- (ECDF3(mu[i]))
  }
  mu<- c(mu, mu, mu)
  prob<- c(prob1, prob2, prob3)
  var= c(rep(paste(expression(mu), "~ Unif(-30, 30)", sep=""), length(mu)/3),
         rep(paste(expression(mu), "~ N(0, 0.5)", sep=""), length(mu)/3),
         rep(paste(expression(mu), "~ N(7, 0.5)", sep=""), length(mu)/3))
  
  DB<- data.frame(mu, prob, var)
 # colnames(DB)<- c("mu, prob", "Prior on MU")
  
  # Create graph:
  library(ggplot2)
  
  y<- expression(paste("P(", mu, " > X)"))
  #
  Plot <-ggplot(DB[1:8001,], aes(prob, mu, colour=prob)) + geom_line(size=10)+ theme_bw() +
          theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
          ylab(y) + xlab("X (in ms)") + 
          scale_x_continuous(breaks=seq1) +
          scale_y_continuous(breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), 
          labels=c("0",".1",".2",".3",".4",".5", '.6', '.7','.8', '.9', '1')) + 
          theme(axis.title.x = element_text(size=16),axis.title.y = element_text(size=16),
          axis.title.y = element_text(size=16), axis.text=element_text(size=16)); Plot
  Plot<- Plot + geom_line(aes(y = prob2, colour = "firebrick1"),size=1.06) + 
                geom_line(aes(y = prob3, colour = "lightseagreen"),size=1.06) +
                theme(legend.position="top")
  if(type=="FFD"){
    Plot<- Plot + geom_text(aes(7, .92, label="FFD"))
  } else{
    Plot<- Plot + geom_text(aes(11, .92, label="GD"))
  }
  
  return(Plot)
}