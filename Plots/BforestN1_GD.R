

# Generates a forest plot for N+1 preview effects on word n+1

# Martin R. Vasilev, 2016

png(file = 'Plots/N1forest_GD.png', width = 1800, height = 1000, units = "px")

source("Plots/functions/get_margs.R")
source("Plots/functions/et_al.R")
source("Plots/functions/get_year.R")

############
#variables #
############
load("Data/data6.Rda")
load("Summaries/N1/sum42.Rda")
data<- data6
data$prec<- 1/data$S.sqr
weight<- NULL

for(i in 1:nrow(data)){
  weight[i]<- data$prec[i]/sum(data$prec)
}
weight<- weight*100
weight<- weight/(max(weight)) # so that the study with the biggest weight lead to 2x magnification
  
data$Paper <- sapply(data$Paper, as.character)
studies<- NULL; year<- NULL

for(i in 1:nrow(data)){
  studies[i]<- et_al(data$Paper[i])
  year[i]<- get_year(data$Paper[i])
}
year[44]<- "0"
studies[34]<- "Yang et al. (2012a)"; studies[31]<- "Yang et al. (2012b)"
studies[32]<- "Yang et al. (2012b), Exp.2"
studies[30]<- "Hyönä et al. (2005)"
studies[23]<- "Rayner et al. (2014b)"
studies[27]<- "Rayner et al. (2014a)"
studies[7]<- "Angele et al. (2013a)"
studies[9]<- "Angele et al. (2013b), Exp.2"
studies[63]<- "Rayner et al. (2007), Exp.2"
studies[2]<- "Veldre et al. (2015a)"; studies[51]<- "Veldre et al. (2015b)"
studies[65]<- "Angele et al. (2013c)"; studies[52]<- "Angele et al. (2013c), Exp.2";
studies[37]<- "Pollatsek et al. (1992), Exp.2" 
# observed:
obsM<- data$T
CI_L<- 1.96*sqrt(data$S.sqr); CI_L<- obsM- CI_L
CI_R<- 1.96*sqrt(data$S.sqr); CI_R<- obsM+ CI_R

# posterior:
pM<- unname(sum42$statistics[3:nrow(sum42$statistics),1])
CrI_L<- unname(sum42$quantiles[3:nrow(sum42$statistics),1])
CrI_R<- unname(sum42$quantiles[3:nrow(sum42$statistics),5])

rng<- c(CI_L, CI_R)
maxX<- 1660
minY<- min(rng)
maxY<- max(rng)
margs<- get_margs(minY, maxY)
#margs[11]<-140

sPlot<- minY-10
start<- maxY*(1/100) # where to start plotting
step<- 22 # step between studies

df<- data.frame(studies, year, obsM, pM, CI_L, CI_R, CrI_L, CrI_R, weight)
df<- df[order(year, studies, decreasing = T),]

  y<- 1:10
  
#  windows()
  par(mfrow=c(2,1), mai= c(0,0.1,0,0.85))
  
  
  
  plot (y, type="l", col="white", lty="solid", lwd=2
        , ylim=c(sPlot, maxY+35), xlim= c(0, maxX)
        , axes=F, xaxs="i", yaxs="i", cex.lab=1.5) #
  
  axis(side=4, at=c(-80 ,margs[1], margs[2], margs[3], margs[4], margs[5], margs[6], margs[7], margs[8],
                    margs[9], margs[10], margs[11], 160), 
       labels=c("", toString(margs[1]), toString(margs[2]), toString(margs[3]), toString(margs[4]), toString(margs[5]),
                toString(margs[6]), toString(margs[7]), toString(margs[8]), toString(margs[9]), 
                toString(margs[10]), toString(margs[11]), "" ), 
       tick=T, cex.axis=1.4)
  
  mtext("Effect size (in ms)", side=4, line= 3, cex=1.5, font=2)  
  
  # plot y grid
  for (i in 1:nrow(df)){
    segments(x0=start+step*i, y0=-70, x1=start+step*i, y1=maxY+18, lwd=1.2, lty=1, col="#E3E3E3")
  }
  
# Plot 95% CIs
for (i in 1:nrow(df)){
  segments(x0=start+step*i- step/4, y0=df$CI_L[i], x1=start+step*i- step/4, y1=df$CI_R[i], lwd=1.2, lty=2, col="black")
  segments(x0=start+step*i-2*(step/4), y0=df$CI_L[i], x1=start+step*i-step/4+step/4, y1=df$CI_L[i], lwd=1.2, lty=1, col="black")
  segments(x0=start+step*i-2*(step/4), y0=df$CI_R[i], x1=start+step*i-step/4+step/4, y1=df$CI_R[i], lwd=1.2, lty=1, col="black")
}    
  
  
# Plot observed means
for (i in 1:nrow(df)){
  points(x= start+step*i- step/4, y=df$obsM[i], pch = 15, cex=1+2*df$weight[i], col="white")
  points(x= start+step*i- step/4, y=df$obsM[i], pch = 0, cex=1+2*df$weight[i], col="black")
}# (data$weight[i]/max(data$weight))
  
  # arithmetic mean:
#  abline(h=mean(obsM), lwd=1.2, lty=2, col="darkorchid")

# Plot posterior means
for (i in 1:nrow(df)){
  points(x= start+step*i +step/4, y=df$pM[i], pch = 15, cex=1+2*df$weight[i], col="darkred")
} #(data$weight[i]/max(data$weight))

 # Plot 95% CrIs
for (i in 1:nrow(df)){
  segments(x0=start+step*i+step/2 - step/4, y0=df$CrI_L[i], x1=start+step*i +step/2 - step/4, y1=df$CrI_R[i], lwd=1.2, lty=1, col="darkred")
  segments(x0=start+step*i-2*(step/4)+step/2, y0=df$CrI_L[i], x1=start+step*i-step/4+step/4+step/2, y1=df$CrI_L[i], lwd=1.2, lty=1, col="darkred")
  segments(x0=start+step*i-2*(step/4)+step/2, y0=df$CrI_R[i], x1=start+step*i-step/4+step/4+step/2, y1=df$CrI_R[i], lwd=1.2, lty=1, col="darkred")
}    
  
  # posterior mean:
#  abline(h=sum42$statistics[1,1], lwd=1.2, lty=2, col="darkred")
  
# pooled mean:
  points(x= (97.3/100)*maxX, y=sum42$statistics[1,1], pch = 18, cex=2.2, col="darkred")
  segments(x0=(97.3/100)*maxX, y0=sum42$quantiles[1,1], x1=(97.3/100)*maxX, y1=sum42$quantiles[1,5], lwd=1.2, lty=1, col="darkred")
  segments(x0=(97.3/100)*maxX-5, y0=sum42$quantiles[1,5], x1=(97.3/100)*maxX+5, y1=sum42$quantiles[1,5], lwd=1.2, lty=1, col="darkred")
  segments(x0=(97.3/100)*maxX-5, y0=sum42$quantiles[1,1], x1=(97.3/100)*maxX+5, y1=sum42$quantiles[1,1], lwd=1.2, lty=1, col="darkred")
  
  abline(h=155)

 #  legend:
  rect(xleft = (98.7/100)*maxX, ybottom = 64, xright = (95.7/100)*maxX, ytop = 150, lwd=1.2, lty = "solid", col="white") 
 #  observed:
  segments(x0=(96.5/100)*maxX, y0=67, x1=(96.5/100)*maxX, y1=79, lwd=1.2, lty=2, col="black")
  points(x= (96.5/100)*maxX, y=72.4, pch = 15, cex=1.3, col="white")
  points(x= (96.5/100)*maxX, y=72.4, pch = 0, cex=1.3, col="black")

 # posterior
  points(x= (97.8/100)*maxX, y=72.4, pch = 15, cex=1.3, col="darkred")
  segments(x0=(97.8/100)*maxX, y0=67, x1=(97.8/100)*maxX, y1=79, lwd=1.2, lty=1, col="darkred")
  
  # text:
  text(x=(97.8/100)*maxX, y=83, "Posterior (95% CrIs)", cex=1.2, font=2, srt = 90, adj=0)
  text(x=(96.5/100)*maxX, y=83, "Observed (95% CIs)", cex=1.2, font=2, srt = 90, adj=0)
  
 
  ###############3
  # Plot Annotation
  
  plot (y, type="l", col="white", lty="solid", lwd=2, ylim=c(0, 60),
        xlim= c(0, maxX), axes=F, xaxs="i", yaxs="i", cex.lab=1.5)#, ann=FALSE
  abline(h=60.1)
  
  # Plot study label  
  for (i in 1:nrow(df)){
    text(x=start+step*i, y=58, df$studies[i], cex=1.3, font=2, srt = 90, adj=1)
    segments(x0=start+step*i, y0=59, x1=start+step*i, y1=60, lwd=1.2, lty=1, col="black")
  }

  
  text(x=(97.3/100)*maxX, y=58, "Posterior mean", cex=1.5, font=2, srt = 90, adj=1)
  
  
  mtext("Studies", side=1, line= -17, cex=1.5, font=2)
  
  dev.off() 
  
  
