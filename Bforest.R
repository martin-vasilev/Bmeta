

# Generates a forest plot for N+1 preview effects on word n+1

# Martin R. Vasilev, 2016

png(file = 'Plots/N1forest_FFD.png', width = 1600, height = 1000, units = "px")

source("Plots/functions/get_margs.R")
source("Plots/functions/et_al.R")

############
#variables #
############
load("Data/data5.Rda")
load("Summaries/N1/sum40.Rda")
data<- data5
data$prec<- 1/data$S.sqr
data$weight<- NULL

for(i in 1:nrow(data5)){
  data$weight[i]<- data$prec[i]/sum(data$prec)
}
data$weight<- data$weight*100
  
data$Paper <- sapply(data$Paper, as.character)
studies<- NULL

for(i in 1:nrow(data)){
  studies[i]<- et_al(data$Paper[i])
}

# observed:
obsM<- data$T
CI_L<- 1.96*sqrt(data$S.sqr); CI_L<- obsM- CI_L
CI_R<- 1.96*sqrt(data$S.sqr); CI_R<- obsM+ CI_R

# posterior:
pM<- unname(sum40$statistics[3:nrow(sum40$statistics),1])
CrI_L<- unname(sum40$quantiles[3:nrow(sum40$statistics),1])
CrI_R<- unname(sum40$quantiles[3:nrow(sum40$statistics),5])

rng<- c(CI_L, CI_R)
maxX<- 1600
minY<- min(rng)
maxY<- max(rng)
margs<- get_margs(minY, maxY)

sPlot<- minY-5
start<- maxY*(1/100) # where to start plotting
step<- 22 # step between studies

df<- data.frame()
  y<- 1:10
  
#  windows()
  par(mfrow=c(2,1), mai= c(0.1,0.1,0.2,0.5))
  
  
  
  plot (y, type="l", col="white", lty="solid", lwd=2
        , ylim=c(sPlot, maxY), xlim= c(0, maxX)
        , axes=F, xaxs="i", yaxs="i", cex.lab=1.5) #
  
  axis(side=4, at=c(margs[1], margs[2], margs[3], margs[4], margs[5], margs[6], margs[7], margs[8], margs[9]), 
       labels=c(toString(margs[1]), toString(margs[2]), toString(margs[3]), toString(margs[4]), toString(margs[5]),
                toString(margs[6]), toString(margs[7]), toString(margs[8]), toString(margs[9]) ), 
       tick=T, cex.axis=1.2)
  
  mtext("Label", side=4)  
  
# Plot 95% CIs
for (i in 1:length(studies)){
  segments(x0=start+step*i- step/4, y0=CI_L[i], x1=start+step*i- step/4, y1=CI_R[i], lwd=1.2, lty=2, col="black")
  segments(x0=start+step*i-2*(step/4), y0=CI_L[i], x1=start+step*i-step/4+step/4, y1=CI_L[i], lwd=1.2, lty=1, col="black")
  segments(x0=start+step*i-2*(step/4), y0=CI_R[i], x1=start+step*i-step/4+step/4, y1=CI_R[i], lwd=1.2, lty=1, col="black")
}    
  
  
# Plot observed means
for (i in 1:length(studies)){
  points(x= start+step*i- step/4, y=obsM[i], pch = 15, cex=1.2, col="white")
  points(x= start+step*i- step/4, y=obsM[i], pch = 0, cex=1.2, col="black")
}# (data$weight[i]/max(data$weight))
  
  # arithmetic mean:
#  abline(h=mean(obsM), lwd=1.2, lty=2, col="darkorchid")

# Plot posterior means
for (i in 1:length(studies)){
  points(x= start+step*i +step/4, y=pM[i], pch = 15, cex=1.2, col="darkred")
} #(data$weight[i]/max(data$weight))

 # Plot 95% CrIs
for (i in 1:length(studies)){
  segments(x0=start+step*i+step/2 - step/4, y0=CrI_L[i], x1=start+step*i +step/2 - step/4, y1=CrI_R[i], lwd=1.2, lty=1, col="darkred")
  segments(x0=start+step*i-2*(step/4)+step/2, y0=CrI_L[i], x1=start+step*i-step/4+step/4+step/2, y1=CrI_L[i], lwd=1.2, lty=1, col="darkred")
  segments(x0=start+step*i-2*(step/4)+step/2, y0=CrI_R[i], x1=start+step*i-step/4+step/4+step/2, y1=CrI_R[i], lwd=1.2, lty=1, col="darkred")
}    
  
  # posterior mean:
#  abline(h=sum40$statistics[1,1], lwd=1.2, lty=1, col="darkred")
  
# pooled mean:
  points(x= (96/100)*maxX, y=sum40$statistics[1,1], pch = 18, cex=2, col="darkred")
  segments(x0=(96/100)*maxX, y0=sum40$quantiles[1,1], x1=(96/100)*maxX, y1=sum40$quantiles[1,5], lwd=1.2, lty=1, col="darkred")
  segments(x0=(96/100)*maxX-5, y0=sum40$quantiles[1,5], x1=(96/100)*maxX+5, y1=sum40$quantiles[1,5], lwd=1.2, lty=1, col="darkred")
  segments(x0=(96/100)*maxX-5, y0=sum40$quantiles[1,1], x1=(96/100)*maxX+5, y1=sum40$quantiles[1,1], lwd=1.2, lty=1, col="darkred")
  
  ###############3
  # Plot Annotation
  
  plot (y, type="l", col="white", lty="solid", lwd=2, ylim=c(0, 60),
        xlim= c(0, maxX), axes=F, xaxs="i", yaxs="i", cex.lab=1.5)#, ann=FALSE
  abline(h=60)
  
  # Plot study label  
  for (i in 1:length(studies)){
    text(x=start+step*i, y=33, studies[i], cex=1.2, font=2, srt = 90, adj=0)
  }

  
  
  dev.off() 
  
  
