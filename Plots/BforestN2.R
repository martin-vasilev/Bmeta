

# Generates a forest plot for N+1 preview effects on word n+1

# Martin R. Vasilev, 2016
rm(list=ls())
png(file = 'Plots/N2forest2_FFD.png', width = 1400, height = 1200, units = "px")

source("Plots/functions/get_margs.R")


############
#variables #
############
load("Data/data1.Rda") # FFD, all studies
#load("Data/data2.Rda")

load("Summaries/N2/sum1.Rda") # FFD, all studies
load("Summaries/N2/sum2.Rda") # FFD, alphabetical studies only 

data<- data1
data$Paper<- c("Rayner et al. (2007), Exp.1 *", "Rayner et al. (2007), Exp.2 *",
               "Kliegl et al. (2007) *", "Angele et al. (2008) *",
               "Yang et al. (2009), Exp.1", "Yang et al. (2009), Exp. 2",
               "Yan et al. (2010)", "Angele & Rayner (2011) *",
               "Risse & Kliegl (2011) *", "Yang et al. (2012)",
               "Radach et al. (2013) *")


data$prec<- 1/data$S.sqr
weight<- NULL

for(i in 1:nrow(data)){
  weight[i]<- data$prec[i]/sum(data$prec)
}
weight<- weight*100
weight<- weight/(max(weight)) # so that the study with the biggest weight lead to 2x magnification

studies<- data$Paper


# observed:
obsM<- data$T
CI_L<- 1.96*sqrt(data$S.sqr); CI_L<- obsM- CI_L
CI_R<- 1.96*sqrt(data$S.sqr); CI_R<- obsM+ CI_R

# posterior:
pM<- unname(sum1$statistics[3:nrow(sum1$statistics),1])
CrI_L<- unname(sum1$quantiles[3:nrow(sum1$statistics),1])
CrI_R<- unname(sum1$quantiles[3:nrow(sum1$statistics),5])

rng<- c(CI_L, CI_R)
maxX<- 300
minY<- min(rng)
maxY<- max(rng)
margs<-c(-40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60)

sPlot<- minY-5
start<- maxY*(1/100) # where to start plotting
step<- 22 # step between studies

df<- data.frame(studies, obsM, pM, CI_L, CI_R, CrI_L, CrI_R, weight)
df<- df[11:1,]
df$N1len<- c("3 let.", "1 char.", "3 let.", "3 let.", "1 char.",
             "2 char.", "1 char.", "6 let.", "3 let.", "3.8 let.", "5.3 let.")

  y<- 1:10
  
#  windows()
  par(mfrow=c(2,1), mai= c(0,0.1,0,0.85))
  
  
  
  plot (y, type="l", col="white", lty="solid", lwd=2
        , ylim=c(sPlot, maxY+35), xlim= c(0, maxX)
        , axes=F, xaxs="i", yaxs="i", cex.lab=1.7) #
  
  axis(side=4, at=c(margs[1], margs[1]+5, margs[2], margs[2]+5, margs[3], margs[3]+5, margs[4], margs[4]+5,
                    margs[5], margs[5]+5, margs[6], margs[6]+5, margs[7], margs[7]+5, margs[8], margs[8]+5,
                    margs[9], margs[9]+5, margs[10], margs[10]+5, margs[11]), 
       labels=c(toString(margs[1]), "", toString(margs[2]), "", toString(margs[3]), "", toString(margs[4]), "",
                toString(margs[5]), "",toString(margs[6]), "", toString(margs[7]), "", toString(margs[8]), "",
                toString(margs[9]), "", toString(margs[10]), "", toString(margs[11])), 
       tick=T, cex.axis=1.5)
  
  mtext("Effect size (in ms)      ", side=4, line= 3, cex=1.8, font=2)  
  
  # plot y grid
  for (i in 1:nrow(df)){
    segments(x0=start+step*i, y0=-70, x1=start+step*i, y1=maxY+35, lwd=1.8, lty=1, col="#E3E3E3")
  }
  
  abline(h=0)
  
# Plot 95% CIs
for (i in 1:nrow(df)){
  segments(x0=start+step*i- step/6, y0=df$CI_L[i], x1=start+step*i- step/6, y1=df$CI_R[i], lwd=1.8, lty=2, col="black")
  segments(x0=start+step*i-2*(step/6), y0=df$CI_L[i], x1=start+step*i-step/6+step/6, y1=df$CI_L[i], lwd=1.8, lty=1, col="black")
  segments(x0=start+step*i-2*(step/6), y0=df$CI_R[i], x1=start+step*i-step/6+step/6, y1=df$CI_R[i], lwd=1.8, lty=1, col="black")
}    
  
  
# Plot observed means
for (i in 1:nrow(df)){
  points(x= start+step*i- step/6, y=df$obsM[i], pch = 15, cex=1+3*df$weight[i], col="white")
  points(x= start+step*i- step/6, y=df$obsM[i], pch = 0, cex=1+3*df$weight[i], col="black")
}# (data$weight[i]/max(data$weight))
  
  # arithmetic mean:
#  abline(h=mean(obsM), lwd=1.2, lty=2, col="darkorchid")

# Plot posterior means
for (i in 1:nrow(df)){
  points(x= start+step*i +step/6, y=df$pM[i], pch = 15, cex=1+3*df$weight[i], col="darkred")
} #(data$weight[i]/max(data$weight))

 # Plot 95% CrIs
for (i in 1:nrow(df)){
  segments(x0=start+step*i+step/6, y0=df$CrI_L[i], x1=start+step*i +step/6 , y1=df$CrI_R[i], lwd=1.8, lty=1, col="darkred")
  segments(x0=start+step*i+step/6-(step/6), y0=df$CrI_L[i], x1=start+step*i+2*(step/6), y1=df$CrI_L[i], lwd=1.8, lty=1, col="darkred")
  segments(x0=start+step*i+step/6-(step/6), y0=df$CrI_R[i], x1=start+step*i+2*(step/6), y1=df$CrI_R[i], lwd=1.8, lty=1, col="darkred")
}    
  
  # posterior mean:
#  abline(h=sum1$statistics[1,1], lwd=1.2, lty=1, col="darkred")
  
# pooled mean (all studies):
  points(x= (97.3/100)*maxX, y=sum1$statistics[1,1], pch = 18, cex=3, col="darkred")
  segments(x0=(97.3/100)*maxX, y0=sum1$quantiles[1,1], x1=(97.3/100)*maxX, y1=sum1$quantiles[1,5], lwd=1.8, lty=1, col="darkred")
  segments(x0=(97.3/100)*maxX-5, y0=sum1$quantiles[1,5], x1=(97.3/100)*maxX+5, y1=sum1$quantiles[1,5], lwd=1.8, lty=1, col="darkred")
  segments(x0=(97.3/100)*maxX-5, y0=sum1$quantiles[1,1], x1=(97.3/100)*maxX+5, y1=sum1$quantiles[1,1], lwd=1.8, lty=1, col="darkred")
  
  # pooled mean (alphabetical studies):
  points(x= (91.3/100)*maxX, y=sum2$statistics[1,1], pch = 18, cex=3, col="darkred")
  segments(x0=(91.3/100)*maxX, y0=sum2$quantiles[1,1], x1=(91.3/100)*maxX, y1=sum2$quantiles[1,5], lwd=1.8, lty=1, col="darkred")
  segments(x0=(91.3/100)*maxX-5, y0=sum2$quantiles[1,5], x1=(91.3/100)*maxX+5, y1=sum2$quantiles[1,5], lwd=1.8, lty=1, col="darkred")
  segments(x0=(91.3/100)*maxX-5, y0=sum2$quantiles[1,1], x1=(91.3/100)*maxX+5, y1=sum2$quantiles[1,1], lwd=1.8, lty=1, col="darkred")
  
  #abline(h=140.5)

  # text:
  s1<- paste(toString(round(sum2$statistics[1,1],1)), " ", "(", toString(round(sum2$quantiles[1,1],1)), ", ",
             toString(round(sum2$quantiles[1,5],1)), ")", sep="")
  text(x=(91.3/100)*maxX, y=-38, s1, cex=1.5, font=2, srt = 90, adj=0)
  
  s2<- paste(toString(round(sum1$statistics[1,1],1)), " ", "(", toString(round(sum1$quantiles[1,1],1)), ", ",
             toString(round(sum1$quantiles[1,5],1)), ")", sep="")
  text(x=(97.3/100)*maxX, y=-38, s2, cex=1.5, font=2, srt = 90, adj=0)
  
  
  
 #  legend:
  rect(xleft = (99/100)*maxX, ybottom = 22, xright = (94.9/100)*maxX, ytop = 73, lwd=1.8, lty = "solid", col="white") 
 #  observed:
  segments(x0=(96/100)*maxX, y0=24, x1=(96/100)*maxX, y1=35, lwd=2, lty=2, col="black")
  points(x= (96/100)*maxX, y=29.8, pch = 15, cex=2, col="white")
  points(x= (96/100)*maxX, y=29.8, pch = 0, cex=2, col="black")

 # posterior
  points(x= (97.8/100)*maxX, y=29.8, pch = 15, cex=2, col="darkred")
  segments(x0=(97.8/100)*maxX, y0=24, x1=(97.8/100)*maxX, y1=35, lwd=2, lty=1, col="darkred")
  
  # text:
  text(x=(97.8/100)*maxX, y=36, "Posterior (95% CrIs)", cex=1.5, font=2, srt = 90, adj=0)
  text(x=(96/100)*maxX, y=36, "Observed (95% CIs)", cex=1.5, font=2, srt = 90, adj=0)
  
 
  
  text(x=(2.5/100)*maxX, y=7, "FFD", cex=3.5, font=2, srt = 90, adj=0)
  
  ###############3
  # Plot Annotation
  
  plot (y, type="l", col="white", lty="solid", lwd=2, ylim=c(0, 60),
        xlim= c(0, maxX), axes=F, xaxs="i", yaxs="i", cex.lab=1.5)#, ann=FALSE
  abline(h=60)
  
  text(x=(3/100)*maxX, y=59.9, "Word N+1", cex=1.7, font=2, srt = 90, adj=1)
  text(x=(4.5/100)*maxX, y=58.2, "length", cex=1.7, font=2, srt = 90, adj=1)
  rect(xleft = (6.5/100)*maxX, ybottom = 52, xright = (82/100)*maxX, ytop = 60, lwd=1.8, lty = "solid", col="#F7F7F7") 
  
  # Plot study label  
  for (i in 1:nrow(df)){
    text(x=start+step*i, y=50, df$studies[i], cex=1.5, font=2, srt = 90, adj=1)
    text(x=start+step*i, y=56, df$N1len[i], cex=1.5, font=2, srt = 90)
    segments(x0=start+step*i, y0=51, x1=start+step*i, y1=52, lwd=1.8, lty=1, col="black")
  }

  
  text(x=(90.6/100)*maxX, y=55.2, "Posterior mean", cex=1.7, font=2, srt = 90, adj=1)
  text(x=(92/100)*maxX, y=58.6, "(alphabetical studies) *", cex=1.7, font=2, srt = 90, adj=1)
  
  text(x=(96.6/100)*maxX, y=55.2, "Posterior mean", cex=1.7, font=2, srt = 90, adj=1)
  text(x=(98/100)*maxX, y=53.8, "(all studies)", cex=1.7, font=2, srt = 90, adj=1)
  
  
  mtext("Studies", side=1, line= -16, cex=1.8, font=2)
  
  dev.off() 
  
  
  
  
  ######################################################
  #         2nd Plot for Gaze duration                 #
  ######################################################
  
  png(file = 'Plots/N2forest2_GD.png', width = 1400, height = 800, units = "px")  

  
  ############
  #variables #
  ############
  
  load("Data/data2.Rda") # GD, all studies
  #load("Data/data2.Rda")
  
  load("Summaries/N2/sum3.Rda") # GD, all studies
  load("Summaries/N2/sum4.Rda") # GD, alphabetical studies only 
  
  data<- data2
  data$Paper<- c("Rayner et al. (2007), Exp.1 *", "Rayner et al. (2007), Exp.2 *",
                 "Kliegl et al. (2007) *", "Angele et al. (2008) *",
                 "Yang et al. (2009), Exp.1", "Yang et al. (2009), Exp. 2",
                 "Yan et al. (2010)", "Angele & Rayner (2011) *",
                 "Risse & Kliegl (2011) *", "Yang et al. (2012)",
                 "Radach et al. (2013) *")
  
  
  data$prec<- 1/data$S.sqr
  weight<- NULL
  
  for(i in 1:nrow(data)){
    weight[i]<- data$prec[i]/sum(data$prec)
  }
  weight<- weight*100
  weight<- weight/(max(weight)) # so that the study with the biggest weight lead to 2x magnification
  
  studies<- data$Paper
  
  
  # observed:
  obsM<- data$T
  CI_L<- 1.96*sqrt(data$S.sqr); CI_L<- obsM- CI_L
  CI_R<- 1.96*sqrt(data$S.sqr); CI_R<- obsM+ CI_R
  
  # posterior:
  pM<- unname(sum3$statistics[3:nrow(sum3$statistics),1])
  CrI_L<- unname(sum3$quantiles[3:nrow(sum3$statistics),1])
  CrI_R<- unname(sum3$quantiles[3:nrow(sum3$statistics),5])
  
  rng<- c(CI_L, CI_R)
  maxX<- 300
  minY<- min(rng)
  maxY<- max(rng)
  margs<-c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80)
  
  sPlot<- minY-5
  start<- maxY*(1/100) # where to start plotting
  step<- 22 # step between studies
  
  df<- data.frame(studies, obsM, pM, CI_L, CI_R, CrI_L, CrI_R, weight)
  df<- df[11:1,]
  df$N1len<- c("3 let.", "1 char.", "3 let.", "3 let.", "1 char.",
               "2 char.", "1 char.", "6 let.", "3 let.", "3.8 let.", "5.3 let.")
  
  y<- 1:10
  
  par(mfrow=c(1,1), mai= c(0,0.1,0,0.85))
  
  plot (y, type="l", col="white", lty="solid", lwd=2
        , ylim=c(sPlot, maxY+35), xlim= c(0, maxX)
        , axes=F, xaxs="i", yaxs="i", cex.lab=1.7) #
  
  axis(side=4, at=c(margs[1], margs[1]+5, margs[2], margs[2]+5, margs[3], margs[3]+5, margs[4], margs[4]+5,
                    margs[5], margs[5]+5, margs[6], margs[6]+5, margs[7], margs[7]+5, margs[8], margs[8]+5,
                    margs[9], margs[9]+5, margs[10], margs[10]+5, margs[11], margs[11]+5, margs[12],
                    margs[12]+5, margs[13], margs[13]+5, margs[14]), 
       labels=c(toString(margs[1]), "", toString(margs[2]), "", toString(margs[3]), "", toString(margs[4]), "",
                toString(margs[5]), "",toString(margs[6]), "", toString(margs[7]), "", toString(margs[8]), "",
                toString(margs[9]), "", toString(margs[10]), "", toString(margs[11]), "", toString(margs[12]),
                "", toString(margs[13]), "", toString(margs[14])), 
       tick=T, cex.axis=1.5)
  
  mtext("Effect size (in ms)                          ", side=4, line= 3, cex=1.8, font=2)  
  
  # plot y grid
  for (i in 1:nrow(df)){
    segments(x0=start+step*i, y0=-70, x1=start+step*i, y1=maxY+35, lwd=1.8, lty=1, col="#E3E3E3")
  }
  
  abline(h=0)
  
  
  # Plot 95% CIs
  for (i in 1:nrow(df)){
    segments(x0=start+step*i- step/6, y0=df$CI_L[i], x1=start+step*i- step/6, y1=df$CI_R[i], lwd=1.8, lty=2, col="black")
    segments(x0=start+step*i-2*(step/6), y0=df$CI_L[i], x1=start+step*i-step/6+step/6, y1=df$CI_L[i], lwd=1.8, lty=1, col="black")
    segments(x0=start+step*i-2*(step/6), y0=df$CI_R[i], x1=start+step*i-step/6+step/6, y1=df$CI_R[i], lwd=1.8, lty=1, col="black")
  }    
  
  
  # Plot observed means
  for (i in 1:nrow(df)){
    points(x= start+step*i- step/6, y=df$obsM[i], pch = 15, cex=1+3*df$weight[i], col="white")
    points(x= start+step*i- step/6, y=df$obsM[i], pch = 0, cex=1+3*df$weight[i], col="black")
  }# (data$weight[i]/max(data$weight))
  
  # arithmetic mean:
  #  abline(h=mean(obsM), lwd=1.2, lty=2, col="darkorchid")
  
  # Plot posterior means
  for (i in 1:nrow(df)){
    points(x= start+step*i +step/6, y=df$pM[i], pch = 15, cex=1+3*df$weight[i], col="darkred")
  } #(data$weight[i]/max(data$weight))
  
  # Plot 95% CrIs
  for (i in 1:nrow(df)){
    segments(x0=start+step*i+step/6, y0=df$CrI_L[i], x1=start+step*i +step/6 , y1=df$CrI_R[i], lwd=1.8, lty=1, col="darkred")
    segments(x0=start+step*i+step/6-(step/6), y0=df$CrI_L[i], x1=start+step*i+2*(step/6), y1=df$CrI_L[i], lwd=1.8, lty=1, col="darkred")
    segments(x0=start+step*i+step/6-(step/6), y0=df$CrI_R[i], x1=start+step*i+2*(step/6), y1=df$CrI_R[i], lwd=1.8, lty=1, col="darkred")
  }    
  
  # posterior mean:
  #  abline(h=sum1$statistics[1,1], lwd=1.2, lty=1, col="darkred")
  
  # pooled mean (all studies):
  points(x= (97.3/100)*maxX, y=sum3$statistics[1,1], pch = 18, cex=3, col="darkred")
  segments(x0=(97.3/100)*maxX, y0=sum3$quantiles[1,1], x1=(97.3/100)*maxX, y1=sum3$quantiles[1,5], lwd=1.8, lty=1, col="darkred")
  segments(x0=(97.3/100)*maxX-5, y0=sum3$quantiles[1,5], x1=(97.3/100)*maxX+5, y1=sum3$quantiles[1,5], lwd=1.8, lty=1, col="darkred")
  segments(x0=(97.3/100)*maxX-5, y0=sum3$quantiles[1,1], x1=(97.3/100)*maxX+5, y1=sum3$quantiles[1,1], lwd=1.8, lty=1, col="darkred")
  
  # pooled mean (alphabetical studies):
  points(x= (91.3/100)*maxX, y=sum4$statistics[1,1], pch = 18, cex=3, col="darkred")
  segments(x0=(91.3/100)*maxX, y0=sum4$quantiles[1,1], x1=(91.3/100)*maxX, y1=sum4$quantiles[1,5], lwd=1.8, lty=1, col="darkred")
  segments(x0=(91.3/100)*maxX-5, y0=sum4$quantiles[1,5], x1=(91.3/100)*maxX+5, y1=sum4$quantiles[1,5], lwd=1.8, lty=1, col="darkred")
  segments(x0=(91.3/100)*maxX-5, y0=sum4$quantiles[1,1], x1=(91.3/100)*maxX+5, y1=sum4$quantiles[1,1], lwd=1.8, lty=1, col="darkred")
  
  #abline(h=140.5)
  
  # text:
  s1<- paste(toString(round(sum4$statistics[1,1],1)), " ", "(", toString(round(sum4$quantiles[1,1],1)), ", ",
             toString(round(sum4$quantiles[1,5],1)), ")", sep="")
  text(x=(91.3/100)*maxX, y=-45, s1, cex=1.5, font=2, srt = 90, adj=0)
  
  s2<- paste(toString(round(sum3$statistics[1,1],1)), " ", "(", toString(round(sum3$quantiles[1,1],1)), ", ",
             toString(round(sum3$quantiles[1,5],1)), ")", sep="")
  text(x=(97.3/100)*maxX, y=-45, s2, cex=1.5, font=2, srt = 90, adj=0)
  
  
  
  #  legend:
  rect(xleft = (99/100)*maxX, ybottom = 30, xright = (94.9/100)*maxX, ytop =81, lwd=1.8, lty = "solid", col="white") 
  #  observed:
  segments(x0=(96/100)*maxX, y0=32, x1=(96/100)*maxX, y1=43, lwd=2, lty=2, col="black")
  points(x= (96/100)*maxX, y=37.8, pch = 15, cex=2, col="white")
  points(x= (96/100)*maxX, y=37.8, pch = 0, cex=2, col="black")
  
  # posterior
  points(x= (97.8/100)*maxX, y=37.8, pch = 15, cex=2, col="darkred")
  segments(x0=(97.8/100)*maxX, y0=32, x1=(97.8/100)*maxX, y1=43, lwd=2, lty=1, col="darkred")
  
  # text:
  text(x=(97.8/100)*maxX, y=44, "Posterior (95% CrIs)", cex=1.5, font=2, srt = 90, adj=0)
  text(x=(96/100)*maxX, y=44, "Observed (95% CIs)", cex=1.5, font=2, srt = 90, adj=0)
  
  
  
  text(x=(2.5/100)*maxX, y=15, "GD", cex=3.5, font=2, srt = 90, adj=0)
  
  
  
  dev.off()     
      