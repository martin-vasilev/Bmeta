
# Function for creating a forest plot for a Bayesian meta-analysis
# V2: Failed to add annotation outside the graph (I do it externally)

# Generates a forest plot for N+1 preview effects on word n+1

# Martin R. Vasilev, 2016

#png(file = 'Plots/N1forest.png', width = 1300, height = 600, units = "px")


et_al<- function(string){
  t<- c(gregexpr(pattern =' ', string), gregexpr(pattern =',', string)); t<- as.numeric(unlist(t))
  t<- t[which(t>0)]; t<- min(t)
  auth<- substring(string, 1, t-1)
  C2<- as.numeric(unlist(gregexpr(pattern =')', string)))
  year<- substring(string, nchar(string)-5, C2)
  out<- paste(auth," et al. ", year, sep="")
  return(out)
}

############
#variables #
############
load("Data/data5.Rda")
data5$prec<- 1/data5$S.sqr
data5$weight<- NULL

for(i in 1:nrow(data5)){
  data5$weight[i]<- data5$prec[i]/sum(data5$prec)
}
data5$weight<- data5$weight*100
  
data5$Paper <- sapply(data5$Paper, as.character)
studies<- data5$Paper
studies<- et_al(studies)
obsM<- data5$T
CI_L<- 1.96*sqrt(data5$S.sqr); CI_L<- obsM- CI_L
CI_R<- 1.96*sqrt(data5$S.sqr); CI_R<- obsM+ CI_R

rng<- c(CI_L, CI_R)

maxX<- 1500
minY<- min(rng)
maxY<- max(rng)
sPlot<- minY-5
start<- maxY*(5/100) # where to start plotting
step<- 22 # step between studies

  y<- 1:10
  
  windows()
  par(mfrow=c(2,1), mai= c(0.1,0.1,0.2,0.5))
  
  library(plyr)
  t1<- round_any(minY, 10, f = floor)
  t2<- round_any(maxY, 10, f = ceiling)
  t<- t2-t1; 
  
  plot (y, type="l", col="white", lty="solid", lwd=2
        , ylim=c(sPlot, maxY), xlim= c(0, maxX)
        , axes=F, xaxs="i", yaxs="i", cex.lab=1.5, ann=FALSE)
  
  axis(side=4, at=c(minY, minY +1/5*(maxY), minY +2/5*(maxY),minY +3/5*(maxY),
                    minY +4/5*(maxY), maxY), 
       labels=c(toString(round(minY)), toString(round(maxY -4/5*(maxY))), toString(round(maxY -3/5*(maxY))),
                toString(round(maxY -2/5*(maxY))), toString(round(maxY -1/5*(maxY))), toString(round(maxY))), 
       tick=T, cex.axis=1.2)
  
  
# Plot 95% CIs
for (i in 1:length(studies)){
  segments(x0=start+step*i, y0=CI_L[i], x1=start+step*i, y1=CI_R[i], lwd=1.2, lty=2, col="darkorchid")
  segments(x0=start+step*i-step/4, y0=CI_L[i], x1=start+step*i+step/4, y1=CI_L[i], lwd=1.2, lty=1, col="darkorchid")
  segments(x0=start+step*i-step/4, y0=CI_R[i], x1=start+step*i+step/4, y1=CI_R[i], lwd=1.2, lty=1, col="darkorchid")
}    
  
  
# Plot observed means
for (i in 1:length(studies)){
  points(x= start+step*i, y=obsM[i], pch = 15, cex=3*(data5$weight[i]/max(data5$weight)), col="white", bg="green")
  points(x= start+step*i, y=obsM[i], pch = 0, cex=3*(data5$weight[i]/max(data5$weight)), col="darkred", bg="green")
}

  

  ###############3
  # Plot Annotation
  
  plot (y, type="l", col="white", lty="solid", lwd=2
        , ylim=c(0, 60), xlim= c(0, maxX)
        , axes=F, xaxs="i", yaxs="i", cex.lab=1.5, ann=FALSE)
  abline(h=60)
  
  # Plot study label  
  for (i in 1:length(studies)){
    text(x=start+step*i, y=30, studies[i], cex=1.2, font=2, srt = 90, adj=0)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  axis(side=2, at=c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
                    90), labels=c("" ,"10","","20", "", "30", "", "40", "", "50",
                    "", "60", "", "70", "", "80", "", "90"), las=1,tick=T, cex.axis=1.2)
  axis(side=4, at=c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
                   90), labels=c("" ,"10","","20", "", "30", "", "40", "", "50",
                                          "", "60", "", "70", "", "80", "", "90"), las=1,
       tick=T, cex.axis=1.2)
        
  axis(side=1, at=c(10, 30, 50, 70, 90, 110, 130), labels=c("ORTH","PHON", "SEM", "UNREL", "PSEUD", "RAN","X"), 
       tick=F, cex.axis=1.2)
  box(lty='solid') 

  abline(h= c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
    90), col= "gray92", lty=5)
  abline(v= c(20, 40, 60, 80, 100, 120), col= "darkgray")
  
  
  sizeM= 2
  sizeI=2.5
  
  pos1<- c(4, 8, 12, 16)
  pos2<- c(24, 28, 32, 36)
  pos3<- c(44, 48, 52, 56)
  pos4<- c(64, 68, 72, 76)
  pos5<- c(84, 88, 92, 96)
  pos6<- c(104, 108, 112, 116)
  pos7<- c(124, 128, 132, 136)
  
  # RAN mask:
  points(x= pos6[1], y= M_RAN_FFD, pch = 19, cex=sizeM, col="lightsteelblue4") #FFD
  points(x= pos6[2], y= M_RAN_SFD, pch = 15, cex=sizeM, col="lightsteelblue4") #SFD
  points(x= pos6[3], y= M_RAN_GD, pch = 17, cex=sizeM, col="lightsteelblue4") #GD
  points(x= pos6[4], y= M_RAN_TVT, pch = 18, cex=sizeM+0.5, col="lightsteelblue4") #TVT
  
  segments(x0=pos6[1], y0=M_RAN_FFD_CrI[1], x1=pos6[1], y1=M_RAN_FFD_CrI[2], lwd=sizeI, col="lightsteelblue4") #FFD
  segments(x0=pos6[2], y0=M_RAN_SFD_CrI[1], x1=pos6[2], y1=M_RAN_SFD_CrI[2], lwd=sizeI, col="lightsteelblue4") #SFD
  segments(x0=pos6[3], y0=M_RAN_GD_CrI[1], x1=pos6[3], y1=M_RAN_GD_CrI[2], lwd=sizeI, col="lightsteelblue4") #GD
  segments(x0=pos6[4], y0=M_RAN_TVT_CrI[1], x1=pos6[4], y1=M_RAN_TVT_CrI[2], lwd=sizeI, col="lightsteelblue4") #TVT
  
  # UNREL mask:
  points(x= pos4[1], y= M_UNREL_FFD, pch = 19, cex=sizeM, col="darkred") #FFD
  points(x= pos4[2], y= M_UNREL_SFD, pch = 15, cex=sizeM, col="darkred") #SFD
  points(x= pos4[3], y= M_UNREL_GD, pch = 17, cex=sizeM, col="darkred") #GD
  points(x= pos4[4], y= M_UNREL_TVT, pch = 18, cex=sizeM+0.5, col="darkred") #TVT
  
  segments(x0=pos4[1], y0=M_UNREL_FFD_CrI[1], x1=pos4[1], y1=M_UNREL_FFD_CrI[2], lwd=sizeI, col="darkred") #FFD
  segments(x0=pos4[2], y0=M_UNREL_SFD_CrI[1], x1=pos4[2], y1=M_UNREL_SFD_CrI[2], lwd=sizeI, col="darkred") #SFD
  segments(x0=pos4[3], y0=M_UNREL_GD_CrI[1], x1=pos4[3], y1=M_UNREL_GD_CrI[2], lwd=sizeI, col="darkred") #GD
  segments(x0=pos4[4], y0=M_UNREL_TVT_CrI[1], x1=pos4[4], y1=M_UNREL_TVT_CrI[2], lwd=sizeI, col="darkred") #TVT
  
  # PSEUD mask:
  points(x= pos5[1], y= M_PSEUD_FFD, pch = 19, cex=sizeM, col="darkblue") #FFD
  points(x= pos5[2], y= M_PSEUD_SFD, pch = 15, cex=sizeM, col="darkblue") #SFD
  points(x= pos5[3], y= M_PSEUD_GD, pch = 17, cex=sizeM, col="darkblue") #GD
  points(x= pos5[4], y= M_PSEUD_TVT, pch = 18, cex=sizeM+0.5, col="darkblue") #TVT
  
  segments(x0=pos5[1], y0=M_PSEUD_FFD_CrI[1], x1=pos5[1], y1=M_PSEUD_FFD_CrI[2], lwd=sizeI, col="darkblue") #FFD
  segments(x0=pos5[2], y0=M_PSEUD_SFD_CrI[1], x1=pos5[2], y1=M_PSEUD_SFD_CrI[2], lwd=sizeI, col="darkblue") #SFD
  segments(x0=pos5[3], y0=M_PSEUD_GD_CrI[1], x1=pos5[3], y1=M_PSEUD_GD_CrI[2], lwd=sizeI, col="darkblue") #GD
  segments(x0=pos5[4], y0=M_PSEUD_TVT_CrI[1], x1=pos5[4], y1=M_PSEUD_TVT_CrI[2], lwd=sizeI, col="darkblue") #TVT
  
  # X mask:
  points(x= pos7[1], y= M_X_FFD, pch = 19, cex=sizeM, col="darkgreen") #FFD
  points(x= pos7[3], y= M_X_GD, pch = 17, cex=sizeM, col="darkgreen") #GD
  
  segments(x0=pos7[1], y0=M_X_FFD_CrI[1], x1=pos7[1], y1=M_X_FFD_CrI[2], lwd=sizeI, col="darkgreen") #FFD
  segments(x0=pos7[3], y0=M_X_GD_CrI[1], x1=pos7[3], y1=M_X_GD_CrI[2], lwd=sizeI, col="darkgreen") #GD
  
  # ORTH mask:
  points(x= pos1[1], y= M_ORTH_FFD, pch = 19, cex=sizeM, col="darkorchid") #FFD
  points(x= pos1[2], y= M_ORTH_SFD, pch = 15, cex=sizeM, col="darkorchid") #SFD
  points(x= pos1[3], y= M_ORTH_GD, pch = 17, cex=sizeM, col="darkorchid") #GD
  points(x= pos1[4], y= M_ORTH_TVT, pch = 18, cex=sizeM+0.5, col="darkorchid") #TVT
  
  segments(x0=pos1[1], y0=M_ORTH_FFD_CrI[1], x1=pos1[1], y1=M_ORTH_FFD_CrI[2], lwd=sizeI, col="darkorchid") #FFD
  segments(x0=pos1[2], y0=M_ORTH_SFD_CrI[1], x1=pos1[2], y1=M_ORTH_SFD_CrI[2], lwd=sizeI, col="darkorchid") #SFD
  segments(x0=pos1[3], y0=M_ORTH_GD_CrI[1], x1=pos1[3], y1=M_ORTH_GD_CrI[2], lwd=sizeI, col="darkorchid") #GD
  segments(x0=pos1[4], y0=M_ORTH_TVT_CrI[1], x1=pos1[4], y1=M_ORTH_TVT_CrI[2], lwd=sizeI, col="darkorchid") #TVT
  
  # SEM mask:
  points(x= pos3[1], y= M_SEM_FFD, pch = 19, cex=sizeM, col="darkorange") #FFD
  points(x= pos3[2], y= M_SEM_SFD, pch = 15, cex=sizeM, col="darkorange") #SFD
  points(x= pos3[3], y= M_SEM_GD, pch = 17, cex=sizeM, col="darkorange") #GD
  
  segments(x0=pos3[1], y0=M_SEM_FFD_CrI[1], x1=pos3[1], y1=M_SEM_FFD_CrI[2], lwd=sizeI, col="darkorange") #FFD
  segments(x0=pos3[2], y0=M_SEM_SFD_CrI[1], x1=pos3[2], y1=M_SEM_SFD_CrI[2], lwd=sizeI, col="darkorange") #SFD
  segments(x0=pos3[3], y0=M_SEM_GD_CrI[1], x1=pos3[3], y1=M_SEM_GD_CrI[2], lwd=sizeI, col="darkorange") #GD
  
  # PHON mask:
  points(x= pos2[1], y= M_PHON_FFD, pch = 19, cex=sizeM, col="peru") #FFD
  points(x= pos2[3], y= M_PHON_GD, pch = 17, cex=sizeM, col="peru") #GD
  
  segments(x0=pos2[1], y0=M_PHON_FFD_CrI[1], x1=pos2[1], y1=M_PHON_FFD_CrI[2], lwd=sizeI, col="peru") #FFD
  segments(x0=pos2[3], y0=M_PHON_GD_CrI[1], x1=pos2[3], y1=M_PHON_GD_CrI[2], lwd=sizeI, col="peru") #GD
  
  # LEGEND:
  
  rect(xleft = 3.5, ybottom = 68, xright = 16.5, ytop = 89, lwd=1.2, lty = "solid", col="white")
  points(x= 7, y= 85, pch = 19, cex=sizeM, col="black") #FFD
  points(x= 7, y= 81.5, pch = 15, cex=sizeM, col="black") #SFD
  points(x= 7, y= 76.5, pch = 17, cex=sizeM, col="black") #GD
  points(x= 7, y= 71.5, pch = 18, cex=sizeM+0.5, col="black") #TVT
  
  segments(x0=5, y0=85, x1=9, y1=85, lwd=sizeI, col="black") #FFD
  segments(x0=5, y0=81.5, x1=9, y1=81.5, lwd=sizeI, col="black") #SFD
  segments(x0=5, y0=76.5, x1=9, y1=76.5, lwd=sizeI, col="black") #GD
  segments(x0=5, y0=71.5, x1=9, y1=71.5, lwd=sizeI, col="black") #TVT
  
  text(x=13, y=85.5, "FFD", cex=1.2, font=2)#FFD
  text(x=13, y=81.5, "SFD", cex=1.2, font=2)#SFD
  text(x=13, y=76.5, "GD", cex=1.2, font=2)#GD
  text(x=13, y=71.5, "TVT", cex=1.2, font=2)#TVT
  
  
  # SAMPLE SIZE:
  rect(xleft = 0, ybottom = -5, xright = 140, ytop = 0, lwd=1.2, lty = "solid", col="snow1")
  text(x= c(pos1[1], pos1[2], pos1[3], pos1[4], pos2[1], pos2[3], pos3[1], pos3[2], pos3[3], pos4[1], pos4[2], pos4[3], pos4[4],
            pos5[1], pos5[2], pos5[3], pos5[4], pos6[1], pos6[2], pos6[3], pos6[4], pos7[1], pos7[3]),
       y= c(-2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2, -2),
       c("24", "16", "25", "8", "9", "10", "8", "9", "11", "29", "17", "32", "9",
         "14", "7", "15", "6", "26", "10", "26", "9", "6", "6"), cex=1.2)
  par(xpd=NA)
  text(x=-3.5, y=-2, "N. Exp.", cex=1.2, font=2)#TVT
  
  # MEAN TEXT STAMPS:
  text(x=-3.5, y=98, "P. Mean", cex=1.2, font=2)#TVT
  rect(xleft = 0, ybottom = 95, xright = 140, ytop = 100, lwd=1.2, lty = "solid", col="snow1")
  
  # study means:
  text(x= c(pos1[1], pos1[2], pos1[3], pos1[4], pos2[1], pos2[3], pos3[1], pos3[2], pos3[3], pos4[1], pos4[2], pos4[3], pos4[4],
            pos5[1], pos5[2], pos5[3], pos5[4], pos6[1], pos6[2], pos6[3], pos6[4], pos7[1], pos7[3]),
       y= c(98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98),
       c(toString(round(M_ORTH_FFD, 1)), toString(round(M_ORTH_SFD, 1)), toString(round(M_ORTH_GD, 1)), toString(round(M_ORTH_TVT, 1)),
         toString(round(M_PHON_FFD, 1)), toString(round(M_PHON_GD, 1)), toString(round(M_SEM_FFD, 1)), toString(round(M_SEM_SFD, 1)),
         toString(round(M_SEM_GD, 1)), toString(round(M_UNREL_FFD, 1)), toString(round(M_UNREL_SFD, 1)), toString(round(M_UNREL_GD, 1)),
         toString(round(M_UNREL_TVT, 1)), toString(round(M_PSEUD_FFD, 1)), toString(round(M_PSEUD_SFD, 1)), toString(round(M_PSEUD_GD, 1)),
         toString(round(M_PSEUD_TVT, 1)), toString(round(M_RAN_FFD, 1)), toString(round(M_RAN_SFD, 1)), toString(round(M_RAN_GD, 1)), 
         toString(round(M_RAN_TVT, 1)), toString(round(M_X_FFD, 1)), toString(round(M_X_GD, 1))), cex=1.2)
  
  
  text(x=70, y=104, "All studies", cex=2, font=2)
  
  dev.off() 
  
  
