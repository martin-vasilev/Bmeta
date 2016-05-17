
# Function for creating a forest plot for a Bayesian meta-analysis
# V2: Failed to add annotation outside the graph (I do it externally)

# Generates a forest plot for N+1 preview effects on word n+1

# Martin R. Vasilev, 2016

png(file = 'Plots/N1forest3.png', width = 700, height = 534, units = "px")

  
  M_UNREL_FFD= M35_meanP; M_UNREL_FFD_CrI= M35_MuCrI
  M_UNREL_SFD= M36_meanP; M_UNREL_SFD_CrI= M36_MuCrI
  M_UNREL_GD= M37_meanP; M_UNREL_GD_CrI= M37_MuCrI
  
  M_PSEUD_FFD= M38_meanP; M_PSEUD_FFD_CrI= M38_MuCrI
  M_PSEUD_GD= M39_meanP; M_PSEUD_GD_CrI= M39_MuCrI

  y<- 1:10
  
  plot (y, type="l", col="white", lty="solid", lwd=2
        , ylab=c("Effect size (in ms)")
        , xlab=c("Parafoveal mask")
        , ylim=c(-5,95), xlim= c(0, 40)
        , axes=F, xaxs="i", yaxs="i", cex.lab=1.5)
  
  axis(side=2, at=c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
                    90), labels=c("" ,"10","","20", "", "30", "", "40", "", "50",
                    "", "60", "", "70", "", "80", "", "90"), las=1,tick=T, cex.axis=1.2)
  axis(side=4, at=c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
                   90), labels=c("" ,"10","","20", "", "30", "", "40", "", "50",
                                          "", "60", "", "70", "", "80", "", "90"), las=1,
       tick=T, cex.axis=1.2)
        
  axis(side=1, at=c(10, 30), labels=c("UNREL", "PSEUD"), 
       tick=F, cex.axis=1.2)
  box(lty='solid') 

  abline(h= c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85,
    90), col= "gray92", lty=5)
  abline(v= c(20), col= "darkgray")
  
  
  sizeM= 2
  sizeI=2.5
  
  pos1<- c(4, 8, 12, 16)
  pos2<- c(24, 28, 32, 36)
  
  
  # UNREL mask:
  points(x= pos1[1], y= M_UNREL_FFD, pch = 19, cex=sizeM, col="darkred") #FFD
  points(x= pos1[2], y= M_UNREL_SFD, pch = 15, cex=sizeM, col="darkred") #SFD
  points(x= pos1[3], y= M_UNREL_GD, pch = 17, cex=sizeM, col="darkred") #GD
  
  segments(x0=pos1[1], y0=M_UNREL_FFD_CrI[1], x1=pos1[1], y1=M_UNREL_FFD_CrI[2], lwd=sizeI, col="darkred") #FFD
  segments(x0=pos1[2], y0=M_UNREL_SFD_CrI[1], x1=pos1[2], y1=M_UNREL_SFD_CrI[2], lwd=sizeI, col="darkred") #SFD
  segments(x0=pos1[3], y0=M_UNREL_GD_CrI[1], x1=pos1[3], y1=M_UNREL_GD_CrI[2], lwd=sizeI, col="darkred") #GD
  
  # PSEUD mask:
  points(x= pos2[1], y= M_PSEUD_FFD, pch = 19, cex=sizeM, col="darkblue") #FFD
  points(x= pos2[3], y= M_PSEUD_GD, pch = 17, cex=sizeM, col="darkblue") #GD
  
  segments(x0=pos2[1], y0=M_PSEUD_FFD_CrI[1], x1=pos2[1], y1=M_PSEUD_FFD_CrI[2], lwd=sizeI, col="darkblue") #FFD
  segments(x0=pos2[3], y0=M_PSEUD_GD_CrI[1], x1=pos2[3], y1=M_PSEUD_GD_CrI[2], lwd=sizeI, col="darkblue") #GD
  
  # LEGEND:
  rect(xleft = 1.5, ybottom = 70, xright = 9, ytop = 90, lwd=1.2, lty = "solid", col="white")
  points(x= 4, y= 85, pch = 19, cex=sizeM, col="black") #FFD
  points(x= 4, y= 80, pch = 15, cex=sizeM, col="black") #SFD
  points(x= 4, y= 75, pch = 17, cex=sizeM, col="black") #GD
  
  segments(x0=2.5, y0=85, x1=5.5, y1=85, lwd=sizeI, col="black") #FFD
  segments(x0=2.5, y0=80, x1=5.5, y1=80, lwd=sizeI, col="black") #SFD
  segments(x0=2.5, y0=75, x1=5.5, y1=75, lwd=sizeI, col="black") #GD
  
  text(x=7.25, y=85.5, "FFD", cex=1.2, font=2)#FFD
  text(x=7.25, y=80, "SFD", cex=1.2, font=2)#SFD
  text(x=7.25, y=75, "GD", cex=1.2, font=2)#GD
  
  
  # SAMPLE SIZE:
  rect(xleft = 0, ybottom = -5, xright = 40, ytop = 0, lwd=1.2, lty = "solid", col="snow1")
  text(x= c(pos1[1], pos1[2], pos1[3], pos2[1], pos2[3]),
       y= c(-2, -2, -2, -2, -2),
       c("11", "7", "13", "6", "7"), cex=1.2)
  par(xpd=NA)
  text(x=-1.7, y=-2, "N. Exp.", cex=1.2, font=2)#TVT
  
  # MEAN TEXT STAMPS:
  text(x=-2, y=98, "P. Mean", cex=1.2, font=2)#TVT
  rect(xleft = 0, ybottom = 95, xright = 40, ytop = 100, lwd=1.2, lty = "solid", col="snow1")
  
  # study means:
  text(x= c(pos1[1], pos1[2], pos1[3], pos2[1], pos2[3]),
       y= c(98, 98, 98, 98, 98),
       c(toString(round(M_UNREL_FFD, 1)), toString(round(M_UNREL_SFD, 1)), toString(round(M_UNREL_GD, 1)),
        toString(round(M_PSEUD_FFD, 1)), toString(round(M_PSEUD_GD, 1))), cex=1.2)
  
  
  text(x=20, y=104, "Non-alphabetical studies", cex=2, font=2)
  
  dev.off() 
  
  
