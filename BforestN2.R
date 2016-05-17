
# Function for creating a forest plot for a Bayesian meta-analysis
# V2: Failed to add annotation outside the graph (I do it externally)

# Generates a forest plot for N+2 preview effects on word n+2

# Martin R. Vasilev, 2016

BforestN2<- function(means= data1$T, variance= data1$S.sqr, means2= data2$T, variance2= data2$S.sqr,
                     L= unlist(M1_thetaCrI[,1]), R= unlist(M1_thetaCrI[,2]), L2= unlist(M3_thetaCrI[,1]),
                     R2= unlist(M3_thetaCrI[,2]), Mu=  M1_meanP, Mu2= M3_meanP, Mu3= M2_meanP, 
                     Mu4= M4_meanP, MuCrI= M1_MuCrI, MuCrI2= M3_MuCrI, MuCrI3= M2_MuCrI, MuCrI4= M4_MuCrI){
  # Load ggplot2:
  if("ggplot2" %in% rownames(installed.packages())==FALSE){
    message("Installing required package 'ggplot2'...")
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  
  df<- data.frame(means,variance)
  df2<- data.frame(means2,variance2)
  N<- length(df$means)
   
  x1<- means
  x2<- x1+0.2
  y1<-(1+3):(N+3)
  y2<- y1+0.2
  
  # GD:
  x1_2<- means2
  x2_2<- x1_2+0.2
  y1_2<-(1+3):(N+3); y1_2<- y1_2-0.2
  y2_2<- y1_2+0.2; 
  

  # FFD:
 # L<- c(4.4811, -2.8502, -2.9760, 1.7699, 4.2097, 6.7473, 5.7019, 0.4585, 0.9351, 1.7846, 6.2478)
#  R<- c(9.082, 2.174, 2.507, 6.397, 9.307, 11.381, 9.807, 6.070, 5.973, 6.395, 11.585)
  
  # GD:
#  L2<- c(-9.270, -2.606, 1.430, -1.494, 6.195, 22.276, 12.048, 9.178, 3.223, -2.645, 13.293)
#  R2<- c(-4.131, 2.897,  7.737, 3.753, 11.711, 27.927, 16.698, 15.498, 8.855, 2.944, 19.234)

 # Stamp<- paste(x1, " (",L, ", ", R,")", sep="")
 # Stamp2<- paste(x1_2, " (",L2, ", ", R2,")", sep="")
  ES<-data.frame(x1,x2,y1,y2, L, R) #, ,name, Stamp)
  ES2<-data.frame(x1_2,x2_2,y1_2,y2_2, L2, R2) #,name, Stamp2)
  # Intervals<- data.frame(df$means,L,R)
  
  squareF <- function(y, means, variance){
    # Function for calculating the size of squares on a forest plot
  
    prec<- 1/variance
    half<- prec*5
    square<-NULL
    square[1]<- means-half*(2.4+20) #x1
    square[2]<- means+half*(2.4+20) #x2
    square[3]<- y-half*(2.4+2.5) #y1
    square[4]<- y+half*(2.4+2.5) #y2
    
    return(square)
  }
    square<-matrix(0, 11, 4)
    square2<-matrix(0, 11, 4)
  for(i in 1:N){
    square[i,]<-squareF(y1[i], means[i], variance[i])
    square2[i,]<-squareF(y1[i], means2[i], variance2[i])
  }
  square<- data.frame(square); colnames(square)<- c("x1","x2","y1","y2")
  square2<- data.frame(square2); colnames(square2)<- c("x1","x2","y1","y2")
  square$y1<-square$y1+0.1; square$y2<-square$y2+0.1
  square2$y1<-square2$y1-0.1; square2$y2<-square2$y2-0.1
  # Initiate & prepare plot
  library(scales)

  
  Plot<- ggplot(df, aes(x=means, y=variance)) + ylim(-1,N+7) + xlim(-30, 30) + geom_blank() + xlab("Effect size (in ms)")+ylab("")+
         theme_bw() + theme(legend.position="none", panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), panel.background = element_blank(),
         axis.line = element_line(colour = "black"))+
         geom_vline(xintercept=0) + theme(axis.text.y = element_blank()) + scale_y_continuous(breaks=NULL)+
         scale_x_continuous(breaks=pretty_breaks(8)) + theme(axis.title.x = element_text(size=16), 
         axis.text=element_text(size=17))
  Plot<- Plot + coord_cartesian(ylim = c(-1,15))     
  
  # Plot pooled estimate (alphabetical-only):
  # FFD:
  Plot<- Plot + geom_point(mapping=aes(x=Mu3, y=2, shape=23), color="black", fill="black", size=8) +
  scale_shape_identity()
  Plot<- Plot + geom_segment(data=ES, aes(x = MuCrI3[1], y =2 , xend = Mu3 , yend = 2))
  Plot<- Plot + geom_segment(data=ES, aes(x = Mu3, y =2 , xend = MuCrI3[2] , yend = 2))
  
  # GD:
  Plot<- Plot + geom_point(mapping=aes(x=Mu4, y=1.62, shape=23), color="red", fill="red", size=8) +
  scale_shape_identity()
  Plot<- Plot + geom_segment(data=ES, aes(x = MuCrI4[1], y =1.62, xend = Mu4, yend = 1.62), color="red")
  Plot<- Plot + geom_segment(data=ES, aes(x = Mu4, y =1.62, xend = MuCrI4[2], yend = 1.62), color="red")

  # Plot pooled estimate (all studies):
  # FFD:
  Plot<- Plot + geom_point(mapping=aes(x=Mu, y=0.5, shape=23), color="black", fill="black", size=8) +
  scale_shape_identity()
  Plot<- Plot + geom_segment(data=ES, aes(x = MuCrI[1], y =0.5 , xend = Mu , yend = 0.5))
  Plot<- Plot + geom_segment(data=ES, aes(x = Mu, y =0.5 , xend = MuCrI[2] , yend = 0.5))
  
  # GD:
  Plot<- Plot + geom_point(mapping=aes(x=Mu2, y=0.13, shape=23), color="red", fill="red", size=8) +
  scale_shape_identity()
  Plot<- Plot + geom_segment(data=ES, aes(x = MuCrI2[1], y =0.13 , xend = Mu2 , yend = 0.13), color="red")
  Plot<- Plot + geom_segment(data=ES, aes(x = Mu2, y =0.13 , xend = MuCrI2[2] , yend = 0.13), color="red")

  # Add error bars:
  Plot<- Plot + geom_segment(data=ES, aes(x = L, y =y1+0.1 , xend = x1 , yend = y1+0.1)) # left-hand
  Plot<- Plot + geom_segment(data=ES, aes(x = R, y =y1+0.1 , xend = x1 , yend = y1+0.1)) # right-hand
  # GD:
  Plot<- Plot + geom_segment(data=ES2, aes(x = L2, y =y1_2+0.1 , xend = x1_2 , yend = y1_2+0.1), color="red") # left-hand
  Plot<- Plot + geom_segment(data=ES2, aes(x = R2, y =y1_2+0.1 , xend = x1_2 , yend = y1_2+0.1), color="red") # right-hand
 
  # Plot effect sizes of individual studies:
  #Plot<- Plot + geom_rect(data=ES, mapping=aes(ymin=y1, ymax=y2, xmin=x1, xmax=x2))
  Plot<- Plot + geom_rect(data=square, mapping=aes(ymin=y1, ymax=y2, xmin=x1, xmax=x2), color="black", fill="black")
  # GD:
  #Plot<- Plot + geom_rect(data=ES2, mapping=aes(ymin=y1_2, ymax=y2_2, xmin=x1_2, xmax=x2_2), color="red", fill="red")
  Plot<- Plot + geom_rect(data=square2, mapping=aes(ymin=y1, ymax=y2, xmin=x1, xmax=x2), color="red", fill="red")
  # http://sape.inf.usi.ch/quick-reference/ggplot2/geom_rect



# Legend:
Plot<- Plot + geom_rect(mapping=aes(ymin= -0.05, ymax=2.35, xmin=19.5, xmax=32.5),size=0.7, color="black", fill="white")

Plot<- Plot + geom_point(mapping=aes(x=23.5, y=1.25, shape=22), color="black", fill="black", size=4.5) +
  scale_shape_identity()

Plot<- Plot + geom_segment(mapping= aes(x = 21.5, y =1.25 , xend = 25.5 , yend = 1.25))

Plot<- Plot + geom_text(mapping=aes(x=28.5, y=1.25, label= "FFD"), size=5)

Plot<- Plot + geom_point(mapping=aes(x=23.5, y=0.65, shape=22), color="red", fill="red", size=4.5) +
  scale_shape_identity()

Plot<- Plot + geom_segment(mapping= aes(x = 21.5, y =0.65 , xend = 25.5 , yend = 0.65), color="red")

Plot<- Plot + geom_text(mapping=aes(x=28.2, y=0.65, label= "GD"), size=5)

Plot<- Plot + geom_text(mapping=aes(x=26, y=1.9, label= "Measure"), size=6)

Plot

return(Plot)

}
