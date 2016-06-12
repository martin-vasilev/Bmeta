
# Martin R. Vasilev, 2016

# creates heatmap graphs based on data created by another function

rm(list=ls())
source("Plots/functions/mask_heatmap.R") # WARNING: takes long to execute

#mask_heatmap() # run only to repeat analysis!!
load("Data/heat_FFD.Rda"); load("Data/heat_GD.Rda")


################
# create plots #
################
library(ggplot2)
library(grid)

## FFD
db$y<- factor(db$y, levels = c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"))
db$x<- factor(db$x, levels = c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"))
colnames(db)<- c("y", "x", "Mean_difference")

Q1<- qplot(x=x, y=y, data=db, fill=Mean_difference) + theme_bw()+ geom_tile(colour="#F0F4F5", size=1.2)+
     scale_fill_gradient2(limits=c(round(min(db$Mean_difference))-1, round(max(db$Mean_difference)))+1,
     low= "darkgreen", mid="white", high="darkred") +xlab("")+
     ylab("")+ ggtitle("FFD")+
     theme(panel.grid.major = element_line(color="black", size=0.2),
     panel.grid.minor = element_blank(), panel.background = element_blank(),
     axis.line = element_line(colour = "black"),axis.title.x = element_text(size=24),
     axis.title = element_text(size=24, face="bold"),
     plot.title= element_text(size=24, face="bold"),
     axis.text=element_text(size=18), legend.text=element_text(size=20),
     legend.key.width=unit(1.5,"cm"), legend.position="bottom",
     legend.title=element_text(size=20, face= "bold"))+
     geom_segment(mapping=aes(x=0.2, y=1, xend=0.48, yend=1),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=0.2, y=2, xend=0.48, yend=2),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=0.2, y=3, xend=0.48, yend=3),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=0.2, y=4, xend=0.48, yend=4),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=0.2, y=5, xend=0.48, yend=5),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=0.2, y=6, xend=0.48, yend=6),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=0.2, y=7, xend=0.48, yend=7),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=1, y=7.86, xend=1, yend=7.56),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=2, y=7.86, xend=2, yend=7.56),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=3, y=7.86, xend=3, yend=7.56),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=4, y=7.86, xend=4, yend=7.56),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=5, y=7.86, xend=5, yend=7.56),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=6, y=7.86, xend=6, yend=7.56),
     arrow=arrow(), size=1.5, color="black")+
     geom_segment(mapping=aes(x=7, y=7.86, xend=7, yend=7.56),
     arrow=arrow(), size=1.5, color="black")


## GD 
db2$y<- factor(db2$y, levels = c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"))
db2$x<- factor(db2$x, levels = c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"))
colnames(db2)<- c("y", "x", "Mean_difference")

Q2<- qplot(x=x, y=y, data=db2, fill=Mean_difference) + theme_bw()+ geom_tile(colour="#F0F4F5", size=1.2)+
  scale_fill_gradient2(limits=c(round(min(db2$Mean_difference))-1, round(max(db2$Mean_difference)))+1,
                       low= "darkgreen", mid="white", high="darkred") +xlab("")+
  ylab("")+ ggtitle("GD")+
  theme(panel.grid.major = element_line(color="black", size=0.2),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"),axis.title.x = element_text(size=24),
        axis.title = element_text(size=24, face="bold"),
        plot.title= element_text(size=24, face="bold"),
        axis.text=element_text(size=18), legend.text=element_text(size=20),
        legend.key.width=unit(1.5,"cm"), legend.position="bottom",
        legend.title=element_text(size=20, face= "bold"))+
        geom_segment(mapping=aes(x=0.2, y=1, xend=0.48, yend=1),
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=0.2, y=2, xend=0.48, yend=2),
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=0.2, y=3, xend=0.48, yend=3),
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=0.2, y=4, xend=0.48, yend=4), 
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=0.2, y=5, xend=0.48, yend=5),
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=0.2, y=6, xend=0.48, yend=6),
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=0.2, y=7, xend=0.48, yend=7), 
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=1, y=7.86, xend=1, yend=7.56),
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=2, y=7.86, xend=2, yend=7.56), 
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=3, y=7.86, xend=3, yend=7.56), 
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=4, y=7.86, xend=4, yend=7.56),
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=5, y=7.86, xend=5, yend=7.56),
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=6, y=7.86, xend=6, yend=7.56), 
        arrow=arrow(), size=1.5, color="black")+
        geom_segment(mapping=aes(x=7, y=7.86, xend=7, yend=7.56),
        arrow=arrow(), size=1.5, color="black") #+
        #annotation_custom(grob = rectGrob(), xmin = 1, xmax = 2, ymin = 1, ymax = -1.3)

###############
# save graphs #
###############

png(file = 'Plots/heat_FFD.png', width = 800, height = 800, units = "px")
Q1
dev.off()

png(file = 'Plots/heat_GD.png', width = 800, height = 800, units = "px")
Q2
dev.off()
