# meta-regression heatmap of parafoveal masks


mask_heatmap<- function(){
  
  source("JReg.R") 
  library(rjags)  
  
  ### load data:
  load('Data/X_FFD.Rda'); load('Data/X_GD.Rda') # X    
  load('Data/RAN_FFD.Rda'); load('Data/RAN_GD.Rda') #RAN
  load('Data/PSEUD_FFD.Rda'); load('Data/PSEUD_GD.Rda') # PSEUD
  load('Data/UNREL_FFD.Rda'); load('Data/UNREL_GD.Rda') # UNREL 
  load('Data/SEM_FFD.Rda'); load('Data/SEM_GD.Rda') # SEM
  load('Data/PHON_FFD.Rda'); load('Data/PHON_GD.Rda') # PHON
  load('Data/ORTH_FFD.Rda'); load('Data/ORTH_GD.Rda') # SEM
  
  db<- data.frame(c("X", "X", "X", "X", "X", "X", "X", #X
                    "RAN", "RAN", "RAN", "RAN", "RAN", "RAN", "RAN", #RAN
                    "PSEUD", "PSEUD", "PSEUD", "PSEUD", "PSEUD", "PSEUD", "PSEUD", #PSEUD
                    "UNREL", "UNREL", "UNREL", "UNREL", "UNREL", "UNREL", "UNREL", #UNREL
                    "SEM", "SEM", "SEM", "SEM", "SEM", "SEM", "SEM", #SEM
                    "PHON", "PHON", "PHON", "PHON", "PHON", "PHON", "PHON", #PHON
                    "ORTH", "ORTH", "ORTH", "ORTH", "ORTH", "ORTH", "ORTH" #ORTH
                    ),
                      rep(c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"),7))
  colnames(db)<- c("y", "x")
  
  # Get beta estimates for comparisons:
  beta<- NULL
  data<- NULL; d1<- NULL; d2<- NULL
  prob<- NULL
  
 ### FFD
  for (i in 1:nrow(db)){
    txt1<- paste(db$y[i],"_FFD", sep="")
    txt2<- paste(db$x[i],"_FFD", sep="")
    d1<- eval(parse(text= txt1)); d1$cov<- 1
    d2<- eval(parse(text= txt2)); d2$cov=-1
    data<- rbind(d1,d2); data<- data[ ,c(3,4,6)]
    
    MR<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(data),
                          "heatmap.txt"), data, n.chains=3, n.adapt=3000, quiet=TRUE)
    message(paste("Creating meta-regression model for ", 
                  paste(db$y[i]), " and ", paste(db$x[i]),
                  " masks for ", "FFD", sep=""))
    R<- coda.samples(MR, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
    sum<- summary(R);# sum
    
    beta[i]<- sum$statistics[1,1]
    
    #print("Calculating probability difference is bigger than 0")
    # S<-jags.samples(MR, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
    #S<-c(S$beta[1,,1],S$beta[1,,2],S$beta[1,,3])
    #ECDF<- ecdf(S); 
    #prob[i]<- 1- ECDF(0)
    print(sprintf("%i models completed out of %i", i-1, nrow(db)))
  }
  
  db$beta<- beta; #db$prob<- prob
  
  save(db, file="Data/heat_FFD.Rda")
  
  
  ### GD
  db2<-db
  for (i in 1:nrow(db2)){
    txt1<- paste(db2$y[i],"_GD", sep="")
    txt2<- paste(db2$x[i],"_GD", sep="")
    d1<- eval(parse(text= txt1)); d1$cov<- 1
    d2<- eval(parse(text= txt2)); d2$cov=-1
    data<- rbind(d1,d2); data<- data[ ,c(3,4,6)]
    
    MR<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(data),
                         "heatmap.txt"), data, n.chains=3, n.adapt=3000, quiet=TRUE)
    message(paste("Creating meta-regression model for ", 
                  paste(db2$y[i]), " and ", paste(db2$x[i]),
                  " masks for ", "FFD", sep=""))
    R<- coda.samples(MR, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
    sum<- summary(R);# sum
    
    beta[i]<- sum$statistics[1,1]
    
    #print("Calculating probability difference is bigger than 0")
    # S<-jags.samples(MR, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
    #S<-c(S$beta[1,,1],S$beta[1,,2],S$beta[1,,3])
    #ECDF<- ecdf(S); 
    #prob[i]<- 1- ECDF(0)
    print(sprintf("%i out of %i models completed", i-1, nrow(db2)))
  }
  
  db2$beta<- beta; #db$prob<- prob
  
  save(db2, file="Data/heat_GD.Rda")
  
  
  ## FFD plot
  db$y<- factor(db$y, levels = c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"))
  db$x<- factor(db$x, levels = c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"))
  
  qplot(x=x, y=y, data=db, fill=beta) + geom_tile()+
    scale_fill_gradient2(limits=c(round(min(db$beta))-1, round(max(db$beta)))+1, low= "green", mid="white", high="red")
  
  ## GD plot
  db2$y<- factor(db2$y, levels = c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"))
  db2$x<- factor(db2$x, levels = c("ORTH", "PHON", "SEM", "UNREL", "PSEUD", "RAN", "X"))
  
  qplot(x=x, y=y, data=db2, fill=beta) + geom_tile()+
    scale_fill_gradient2(limits=c(round(min(db2$beta))-1, round(max(db2$beta)))+1, low= "green", mid="white", high="red")


}