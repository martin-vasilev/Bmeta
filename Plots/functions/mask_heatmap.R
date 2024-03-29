# meta-regression heatmap of parafoveal masks


mask_heatmap<- function(analysis="all"){
  
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
  
  if (analysis=="all"){
    message("Analysing all studies!")
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
      
      G<- gelman.diag(R, confidence=0.95)
      if(G$psrf[1,2]>1.05){
        stop("Model did not converge!")
      }
        
      sum<- summary(R);# sum
      
      beta[i]<- sum$statistics[1,1]
      
      print("Calculating probability difference is bigger than 0")
       S<-jags.samples(MR, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
      S<-c(S$beta[1,,1],S$beta[1,,2],S$beta[1,,3])
      ECDF<- ecdf(S); 
      prob[i]<- 1- ECDF(0)
      print(sprintf("%i out of %i models completed", i, nrow(db)*2))
    }
    
    db$beta<- beta; db$prob<- prob
    
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
                    " masks for ", "GD", sep=""))
      R<- coda.samples(MR, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
      
      G<- gelman.diag(R, confidence=0.95)
      if(G$psrf[1,2]>1.05){
        stop("Model did not converge!")
      }
      
      sum<- summary(R);# sum
      
      beta[i]<- sum$statistics[1,1]
      
      print("Calculating probability difference is bigger than 0")
       S<-jags.samples(MR, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
      S<-c(S$beta[1,,1],S$beta[1,,2],S$beta[1,,3])
      ECDF<- ecdf(S); 
      prob[i]<- 1- ECDF(0)
      print(sprintf("%i out of %i models completed", i+nrow(db), nrow(db2)*2))
    }
    
    db2$beta<- beta; db2$prob<- prob
    
    save(db2, file="Data/heat_GD.Rda")
    message("ALL DONE!")
    
  } else{
    message("Analysing alphabetical-only studies!")
    
    ### FFD
    for (i in 1:nrow(db)){
      txt1<- paste(db$y[i],"_FFD", sep="")
      txt2<- paste(db$x[i],"_FFD", sep="")
      d1<- eval(parse(text= txt1)); d1$cov<- 1
      d2<- eval(parse(text= txt2)); d2$cov=-1
      d1<- subset(d1, Language!= "Chinese")
      d2<- subset(d2, Language!= "Chinese")
      data<- rbind(d1,d2); data<- data[ ,c(3,4,6)]
      
      MR<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(data),
                           "heatmap.txt"), data, n.chains=3, n.adapt=3000, quiet=TRUE)
      message(paste("Creating meta-regression model for ", 
                    paste(db$y[i]), " and ", paste(db$x[i]),
                    " masks for ", "FFD", sep=""))
      R<- coda.samples(MR, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
      
      G<- gelman.diag(R, confidence=0.95)
      if(G$psrf[1,2]>1.05){
        stop("Model did not converge!")
      }
      
      sum<- summary(R);# sum
      
      beta[i]<- sum$statistics[1,1]
      
      print("Calculating probability difference is bigger than 0")
      S<-jags.samples(MR, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
      S<-c(S$beta[1,,1],S$beta[1,,2],S$beta[1,,3])
      ECDF<- ecdf(S); 
      prob[i]<- 1- ECDF(0)
      print(sprintf("%i out of %i models completed", i, nrow(db)*2))
    }
    
    db$beta<- beta; db$prob<- prob
    
    save(db, file="Data/heat_FFD_alpha.Rda")
    
    
    ### GD
    db2<-db
    for (i in 1:nrow(db2)){
      txt1<- paste(db2$y[i],"_GD", sep="")
      txt2<- paste(db2$x[i],"_GD", sep="")
      d1<- eval(parse(text= txt1)); d1$cov<- 1
      d2<- eval(parse(text= txt2)); d2$cov=-1
      d1<- subset(d1, Language!= "Chinese")
      d2<- subset(d2, Language!= "Chinese")
      data<- rbind(d1,d2); data<- data[ ,c(3,4,6)]
      
      MR<- jags.model(JReg("dunif(-200, 200)", "dunif(0, 200)", "dunif(-200, 200)", nrow(data),
                           "heatmap.txt"), data, n.chains=3, n.adapt=3000, quiet=TRUE)
      message(paste("Creating meta-regression model for ", 
                    paste(db2$y[i]), " and ", paste(db2$x[i]),
                    " masks for ", "GD", sep=""))
      R<- coda.samples(MR, c('mu', 'tau', "beta"), n.iter=75000, thin=5)
      
      G<- gelman.diag(R, confidence=0.95)
      if(G$psrf[1,2]>1.05){
        stop("Model did not converge!")
      }
      
      sum<- summary(R);# sum
      
      beta[i]<- sum$statistics[1,1]
      
      print("Calculating probability difference is bigger than 0")
      S<-jags.samples(MR, variable.names='beta', n.iter=75000, thin=5, n.adapt=3000)
      S<-c(S$beta[1,,1],S$beta[1,,2],S$beta[1,,3])
      ECDF<- ecdf(S); 
      prob[i]<- 1- ECDF(0)
      print(sprintf("%i out of %i models completed", i+nrow(db), nrow(db2)*2))
    }
    
    db2$beta<- beta; db2$prob<- prob
    
    save(db2, file="Data/heat_GD_alpha.Rda")
    
    message("ALL DONE!")
  }

}