
# Script used for checking N+1 effect size data
# Martin Vasilev, 2016

# Extract effect sizes for examination (N+1)
rm(list=ls())
source("get_var.R")
source("impute_SD.R")

Impute=TRUE

if(Impute){ # for data analysis
  load("Data/ES_N1.Rda")
  load("Data/dataN1.Rda")
  dataN1<- impute_SD(dataN1)
  
} else{ # for funnel plots

  load("Data/ES_N1.Rda")
  load("Data/dataN1.Rda")
}

#----------------------
# Random letter string:
#----------------------
    #FFD:
RAN_FFD<- subset(ES_N1, FFD_RAN_ms!="NA")
RAN_FFD<- RAN_FFD[,1:4]; RAN_FFD[, 3]<- NULL 
d1<- subset(dataN1, dataN1$FFD_N1_inval_RAN!="NA")
RAN_FFD$S.sqr<-get_var(d1$FFD_N1_val_SD, d1$FFD_N1_inval_RAN_SD, d1$N)
RAN_FFD$N<- d1$N
colnames(RAN_FFD)<- c("Paper", "Language", "T", "S.sqr", "N")

    #SFD:
RAN_SFD<- subset(ES_N1, SFD_RAN_ms!="NA")
RAN_SFD<- RAN_SFD[,1:36]; RAN_SFD<-RAN_SFD[,-c(3:35)]
d1<- subset(dataN1, dataN1$SFD_N1_inval_RAN!="NA")
RAN_SFD$S.sqr<-get_var(d1$SFD_N1_val_SD, d1$SFD_N1_inval_RAN_SD, d1$N)
RAN_SFD$N<- d1$N
colnames(RAN_SFD)<- c("Paper", "Language", "T", "S.sqr", "N")

    #GD:
RAN_GD<- subset(ES_N1, GD_RAN_ms!="NA")
RAN_GD<- RAN_GD[,1:20]; RAN_GD<-RAN_GD[,-c(3:19)]
d1<- subset(dataN1, dataN1$GD_N1_inval_RAN!="NA")
RAN_GD$S.sqr<-get_var(d1$GD_N1_val_SD, d1$GD_N1_inval_RAN_SD, d1$N)
RAN_GD$N<- d1$N
colnames(RAN_GD)<- c("Paper", "Language", "T", "S.sqr", "N")

    #TVT:
RAN_TVT<- subset(ES_N1, Total_RAN_ms!="NA")
RAN_TVT<- RAN_TVT[,1:52]; RAN_TVT<-RAN_TVT[,-c(3:51)]
d1<- subset(dataN1, dataN1$Total_N1_inval_RAN!="NA")
RAN_TVT$S.sqr<-get_var(d1$Total_N1_val_SD, d1$Total_N1_inval_RAN_SD, d1$N)
RAN_TVT$N<- d1$N
colnames(RAN_TVT)<- c("Paper", "Language", "T", "S.sqr", "N")

save(RAN_FFD, file="Data/RAN_FFD.Rda")
save(RAN_SFD, file="Data/RAN_SFD.Rda")
save(RAN_GD, file="Data/RAN_GD.Rda")
save(RAN_TVT, file="Data/RAN_TVT.Rda")


#----------------
# Unrelated word:
#----------------

   #FFD:
UNREL_FFD<- subset(ES_N1, FFD_UNREL_ms!="NA")
UNREL_FFD<- UNREL_FFD[,1:7]; UNREL_FFD<- UNREL_FFD[, -c(3:6)] 
d1<- subset(dataN1, dataN1$FFD_N1_inval_UNREL!="NA")
UNREL_FFD$S.sqr<-get_var(d1$FFD_N1_val_SD, d1$FFD_N1_inval_UNREL_SD, d1$N)
UNREL_FFD$N<- d1$N
colnames(UNREL_FFD)<- c("Paper", "Language", "T", "S.sqr", "N")

   #SFD:
UNREL_SFD<- subset(ES_N1, SFD_UNREL_ms!="NA")
UNREL_SFD<- UNREL_SFD[,1:39]; UNREL_SFD<-UNREL_SFD[,-c(3:38)]
d1<- subset(dataN1, dataN1$SFD_N1_inval_UNREL!="NA")
UNREL_SFD$S.sqr<-get_var(d1$SFD_N1_val_SD, d1$SFD_N1_inval_UNREL_SD, d1$N)
UNREL_SFD$N<- d1$N
colnames(UNREL_SFD)<- c("Paper", "Language", "T", "S.sqr", "N")

   #GD:
UNREL_GD<- subset(ES_N1, GD_UNREL_ms!="NA")
UNREL_GD<- UNREL_GD[,1:23]; UNREL_GD<- UNREL_GD[,-c(3:22)]
d1<- subset(dataN1, dataN1$GD_N1_inval_UNREL!="NA")
UNREL_GD$S.sqr<-get_var(d1$GD_N1_val_SD, d1$GD_N1_inval_UNREL_SD, d1$N)
UNREL_GD$N<- d1$N
colnames(UNREL_GD)<- c("Paper", "Language", "T", "S.sqr", "N")

   #TVT:
UNREL_TVT<- subset(ES_N1, Total_UNREL_ms!="NA")
UNREL_TVT<- UNREL_TVT[,1:55]; UNREL_TVT<- UNREL_TVT[,-c(3:54)]
d1<- subset(dataN1, dataN1$Total_N1_inval_UNREL!="NA")
UNREL_TVT$S.sqr<-get_var(d1$Total_N1_val_SD, d1$Total_N1_inval_UNREL_SD, d1$N)
UNREL_TVT$N<- d1$N
colnames(UNREL_TVT)<- c("Paper", "Language", "T", "S.sqr", "N")

save(UNREL_FFD, file="Data/UNREL_FFD.Rda")
save(UNREL_SFD, file="Data/UNREL_SFD.Rda")
save(UNREL_GD, file="Data/UNREL_GD.Rda")
save(UNREL_TVT, file="Data/UNREL_TVT.Rda")


#----------------
#     X mask:
#----------------
   #FFD:
X_FFD<- subset(ES_N1, FFD_X_ms!="NA")
X_FFD<- X_FFD[,1:5]; X_FFD <-X_FFD[, -c(3:4)] 
d1<- subset(dataN1, dataN1$FFD_N1_inval_X!="NA")
X_FFD$S.sqr<- get_var(d1$FFD_N1_val_SD, d1$FFD_N1_inval_X_SD, d1$N)
X_FFD$N<- d1$N
colnames(X_FFD)<- c("Paper", "Language", "T", "S.sqr", "N")

   #SFD:
X_SFD<- subset(ES_N1, SFD_X_ms!="NA")
X_SFD<- X_SFD[,1:37]; X_SFD<- X_SFD[,-c(3:36)]
d1<- subset(dataN1, dataN1$SFD_N1_inval_X!="NA")
X_SFD$S.sqr<-get_var(d1$SFD_N1_val_SD, d1$SFD_N1_inval_X_SD, d1$N)
X_SFD$N<- d1$N
colnames(X_SFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#GD:
X_GD<- subset(ES_N1, GD_X_ms!="NA")
X_GD<- X_GD[,1:21]; X_GD<- X_GD[,-c(3:20)]
d1<- subset(dataN1, dataN1$GD_N1_inval_X!="NA")
X_GD$S.sqr<-get_var(d1$GD_N1_val_SD, d1$GD_N1_inval_X_SD, d1$N)
X_GD$N<- d1$N
colnames(X_GD)<- c("Paper", "Language", "T", "S.sqr", "N")

#TVT: No data

save(X_FFD, file="Data/X_FFD.Rda")
save(X_SFD, file="Data/X_SFD.Rda")
save(X_GD, file="Data/X_GD.Rda")


#----------------
#  Pseudoword:
#----------------

#FFD:
PSEUD_FFD<- subset(ES_N1, FFD_PSEUD_ms!="NA")
PSEUD_FFD<- PSEUD_FFD[,1:10]; PSEUD_FFD<- PSEUD_FFD[, -c(3:9)] 
d1<- subset(dataN1, dataN1$FFD_N1_inval_PSEUD!="NA")
PSEUD_FFD$S.sqr<-get_var(d1$FFD_N1_val_SD, d1$FFD_N1_inval_PSEUD_SD, d1$N)
PSEUD_FFD$N<- d1$N
colnames(PSEUD_FFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#SFD:
PSEUD_SFD<- subset(ES_N1, SFD_PSEUD_ms!="NA")
PSEUD_SFD<- PSEUD_SFD[,1:42]; PSEUD_SFD<-PSEUD_SFD[,-c(3:41)]
d1<- subset(dataN1, dataN1$SFD_N1_inval_PSEUD!="NA")
PSEUD_SFD$S.sqr<- get_var(d1$SFD_N1_val_SD, d1$SFD_N1_inval_PSEUD_SD, d1$N)
PSEUD_SFD$N<- d1$N
colnames(PSEUD_SFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#GD:
PSEUD_GD<- subset(ES_N1, GD_PSEUD_ms!="NA")
PSEUD_GD<- PSEUD_GD[,1:26]; PSEUD_GD<- PSEUD_GD[,-c(3:25)]
d1<- subset(dataN1, dataN1$GD_N1_inval_PSEUD!="NA")
PSEUD_GD$S.sqr<- get_var(d1$GD_N1_val_SD, d1$GD_N1_inval_PSEUD_SD, d1$N)
PSEUD_GD$N<- d1$N
colnames(PSEUD_GD)<- c("Paper", "Language", "T", "S.sqr", "N")

#TVT:
PSEUD_TVT<- subset(ES_N1, Total_PSEUD_ms!="NA")
PSEUD_TVT<- PSEUD_TVT[,1:58]; PSEUD_TVT<- PSEUD_TVT[,-c(3:57)]
d1<- subset(dataN1, dataN1$Total_N1_inval_PSEUD!="NA")
PSEUD_TVT$S.sqr<-get_var(d1$Total_N1_val_SD, d1$Total_N1_inval_PSEUD_SD, d1$N)
PSEUD_TVT$N<- d1$N
colnames(PSEUD_TVT)<- c("Paper", "Language", "T", "S.sqr", "N")

save(PSEUD_FFD, file="Data/PSEUD_FFD.Rda")
save(PSEUD_SFD, file="Data/PSEUD_SFD.Rda")
save(PSEUD_GD, file="Data/PSEUD_GD.Rda")
save(PSEUD_TVT, file="Data/PSEUD_TVT.Rda")

#----------------------
#  Orthographical mask:
#----------------------

#FFD:
ORTH_FFD<- subset(ES_N1, FFD_ORTH_ms!="NA")
ORTH_FFD<- ORTH_FFD[,1:6]; ORTH_FFD<- ORTH_FFD[, -c(3:5)] 
d1<- subset(dataN1, dataN1$FFD_N1_inval_ORTH!="NA")
ORTH_FFD$S.sqr<-get_var(d1$FFD_N1_val_SD, d1$FFD_N1_inval_ORTH_SD, d1$N)
ORTH_FFD$N<- d1$N
colnames(ORTH_FFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#SFD:
ORTH_SFD<- subset(ES_N1, SFD_ORTH_ms!="NA")
ORTH_SFD<- ORTH_SFD[,1:38]; ORTH_SFD<-ORTH_SFD[,-c(3:37)]
d1<- subset(dataN1, dataN1$SFD_N1_inval_ORTH!="NA")
ORTH_SFD$S.sqr<-get_var(d1$SFD_N1_val_SD, d1$SFD_N1_inval_ORTH_SD, d1$N)
ORTH_SFD$N<- d1$N
colnames(ORTH_SFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#GD:
ORTH_GD<- subset(ES_N1, GD_ORTH_ms!="NA")
ORTH_GD<- ORTH_GD[,1:22]; ORTH_GD<- ORTH_GD[,-c(3:21)]
d1<- subset(dataN1, dataN1$GD_N1_inval_ORTH!="NA")
ORTH_GD$S.sqr<-get_var(d1$GD_N1_val_SD, d1$GD_N1_inval_ORTH_SD, d1$N)
ORTH_GD$N<- d1$N
colnames(ORTH_GD)<- c("Paper", "Language", "T", "S.sqr", "N")

#TVT:
ORTH_TVT<- subset(ES_N1, Total_ORTH_ms!="NA")
ORTH_TVT<- ORTH_TVT[,1:54]; ORTH_TVT<- ORTH_TVT[,-c(3:53)]
d1<- subset(dataN1, dataN1$Total_N1_inval_ORTH!="NA")
ORTH_TVT$S.sqr<-get_var(d1$Total_N1_val_SD, d1$Total_N1_inval_ORTH_SD, d1$N)
ORTH_TVT$N<- d1$N
colnames(ORTH_TVT)<- c("Paper", "Language", "T", "S.sqr", "N")

save(ORTH_FFD, file="Data/ORTH_FFD.Rda")
save(ORTH_SFD, file="Data/ORTH_SFD.Rda")
save(ORTH_GD, file="Data/ORTH_GD.Rda")
save(ORTH_TVT, file="Data/ORTH_TVT.Rda")


#----------------------
#    Semantic mask:
#----------------------

#FFD:
SEM_FFD<- subset(ES_N1, FFD_SEM_ms!="NA")
SEM_FFD<- SEM_FFD[,1:8]; SEM_FFD<- SEM_FFD[, -c(3:7)] 
d1<- subset(dataN1, dataN1$FFD_N1_inval_SEM!="NA")
SEM_FFD$S.sqr<-get_var(d1$FFD_N1_val_SD, d1$FFD_N1_inval_SEM_SD, d1$N)
SEM_FFD$N<- d1$N
colnames(SEM_FFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#SFD:
SEM_SFD<- subset(ES_N1, SFD_SEM_ms!="NA")
SEM_SFD<- SEM_SFD[,1:40]; SEM_SFD<-SEM_SFD[,-c(3:39)]
d1<- subset(dataN1, dataN1$SFD_N1_inval_SEM!="NA")
SEM_SFD$S.sqr<-get_var(d1$SFD_N1_val_SD, d1$SFD_N1_inval_SEM_SD, d1$N)
SEM_SFD$N<- d1$N
colnames(SEM_SFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#GD:
SEM_GD<- subset(ES_N1, GD_SEM_ms!="NA")
SEM_GD<- SEM_GD[,1:24]; SEM_GD<- SEM_GD[,-c(3:23)]
d1<- subset(dataN1, dataN1$GD_N1_inval_SEM!="NA")
SEM_GD$S.sqr<-get_var(d1$GD_N1_val_SD, d1$GD_N1_inval_SEM_SD, d1$N)
SEM_GD$N<- d1$N
colnames(SEM_GD)<- c("Paper", "Language", "T", "S.sqr", "N")

#TVT:
SEM_TVT<- subset(ES_N1, Total_SEM_ms!="NA")
SEM_TVT<- SEM_TVT[,1:56]; SEM_TVT<- SEM_TVT[,-c(3:55)]
d1<- subset(dataN1, dataN1$Total_N1_inval_SEM!="NA")
SEM_TVT$S.sqr<-get_var(d1$Total_N1_val_SD, d1$Total_N1_inval_SEM_SD, d1$N)
SEM_TVT$N<- d1$N
colnames(SEM_TVT)<- c("Paper", "Language", "T", "S.sqr", "N")

save(SEM_FFD, file="Data/SEM_FFD.Rda")
save(SEM_SFD, file="Data/SEM_SFD.Rda")
save(SEM_GD, file="Data/SEM_GD.Rda")
save(SEM_TVT, file="Data/SEM_TVT.Rda")

#----------------------
#    Phonological mask:
#----------------------

#FFD:
PHON_FFD<- subset(ES_N1, FFD_PHON_ms!="NA")
PHON_FFD<- PHON_FFD[,1:9]; PHON_FFD<- PHON_FFD[, -c(3:8)] 
d1<- subset(dataN1, dataN1$FFD_N1_inval_PHON!="NA")
PHON_FFD$S.sqr<-get_var(d1$FFD_N1_val_SD, d1$FFD_N1_inval_PHON_SD, d1$N)
PHON_FFD$N<- d1$N
colnames(PHON_FFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#SFD:
PHON_SFD<- subset(ES_N1, SFD_PHON_ms!="NA")
PHON_SFD<- PHON_SFD[,1:41]; PHON_SFD<-PHON_SFD[,-c(3:40)]
d1<- subset(dataN1, dataN1$SFD_N1_inval_PHON!="NA")
PHON_SFD$S.sqr<-get_var(d1$SFD_N1_val_SD, d1$SFD_N1_inval_PHON_SD, d1$N)
PHON_SFD$N<- d1$N
colnames(PHON_SFD)<- c("Paper", "Language", "T", "S.sqr", "N")

#GD:
PHON_GD<- subset(ES_N1, GD_PHON_ms!="NA")
PHON_GD<- PHON_GD[,1:25]; PHON_GD<- PHON_GD[,-c(3:24)]
d1<- subset(dataN1, dataN1$GD_N1_inval_PHON!="NA")
PHON_GD$S.sqr<-get_var(d1$GD_N1_val_SD, d1$GD_N1_inval_PHON_SD, d1$N)
PHON_GD$N<- d1$N
colnames(PHON_GD)<- c("Paper", "Language", "T", "S.sqr", "N")

#TVT:
PHON_TVT<- subset(ES_N1, Total_PHON_ms!="NA")
PHON_TVT<- PHON_TVT[,1:57]; PHON_TVT<- PHON_TVT[,-c(3:56)]
d1<- subset(dataN1, dataN1$Total_N1_inval_PHON!="NA")
PHON_TVT$S.sqr<-get_var(d1$Total_N1_val_SD, d1$Total_N1_inval_PHON_SD, d1$N)
PHON_TVT$N<- d1$N
colnames(PHON_TVT)<- c("Paper", "Language", "T", "S.sqr", "N")

save(PHON_FFD, file="Data/PHON_FFD.Rda")
save(PHON_SFD, file="Data/PHON_SFD.Rda")
save(PHON_GD, file="Data/PHON_GD.Rda")
save(PHON_TVT, file="Data/PHON_TVT.Rda")


## Calculate the "ultimate" N+1 effect:

# It's necessary to calculate it for each measure due to the fact that
# different studies report different measures.

# FFD:
RAN_FFD$Paper<- as.character(RAN_FFD$Paper)
UNREL_FFD$Paper<- as.character(UNREL_FFD$Paper)
X_FFD$Paper<- as.character(X_FFD$Paper)
PSEUD_FFD$Paper<- as.character(PSEUD_FFD$Paper)

n<- c(RAN_FFD$Paper, UNREL_FFD$Paper, X_FFD$Paper, PSEUD_FFD$Paper)
out<- n[duplicated(n)]
u<-n

for(i in 1:length(u)){
  if(is.element(n[i], out)){
    u[i]<- NA
  }
}

u<- u[which(u!="NA")]

T<-NULL
S.sqr<- NULL
N<-NULL
Paper<- NULL
Language<- NULL
a<- NULL # placeholder for finding the row number of study i

for(i in 1:length(u)){ # get the ES for studies with only one type of mask
  
   Paper[i]<- u[i]  
   a<- which(dataN1$Paper== u[i])
   N[i]<-dataN1$N[a]
   Language[i]<- as.character(dataN1$Language[a]) 
   
   if(is.na(ES_N1$FFD_RAN_ms[a])==FALSE){
     T[i]<- ES_N1$FFD_RAN_ms[a]
     S.sqr[i]<- get_var(dataN1$FFD_N1_val_SD[a], dataN1$FFD_N1_inval_RAN_SD[a], dataN1$N[a])
     print("                        "); print(u[i]); print("RAN"); print(T[i])
   }
  
   if(is.na(ES_N1$FFD_UNREL_ms[a])==FALSE){
     T[i]<- ES_N1$FFD_UNREL_ms[a]
     S.sqr[i]<- get_var(dataN1$FFD_N1_val_SD[a], dataN1$FFD_N1_inval_UNREL_SD[a], dataN1$N[a])
     print("                        "); print(u[i]); print("UNREL"); print(T[i])
   }  
    
   if(is.na(ES_N1$FFD_X_ms[a])==FALSE){
     T[i]<- ES_N1$FFD_X_ms[a]
     S.sqr[i]<- get_var(dataN1$FFD_N1_val_SD[a], dataN1$FFD_N1_inval_X_SD[a], dataN1$N[a])
     print("                        "); print(u[i]); print("X"); print(T[i])
   } 
   
   if(is.na(ES_N1$FFD_PSEUD_ms[a])==FALSE){
     T[i]<- ES_N1$FFD_PSEUD_ms[a]
     S.sqr[i]<- get_var(dataN1$FFD_N1_val_SD[a], dataN1$FFD_N1_inval_PSEUD_SD[a], dataN1$N[a])
     print("                        "); print(u[i]); print("PSEUD"); print(T[i])
   } 
}

data5<- data.frame(Paper, Language, T, S.sqr, N)

# Take the grand average of the remaining studies:
T<-NULL;T1<-NULL; T2<-NULL; T3<-NULL; T4<-NULL
S.sqr<- NULL; S.sqr2<- NULL; S.sqr3<- NULL; S.sqr4<- NULL;
Paper<- NULL; Language<- NULL; N<-NULL

u<- n[duplicated(n)]

for(i in 1:length(u)){
  
  Paper[i]<- u[i]  
  a<- which(dataN1$Paper== u[i])
  N[i]<-dataN1$N[a]
  Language[i]<- as.character(dataN1$Language[a]) 
  
  # effect size
  T1<- ES_N1$FFD_RAN_ms[a]
  T2<- ES_N1$FFD_UNREL_ms[a]
  T3<- ES_N1$FFD_X_ms[a]
  T4<- ES_N1$FFD_PSEUD_ms[a]
  if(is.na(T1)){
    T1<-NA
  }
  if(is.na(T2)){
    T2<-NA
  }
  if(is.na(T3)){
    T3<-NA
  }
  if(is.na(T4)){
    T4<-NA
  }
  
  T[i]<- mean(na.omit(c(T1, T2, T3, T4)))
  
  # variance
  
  S.sqr1<- get_var(dataN1$FFD_N1_val_SD[a], dataN1$FFD_N1_inval_RAN_SD[a], dataN1$N[a])
  S.sqr2<- get_var(dataN1$FFD_N1_val_SD[a], dataN1$FFD_N1_inval_UNREL_SD[a], dataN1$N[a])
  S.sqr3<- get_var(dataN1$FFD_N1_val_SD[a], dataN1$FFD_N1_inval_PSEUD_SD[a], dataN1$N[a])
  S.sqr4<- get_var(dataN1$FFD_N1_val_SD[a], dataN1$FFD_N1_inval_X_SD[a], dataN1$N[a])
  
  if(is.na(S.sqr1)){
    S.sqr1<-NA
  }
  if(is.na(S.sqr2)){
    S.sqr2<-NA
  }
  if(is.na(S.sqr3)){
    S.sqr3<-NA
  }
  if(is.na(S.sqr4)){
    S.sqr4<-NA
  }
  
  S.sqr[i]<- mean(na.omit(c(S.sqr1, S.sqr2, S.sqr3, S.sqr4)))
  
}

data5b<- data.frame(Paper, Language, T, S.sqr, N)

data5<- rbind(data5, data5b)

save(data5, file="Data/data5.Rda")


# GD:
RAN_GD$Paper<- as.character(RAN_GD$Paper)
UNREL_GD$Paper<- as.character(UNREL_GD$Paper)
X_GD$Paper<- as.character(X_GD$Paper)
PSEUD_GD$Paper<- as.character(PSEUD_GD$Paper)

n<- c(RAN_GD$Paper, UNREL_GD$Paper, X_GD$Paper, PSEUD_GD$Paper)
out<- n[duplicated(n)]
u<-n

for(i in 1:length(u)){
  if(is.element(n[i], out)){
    u[i]<- NA
  }
}

u<- u[which(u!="NA")]

T<-NULL
S.sqr<- NULL
N<-NULL
Paper<- NULL
Language<- NULL
a<- NULL # placeholder for finding the row number of study i

for(i in 1:length(u)){ # get the ES for studies with only one type of mask
  
  Paper[i]<- u[i]  
  a<- which(dataN1$Paper== u[i])
  N[i]<-dataN1$N[a]
  Language[i]<- as.character(dataN1$Language[a]) 
  
  if(is.na(ES_N1$GD_RAN_ms[a])==FALSE){
    T[i]<- ES_N1$GD_RAN_ms[a]
    S.sqr[i]<- get_var(dataN1$GD_N1_val_SD[a], dataN1$GD_N1_inval_RAN_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("RAN"); print(T[i])
  }
  
  if(is.na(ES_N1$GD_UNREL_ms[a])==FALSE){
    T[i]<- ES_N1$GD_UNREL_ms[a]
    S.sqr[i]<- get_var(dataN1$GD_N1_val_SD[a], dataN1$GD_N1_inval_UNREL_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("UNREL"); print(T[i])
  }  
  
  if(is.na(ES_N1$GD_X_ms[a])==FALSE){
    T[i]<- ES_N1$GD_X_ms[a]
    S.sqr[i]<- get_var(dataN1$GD_N1_val_SD[a], dataN1$GD_N1_inval_X_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("X"); print(T[i])
  } 
  
  if(is.na(ES_N1$GD_PSEUD_ms[a])==FALSE){
    T[i]<- ES_N1$GD_PSEUD_ms[a]
    S.sqr[i]<- get_var(dataN1$GD_N1_val_SD[a], dataN1$GD_N1_inval_PSEUD_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("PSEUD"); print(T[i])
  } 
}

data6<- data.frame(Paper, Language, T, S.sqr, N)

# Take the grand average of the remaining studies:
T<-NULL;T1<-NULL; T2<-NULL; T3<-NULL; T4<-NULL
S.sqr<- NULL; S.sqr2<- NULL; S.sqr3<- NULL; S.sqr4<- NULL;
Paper<- NULL; Language<- NULL; N<-NULL

u<- n[duplicated(n)]

for(i in 1:length(u)){
  
  Paper[i]<- u[i]  
  a<- which(dataN1$Paper== u[i])
  N[i]<-dataN1$N[a]
  Language[i]<- as.character(dataN1$Language[a]) 
  
  # effect size
  T1<- ES_N1$GD_RAN_ms[a]
  T2<- ES_N1$GD_UNREL_ms[a]
  T3<- ES_N1$GD_X_ms[a]
  T4<- ES_N1$GD_PSEUD_ms[a]
  if(is.na(T1)){
    T1<-NA
  }
  if(is.na(T2)){
    T2<-NA
  }
  if(is.na(T3)){
    T3<-NA
  }
  if(is.na(T4)){
    T4<-NA
  }
  
  T[i]<- mean(na.omit(c(T1, T2, T3, T4)))
  
  # variance
  
  S.sqr1<- get_var(dataN1$GD_N1_val_SD[a], dataN1$GD_N1_inval_RAN_SD[a], dataN1$N[a])
  S.sqr2<- get_var(dataN1$GD_N1_val_SD[a], dataN1$GD_N1_inval_UNREL_SD[a], dataN1$N[a])
  S.sqr3<- get_var(dataN1$GD_N1_val_SD[a], dataN1$GD_N1_inval_PSEUD_SD[a], dataN1$N[a])
  S.sqr4<- get_var(dataN1$GD_N1_val_SD[a], dataN1$GD_N1_inval_X_SD[a], dataN1$N[a])
  
  if(is.na(S.sqr1)){
    S.sqr1<-NA
  }
  if(is.na(S.sqr2)){
    S.sqr2<-NA
  }
  if(is.na(S.sqr3)){
    S.sqr3<-NA
  }
  if(is.na(S.sqr4)){
    S.sqr4<-NA
  }
  
  S.sqr[i]<- mean(na.omit(c(S.sqr1, S.sqr2, S.sqr3, S.sqr4)))
  
}

data6b<- data.frame(Paper, Language, T, S.sqr, N)

data6<- rbind(data6, data6b)

save(data6, file="Data/data6.Rda")




# SFD:
RAN_SFD$Paper<- as.character(RAN_SFD$Paper)
UNREL_SFD$Paper<- as.character(UNREL_SFD$Paper)
X_SFD$Paper<- as.character(X_SFD$Paper)
PSEUD_SFD$Paper<- as.character(PSEUD_SFD$Paper)

n<- c(RAN_SFD$Paper, UNREL_SFD$Paper, X_SFD$Paper, PSEUD_SFD$Paper)
out<- n[duplicated(n)]
u<-n

for(i in 1:length(u)){
  if(is.element(n[i], out)){
    u[i]<- NA
  }
}

u<- u[which(u!="NA")]

T<-NULL
S.sqr<- NULL
N<-NULL
Paper<- NULL
Language<- NULL
a<- NULL # placeholder for finding the row number of study i

for(i in 1:length(u)){ # get the ES for studies with only one type of mask
  
  Paper[i]<- u[i]  
  a<- which(dataN1$Paper== u[i])
  N[i]<-dataN1$N[a]
  Language[i]<- as.character(dataN1$Language[a]) 
  
  if(is.na(ES_N1$SFD_RAN_ms[a])==FALSE){
    T[i]<- ES_N1$SFD_RAN_ms[a]
    S.sqr[i]<- get_var(dataN1$SFD_N1_val_SD[a], dataN1$SFD_N1_inval_RAN_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("RAN"); print(T[i])
  }
  
  if(is.na(ES_N1$SFD_UNREL_ms[a])==FALSE){
    T[i]<- ES_N1$SFD_UNREL_ms[a]
    S.sqr[i]<- get_var(dataN1$SFD_N1_val_SD[a], dataN1$SFD_N1_inval_UNREL_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("UNREL"); print(T[i])
  }  
  
  if(is.na(ES_N1$SFD_X_ms[a])==FALSE){
    T[i]<- ES_N1$SFD_X_ms[a]
    S.sqr[i]<- get_var(dataN1$SFD_N1_val_SD[a], dataN1$SFD_N1_inval_X_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("X"); print(T[i])
  } 
  
  if(is.na(ES_N1$SFD_PSEUD_ms[a])==FALSE){
    T[i]<- ES_N1$SFD_PSEUD_ms[a]
    S.sqr[i]<- get_var(dataN1$SFD_N1_val_SD[a], dataN1$SFD_N1_inval_PSEUD_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("PSEUD"); print(T[i])
  } 
}

data7<- data.frame(Paper, Language, T, S.sqr, N)

# Take the grand average of the remaining studies:
T<-NULL;T1<-NULL; T2<-NULL; T3<-NULL; T4<-NULL
S.sqr<- NULL; S.sqr2<- NULL; S.sqr3<- NULL; S.sqr4<- NULL;
Paper<- NULL; Language<- NULL; N<-NULL

u<- n[duplicated(n)]

for(i in 1:length(u)){
  
  Paper[i]<- u[i]  
  a<- which(dataN1$Paper== u[i])
  N[i]<-dataN1$N[a]
  Language[i]<- as.character(dataN1$Language[a]) 
  
  # effect size
  T1<- ES_N1$SFD_RAN_ms[a]
  T2<- ES_N1$SFD_UNREL_ms[a]
  T3<- ES_N1$SFD_X_ms[a]
  T4<- ES_N1$SFD_PSEUD_ms[a]
  if(is.na(T1)){
    T1<-NA
  }
  if(is.na(T2)){
    T2<-NA
  }
  if(is.na(T3)){
    T3<-NA
  }
  if(is.na(T4)){
    T4<-NA
  }
  
  T[i]<- mean(na.omit(c(T1, T2, T3, T4)))
  
  # variance
  
  S.sqr1<- get_var(dataN1$SFD_N1_val_SD[a], dataN1$SFD_N1_inval_RAN_SD[a], dataN1$N[a])
  S.sqr2<- get_var(dataN1$SFD_N1_val_SD[a], dataN1$SFD_N1_inval_UNREL_SD[a], dataN1$N[a])
  S.sqr3<- get_var(dataN1$SFD_N1_val_SD[a], dataN1$SFD_N1_inval_PSEUD_SD[a], dataN1$N[a])
  S.sqr4<- get_var(dataN1$SFD_N1_val_SD[a], dataN1$SFD_N1_inval_X_SD[a], dataN1$N[a])
  
  if(is.na(S.sqr1)){
    S.sqr1<-NA
  }
  if(is.na(S.sqr2)){
    S.sqr2<-NA
  }
  if(is.na(S.sqr3)){
    S.sqr3<-NA
  }
  if(is.na(S.sqr4)){
    S.sqr4<-NA
  }
  
  S.sqr[i]<- mean(na.omit(c(S.sqr1, S.sqr2, S.sqr3, S.sqr4)))
  
}

data7b<- data.frame(Paper, Language, T, S.sqr, N)

data7<- rbind(data7, data7b)

save(data7, file="Data/data7.Rda")



# TVT:
RAN_TVT$Paper<- as.character(RAN_TVT$Paper)
UNREL_TVT$Paper<- as.character(UNREL_TVT$Paper)
PSEUD_TVT$Paper<- as.character(PSEUD_TVT$Paper)
n<- c(RAN_TVT$Paper, UNREL_TVT$Paper, PSEUD_TVT$Paper)
out<- n[duplicated(n)]
u<-n

for(i in 1:length(u)){
  if(is.element(n[i], out)){
    u[i]<- NA
  }
}

u<- u[which(u!="NA")]

T<-NULL
S.sqr<- NULL
N<-NULL
Paper<- NULL
Language<- NULL
a<- NULL # placeholder for finding the row number of study i

for(i in 1:length(u)){ # get the ES for studies with only one type of mask
  
  Paper[i]<- u[i]  
  a<- which(dataN1$Paper== u[i])
  N[i]<-dataN1$N[a]
  Language[i]<- as.character(dataN1$Language[a]) 
  
  if(is.na(ES_N1$Total_RAN_ms[a])==FALSE){
    T[i]<- ES_N1$Total_RAN_ms[a]
    S.sqr[i]<- get_var(dataN1$Total_N1_val_SD[a], dataN1$Total_N1_inval_RAN_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("RAN"); print(T[i])
  }
  
  if(is.na(ES_N1$Total_UNREL_ms[a])==FALSE){
    T[i]<- ES_N1$Total_UNREL_ms[a]
    S.sqr[i]<- get_var(dataN1$Total_N1_val_SD[a], dataN1$Total_N1_inval_UNREL_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("UNREL"); print(T[i])
  }  
  
  if(is.na(ES_N1$Total_X_ms[a])==FALSE){
    T[i]<- ES_N1$Total_X_ms[a]
    S.sqr[i]<- get_var(dataN1$Total_N1_val_SD[a], dataN1$Total_N1_inval_X_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("X"); print(T[i])
  } 
  
  if(is.na(ES_N1$Total_PSEUD_ms[a])==FALSE){
    T[i]<- ES_N1$Total_PSEUD_ms[a]
    S.sqr[i]<- get_var(dataN1$Total_N1_val_SD[a], dataN1$Total_N1_inval_PSEUD_SD[a], dataN1$N[a])
    print("                        "); print(u[i]); print("PSEUD"); print(T[i])
  } 
}

data8<- data.frame(Paper, Language, T, S.sqr, N)

# Take the grand average of the remaining studies:
T<-NULL;T1<-NULL; T2<-NULL; T3<-NULL; T4<-NULL
S.sqr<- NULL; S.sqr2<- NULL; S.sqr3<- NULL; S.sqr4<- NULL;
Paper<- NULL; Language<- NULL; N<-NULL

u<- n[duplicated(n)]

for(i in 1:length(u)){
  
  Paper[i]<- u[i]  
  a<- which(dataN1$Paper== u[i])
  N[i]<-dataN1$N[a]
  Language[i]<- as.character(dataN1$Language[a]) 
  
  # effect size
  T1<- ES_N1$Total_RAN_ms[a]
  T2<- ES_N1$Total_UNREL_ms[a]
  T3<- ES_N1$Total_X_ms[a]
  T4<- ES_N1$Total_PSEUD_ms[a]
  if(is.na(T1)){
    T1<-NA
  }
  if(is.na(T2)){
    T2<-NA
  }
  if(is.na(T3)){
    T3<-NA
  }
  if(is.na(T4)){
    T4<-NA
  }
  
  T[i]<- mean(na.omit(c(T1, T2, T3, T4)))
  
  # variance
  
  S.sqr1<- get_var(dataN1$Total_N1_val_SD[a], dataN1$Total_N1_inval_RAN_SD[a], dataN1$N[a])
  S.sqr2<- get_var(dataN1$Total_N1_val_SD[a], dataN1$Total_N1_inval_UNREL_SD[a], dataN1$N[a])
  S.sqr3<- get_var(dataN1$Total_N1_val_SD[a], dataN1$Total_N1_inval_PSEUD_SD[a], dataN1$N[a])
  S.sqr4<- get_var(dataN1$Total_N1_val_SD[a], dataN1$Total_N1_inval_X_SD[a], dataN1$N[a])
  
  if(is.na(S.sqr1)){
    S.sqr1<-NA
  }
  if(is.na(S.sqr2)){
    S.sqr2<-NA
  }
  if(is.na(S.sqr3)){
    S.sqr3<-NA
  }
  if(is.na(S.sqr4)){
    S.sqr4<-NA
  }
  
  S.sqr[i]<- mean(na.omit(c(S.sqr1, S.sqr2, S.sqr3, S.sqr4)))
  
}

data8b<- data.frame(Paper, Language, T, S.sqr, N)

data8<- rbind(data8, data8b)

save(data8, file="Data/data8.Rda")

