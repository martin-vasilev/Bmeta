
# Martin R. Vasilev, 2016

# Creates a facilitation vs interefenence graph 

# I use only FFD and GD as these are the most commonly-reported measures 

rm(list=ls())

#ls()[which(ls()!= "sum9")]

#####################
# create data frame # 
#####################

### RAN
load("Summaries/N1/sum1.Rda")
M1_meanP<- sum1$statistics[1,1] # Pooled mean
M1_MuCrI<- c(sum1$quantiles[1,1], sum1$quantiles[1,5])
M_RAN_FFD= M1_meanP; M_RAN_FFD_CrI= M1_MuCrI

load("Summaries/N1/sum3.Rda")
M3_meanP<- sum3$statistics[1,1] # Pooled mean
M3_MuCrI<- c(sum3$quantiles[1,1], sum3$quantiles[1,5])
M_RAN_GD= M3_meanP; M_RAN_GD_CrI= M3_MuCrI

### UNREL
load("Summaries/N1/sum5.Rda")
M5_meanP<- sum5$statistics[1,1] # Pooled mean
M5_MuCrI<- c(sum5$quantiles[1,1], sum5$quantiles[1,5])
M_UNREL_FFD= M5_meanP; M_UNREL_FFD_CrI= M5_MuCrI

load("Summaries/N1/sum7.Rda")
M7_meanP<- sum7$statistics[1,1] # Pooled mean
M7_MuCrI<- c(sum7$quantiles[1,1], sum7$quantiles[1,5])
M_UNREL_GD= M7_meanP; M_UNREL_GD_CrI= M7_MuCrI


### PSEUDO
load("Summaries/N1/sum11.Rda")
M11_meanP<- sum11$statistics[1,1] # Pooled mean
M11_MuCrI<- c(sum11$quantiles[1,1], sum11$quantiles[1,5])
M_PSEUD_FFD= M11_meanP; M_PSEUD_FFD_CrI= M11_MuCrI

load("Summaries/N1/sum13.Rda")
M13_meanP<- sum13$statistics[1,1] # Pooled mean
M13_MuCrI<- c(sum13$quantiles[1,1], sum13$quantiles[1,5])
M_PSEUD_GD= M13_meanP; M_PSEUD_GD_CrI= M13_MuCrI

### X
load("Summaries/N1/sum9.Rda")
M9_meanP<- sum9$statistics[1,1] # Pooled mean
M9_MuCrI<- c(sum9$quantiles[1,1], sum9$quantiles[1,5])
M_X_FFD= M9_meanP; M_X_FFD_CrI= M9_MuCrI

load("Summaries/N1/sum10.Rda")
M10_meanP<- sum10$statistics[1,1] # Pooled mean
M10_MuCrI<- c(sum10$quantiles[1,1], sum10$quantiles[1,5])
M_X_GD= M10_meanP; M_X_GD_CrI= M10_MuCrI

### ORTH
load("Summaries/N1/sum15.Rda")
M15_meanP<- sum15$statistics[1,1] # Pooled mean
M15_MuCrI<- c(sum15$quantiles[1,1], sum15$quantiles[1,5])
M_ORTH_FFD= M15_meanP; M_ORTH_FFD_CrI= M15_MuCrI

load("Summaries/N1/sum17.Rda")
M17_meanP<- sum17$statistics[1,1] # Pooled mean
M17_MuCrI<- c(sum17$quantiles[1,1], sum17$quantiles[1,5])
M_ORTH_GD= M17_meanP; M_ORTH_GD_CrI= M17_MuCrI

### SEM
load("Summaries/N1/sum19.Rda")
M19_meanP<- sum19$statistics[1,1] # Pooled mean
M19_MuCrI<- c(sum19$quantiles[1,1], sum19$quantiles[1,5])
M_SEM_FFD= M19_meanP; M_SEM_FFD_CrI= M19_MuCrI

load("Summaries/N1/sum21.Rda")
M21_meanP<- sum21$statistics[1,1] # Pooled mean
M21_MuCrI<- c(sum21$quantiles[1,1], sum21$quantiles[1,5])
M_SEM_GD= M21_meanP; M_SEM_GD_CrI= M21_MuCrI

M_PHON_FFD= M22_meanP; M_PHON_FFD_CrI= M22_MuCrI
M_PHON_GD= M23_meanP; M_PHON_GD_CrI= M23_MuCrI




############################################################
load("Summaries/Posterior_samples/RAN_FFD.Rda")

S1_s<- sort(S1)
t1<- 0.025*length(S1_s); t2<- length(S1_s)-0.025*length(S1_s)
S1_95<- S1_s[t1:t2]

M<- NULL
for(i in 1:10){
  M[i]<- mean(S1_95[i])
}


