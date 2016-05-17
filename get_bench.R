get_bench<- function(){
  
  # load N+1 data
 load("Data/dataN1_Imputed.Rda")
  dataN1<-data
 
  # code new studies:
  ID<- NULL
  Language<- NULL
  N<- NULL
  FFD_M<- NULL
  FFD_SD<- NULL
  SFD_M<- NULL
  SFD_SD<- NULL
  GD_M<- NULL
  GD_SD<- NULL
  TVT_M<- NULL
  TVT_SD<- NULL
  
  #1: Gu & Li (2015)
  j=1
  ID[j]<- j
  Language[j]<- "Chinese"
  N[j]<- 30
  FFD_M[j]<- (286+273)/2  # Table 2
  FFD_SD[j]<- ((9+7)/2)*sqrt(N[j])
  SFD_M[j]<- NA
  SFD_SD[j]<- NA
  GD_M[j]<- (479+489)/2
  GD_SD[j]<- ((23+24)/2)*sqrt(N[j])
  TVT_M[j]<- NA
  TVT_SD[j]<- NA
  
  
  #2: Briihl & Inhoff (1995) 
  j=2
  ID[j]<- j
  Language[j]<- "English"
  N[j]<- 32      
  FFD_M[j]<- 251    # Table 2
  FFD_SD[j]<- 6*sqrt(N[j])
  SFD_M[j]<- NA
  SFD_SD[j]<- NA
  GD_M[j]<- 313
  GD_SD[j]<- 9.4*sqrt(N[j])
  TVT_M[j]<- NA
  TVT_SD[j]<- NA
  
  #3: Angele & Rayner (2013) 
  j=3
  ID[j]<- j
  Language[j]<- "English"
  N[j]<- 32
  FFD_M[j]<- 247
  FFD_SD[j]<- 88
  SFD_M[j]<- NA
  SFD_SD[j]<- NA
  GD_M[j]<- 295
  GD_SD[j]<- 131
  TVT_M[j]<- 345
  TVT_SD[j]<- 184
  
  #4: White & Liversedge (2006), Exp.2
  j=4
  ID[j]<- j
  Language[j]<- "English"
  N[j]<- 44
  FFD_M[j]<- NA
  FFD_SD[j]<- NA
  SFD_M[j]<- NA
  SFD_SD[j]<- NA
  GD_M[j]<- (365+399)/2
  GD_SD[j]<- (156+179)/2
  TVT_M[j]<- NA
  TVT_SD[j]<- NA
  
  #5: Choi & Gordon (2013) 
  j=5
  ID[j]<- j
  Language[j]<- "English"
  N[j]<- 28
  FFD_M[j]<- (210 + 230)/2 # Table 4
  FFD_SD[j]<- (35 + 52)/2
  SFD_M[j]<- (211 + 233)/2
  SFD_SD[j]<- (33 + 54)/2
  GD_M[j]<- (228 + 260)/2
  GD_SD[j]<- (39 + 57)/2
  TVT_M[j]<- NA
  TVT_SD[j]<- NA
    
  #6: Choi & Gordon (2013), Exp.2 
  # I code only "new" words in order to avoid repetition effects
  j=6
  ID[j]<- j
  Language[j]<- "English"
  N[j]<- 40     # Table 7
  FFD_M[j]<- 221
  FFD_SD[j]<- 32
  SFD_M[j]<- 224
  SFD_SD[j]<- 34
  GD_M[j]<- 239
  GD_SD[j]<- 38
  TVT_M[j]<- NA
  TVT_SD[j]<- NA
  
  #7: Deutsch, Frost, Pollatsek, & Rayner (2005), Exp.2 
  j=7
  ID[j]<- j
  Language[j]<- "Hebrew"
  N[j]<- 42    # Table 2
  FFD_M[j]<- 222
  FFD_SD[j]<- 15.3
  SFD_M[j]<- NA
  SFD_SD[j]<- NA
  GD_M[j]<- 250
  GD_SD[j]<- 24.1
  TVT_M[j]<- NA
  TVT_SD[j]<- NA
  
  #8: Deutsch, Frost, Pollatsek, & Rayner (2005), Exp.4 
  j=8
  ID[j]<- j
  Language[j]<- "Hebrew"
  N[j]<- 42   # Table 4
  FFD_M[j]<- 226
  FFD_SD[j]<- 21.6
  SFD_M[j]<- NA
  SFD_SD[j]<- NA
  GD_M[j]<- 258
  GD_SD[j]<- 45.1
  TVT_M[j]<- NA
  TVT_SD[j]<- NA
  
  #9: Stoops (2013), Exp.1
  j=9
  ID[j]<- j
  Language[j]<- "Russian"
  N[j]<- 48-6
  FFD_M[j]<- (249 + 243)/2   # Table 5
  FFD_SD[j]<- (133 + 128)/2 
  SFD_M[j]<- NA
  SFD_SD[j]<- NA
  GD_M[j]<- (303 + 295)/2   # Table 7
  GD_SD[j]<- (179 + 183)/2
  TVT_M[j]<- NA
  TVT_SD[j]<- NA
  
  
  bench_FFD1<-data.frame(ID, Language, N, FFD_M, FFD_SD)
  bench_SFD1<-data.frame(ID, Language, N, SFD_M, SFD_SD)
  bench_GD1<-data.frame(ID, Language, N, GD_M, GD_SD)
  bench_TVT1<-data.frame(ID, Language, N, TVT_M, TVT_SD)
  
  # Change ID so that it starts after ES_N1:
  
  for(i in 1:nrow(bench_FFD1)){
    bench_FFD1$ID[i]<- i+88
    bench_SFD1$ID[i]<- i+88
    bench_GD1$ID[i]<- i+88
    bench_TVT1$ID[i]<- i+88
  }
  
  
  # extract existing data:
  ID<- dataN1$ID
  Language<- dataN1$Language
  N<- dataN1$N
  FFD_M<- dataN1$FFD_N1_val
  FFD_SD<- dataN1$FFD_N1_val_SD
  SFD_M<- dataN1$SFD_N1_val
  SFD_SD<- dataN1$SFD_N1_val_SD
  GD_M<- dataN1$GD_N1_val
  GD_SD<- dataN1$GD_N1_val_SD
  TVT_M<- dataN1$Total_N1_val
  TVT_SD<- dataN1$Total_N1_val_SD  
  
  bench_FFD<-data.frame(ID, Language, N, FFD_M, FFD_SD)
  bench_SFD<-data.frame(ID, Language, N, SFD_M, SFD_SD)
  bench_GD<-data.frame(ID, Language, N, GD_M, GD_SD)
  bench_TVT<-data.frame(ID, Language, N, TVT_M, TVT_SD)
  
  # bind with coded data:
  bench_FFD<-rbind(bench_FFD, bench_FFD1)
  bench_SFD<-rbind(bench_SFD, bench_SFD1)
  bench_GD<-rbind(bench_GD, bench_GD1)
  bench_TVT<-rbind(bench_TVT, bench_TVT1)
  
  # Remove entries if no mean values are present..
  bench_FFD<- subset(bench_FFD, FFD_M !="NA")
  bench_SFD<- subset(bench_SFD, SFD_M !="NA")
  bench_GD<- subset(bench_GD, GD_M !="NA")
  bench_TVT<- subset(bench_TVT, TVT_M !="NA")
  
  save(bench_FFD, file="Data/bench_FFD.Rda")
  save(bench_SFD, file="Data/bench_SFD.Rda")
  save(bench_GD, file="Data/bench_GD.Rda")
  save(bench_TVT, file="Data/bench_TVT.Rda")
  
}