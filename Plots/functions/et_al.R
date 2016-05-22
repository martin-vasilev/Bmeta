
et_al<- function(string){
  t<- c(gregexpr(pattern =' ', string), gregexpr(pattern =',', string)); t<- as.numeric(unlist(t))
  t<- t[which(t>0)]; t<- min(t)
  auth<- substring(string, 1, t-1)
  #C2<- as.numeric(unlist(gregexpr(pattern =')', string)))
  
  num<- as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(string)), "")))
  
  # check for experiment info:
  exp<- gregexpr(pattern = 'Exp', string)
  expS<-NULL
  if(exp!= -1){
    expS<- substr(string, unlist(exp), unlist(exp)+4)
  } else{
    expS<- NA
  }
  
  year<- paste(num[1:4], collapse = "")
  if(auth=="Wakeford"){
    year= "n.d."
  }
  
  if(exp== -1){
    out<- paste(auth," et al. ", "(", year, ")", sep="")
  }else{
    out<- paste(auth," et al. ", "(", year, ")", ", ", expS, sep="")
  }
  
  
  return(out)
}
