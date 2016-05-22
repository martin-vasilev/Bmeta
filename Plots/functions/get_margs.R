get_margs<- function(minY, maxY){
  library(plyr)
  
  if(sum(minY>0)==0){
    minY<- minY-10
  } else {
    minY<- minY+10
  }
  minY<- round_any(minY, 10, floor)
  
  if(sum(maxY>0)==0){
    maxY<- maxY-10
  } else {
    maxY<- maxY + 10
  }
  maxY<- round_any(maxY, 10, ceiling)
  
  rng<- seq(minY, maxY, 10)
  len<- length(rng)/2
  out<- NULL
  for(i in 1:len){
    out[i]<- rng[i*2]
  }
  
  return(out)
}
