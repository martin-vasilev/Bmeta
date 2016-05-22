get_year<- function(string){
  
  num<- as.numeric(unlist(strsplit(gsub("[^0-9]", "", unlist(string)), "")))
  year<- paste(num[1:4], collapse = "")
  
  return(year)
}