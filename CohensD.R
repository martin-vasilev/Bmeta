# Function for calculating effect sizes (intended for use in data checking)
# Martin R. Vasilev, 2016

CohensD<- function(m1, m2, s1, s2, N=NULL){
  
  if(hasArg(N)){
    type<- "SE"
  } else {
    type<- "SD"
  }
  
  md<- m2-m1 # get the mean difference
  es<- NULL
  SDpooled<- NULL
  sd1<- NULL; sd2<- NULL
    
  if (hasArg(s1) & hasArg(s2)){
    
    if(type=="SD"){ # default, computes effect size based on standard deviation
        message("assuming SDs as input")
        SDpooled <- sqrt((s1^2 + s2^2)/2)
    } else {
        if(hasArg(N)){
          message("assuming SEs as input")
          SDpooled <- sqrt(((s1*sqrt(N))^2 + (s2*sqrt(N))^2)/2)
        } else{
          stop("Please enter sample size at position 5")
        }
      }
    
    es<- md/SDpooled
  
   string1<- paste("Mean difference: ", toString(md), "ms")  
   string2<- paste("Effect size: ", toString(es))
    
   print(string1)
   print(string2)
 
  } else{
    message("SDs missing.. calculating only mean difference")
    string1<- paste("Mean difference: ", toString(md), "ms")
    print(string1)
  }
 
}

