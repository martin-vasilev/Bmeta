# Function for imputing missing standard deviations from studies
# Martin Vasilev, 2016

impute_SD<- function(data){
  
  imputeF<- function(s, n){ # based on Furukawa et al. (2006)
    Sp<- sum((n-1)*(s^2))/sum(n-1)
    return(sqrt(Sp))
  }
  
  # code the IDs of studies that need to have their SDs imputed..
  impute<- c(5, 27, 28, 30, 37, 38, 39, 48, 49, 51, 61, 62, 69, 70, 86)
  imputePend<- c(24, 88, 31, 32, 33, 76, 83) # Temporary, contacted authors for data
  impute<- c(impute, imputePend); rm(imputePend)
  impute<- sort(impute)
  
  # subset the data by language type (alphabetical vs Chinese)
  dataAll<- data
  dataAlph<- subset(dataAll, Language!="Chinese")
  dataCh<- subset(dataAll, Language=="Chinese")
  
  # Prepare variables for imputing
  var<- c("FFD", "SFD", "GD", "Total")
  prev<- c("val", "RAN", "X", "UNREL", "PSEUD", "ORTH",  "SEM", "PHON")
  string<-NULL # for accessing the mean
  string2<-NULL  # for accessing the SD
  string3<-NULL  # for getting the imputed value
  string4<-NULL  # for getting the number of valid SDs to pool from
  string5<- NULL # for getting sample size
  a<-NULL; b<- NULL; n<- NULL
  library(gtools)
  
  # Record all info to a text file
    sink("imputation_log.txt")
    cat(toString(Sys.time())); cat("\n"); cat("\n")
    cat("Imputation  Log summary for N+1 studies"); cat("\n")
  
  for(i in 1:length(impute)){ # for each study..
    cat("\n")
    cat("--------------------------------------------------")
    cat("\n")
    cat(toupper(sprintf("  Checking study:    %s", data$Paper[impute[i]])))
    cat("\n")
      
    for(j in 1:4){ # for each dependent variable..
      
      for(k in 1:8){ # for each preview condition..
        
        if(k==1){
          
          # get string:
          string<- paste("data", "$", var[j],"_N1_", prev[k], "[impute[i]]", sep="") # mean
          string2<- paste("data", "$", var[j],"_N1_", prev[k], "_SD", "[impute[i]]", sep="") # standard deviation
          string3<- paste("mean(na.omit(" ,"dataAll", "$", var[j],"_N1_", prev[k], "_SD", "))", sep="")
          string4<- paste("data", "$", var[j], "_N1_", prev[k], "_SD", sep="" )
        } else{
          string<- paste("data", "$", var[j],"_N1_","inval_", prev[k], "[impute[i]]", sep="") # mean
          string2<- paste("data", "$", var[j],"_N1_","inval_", prev[k], "_SD", "[impute[i]]", sep="") # standard deviation
          string3<- paste("mean(na.omit(" ,"dataAll", "$", var[j],"_N1_", prev[k], "_SD", "))", sep="")
          string4<- paste("data", "$", var[j], "_N1_", "inval_", prev[k], "_SD", sep="" )
        }
        
          if(invalid(eval(parse(text=string)))==FALSE){ # continue only if there is a mean value for study i
            
          if(invalid(eval(parse(text=string2)))==TRUE){ # continue only if the variance is missing..
            
            cat("\n")
            cat(sprintf("  Missing variance for: measure %s, preview condition %s", var[j], prev[k]))
            cat("\n")
            
            # Impute SDs:
            a<- eval(parse(text=string4)) # get vector with all SDs of this preview type
            a<- na.omit(a) # remove NAs 
            string5=paste("data$N[which(", string4, "!=", "'", "NA", "'" ,")]", sep="") # get N
            n<- eval(parse(text=string5))
            b<- imputeF(a, n)
            
            # Save imputed value
            eval(parse(text=paste(string2, "<- ", b, sep="")))
            
            # Print info about studies:
            cat(sprintf("         Pooling from %s studies (%s Ns) with mask %s ", length(a), length(n), prev[k]));  cat("\n")
            cat(sprintf("         New imputed value: %s", b))
          } 
          
        } # end of imputation part
        
      }  # for preview condition k
          
    } # for dependent variable j
    
  } # for study i
  
    sink() # close text file
    file.show("imputation_log.txt")
    
    save(data, file= "Data/dataN1_imputed.Rda")
    
  return(data)
} # end of function

