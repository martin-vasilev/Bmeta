
JReg <- function(priorMu, priorTau, N , modelName){
 
# Tentative code for creating JAGS meta-regression models
  
# Martin R. Vasilev, 2016  
  
  filename= paste("Models/", modelName, sep="")
  
  # Create JAGS model based on user input
   
  if(grepl("dgamma", priorTau)==FALSE){
    cat("model {", "\n",
             "for (i in 1:", N,")", " ", "{", "\n",
             "P[i] <- 1/S.sqr[i]      # Calculate precision", "\n",
             "T[i] ~ dnorm(theta[i] + beta*cov[i], P[i]) # study effects", "\n",
             "theta[i] ~ dnorm(mu, prec) # random effects", "\n",
             "\n",
             "}", "\n",
             "mu ~ ", priorMu, " ", "", "# mean difference prior", "\n",
             "tau ~ ", priorTau, " ", "# tau prior", "\n",
             "beta ~ dnorm(0,1.0E-4) #additional prior for beta (covariate)", "\n",
             "tau.sqr <- tau*tau   # between-study variance", "\n",
             "prec<- 1/tau.sqr # precision of tau", "\n",
             "# Note: precision means that studies with smaller", "\n",
             "#       variance are given greater weight.", "\n",
             "}",
             sep="", file= filename)}
  else{
    cat("model {", "\n",
        "for (i in 1:", N,")", " ", "{", "\n",
        "P[i] <- 1/S.sqr[i]      # Calculate precision", "\n",
        "T[i] ~ dnorm(theta[i], P[i]) # study effects", "\n",
        "theta[i] ~ dnorm(mu, prec) # random effects", "\n",
        "\n",
        "}", "\n",
        "mu ~ ", priorMu, " ", "", "# mean difference prior", "\n",
        "prec ~ ", priorTau, " ", "# gamma prior on precision", "\n",
        "tau<- 1/sqrt(prec) # tau", "\n",
        "# Note: precision means that studies with smaller", "\n",
        "#       variance are given greater weight.", "\n",
        "}",
        sep="", file= filename)
  }
  
  return(filename)
  
}

