
JModel <- function(priorMu, priorTau, N , modelName){
 
# A function for generating JAGS txt models
# Designed to work for a random-effects meta-analysis model
  
# Martin R. Vasilev, 2016  
  
  filename= paste("Models/", modelName, sep="")
  
  # Create JAGS model based on user input
     cat("model {", "\n",
             "for (i in 1:", N,")", " ", "{", "\n",
             "P[i] <- 1/S.sqr[i]      # Calculate precision", "\n",
             "T[i] ~ dnorm(theta[i], P[i]) # study effects", "\n",
             "theta[i] ~ dnorm(mu, prec) # random effects", "\n",
             "\n",
             "}", "\n",
             "mu ~ ", priorMu, " ", "", "# mean difference prior", "\n",
             "tau ~ ", priorTau, " ", "# Uniform on SE", "\n",
             "tau.sqr <- tau*tau   # between-study variance", "\n",
             "prec<- 1/tau.sqr # precision of tau", "\n",
             "# Note: precision means that studies with smaller", "\n",
             "#       variance are given greater weight.", "\n",
             "}",
             sep="", file= filename)
  
  return(filename)
  
}

