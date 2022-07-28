library(tidyverse)
library(tibble)
library(rjags)

# parameters
nv=0.075
dv=0.175
nullORR=0.075
altORR=0.275
beta_a=0.0811
beta_b=1
alpha=0.05
n_init=2000
gridsearch_step=1

# simulation parameter
rep=10000
prior_n=50000

posteriorcal <- function(sampsize,resp){
  # prior
  pri <- rbeta(prior_n,beta_a,beta_b)
  # DEFINE the model    
  model_string  <- "model{
    # Likelihood model for y
    y ~ dbin(p, n)

    # Prior models for p in binomial
    p ~ dbeta(a, b)
}"
  
  data_list <- list(a = beta_a, b = beta_b, y = resp, n = sampsize)
  
  params_to_monitor <- c("p")
  
  # COMPILE the model
  binom_jags <- jags.model(
    textConnection(model_string),
    data = data_list,
    inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 1000)
  )
  
  # SIMULATE the posterior    
  binom_sim <- coda.samples(model = binom_jags, variable.names = c("p"), n.iter = 50000)
  
  # Store the chains in a data frame
  Binom_chains <- data.frame(binom_sim[[1]])
  
  # Plot the posterior    
  # plot(binom_sim, trace = FALSE)
  plot(density(Binom_chains$p))
  bayesig <- mean(Binom_chains$p>=nv)
  postmed <- median(Binom_chains$p)
  result <- list("bayesig" = bayesig, "postmed" = postmed)
  return(result) 
}

# grid search: fix r and search n, then increase r and search n...
# resplist <- seq(from = 1, to = 4, by = 1)
# for (i in resplist){
#   samplesize <- seq(from=21,to=21,by=1)
#   final <- sapply(samplesize, posteriorcal,i)
# }

posteriorcal(22,5)

sigmat <- matrix(0, nrow = 10, ncol = 10)
postmed <- matrix(0, nrow = 10, ncol = 10)
# use Analytical expression for posterior to check
# lines(density(rbeta(10000,beta_a+5,beta_b+22-5)),col="red")
for (r in 1:10){
  for (n in 18:27){
    rand <- rbeta(100000,beta_a+r,beta_b+n-r)
    sigmat[r,n-17] <- mean(rand>=nv)
    postmed[r,n-17] <- median(rand)
    if (sigmat[r,n-17]<0.95){sigmat[r,n-17]=NA}
    if (postmed[r,n-17]<0.175){postmed[r,n-17]=NA}
  }
}
