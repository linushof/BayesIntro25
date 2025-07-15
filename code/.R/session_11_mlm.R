# preparation -------------------------------------------------------------

library(rethinking)
library(tidyverse)

# Exercise 1 --------------------------------------------------------------

# simulate data

sim_quality_1 <- function(G) {
  N <- length(G)
  quality <- ifelse(G==1, .75, .6) + rnorm(N, 0, .2)
  data.frame(G, quality)
}

G <- as.factor(sample(c(1, 2), 1e3, replace = T))
d1 <- sim_quality_1(G)

# complete pooling
m1 <- ulam(
  alist(
    
    # likelihood
    quality ~ dnorm(mu, sigma) , 
    
    #prior
    mu ~ dnorm(.5, .2) , 
    sigma ~ dexp(.5)
    
  ) , 
  data = d1 ,
  chains = 4, 
  cores = 4, 
  log_lik = TRUE, 
  file = "models/session_11_m1.rds" 
)
precis(m1)


## no pooling
m2 <- ulam(
  alist(
    
    # likelihood
    quality ~ dnorm(mu, sigma) ,
    mu <- a[G] , 
    
    # prior
    a[G] ~ dnorm(.5, .2) ,
    sigma ~ dexp(.5)
  ) , 
  
  data = d1 , 
  chains = 4 , 
  cores = 4 , 
  log_lik = TRUE,
  file = "models/session_11_m2.rds"
)
precis(m2, depth = 2)


# Exercise 2 --------------------------------------------------------------

## partial pooling
m3 <- ulam(
  alist(
    
    # likelihood
    quality ~ dnorm(mu, sigma) , 
    mu <- a[G] , 
    
    # prior
    a[G] ~ dnorm(a_bar, tau) ,
    a_bar ~ dnorm(.5, .2) , 
    tau ~ dnorm(0, .1) , 
    sigma ~ dexp(.5)
    
  ) , 
  data = d1, 
  chains = 4, 
  cores = 4, 
  log_lik = TRUE , 
  file = "models/session_11_m3.rds"
)
precis(m3, depth = 2)

# compare model fit
compare(m1, m2, m3)

# Exercise 3 ---------------------------------------------------------------

# generative model 

sim_quality_2 <- function(G, a, b, oa, ob) {
  N <- length(G)
  numeracy <- rnorm(N, .5, .2)
  quality <- a + oa[G] + (b + ob[G])*numeracy + rnorm(N, 0, .1)
  data.frame(G, numeracy, quality)
}

# simulate data

G <- as.factor(sample(1:20, 1e4, replace = T))
oa <- round(sample(seq(-.3, .3, .05), 20, replace = T), 2)
ob <- round(sample(seq(-.2, .2, .05), 20, replace = T), 2)
d2 <- sim_quality_2(G, a = .5, b = 0, oa = oa, ob = ob)
head(tibble(oa=.5+oa, ob=ob), 5)

# test model

## partial pooling 

m4 <- ulam( 
  alist(
    
    # likelihood
    quality ~ dnorm(mu, sigma) , 
    mu <-  a[G] + b[G] * numeracy ,
    
    # prior
    a[G] ~ dnorm(a_bar, tau_a) ,
    b[G] ~ dnorm(b_bar, tau_b) ,
    
    a_bar ~ dnorm(.5, .2) , 
    b_bar ~ dnorm(0, .15) , 
    tau_a ~ dexp(.5) , 
    tau_b ~ dexp(.5) ,
    sigma ~ dexp(1)
    
  ) , 
  data = d2, 
  chains = 4, 
  cores = 4, 
  iter = 4000,
  log_lik = TRUE,
  file = "models/session_11_m4.rds"
)

precis(m4, depth = 2)
plot(precis(m4, depth = 2))


m5 <- ulam( 
  alist(
    
    # likelihood
    quality ~ dnorm(mu, sigma) , 
    mu <-  a + b * numeracy ,
    
    #prior
    a ~ dnorm(.5, .2) , 
    b ~ dnorm(0, .15),
    sigma ~ dexp(1)
    
  ) , 
  data = d2, 
  chains = 4, 
  cores = 4, 
  iter = 4000,
  log_lik = TRUE,
  file = "models/session_11_m5.rds"
)

precis(m5)
compare(m4, m5)
