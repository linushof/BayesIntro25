# load packages
library(tidyverse)

# MCMC --------------------------------------------------------------------

# Metropolis Algorithm 

num_weeks <- 1e4 # number of iterations/days
positions <- rep(0,num_weeks) # storage for positions/islands

current <- sample(1:10, 1)
for ( i in 1:num_weeks ) {
  
  # record current position
  positions[i] <- current
  # flip coin to generate proposal
  proposal <- current + sample( c(-1,1) , size=1 )
  # now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1
  prob_move <- proposal/current
  current <- ifelse( runif(1) < prob_move , proposal , current )
  
  }


# sequence 
data <- data.frame(position = positions, move = 1:1e4)
ggplot(data, aes(x=move, y = position)) +
  geom_line(color = "#ff02ff", linewidth=.5) + 
  geom_point() +
  scale_x_continuous(limits = c(5000,5100)) + # adjust limits between 0 and 10,000
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1)) +
  theme_minimal(base_size = 20)

# frequencies
ggplot(data, aes(x=position)) +
  geom_bar(color = "#ff02ff", fill = "#ff02ff", alpha = .5) + 
  scale_x_continuous(breaks = seq(1,10,1)) + 
  theme_minimal(base_size = 20)


# Rethinking --------------------------------------------------------------
library(rethinking)
library(parallel)

shaq <- read_csv("data/shaq.csv")
dat <- list( 
  N = nrow(shaq) , 
  pts = shaq$PTS ,
  min = shaq$Minutes ,
  fga = shaq$FGA , 
  fta = shaq$FTA , 
  min_bar = round(mean(shaq$Minutes), 2) , 
  fga_bar = round(mean(shaq$FGA), 2) ,
  fta_bar = round(mean(shaq$FTA), 2)
)


# use ulam (interfacec to Stans HMC Sampler)
cores <- detectCores(logical = TRUE)
n_chains <- 4

mshaq <- ulam(
  alist(
    
    # likelihood
    pts ~ dnorm(mu, sigma), 
    mu <- a + b_1 * (min - min_bar) + b_2 * (fga - fga_bar) * 2 + b_3 * (fta - fta_bar),
    
    # prior
    a ~ dnorm(20, 8),
    b_1 ~ dnorm(0, 2), 
    b_2 ~ dunif(0, 2), 
    b_3 ~ dunif(0, 1), 
    sigma ~ dunif(0,10)
  ),
  data = dat, 
  chains = n_chains , # number of independent MCMC chains 
  cores = n_chains , # number of cores that 
  iter = 4000)

precis(mshaq)
rethinking::traceplot(mshaq)

# Postscript: RStan -------------------------------------------------------------------

library(rstan)
rstan_options(auto_write = TRUE)


# step 1: write .stan file
stancode(mshaq)


# step 2 and 3: specify and fit the Stan model 
m1shaq <- stan("code/.stan/session_9_m1shaq.stan", 
               data=dat, 
               chains = n_chains, 
               cores = n_chains, 
               iter = 4000)


# step 4: inspect and evaluate the model
m1shaq
stan_dens(m1shaq)
pairs(m1shaq, pars = c("mu", "sigma"))
traceplot(m1shaq)

# Mediation / Pipe example

# step 3: Fit the model
m2shaq <- stan("code/.stan/session_9_m2shaq.stan", 
               data=dat, 
               chains = 4, 
               cores = 4, 
               iter = 4000)

# step 4: inspect and evaluate the model
m2shaq
plot(m2shaq)
pairs(m2shaq, pars = c("a", "b1", "b2", "b3", "sigma"))
traceplot(m2shaq)
stan_dens(m2shaq, separate_chains = TRUE)


