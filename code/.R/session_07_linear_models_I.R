rm(list=ls())

# load packages
library(tidyverse)
library(rethinking)

# Exercise: Summing up random variables ----------------------------------------------------------------

hist(replicate(1000, sum(rbeta(100, 2,1))))

# Exercise Gaussian model: Prior definition and prior predictive check  ----------------------------------------------------------
# y ~ Normal(mu, sigma) 

# mu prior

range <- seq(-10, 60, length.out = 100) # range
d <- dnorm(range, mean = 20, sd = 8) # densities
mu <- data.frame(range, d)
ggplot(mu, aes(x = range, y = d)) +
  geom_line(linewidth = 1, color = "#ff02ff") +
  scale_x_continuous(limits = c(-10,60), breaks = seq(-10,60, 5)) + 
  labs(x = expression(mu), 
       y = "Density") 

# sigma sigma prior

range <- seq(-1, 11, length.out = 100) # sample space
d <- dunif(range, min = 0, max = 10) # densities
sigma <- data.frame(range, d)
ggplot(sigma, aes(x = range, y = d)) +
  geom_line(linewidth = 1, color = "#ff02ff") +
  scale_x_continuous(limits = c(-1,11), breaks = seq(-1,11, 1)) + 
  labs(x = expression(sigma), 
       y = "Density") 

# prior predictive check

mu_samples <- rnorm(1e4, 20, 8)
sigma_samples <- runif(1e4, 0, 10)

prior_pred_gauss <- tibble(pts = rnorm(1e4, mu_samples, sigma_samples))

prior_pred_gauss %>% ggplot(aes(x = pts)) + 
  geom_histogram(fill = "#ff02ff", 
                 alpha = .5, 
                 color = "#ff02ff", 
                 bins = 20) + 
  labs(x = "PTS", 
       y = "Frequency")


# Gaussian model validation -------------------------------------------------

# Simulate data

N_games <- 1e3
mu <- 30
sd <- 5
sim_pts <- data.frame( pts = round(rnorm(N_games, mu, sd), 0) )

sim_pts %>% 
ggplot(aes(x = pts)) + 
  geom_histogram(fill = "#ff02ff", alpha = .5, color = "#ff02ff", bins = 30) + 
  labs(x = "PTS", 
       y = "Frequency") 

## Fit model on simulated data
m_gauss_sim <- alist(
  #likelihood
  pts ~ dnorm( mu , sigma ) ,
  
  #priors
  mu ~ dnorm( 20 , 8 ) ,
  sigma ~ dunif( 0 , 10 ))

m_gauss_sim_fit <- quap(m_gauss_sim, data=sim_pts)
m_gauss_sim_fit
m_gauss_sim_fit@formula
m_gauss_sim_fit@start
m_gauss_sim_fit@coef
m_gauss_sim_fit@data

precis(m_gauss_sim_fit)
plot(m_gauss_sim_fit)
pairs(m_gauss_sim_fit, pars = c("mu", "sigma"))
extract.prior(m_gauss_sim_fit, n=1000)
extract.samples(m_gauss_sim_fit, n=1000)
PI(extract.samples(m_gauss_sim_fit, n=1000)$mu)
PI(extract.samples(m_gauss_sim_fit, n=1000)$sigma)
HPDI(extract.samples(m_gauss_sim_fit, n=1000)$mu)
HPDI(extract.samples(m_gauss_sim_fit, n=1000)$sigma)

# Exercise Gaussian model: Model fit and evaluation -------------------------------

# load data
shaq <- read_csv("data/shaq.csv")

# plot data
shaq %>% 
ggplot(aes(x = PTS)) + 
  geom_histogram(fill = "#ff02ff", alpha = .5, color = "#ff02ff", bins = 50) + 
  labs(x = "PTS", 
       y = "Frequency")

## fit model
m_gauss_shaq_fit <- quap(
  alist(PTS ~ dnorm( mu , sigma ),
        mu ~ dnorm( 20 , 8 ) ,
        sigma ~ dunif( 0 , 10 )) ,
  data = shaq)

pairs(m_gauss_shaq_fit, pars = c("mu", "sigma"))
# evaluate 

## posterior summaries
precis(m_gauss_shaq_fit)

## full posterior using posterior samples

m_gauss_shaq_smp <- extract.samples(m_gauss_shaq_fit, n = 1e3) # draw posterior samples

### mu poseterior
m_gauss_shaq_smp %>%  
  ggplot(aes(x = mu)) +
  geom_density(color = "#ff02ff", linewidth = 1, alpha = .1) +
  labs(x = expression(mu), 
       y = "Density") 

### sigma posterior
m_gauss_shaq_smp %>%  
  ggplot(aes(x = sigma)) +
  geom_density(color = "#ff02ff", linewidth = 1, alpha = .1) +
  labs(x = expression(sigma), 
       y = "Density")
  
## Percentile and highest density percentile intervals 

### mu
PI(m_gauss_shaq_smp$mu)
HPDI(m_gauss_shaq_smp$mu)

### sigma
PI(m_gauss_shaq_smp$sigma)
HPDI(m_gauss_shaq_smp$sigma)


## Posterior predictive checks 

### dnorm
range <- seq(-10, 60, length.out = 100) # range
m_gauss_shaq_post_dens <- m_gauss_shaq_smp %>% 
  mutate(smp = row_number()) %>%
  expand_grid(range) %>% 
  mutate(d = dnorm(range, mu, sigma))

m_gauss_shaq_post_dens %>% 
  ggplot(aes(x = range, y = d, group = smp)) +
  geom_line(color = "#ff02ff", size = .5, alpha = .1) +
  scale_x_continuous(limits = c(-10,60), breaks = seq(-10,60, 5)) + 
  labs(x = expression(mu), 
       y = "Density")

### rnorm

m_gauss_shaq_post_pred <- tibble(pts = round(rnorm(nrow(m_gauss_shaq_smp), mean = m_gauss_shaq_smp$mu, sd = m_gauss_shaq_smp$sigma), 0))
m_gauss_shaq_post_pred %>%  
  ggplot(aes(x = pts)) + 
  geom_histogram(fill = "#ff02ff", alpha = .5, color = "#ff02ff", bins = 30) +
  geom_histogram(data=shaq, aes(x=PTS), fill = "yellow", alpha = .5, color = "yellow", bins = 50) 
  


# Exercise linear model: Bayesian workflow steps 4-7  ------------------------------------------------------------
# y ~ Normal(mu, sigma)
# mu = a + b*FGA

#  Prior definition

## beta prior
range <- seq(-1, 4, length.out = 100) # sample space
d <- dunif(range, min = 0, max = 3) # densities
beta <- data.frame(range, d)
ggplot(beta, aes(x = range, y = d)) +
  geom_line(size = 1, color = "#ff02ff") +
  scale_x_continuous(limits = c(-1,4), breaks = seq(-1,4, 1)) + 
  labs(x = expression(beta), 
       y = "Density") 

## alpha prior
range <- seq(-1, 16, length.out = 100) # sample space
d <- dunif(range, min = 0, max = 15) # densities
alpha <- data.frame(range, d)
ggplot(alpha, aes(x = range, y = d)) +
  geom_line(size = 1, color = "#ff02ff") +
  scale_x_continuous(limits = c(-1,16), breaks = seq(-1,16, 1)) + 
  labs(x = expression(alpha), 
       y = "Density") 


# prior predictive check

alpha_samples <- runif(1e3, 0, 10)
beta_samples <- runif(1e3, 0, 3)
sigma_samples <- runif(1e3, 0, 10)

prior_pred_lm_dens <- tibble(alpha_samples, beta_samples)
prior_pred_lm_dens %>% 
  ggplot(aes(x = seq(0,30,1))) + 
  geom_abline(aes(intercept = alpha_samples, slope = beta_samples), 
              color = "#FDB927", 
              size = .5, 
              alpha = .1) + 
  scale_x_continuous(limits = c(0,40), breaks = seq(0,40,10)) + 
  scale_y_continuous(limits = c(0,80), breaks = seq(0,80,10)) + 
  labs(x = "FGA",
       y = "Points")

FGA <-  round(rnorm(1e3, 20, 10), 0)
mu_samples <- alpha_samples + beta_samples*FGA
prior_pred_lm <- tibble(pts = rnorm(1e3, mu_samples, sigma_samples))
prior_pred_lm %>% ggplot(aes(x = pts)) + 
  geom_histogram(fill = "#FDB927", alpha = .5, color = "#FDB927", bins = 30) + 
  labs(x = "PTS", 
       y = "Frequency") 


# validate the model

## simulate data

sim_pts <- function(FGA, b, sd){ 
  
  u <- rnorm(length(FGA), 0, sd)
  pts <- round(b*FGA + u, 0)
  
}

set.seed(124)
FGA <- round(rnorm(1e3, 20, 10), 0)
sim_dat <- tibble(FGA = FGA ,
                  pts = sim_pts(FGA, 2, 7))

sim_dat %>% 
  ggplot(aes(x = FGA, y = pts)) + 
  geom_jitter(fill = "#ff02ff", alpha = .3, color = "#ff02ff") + 
  labs(x = "FGA", 
       y = "PTS") 

## fit data

m_lm_sim_fit <- quap(
  alist(
    pts ~ dnorm(mu, sigma), # likelihood
    mu <- a + b * FGA, # linear model
    a ~ dunif(0,10), # prior intercept
    b ~ dunif(0, 3), # prior rate of change (slope)
    sigma ~ dunif(0,10) # prior sd
  ),
  data = sim_dat,
  start = list(a=.01,b=.01,sigma=.01))
precis(m_lm_sim_fit)


# analyze real data 

#plot data
shaq %>% 
ggplot(aes(x = FGA, y = PTS)) + 
  geom_jitter(fill = "#ff02ff", alpha = .3, color = "#ff02ff") + 
  labs(x = "FGA", 
       y = "PTS")

# fit data 

## without mean centering
m_lm_shaq_fit <- quap(
  alist(
    PTS ~ dnorm(mu, sigma), # likelihood
    mu <- a + b * FGA, # linear model
    a ~ dunif(0,10), # prior intercept
    b ~ dunif(0,3), # prior rate of change (slope)
    sigma ~ dunif(0,10) # prior sd
  ),
  data = shaq)
precis(m_lm_shaq_fit)

# with mean centering

FGA_bar <-round(mean(shaq$FGA),0)

m_lm_shaq_fit_cent  <- quap(
  alist(
    PTS ~ dnorm(mu, sigma), # likelihood
    mu <- a + b * (FGA-FGA_bar), # linear model
    a ~ dnorm(20,5), # prior intercept
    b ~ dunif(0,3), # prior rate of change (slope)
    sigma ~ dunif(0,10) # prior sd
  ),
  data = shaq)
precis(m_lm_shaq_fit_cent)


# evaluate the model
m_lm_shaq_smp <- extract.samples(m_lm_shaq_fit_cent, n = 1000)

## Posterior predictive checks 

### regression lines
ggplot(shaq, aes(x = FGA-FGA_bar, y = PTS)) +
  geom_jitter(color = "#ff02ff", 
              fill = "#ff02ff", 
              alpha = .2, 
              size = 2) +
  geom_abline(data = m_lm_shaq_smp, aes(intercept = a, slope = b), 
              color = "#FDB927", 
              linewidth = .1, 
              alpha = .5) +
  labs(x = "FGA", y = "Points")


### data
m_lm_shaq_smp <- extract.samples(m_lm_shaq_fit_cent, n = 1207)
data <- data.frame(m_lm_shaq_smp, FGA=shaq$FGA)

m_lm_shaq_post_pred <- data %>% mutate(PTS = a + b*(FGA-FGA_bar) + rnorm(1207,0,sd))

m_lm_shaq_post_pred %>% 
  ggplot(aes(x = PTS)) + 
  geom_histogram(data = shaq, aes(x=PTS), fill = "#ff02ff", alpha = .5, color = "#ff02ff", bins = 30) +
  geom_histogram(fill = "#FDB927", alpha = .5, color = "#FDB927", bins = 30) + 
  labs(x = "PTS", 
       y = "Frequency") 
