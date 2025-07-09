library(rethinking)
library(tidyverse)

# logistic transformation  ----------------------------------------------

## Interpretation

dat <- tibble(logit = seq(-10,10, length.out = 100) , 
              logistic = inv_logit(logit)) 

dat %>% 
  ggplot(aes(logit, logistic)) +
  geom_line(size = 2)


# prior selection for logistic regression ---------------------------------

## intercept 

dat <-  tibble(a1 = rnorm(1e4, 0,5) ,
               a2 = rnorm(1e4, 0,1) , 
               a3 = rnorm(1e4, 0,.1) ,
               p1 = plogis(a1) , 
               p2 = plogis(a2) ,
               p3 = plogis(a3)) %>% 
  pivot_longer(cols = a1:p3, names_to = "scale", values_to = "value")

dat %>% 
  ggplot(aes(x=value)) +
  geom_density(size = 1) + 
  facet_wrap(~scale, scales = "free") + 
  labs(x = "Logit Prediction",
       y = "Density") #+ 

## slope and intercept prior

N <- 1e2

prior <-  tibble(index = 1:(2*N) , 
                 prior = c(rep("wide",N), rep("narrow",N)) , 
                 a = c(rnorm(N, 0, 5), rnorm(N, 0, .5)) , 
                 b = c(rnorm(N, 0, 5), rnorm(N, 0, .5)))


dat <- expand_grid(prior, x = seq(-3,3, len=100)) %>% 
  mutate(p = plogis(a+b*x))

dat %>% 
  ggplot(aes(x, p, group = index)) +
  geom_line() + 
  facet_wrap(~factor(prior, levels = c("wide", "narrow"))) +
  labs(x = "Logit",
       y = "Logistic")


# Simulation Berkeley admission rates ------------------------------------------------


## Simulation --------------------------------------------------------------

N <- 1000 # number of applicants 
G <- sample(1:2, size = N, replace = TRUE)
D <- rbern(N, ifelse(G==1, .3, .8)) + 1 
accept_rate <- matrix( c(.1, .3, .1, .3), nrow = 2) # without direct discrimination
A <- rbern(N, accept_rate[D, G])

table(G,D)
table(G,A)

dat <- list(A=A, D=D, G=G)


# estimate total effect of G (ignore D)

m1 <- ulam(
  alist(
    # likelihood
    A ~ dbern(p) , 
    logit(p) <- a[G]  , 
    
    # priors
    a[G] ~ dnorm(0,1) 
  ) , 
  data=dat, 
  chains = 4, 
  cores = 4, 
  file = "models/session_10_m1" # loads model fit file when previously fitted and stored
)

traceplot(m1)
precis(m1, depth = 2) # coefficients on log odds scale
plogis(coef(m1)) # interpret as probability 


## estimate direct effect of G (include D as covariate)

m2 <- ulam(
  alist(
    # likelihood
    A ~ dbern(p) , 
    logit(p) <- a[G, D] , 
    
    #priors
    matrix[G, D]:a ~ dnorm(0,1) 
    
  ) , 
  data = dat, 
  chains = 4, 
  cores = 4,  
  file = "models/session_10_m2"
)

precis(m2, depth = 3)  
inv_logit(coef(m2))
traceplot(m2)


# Real Data ---------------------------------------------------------------

# load and prepare data

data("UCBadmit", "UCBadmit_long")
dat_wide <- UCBadmit
dat_long <- UCBadmit_long


dat <- list(
  A = dat_wide$admit , 
  N = dat_wide$applications , 
  G = ifelse(dat_wide$applicant.gender=="female", 1, 2) , 
  D = as.integer(dat_wide$dept)
  )

# estimate total effect of G
m3 <- ulam(
  alist(
    # likelihood
    A ~ dbinom(N, p) ,
    logit(p) <- a[G] ,
    
    # priors
    a[G] ~ dnorm(0,1) 
  ) , 
  data = dat, 
  chains = 4, 
  cores = 4,
  file = "models/session_10_m3"
)

precis(m3, depth = 2)
inv_logit(coef(m3))
traceplot(m3)

# posterior distribution of differences

m3.post <- extract.samples(m3)

posterior <-  tibble(p1 = plogis(m3.post$a[,1]) , 
                     p2 = plogis(m3.post$a[,2]) , 
                     diff = p1 - p2)


ggplot(posterior, aes(diff)) + 
  geom_density(linewidth = 2) +
  labs(x="Admission Probability G1 - Admission Probability G2" , 
       y="Density") 
  

### estimate direct effect 

m4 <- ulam(
  alist(
    # likelihood
    A ~ dbinom(N, p) ,
    logit(p) <- a[G, D] ,
    
    # prior
    matrix[G, D]:a ~ dnorm(0,1) 
  ) , 
  data = dat,
  chains = 4,
  cores = 4,
  file = "models/session_10_m4"
)

precis(m4, depth = 3)
inv_logit(coef(m4))
traceplot(m4)

#### posterior distribution of differences
m4.post <- extract.samples(m4)

PrA <- plogis(m4.post$a)

diff_prob_D <- sapply(1:6, function(i) PrA[, 1, i] - PrA[, 2, i])
diffs <- as.data.frame(diff_prob_D)
names(diffs) <- dep

diffs |> 
  pivot_longer(cols = D1:D6, names_to = "Department", values_to = "Diff")  |> 
  ggplot(aes(Diff, color = Department)) + 
  geom_density(alpha = .3, linewidth = 2) +
  labs(x="Admission Probability G1 - Admission Probability G2" , 
       y="Density")