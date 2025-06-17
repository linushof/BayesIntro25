library(tidyverse)

sim_tosses <- function(n, p){
  
  sample(c("L", "W"), size=n, replace=T, prob=c(p, 1-p))
  
}

# Exercise 3: Applying Bayes' rule --------------------------------------------------------------

# define function

compute_post <- function(data, candidates, n){
  
  L <- sum(data=="L") # data 
  
  likelihood <- dbinom(L, n, prob = candidates$cp) # aka the 'likelihood'
  
  posterior <- likelihood * candidates$prior # updating 
  
  posterior_norm <- posterior/sum(posterior) # standardization
  
  data.frame(candidates, lh=round(likelihood, 3), post=round(posterior_norm,3))
  
}

# simulate data
set.seed(1671)
n <- 1000
data <- sim_tosses(n, p = .5)

# compute posterior

candidates <- tibble(cp = seq(0,1,.1) , 
                     prior = rep(1/length(cp), length(cp)))
candidates

estimation <- compute_post(data, candidates, n)
estimation

# check results

#source("code/.R/dark_theme.R")
estimation %>% 
  pivot_longer(cols = c(prior,post), names_to = "type", values_to = "probability") %>% # transform tata
  ggplot(aes(x=cp, y = probability, color = type, linetype = type)) + 
  geom_line(linewidth = 2) + 
  labs(x = "Candidates", 
       y = "Probability", 
       color = "Probability Type",
       linetype = "Probability Type") 


# Step-by-step updating ---------------------------------------------------

# storage for each new data point

samples <- vector("numeric", n)
results <- vector("list", n)

# update posterior after each new data point (loop over data points) and set previous posterior as new prior

for (i in seq_along(1:n)){
  
  estimation <- compute_post(data[i], candidates, n=1)
  results[[i]] <- expand_grid(n = i, estimation)
  
  candidates$prior <- estimation$post
  
}

# plot updating
label <- tibble(n = 1:n,  data)
plot <- results %>% 
  bind_rows() %>% 
  pivot_longer(cols = c(prior, post), names_to = "type", values_to = "probability") %>% 
  ggplot(aes(x=cp, y = probability, color = type)) + 
  facet_wrap(~n) +
  geom_line(linewidth = 2, aes(linetype = type)) + 
  labs(x = "Candidates", 
       y = "Probability", 
       color = "Probability Type",
       linetype = "Probability Type") #+ 

plot + geom_text(
  data    = label,
  mapping = aes(x = -Inf, y = -Inf, label = data),
  hjust   = -1,
  vjust   = -12, 
  color = 'black'
)

#ggsave("materials/plots/session_04_bayesian_updating_stepwise.png", height = 7, width = 12)


# Posterior ---------------------------------------------------------------

# simulate and estimate new data

p <- .5
n <- 10
candidates <- tibble(cp = seq(0,1,.01) , prior = rep(1/length(cp), length(cp)))

set.seed(12123)
estimation <- compute_post(sim_tosses(n,p), candidates, n)


# plot posterior 
p <- estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", linewidth = 1) + 
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability") #+
#dark_theme()

#ggsave("materials/plots/session_04_posterior.png", height = 5, width = 6)
p + geom_area(fill = "#F8766D", alpha = 0.4)
# ggsave("materials/plots/session_04_posterior_fill.png", height = 5, width = 6)


# Percentile intervals ----------------------------------------------------

# sample from posterior
estimation
posterior_samples <- sample(estimation$cp, prob = estimation$post, size = 1e4, replace = TRUE) 

median(posterior_samples)

# 50%
PI50 <- quantile(posterior_samples, probs = c(.25, .75))

estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", linewidth = 1) + 
  geom_ribbon(data = subset(estimation, cp >= PI50[1] & cp <= PI50[2]),
              aes(ymin = 0, ymax = post),
              fill = "#F8766D",
              alpha = 0.4) +
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability") 
#ggsave("materials/plots/session_04_pi50.png", height = 5, width = 6)


# 80%
PI80 <- quantile(posterior_samples, probs = c(.10, .90))

estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", linewidth = 1) + 
  geom_ribbon(data = subset(estimation, cp >= PI80[1] & cp <= PI80[2]),
              aes(ymin = 0, ymax = post),
              fill = "#F8766D",
              alpha = 0.4) +
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability") 

#ggsave("materials/plots/session_04_pi80.png", height = 5, width = 6)



# Highest density percentile intervals ------------------------------------

# 50% 

#install.packages('HDInterval')
library(HDInterval)

HPDI_lower <- hdi(posterior_samples, .5)[1]
HPDI_upper <- hdi(posterior_samples, .5)[2]

estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", linewidth = 1) + 
  geom_ribbon(data = subset(estimation, cp >= HPDI_lower & cp <= HPDI_upper),
              aes(ymin = 0, ymax = post),
              fill = "#F8766D",
              alpha = 0.4) +
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability")

#ggsave("materials/plots/session_04_hpid50.png", height = 5, width = 6)

# 80 % 
HPDI_lower <- hdi(posterior_samples, .80)[1]
HPDI_upper <- hdi(posterior_samples, .80)[2]

estimation %>%
  ggplot(aes(x=cp, y = post)) + 
  geom_line(linetype = "dashed", color = "#F8766D", linewidth = 1) + 
  geom_ribbon(data = subset(estimation, cp >= HPDI_lower & cp <= HPDI_upper),
              aes(ymin = 0, ymax = post),
              fill = "#F8766D",
              alpha = 0.4) +
  theme_minimal() + 
  labs(x = "Candidates", 
       y = "Probability")

#ggsave("materials/plots/session_04_hpid80.png", height = 5, width = 6)