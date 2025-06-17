#data <- read_xlsx('data/tossing.xlsx', col_names = TRUE) 
#write_csv(data, "data/globetossing.csv")


# preparation -------------------------------------------------------------

# load packages
library(tidyverse)
library(readxl)
library(scico)

# read and prepare data
data <- read_csv("data/globetossing.csv")

# collect individual estimates of the prior estimates (0 tosses) and posterior estimates (25 tosses, 9 land)
individual <- data %>%
  pivot_longer(cols = P0:P100, names_to = 'cp', names_prefix = 'P', values_to = 'prob') %>% 
  mutate(cp = as.double(cp)/100, 
         prob = prob/100)

# compute the normalized average estimates for prior/post across all students
crowd <- individual %>%
  group_by(Time, cp) %>% 
  summarise(total = sum(prob)) %>%
  mutate(prob = total/sum(total)) 

# define function for actual Bayesian updating 
compute_post <- function(prior){
  
  likelihood <- dbinom(9, 25, prob = prior$cp) # 'likelihood' for 9 land out of 25 tosses
  posterior <- likelihood * prior$prob # updating 
  posterior_norm <- posterior/sum(posterior) # standardization
  data.frame(prior, lh=round(likelihood, 3), post=round(posterior_norm,3))
  
}

# know we do the optimal Bayesian updating ... 


# ... assuming no prior knowledge (flat prior --> purely data-driven)
flat_prior <- tibble(cp = seq(0,1,.1) , 
                     prob = 1/11)
naive_post <- compute_post(flat_prior)

# ...assuming prior knowledge, separately for each student  

individual_priors <- individual %>% filter(Time=="Prior") 

N <- length(unique(individual$Student))
prior_list <- vector('list', N)
posterior_list <- vector('list', N)

for (i in 1:N){
  prior_list[[i]] <-  individual_priors %>% filter(Student==paste('S',i, sep = ''))
  posterior_list[[i]] <- compute_post(prior_list[[i]]) 
}

posterior_list[[9]]$post <- rep(0,11) # because of 'bad' prior setting (all posteriors would actually be 0)
individual_posteriors <- bind_rows(posterior_list) 

## assuming the prior of the entire group 

crowd_prior <- crowd %>% filter(Time=="Prior") 
crowd_posterior <- compute_post(crowd_prior)



# plots -------------------------------------------------------------------

# for each student, show student prior (yellow), student posterior (pink), 
# posterior from optimally updating the prior knowledge (green), and the
# purely data-driven Bayesian (white)
individual %>% 
  ggplot(aes(cp, y=prob, color = Time)) + 
  facet_wrap(~Student, nrow=2) + 
  geom_vline(xintercept = .29, color = 'black', linewidth=1) +
  geom_line(data=individual_posteriors, aes(y=post), color = 'green', linewidth=1, linetype = 'dashed') +
  geom_line(data=naive_post, aes(y=post), color = 'white', linewidth=1, linetype = 'dashed') +
  geom_line(linewidth=1.5, alpha = 1) +
  scale_x_continuous(breaks=seq(0,1,.2)) +
  scale_color_scico_d(palette = 'buda', begin = .1, end = .9) + 
  theme_dark()


# do the same, but now assuming the prior of the entire group
crowd %>% 
  ggplot(aes(cp, y=prob, color = Time)) + 
  geom_vline(xintercept = .29, color = 'black', linewidth=1) +
  geom_line(data=crowd_posterior, aes(y=post), color = 'green', linewidth=1, linetype = 'dashed') +
  geom_line(data=naive_post, aes(y=post), color = 'white', linewidth=1, linetype = 'dashed') +
  geom_line(linewidth=1.5, alpha = 1) +
  scale_x_continuous(breaks=seq(0,1,.2)) +
  scale_color_scico_d(palette = 'buda', begin = .1, end = .9) + 
  theme_minimal()
