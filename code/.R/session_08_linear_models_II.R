rm(list=ls())

# load packages
pacman::p_load(tidyverse, rethinking, ggdist, gghalves, ggpubr)



# Starting Example --------------------------------------------------------

# simulate data by assuming that X differs Y differently (depending on Z)
N <- 100
dat <- tibble(X = rnorm(N, 0, 1) , 
              Y0 = X + rnorm(N,0,.3) , 
              Y1 = -1*X + rnorm(N,0,.3)) |> 
  pivot_longer(Y0:Y1, names_to = "Z", values_to = "Y", names_prefix = "y") |> 
  mutate(Z=as.factor(Z))

# plot data
# we clearly see that the data stems from two different data-generating processes here
p1 <- dat |> 
  ggplot(aes(X,Y)) +
  geom_jitter(size=2) + 
  theme_minimal()
p1

# if we simply estimate the relationship between X and Y while ignoring the influence of Z,
# we obtain  a spurious/wrong estimate
m1 <- quap(
  alist(
    #likelihood
    Y ~ dnorm(mu, sigma) , 
    mu <- a + b*X , 
    
    # prior
    a ~ dcauchy(0,5),
    b ~ dcauchy(0,5),
    sigma ~ dgamma(1,2)
  ) , 
  data=dat)

# the result falsely indicate that there is no relationship between X and Y 
precis(m1)
p1 <- p1 + geom_smooth(method="lm")
p1

# we now look at the data by highlighting from which process/group they stem
p2 <- dat |> 
  ggplot(aes(X,Y, color=Z)) +
  geom_jitter(size=2) + 
  theme_minimal()
p2
# the code below estimates two independent regression line (one for Z=1 and one for Z=0)
# more on this code in later session on multilevel modeling
# rerun this code if it gets you an error, the first time(s)
m2 <- quap(
  alist(
    #likelihood
    Y ~ dnorm(mu, sigma) , 
    mu <- a + b[Z]*X , 
    
    # prior
    a ~ dcauchy(0,1),
    b[Z] ~ dcauchy(0,1),
    sigma ~ dgamma(1,2)
  ) , 
  data=dat)

# the results now show the correct estimates for both groups
precis(m2, depth=2)
p2 <- p2 + geom_smooth(method="lm")
p2


# Exercise: Multiple Regression Model --------------------------------------------------------------------

## generative simulation 

sim_pts <- function(FGA , # number of field goal attempts (>= 0)
                    FTA , # number of free throw attempts (>= 0)
                    hFG , # hit rate (accuracy) field goal attempts (between 0 and 1)
                    hFT # hit rate (accuracy) field goal attempts (betweem 0 and 1)
                    ){ 
  
  # simulate made free throws
  FT <- rbinom(1, FTA, prob = hFT)
  
  # simulate number of points from field goal attempts  
  FG <- rbinom(1, FGA, prob = hFG) * 2
  
  # count points and add some noise
  FT + FG + rnorm(1, mean=0, sd=4)
  
}    

# simulate data for 1000 games
N_games <- 1e3

# create values for each variable in sim_pts  
hFG <-  .7
hFT <-  .4
FGA <- round(rgamma(N_games, 20, 1),0)
FTA <- round(rgamma(N_games, 5, 1),0)
pts <- vector("numeric", length = N_games)


set.seed(1471)
for (i in seq_along(1:N_games)) { 
  
  pts[i] <-  sim_pts(FGA[i], FTA[i], hFG, hFT)
  
  }

dat <- tibble(FGA, FTA, pts)  

ggplot(dat, aes(x = pts)) + 
  geom_histogram(fill = "#ff02ff", alpha = .5, color = "#ff02ff", bins = 30) + 
  labs(x = "PTS", 
       y = "Frequency")


m1_shaq <- quap(
  alist(
    pts ~ dnorm(mu, sd), 
    mu <- a + b_1 * FGA * 2 + b_2 * FTA,
    a ~ dunif(0,10),
    b_1 ~ dunif(0, 1),
    b_2 ~ dunif(0, 1),
    sd ~ dunif(0,8)
  ),
  data = dat)
precis(m1_shaq)

plot(m1_shaq)
pairs(m1_shaq, pars = c("a", "b_1", "b_2", "sd"))

## mean-centering

FGA_bar <- round(mean(dat$FGA),0)
FTA_bar <- round(mean(dat$FTA),0)

m2_shaq <- quap(
  alist(
    pts ~ dnorm(mu, sd), 
    mu <- a + b_1 * (FGA-FGA_bar) * 2 + b_2 * (FTA-FTA_bar), 
    a ~ dnorm(20,8), 
    b_1 ~ dunif(0, 3),
    b_2 ~ dunif(0, 1),
    sd ~ dunif(0,8) 
  ),
  data = dat)
precis(m2_shaq)

plot(m2_shaq)
pairs(m2_shaq, pars = c("a", "b_1", "b_2", "sd"))


# load and prepare data 


# Pipe --------------------------------------------------------------------

shaq <- read_csv("data/shaq.csv")
dat <- list(FGA = shaq$FGA ,
            FTA = shaq$FTA , 
            PTS = shaq$PTS ,
            MIN = shaq$Minutes ,
            FTA_bar = round(mean(shaq$FTA),0) , 
            FGA_bar = round(mean(shaq$FGA),0) , 
            MIN_bar = round(mean(shaq$FGA),0))

# without mediator

ggplot(shaq, aes(x = Minutes, y = PTS)) + 
  geom_point(size = 2, color =  "#ff02ff", alpha = .5) +
  labs(x = "Minutes", 
       y = "Points") 

m3_shaq <- quap(
  alist(
    PTS ~ dnorm(mu, sd),
    mu <- a + b_1 * (MIN - MIN_bar),
    a ~ dnorm(20, 8),
    b_1 ~ dunif(0, 2), 
    sd ~ dunif(0,8) 
  ),
  data = dat)
precis(m3_shaq)

# with mediator

m4_shaq <- quap(
  alist(
    PTS ~ dnorm(mu, sd), 
    mu <- a + b_1 * (MIN- MIN_bar) + b_2 * (FGA - FGA_bar) * 2 + b_3 * (FTA - FTA_bar),
    a ~ dnorm(20, 8),
    b_1 ~ dnorm(0, 2), 
    b_2 ~ dunif(0, 2), 
    b_3 ~ dunif(0, 1), 
    sd ~ dunif(0,8)
  ),
  data = dat)
precis(m4_shaq)



# Fork --------------------------------------------------------------------

diamonds$clarity <- as.numeric(diamonds$clarity)
diamonds$cut <- as.numeric(diamonds$cut)
diamonds$color <- as.numeric(diamonds$color)


ggplot(diamonds, aes(y = price, x = as.factor(clarity))) + 
  stat_halfeye(adjust = 1.5, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .15, outliers = FALSE, alpha = .3) +
  geom_half_point(side = "l", range_scale = .4, alpha = .1, size =.1) +
  labs(x="Clarity", 
       y="Price") +
  theme_minimal(base_size = 20) 

ggplot(diamonds, aes(y = price, x = as.factor(cut))) + 
  stat_halfeye(adjust = 1.5, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .15, outliers = FALSE, alpha = .3) +
  geom_half_point(side = "l", range_scale = .4, alpha = .1, size =.1) +
  labs(x="Cut", 
       y="Price") +
  theme_minimal(base_size = 20) 

ggplot(diamonds, aes(y = price, x = as.factor(color*-1))) + 
  stat_halfeye(adjust = 1.5, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .15, outliers = FALSE, alpha = .3) +
  geom_half_point(side = "l", range_scale = .4, alpha = .1, size =.1) +
  labs(x="Color", 
       y="Price") +
  theme_minimal(base_size = 20) 

ggplot(diamonds, aes(y = price, x = carat)) + 
  geom_jitter(alpha = .1, size =.1) +
  theme_minimal()


dat <- list(
  p = standardize(diamonds$price), 
  cl = standardize(diamonds$clarity),
  ca = standardize(diamonds$carat)
)

m1 <- quap(
  alist(
    p ~ dnorm(mu, sd),
    mu <- a + b * cl, 
    a ~ dnorm(0, .1), 
    b ~ dnorm(0, .5), 
    sd ~ dexp(1) 
  ),
  data = dat)


precis(m1)


m2 <- quap(
  alist(
    p ~ dnorm(mu, sd),
    mu <- a + b * ca,
    a ~ dnorm(0, .1), 
    b ~ dnorm(0, .5), 
    sd ~ dexp(1)
  ),
  data = dat)


precis(m2)


m3 <- quap(
  alist(
    cl ~ dnorm(mu, sd),
    mu <- a + b * ca,
    a ~ dnorm(0, .1), 
    b ~ dnorm(0, .5), 
    sd ~ dexp(1)),
  data = dat)


precis(m3)


m4 <- quap(
  alist(
    p ~ dnorm(mu, sd),
    mu <- a + b1 * cl + b2 * ca,
    a ~ dnorm(0, .1), 
    b1 ~ dnorm(0, .5), 
    b2 ~ dnorm(0, .5), 
    sd ~ dexp(1)
  ),
  data = dat)


precis(m4)


# Appendix: Collider Bias ----------------------------------------------------------------


set.seed(123)

# Simulate 10,000 students
n <- 10000

# Grades and athleticism are independent in the population
grades <- rnorm(n, mean = 100, sd = 10)
athleticism <- rnorm(n, mean = 100, sd = 10)

min <- min(grades)
max <- max(grades)
grades <- runif(n, min=min, max=max)
athleticism <- runif(n, min=min, max=max)
cor(grades, athleticism)

# Elite university admission depends on either being smart or athletic
# Use a logistic model to determine admission probability
logit_admission <- 0.05 * grades + 0.05 * athleticism - 10
prob_admission <- plogis(logit_admission)

# Simulate admission decisions
admitted <- rbinom(n, 1, prob_admission)

# Check correlation only among admitted students
grades_admitted <- grades[admitted == 1]
athleticism_admitted <- athleticism[admitted == 1]
cor_admitted <- cor(grades_admitted, athleticism_admitted)

# Check correlation in full population
cor_all <- cor(grades, athleticism)

cat("Correlation in full population:", round(cor_all, 3), "\n")
cat("Correlation among admitted students:", round(cor_admitted, 3), "\n")


