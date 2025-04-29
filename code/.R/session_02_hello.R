# Exercise 1

x <- 10
(((x + 2) * 3) - 6) / 3

# Exercise 2

o1 <- 10
o2 <- o1 + 2
o3 <- o2 * 3
o4 <- o3 - 6
o5 <- o4 / 3

o5*3 == o4
o4 + 6 == o3
o3 / 3 == o2
o2 - 2 == o1

# Exercise 3

x <- 1:5
(((x + 2) * 3) - 6) / 3

# Exercise 4 

n_obs <- 5
dat <- data.frame(v1 = rnorm(n_obs, mean=1),
                  v2 = rnorm(n_obs, mean=2),
                  v3 = rnorm(n_obs, mean=3),
                  v4 = rnorm(n_obs, mean=4),
                  v5 = rnorm(n_obs, mean=5))

c(mean(dat[,1]), mean(dat[,2]), mean(dat[,3]), mean(dat[,4]), mean(dat[,5]))
sapply(dat, mean)
?sapply()
colMeans(dat)


# Exercise 5

sequence <- 1:5
output <- vector('numeric', length(sequence))

for (i in sequence){ 
  
  output[i] <- mean(dat[,i])
  
  }

output


# Exercise 6

# install.packages("ggplot2")
library(ggplot2)


# visualization -----------------------------------------------------------

# Build dataset with 2 groups and different distributions
group_n <- 1000 # number of observations per group
group <- c( rep("Group 1", group_n), rep("Group 2", group_n) ) # create grouping variable
value <-  c( rnorm(group_n, mean = 0), rnorm(group_n, mean=4)) # generate random values for both groups
data <- data.frame(group, value) # store variables in a data frame

# Represent it
ggplot(data, mapping = aes(x=value, fill = group) ) + 
  geom_histogram(bins = 100, position = "identity", alpha = .7, color = "gray")

ggplot(data, mapping = aes(x=value, fill = group)) + # data that should be plotted
  geom_histogram(position = "identity", alpha = .5, color = "gray") + # histogram & settings
  scale_fill_manual(values=c("#69b3a2", "#404080")) + #fill color of histogram bars
  theme_dark() # plot theme


# Exercise 7

ggplot(data, mapping = aes(x=value, fill = group)) + # data that should be plotted
  geom_histogram(position = "identity", alpha = .5, color = "gray") + # histogram & settings
  scale_fill_manual(values=c("#69b3a2", "#404080")) + # fill color of histogram bars
  theme_dark() + # plot theme
  labs(title = "Distribution of Group 1 vs. Group 2",
       x = "Value", 
       y = "Frequency",
       fill = "Group") + 
  facet_wrap(~group, nrow = 2)


#Exercise 8

data$value_2 <- c( data[1:1000, "value"]*2 + rnorm(group_n, mean = 0) ,  
                   data[1001:2000, "value"]*-.5 + rnorm(group_n, mean = 0))

ggplot(data, mapping = aes(x=value, y = value_2, color = group)) + # data that should be plotted
  geom_point(alpha = .5) + # histogram & settings
  scale_color_manual(values=c("#69b3a2", "#404080")) + # fill color of histogram bars
  labs(title = "Distribution of Group 1 vs. Group 2",
       x = "Value 1", 
       y = "Value 2",
       color = "hfsio") + 
  theme_dark() + # plot theme
  facet_wrap(~group)
