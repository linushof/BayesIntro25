
# Exercise 1 (Tossing & Counting) --------------------------------------------------------------


# globe tossing function

sim_tosses <- function(n, p){
  
  sample(c("L", "W"), size=n, replace=T, prob=c(p, 1-p))
  
}

data <- sim_tosses(1e2, .5)


# counting function

counter <- function(data, cp){ 
  
  sides <- length(cp)-1
  
  L <- sum(data=="L")
  W <- sum(data=="W")
  ways <- (cp*sides)^L * ((1-cp)*sides)^W 
  data.frame(cp, ways) 
  
}

counter(c("L", "W", "L"), cp=seq(0,1,.25)) # reproduces example from slides
counter(sim_tosses(100, .5), cp=seq(0,1,.1)) 

# Exercise 2 (Prior knowledge) ---------------------------------------------------------

old_data <- c("L", "W", "L")
old_ways <- counter(old_data, cp=seq(0,1,.25))

new_data <- "W"
new_ways <- counter(new_data, cp=seq(0,1,.25))
  
results <- data.frame(cp = old_ways$cp , 
           old = old_ways$ways , 
           new = new_ways$ways , 
           total = old_ways$ways * new_ways$ways
           )

## with more data

new_data <- sim_tosses(100, .5)


new_data

new_ways <- counter(new_data, cp=seq(0,1,.25))

results <- data.frame(cp = old_ways$cp , 
                      old = old_ways$ways , 
                      new = new_ways$ways , 
                      total = old_ways$ways * new_ways$ways
)




results$prob <- round(results$total/sum(results$total),3)
results

