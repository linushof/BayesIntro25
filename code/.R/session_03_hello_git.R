# load packages

library(tidyverse)

#read data

dat <- read_csv('data/stocks.csv')

# compute returns

A_return <- (dat[nrow(dat),'stock_A']-dat[1,'stock_A'])/dat[1,'stock_A'] # 10-years 
A_ann <- (1+A_return)^(1/10)-1 # annualized

B_return <- (dat[nrow(dat),'stock_B']-dat[2,'stock_B'])/dat[2,'stock_B'] # 10-years 
B_ann <- (1+B_return^(1/10))-1 # annualized


# visualize stocks

## pivot data into long format

dat_long <- dat %>% 
  pivot_longer(cols=stock_A:stock_B, names_to = 'Stock', values_to = 'Points')

## plot stock development

ggplot(dat_long, aes(x=year, y=Points, color=Stock)) +
  geom_line(linewidth = 1) + 
  labs(title='BLUE & RED Stock', 
       subtitle='10 YR RETURN') +
  theme_minimal()