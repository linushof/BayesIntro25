
// data block
data {
  int<lower=0> N ;
  vector[N] pts ;
  vector[N] min ;
  real min_bar ; 
  vector[N] fga ; 
  real fga_bar ; 
  vector[N] fta ; 
  real fta_bar ;
}

// parameter block
parameters {
  real a ;
  real b1 ;
  real<lower=0,upper=2> b2 ;
  real<lower=0,upper=1> b3 ;
  real<lower=0,upper=10> sigma ;
}

// model block
model {
  a ~  normal( 20 , 8 ) ; 
  b1 ~ normal( 0 , 2 ) ;
  b2 ~ uniform( 0 , 2 ) ;
  b3 ~ uniform( 0 , 1 ) ;
  sigma ~ uniform( 0 , 10 ) ;
  vector[N] mu ;
  mu = a + b1 * (min - min_bar) + b2 * (fga - fga_bar) * 2 + b3 * (fta - fta_bar) ;
  pts ~ normal(mu, sigma) ;
}

