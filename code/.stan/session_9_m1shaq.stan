
// data block
data {
  int<lower=0> N ;
  vector[N] pts ;
}

// parameter block
parameters {
  real mu ;
  real<lower=0,upper=10> sigma;
}

// model definition
model {
  sigma ~ uniform( 0 , 10 );
    mu ~ normal( 20 , 8 );
    pts ~ normal( mu , sigma );
}

