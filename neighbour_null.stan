data {
  int<lower=0> num_data;              // Number of observations
  array[num_data] real num_old;       // Number of old in population (-focal)
  array[num_data] real num_young;     // Number of young in population (-focal)
  array[num_data] int neighbour_age;  // Neighbour old = 1, young = 0
}

parameters {
  real<lower=0, upper=1> theta;       // Probability of neighbour being old
}

model {
  theta ~ beta(num_old, num_young);   // Prior on probability
  
  for(i in 1:num_data){
    neighbour_age[i] ~ bernoulli(theta); 
  }
  
} 

generated quantities {
  array[num_data] int z;
  for(i in 1:num_data){
    z[i] = bernoulli_rng(theta);
  }
}

// array[num_data] real<lower=0, upper=1> logit_old_popn; // Logit proportion population old (-focal)
// theta ~ normal(logit_old_popn, 1); // Prior on probability

// data {
//   int<lower=0> n_data;            // number of observations
//   vector[n_data] prop_old_popn;   // proportion of (population - focal) that is old
//   array[n_data] int n_draws ;        // number of generated quantities to produce for each proportion
// }
// 
// generated quantities {
//   array[n_data,n_data] int z;
//   for(i in 1:n_data){
//     z[,i] = binomial_rng(n_draws, prop_old_popn);
//   }
// }

// data {
//   int<lower=0> n_eles;            // number of elephants
//   int<lower=0> n_expm;            // number of experiments
//   int<lower=0> n_stim;            // number of stimuli
//   vector[n_data] y;               // old neighbour = 1, young neighbour = 0
//   vector[n_data] focal;           // old neighbour = 1, young neighbour = 0
//   vector[n_data] pb_num;          // old neighbour = 1, young neighbour = 0
//   vector[n_data] stim_id;         // old neighbour = 1, young neighbour = 0
// }
// parameters {
//   vector[n_eles] eles_random;
//   vector[n_expm] expm_random;
//   vector[n_stim] stim_random;
// }
// 
// transformed parameters {
//   vector[n_data] x;
//   for(i in 1:n_data){
//     x[i] = 1 + eles_random[focal[i]] + expm_random[pb_num[i]] + stim_random[stim_id[i]];
//   }
// }
// 
// model {
//   y ~ binomial(x, prob = prop_old_popn);
// }


