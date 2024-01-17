// model code adapted from cahpter 1 age estimation -- haven't spoken to Dan about this so may have done it entirely wrong, but currently R isn't complaining!

functions {
}

data {
    int n_obs;       // number of observations
    int n_direct;    // number of looking directions (3)
    int<lower=1, upper=n_direct> looking_direction[n_obs]; // observed looking directions
    vector[n_obs] focal_age;            // age category of individual looking
    vector[n_obs] partner_age;          // age category of partner looking at/side/away
    //vector[n_obs] age_difference;       // age difference between focal and partner
    vector[n_obs] stim_type;            // type of stimulus
    vector[n_obs] time_since_stim;      // seconds since stimulus started (-ve = before, +ve = during/after)
    vector[n_obs] focal_id;             // ID of focal (random effect)
    vector[n_obs] stim_id;              // sound file played (random effect)
    vector[n_obs] playback_id;          // experiment number (random effect)
}

parameters {
  real<lower=0> sigma_age;
  real b_focal;
  real b_partner;
  real b_stimtype;
  real b_time;
  real b_id;
  real b_stimid;
  real b_pbnum;
}

transformed parameters {
  ordered[n_direct-1] thresholds;
  vector<lower=0>[n_obs] exposures;

  // THRESHOLDS FOR LOOKING DIRECTION -- THIS IS WHERE I GET STUCK BECAUSE THE CATEGORIES ARE NOT NUMERIC: BEFORE IT WAS WORKING BY CALCULATING THE OBSERVED AGE GIVEN THE TRUE AGE AND MARGIN FOR ERROR IN AGE ESTIMATION AND THEN ASSIGNING A CATEGORY BASED ON THE THRESHOLDS. THERE ARE NO SUCH THRESHOLDS FOR THESE DATA.
  thresholds[1] = 5;
  thresholds[2] = 10;
  thresholds[3] = 15;
  thresholds[4] = 20;
  
  // Non-centred age. The same as observed_age ~ normal(true_age,sigma_age)
  exposures = focal_age*b_focal + partner_age*b_partner + stim_type*b_stimtype + time_since_stim*b_time + focal_id*b_id + stim_id*b_stimid + playback_id*b_pbnum;
  
}

model {
  for(i in 1:n_obs) {
    looking_direction[i] ~ ordered_logistic(exposures[i], thresholds);
  }
  exposures ~ std_normal();
  sigma_age ~ exponential(0.5);
  b_focal ~ normal(0,1);
  b_partner ~ normal(0,1);
  b_stimtype ~ normal(0,1);
  b_time ~ normal(0,1);
  b_id ~ normal(0,1);
  b_stimid ~ normal(0,1);
  b_pbnum ~ normal(0,1);
}

