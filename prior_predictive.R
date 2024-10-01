#### INFORMATION ####
# SCRIPT TO RUN ALL PRIOR PREDICTIVE CHECKS FOR ALL MODELS AND JUST SEE HOW THEY COMPARE TO ONE ANOTHER BECAUSE THE PRIORS HAVE ENDED UP ALL BEING QUITE DIFFERENT AND I DON'T REMEMBER WHY!

#### SET UP ####
#library(tidyverse) ; library(brms)
library(tidyverse, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')

theme_set(theme_bw())

#### LOOKING DIRECTION 1 -- all N(0,1) except for previous action = N(1,1). LOOKS MUCH BETTER WITH N(1,1) THAN WITH N(0,1). ####
rm(list = ls()) ; gc()
load('looking_direction/looking_ordinal_model1_run.RData')

priors
pp_check(lom1_prior, ndraws = 50)

priors_new <- c(
  # focal age
  prior(normal(0,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # interaction
  prior(normal(0,1),     class = b,    coef = age_combo1_2),
  prior(normal(0,1),     class = b,    coef = age_combo1_3),
  prior(normal(0,1),     class = b,    coef = age_combo1_4),
  prior(normal(0,1),     class = b,    coef = age_combo2_1),
  prior(normal(0,1),     class = b,    coef = age_combo2_2),
  prior(normal(0,1),     class = b,    coef = age_combo2_3),
  prior(normal(0,1),     class = b,    coef = age_combo2_4),
  prior(normal(0,1),     class = b,    coef = age_combo3_1),
  prior(normal(0,1),     class = b,    coef = age_combo3_2),
  prior(normal(0,1),     class = b,    coef = age_combo3_3),
  prior(normal(0,1),     class = b,    coef = age_combo3_4),
  prior(normal(0,1),     class = b,    coef = age_combo4_1),
  prior(normal(0,1),     class = b,    coef = age_combo4_2),
  prior(normal(0,1),     class = b,    coef = age_combo4_3),
  prior(normal(0,1),     class = b,    coef = age_combo4_4),
  # stim type
  prior(normal(0,1),     class = b,    coef = stim_typeh),
  prior(normal(0,1),     class = b,    coef = stim_typel),
  # time spline
  prior(normal(0,1),     class = b,    coef = safter_stim_1),
  # action in previous second
  prior(normal(0,1),     class = b,    coef = molook_tminus1_num),
  prior(dirichlet(2,2),  class = simo, coef = molook_tminus1_num1))
lom1_prior_new <- brm(
  formula = look_index ~ 1 + mo(f_age_num) + age_combo + stim_type + # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                           # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                    # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(lom1_prior_new, ndraws = 50)

#### LOOKING DIRECTION 2 -- all N(0,1) except for previous action = N(1,1). LOOKS VERY SIMILAR BUT POSSIBLE BETTER WITH N(0,1) THAN N(1,1) ####
rm(list = ls()) ; gc()
load('looking_direction/looking_ordinal_2bda_run.RData')

priors
pp_check(lom2_prior, ndraws = 50)

priors_new <- c(
  # focal age
  prior(normal(0,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # interaction
  prior(normal(0,1),      class = b,    coef = age_combo1_2),
  prior(normal(0,1),      class = b,    coef = age_combo1_3),
  prior(normal(0,1),      class = b,    coef = age_combo1_4),
  prior(normal(0,1),      class = b,    coef = age_combo2_1),
  prior(normal(0,1),      class = b,    coef = age_combo2_2),
  prior(normal(0,1),      class = b,    coef = age_combo2_3),
  prior(normal(0,1),      class = b,    coef = age_combo2_4),
  prior(normal(0,1),      class = b,    coef = age_combo3_1),
  prior(normal(0,1),      class = b,    coef = age_combo3_2),
  prior(normal(0,1),      class = b,    coef = age_combo3_3),
  prior(normal(0,1),      class = b,    coef = age_combo3_4),
  prior(normal(0,1),      class = b,    coef = age_combo4_1),
  prior(normal(0,1),      class = b,    coef = age_combo4_2),
  prior(normal(0,1),      class = b,    coef = age_combo4_3),
  prior(normal(0,1),      class = b,    coef = age_combo4_4),
  # stim type
  prior(normal(0,1),      class = b,    coef = stim_typeh),
  prior(normal(0,1),      class = b,    coef = stim_typel),
  # before/during/after
  prior(normal(0,1),      class = b,    coef = bdabefore),
  prior(normal(0,1),      class = b,    coef = bdaduring),
  # action in previous second
  prior(normal(0,1),      class = b,    coef = moprev_num),
  prior(dirichlet(2),     class = simo, coef = moprev_num1))
lom2_prior_new <- brm(
  formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = look, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(lom2_prior_new, ndraws = 50)

#### MOVEMENT DIRECTION 1 -- all N(0,1) except for divide sd by number of ordered categories for age and previous movement (so N(0,0.25) and N(0,0.333)). NO CHANGE WHEN SHIFT TO N(0,1) FOR EVERYTHING ####
rm(list = ls()) ; gc()
load('movement_direction/movement_ordinal_model1_run.RData')

priors
pp_check(mom1_prior, ndraws = 50)

priors_new <- c(
  # focal age
  prior(normal(0,1),        class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2),   class = simo, coef = mof_age_num1),
  # interaction between ages
  prior(normal(0,1),        class = b,    coef = age_combo1_2),
  prior(normal(0,1),        class = b,    coef = age_combo1_3),
  prior(normal(0,1),        class = b,    coef = age_combo1_4),
  prior(normal(0,1),        class = b,    coef = age_combo2_1),
  prior(normal(0,1),        class = b,    coef = age_combo2_2),
  prior(normal(0,1),        class = b,    coef = age_combo2_3),
  prior(normal(0,1),        class = b,    coef = age_combo2_4),
  prior(normal(0,1),        class = b,    coef = age_combo3_1),
  prior(normal(0,1),        class = b,    coef = age_combo3_2),
  prior(normal(0,1),        class = b,    coef = age_combo3_3),
  prior(normal(0,1),        class = b,    coef = age_combo3_4),
  prior(normal(0,1),        class = b,    coef = age_combo4_1),
  prior(normal(0,1),        class = b,    coef = age_combo4_2),
  prior(normal(0,1),        class = b,    coef = age_combo4_3),
  prior(normal(0,1),        class = b,    coef = age_combo4_4),
  # stimulus type
  prior(normal(0,1),        class = b,    coef = stim_typel),
  prior(normal(0,1),        class = b,    coef = stim_typeh),
  # time spline
  prior(normal(0,1),        class = b,    coef = safter_stim_1),
  # action in previous second
  prior(normal(0,1),        class = b,    coef = momove_tminus1_num),
  prior(dirichlet(2,2,2,2), class = simo, coef = momove_tminus1_num1))
mom1_prior_new <- brm(
  formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +
    s(after_stim) + mo(move_tminus1_num) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors_new,
  chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(mom1_prior_new, ndraws = 50)

#### MOVEMENT DIRECTION 2 -- all N(0,1) except for previous action = N(1,1). NO CHANGE WHEN SHIFT TO N(0,1) FOR EVERYTHING ####
rm(list = ls()) ; gc()
load('movement_direction/moving_ordinal_2bda_run.RData')

priors
pp_check(mom2_prior, ndraws = 50)

priors_new <- c(
    # focal age
    prior(normal(0,1),      class = b,    coef = mof_age_num),
    prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
    # interaction
    prior(normal(0,1),      class = b,    coef = age_combo1_2),
    prior(normal(0,1),      class = b,    coef = age_combo1_3),
    prior(normal(0,1),      class = b,    coef = age_combo1_4),
    prior(normal(0,1),      class = b,    coef = age_combo2_1),
    prior(normal(0,1),      class = b,    coef = age_combo2_2),
    prior(normal(0,1),      class = b,    coef = age_combo2_3),
    prior(normal(0,1),      class = b,    coef = age_combo2_4),
    prior(normal(0,1),      class = b,    coef = age_combo3_1),
    prior(normal(0,1),      class = b,    coef = age_combo3_2),
    prior(normal(0,1),      class = b,    coef = age_combo3_3),
    prior(normal(0,1),      class = b,    coef = age_combo3_4),
    prior(normal(0,1),      class = b,    coef = age_combo4_1),
    prior(normal(0,1),      class = b,    coef = age_combo4_2),
    prior(normal(0,1),      class = b,    coef = age_combo4_3),
    prior(normal(0,1),      class = b,    coef = age_combo4_4),
    # stim type
    prior(normal(0,1),      class = b,    coef = stim_typeh),
    prior(normal(0,1),      class = b,    coef = stim_typel),
    # before/during/after
    prior(normal(0,1),      class = b,    coef = bdabefore),
    prior(normal(0,1),      class = b,    coef = bdaduring),
    # action in previous second
    prior(normal(0,1),      class = b,    coef = moprev_num), # change to normal(0,1) -- with ordinal model, I think this will mean you are more likely to get move towards when prev = low than get move away when prev = high
    prior(dirichlet(2),     class = simo, coef = moprev_num1))
mom2_prior_new <- brm(
  formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = move, family = cumulative("logit"),
  prior = priors_new,
  chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(mom2_prior_new, ndraws = 50)

#### MOVEMENT BINOMIAL -- all N(-1,1). Shift to N(0,1) gives data falling at very extreme edges ####
rm(list = ls()) ; gc()
load('movement_direction/movement_binomial_run.RData')

priors
pp_check(mbm_prior, ndraws = 50)

priors_new <- c(
  # focal age
  prior(normal(0,1),     class = b,    coef = mof_age_num),   # don't want to assume movement less likely as older
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(0,1),     class = b,    coef = stim_typectd),  # don't want to force movement less likely when stimulus is control
  prior(normal(0,1),     class = b,    coef = stim_typel),    # don't assume less movement for scary stim
  prior(normal(0,1),     class = b,    coef = stim_typeh),    # don't assume less movement for scary stim
  # time spline
  prior(normal(0,1),     class = b,    coef = safter_stim_1),
  # action in previous second
  prior(normal(1,1),     class = b,    coef = move_tminus1_num))
mbm_prior_new <- brm(
  formula = move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors_new,
  chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(mbm_prior_new, ndraws = 50)

#### NEAREST NEIGHBOUR -- all N(0,1) except for previous action = N(1,1) ####
rm(list = ls()) ; gc()
load('nearest_neighbour/neighbour_binomial_run.RData')

priors
pp_check(nbm_prior, ndraws = 50) # y is quite skewed, prior is mostly symmetrical, but data still fall within it
