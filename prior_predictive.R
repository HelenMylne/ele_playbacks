#### INFORMATION ####
# SCRIPT TO RUN ALL PRIOR PREDICTIVE CHECKS FOR ALL MODELS AND JUST SEE HOW THEY COMPARE TO ONE ANOTHER BECAUSE THE PRIORS HAVE ENDED UP ALL BEING QUITE DIFFERENT AND I DON'T REMEMBER WHY!

#### SET UP ####
#library(tidyverse) ; library(brms)
library(tidyverse, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')

theme_set(theme_bw())

#### LOOKING DIRECTION 1 -- all N(0,1) except for previous action = N(1,1) ####
load('looking_direction/looking_ordinal_model1_run.RData')

priors
check <- pp_check(lom1_prior, ndraws = 50)
check +
  scale_x_continuous(breaks = c(1,2,3)) +
  labs(x = 'looking direction (1 = away, 3 = towards)',
       # title = 'LOM1',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_LOM1.png', path = 'looking_direction/',
       width = 1200, height = 1200, units = 'px')

#### LOOKING DIRECTION 2 -- all N(0,1) except for previous action = N(1,1) ####
rm(list = ls()) ; gc()
load('looking_direction/looking_ordinal_2bda_run.RData')

priors
check <- pp_check(lom2_prior, ndraws = 50)
check +
  scale_x_continuous(breaks = c(1,2,3)) +
  labs(x = 'looking direction (1 = away, 3 = towards)',
       # title = 'LOM2',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_LOM2.png', path = 'looking_direction/',
       width = 1200, height = 1200, units = 'px')

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
  prior(normal(1,1),        class = b,    coef = momove_tminus1_num),
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

check <- pp_check(mom1_prior_new, ndraws = 50)
check +
  scale_x_continuous(breaks = 1:5)+
  labs(x = 'movement direction (1 = away, 5 = towards)',
       # title = 'MOM1',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_MOM1.png', path = 'movement_direction/',
       width = 1200, height = 1200, units = 'px')

#### MOVEMENT DIRECTION 2 -- all N(0,1) except for previous action = N(1,1) ####
rm(list = ls()) ; gc()
load('movement_direction/moving_ordinal_2bda_run.RData')

priors
check <- pp_check(mom2_prior, ndraws = 50)
check +
  labs(x = 'movement direction (1 = away, 5 = towards)',
       # title = 'MOM2',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_MOM2.png', path = 'movement_direction/',
       width = 1200, height = 1200, units = 'px')

#### MOVEMENT BINOMIAL -- all N(-1,1) ####
rm(list = ls()) ; gc()
load('movement_direction/movement_binomial_run.RData')

priors
check <- pp_check(mbm_prior, ndraws = 50)
check +
  scale_x_continuous(breaks = c(0, 1))+
  labs(x = 'movement probability',
       # title = 'MBM',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_MBM.png', path = 'movement_direction/',
       width = 1200, height = 1200, units = 'px')

#### NEAREST NEIGHBOUR -- all N(0,1) except for previous action = N(1,1) ####
rm(list = ls()) ; gc()
load('nearest_neighbour/neighbour_binomial_run.RData')

priors
check <- pp_check(nbm_prior, ndraws = 50)
check +
  scale_x_continuous(breaks = c(0, 1))+
  labs(x = 'nearest neighbour probability',
       # title = 'NBM',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_NBM.png', path = 'nearest_neighbour/',
       width = 1200, height = 1200, units = 'px')

