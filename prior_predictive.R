#### INFORMATION ####
# SCRIPT TO RUN ALL PRIOR PREDICTIVE CHECKS FOR ALL MODELS AND JUST SEE HOW THEY COMPARE TO ONE ANOTHER BECAUSE THE PRIORS HAVE ENDED UP ALL BEING QUITE DIFFERENT AND I DON'T REMEMBER WHY!

#### SET UP ####
#library(tidyverse) ; library(brms)
library(tidyverse, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')

theme_set(theme_bw())

#### LOOKING DIRECTION 1c -- all N(0,1) except for previous action = N(1,1) ####
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

#### LOOKING DIRECTION 2c -- all N(0,1) except for previous action = N(1,1) ####
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

#### LOOKING DIRECTION 2p -- all N(0,1) ####
rm(list = ls()) ; gc()
load('looking_direction/looking_noprev_2bda_run.RData')

priors
check <- pp_check(lom_noprev_prior, ndraws = 50)
check +
  scale_x_continuous(breaks = c(1,2,3)) +
  labs(x = 'looking direction (1 = away, 3 = towards)',
       # title = 'LOM2',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_LOM_noprev.png', path = 'looking_direction/',
       width = 1200, height = 1200, units = 'px')

#### MOVEMENT DIRECTION 1c -- all N(0,1) except for previous action = N(1,1) ####
rm(list = ls()) ; gc()
load('movement_direction/ordinal_withprev/movement_ordinal_model1_run.RData')

priors
pp_check(mom1_prior, ndraws = 50)

check <- pp_check(mom1_prior, ndraws = 50)
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

#### MOVEMENT DIRECTION 2c -- all N(0,1) except for previous action = N(1,1) ####
rm(list = ls()) ; gc()
load('movement_direction/ordinal_withprev/moving_ordinal_2bda_run.RData')

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

#### MOVEMENT DIRECTION 2p -- all N(0,1) ####
rm(list = ls()) ; gc()
load('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')

priors
check <- pp_check(mom2_prior, ndraws = 50)
check +
  labs(x = 'movement direction (1 = away, 5 = towards)',
       # title = 'MOM2',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_MOM_noprev.png', path = 'movement_direction/',
       width = 1200, height = 1200, units = 'px')

#### MOVEMENT BINOMIAL c -- all N(-1,1) ####
rm(list = ls()) ; gc()
load('movement_direction/binomial_withprev/movement_binomial_run.RData')

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

#### MOVEMENT BINOMIAL p -- all N(-1,1) ####
rm(list = ls()) ; gc()
load('movement_direction/binomial_noprev/movement_noprev_run.RData')

priors
check <- pp_check(prop_prior, ndraws = 50)
check +
  scale_x_continuous(breaks = c(0, 1))+
  labs(x = 'movement probability',
       # title = 'MBM',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_MBM_noprev.png', path = 'movement_direction/',
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


#### NEAREST NEIGHBOUR -- all N(-1,1) ####
rm(list = ls()) ; gc()
load('nearest_neighbour/neighbour_noprev_run.RData')

priors
check <- pp_check(nbm_prior, ndraws = 50)
check +
  scale_x_continuous(breaks = c(0, 1))+
  labs(x = 'nearest neighbour probability',
       # title = 'NBM',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

priors <- c(
  # age combination
  prior(normal(-1,1),      class = b,    coef = age_combo1_1),
  prior(normal(-1,1),      class = b,    coef = age_combo1_2),
  prior(normal(-1,1),      class = b,    coef = age_combo1_3),
  prior(normal(-1,1),      class = b,    coef = age_combo1_4),
  prior(normal(-1,1),      class = b,    coef = age_combo2_1),
  prior(normal(-1,1),      class = b,    coef = age_combo2_2),
  prior(normal(-1,1),      class = b,    coef = age_combo2_3),
  prior(normal(-1,1),      class = b,    coef = age_combo2_4),
  prior(normal(-1,1),      class = b,    coef = age_combo3_1),
  prior(normal(-1,1),      class = b,    coef = age_combo3_2),
  prior(normal(-1,1),      class = b,    coef = age_combo3_3),
  prior(normal(-1,1),      class = b,    coef = age_combo3_4),
  prior(normal(-1,1),      class = b,    coef = age_combo4_1),
  prior(normal(-1,1),      class = b,    coef = age_combo4_2),
  prior(normal(-1,1),      class = b,    coef = age_combo4_3),
  prior(normal(-1,1),      class = b,    coef = age_combo4_4),
  # stim type
  prior(normal(-1,1),      class = b,    coef = stim_typeh),
  prior(normal(-1,1),      class = b,    coef = stim_typel),
  # before/during/after
  prior(normal(-1,1),      class = b,    coef = bdabefore),
  prior(normal(-1,1),      class = b,    coef = bdaduring))

## prior predictive check
num_chains <- 4
num_iter <- 2000
nbm_prior <- brm(
  formula = action ~ 0 + age_combo + stim_type + bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

pp_check(nbm_prior) # y is quite skewed, prior is mostly symmetrical, but data still fall within it

ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_NBM_noprev.png', path = 'nearest_neighbour/',
       width = 1200, height = 1200, units = 'px')
