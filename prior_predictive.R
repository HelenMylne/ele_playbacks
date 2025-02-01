#### INFORMATION ####
# SCRIPT TO RUN ALL PRIOR PREDICTIVE CHECKS FOR ALL MODELS AND JUST SEE HOW THEY COMPARE TO ONE ANOTHER BECAUSE THE PRIORS HAVE ENDED UP ALL BEING QUITE DIFFERENT AND I DON'T REMEMBER WHY!

#### SET UP ####
#library(tidyverse) ; library(brms)
library(tidyverse, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')

theme_set(theme_bw())

behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')
num_chains <- 4
num_iter <- 2000

#### LOOKING DIRECTION -- all N(-1,1) ####
# rm(list = ls()) ; gc()
# load('looking_direction/looking_noprev_2bda_run.RData')

## select specific data
look <- behav %>%
  filter(activity == 'look') %>%
  select(-activity, -stim_start, -stim_stop) %>%
  rename(look_index = action_index) %>%
  mutate(action = ifelse(action_name == 'out_of_sight', 9,
                         look_index),
         f_age_num = as.numeric(f_age_num),
         p_age_num = as.numeric(p_age_num)) %>%
  filter(!is.na(f_age_num)) %>%
  filter(!is.na(p_age_num)) %>%
  filter(look_index != 9)

priors <- c(
  # focal age
  prior(normal(-1,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # interaction
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
  prior(normal(-1,1),      class = b,    coef = bdaduring),
  # interaction
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdaduring),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdaduring))

## prior predictive check
lom_noprev_prior <- brm(
  formula = look_index ~ mo(f_age_num) + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = look, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

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

#### MOVEMENT DIRECTION -- all N(0,1) ####
# rm(list = ls()) ; gc()
# load('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')
move <- behav %>% 
  filter(activity == 'move')

move <- move %>%
  filter(action_name != 'not_moving') %>%
  filter(action_name != 'out_of_sight') %>%
  rename(move_index = action_index,
         moving_direction = action_name) %>%
  filter(!is.na(p_age_num)) %>%
  filter(!is.na(f_age_num)) %>%
  mutate(f_age_num = as.integer(f_age_num))

priors <- c(
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
  # interaction
  prior(normal(0,1),      class = b,    coef = stim_typeh:bdabefore),
  prior(normal(0,1),      class = b,    coef = stim_typeh:bdaduring),
  prior(normal(0,1),      class = b,    coef = stim_typel:bdabefore),
  prior(normal(0,1),      class = b,    coef = stim_typel:bdaduring))

## prior predictive check
mom_prior <- brm(
  formula = move_index ~ mo(f_age_num) + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = move, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

priors
check <- pp_check(mom_prior, ndraws = 50)
check +
  labs(x = 'movement direction (1 = away, 5 = towards)',
       # title = 'MOM2',
       y = 'density')+
  theme(legend.position = 'bottom',
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), device = 'png',
       filename = 'priorcheck_MOM_noprev.png', path = 'movement_direction/',
       width = 1200, height = 1200, units = 'px')

#### MOVEMENT BINOMIAL -- all N(-1,1) ####
# rm(list = ls()) ; gc()
# load('movement_direction/binomial_noprev/movement_noprev_run.RData')

move <- behav %>%
  filter(activity == 'move')
move_no_na <- move %>%
  # remove out of sight observations
  filter(action_index != 9) %>%
  # convert to binary move or no move
  mutate(move_index = ifelse(action_index == 0, 0, 1),
         moving_direction = ifelse(action_name == 'not_moving',
                                   'not_moving', 'moving')) %>%
  # clean up
  mutate(f_age_num = as.integer(f_age_num)) %>%
  mutate(focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num) %>%
  select(focal, moving_direction, move_index,
         f_age_cat, f_age_num,
         stim_type, bda,
         focal_id, stim_id, playback_id) %>%
  filter(!is.na(f_age_num))

# set priors
priors <- c(
  # focal age
  prior(normal(-1,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(-1,1),      class = b,    coef = stim_typel),
  prior(normal(-1,1),      class = b,    coef = stim_typeh),
  # time
  prior(normal(-1,1),      class = b,    coef = bdabefore),
  prior(normal(-1,1),      class = b,    coef = bdaduring),
  # interaction
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdaduring),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdaduring)
)

prop_prior <- brm(
  formula = move_index ~ 0 + mo(f_age_num) + stim_type * bda +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

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

#### NEAREST NEIGHBOUR -- all N(-1,1) ####
# rm(list = ls()) ; gc()
# load('nearest_neighbour/neighbour_noprev_run.RData')

## select specific data
nn <- behav %>%
  filter(activity == 'nn') %>%
  select(-activity, -stim_start, -stim_stop) %>%
  mutate(prev = NA,
         action = ifelse(action_name == 'out_of_sight', 9,
                         action_index - 1),
         f_age_num = as.factor(as.numeric(f_age_num)),
         p_age_num = as.factor(as.numeric(p_age_num))) %>%
  filter(!is.na(p_age_num)) %>%
  relocate(action, .after = action_index)

# create variable for nearest neighbour at time t-1
focals <- unique(nn$focal)
for(f in 1:length(focals)){
  focal <- nn %>% filter(focal == focals[f])
  nn <- nn %>% anti_join(focal, by = 'focal')
  partners <- unique(focal$partner)
  for(p in 1:length(partners)){
    focal_partner <- focal %>% filter(partner == partners[p])
    focal <- focal %>% anti_join(focal_partner, by = 'partner')
    for(i in 2:nrow(focal_partner)){
      focal_partner$prev[i] <- focal_partner$action[i-1]
    }
    focal <- rbind(focal, focal_partner)
  }
  nn <- rbind(nn, focal)
}
rm(focal, focals, focal_partner, f, p, i, partners) ; gc()

## remove observations with missing data
nn <- nn %>%
  filter(action != 9) %>%
  filter(prev != 9) %>%
  filter(!is.na(prev)) %>%
  mutate(age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num), 'partner younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num), 'age matched',
                                        'partner older')))

priors <- c(
  # age combination
  prior(normal(0,1),          class = b,  coef = age_relOY),
  prior(normal(0,1),          class = b,  coef = age_relYO),
  prior(normal(0,1),          class = b,  coef = age_relYY),
  # stim type
  prior(normal(0,1),          class = b,  coef = stim_typeh),
  prior(normal(0,1),          class = b,  coef = stim_typel),
  # before/during/after
  prior(normal(0,1),          class = b,  coef = bdabefore),
  prior(normal(0,1),          class = b,  coef = bdaduring),
  # interaction
  prior(normal(0,1),          class = b,  coef = stim_typeh:bdabefore),
  prior(normal(0,1),          class = b,  coef = stim_typeh:bdaduring),
  prior(normal(0,1),          class = b,  coef = stim_typel:bdabefore),
  prior(normal(0,1),          class = b,  coef = stim_typel:bdaduring),
  # random effects / intercepts
  prior(student_t(3, 0, 0.5), class = sd, group = focal),
  prior(student_t(3, 0, 0.5), class = sd, group = pb_num),
  prior(student_t(3, 0, 0.5), class = sd, group = stim_num),
  prior(student_t(3, 0, 1),   class = Intercept))

nbm_prior <- brm(
  formula = action ~ 0 + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

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
       filename = 'priorcheck_NBM_noprev.png', path = 'nearest_neighbour/',
       width = 1200, height = 1200, units = 'px')
