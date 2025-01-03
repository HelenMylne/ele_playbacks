#### information                                                     ####
# same models as before, but removing effect of previous second

#### set up                                                          ####
library(tidyverse)
library(brms)
library(LaplacesDemon)
library(patchwork)
library(ggridges)

theme_set(theme_bw())

set.seed(12345)

############ movement proportion                                     ############
pdf('../outputs/movement_binomial_model/movement_noprev_dataprep_month.pdf')
set.seed(12345)
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')
move <- behav %>%
  filter(activity == 'move')

num_chains <- 4
num_iter <- 5000

#### filter data                                                     ####
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
str(move_no_na)

#### plot raw                                                        ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
move_no_na %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_bar(aes(x = moving_direction,
               fill = f_age_cat),
           position = 'dodge')+
  facet_grid(bda ~ stim_type,
             labeller = labeller(stim_type = stim_labels),
             scales = 'free_y')+
  labs(fill = 'age category (years)',
       y = 'seconds moving',
       x = '')+
  scale_fill_viridis_d()
print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('movement_direction/movement_noprev_run_month.RData')

#### set priors                                                      ####
# set priors
get_prior(formula = move_index ~ 0 + mo(f_age_num) + stim_type * bda +
            (1|focal_id) + (1|stim_id) + (1|playback_id),
          data = move_no_na,
          family = bernoulli("logit"))
#                prior class                 coef       group resp dpar nlpar lb ub       source
#               (flat)     b                                                             default
#               (flat)     b            bdabefore                                   (vectorized)
#               (flat)     b            bdaduring                                   (vectorized)
#               (flat)     b          mof_age_num                                   (vectorized)
#               (flat)     b         stim_typectd                                   (vectorized)
#               (flat)     b           stim_typeh                                   (vectorized)
#               (flat)     b stim_typeh:bdabefore                                   (vectorized)
#               (flat)     b stim_typeh:bdaduring                                   (vectorized)
#               (flat)     b           stim_typel                                   (vectorized)
#               (flat)     b stim_typel:bdabefore                                   (vectorized)
#               (flat)     b stim_typel:bdaduring                                   (vectorized)
# student_t(3, 0, 2.5)    sd                                                   0         default
# student_t(3, 0, 2.5)    sd                         focal_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept    focal_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd                      playback_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept playback_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd                          stim_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept     stim_id                  0    (vectorized)
#         dirichlet(1)  simo         mof_age_num1                                        default

# set priors
priors <- c(
  # focal age
  prior(normal(-1,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(-1,1),      class = b,    coef = stim_typectd),
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

#### prior predictive check                                          ####
prop_prior <- brm(
  formula = move_index ~ 0 + mo(f_age_num) + stim_type * bda +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2.5, seed = 12345,
  sample_prior = 'only')
pp_check(prop_prior)

print(paste0('priors set and checked at ', Sys.time()))

#### fit model                                                       ####
prop_fit <- brm(
  formula = move_index ~ 0 + mo(f_age_num) + stim_type * bda +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2.5, seed = 12345,
  control = list(adapt_delta = 0.999,
                 max_treedepth = 12))

# save workspace
save.image('movement_direction/movement_noprev_run_month.RData')

# inspect model
summary(prop_fit)

print(paste0('model fitted at ', Sys.time()))
dev.off()
