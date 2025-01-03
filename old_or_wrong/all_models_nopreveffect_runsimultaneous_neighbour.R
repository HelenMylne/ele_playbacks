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

######## import data about playbacks                                 ####
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')

## remove individuals where ages are unknown
behav <- behav %>%
  filter(!is.na(f_age_num))

######## nearest neighbour                                           ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelchecks_month.pdf')

#### create data                                                     ####
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
  filter(!is.na(prev))

# measure percentages of time spent in each state for report
nn <- nn %>%
  mutate(age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num), 'partner younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num), 'age matched',
                                        'partner older')))
round(prop.table(table(nn$age_difference[nn$bda == 'before']))*100,2)
# age matched   partner older partner younger
# 31.95           34.89           33.16
round(prop.table(table(nn$age_difference[nn$bda == 'during']))*100,2)
# age matched   partner older partner younger
# 31.81           34.19           34.00
round(prop.table(table(nn$age_difference[nn$bda == 'after']))*100,2)
# age matched   partner older partner younger
# 32.94           32.93           34.13

#### set prior                                                       ####
get_prior(formula = action ~ 0 + age_combo + stim_type * bda +
            (1|focal) + (1|stim_num) + (1|pb_num),
          data = nn, family = bernoulli("logit"))
#                prior class                 coef    group resp dpar nlpar lb ub       source
#               (flat)     b                                                          default
#               (flat)     b         age_combo1_1                                (vectorized)
#               (flat)     b         age_combo1_2                                (vectorized)
#               (flat)     b         age_combo1_3                                (vectorized)
#               (flat)     b         age_combo1_4                                (vectorized)
#               (flat)     b         age_combo2_1                                (vectorized)
#               (flat)     b         age_combo2_2                                (vectorized)
#               (flat)     b         age_combo2_3                                (vectorized)
#               (flat)     b         age_combo2_4                                (vectorized)
#               (flat)     b         age_combo3_1                                (vectorized)
#               (flat)     b         age_combo3_2                                (vectorized)
#               (flat)     b         age_combo3_3                                (vectorized)
#               (flat)     b         age_combo3_4                                (vectorized)
#               (flat)     b         age_combo4_1                                (vectorized)
#               (flat)     b         age_combo4_2                                (vectorized)
#               (flat)     b         age_combo4_3                                (vectorized)
#               (flat)     b         age_combo4_4                                (vectorized)
#               (flat)     b            bdabefore                                (vectorized)
#               (flat)     b            bdaduring                                (vectorized)
#               (flat)     b           stim_typeh                                (vectorized)
#               (flat)     b stim_typeh:bdabefore                                (vectorized)
#               (flat)     b stim_typeh:bdaduring                                (vectorized)
#               (flat)     b           stim_typel                                (vectorized)
#               (flat)     b stim_typel:bdabefore                                (vectorized)
#               (flat)     b stim_typel:bdaduring                                (vectorized)
# student_t(3, 0, 2.5)    sd                                                0         default
# student_t(3, 0, 2.5)    sd                         focal                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept    focal                  0    (vectorized)
# student_t(3, 0, 2.5)    sd                        pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept   pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)    sd                      stim_num                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept stim_num                  0    (vectorized)

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
  prior(normal(-1,1),      class = b,    coef = bdaduring),
  # interaction
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdaduring),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdaduring))

## prior predictive check
num_chains <- 4
num_iter <- 3000
nbm_prior <- brm(
  formula = action ~ 0 + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/3, seed = 12345,
  sample_prior = 'only')
pp_check(nbm_prior)

#### fit model                                                       ####
nbm_fit <- brm(
  formula = action ~ 0 + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/3, seed = 12345,
  control = list(adapt_delta = 0.999,
                 max_treedepth = 12))
# Warning messages:
# 1: There were 226 divergent transitions after warmup. See https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup # to find out why this is a problem and how to eliminate them.
# 2: There were 3770 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 3: Examine the pairs() plot to diagnose sampling problems
# 4: The largest R-hat is 1.07, indicating chains have not mixed. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#r-hat
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#bulk-ess
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#tail-ess
save.image('nearest_neighbour/neighbour_noprev_run_month.RData')
