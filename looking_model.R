#### information ####
# script for looking direction analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
# library(cmdstanr) ; set_cmdstan_path('../packages/.cmdstan/cmdstan-2.31.0/') ; library(brms) ; library(tidyverse) ; library(LaplacesDemon)
library(StanHeaders, lib.loc = '../packages/')
library(rstan, lib.loc = '../packages/')
library(brms, lib.loc = '../packages/')
#library(cmdstanr, lib.loc = '../packages/') # library(cmdstanr)
#set_cmdstan_path('../packages/.cmdstan/cmdstan-2.31.0/')
library(tidyverse, lib.loc = '../packages/')
library(LaplacesDemon, lib.loc = '../packages/')
library(patchwork, lib.loc = '../packages/')

theme_set(theme_classic())
set.seed(12345)

pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1_modelprep.pdf')

#### data prep ####
# https://dagitty.net/dags.html?id=dw8twK
# read in data
ages <- readRDS('data_processed/behaviour_by_second_indexvariables_bda.RDS') %>%
  # ages <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS') %>%
  select(focal, f_age_cat, f_age_num) %>%
  distinct() %>%
  filter(!is.na(f_age_cat)) %>%
  mutate(partner = focal,
         p_age_cat = f_age_cat,
         p_age_num = f_age_num)

stim_starts <- readRDS('data_processed/stimuli.RDS') %>%
  # stim_starts <- readRDS('../data_processed/stimuli.RDS') %>%
  filter(status == 'START' & behavior == 'STIMULUS') %>%
  select(pb_num,time,stim_num,stim_type,group_size,comment)
table(stim_starts$pb_num)
# 1  3  4  6  7 10 11 13 14 15 16 17 18 19 21 22 23 24 25 28 29 30 31 32 33 34 35 36 37 38 41 42 43 44 45 46 47 48 50 51 52 53 55 56 58 59 60 61
# 1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  2  1  1  2  1  1  3  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  5  1  1  1  1  1  1
multiple_starts <- c(10, 24, 29, 32, 46, 53)
check <- stim_starts %>%
  filter(pb_num %in% multiple_starts) # for stim 10+29+46+53 take first time, for 24+32 use second.
for(i in multiple_starts){
  x <- check %>% filter(pb_num == i)
  check <- anti_join(check, x)
  if(i %in% c(10,29,46,53)){
    x <- x[1,]
  }
  if(i %in% c(24,32)){
    x <- x[2,]
  }
  check <- rbind(check, x)
}
stim_starts <- stim_starts %>%
  filter(! pb_num %in% multiple_starts) %>%
  rbind(check) %>%
  mutate(time = as.numeric(time)) %>%
  mutate(stim_start = round(time, 0)) %>%
  select(pb_num,stim_start,stim_num,stim_type,group_size)

## looking direction data
cols_of_interest <- c('b1_look','b2_look','b3_look','b4_look',
                      'b5_look','b6_look','b7_look','b8_look')
cols_of_interest_name <- c('b1_look_name','b2_look_name','b3_look_name','b4_look_name',
                           'b5_look_name','b6_look_name','b7_look_name','b8_look_name')
cols_of_interest_index <- c('b1_look_index','b2_look_index','b3_look_index','b4_look_index',
                            'b5_look_index','b6_look_index','b7_look_index','b8_look_index')
look <- readRDS('data_processed/behaviour_by_second_indexvariables.RDS') %>%
  # look <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
  # select relevant variables
  select(subject,pb_num,second,out_frame_name,
         all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
  # convert to tidy format
  rename(b1_look = b1_look_name, b2_look = b2_look_name,
         b3_look = b3_look_name, b4_look = b4_look_name,
         b5_look = b5_look_name, b6_look = b6_look_name,
         b7_look = b7_look_name, b8_look = b8_look_name) %>%
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_name', values_to = 'looking_direction') %>%
  rename(b1_look = b1_look_index, b2_look = b2_look_index,
         b3_look = b3_look_index, b4_look = b4_look_index,
         b5_look = b5_look_index, b6_look = b6_look_index,
         b7_look = b7_look_index, b8_look = b8_look_index) %>%
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_index', values_to = 'look_index') %>%
  filter(elephant_activity_name == elephant_activity_index) %>%
  select(-elephant_activity_index) %>%
  # clean up
  rename(elephant_activity = elephant_activity_name,
         focal = subject) %>%
  # remove self-looking directions
  filter(looking_direction != 'impossible_partner') %>%
  # remove non-existent elephants (e.g. elephants 5-8 in a 4-elephant group)
  mutate(looking_direction = ifelse(out_frame_name == 'out_of_sight',
                                   'out_of_sight', looking_direction),
         look_index = ifelse(out_frame_name == 'out_of_sight', 9, look_index)) %>%
  # join with explanatory variables
  separate(elephant_activity, into = c('partner','activity'), sep = '_', remove = T) %>%
  mutate(partner = paste0(partner, '_e', pb_num),
         pb_num = as.numeric(pb_num)) %>%
  left_join(ages[,c('focal','f_age_cat','f_age_num')], by = 'focal') %>%
  left_join(ages[,c('partner','p_age_cat','p_age_num')], by = 'partner') %>%
  left_join(stim_starts, by = 'pb_num') %>%
  # remove elephants with unknown ages
  filter(!is.na(f_age_num)) %>%  # b2_e13 + b2_e34 + b6_e7 = unknown age
  filter(!is.na(p_age_num)) %>%  # b2_e13 + b2_e34 + b6_e7 = unknown age
  # create additional model variables
  mutate(time_since_stim = second - stim_start,
         after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60),
         age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num),
                                 'partner_younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num),
                                        'matched',
                                        'partner_older'))) %>%
  # clean up
  select(pb_num,focal,partner,
         activity,looking_direction,look_index,
         stim_num,stim_type,
         time_since_stim, after_stim,
         f_age_cat,p_age_cat,f_age_num,p_age_num,
         age_difference) %>%
  mutate(f_age_num = as.factor(f_age_num),
         p_age_num = as.factor(p_age_num),
         age_combo = paste0(f_age_num,'_',p_age_num),
         look_tminus1 = NA,
         look_tminus1_num = NA)
rm(list = ls() [ ! ls() %in% 'look']) ; gc()

length(which(is.na(look$looking_direction) == TRUE)) # 0

# create variable for looking direction at time t-1
focals <- unique(look$focal)
for(f in 1:length(focals)){
  focal <- look %>% filter(focal == focals[f])
  look <- look %>% anti_join(focal, by = 'focal')
  partners <- unique(focal$partner)
  for(p in 1:length(partners)){
    focal_partner <- focal %>% filter(partner == partners[p])
    focal <- focal %>% anti_join(focal_partner, by = 'partner')
    for(i in 2:nrow(focal_partner)){
      focal_partner$look_tminus1[i] <- focal_partner$looking_direction[i-1]
      focal_partner$look_tminus1_num[i] <- focal_partner$look_index[i-1]
    }
    focal <- rbind(focal, focal_partner)
  }
  look <- rbind(look, focal)
}
rm(focal, focals, focal_partner, f, p, i, partners) ; gc()

## create model data
look_no_na <- look %>%
  # remove out of sight observations
  filter(look_index != 9) %>%
  filter(look_tminus1_num != 9) %>%
  # remove any occasions with missing data
  filter(is.na(age_difference) == FALSE) %>%
  filter(is.na(look_tminus1) == FALSE) %>%
  # clean up factor variables
  mutate(age_difference = factor(age_difference,
                                 levels = c('partner_younger',
                                            'matched',
                                            'partner_older')),
         looking_direction = factor(looking_direction,
                                    levels = c('look directly away',
                                               'side-on',
                                               'look at directly')),
         look_tminus1 = factor(look_tminus1,
                               levels = c('look directly away',
                                          'side-on',
                                          'look at directly')),
         age_diff_num = as.integer(age_difference),
         f_age_num = as.integer(f_age_num),
         p_age_num = as.integer(p_age_num),
         focal_id = as.integer(as.factor(focal)),
         stim_id = as.integer(as.factor(stim_num))) %>%
  # clean up data frame
  rename(playback_id = pb_num) %>%
  select(focal, partner, looking_direction, look_index,
         f_age_cat, p_age_cat, f_age_num, p_age_num,
         age_difference, age_diff_num, age_combo,
         time_since_stim, after_stim, stim_type,
         look_tminus1, look_tminus1_num,
         focal_id, stim_id, playback_id)
str(look_no_na)
# $ focal            : chr [1:170229] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# $ partner          : chr [1:170229] "b2_e1" "b2_e1" "b2_e1" "b2_e1" ...
# $ looking_direction: Factor w/ 3 levels "look directly away",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ look_index       : num [1:170229] 2 2 2 2 2 2 2 2 2 2 ...
# $ f_age_cat        : chr [1:170229] "26-35" "26-35" "26-35" "26-35" ...
# $ p_age_cat        : chr [1:170229] "26-35" "26-35" "26-35" "26-35" ...
# $ f_age_num        : int [1:170229] 4 4 4 4 4 4 4 4 4 4 ...
# $ p_age_num        : int [1:170229] 4 4 4 4 4 4 4 4 4 4 ...
# $ age_difference   : Factor w/ 3 levels "partner_younger",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ age_diff_num     : int [1:170229] 2 2 2 2 2 2 2 2 2 2 ...
# $ age_combo        : chr [1:170229] "4_4" "4_4" "4_4" "4_4" ...
# $ time_since_stim  : num [1:170229] -81 -80 -79 -78 -77 -76 -75 -74 -73 -72 ...
# $ after_stim       : num [1:170229] 0 0 0 0 0 0 0 0 0 0 ...
# $ stim_type        : chr [1:170229] "ctd" "ctd" "ctd" "ctd" ...
# $ look_tminus1     : Factor w/ 3 levels "look directly away",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ look_tminus1_num : num [1:170229] 2 2 2 2 2 2 2 2 2 2 ...
# $ focal_id         : int [1:170229] 1 1 1 1 1 1 1 1 1 1 ...
# $ stim_id          : int [1:170229] 5 5 5 5 5 5 5 5 5 5 ...
# $ playback_id      : num [1:170229] 1 1 1 1 1 1 1 1 1 1 ...

## check numbers for current vs previous second
table(look_no_na$looking_direction, look_no_na$look_tminus1)
#                    look directly away     side-on    look at directly
# look directly away              74162         903                   5
# side-on                           858       65155                 622
# look at directly                   22         608               27894

#### set priors ####
get_prior(formula = looking_direction ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
            s(after_stim) + mo(look_tminus1_num) +                                          # controls, treat time as a spline
            (1|focal_id) + (1|stim_id) + (1|playback_id),                                   # random effects
          data = look_no_na,
          family = cumulative("logit"))
#                prior     class                coef       group resp dpar nlpar lb ub       source
#               (flat)         b                                                            default
#               (flat)         b        age_combo1_2                                   (vectorized)
#               (flat)         b        age_combo1_3                                   (vectorized)
#               (flat)         b        age_combo1_4                                   (vectorized)
#               (flat)         b        age_combo2_1                                   (vectorized)
#               (flat)         b        age_combo2_2                                   (vectorized)
#               (flat)         b        age_combo2_3                                   (vectorized)
#               (flat)         b        age_combo2_4                                   (vectorized)
#               (flat)         b        age_combo3_1                                   (vectorized)
#               (flat)         b        age_combo3_2                                   (vectorized)
#               (flat)         b        age_combo3_3                                   (vectorized)
#               (flat)         b        age_combo3_4                                   (vectorized)
#               (flat)         b        age_combo4_1                                   (vectorized)
#               (flat)         b        age_combo4_2                                   (vectorized)
#               (flat)         b        age_combo4_3                                   (vectorized)
#               (flat)         b        age_combo4_4                                   (vectorized)
#               (flat)         b         mof_age_num                                   (vectorized)
#               (flat)         b  molook_tminus1_num                                   (vectorized)
#               (flat)         b       safter_stim_1                                   (vectorized)
#               (flat)         b          stim_typeh                                   (vectorized)
#               (flat)         b          stim_typel                                   (vectorized)
# student_t(3, 0, 2.5) Intercept                                                            default
# student_t(3, 0, 2.5) Intercept                   1                                   (vectorized)
# student_t(3, 0, 2.5) Intercept                   2                                   (vectorized)
# student_t(3, 0, 2.5)        sd                                                  0         default
# student_t(3, 0, 2.5)        sd                        focal_id                  0    (vectorized)
# student_t(3, 0, 2.5)        sd           Intercept    focal_id                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                     playback_id                  0    (vectorized)
# student_t(3, 0, 2.5)        sd           Intercept playback_id                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                         stim_id                  0    (vectorized)
# student_t(3, 0, 2.5)        sd           Intercept     stim_id                  0    (vectorized)
# student_t(3, 0, 2.5)       sds                                                  0         default
# student_t(3, 0, 2.5)       sds       s(after_stim)                              0    (vectorized)
# dirichlet(1)      simo        mof_age_num1                                                default
# dirichlet(1)      simo molook_tminus1_num1                                                default

priors <- c(
  # focal age
  prior(normal(0,1),      class = b,    coef = mof_age_num), # nn = normal(0,0.25)
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # partner age
  #prior(normal(0,0.25),     class = b,    coef = mopartner_age),
  #prior(dirichlet(2,2,2),   class = simo, coef = mopartner_age1),
  # age interaction
  #prior(normal(0,0.25),     class = b,    coef = mofocal_age:mopartner_age),
  #prior(dirichlet(2,2,2),   class = simo, coef = mofocal_age:mopartner_age1),
  #prior(dirichlet(2,2,2),   class = simo, coef = mofocal_age:mopartner_age2),
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
  #prior(student_t(3,0,2.5), class = sds, coef = s(after_stim)), # not sure why this is in get_prior() but not model but it's not needed
  # action in previous second
  prior(normal(1,1), # normal(0,0.333),
        class = b,    coef = molook_tminus1_num),
  prior(dirichlet(2,2),    class = simo, coef = molook_tminus1_num1))

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
lom1_prior <- brm(
  formula = look_index ~ 1 + mo(f_age_num) + age_combo + stim_type + # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                           # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                    # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

pp_check(lom1_prior) # prior expects 1 and 3 most likely, 2 least likely. data show 1 least likely, 2 middle, 3 most.

print(paste0('priors set at ',Sys.time()))

## reset plotting
dev.off()
pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1_modelchecks.pdf')

#### fit model ####
lom1_fit <- brm(
  formula = look_index ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                             # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                      # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)
# Warning messages:
# 1: There were 4 divergent transitions after warmup. See https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup to find out why this is a problem and how to eliminate them.
# 2: Examine the pairs() plot to diagnose sampling problems

# save workspace
save.image('ele_playbacks/looking_direction/looking_ordinal_model1_run.RData') # save.image('looking_direction/looking_ordinal_model1_run.RData')

# inspect model
summary(lom1_fit)
# Family: cumulative; Links: mu = logit; disc = identity
# Formula: look_index ~ 1 + mo(f_age_num) + age_combo + stim_type + s(after_stim) + mo(look_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id)
# Data: look_no_na (Number of observations: 170229)
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; total post-warmup draws = 4000
#
# Smooth Terms:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(safter_stim_1)     1.41      0.82     0.26     3.40 1.00     1175     1687
#
# Group-Level Effects:
#   ~focal_id (Number of levels: 176)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.46      0.04     0.38     0.54 1.00     1399     2109
#
# ~playback_id (Number of levels: 48)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.08      0.06     0.00     0.21 1.00      842     1621
#
# ~stim_id (Number of levels: 30)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.10      0.06     0.01     0.24 1.00      838     2118
#
# Population-Level Effects:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]           4.23      0.23     3.77     4.69 1.00     2077     2525
# Intercept[2]          13.29      0.24    12.81    13.75 1.00     2193     2506
# age_combo1_2          -0.14      0.20    -0.52     0.24 1.00     3360     3220
# age_combo1_3           0.02      0.24    -0.46     0.48 1.00     3188     3299
# age_combo1_4          -0.09      0.28    -0.63     0.45 1.00     3887     3328
# age_combo2_1           0.27      0.24    -0.20     0.73 1.00     1165     2096
# age_combo2_2          -0.04      0.22    -0.46     0.39 1.00     1104     2153
# age_combo2_3           0.08      0.21    -0.33     0.49 1.00     1058     2070
# age_combo2_4          -0.06      0.22    -0.51     0.38 1.00     1076     2203
# age_combo3_1          -0.10      0.31    -0.68     0.52 1.00      966     2258
# age_combo3_2           0.09      0.28    -0.43     0.66 1.00      851     1581
# age_combo3_3           0.13      0.28    -0.40     0.70 1.00      866     1697
# age_combo3_4          -0.14      0.29    -0.67     0.45 1.00      854     1735
# age_combo4_1           0.30      0.45    -0.55     1.19 1.00     1018     2068
# age_combo4_2           0.22      0.41    -0.58     1.03 1.00      857     1623
# age_combo4_3           0.06      0.41    -0.74     0.86 1.00      836     1773
# age_combo4_4          -0.14      0.41    -0.94     0.68 1.00      826     1642
# stim_typeh            -0.04      0.11    -0.25     0.18 1.00     2693     2882
# stim_typel             0.06      0.13    -0.19     0.32 1.00     2872     3143
# safter_stim_1          0.69      0.90    -1.11     2.46 1.00     4971     3035
# mof_age_num           -0.07      0.16    -0.40     0.26 1.00      993     1792
# molook_tminus1_num     8.43      0.04     8.36     8.50 1.00     7673     3049
#
# Simplex Parameters:
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS  Tail_ESS
# mof_age_num1[1]            0.33      0.18     0.05     0.70 1.00     5893      3066
# mof_age_num1[2]            0.33      0.18     0.05     0.72 1.00     6907      2550
# mof_age_num1[3]            0.34      0.18     0.06     0.73 1.00     6582      2887
# molook_tminus1_num1[1]     0.51      0.00     0.50     0.51 1.00     9431      3310
# molook_tminus1_num1[2]     0.49      0.00     0.49     0.50 1.00     9431      3310
#
# Family Specific Parameters:
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA
#
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message: There were 4 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

print(paste0('model run at ',Sys.time()))

#### check outputs ####
# load('ele_playbacks/looking_direction/looking_ordinal_model1_run.RData') # load('looking_direction/looking_ordinal_model1_run.RData')

## check Stan code
lom1_fit$model
# // generated with brms 2.20.4
# functions {
#   /* cumulative-logit log-PDF for a single response
#   * Args:
#     *   y: response category
#   *   mu: latent mean parameter
#   *   disc: discrimination parameter
#   *   thres: ordinal thresholds
#   * Returns:
#     *   a scalar to be added to the log posterior
#   */
#     real cumulative_logit_lpmf(int y, real mu, real disc, vector thres) {
#       int nthres = num_elements(thres);
#       if (y == 1) {
#         return log_inv_logit(disc * (thres[1] - mu));
#       } else if (y == nthres + 1) {
#         return log1m_inv_logit(disc * (thres[nthres] - mu));
#       } else {
#         return log_diff_exp(
#           log_inv_logit(disc * (thres[y] - mu)),
#           log_inv_logit(disc * (thres[y - 1] - mu))
#         );
#       }
#     }
#   /* cumulative-logit log-PDF for a single response and merged thresholds
#   * Args:
#     *   y: response category
#   *   mu: latent mean parameter
#   *   disc: discrimination parameter
#   *   thres: vector of merged ordinal thresholds
#   *   j: start and end index for the applid threshold within 'thres'
#   * Returns:
#     *   a scalar to be added to the log posterior
#   */
#     real cumulative_logit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
#       return cumulative_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
#     }
#   /* ordered-logistic log-PDF for a single response and merged thresholds
#   * Args:
#     *   y: response category
#   *   mu: latent mean parameter
#   *   thres: vector of merged ordinal thresholds
#   *   j: start and end index for the applid threshold within 'thres'
#   * Returns:
#     *   a scalar to be added to the log posterior
#   */
#     real ordered_logistic_merged_lpmf(int y, real mu, vector thres, int[] j) {
#       return ordered_logistic_lpmf(y | mu, thres[j[1]:j[2]]);
#     }
#   /* compute monotonic effects
#   * Args:
#     *   scale: a simplex parameter
#   *   i: index to sum over the simplex
#   * Returns:
#     *   a scalar between 0 and rows(scale)
#   */
#     real mo(vector scale, int i) {
#       if (i == 0) {
#         return 0;
#       } else {
#         return rows(scale) * sum(scale[1:i]);
#       }
#     }
#   /* integer sequence of values
#   * Args:
#     *   start: starting integer
#   *   end: ending integer
#   * Returns:
#     *   an integer sequence from start to end
#   */
#     int[] sequence(int start, int end) {
#       array[end - start + 1] int seq;
#       for (n in 1:num_elements(seq)) {
#         seq[n] = n + start - 1;
#       }
#       return seq;
#     }
#   // compute partial sums of the log-likelihood
#   real partial_log_lik_lpmf(int[] seq, int start, int end, data int[] Y, data int nthres, data matrix Xc, vector b, vector Intercept, int[] Xmo_1, vector simo_1, int[] Xmo_2, vector simo_2, vector bsp, data matrix Xs, vector bs, data matrix Zs_1_1, vector s_1_1, real disc, data int[] J_1, data vector Z_1_1, vector r_1_1, data int[] J_2, data vector Z_2_1, vector r_2_1, data int[] J_3, data vector Z_3_1, vector r_3_1) {
#     real ptarget = 0;
#     int N = end - start + 1;
#     // initialize linear predictor term
#     vector[N] mu = rep_vector(0.0, N);
#     mu += Xc[start:end] * b + Xs[start:end] * bs + Zs_1_1[start:end] * s_1_1;
#     for (n in 1:N) {
#       // add more terms to the linear predictor
#       int nn = n + start - 1;
#       mu[n] += (bsp[1]) * mo(simo_1, Xmo_1[nn]) + (bsp[2]) * mo(simo_2, Xmo_2[nn]) + r_1_1[J_1[nn]] * Z_1_1[nn] + r_2_1[J_2[nn]] * Z_2_1[nn] + r_3_1[J_3[nn]] * Z_3_1[nn];
#     }
#     for (n in 1:N) {
#       int nn = n + start - 1;
#       ptarget += ordered_logistic_lpmf(Y[nn] | mu[n], Intercept);
#     }
#     return ptarget;
#   }
# }
# data {
#   int<lower=1> N;  // total number of observations
#   array[N] int Y;  // response variable
#   int<lower=2> nthres;  // number of thresholds
#   int<lower=1> K;  // number of population-level effects
#   matrix[N, K] X;  // population-level design matrix
#   int<lower=1> Kc;  // number of population-level effects after centering
#   int<lower=1> Ksp;  // number of special effects terms
#   int<lower=1> Imo;  // number of monotonic variables
#   array[Imo] int<lower=1> Jmo;  // length of simplexes
#   array[N] int Xmo_1;  // monotonic variable
#   vector[Jmo[1]] con_simo_1;  // prior concentration of monotonic simplex
#   array[N] int Xmo_2;  // monotonic variable
#   vector[Jmo[2]] con_simo_2;  // prior concentration of monotonic simplex
#   // data for splines
#   int Ks;  // number of linear effects
#   matrix[N, Ks] Xs;  // design matrix for the linear effects
#   // data for spline 1
#   int nb_1;  // number of bases
#   array[nb_1] int knots_1;  // number of knots
#   // basis function matrices
#   matrix[N, knots_1[1]] Zs_1_1;
#   int grainsize;  // grainsize for threading
#   // data for group-level effects of ID 1
#   int<lower=1> N_1;  // number of grouping levels
#   int<lower=1> M_1;  // number of coefficients per level
#   array[N] int<lower=1> J_1;  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_1_1;
#   // data for group-level effects of ID 2
#   int<lower=1> N_2;  // number of grouping levels
#   int<lower=1> M_2;  // number of coefficients per level
#   array[N] int<lower=1> J_2;  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_2_1;
#   // data for group-level effects of ID 3
#   int<lower=1> N_3;  // number of grouping levels
#   int<lower=1> M_3;  // number of coefficients per level
#   array[N] int<lower=1> J_3;  // grouping indicator per observation
#   // group-level predictor values
#   vector[N] Z_3_1;
#   int prior_only;  // should the likelihood be ignored?
# }
# transformed data {
#   matrix[N, Kc] Xc;  // centered version of X
#   vector[Kc] means_X;  // column means of X before centering
#   int seq[N] = sequence(1, N);
#   for (i in 1:K) {
#     means_X[i] = mean(X[, i]);
#     Xc[, i] = X[, i] - means_X[i];
#   }
# }
# parameters {
#   vector[Kc] b;  // regression coefficients
#   ordered[nthres] Intercept;  // temporary thresholds for centered predictors
#   simplex[Jmo[1]] simo_1;  // monotonic simplex
#   simplex[Jmo[2]] simo_2;  // monotonic simplex
#   vector[Ksp] bsp;  // special effects coefficients
#   vector[Ks] bs;  // unpenalized spline coefficients
#   // parameters for spline 1
#   // standardized penalized spline coefficients
#   vector[knots_1[1]] zs_1_1;
#   vector<lower=0>[nb_1] sds_1;  // SDs of penalized spline coefficients
#   vector<lower=0>[M_1] sd_1;  // group-level standard deviations
#   array[M_1] vector[N_1] z_1;  // standardized group-level effects
#   vector<lower=0>[M_2] sd_2;  // group-level standard deviations
#   array[M_2] vector[N_2] z_2;  // standardized group-level effects
#   vector<lower=0>[M_3] sd_3;  // group-level standard deviations
#   array[M_3] vector[N_3] z_3;  // standardized group-level effects
# }
# transformed parameters {
#   // penalized spline coefficients
#   vector[knots_1[1]] s_1_1;
#   real disc = 1;  // discrimination parameters
#   vector[N_1] r_1_1;  // actual group-level effects
#   vector[N_2] r_2_1;  // actual group-level effects
#   vector[N_3] r_3_1;  // actual group-level effects
#   real lprior = 0;  // prior contributions to the log posterior
#   // compute penalized spline coefficients
#   s_1_1 = sds_1[1] * zs_1_1;
#   r_1_1 = (sd_1[1] * (z_1[1]));
#   r_2_1 = (sd_2[1] * (z_2[1]));
#   r_3_1 = (sd_3[1] * (z_3[1]));
#   lprior += normal_lpdf(b[1] | 0, 1);
#   lprior += normal_lpdf(b[2] | 0, 1);
#   lprior += normal_lpdf(b[3] | 0, 1);
#   lprior += normal_lpdf(b[4] | 0, 1);
#   lprior += normal_lpdf(b[5] | 0, 1);
#   lprior += normal_lpdf(b[6] | 0, 1);
#   lprior += normal_lpdf(b[7] | 0, 1);
#   lprior += normal_lpdf(b[8] | 0, 1);
#   lprior += normal_lpdf(b[9] | 0, 1);
#   lprior += normal_lpdf(b[10] | 0, 1);
#   lprior += normal_lpdf(b[11] | 0, 1);
#   lprior += normal_lpdf(b[12] | 0, 1);
#   lprior += normal_lpdf(b[13] | 0, 1);
#   lprior += normal_lpdf(b[14] | 0, 1);
#   lprior += normal_lpdf(b[15] | 0, 1);
#   lprior += normal_lpdf(b[16] | 0, 1);
#   lprior += normal_lpdf(b[17] | 0, 1);
#   lprior += student_t_lpdf(Intercept | 3, 0, 2.5);
#   lprior += dirichlet_lpdf(simo_1 | con_simo_1);
#   lprior += dirichlet_lpdf(simo_2 | con_simo_2);
#   lprior += normal_lpdf(bsp[1] | 0, 1);
#   lprior += normal_lpdf(bsp[2] | 1, 1);
#   lprior += normal_lpdf(bs[1] | 0, 1);
#   lprior += student_t_lpdf(sds_1 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
#   lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
#   lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
#   lprior += student_t_lpdf(sd_3 | 3, 0, 2.5)
#   - 1 * student_t_lccdf(0 | 3, 0, 2.5);
# }
# model {
#   // likelihood including constants
#   if (!prior_only) {
#     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, nthres, Xc, b, Intercept, Xmo_1, simo_1, Xmo_2, simo_2, bsp, Xs, bs, Zs_1_1, s_1_1, disc, J_1, Z_1_1, r_1_1, J_2, Z_2_1, r_2_1, J_3, Z_3_1, r_3_1);
#   }
#   // priors including constants
#   target += lprior;
#   target += std_normal_lpdf(zs_1_1);
#   target += std_normal_lpdf(z_1[1]);
#   target += std_normal_lpdf(z_2[1]);
#   target += std_normal_lpdf(z_3[1]);
# }
# generated quantities {
#   // compute actual thresholds
#   vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
# }
lom1_fit$formula
# look_index ~ 1 + mo(f_age_num) + age_combo + stim_type + s(after_stim) + mo(look_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id)

## extract posterior distribution
draws <- as_draws_df(lom1_fit) %>%
  select(-lprior, -`lp__`)
parameters <- colnames(draws)[1:(ncol(draws)-3)]
draws <- draws  %>%
  pivot_longer(cols = all_of(parameters),
               names_to = 'parameter',
               values_to = 'draw') %>%
  rename(chain = `.chain`,
         position = `.iteration`,
         draw_id = `.draw`) %>%
  mutate(invlogit_draw = invlogit(draw))
# nearest neighbour version -- run with looking direction first but if it throws an error then come back to the nearest neighbour code to fix it
# draws <- as_draws_df(nn_fit) %>%
#   select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>%
#   pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>%
#   mutate(iteration = rep(rep(1:(num_iter/2),
#                              each = length(unique(parameter))),
#                          num_chains),
#          chain = rep(1:num_chains,
#                      each = length(unique(parameter))*(num_iter/2)),
#          invlogit_draw = invlogit(draw))

print(paste0('posterior extracted at ',Sys.time()))

## look at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

#### calculate log cumulative odds ####
(prop <- table(look_no_na$looking_direction) / nrow(look_no_na))
# look directly away      side-on   look at directly
#          0.4409942    0.3914433          0.1675625
(cum_prop <- cumsum(prop))
# look directly away      side-on   look at directly
#          0.4409942    0.8324375          1.0000000
(log_cum_odds <- logit(cum_prop))
# look directly away      side-on   look at directly
#         -0.2371282    1.6030016                Inf

print(paste0('cumulative log odds calculated at ',Sys.time()))

#### plot marginal effects ####
## extract marginal effects
marg <- conditional_effects(lom1_fit,
                            categorical = TRUE,
                            method = 'posterior_epred')
names(marg)
# "age_combo:cats__","stim_type:cats__","f_age_num:cats__","look_tminus1_num:cats__","after_stim:cats__"
agecombo_effect <- marg[[1]]
stim_effect <- marg[[2]]
agefocal_effect <- marg[[3]]
prevsec_effect <- marg[[4]]
time_effect <- marg[[5]]

## plot marginal effects
(focal_age_plot <- ggplot(agefocal_effect)+
    # geom_ribbon(aes(x = f_age_num,
    #                 ymax = upper__, ymin = lower__,
    #                 fill = cats__),
    #             alpha = 0.4)+
    # geom_line(aes(x = f_age_num,
    #               y = estimate__,
    #               colour = cats__),
    #           linewidth = 1)+
    geom_errorbar(aes(x = f_age_num,
                      ymax = upper__, ymin = lower__,
                      colour = cats__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = f_age_num,
                   y = estimate__,
                   colour = cats__),
               size = 3)+ # cex = 3?
    xlab(label = 'focal age')+
    ylab('probability of looking direction')+
    scale_colour_viridis_d(name = 'looking direction:',
                           breaks = c('1','2','3'),
                           labels = c('look towards',
                                      'side on',
                                      'look away'))+
    scale_fill_viridis_d(name = 'looking direction:',
                         breaks = c('1','2','3'),
                         labels = c('look towards',
                                    'side on',
                                    'look away'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)))
ggsave(plot = focal_age_plot, filename = 'outputs/looking_ordinal_model_1/looking_ordinal1_marginaleffects_focalage_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

focal_age_labels <- c('focal age category 1',
                      'focal age category 2',
                      'focal age category 3',
                      'focal age category 4')
names(focal_age_labels) <- 1:4
(agecombo_plot <- agecombo_effect %>%
    separate(col = age_combo, sep = '_', remove = F,
             into = c('focal_age','partner_age')) %>%
    mutate(agecombo = paste0(focal_age,'-',partner_age)) %>%
    ggplot()+
    geom_errorbar(aes(#x = agecombo,
                      x = partner_age,
                      colour = as.factor(cats__), # looking direction?
                      ymax = upper__, ymin = lower__),
                  linewidth = 1,
                  width = 0.4)+
    geom_point(aes(#x = agecombo,
                   x = partner_age,
                   colour = as.factor(cats__),    # looking direction?
                   #shape = focal_age,
                   y = estimate__),
               size = 3)+
    # geom_ribbon(aes(#x = agecombo,
    #                 x = as.numeric(partner_age),
    #                 fill = as.factor(cats__),     # looking direction?
    #                 ymax = upper__, ymin = lower__),
    #             alpha = 0.4)+
    # geom_line(aes(#x = agecombo,
    #               x = as.numeric(partner_age),
    #               colour = as.factor(cats__),     # looking direction?
    #               y = estimate__),
    #           linewidth = 1)+
    facet_wrap(. ~ focal_age,
               labeller = labeller(focal_age = focal_age_labels))+
    ylab('probability of looking direction')+
    scale_colour_viridis_d(name = 'looking direction:',
                           breaks = c('1','2','3'),
                           labels = c('look towards',
                                      'side on',
                                      'look away'))+
    # scale_fill_viridis_d(name = 'looking direction:',
    #                      breaks = c('1','2','3'),
    #                      labels = c('look towards',
    #                                 'side on',
    #                                 'look away'))+
    scale_x_discrete(name = 'partner age category')+
    #scale_x_continuous(name = 'partner age category')+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = agecombo_plot, filename = 'outputs/looking_ordinal_model_1/looking_ordinal1_marginaleffects_agepartner_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(stim_plot <- ggplot(stim_effect)+
   geom_errorbar(aes(x = stim_type,
                     ymin = lower__, ymax = upper__,
                     colour = cats__),
                 linewidth = 1, width = 0.2)+
   geom_point(aes(x = stim_type,
                  y = estimate__,
                  colour = cats__),
              cex = 3)+ # size = 3?
   xlab(label = 'stimulus type') + ylab('probability of looking direction')+
   scale_colour_viridis_d(name = 'looking direction:',
                          breaks = c('1','2','3'),
                          labels = c('look towards', 'side on', 'look away'))+
   scale_x_discrete(breaks = c('ctd','l','h'),
                    labels = c('dove (control)', 'lion', 'human'),
                    limits = c('ctd','l','h'))+
   theme(legend.position = 'bottom',
         axis.title = element_text(size = 16),
         axis.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10)) )
ggsave(plot = stim_plot, filename = 'outputs/looking_ordinal_model_1/looking_ordinal1_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + agecombo_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = 'outputs/looking_ordinal_model_1/looking_ordinal1_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(lom1_fit, ndraws = 100)
print(paste0('posterior predictive check completed at ',Sys.time()))

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "safter_stim_1","sds_s(after_stim)",
                          "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
                          "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
                          "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
                          "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "bsp_mof_age_num",#"bsp_mopartner_age",
                          "bsp_molook_tminus1_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          #"simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
                          "simo_molook_tminus1_num1[1]","simo_molook_tminus1_num1[2]"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing doesn't look brilliant, esp. for bsp_mof_age_num, but only horrendous one is playback ID

## look at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,1))
hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
#hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')

## stim type
lion <- draws_cut %>% filter(parameter == 'b_stim_typel')
human <- draws_cut %>% filter(parameter == 'b_stim_typeh')
plot(density(lion$draw), main = 'lion vs dove') ; abline(v = 0, lty = 2)
plot(density(human$draw), main = 'human vs dove') ; abline(v = 0, lty = 2)

## focal age
age1 <- draws_cut %>% filter(parameter == 'bsp_mof_age_num')
age2 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[1]')
age3 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[2]')
age4 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[3]')
par(mfrow = c(2,2))
plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)

## looking direction in previous second
prevsec1 <- draws_cut %>% filter(parameter == 'bsp_molook_tminus1_num')
prevsec2 <- draws_cut %>% filter(parameter == 'simo_molook_tminus1_num1[1]')
prevsec3 <- draws_cut %>% filter(parameter == 'simo_molook_tminus1_num1[2]')
par(mfrow = c(3,1))
plot(density(prevsec1$draw), main = 't-1 younger') ; abline(v = 0, lty = 2)
plot(density(prevsec2$draw), main = 't-1 matched') ; abline(v = 0, lty = 2)
plot(density(prevsec3$draw), main = 't-1 older') ; abline(v = 0, lty = 2)

# ## time since stimulus -- come back to this!
# timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
# times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
# time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
# time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
# time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
# time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
# time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
# time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
# time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
# time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
# par(mfrow = c(5,2))
# plot(density(timeb$draw), main = 'time slope') ; abline(v = 0, lty = 2)
# plot(density(times$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
# plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
# plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
# plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
# plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
# plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
# plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
# plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
# plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)

print(paste0('traceplots completed at ',Sys.time()))

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
ggplot(look_no_na, aes(x = f_age_num, y = look_index,
                       colour = age_difference))+
  geom_jitter(alpha = 0.1)+
  facet_wrap(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_y_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','older'))+
  labs(colour = 'age difference')

## plot control data
look_no_na %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = time_since_stim, y = look_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner_younger','matched','partner_older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started (s)')+
  ggtitle('dove (raw data)')

## plot lion data
look_no_na %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = time_since_stim, y = look_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner_younger','matched','partner_older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started (s)')+
  ggtitle('lion (raw data)')

## plot human data
look_no_na %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = time_since_stim, y = look_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner_younger','matched','partner_older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started (s)')+
  ggtitle('human (raw data)')

print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('ele_playbacks/looking_direction/looking_ordinal_model1_run.RData') # save.image('looking_direction/looking_ordinal_model1_run.RData')
dev.off()
pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1predictions.pdf')

#### predict from model ####
# load('ele_playbacks/looking_direction/looking_ordinal_model1_run.RData') # load('looking_direction/looking_ordinal_model1_run.RData')
rm(list = ls()[! ls() %in% c('lom1_fit','look_no_na')]) ; gc()

pred <- posterior_epred(object = lom1_fit,
                        newdata = look_no_na)
save.image('ele_playbacks/looking_direction/looking_ordinal_model1_predictions.RData')

## convert to data frame
look_no_na$data_row <- 1:nrow(look_no_na)
extract_predictions <- function(prediction_array, layer, df){
  predictions <- as.data.frame(prediction_array[,,layer])
  colnames(predictions) <- 1:nrow(df)
  predictions <- predictions %>%
    pivot_longer(cols = everything(),
                 names_to = 'data_row', values_to = 'epred') %>%
    mutate(data_row = as.integer(data_row)) %>%
    left_join(df, by = 'data_row') %>%
    mutate(pred_type = ifelse(layer == 1, 'look directly away',
                                     ifelse(layer == 2, 'side-on',
                                                   ifelse(layer == 3, 'look at directly',
                                                          'CHECK -- PROBLEM IN DATA'))),
           pred_type_num = layer)
  return(predictions)
}
pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = look_no_na)
pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = look_no_na)
pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = look_no_na)

## combine data frames
pred <- rbind(pred1, pred2, pred3)
save.image('ele_playbacks/looking_direction/looking_ordinal_model1_predictions.RData')
rm(pred1, pred2, pred3) ; gc()

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions ####
# load('ele_playbacks/looking_direction/looking_ordinal_model1_predictions.RData')  #load('looking_direction/looking_ordinal_model1_predictions.RData')

## make labels for looking in previous second
prevsec_labels <- c('directly away at t-1',
                    'side-on at t-1',
                    'directly towards at t-1')
names(prevsec_labels) <- 1:3

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
(ctd_plot <- pred %>%
    filter(stim_type == 'ctd',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ after_stim,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'cape turtle dove (control)')+
    theme(legend.position = 'bottom'))
ggsave(plot = last_plot(),
       file = 'outputs/looking_ordinal_model_1/looking_ordinal_model1_predictions_ctd.png',
       device = 'png', height = 8, width = 8)

(lion_plot <- pred %>%
    filter(stim_type == 'l',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ after_stim,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'lion')+
    theme(legend.position = 'bottom'))
ggsave(plot = last_plot(),
       file = 'outputs/looking_ordinal_model_1/looking_ordinal_model1_predictions_lion.png',
       device = 'png', height = 8, width = 8)

(human_plot <- pred %>%
    filter(stim_type == 'h',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ after_stim,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'human')+
    theme(legend.position = 'bottom'))
ggsave(plot = last_plot(),
       file = 'outputs/looking_ordinal_model_1/looking_ordinal_model1_predictions_human.png',
       device = 'png', height = 8, width = 8)

# (ctd_plot + lion_plot + human_plot)+
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(), file = 'outputs/looking_ordinal_model_1/looking_ordinal_model1_predictions_violin.png',
#        device = 'png', height = 8, width = 24)

## reset plotting
dev.off()
save.image('ele_playbacks/looking_direction/looking_ordinal_model1_predictions.RData')

#### calculate posterior contrasts from predictions ####
# load('ele_playbacks/looking_direction/looking_ordinal_model1_predictions.RData') # load('looking_direction/looking_ordinal_model1_predictions.RData')
# rm(prevsec_labels, ctd_plot, human_plot, lion_plot) ; gc()
pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1_modelcontrasts.pdf')

## stim type ####
stim_new <- look_no_na %>%
  dplyr::select(f_age_num, age_combo, stim_type, look_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, look_tminus1_num, after_stim,
                                                         focal_id, stim_id, playback_id))))

## redo predictions with different stimulus types: all doves
ctd_look <- stim_new %>%
  mutate(stim_type = 'ctd')
ctd_mtx <- posterior_epred(object = lom1_fit, newdata = ctd_look)
colnames(ctd_mtx) <- ctd_look$unique_data_combo
ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all lions
lion_look <- stim_new %>%
  mutate(stim_type = 'l')
lion_mtx <- posterior_epred(object = lom1_fit, newdata = lion_look)
colnames(lion_mtx) <- lion_look$unique_data_combo
lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all humans
human_look <- stim_new %>%
  mutate(stim_type = 'h')
human_mtx <- posterior_epred(object = lom1_fit, newdata = human_look)
colnames(human_mtx) <- human_look$unique_data_combo
human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('ele_playbacks/looking_direction/looking_ordinal_model1_stimuluscontrasts.RData') # save.image('looking_direction/looking_ordinal_model1_stimuluscontrasts.RData')

## count types of each prediction
stim_pred <- ctd_look %>%
  dplyr::select(-stim_type) %>%
  mutate(ctd_prop1_mu = apply(ctd_mtx[,,1], 2, mean),
         ctd_prop2_mu = apply(ctd_mtx[,,2], 2, mean),
         ctd_prop3_mu = apply(ctd_mtx[,,3], 2, mean),
         ctd_prop1_sd = apply(ctd_mtx[,,1], 2, sd),
         ctd_prop2_sd = apply(ctd_mtx[,,2], 2, sd),
         ctd_prop3_sd = apply(ctd_mtx[,,3], 2, sd),
         lion_prop1_mu = apply(lion_mtx[,,1], 2, mean),
         lion_prop2_mu = apply(lion_mtx[,,2], 2, mean),
         lion_prop3_mu = apply(lion_mtx[,,3], 2, mean),
         lion_prop1_sd = apply(lion_mtx[,,1], 2, sd),
         lion_prop2_sd = apply(lion_mtx[,,2], 2, sd),
         lion_prop3_sd = apply(lion_mtx[,,3], 2, sd),
         human_prop1_mu = apply(human_mtx[,,1], 2, mean),
         human_prop2_mu = apply(human_mtx[,,2], 2, mean),
         human_prop3_mu = apply(human_mtx[,,3], 2, mean),
         human_prop1_sd = apply(human_mtx[,,1], 2, sd),
         human_prop2_sd = apply(human_mtx[,,2], 2, sd),
         human_prop3_sd = apply(human_mtx[,,3], 2, sd)) %>%
  pivot_longer(cols = c(ctd_prop1_mu,ctd_prop2_mu,ctd_prop3_mu,
                        lion_prop1_mu,lion_prop2_mu,lion_prop3_mu,
                        human_prop1_mu,human_prop2_mu,human_prop3_mu),
               names_to = 'stim_propdir_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(ctd_prop1_sd,ctd_prop2_sd,ctd_prop3_sd,
                        lion_prop1_sd,lion_prop2_sd,lion_prop3_sd,
                        human_prop1_sd,human_prop2_sd,human_prop3_sd),
               names_to = 'stim_propdir_sd', values_to = 'stdv_propn') %>%
  separate(col = stim_propdir_mu, into = c('stim_propdir_mu','mu'),
           sep = '_m', remove = T) %>%
  select(-mu) %>%
  separate(col = stim_propdir_sd, into = c('stim_propdir_sd','sd'),
           sep = '_s', remove = T) %>%
  select(-sd) %>%
  filter(stim_propdir_mu == stim_propdir_sd) %>%
  separate(col = stim_propdir_mu, into = c('stim_type', 'look_pred'),
           sep = '_prop', remove = T) %>%
  select(-stim_propdir_sd) %>%
  mutate(look_pred = as.numeric(look_pred)) %>%
  mutate(pred_type = ifelse(look_pred == 1, 'away',
                            ifelse(look_pred == 2, 'side', 'twds')))

## convert full predictive distribution to long format
stim_pred_all <- ctd_mtx[,,1] %>%
  as.data.frame()
colnames(stim_pred_all) <- rownames(ctd_look)
stim_pred_all <- pivot_longer(stim_pred_all, cols = everything(),
                              names_to = 'rownum', values_to = 'probability')
ctd_look$rownum <- rownames(ctd_look)
stim_pred_all <- stim_pred_all %>%
  left_join(ctd_look, by = 'rownum') %>%
  mutate(predict_num = 1,
         predict_cat = 'away')
for(i in 2:3){
  stim_pred_i <- ctd_mtx[,,i] %>%
    as.data.frame()
  colnames(stim_pred_i) <- rownames(ctd_look)
  stim_pred_i <- pivot_longer(stim_pred_i, cols = everything(),
                              names_to = 'rownum', values_to = 'probability')
  stim_pred_i <- stim_pred_i %>%
    left_join(ctd_look, by = 'rownum') %>%
    mutate(predict_num = i,
           predict_cat = ifelse(i == 2, 'side', 'twds'))
  stim_pred_all <- rbind(stim_pred_all, stim_pred_i)
}

## calculate contrasts
ctd_vs_lion_away <- lion_mtx[,,1] - ctd_mtx[,,1]
ctd_vs_lion_side <- lion_mtx[,,2] - ctd_mtx[,,2]
ctd_vs_lion_twds <- lion_mtx[,,3] - ctd_mtx[,,3]
ctd_vs_human_away <- human_mtx[,,1] - ctd_mtx[,,1]
ctd_vs_human_side <- human_mtx[,,2] - ctd_mtx[,,2]
ctd_vs_human_twds <- human_mtx[,,3] - ctd_mtx[,,3]
lion_vs_human_away <- human_mtx[,,1] - lion_mtx[,,1]
lion_vs_human_side <- human_mtx[,,2] - lion_mtx[,,2]
lion_vs_human_twds <- human_mtx[,,3] - lion_mtx[,,3]

## plot -- no difference between stimuli
plot(density(ctd_vs_lion_away), col = 'red', las = 1, xlim = c(-0.1, 0.1),
     main = 'contrasts between stim types:\nred = look away, purple = side on, blue = look at;\nsolid = ctd vs l, dashed = ctd vs h, dotted = l vs h')
mean(ctd_vs_lion_away) ; sd(ctd_vs_lion_away)
# -0.0005397117 ± 0.001618997
lines(density(ctd_vs_lion_side), col = 'purple')
mean(ctd_vs_lion_side) ; sd(ctd_vs_lion_side)
# 0.0001635726 ± 0.001926623
lines(density(ctd_vs_lion_twds), col = 'blue')
mean(ctd_vs_lion_twds) ; sd(ctd_vs_lion_twds)
# 0.0003761391 ± 0.001507346
lines(density(ctd_vs_human_away), col = 'red', lty = 2)
mean(ctd_vs_human_away) ; sd(ctd_vs_human_away)
# 0.0004139927 ± 0.001397755
lines(density(ctd_vs_human_side), col = 'purple', lty = 2)
mean(ctd_vs_human_side) ; sd(ctd_vs_human_side)
# -0.0001224977 ± 0.001671439
lines(density(ctd_vs_human_twds), col = 'blue', lty = 2)
mean(ctd_vs_human_twds) ; sd(ctd_vs_human_twds)
# -0.000291495 ± 0.001322645
lines(density(lion_vs_human_away), col = 'red', lty = 3)
mean(lion_vs_human_away) ; sd(lion_vs_human_away)
# 0.0009537043 ± 0.00188077
lines(density(lion_vs_human_side), col = 'purple', lty = 3)
mean(lion_vs_human_side) ; sd(lion_vs_human_side)
# -0.0002860702 ± 0.002384605
lines(density(lion_vs_human_twds), col = 'blue', lty = 3)
mean(lion_vs_human_twds) ; sd(lion_vs_human_twds)
# -0.0006676341 ± 0.001820435

## summarise contrasts
contrasts <- look_no_na %>%
  select(-stim_type) %>%
  mutate(ctd_vs_lion_away_mu = apply(ctd_vs_lion_away, 2, mean),
         ctd_vs_lion_away_sd = apply(ctd_vs_lion_away, 2, sd),
         ctd_vs_lion_side_mu = apply(ctd_vs_lion_side, 2, mean),
         ctd_vs_lion_side_sd = apply(ctd_vs_lion_side, 2, sd),
         ctd_vs_lion_twds_mu = apply(ctd_vs_lion_twds, 2, mean),
         ctd_vs_lion_twds_sd = apply(ctd_vs_lion_twds, 2, sd),
         ctd_vs_human_away_mu = apply(ctd_vs_human_away, 2, mean),
         ctd_vs_human_away_sd = apply(ctd_vs_human_away, 2, sd),
         ctd_vs_human_side_mu = apply(ctd_vs_human_side, 2, mean),
         ctd_vs_human_side_sd = apply(ctd_vs_human_side, 2, sd),
         ctd_vs_human_twds_mu = apply(ctd_vs_human_twds, 2, mean),
         ctd_vs_human_twds_sd = apply(ctd_vs_human_twds, 2, sd),
         lion_vs_human_away_mu = apply(lion_vs_human_away, 2, mean),
         lion_vs_human_away_sd = apply(lion_vs_human_away, 2, sd),
         lion_vs_human_side_mu = apply(lion_vs_human_side, 2, mean),
         lion_vs_human_side_sd = apply(lion_vs_human_side, 2, sd),
         lion_vs_human_twds_mu = apply(lion_vs_human_twds, 2, mean),
         lion_vs_human_twds_sd = apply(lion_vs_human_twds, 2, sd))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(ctd_vs_lion_away_mu,ctd_vs_lion_side_mu,ctd_vs_lion_twds_mu,
                        ctd_vs_human_away_mu,ctd_vs_human_side_mu,ctd_vs_human_twds_mu,
                        lion_vs_human_away_mu,lion_vs_human_side_mu,lion_vs_human_twds_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('stim_a','vs','stim_b','look_pred','mu'),
           sep = '_', remove = T) %>%
  mutate(contrast = paste0(stim_a,'_vs_',stim_b),
         look_pred = ifelse(look_pred == 'away', 1,
                            ifelse(look_pred == 'side', 2, 3))) %>%
  mutate(pred_type = ifelse(look_pred == 1, 'look away',
                            ifelse(look_pred == 2, 'side on', 'look at')),
         look_tminus1 = ifelse(look_tminus1_num == 1,
                               'look away at t-1',
                               ifelse(look_tminus1_num == 2,
                                      'side on at t-1',
                                      'look at at t-1'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('look away','side on','look at')),
         look_tminus1 = factor(look_tminus1,
                               levels = c('look away at t-1',
                                          'side on at t-1',
                                          'look at at t-1'))) %>%
  select(-mu, -vs, -ctd_vs_lion_away_sd, -ctd_vs_lion_side_sd, -ctd_vs_lion_twds_sd,
         -ctd_vs_human_away_sd, -ctd_vs_human_side_sd, -ctd_vs_human_twds_sd,
         -lion_vs_human_away_sd, -lion_vs_human_side_sd, -lion_vs_human_twds_sd)

## plot contrasts
stim_pred %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = as.factor(f_age_num)))+
  scale_colour_viridis_d()+
  facet_wrap(pred_type ~ stim_type, scales = 'free')

stim_pred_all %>% # something is wrong with this - they're only ctd
  ggplot()+
  geom_density(aes(x = probability, colour = predict_cat))+
  facet_wrap(stim_type ~ f_age_num, scales = 'free_y')

contrasts_long %>%
  ggplot()+
  geom_density(aes(x = difference))+
  facet_grid(pred_type ~ contrast)

#pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1_contrasts_stimuli.pdf')
for(i in unique(contrasts_long$contrast)){
  plot <- contrasts_long %>%
    filter(contrast == i) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = pred_type))+
    facet_wrap(look_tminus1 ~ f_age_num,
               scales = 'free')+
    labs(title = i)
  print(plot)
}
#dev.off()
save.image('ele_playbacks/looking_direction/looking_ordinal_model1_stimuluscontrasts.RData')

## focal age ####
load('ele_playbacks/looking_direction/looking_ordinal_model1_stimuluscontrasts.RData')
rm(list = ls()[!ls() %in% c('lom1_fit','look_no_na')]) ; gc()

## create new dataframe to predict from
age_new <- look_no_na %>%
  dplyr::select(f_age_num, age_combo, stim_type, look_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))

## predict with original ages
age_look_org <- age_new
age_mtx_org <- posterior_epred(object = lom1_fit, newdata = age_look_org)
colnames(age_mtx_org) <- age_look_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with altered ages
age_look_alt <- age_new %>%
  mutate(f_age_num_original = f_age_num,
         age_combo_original = age_combo) %>%
  mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
  separate(age_combo, into = c('f_age_old','p_age'), sep = '_') %>%
  mutate(age_combo = paste0(f_age_num, '_', p_age)) %>%
  dplyr::select(f_age_num_original, f_age_num,
                age_combo_original, age_combo,
                stim_type, look_tminus1_num, after_stim,
                focal_id, stim_id, playback_id,
                unique_data_combo)
age_mtx_alt <- posterior_epred(object = lom1_fit, newdata = age_look_alt)
colnames(age_mtx_alt) <- age_look_alt$unique_data_combo
age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('ele_playbacks/looking_direction/looking_ordinal_model1_agecontrasts.RData')
# load('ele_playbacks/looking_direction/looking_ordinal_model1_agecontrasts.RData') # load('looking_direction/looking_ordinal_model1_agecontrasts.RData')

## summarise and convert to long format
age_pred <- age_look_org %>%
  #dplyr::select(-f_age_num) %>%
  mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
         age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
         age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
         age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
         age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
         age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
         age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
         age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
         age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
         age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
         age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
         age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd)) %>%
  pivot_longer(cols = c(age_org_prop1_mu,age_org_prop2_mu,age_org_prop3_mu,
                        age_alt_prop1_mu,age_alt_prop2_mu,age_alt_prop3_mu),
               names_to = 'focal_agelook_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(age_org_prop1_sd,age_org_prop2_sd,age_org_prop3_sd,
                        age_alt_prop1_sd,age_alt_prop2_sd,age_alt_prop3_sd),
               names_to = 'focal_agelook_sd', values_to = 'stdv_propn') %>%
  separate(col = focal_agelook_mu, into = c('focal_agelook_mu','mu'),
           sep = '_m', remove = T) %>%
  separate(col = focal_agelook_sd, into = c('focal_agelook_sd','sd'),
           sep = '_s', remove = T) %>%
  select(-mu, -sd) %>%
  filter(focal_agelook_mu == focal_agelook_sd) %>%
  separate(col = focal_agelook_mu, into = c('original_altered', 'look_pred'),
           sep = '_prop', remove = T) %>%
  select(-focal_agelook_sd) %>%
  mutate(look_pred = as.numeric(look_pred),
         f_age_num = ifelse(original_altered == 'age_org',
                            f_age_num,
                            ifelse(original_altered == 'age_alt' & f_age_num == 4,
                                   1, f_age_num + 1))) %>%
  mutate(pred_type = ifelse(look_pred == 1, 'look away',
                            ifelse(look_pred == 2, 'side on', 'look at')))

## convert full predictive distribution to long format
age_pred_all <- age_mtx_org[,,1] %>%
  as.data.frame()
colnames(age_pred_all) <- rownames(age_look_org)
age_pred_all <- pivot_longer(age_pred_all, cols = everything(),
                             names_to = 'rownum', values_to = 'probability')
age_look_org$rownum <- rownames(age_look_org)
age_pred_all <- age_pred_all %>%
  left_join(age_look_org, by = 'rownum') %>%
  mutate(predict_num = 1,
         predict_cat = 'look away')
for(i in 2:3){
  age_pred_i <- age_mtx_org[,,i] %>%
    as.data.frame()
  colnames(age_pred_i) <- rownames(age_look_org)
  age_pred_i <- pivot_longer(age_pred_i, cols = everything(),
                             names_to = 'rownum', values_to = 'probability')
  age_pred_i <- age_pred_i %>%
    left_join(age_look_org, by = 'rownum') %>%
    mutate(predict_num = i,
           predict_cat = ifelse(i == 2, 'side on', 'look at'))
  age_pred_all <- rbind(age_pred_all, age_pred_i)
}

## calculate contrasts
alt_vs_org_away <- age_mtx_alt[,which(look_no_na$f_age_num != 4),1] - age_mtx_org[,which(look_no_na$f_age_num != 4),1]
alt_vs_org_side <- age_mtx_alt[,which(look_no_na$f_age_num != 4),2] - age_mtx_org[,which(look_no_na$f_age_num != 4),2]
alt_vs_org_twds <- age_mtx_alt[,which(look_no_na$f_age_num != 4),3] - age_mtx_org[,which(look_no_na$f_age_num != 4),3]
away_12 <- age_mtx_alt[,which(look_no_na$f_age_num == 1),1] - age_mtx_org[,which(look_no_na$f_age_num == 1),1]
away_23 <- age_mtx_alt[,which(look_no_na$f_age_num == 2),1] - age_mtx_org[,which(look_no_na$f_age_num == 2),1]
away_34 <- age_mtx_alt[,which(look_no_na$f_age_num == 3),1] - age_mtx_org[,which(look_no_na$f_age_num == 3),1]
away_14 <- age_mtx_org[,which(look_no_na$f_age_num == 4),1] - age_mtx_alt[,which(look_no_na$f_age_num == 4),1]
side_12 <- age_mtx_alt[,which(look_no_na$f_age_num == 1),2] - age_mtx_org[,which(look_no_na$f_age_num == 1),2]
side_23 <- age_mtx_alt[,which(look_no_na$f_age_num == 2),2] - age_mtx_org[,which(look_no_na$f_age_num == 2),2]
side_34 <- age_mtx_alt[,which(look_no_na$f_age_num == 3),2] - age_mtx_org[,which(look_no_na$f_age_num == 3),2]
side_14 <- age_mtx_org[,which(look_no_na$f_age_num == 4),2] - age_mtx_alt[,which(look_no_na$f_age_num == 4),2]
twds_12 <- age_mtx_alt[,which(look_no_na$f_age_num == 1),3] - age_mtx_org[,which(look_no_na$f_age_num == 1),3]
twds_23 <- age_mtx_alt[,which(look_no_na$f_age_num == 2),3] - age_mtx_org[,which(look_no_na$f_age_num == 2),3]
twds_34 <- age_mtx_alt[,which(look_no_na$f_age_num == 3),3] - age_mtx_org[,which(look_no_na$f_age_num == 3),3]
twds_14 <- age_mtx_org[,which(look_no_na$f_age_num == 4),3] - age_mtx_alt[,which(look_no_na$f_age_num == 4),3]

## plot
plot(density(alt_vs_org_away), col = 'red', las = 1, xlim = c(-0.1, 0.1),
     main = 'contrasts between adjacent age category:\nred = look away, purple = side on, blue = look at')
lines(density(alt_vs_org_side), col = 'purple')
lines(density(alt_vs_org_twds), col = 'blue')

## calculate reportable values
mean(alt_vs_org_away) ; sd(alt_vs_org_away) # 0.0003594844 ± 0.002598793
mean(alt_vs_org_side) ; sd(alt_vs_org_side) # -4.037212e-05 ± 0.00321784
mean(alt_vs_org_twds) ; sd(alt_vs_org_twds) # -0.0003191122 ± 0.002609183

mean(away_12) ; sd(away_12) #  ± 
mean(away_23) ; sd(away_23) #  ± 
mean(away_34) ; sd(away_34) #  ± 
mean(away_14) ; sd(away_14) #  ± 

mean(side_12) ; sd(side_12) #  ± 
mean(side_23) ; sd(side_23) #  ± 
mean(side_34) ; sd(side_34) #  ± 
mean(side_14) ; sd(side_14) #  ± 

mean(twds_12) ; sd(twds_12) #  ± 
mean(twds_23) ; sd(twds_23) #  ± 
mean(twds_34) ; sd(twds_34) #  ± 
mean(twds_14) ; sd(twds_14) #  ± 

## summarise contrasts
contrasts <- look_no_na %>%
  filter(f_age_num != 4) %>%
  mutate(alt_vs_org_away_mu = apply(alt_vs_org_away, 2, mean),
         alt_vs_org_away_sd = apply(alt_vs_org_away, 2, sd),
         alt_vs_org_side_mu = apply(alt_vs_org_side, 2, mean),
         alt_vs_org_side_sd = apply(alt_vs_org_side, 2, sd),
         alt_vs_org_twds_mu = apply(alt_vs_org_twds, 2, mean),
         alt_vs_org_twds_sd = apply(alt_vs_org_twds, 2, sd))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(alt_vs_org_away_mu,alt_vs_org_side_mu,alt_vs_org_twds_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','vs','org','look_pred','mu'),
           sep = '_', remove = T) %>%
  select(-alt_vs_org_away_sd, -alt_vs_org_side_sd, -alt_vs_org_twds_sd, -alt, -vs, -org, -mu)

contrasts_age <- look_no_na %>%
  filter(f_age_num == 1) %>%
  mutate(age_contrast = '1v2') %>% 
  mutate(away_mu = apply(away_12, 2, mean),
         away_sd = apply(away_12, 2, sd),
         side_mu = apply(side_12, 2, mean),
         side_sd = apply(side_12, 2, sd),
         twds_mu = apply(twds_12, 2, mean),
         twds_sd = apply(twds_12, 2, sd))
cont_age2 <- look_no_na %>%
  filter(f_age_num == 2) %>%
  mutate(age_contrast = '2v3') %>% 
  mutate(away_mu = apply(away_23, 2, mean),
         away_sd = apply(away_23, 2, sd),
         side_mu = apply(side_23, 2, mean),
         side_sd = apply(side_23, 2, sd),
         twds_mu = apply(twds_23, 2, mean),
         twds_sd = apply(twds_23, 2, sd))
cont_age3 <- look_no_na %>%
  filter(f_age_num == 3) %>%
  mutate(age_contrast = '3v4') %>% 
  mutate(away_mu = apply(away_34, 2, mean),
         away_sd = apply(away_34, 2, sd),
         side_mu = apply(side_34, 2, mean),
         side_sd = apply(side_34, 2, sd),
         twds_mu = apply(twds_34, 2, mean),
         twds_sd = apply(twds_34, 2, sd))
cont_age4 <- look_no_na %>%
  filter(f_age_num == 4) %>%
  mutate(age_contrast = '1v4') %>% 
  mutate(away_mu = apply(away_14, 2, mean),
         away_sd = apply(away_14, 2, sd),
         side_mu = apply(side_14, 2, mean),
         side_sd = apply(side_14, 2, sd),
         twds_mu = apply(twds_14, 2, mean),
         twds_sd = apply(twds_14, 2, sd))
contrasts_age <- rbind(contrasts_age, cont_age2, cont_age3, cont_age4)
rm(cont_age2, cont_age3, cont_age4)

contrasts_age_long <- contrasts_age %>%
  pivot_longer(cols = c(away_mu,side_mu,twds_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('look_pred','mu'),
           sep = '_', remove = T) %>%
  select(-away_sd,-side_sd,-twds_sd,-mu)

## plot contrasts
age_pred %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = as.factor(f_age_num)))+
  facet_wrap(stim_type ~ pred_type, scales = 'free_y')

age_pred_all %>%
  ggplot()+
  geom_density(aes(x = probability, colour = predict_cat))+
  facet_wrap(stim_type ~ f_age_num, scales = 'free_y')

contrasts_long <- contrasts_long %>%
  separate(look_pred, into = c('look_pred','ages'),
           sep = 4, remove = T) %>% 
  mutate(pred_type = ifelse(look_pred == 'away', 'look away',
                            ifelse(look_pred == 'side', 'side on', 'look at'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('look away','side on','look at'))) %>%
  mutate(f_age_new = ifelse(f_age_num == 4, 1, f_age_num+1)) %>%
  select(pred_type, f_age_num, f_age_new, difference, stim_type, after_stim, look_tminus1_num) %>%
  mutate(contrast = paste0('org: ',f_age_num,', new: ', f_age_new)) %>%
  mutate(look_tminus1 = ifelse(look_tminus1_num == 1,
                             'look away at t-1',
                             ifelse(look_tminus1_num == 2,
                                    'side on at t-1',
                                    'look at at t-1')))
#pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1_contrasts_ages.pdf')
for(i in unique(contrasts_long$contrast)){
  plot <- contrasts_long %>%
    filter(contrast == i) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = pred_type))+
    facet_grid(look_tminus1 ~ stim_type,
               scales = 'free')+
    labs(title = i)
  print(plot)
}
#dev.off()

contrasts_age_long <- contrasts_age_long %>%
  mutate(pred_type = ifelse(look_pred == 'away', 'look away',
                            ifelse(look_pred == 'side', 'side on', 'look at'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('look away','side on','look at'))) %>%
  mutate(f_age_new = ifelse(f_age_num == 4, 1, f_age_num+1)) %>%
  select(pred_type, f_age_num, f_age_new, difference, stim_type, after_stim, look_tminus1_num) %>%
  mutate(contrast = paste0('org: ',f_age_num,', new: ', f_age_new)) %>%
  mutate(look_tminus1 = ifelse(look_tminus1_num == 1,
                               'look away at t-1',
                               ifelse(look_tminus1_num == 2,
                                      'side on at t-1',
                                      'look at at t-1')))

save.image('ele_playbacks/looking_direction/looking_ordinal_model1_agecontrasts.RData')

## clean up a bit #####
# load('ele_playbacks/looking_direction/looking_ordinal_model1_agecontrasts.RData') # load('looking_direction/looking_ordinal_model1_agecontrasts.RData')
rm(list = ls()[! ls() %in% c('alt_vs_org_away','alt_vs_org_side','alt_vs_org_twds',
                             'look_no_na','lom1_fit')]) ; gc()

## plot full density instead of means
colnames(alt_vs_org_away) <- look_no_na$data_row[which(look_no_na$f_age_num != 4)]
colnames(alt_vs_org_side) <- look_no_na$data_row[which(look_no_na$f_age_num != 4)]
colnames(alt_vs_org_twds) <- look_no_na$data_row[which(look_no_na$f_age_num != 4)]

mtx_to_df <- function(mtx, pred_type){
  df <- mtx %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(),
                 names_to = 'data_row',
                 values_to = 'contrast') %>%
    mutate(data_row = as.integer(data_row)) %>%
    left_join(look_no_na, by = 'data_row') %>%
    mutate(categories = factor(ifelse(f_age_num == 1,
                                      "10-15 to 16-20",
                                      ifelse(f_age_num == 2,
                                             "16-20 to 21-25",
                                             ifelse(f_age_num == 3,
                                                    "21-25 to 26-35",
                                                    "10-15 to 26-35"))),
                               levels = c("10-15 to 16-20", "16-20 to 21-25",
                                          "21-25 to 26-35","10-15 to 26-35"))) %>%
    mutate(contrast = ifelse(f_age_num == 4,
                             contrast * (-1), # age_contrast shows 4 -> 1 not 1-> 4
                             contrast),
           diff_cats = ifelse(f_age_num == 4,
                              'youngest to oldest', 'increase by one'),
           prediction_type = pred_type)
  return(df)
}
away <- mtx_to_df(alt_vs_org_away, pred_type = 'look away')
side <- mtx_to_df(alt_vs_org_side, pred_type = 'side on')
twds <- mtx_to_df(alt_vs_org_twds, pred_type = 'look at')

plot_contrasts <- rbind(away, side, twds) %>%
  mutate(prediction_type = factor(prediction_type,
                                  levels = c('look away',
                                             'side on',
                                             'look at')))

ggplot(plot_contrasts)+
  geom_density(aes(x = contrast,
                   fill = diff_cats, # fill = f_age_cat,
                   colour = diff_cats # colour = f_age_cat
  ),
  #fill = '#21918c', colour = '#21918c',
  alpha = 0.4)+
  scale_colour_viridis_d(begin = 0, end = 0.5)+
  scale_fill_viridis_d(begin = 0, end = 0.5)+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_grid(prediction_type ~ categories, scales = 'free_y')+#, nrow = 3, ncol = 4)+
  labs(x = 'contrast between age categories',
       fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
       colour = 'change in age\ncategory'  # colour = 'original\nage category'
  )+
  theme(legend.position = 'none')+ #c(0.8, 0.9))+
  theme_bw()
ggsave(plot = last_plot(), device = 'png',
       filename = 'looking_ordinal1_agecontrasts.png',
       path = 'outputs/looking_ordinal_model_1/',
       width = 2400, height = 3200, unit = 'px')
ggsave(plot = last_plot(), device = 'svg',
       filename = 'looking_ordinal1_agecontrasts.svg',
       path = 'outputs/looking_ordinal_model_1/',
       width = 2400, height = 3200, unit = 'px')

## time since stimulus ####
# load('looking_direction/looking_ordinal_model1_agecontrasts.RData')
rm(list = ls()[!ls() %in% c('lom1_fit','look_no_na')]) ; gc()

## create new dataframe to predict from
time_new <- look_no_na %>%
  dplyr::select(f_age_num, age_combo, stim_type, look_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))

## predict with original times
time_look_org <- time_new
time_mtx_org <- posterior_epred(object = lom1_fit, newdata = time_look_org)
colnames(time_mtx_org) <- time_look_org$unique_data_combo
time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +15 seconds
time_look_alt_0.25 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1/4) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.25 <- posterior_epred(object = lom1_fit, newdata = time_look_alt_0.25)
colnames(time_mtx_alt_0.25) <- time_look_alt_0.25$unique_data_combo
time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +30 seconds
time_look_alt_0.50 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1/2) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.50 <- posterior_epred(object = lom1_fit, newdata = time_look_alt_0.50)
colnames(time_mtx_alt_0.50) <- time_look_alt_0.50$unique_data_combo
time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +45 seconds
time_look_alt_0.75 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 3/4) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.75 <- posterior_epred(object = lom1_fit, newdata = time_look_alt_0.75)
colnames(time_mtx_alt_0.75) <- time_look_alt_0.75$unique_data_combo
time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +60 seconds
time_look_alt_1.00 <- time_new %>%
  # dplyr::select(f_age_num, age_combo, stim_type, look_tminus1_num, after_stim,
  #               focal_id, stim_id, playback_id) %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_1.00 <- posterior_epred(object = lom1_fit, newdata = time_look_alt_1.00)
colnames(time_mtx_alt_1.00) <- time_look_alt_1.00$unique_data_combo
time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('ele_playbacks/looking_direction/looking_ordinal_model1_timecontrasts.RData')
# load('ele_playbacks/looking_direction/looking_ordinal_model1_timecontrasts.RData')

## summarise and convert to long format
time_pred <- time_look_org %>%
  mutate(time_org_0.00_away_mu = apply(time_mtx_org[,,1], 2, mean),
         time_org_0.00_side_mu = apply(time_mtx_org[,,2], 2, mean),
         time_org_0.00_twds_mu = apply(time_mtx_org[,,3], 2, mean),
         time_org_0.00_away_sd = apply(time_mtx_org[,,1], 2, sd),
         time_org_0.00_side_sd = apply(time_mtx_org[,,2], 2, sd),
         time_org_0.00_twds_sd = apply(time_mtx_org[,,3], 2, sd),
         time_alt_0.25_away_mu = apply(time_mtx_alt_0.25[,,1], 2, mean),
         time_alt_0.25_side_mu = apply(time_mtx_alt_0.25[,,2], 2, mean),
         time_alt_0.25_twds_mu = apply(time_mtx_alt_0.25[,,3], 2, mean),
         time_alt_0.25_away_sd = apply(time_mtx_alt_0.25[,,1], 2, sd),
         time_alt_0.25_side_sd = apply(time_mtx_alt_0.25[,,2], 2, sd),
         time_alt_0.25_twds_sd = apply(time_mtx_alt_0.25[,,3], 2, sd),
         time_alt_0.50_away_mu = apply(time_mtx_alt_0.50[,,1], 2, mean),
         time_alt_0.50_side_mu = apply(time_mtx_alt_0.50[,,2], 2, mean),
         time_alt_0.50_twds_mu = apply(time_mtx_alt_0.50[,,3], 2, mean),
         time_alt_0.50_away_sd = apply(time_mtx_alt_0.50[,,1], 2, sd),
         time_alt_0.50_side_sd = apply(time_mtx_alt_0.50[,,2], 2, sd),
         time_alt_0.50_twds_sd = apply(time_mtx_alt_0.50[,,3], 2, sd),
         time_alt_0.75_away_mu = apply(time_mtx_alt_0.75[,,1], 2, mean),
         time_alt_0.75_side_mu = apply(time_mtx_alt_0.75[,,2], 2, mean),
         time_alt_0.75_twds_mu = apply(time_mtx_alt_0.75[,,3], 2, mean),
         time_alt_0.75_away_sd = apply(time_mtx_alt_0.75[,,1], 2, sd),
         time_alt_0.75_side_sd = apply(time_mtx_alt_0.75[,,2], 2, sd),
         time_alt_0.75_twds_sd = apply(time_mtx_alt_0.75[,,3], 2, sd),
         time_alt_1.00_away_mu = apply(time_mtx_alt_1.00[,,1], 2, mean),
         time_alt_1.00_side_mu = apply(time_mtx_alt_1.00[,,2], 2, mean),
         time_alt_1.00_twds_mu = apply(time_mtx_alt_1.00[,,3], 2, mean),
         time_alt_1.00_away_sd = apply(time_mtx_alt_1.00[,,1], 2, sd),
         time_alt_1.00_side_sd = apply(time_mtx_alt_1.00[,,2], 2, sd),
         time_alt_1.00_twds_sd = apply(time_mtx_alt_1.00[,,3], 2, sd)) %>%
  pivot_longer(cols = c(time_org_0.00_away_mu,time_org_0.00_side_mu,time_org_0.00_twds_mu,
                        time_alt_0.25_away_mu,time_alt_0.25_side_mu,time_alt_0.25_twds_mu,
                        time_alt_0.50_away_mu,time_alt_0.50_side_mu,time_alt_0.50_twds_mu,
                        time_alt_0.75_away_mu,time_alt_0.75_side_mu,time_alt_0.75_twds_mu,
                        time_alt_1.00_away_mu,time_alt_1.00_side_mu,time_alt_1.00_twds_mu),
               names_to = 'time_org_alt_lookdir_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(time_org_0.00_away_sd,time_org_0.00_side_sd,time_org_0.00_twds_sd,
                        time_alt_0.25_away_sd,time_alt_0.25_side_sd,time_alt_0.25_twds_sd,
                        time_alt_0.50_away_sd,time_alt_0.50_side_sd,time_alt_0.50_twds_sd,
                        time_alt_0.75_away_sd,time_alt_0.75_side_sd,time_alt_0.75_twds_sd,
                        time_alt_1.00_away_sd,time_alt_1.00_side_sd,time_alt_1.00_twds_sd),
               names_to = 'time_org_alt_lookdir_sd', values_to = 'stdv_propn') %>%
  separate(col = time_org_alt_lookdir_mu,
           into = c('time_mu','org_mu','alt_mu','lookdir_mu','mu'),
           sep = '_', remove = T) %>%
  separate(col = time_org_alt_lookdir_sd,
           into = c('time_sd','org_sd','alt_sd','lookdir_sd','sd'),
           sep = '_', remove = T) %>%
  select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>%
  filter(alt_mu == alt_sd & lookdir_sd == lookdir_mu) %>%
  mutate(look_pred = ifelse(lookdir_mu == 'away', 1,
                          ifelse(lookdir_mu == 'side', 2,
                                 ifelse(lookdir_mu == 'twds', 3, 4)))) %>%
  select(-alt_sd, -lookdir_mu, -lookdir_sd) %>%
  rename(mins_added = alt_mu) %>%
  mutate(pred_type = ifelse(look_pred == 1, 'look away',
                            ifelse(look_pred == 2, 'side on', 'look at')))

## convert full predictive distribution to long format
time_pred_all <- time_mtx_org[,,1] %>%
  as.data.frame()
colnames(time_pred_all) <- rownames(time_look_org)
time_pred_all <- pivot_longer(time_pred_all, cols = everything(),
                              names_to = 'rownum', values_to = 'probability')
time_look_org$rownum <- rownames(time_look_org)
time_pred_all <- time_pred_all %>%
  left_join(time_look_org, by = 'rownum') %>%
  mutate(predict_num = 1,
         predict_cat = 'look away')
for(i in 2:3){
  time_pred_new <- time_mtx_org[,,i] %>%
    as.data.frame()
  colnames(time_pred_new) <- rownames(time_look_org)
  time_pred_new <- pivot_longer(time_pred_new, cols = everything(),
                                names_to = 'rownum', values_to = 'probability')
  time_pred_new <- time_pred_new %>%
    left_join(time_look_org, by = 'rownum') %>%
    mutate(predict_num = i,
           predict_cat = ifelse(i == 2, 'side on', 'look at'))
  time_pred_all <- rbind(time_pred_all, time_pred_new)
}

## calculate contrasts
alt0.25_vs_0.00_away <- time_mtx_alt_0.25[,,1] - time_mtx_org[,,1]
alt0.25_vs_0.00_side <- time_mtx_alt_0.25[,,2] - time_mtx_org[,,2]
alt0.25_vs_0.00_twds <- time_mtx_alt_0.25[,,3] - time_mtx_org[,,3]

alt0.50_vs_0.25_away <- time_mtx_alt_0.50[,,1] - time_mtx_alt_0.25[,,1]
alt0.50_vs_0.25_side <- time_mtx_alt_0.50[,,2] - time_mtx_alt_0.25[,,2]
alt0.50_vs_0.25_twds <- time_mtx_alt_0.50[,,3] - time_mtx_alt_0.25[,,3]

alt0.75_vs_0.50_away <- time_mtx_alt_0.75[,,1] - time_mtx_alt_0.50[,,1]
alt0.75_vs_0.50_side <- time_mtx_alt_0.75[,,2] - time_mtx_alt_0.50[,,2]
alt0.75_vs_0.50_twds <- time_mtx_alt_0.75[,,3] - time_mtx_alt_0.50[,,3]

alt1.00_vs_0.75_away <- time_mtx_alt_1.00[,,1] - time_mtx_alt_0.75[,,1]
alt1.00_vs_0.75_side <- time_mtx_alt_1.00[,,2] - time_mtx_alt_0.75[,,2]
alt1.00_vs_0.75_twds <- time_mtx_alt_1.00[,,3] - time_mtx_alt_0.75[,,3]

## plot contrasts -- no effect of time whatsoever
plot(density(alt0.25_vs_0.00_away), col = 'red', las = 1, xlim = c(-0.001, 0.001),
     main = 'contrasts between stim types:\nsolid = look away, dashed = side on, dotted = older;\nred = 0-15s, blue = 15-30s, green = 30-45s, black = 45-60s')
mean(alt0.25_vs_0.00_away) ; sd(alt0.25_vs_0.00_away)
# -0.0001907893 ± 0.0005875004
lines(density(alt0.50_vs_0.25_away), col = 'blue')
mean(alt0.50_vs_0.25_away) ; sd(alt0.50_vs_0.25_away)
# -0.000262113 ± 0.0006254504
lines(density(alt0.75_vs_0.50_away), col = 'green')
mean(alt0.75_vs_0.50_away) ; sd(alt0.75_vs_0.50_away)
# -0.000292715 ± 0.0007181015
lines(density(alt1.00_vs_0.75_away), col = 'black')
mean(alt1.00_vs_0.75_away) ; sd(alt1.00_vs_0.75_away)
# -0.0001711341 ± 0.0007348472
lines(density(alt0.25_vs_0.00_side), col = 'red', lty = 2)
mean(alt0.25_vs_0.00_side) ; sd(alt0.25_vs_0.00_side)
# 6.358331e-05 ± 0.0006851622
lines(density(alt0.50_vs_0.25_side), col = 'blue', lty = 2)
mean(alt0.50_vs_0.25_side) ; sd(alt0.50_vs_0.25_side)
# 8.563393e-05 ± 0.0007473229
lines(density(alt0.75_vs_0.50_side), col = 'green', lty = 2)
mean(alt0.75_vs_0.50_side) ; sd(alt0.75_vs_0.50_side)
# 9.272578e-05 ± 0.0008624582
lines(density(alt1.00_vs_0.75_side), col = 'black')
mean(alt1.00_vs_0.75_side) ; sd(alt1.00_vs_0.75_side)
# 4.990596e-05 ± 0.0008522631
lines(density(alt0.25_vs_0.00_twds), col = 'red', lty = 3)
mean(alt0.25_vs_0.00_twds) ; sd(alt0.25_vs_0.00_twds)
# 0.000127206 ± 0.0005434647
lines(density(alt0.50_vs_0.25_twds), col = 'blue', lty = 3)
mean(alt0.50_vs_0.25_twds) ; sd(alt0.50_vs_0.25_twds)
# 0.0001764791 ± 0.0005787657
lines(density(alt0.75_vs_0.50_twds), col = 'green', lty = 3)
mean(alt0.75_vs_0.50_twds) ; sd(alt0.75_vs_0.50_twds)
# 0.0001999892 ± 0.000660076
lines(density(alt1.00_vs_0.75_twds), col = 'black')
mean(alt1.00_vs_0.75_twds) ; sd(alt1.00_vs_0.75_twds)
# 0.0001212281 ± 0.0006654675

## summarise contrasts
contrasts <- look_no_na %>%
  mutate(alt0.25_vs_0.00_away_mu = apply(alt0.25_vs_0.00_away, 2, mean),
         alt0.25_vs_0.00_away_sd = apply(alt0.25_vs_0.00_away, 2, sd),
         alt0.25_vs_0.00_side_mu = apply(alt0.25_vs_0.00_side, 2, mean),
         alt0.25_vs_0.00_side_sd = apply(alt0.25_vs_0.00_side, 2, sd),
         alt0.25_vs_0.00_twds_mu = apply(alt0.25_vs_0.00_twds, 2, mean),
         alt0.25_vs_0.00_twds_sd = apply(alt0.25_vs_0.00_twds, 2, sd),
         alt0.50_vs_0.25_away_mu = apply(alt0.50_vs_0.25_away, 2, mean),
         alt0.50_vs_0.25_away_sd = apply(alt0.50_vs_0.25_away, 2, sd),
         alt0.50_vs_0.25_side_mu = apply(alt0.50_vs_0.25_side, 2, mean),
         alt0.50_vs_0.25_side_sd = apply(alt0.50_vs_0.25_side, 2, sd),
         alt0.50_vs_0.25_twds_mu = apply(alt0.50_vs_0.25_twds, 2, mean),
         alt0.50_vs_0.25_twds_sd = apply(alt0.50_vs_0.25_twds, 2, sd),
         alt0.75_vs_0.50_away_mu = apply(alt0.75_vs_0.50_away, 2, mean),
         alt0.75_vs_0.50_away_sd = apply(alt0.75_vs_0.50_away, 2, sd),
         alt0.75_vs_0.50_side_mu = apply(alt0.75_vs_0.50_side, 2, mean),
         alt0.75_vs_0.50_side_sd = apply(alt0.75_vs_0.50_side, 2, sd),
         alt0.75_vs_0.50_twds_mu = apply(alt0.75_vs_0.50_twds, 2, mean),
         alt0.75_vs_0.50_twds_sd = apply(alt0.75_vs_0.50_twds, 2, sd),
         alt1.00_vs_0.75_away_mu = apply(alt1.00_vs_0.75_away, 2, mean),
         alt1.00_vs_0.75_away_sd = apply(alt1.00_vs_0.75_away, 2, sd),
         alt1.00_vs_0.75_side_mu = apply(alt1.00_vs_0.75_side, 2, mean),
         alt1.00_vs_0.75_side_sd = apply(alt1.00_vs_0.75_side, 2, sd),
         alt1.00_vs_0.75_twds_mu = apply(alt1.00_vs_0.75_twds, 2, mean),
         alt1.00_vs_0.75_twds_sd = apply(alt1.00_vs_0.75_twds, 2, sd))
contrasts_long <- contrasts %>%
  select(-alt0.25_vs_0.00_away_sd,-alt0.25_vs_0.00_side_sd,-alt0.25_vs_0.00_twds_sd,
         -alt0.50_vs_0.25_away_sd,-alt0.50_vs_0.25_side_sd,-alt0.50_vs_0.25_twds_sd,
         -alt0.75_vs_0.50_away_sd,-alt0.75_vs_0.50_side_sd,-alt0.75_vs_0.50_twds_sd,
         -alt1.00_vs_0.75_away_sd,-alt1.00_vs_0.75_side_sd,-alt1.00_vs_0.75_twds_sd) %>%
  pivot_longer(cols = c(alt0.25_vs_0.00_away_mu,alt0.25_vs_0.00_side_mu,alt0.25_vs_0.00_twds_mu,
                        alt0.50_vs_0.25_away_mu,alt0.50_vs_0.25_side_mu,alt0.50_vs_0.25_twds_mu,
                        alt0.75_vs_0.50_away_mu,alt0.75_vs_0.50_side_mu,alt0.75_vs_0.50_twds_mu,
                        alt1.00_vs_0.75_away_mu,alt1.00_vs_0.75_side_mu,alt1.00_vs_0.75_twds_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','contrast'), sep = 3) %>%
  separate(contrast, into = c('later','vs','earlier','pred_type','mu'),
           sep = '_', remove = T) %>%
  select(-alt, -vs, -mu) %>%
  mutate(later = as.numeric(later),
         earlier = as.numeric(earlier),
         contrast = paste0(later,'_',earlier))

## plot contrasts
times <- unique(time_pred$after_stim)
time_pred %>%
  filter(after_stim %in% times[seq(1, length(times), length.out = 8)]) %>%
  mutate(after_stim = round(after_stim, 2)) %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = pred_type))+
  facet_wrap(as.factor(after_stim) ~ stim_type,
             scales = 'free')

time_pred_all %>%
  ggplot()+
  geom_density(aes(x = probability, colour = predict_cat))+
  facet_wrap(stim_type ~ f_age_num, scales = 'free_y')

contrasts_long <- contrasts_long %>%
  mutate(pred_type = ifelse(pred_type == 'away', 'look away',
                            ifelse(pred_type == 'side', 'side on', 'look at'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('look away','side on','look at'))) %>%
  select(pred_type, f_age_num, contrast, earlier, later, difference, stim_type, after_stim, look_tminus1_num) %>%
  mutate(look_tminus1 = ifelse(look_tminus1_num == 1,
                             'look away at t-1',
                             ifelse(look_tminus1_num == 2,
                                    'side on at t-1',
                                    'look at at t-1')))

for(i in unique(contrasts_long$contrast)){
  plot <- contrasts_long %>%
    filter(contrast == i) %>%
    filter(after_stim %in% times[seq(1, length(times), length.out = 8)]) %>%
    mutate(after_stim = round(after_stim, 2)) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = pred_type))+
    facet_grid(look_tminus1 ~ after_stim,
               scales = 'free')+
    labs(title = i)
  print(plot)
}

save.image('ele_playbacks/looking_direction/looking_ordinal_model1_timecontrasts.RData')
dev.off()
