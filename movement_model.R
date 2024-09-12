#### information ####
# script for movement direction analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
# library(tidyverse) ; library(brms) ; library(LaplacesDemon) ; library(patchwork)
library(StanHeaders, lib.loc = '../../packages/')
library(rstan, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')
library(tidyverse, lib.loc = '../../packages/')
#library(cmdstanr, lib.loc = '../../packages/') ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/')
#library(cmdstanr) ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')
library(ggridges, lib.loc = '../../packages/')

theme_set(theme_bw())
set.seed(12345)

#### import data for both models ####
# https://dagitty.net/dags.html?id=dw8twK
# read in data
ages <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS') %>%
  select(focal, f_age_cat, f_age_num) %>%
  distinct() %>%
  filter(!is.na(f_age_cat)) %>%
  mutate(partner = focal,
         p_age_cat = f_age_cat,
         p_age_num = f_age_num)

stim_starts <- readRDS('../data_processed/stimuli.RDS') %>%
  filter(status == 'START' & behavior == 'STIMULUS') %>%
  select(pb_num,time,stim_num,stim_type,group_size,comment)
table(stim_starts$pb_num)
#1  3  4  6  7 10 11 13 14 15 16 17 18 19 21 22 23 24 25 28 29 30 31 32 33 34 35 36 37 38 41 42 43 44 45 46 47 48 50 51 52 53 55 56 58 59 60 61
#1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  2  1  1  2  1  1  3  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  5  1  1  1  1  1  1
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

## movement data
cols_of_interest <- c('b1_move','b2_move','b3_move','b4_move',
                      'b5_move','b6_move','b7_move','b8_move')
cols_of_interest_name <- c('b1_move_name','b2_move_name','b3_move_name','b4_move_name',
                           'b5_move_name','b6_move_name','b7_move_name','b8_move_name')
cols_of_interest_index <- c('b1_move_index','b2_move_index','b3_move_index','b4_move_index',
                            'b5_move_index','b6_move_index','b7_move_index','b8_move_index')
move <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
  # select relevant variables
  select(subject,pb_num,second,out_frame_name,
         all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
  # convert to tidy format
  rename(b1_move = b1_move_name, b2_move = b2_move_name,
         b3_move = b3_move_name, b4_move = b4_move_name,
         b5_move = b5_move_name, b6_move = b6_move_name,
         b7_move = b7_move_name, b8_move = b8_move_name) %>%
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_name', values_to = 'moving_direction') %>%
  rename(b1_move = b1_move_index, b2_move = b2_move_index,
         b3_move = b3_move_index, b4_move = b4_move_index,
         b5_move = b5_move_index, b6_move = b6_move_index,
         b7_move = b7_move_index, b8_move = b8_move_index) %>%
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_index', values_to = 'move_index') %>%
  filter(elephant_activity_name == elephant_activity_index) %>%
  select(-elephant_activity_index) %>%
  # clean up
  rename(elephant_activity = elephant_activity_name,
         focal = subject) %>%
  # remove non-existent elephants (e.g. elephants 5-8 in a 4-elephant group)
  mutate(moving_direction = ifelse(out_frame_name == 'out_of_sight' &
                                            is.na(moving_direction) == FALSE,
                                   ifelse(moving_direction == 'impossible_partner',
                                          'impossible_partner','out_of_sight'),
                                   moving_direction),
         move_index = ifelse(out_frame_name == 'out_of_sight' &
                               is.na(moving_direction) == FALSE,
                           9, move_index)) %>%
  filter(is.na(move_index) == FALSE) %>%
  # remove movement relative to self
  filter(moving_direction != 'impossible_partner') %>%
  # join with explanatory variables
  separate(elephant_activity, into = c('partner','activity'),
           sep = '_', remove = T) %>%
  mutate(partner = paste0(partner, '_e', pb_num),
         pb_num = as.numeric(pb_num)) %>%
  left_join(ages[,c('focal','f_age_cat','f_age_num')], by = 'focal') %>%
  left_join(ages[,c('partner','p_age_cat','p_age_num')], by = 'partner') %>%
  left_join(stim_starts, by = 'pb_num') %>%
  # remove elephants with unknown ages
  filter(!is.na(f_age_num)) %>%  # b2_e13 + b2_e34 + b6_e7 = unknown age
  # create additional variables
  mutate(time_since_stim = second - stim_start,
         after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60),
         age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num),
                                 'partner_younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num),
                                        'matched',
                                        'partner_older'))) %>%
  # clean up
  select(pb_num,focal,partner,
         activity,moving_direction,move_index,
         stim_num,stim_type,
         time_since_stim, after_stim,
         f_age_cat,p_age_cat,f_age_num,p_age_num,
         age_difference) %>%
  mutate(f_age_num = as.factor(f_age_num),
         p_age_num = as.factor(p_age_num),
         age_combo = paste0(f_age_num,'_',p_age_num),
         move_tminus1 = NA,
         move_tminus1_num = NA)
rm(list = ls() [ ! ls() %in% 'move']) ; gc()
length(which(is.na(move$moving_direction) == TRUE)) # 0

## create variable for moving_direction at time t-1
focals <- unique(move$focal)
for(f in 1:length(focals)){
  focal <- move %>% filter(focal == focals[f])
  move <- move %>% anti_join(focal, by = 'focal')
  partners <- unique(focal$partner)
  for(p in 1:length(partners)){
    focal_partner <- focal %>% filter(partner == partners[p])
    focal <- focal %>% anti_join(focal_partner, by = 'partner')
    for(i in 2:nrow(focal_partner)){
      focal_partner$move_tminus1[i] <- focal_partner$moving_direction[i-1]
      focal_partner$move_tminus1_num[i] <- focal_partner$move_index[i-1]
    }
    focal <- rbind(focal, focal_partner)
  }
  move <- rbind(move, focal)
}
rm(list = ls()[ ! ls() %in% c('move', 'focals')]) ; gc()

############### Probability of moving ###############
pdf('../outputs/movement_binomial_model/movement_binomial_modelprep.pdf')

#### filter data ####
move_no_na <- move %>%
  # remove out of sight observations
  filter(move_index != 9) %>%
  filter(move_tminus1_num != 9) %>%
  # remove first observation so can't see what they were doing in previous second
  filter(is.na(move_tminus1) == FALSE) %>%
  # convert to binary move or no move
  mutate(move_index = ifelse(move_index == 0, 0, 1),
         moving_direction = ifelse(moving_direction == 'not_moving',
                                   'not_moving', 'moving'),
         move_tminus1_num = ifelse(move_tminus1_num == 0, 0, 1),
         move_tminus1 = ifelse(move_tminus1 == 'not_moving',
                                   'not_moving', 'moving')) %>%
  # clean up
  mutate(f_age_num = as.integer(f_age_num)) %>%
  mutate(focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num) %>%
  select(focal, moving_direction, move_index,
         f_age_cat, f_age_num,
         time_since_stim, after_stim, stim_type,
         move_tminus1, move_tminus1_num,
         focal_id, stim_id, playback_id) %>%
  distinct()
str(move_no_na)
# tibble [54,299 Ã— 13] (S3: tbl_df/tbl/data.frame)
# $ focal           : chr [1:54299] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# $ moving_direction: chr [1:54299] "not_moving" "not_moving" "not_moving" "not_moving" ...
# $ move_index      : num [1:54299] 0 0 0 0 0 0 0 0 0 0 ...
# $ f_age_cat       : chr [1:54299] "26-35" "26-35" "26-35" "26-35" ...
# $ f_age_num       : int [1:54299] 4 4 4 4 4 4 4 4 4 4 ...
# $ time_since_stim : num [1:54299] -81 -80 -79 -78 -77 -76 -75 -74 -73 -72 ...
# $ after_stim      : num [1:54299] 0 0 0 0 0 0 0 0 0 0 ...
# $ stim_type       : chr [1:54299] "ctd" "ctd" "ctd" "ctd" ...
# $ move_tminus1    : chr [1:54299] "not_moving" "not_moving" "not_moving" "not_moving" ...
# $ move_tminus1_num: num [1:54299] 0 0 0 0 0 0 0 0 0 0 ...
# $ focal_id        : int [1:54299] 1 1 1 1 1 1 1 1 1 1 ...
# $ stim_id         : int [1:54299] 5 5 5 5 5 5 5 5 5 5 ...
# $ playback_id     : num [1:54299] 1 1 1 1 1 1 1 1 1 1 ...

#### set priors ####
# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
get_prior(formula = move_index ~ 0 + mo(f_age_num) + stim_type +   # fixed effects
            s(after_stim) + move_tminus1_num +                     # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),          # random effects
          data = move_no_na,
          family = bernoulli("logit"))
#                prior class             coef       group resp dpar nlpar lb ub        source
#               (flat)     b                                                     default
#               (flat)     b      mof_age_num                                    (vectorized)
#               (flat)     b move_tminus1_num                                    (vectorized)
#               (flat)     b    safter_stim_1                                    (vectorized)
#               (flat)     b     stim_typectd                                    (vectorized)
#               (flat)     b       stim_typeh                                    (vectorized)
#               (flat)     b       stim_typel                                    (vectorized)
# student_t(3, 0, 2.5)    sd                                               0     default
# student_t(3, 0, 2.5)    sd                     focal_id                  0     (vectorized)
# student_t(3, 0, 2.5)    sd        Intercept    focal_id                  0     (vectorized)
# student_t(3, 0, 2.5)    sd                  playback_id                  0     (vectorized)
# student_t(3, 0, 2.5)    sd        Intercept playback_id                  0     (vectorized)
# student_t(3, 0, 2.5)    sd                      stim_id                  0     (vectorized)
# student_t(3, 0, 2.5)    sd        Intercept     stim_id                  0     (vectorized)
# student_t(3, 0, 2.5)   sds                                               0     default
# student_t(3, 0, 2.5)   sds    s(after_stim)                              0     (vectorized)
#         dirichlet(1)  simo     mof_age_num1                                    default

# centre on -1 (logit(0.25) approx. = -1, and more likely to be not moving than moving)
priors <- c(
  # focal age
  prior(normal(-1,1),     class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(-1,1),     class = b,    coef = stim_typectd),
  prior(normal(-1,1),     class = b,    coef = stim_typel),
  prior(normal(-1,1),     class = b,    coef = stim_typeh),
  # time spline
  prior(normal(-1,1),     class = b,    coef = safter_stim_1),
  # action in previous second
  prior(normal(-1,1),     class = b,    coef = move_tminus1_num))

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
mbm_prior <- brm(
  formula = move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(mbm_prior)

print(paste0('priors set and checked at ', Sys.time()))

## reset plotting
dev.off()
pdf('../outputs/movement_binomial_model/movement_binomial_modelchecks.pdf')

#### fit model ####
mbm_fit <- brm(
  formula = move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)
# Warning messages:
# 1: There were 107 divergent transitions after warmup. See https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup to find out why this is a problem and how to eliminate them.
# 2: Examine the pairs() plot to diagnose sampling problems
# 3: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#tail-ess

# save workspace
save.image('movement_direction/movement_binomial_run.RData') # save.image('ele_playbacks/movement_direction/movement_binomial_run.RData')

# inspect model
summary(mbm_fit)
# Family: bernoulli
# Links: mu = logit
# Formula: move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num + (1 | focal_id) + (1 | stim_id) + (1 | playback_id)
# Data: move_no_na (Number of observations: 54299)
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
#
# Smooth Terms:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(safter_stim_1)     5.83      2.21     2.60    11.78 1.01      441      191
#
# Group-Level Effects:
#   ~focal_id (Number of levels: 176)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.10      0.07     0.00     0.25 1.00      976     1731
#
# ~playback_id (Number of levels: 48)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.66      0.11     0.47     0.88 1.00      930     2155
#
# ~stim_id (Number of levels: 30)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.22      0.16     0.01     0.56 1.01      468     1383
#
# Population-Level Effects:
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# stim_typectd        -4.34      0.23    -4.77    -3.88 1.01     1638     1729
# stim_typeh          -3.68      0.26    -4.19    -3.18 1.00     1386     2323
# stim_typel          -3.69      0.29    -4.25    -3.11 1.00     1640     2310
# move_tminus1_num     7.04      0.07     6.91     7.17 1.00     4895     2705
# safter_stim_1       -0.81      0.98    -2.74     1.10 1.00     4673     2495
# mof_age_num         -0.21      0.07    -0.35    -0.09 1.00     1750     2663
#
# Simplex Parameters:
#                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mof_age_num1[1]     0.47      0.15     0.16     0.74 1.00     3277     2710
# mof_age_num1[2]     0.22      0.12     0.04     0.50 1.00     1928     2970
# mof_age_num1[3]     0.31      0.13     0.07     0.59 1.01     4481     2615
#
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message: There were 107 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

print(paste0('model fitted at ', Sys.time()))

#### check outputs ####
# load('movement_direction/movement_binomial_run.RData') # load('ele_playbacks/movement_direction/movement_binomial_run.RData')

## check Stan code
mbm_fit$model
# // generated with brms 2.20.4
# functions {
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
#   real partial_log_lik_lpmf(int[] seq, int start, int end, data int[] Y, data matrix X, vector b, int[] Xmo_1, vector simo_1, vector bsp, data matrix Xs, vector bs, data matrix Zs_1_1, vector s_1_1, data int[] J_1, data vector Z_1_1, vector r_1_1, data int[] J_2, data vector Z_2_1, vector r_2_1, data int[] J_3, data vector Z_3_1, vector r_3_1) {
#     real ptarget = 0;
#     int N = end - start + 1;
#     // initialize linear predictor term
#     vector[N] mu = rep_vector(0.0, N);
#     mu += Xs[start:end] * bs + Zs_1_1[start:end] * s_1_1;
#     for (n in 1:N) {
#       // add more terms to the linear predictor
#       int nn = n + start - 1;
#       mu[n] += (bsp[1]) * mo(simo_1, Xmo_1[nn]) + r_1_1[J_1[nn]] * Z_1_1[nn] + r_2_1[J_2[nn]] * Z_2_1[nn] + r_3_1[J_3[nn]] * Z_3_1[nn];
#     }
#     ptarget += bernoulli_logit_glm_lpmf(Y[start:end] | X[start:end], mu, b);
#     return ptarget;
#   }
# }
# data {
#   int<lower=1> N;  // total number of observations
#   array[N] int Y;  // response variable
#   int<lower=1> K;  // number of population-level effects
#   matrix[N, K] X;  // population-level design matrix
#   int<lower=1> Ksp;  // number of special effects terms
#   int<lower=1> Imo;  // number of monotonic variables
#   array[Imo] int<lower=1> Jmo;  // length of simplexes
#   array[N] int Xmo_1;  // monotonic variable
#   vector[Jmo[1]] con_simo_1;  // prior concentration of monotonic simplex
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
#   int seq[N] = sequence(1, N);
# }
# parameters {
#   vector[K] b;  // regression coefficients
#   simplex[Jmo[1]] simo_1;  // monotonic simplex
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
#   vector[N_1] r_1_1;  // actual group-level effects
#   vector[N_2] r_2_1;  // actual group-level effects
#   vector[N_3] r_3_1;  // actual group-level effects
#   real lprior = 0;  // prior contributions to the log posterior
#   // compute penalized spline coefficients
#   s_1_1 = sds_1[1] * zs_1_1;
#   r_1_1 = (sd_1[1] * (z_1[1]));
#   r_2_1 = (sd_2[1] * (z_2[1]));
#   r_3_1 = (sd_3[1] * (z_3[1]));
#   lprior += normal_lpdf(b[1] | -1, 1);
#   lprior += normal_lpdf(b[2] | -1, 1);
#   lprior += normal_lpdf(b[3] | -1, 1);
#   lprior += normal_lpdf(b[4] | -1, 1);
#   lprior += dirichlet_lpdf(simo_1 | con_simo_1);
#   lprior += normal_lpdf(bsp[1] | -1, 1);
#   lprior += normal_lpdf(bs[1] | -1, 1);
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
#     target += reduce_sum(partial_log_lik_lpmf, seq, grainsize, Y, X, b, Xmo_1, simo_1, bsp, Xs, bs, Zs_1_1, s_1_1, J_1, Z_1_1, r_1_1, J_2, Z_2_1, r_2_1, J_3, Z_3_1, r_3_1);
#   }
#   // priors including constants
#   target += lprior;
#   target += std_normal_lpdf(zs_1_1);
#   target += std_normal_lpdf(z_1[1]);
#   target += std_normal_lpdf(z_2[1]);
#   target += std_normal_lpdf(z_3[1]);
# }
# generated quantities {
# }

mbm_fit$formula
# move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num + (1 | focal_id) + (1 | stim_id) + (1 | playback_id)

## extract posterior distribution
draws <- as_draws_df(mbm_fit) %>%
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

print(paste0('posterior extracted at ',Sys.time()))

#### calculate log cumulative odds ####
(prop <- table(move_no_na$moving_direction) / nrow(move_no_na))
#    moving not_moving
# 0.1724525  0.8275475
(cum_prop <- cumsum(prop))
#    moving not_moving
# 0.1724525  1.0000000
(log_cum_odds <- logit(cum_prop))
#    moving not_moving
# -1.568344        Inf

#### plot marginal effects ####
## extract marginal effects
marg <- conditional_effects(mbm_fit,
                            effects = c('f_age_num', 'stim_type', 'after_stim', 'move_tminus1_num'),
                            #categorical = TRUE,
                            method = 'posterior_epred')
names(marg)
# "f_age_num" "stim_type" "after_stim" "move_tminus1_num"
agefocal_effect <- marg[[1]]
stim_effect <- marg[[2]]
time_effect <- marg[[3]]
prevsec_effect <- marg[[4]]

## plot marginal effects
(focal_age_plot <- ggplot(agefocal_effect)+
    geom_errorbar(aes(x = f_age_num,
                      #colour = cats__,
                      ymax = upper__, ymin = lower__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = f_age_num,
                   #colour = cats__,
                   y = estimate__),
               size = 3)+ # cex = 3?
    xlab(label = 'focal age')+
    ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('move away directly',
                                      'move away at an angle',
                                      'side on',
                                      'approach at an angle',
                                      'approach directly'))+
    scale_fill_viridis_d(name = 'movement direction:',
                         breaks = c('1','2','3','4','5'),
                         labels = c('move away directly',
                                    'move away at an angle',
                                    'side on',
                                    'approach at an angle',
                                    'approach directly'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)))
ggsave(plot = focal_age_plot, filename = '../outputs/movement_binomial_model/movement_binomial_marginaleffects_focalage.png', device = 'png',
       width = 8.3, height = 5.8)

focal_age_labels <- c('focal age category 1',
                      'focal age category 2',
                      'focal age category 3',
                      'focal age category 4')
names(focal_age_labels) <- 1:4

(stim_plot <- ggplot(stim_effect)+
    geom_errorbar(aes(x = stim_type,
                      #colour = cats__,
                      ymin = lower__, ymax = upper__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type,
                   #colour = cats__,
                   y = estimate__),
               cex = 3)+ # size = 3?
    xlab(label = 'stimulus type') + ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('move away directly',
                                      'move away at an angle',
                                      'side on',
                                      'approach at an angle',
                                      'approach directly'))+
    scale_x_discrete(breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = stim_plot, filename = '../outputs/movement_binomial_model/movement_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = '../outputs/movement_binomial_model/movement_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(mbm_fit, ndraws = 100) # really good fit

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_stim_typectd","b_stim_typeh","b_stim_typel",
                          "b_move_tminus1_num","bsp_mof_age_num",
                          "bs_safter_stim_1","sds_safter_stim_1",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing doesn't move brilliant, esp. for bsp_mofocal_age, but only horrendous one is playback ID

## stim type
dove <- draws_cut %>%
  filter(parameter == 'b_stim_typectd') %>%
  rename(dove = draw) %>%
  select(-parameter)
lion <- draws_cut %>%
  filter(parameter == 'b_stim_typel') %>%
  rename(lion = draw) %>%
  select(-parameter)
human <- draws_cut %>%
  filter(parameter == 'b_stim_typeh') %>%
  rename(human = draw) %>%
  select(-parameter)

dove_lion <- dove %>%
  left_join(lion, by = c('chain','position','draw_id')) %>%
  mutate(difference = lion - dove)
dove_human <- dove %>%
  left_join(human, by = c('chain','position','draw_id')) %>%
  mutate(difference = human - dove)
lion_human <- lion %>%
  left_join(human, by = c('chain','position','draw_id')) %>%
  mutate(difference = human - lion)

par(mfrow = c(3,1))
plot(density(dove_lion$difference), main = 'lion vs dove') ; abline(v = 0, lty = 2)
plot(density(dove_human$difference), main = 'lion vs human') ; abline(v = 0, lty = 2)
plot(density(lion_human$difference), main = 'human vs lion') ; abline(v = 0, lty = 2)
par(mfrow = c(1,1))

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

## movement direction in previous second
prevsec <- draws_cut %>% filter(parameter == 'b_move_tminus1_num')
plot(density(prevsec$draw), main = 't-1 slope') ; abline(v = 0, lty = 2)

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

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
save.image('movement_direction/movement_binomial_run.RData')

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
move_no_na %>%
  ggplot(aes(x = after_stim, y = move_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ stim_type,
             labeller = labeller(f_age_num = age_labels,
                                 stim_type = stim_labels))+
  scale_x_continuous(name = 'time since stimulus started')
print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('movement_direction/movement_binomial_run.RData') # save.image('ele_playbacks/movement_direction/movement_binomial_run.RData')
dev.off()

#### predict from model ####
pdf('../outputs/movement_binomial_model/movement_binomial_modelpredictions.pdf')
# load('movement_direction/movement_binomial_run.RData')
rm(list = ls()[! ls() %in% c('mbm_fit','move_no_na','move','focals')]) ; gc()

pred <- posterior_epred(object = mbm_fit,
                        newdata = move_no_na)
save.image('movement_direction/movement_binomial_predictions.RData')

## convert to data frame
move_no_na$data_row <- 1:nrow(move_no_na)
pred <- as.data.frame(pred)
colnames(pred) <- 1:nrow(move_no_na)
pred <- pred %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row', values_to = 'epred') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(move_no_na, by = 'data_row')

save.image('movement_direction/movement_binomial_predictions.RData')

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions ####
# load('movement_direction/movement_binomial_predictions.RData')

## make labels for movement in previous second
prevsec_labels <- c('not moving at t-1',
                    'moving at t-1')
names(prevsec_labels) <- c(0,1)

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
(ctd_plot <- pred %>%
    filter(stim_type == 'ctd',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_boxplot(aes(x = as.factor(f_age_num), y = epred,
                     fill = as.factor(move_tminus1_num))) +
    facet_grid(. ~ after_stim)+#,
               #labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'cape turtle dove (control)')+
    theme(legend.position = 'bottom'))
(lion_plot <- pred %>%
    filter(stim_type == 'l',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    # colour = factor(pred_type, levels = c('not moving', 'moving')),
                    # fill = factor(pred_type, levels = c('not moving', 'moving'))
                    fill = as.factor(move_tminus1_num)
                    )) +
    facet_grid(. ~ after_stim)+#,
               #labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'lion')+
    theme(legend.position = 'bottom'))
(human_plot <- pred %>%
    filter(stim_type == 'h',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    # colour = factor(pred_type, levels = c('not moving', 'moving')),
                    # fill = factor(pred_type, levels = c('not moving', 'moving'))
                    fill = as.factor(move_tminus1_num)
                    )) +
    facet_grid(. ~ after_stim)+#,
               #labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'human')+
    theme(legend.position = 'bottom'))
(ctd_plot + lion_plot + human_plot)+
  plot_annotation(tag_levels = 'a') +
  plot_layout(guides = "collect")# & theme(legend.position = 'bottom')
ggsave(plot = last_plot(),
       file = 'movement_binomial_predictions_violin.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 32, width = 16)

## reset plotting
dev.off()
pdf('../outputs/movement_binomial_model/movement_binomial_modelcontrasts.pdf')

rm(list = ls()[! ls() %in% c('move','focals')]) ; gc()

#### calculate posterior contrasts from predictions ####
load('movement_direction/movement_binomial_predictions.RData')

## stim type ####
move_new <- move_no_na %>%
  dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))

## redo predictions with different stimulus types: all doves
ctd_move <- move_new %>%
  mutate(stim_type = 'ctd')
ctd_mtx <- posterior_epred(object = mbm_fit, newdata = ctd_move)
colnames(ctd_mtx) <- ctd_move$unique_data_combo
ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all lions
lion_move <- move_new %>%
  mutate(stim_type = 'l')
lion_mtx <- posterior_epred(object = mbm_fit, newdata = lion_move)
colnames(lion_mtx) <- lion_move$unique_data_combo
lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all humans
human_move <- move_new %>%
  mutate(stim_type = 'h')
human_mtx <- posterior_epred(object = mbm_fit, newdata = human_move)
colnames(human_mtx) <- human_move$unique_data_combo
human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## calculate contrasts
ctd_vs_lion <- lion_mtx - ctd_mtx
ctd_vs_human <- human_mtx - ctd_mtx
lion_vs_human <- human_mtx - lion_mtx

## summarise contrasts
contrasts <- move_no_na %>%
  select(-stim_type) %>%
  mutate(ctd_vs_lion_mu = apply(ctd_vs_lion, 2, mean),
         ctd_vs_lion_sd = apply(ctd_vs_lion, 2, sd),
         ctd_vs_human_mu = apply(ctd_vs_human, 2, mean),
         ctd_vs_human_sd = apply(ctd_vs_human, 2, sd),
         lion_vs_human_mu = apply(lion_vs_human, 2, mean),
         lion_vs_human_sd = apply(lion_vs_human, 2, sd))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(ctd_vs_lion_mu, ctd_vs_human_mu, lion_vs_human_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('contrast','mu'),
           sep = -3, remove = T) %>%
  select(-mu, -ctd_vs_lion_sd, -ctd_vs_human_sd, -lion_vs_human_sd)

## save contrasts
save.image('movement_direction/movement_binomial_stimuluscontrasts.RData')

## produce values for reporting
median(ctd_vs_lion)  ; mean(ctd_vs_lion)  ; sd(ctd_vs_lion)
# 0.00688384         ; 0.01209256         ; 0.01565171
median(ctd_vs_human) ; mean(ctd_vs_human) ; sd(ctd_vs_human)
# 0.006664593        ; 0.01158384         ; 0.01539733
median(lion_vs_human); mean(lion_vs_human); sd(lion_vs_human)
# -9.648387e-05      ; -0.0005087185      ; 0.009427985

## plot contrasts
contrasts_long %>%
  mutate(contrast = ifelse(contrast == 'ctd_vs_human',
                           'dove -> human',
                           ifelse(contrast == 'ctd_vs_lion',
                                  'dove -> lion', 'lion -> human'))) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = contrast))+
  scale_colour_viridis_d()+
  labs(colour = 'effect of changing stimulus')

save.image('movement_direction/movement_binomial_stimuluscontrasts.RData')
rm(ctd_move, lion_move, human_move, ctd_mtx, human_mtx, lion_mtx) ; gc()

## focal age ####
load('movement_direction/movement_binomial_stimuluscontrasts.RData')
move_new <- move_new %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(stim_type, move_tminus1_num, after_stim,
                                                         focal_id, stim_id, playback_id))))

## predict with original ages
age_move_org <- move_new
age_mtx_org <- posterior_epred(object = mbm_fit, newdata = age_move_org)
colnames(age_mtx_org) <- age_move_org$unique_data_combo
#age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with altered ages
age_move_alt <- move_new %>%
  mutate(f_age_num_original = f_age_num) %>%
  mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
  relocate(f_age_num_original)
age_mtx_alt <- posterior_epred(object = mbm_fit, newdata = age_move_alt)
colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
#age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),]

save.image('movement_direction/movement_binomial_agecontrasts.RData')

## calculate contrasts
age_contrast <- age_mtx_alt - age_mtx_org

## plot overall effect
plot(density(age_contrast))

## plot effect per category
age1_vs_age2 <- age_contrast[,which(age_move_org$f_age_num == 1)]
age2_vs_age3 <- age_contrast[,which(age_move_org$f_age_num == 2)]
age3_vs_age4 <- age_contrast[,which(age_move_org$f_age_num == 3)]
age1_vs_age4 <- age_contrast[,which(age_move_org$f_age_num == 4)]*-1

## plot category effects
plot(density(age1_vs_age2), col = 'blue',
     xlim = c(-0.2,0), ylim = c(0,500), las = 1,
     main = 'contrasts between age categories:\nblue = 1->2, red = 2->3,\ngreen = 3->4, purple = 1->4')
lines(density(age2_vs_age3), col = 'red')
lines(density(age3_vs_age4), col = 'green')
lines(density(age1_vs_age4), col = 'purple')

## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
median(age1_vs_age2) ; mean(age1_vs_age2) ; sd(age1_vs_age2)
# -0.00397975        ; -0.006308783       ; 0.007132005
median(age2_vs_age3) ; mean(age2_vs_age3) ; sd(age2_vs_age3)
# -0.001453682       ; -0.002412628       ; 0.00299898
median(age3_vs_age4) ; mean(age3_vs_age4) ; sd(age3_vs_age4)
# -0.001878611       ; -0.003272488       ; 0.004519514
median(age1_vs_age4) ; mean(age1_vs_age4) ; sd(age1_vs_age4)
# -0.006148561       ; -0.009458391       ; 0.01100978

## save output
save.image('movement_direction/movement_binomial_agecontrasts.RData')

## plot predictions
pred <- pred %>% 
  mutate(draw_id = rep(1:4000, each = nrow(age_move_org)),
         stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
                                 ifelse(stim_type == 'l','lion','human'))) %>% 
  mutate(stim_type_long = factor(stim_type_long,
                                 levels = c('dove (control)','lion','human')))

pred %>% 
  mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving', 'no', 'yes')) %>% 
  mutate(move_tminus1 = factor(move_tminus1, levels = c('yes','no'))) %>% 
  ggplot()+
  geom_boxplot(aes(x = f_age_cat,
                   # fill = as.factor(move_index), # successfully predicts actual data
                   fill = move_tminus1, 
                   y = epred))+
  labs(x = 'focal age category',
       y = 'predicted probability of moving',
       fill = 'moving in previous second')+
  facet_wrap(. ~ stim_type_long)+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')

pred %>% 
  mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving',
                               'not moving at t-1','moving at t-1')) %>% 
  mutate(move_tminus1 = factor(move_tminus1, levels = c('moving at t-1',
                                                        'not moving at t-1'))) %>% 
  ggplot()+
  geom_density(aes(x = epred,
                   fill = f_age_cat),
               alpha = 0.4)+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density')+
  facet_grid(move_tminus1 ~ stim_type_long,
             scales = 'free')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')

pred %>% 
  mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving',
                               'not moving at t-1','moving at t-1')) %>% 
  mutate(move_tminus1 = factor(move_tminus1, levels = c('moving at t-1',
                                                        'not moving at t-1'))) %>% 
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          fill = f_age_cat),
                      alpha = 0.6)+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density')+
  facet_grid(stim_type_long ~ move_tminus1,
             scales = 'free_x')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
ggsave(filename = 'mbm_predicted_ridges.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 1800, width = 1500, units = 'px')

## plot contrasts
colnames(age_contrast) <- move_no_na$data_row
plot_contrasts <- age_contrast %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row',
               values_to = 'contrast') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(move_no_na, by = 'data_row') %>%
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
                            'youngest to oldest', 'increase by one'))
ggplot(plot_contrasts)+
  geom_density(aes(x = contrast,
                   fill = diff_cats, # fill = f_age_cat,
                   colour = diff_cats # colour = f_age_cat
                   ),
               #fill = '#21918c', colour = '#21918c',
               alpha = 0.4)+
  scale_x_continuous(limits = c(-0.1, 0.1))+
  scale_colour_viridis_d(begin = 0, end = 0.5)+
  scale_fill_viridis_d(begin = 0, end = 0.5)+
  facet_wrap(. ~ categories, scales = 'free_y')+
  labs(x = 'contrast between age categories',
       fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
       colour = 'change in age\ncategory'  # colour = 'original\nage category'
       )+
  theme(legend.position = 'none')+ #c(0.8, 0.9))+
  theme_bw()
ggsave(plot = last_plot(), device = 'png',
       filename = 'mbm_density_agecontrasts.png',
       path = '../outputs/movement_binomial_model/',
       width = 2400, height = 1800, unit = 'px')
ggsave(plot = last_plot(), device = 'svg',
       filename = 'mbm_density_agecontrasts.svg',
       path = '../outputs/movement_binomial_model/',
       width = 2400, height = 1800, unit = 'px')

## replot contrasts and check that it matches -- 2 different scripts, wrote plots independently -- same data seems to produce different graphs
age_contrast %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(),
               names_to = 'unique_data_combo',
               values_to = 'contrast') %>% 
  mutate(unique_data_combo = as.integer(unique_data_combo)) %>% 
  left_join(distinct(age_move_org), by = 'unique_data_combo') %>% 
  rename(f_age_num_org = f_age_num) %>% 
  mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1, f_age_num_org + 1)) %>% 
  mutate(f_age_cat_org = ifelse(f_age_num_org == 1, '10-15 yrs',
                                ifelse(f_age_num_org == 2, '16-20 yrs',
                                       ifelse(f_age_num_org == 3, '21-25 yrs',
                                              ifelse(f_age_num_org == 4, '26-35 yrs',
                                                     '>36 yrs')))),
         f_age_cat_alt = ifelse(f_age_num_alt == 1, '10-15 yrs',
                                ifelse(f_age_num_alt == 2, '16-20 yrs',
                                       ifelse(f_age_num_alt == 3, '21-25 yrs',
                                              ifelse(f_age_num_alt == 4, '26-35 yrs',
                                                     '>36 yrs')))),
         contrast = ifelse(f_age_num_org == 4, contrast*(-1), contrast)) %>% 
  relocate(f_age_num_alt, .after = (f_age_num_org)) %>% 
  relocate(f_age_cat_org, .after = (f_age_num_alt)) %>% 
  relocate(f_age_cat_alt, .after = (f_age_cat_org)) %>% 
  mutate(comparison = ifelse(f_age_num_org == 4,
                             paste0(f_age_cat_alt, ' to ', f_age_cat_org),
                             paste0(f_age_cat_org, ' to ', f_age_cat_alt))) %>% 
  mutate(prev_move = ifelse(move_tminus1_num == 0, 'not moving at t-1', 'moving at t-1'),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion',
                                   'human'))) %>% 
  mutate(stim_type = factor(stim_type,
                            levels = c('dove (control)', 'lion', 'human'))) %>% 
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = contrast,
                  fill = comparison),
              position = position_dodge(0.5))+
  geom_hline(yintercept = 0, lty = 2)+
  scale_fill_viridis_d()+
  facet_grid(stim_type ~ prev_move)+
  labs(fill = 'age comparison')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'mbm_violin_agecontrasts.png',
       path = '../outputs/movement_binomial_model/',
       height = 1600, width = 1600, unit = 'px')

## time since stimulus -- including all 0 times ####
load('movement_direction/movement_binomial_agecontrasts.RData')
rm(list = ls()[!ls() %in% c('mbm_fit','move_no_na','move','focals')]) ; gc()

## create new data frame to calculate from
time_new <- move_no_na %>%
  dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))

## predict with original times
time_move_org <- time_new
time_mtx_org <- posterior_epred(object = mbm_fit, newdata = time_move_org)
colnames(time_mtx_org) <- time_move_org$unique_data_combo
time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with shifted times: +15 seconds
time_move_alt_0.25 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1/4) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.25 <- posterior_epred(object = mbm_fit, newdata = time_move_alt_0.25)
colnames(time_mtx_alt_0.25) <- time_move_alt_0.25$unique_data_combo
time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with shifted times: +30 seconds
time_move_alt_0.50 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1/2) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.50 <- posterior_epred(object = mbm_fit, newdata = time_move_alt_0.50)
colnames(time_mtx_alt_0.50) <- time_move_alt_0.50$unique_data_combo
time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with shifted times: +45 seconds
time_move_alt_0.75 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 3/4) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.75 <- posterior_epred(object = mbm_fit, newdata = time_move_alt_0.75)
colnames(time_mtx_alt_0.75) <- time_move_alt_0.75$unique_data_combo
time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with shifted times: +60 seconds
time_move_alt_1.00 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_1.00 <- posterior_epred(object = mbm_fit, newdata = time_move_alt_1.00)
colnames(time_mtx_alt_1.00) <- time_move_alt_1.00$unique_data_combo
time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),]

save.image('movement_direction/movement_binomial_timecontrasts.RData')

## summarise and convert to long format
load('movement_direction/movement_binomial_timecontrasts.RData')
time_pred <- time_move_org %>%
  mutate(time_org_0.00_mu = apply(time_mtx_org, 2, mean),
         time_org_0.00_sd = apply(time_mtx_org, 2, sd),
         time_alt_0.25_mu = apply(time_mtx_alt_0.25, 2, mean),
         time_alt_0.25_sd = apply(time_mtx_alt_0.25, 2, sd),
         time_alt_0.50_mu = apply(time_mtx_alt_0.50, 2, mean),
         time_alt_0.50_sd = apply(time_mtx_alt_0.50, 2, sd),
         time_alt_0.75_mu = apply(time_mtx_alt_0.75, 2, mean),
         time_alt_0.75_sd = apply(time_mtx_alt_0.75, 2, sd),
         time_alt_1.00_mu = apply(time_mtx_alt_1.00, 2, mean),
         time_alt_1.00_sd = apply(time_mtx_alt_1.00, 2, sd)) %>%
  pivot_longer(cols = c(time_org_0.00_mu, time_alt_0.25_mu, time_alt_0.50_mu, time_alt_0.75_mu, time_alt_1.00_mu,),
               names_to = 'time_org_alt_prop_agemove_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(time_org_0.00_sd, time_alt_0.25_sd, time_alt_0.50_sd, time_alt_0.75_sd, time_alt_1.00_sd,),
               names_to = 'time_org_alt_prop_agemove_sd', values_to = 'stdv_propn') %>%
  separate(col = time_org_alt_prop_agemove_mu,
           into = c('time_mu','org_mu','alt_mu','mu'),
           sep = '_', remove = T) %>%
  separate(col = time_org_alt_prop_agemove_sd,
           into = c('time_sd','org_sd','alt_sd','sd'),
           sep = '_', remove = T) %>%
  select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>%
  filter(alt_mu == alt_sd) %>%
  dplyr::select(-alt_sd) %>%
  # mutate(move_pred = ifelse(prop_agemove_mu == 'prop1', 1,
  #                           ifelse(prop_agemove_mu == 'prop2', 2,
  #                                  ifelse(prop_agemove_mu == 'prop3', 3, 4)))) %>%
  # select(-prop_agemove_mu, -prop_agemove_sd) %>%
  # mutate(pred_type = ifelse(move_pred == 1, 'younger',
  #                           ifelse(move_pred == 2, 'matched', 'older'))) %>%
  rename(mins_added = alt_mu)

## calculate contrasts
alt0.25_vs_0.00 <- time_mtx_alt_0.25 - time_mtx_org
alt0.50_vs_0.25 <- time_mtx_alt_0.50 - time_mtx_alt_0.25
alt0.75_vs_0.50 <- time_mtx_alt_0.75 - time_mtx_alt_0.50
alt1.00_vs_0.75 <- time_mtx_alt_1.00 - time_mtx_alt_0.75
alt1.00_vs_0.00 <- time_mtx_alt_1.00 - time_mtx_org

## summarise contrasts
contrasts <- move_no_na %>%
  mutate(alt0.25_vs_0.00_mu = apply(alt0.25_vs_0.00, 2, mean),
         alt0.25_vs_0.00_sd = apply(alt0.25_vs_0.00, 2, sd),
         alt0.50_vs_0.25_mu = apply(alt0.50_vs_0.25, 2, mean),
         alt0.50_vs_0.25_sd = apply(alt0.50_vs_0.25, 2, sd),
         alt0.75_vs_0.50_mu = apply(alt0.75_vs_0.50, 2, mean),
         alt0.75_vs_0.50_sd = apply(alt0.75_vs_0.50, 2, sd),
         alt1.00_vs_0.75_mu = apply(alt1.00_vs_0.75, 2, mean),
         alt1.00_vs_0.75_sd = apply(alt1.00_vs_0.75, 2, sd),
         alt1.00_vs_0.00_mu = apply(alt1.00_vs_0.00, 2, mean),
         alt1.00_vs_0.00_sd = apply(alt1.00_vs_0.00, 2, sd))
contrasts_long <- contrasts %>%
  select(-alt0.25_vs_0.00_sd,-alt0.50_vs_0.25_sd,-alt0.75_vs_0.50_sd,
         -alt1.00_vs_0.75_sd,-alt1.00_vs_0.00_sd) %>%
  pivot_longer(cols = c(alt0.25_vs_0.00_mu,alt0.50_vs_0.25_mu,
                        alt0.75_vs_0.50_mu,alt1.00_vs_0.75_mu,alt1.00_vs_0.00_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','contrast'), sep = 3) %>%
  separate(contrast, into = c('later','vs','earlier','mu'),
           sep = '_', remove = T) %>%
  select(-alt, -vs, -mu) %>%
  mutate(later = as.numeric(later),
         earlier = as.numeric(earlier),
         contrast = paste0(later,'_',earlier))

## produce values for reporting
mean(alt0.25_vs_0.00) ; sd(alt0.25_vs_0.00)
#     0.004308305     ±     0.007889397     -- 15 seconds later
mean(alt0.50_vs_0.25) ; sd(alt0.50_vs_0.25)
#     0.002013707     ±     0.005036902     -- 15 seconds later
mean(alt0.75_vs_0.50) ; sd(alt0.75_vs_0.50)
#    -0.002103559     ±     0.004964896     -- 15 seconds later
mean(alt1.00_vs_0.75) ; sd(alt1.00_vs_0.75)
#    -0.003110743     ±     0.006282314     -- 15 seconds later
mean(alt1.00_vs_0.00) ; sd(alt1.00_vs_0.00)
#     0.00110771      ±     0.01238309      -- 1 minute later

## plot contrasts
time_pred %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = mins_added))+
  facet_grid(mins_added ~ stim_type)
contrasts_long %>%
  ggplot()+
  geom_density(aes(x = difference))+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_wrap(contrast ~ .)#, scales = 'free')

save.image('movement_direction/movement_binomial_timecontrasts.RData')

## time since stimulus -- starting from 1 second after stim starts ####
# load('movement_direction/movement_binomial_timecontrasts.RData')

## calculate contrasts
alt0.25_vs_0.00_nozeros <- alt0.25_vs_0.00[,which(time_move_org$after_stim > 0)]
alt0.50_vs_0.25_nozeros <- alt0.50_vs_0.25[,which(time_move_org$after_stim > 0)]
alt0.75_vs_0.50_nozeros <- alt0.75_vs_0.50[,which(time_move_org$after_stim > 0)]
alt1.00_vs_0.75_nozeros <- alt1.00_vs_0.75[,which(time_move_org$after_stim > 0)]
alt1.00_vs_0.00_nozeros <- alt1.00_vs_0.00[,which(time_move_org$after_stim > 0)]

## summarise contrasts
contrasts_nozeros <- move_no_na %>%
  filter(after_stim > 0) %>%
  mutate(alt0.25_vs_0.00_mu = apply(alt0.25_vs_0.00_nozeros, 2, mean),
         alt0.25_vs_0.00_sd = apply(alt0.25_vs_0.00_nozeros, 2, sd),
         alt0.50_vs_0.25_mu = apply(alt0.50_vs_0.25_nozeros, 2, mean),
         alt0.50_vs_0.25_sd = apply(alt0.50_vs_0.25_nozeros, 2, sd),
         alt0.75_vs_0.50_mu = apply(alt0.75_vs_0.50_nozeros, 2, mean),
         alt0.75_vs_0.50_sd = apply(alt0.75_vs_0.50_nozeros, 2, sd),
         alt1.00_vs_0.75_mu = apply(alt1.00_vs_0.75_nozeros, 2, mean),
         alt1.00_vs_0.75_sd = apply(alt1.00_vs_0.75_nozeros, 2, sd),
         alt1.00_vs_0.00_mu = apply(alt1.00_vs_0.00_nozeros, 2, mean),
         alt1.00_vs_0.00_sd = apply(alt1.00_vs_0.00_nozeros, 2, sd))
contrasts_long_nozeros <- contrasts_nozeros %>%
  select(-alt0.25_vs_0.00_sd,-alt0.50_vs_0.25_sd,-alt0.75_vs_0.50_sd,
         -alt1.00_vs_0.75_sd,-alt1.00_vs_0.00_sd) %>%
  pivot_longer(cols = c(alt0.25_vs_0.00_mu,alt0.50_vs_0.25_mu,
                        alt0.75_vs_0.50_mu,alt1.00_vs_0.75_mu,alt1.00_vs_0.00_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','contrast'), sep = 3) %>%
  separate(contrast, into = c('later','vs','earlier','mu'),
           sep = '_', remove = T) %>%
  select(-alt, -vs, -mu) %>%
  mutate(later = as.numeric(later),
         earlier = as.numeric(earlier),
         contrast = paste0(later,'_',earlier))

## produce values for reporting
mean(alt0.25_vs_0.00_nozeros) ; sd(alt0.25_vs_0.00_nozeros)
#     0.0001018912            ±         0.005182471
mean(alt0.50_vs_0.25_nozeros) ; sd(alt0.50_vs_0.25_nozeros)
#     -0.0006757466           ±         0.005095702
mean(alt0.75_vs_0.50_nozeros) ; sd(alt0.75_vs_0.50_nozeros)
#     -0.0008940275           ±         0.006001355
mean(alt1.00_vs_0.75_nozeros) ; sd(alt1.00_vs_0.75_nozeros)
#     -0.0006255946           ±         0.006556937
mean(alt1.00_vs_0.00_nozeros) ; sd(alt1.00_vs_0.00_nozeros)
#     -0.002093478            ±          0.01632239

## plot contrasts
contrasts_long_nozeros %>%
  ggplot()+
  geom_density(aes(x = difference))+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_wrap(contrast ~ ., scales = 'free'
  )
save.image('movement_direction/movement_binomial_timecontrasts.RData')
dev.off()
print(paste0('probability of moving complete at ',Sys.time()))

############# Probability of different directions once moving ###############
pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelprep.pdf')

#### filter data ####
move_no_na <- move %>%
  # remove out of sight observations
  filter(move_index != 9) %>%
  filter(move_tminus1_num != 9) %>%
  # remove all times when elephant was not moving so only about direction
  filter(moving_direction != 'not_moving') %>%
  filter(move_tminus1 != 'not_moving') %>%
  filter(is.na(move_tminus1) == FALSE) %>%
  # remove any occasions with missing age data
  filter(is.na(f_age_num) == FALSE) %>%
  filter(is.na(p_age_num) == FALSE) %>%
  filter(is.na(age_difference) == FALSE) %>%
  # clean up
  mutate(age_difference = factor(age_difference,
                                 levels = c('partner_younger',
                                            'matched',
                                            'partner_older')),
         moving_direction = factor(moving_direction,
                                   levels = c('move away directly',
                                              'move away at an angle',
                                              'move directly with',
                                              'approach at an angle',
                                              'approach directly')),
         move_tminus1 = factor(move_tminus1,
                                   levels = c('move away directly',
                                              'move away at an angle',
                                              'move directly with',
                                              'approach at an angle',
                                              'approach directly'))) %>%
  mutate(age_diff_num = as.integer(age_difference),
         f_age_num = as.integer(f_age_num),
         p_age_num = as.integer(p_age_num)) %>%
  mutate(age_combo = paste0(f_age_num, '_', p_age_num),
         focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num) %>%
  select(focal, partner, moving_direction, move_index,
         f_age_cat, p_age_cat, f_age_num, p_age_num,
         age_difference, age_diff_num, age_combo,
         time_since_stim, after_stim, stim_type,
         move_tminus1, move_tminus1_num,
         focal_id, stim_id, playback_id)
str(move_no_na)
# tibble [29,814 Ã— 19] (S3: tbl_df/tbl/data.frame)
# $ focal           : chr [1:29814] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# $ partner         : chr [1:29814] "b2_e1" "b2_e1" "b2_e1" "b2_e1" ...
# $ moving_direction: Factor w/ 5 levels "move away directly",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ move_index      : num [1:29814] 1 1 1 1 1 1 1 1 1 1 ...
# $ f_age_cat       : chr [1:29814] "26-35" "26-35" "26-35" "26-35" ...
# $ p_age_cat       : chr [1:29814] "26-35" "26-35" "26-35" "26-35" ...
# $ f_age_num       : int [1:29814] 4 4 4 4 4 4 4 4 4 4 ...
# $ p_age_num       : int [1:29814] 4 4 4 4 4 4 4 4 4 4 ...
# $ age_difference  : Factor w/ 3 levels "partner_younger",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ age_diff_num    : int [1:29814] 2 2 2 2 2 2 2 2 2 2 ...
# $ age_combo       : chr [1:29814] "4_4" "4_4" "4_4" "4_4" ...
# $ time_since_stim : num [1:29814] -10 -9 -8 -7 -6 227 228 229 230 231 ...
# $ after_stim      : num [1:29814] 0 0 0 0 0 ...
# $ stim_type       : chr [1:29814] "ctd" "ctd" "ctd" "ctd" ...
# $ move_tminus1    : Factor w/ 5 levels "move away directly",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ move_tminus1_num: num [1:29814] 1 1 1 1 1 1 1 1 1 1 ...
# $ focal_id        : int [1:29814] 1 1 1 1 1 1 1 1 1 1 ...
# $ stim_id         : int [1:29814] 5 5 5 5 5 5 5 5 5 5 ...
# $ playback_id     : num [1:29814] 1 1 1 1 1 1 1 1 1 1 ...

#### set priors ####
# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
get_prior(formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
            s(after_stim) + mo(move_tminus1_num) +                             # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),                      # random effects
          data = move_no_na,
          family = cumulative("logit"))
#                prior     class                coef       group resp dpar nlpar  lb ub       source
#               (flat)         b                                                             default
#               (flat)         b        age_combo1_2                                    (vectorized)
#               (flat)         b        age_combo1_3                                    (vectorized)
#               (flat)         b        age_combo1_4                                    (vectorized)
#               (flat)         b        age_combo2_1                                    (vectorized)
#               (flat)         b        age_combo2_2                                    (vectorized)
#               (flat)         b        age_combo2_3                                    (vectorized)
#               (flat)         b        age_combo2_4                                    (vectorized)
#               (flat)         b        age_combo3_1                                    (vectorized)
#               (flat)         b        age_combo3_2                                    (vectorized)
#               (flat)         b        age_combo3_3                                    (vectorized)
#               (flat)         b        age_combo3_4                                    (vectorized)
#               (flat)         b        age_combo4_1                                    (vectorized)
#               (flat)         b        age_combo4_2                                    (vectorized)
#               (flat)         b        age_combo4_3                                    (vectorized)
#               (flat)         b        age_combo4_4                                    (vectorized)
#               (flat)         b         mof_age_num                                    (vectorized)
#               (flat)         b  momove_tminus1_num                                    (vectorized)
#               (flat)         b       safter_stim_1                                    (vectorized)
#               (flat)         b          stim_typeh                                    (vectorized)
#               (flat)         b          stim_typel                                    (vectorized)
# student_t(3, 0, 2.5) Intercept                                                             default
# student_t(3, 0, 2.5) Intercept                   1                                    (vectorized)
# student_t(3, 0, 2.5) Intercept                   2                                    (vectorized)
# student_t(3, 0, 2.5) Intercept                   3                                    (vectorized)
# student_t(3, 0, 2.5) Intercept                   4                                    (vectorized)
# student_t(3, 0, 2.5)        sd                                                   0         default
# student_t(3, 0, 2.5)        sd                        focal_id                   0    (vectorized)
# student_t(3, 0, 2.5)        sd           Intercept    focal_id                   0    (vectorized)
# student_t(3, 0, 2.5)        sd                     playback_id                   0    (vectorized)
# student_t(3, 0, 2.5)        sd           Intercept playback_id                   0    (vectorized)
# student_t(3, 0, 2.5)        sd                         stim_id                   0    (vectorized)
# student_t(3, 0, 2.5)        sd           Intercept     stim_id                   0    (vectorized)
# student_t(3, 0, 2.5)       sds                                                   0         default
# student_t(3, 0, 2.5)       sds       s(after_stim)                               0    (vectorized)
#         dirichlet(1)      simo        mof_age_num1                                         default
#         dirichlet(1)      simo momove_tminus1_num1                                         default

priors <- c(
  # focal age
  prior(normal(0,0.25),   class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # interaction age
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
  # stimulus type
  prior(normal(0,1),      class = b,    coef = stim_typeh),
  prior(normal(0,1),      class = b,    coef = stim_typel),
  # time spline
  prior(normal(0,1),        class = b,    coef = safter_stim_1),
  #prior(student_t(3,0,2.5), class = sds,  coef = s(after_stim)), # included in move, wasn't in movement direction but not sure why?
  # action in previous second
  prior(normal(0,0.333),  class = b,    coef = momove_tminus1_num),
  prior(dirichlet(2,2,2,2),     class = simo, coef = momove_tminus1_num1))

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
mom1_prior <- brm(
  formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +
    s(after_stim) + mo(move_tminus1_num) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
# Warning message: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#tail-ess

pp_check(mom1_prior)

print(paste0('priors set and checked at ', Sys.time()))

## reset plotting
dev.off()
pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelchecks.pdf')

#### fit model ####
mom1_fit <- brm(
  formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +
    s(after_stim) + mo(move_tminus1_num) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)
# Warning messages:
# 1: There were 23 divergent transitions after warmup. See
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup to find out why this is a problem and how to eliminate them.
# 2: Examine the pairs() plot to diagnose sampling problems

# save workspace
save.image('movement_direction/movement_ordinal_model1_run.RData') # save.image('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')

# inspect model
summary(mom1_fit)
# Family: cumulative
# Links: mu = logit; disc = identity
# Formula: move_index ~ 1 + mo(f_age_num) + age_combo + stim_type + s(after_stim) + mo(move_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id)
# Data: move_no_na (Number of observations: 29814)
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; total post-warmup draws = 4000
#
# Smooth Terms:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(safter_stim_1)     0.36      0.35     0.02     1.31 1.00     1670     2441
#
# Group-Level Effects:
#   ~focal_id (Number of levels: 155)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.17      0.06     0.03     0.28 1.01      628      579
#
# ~playback_id (Number of levels: 45)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.07      0.05     0.00     0.18 1.00      994     1087
#
# ~stim_id (Number of levels: 29)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.08      0.05     0.01     0.20 1.00     1267     1524
#
# Population-Level Effects:
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]           3.90      0.22     3.47     4.33 1.00     1532     2131
# Intercept[2]          10.21      0.24     9.74    10.67 1.00     1689     2370
# Intercept[3]          13.65      0.25    13.16    14.14 1.00     1844     2672
# Intercept[4]          19.71      0.27    19.19    20.25 1.00     1859     2452
# age_combo1_2           0.10      0.24    -0.35     0.55 1.00     1778     2564
# age_combo1_3           0.03      0.24    -0.43     0.50 1.00     1713     2506
# age_combo1_4           0.23      0.31    -0.37     0.84 1.00     2452     2722
# age_combo2_1           0.04      0.24    -0.42     0.51 1.00     1145     2058
# age_combo2_2           0.02      0.21    -0.38     0.43 1.01      808     1391
# age_combo2_3           0.09      0.20    -0.29     0.50 1.01      788     1346
# age_combo2_4          -0.01      0.23    -0.45     0.44 1.00      818     1985
# age_combo3_1          -0.06      0.28    -0.62     0.49 1.00      727     1646
# age_combo3_2          -0.02      0.26    -0.51     0.49 1.01      623     1283
# age_combo3_3          -0.01      0.26    -0.50     0.50 1.01      649     1304
# age_combo3_4          -0.28      0.28    -0.80     0.27 1.00      723     1595
# age_combo4_1           0.01      0.43    -0.85     0.86 1.00     1000     2028
# age_combo4_2           0.12      0.37    -0.61     0.82 1.01      695     1491
# age_combo4_3           0.25      0.36    -0.47     0.93 1.01      762     1283
# age_combo4_4           0.09      0.38    -0.66     0.80 1.00      772     1287
# stim_typeh            -0.04      0.09    -0.22     0.15 1.00     3490     3163
# stim_typel            -0.07      0.11    -0.28     0.15 1.00     3343     3054
# safter_stim_1          0.36      0.59    -0.71     1.73 1.00     2403     2225
# mof_age_num           -0.07      0.13    -0.33     0.19 1.00      972     1743
# momove_tminus1_num     5.87      0.05     5.78     5.96 1.00     3745     2876
#
# Simplex Parameters:
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mof_age_num1[1]            0.33      0.18     0.05     0.72 1.00     3839     2640
# mof_age_num1[2]            0.33      0.17     0.05     0.70 1.00     4989     2890
# mof_age_num1[3]            0.34      0.18     0.05     0.72 1.00     4998     2813
# momove_tminus1_num1[1]     0.30      0.00     0.30     0.31 1.00     4088     2820
# momove_tminus1_num1[2]     0.21      0.00     0.20     0.21 1.00     4109     2995
# momove_tminus1_num1[3]     0.21      0.00     0.20     0.21 1.00     3944     2887
# momove_tminus1_num1[4]     0.28      0.00     0.28     0.29 1.00     6570     3332
#
# Family Specific Parameters:
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA

# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message: There were 23 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

print(paste0('model fitted at ', Sys.time()))

#### check outputs ####
# load('movement_direction/movement_ordinal_model1_run.RData') # load('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')
## check Stan code
mom1_fit$model
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
#   lprior += normal_lpdf(bsp[1] | 0, 0.25);
#   lprior += normal_lpdf(bsp[2] | 0, 0.333);
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

mom1_fit$formula
# move_index ~ 1 + mo(f_age_num) + age_combo + stim_type + s(after_stim) + mo(move_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id)

## extract posterior distribution
draws <- as_draws_df(mom1_fit) %>%
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
# nearest neighbour version -- run with movement direction first but if it throws an error then come back to the nearest neighbour code to fix it
# draws <- as_draws_df(move_fit) %>%
#   select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>%
#   pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>%
#   mutate(iteration = rep(rep(1:(num_iter/2),
#                              each = length(unique(parameter))),
#                          num_chains),
#          chain = rep(1:num_chains,
#                      each = length(unique(parameter))*(num_iter/2)),
#          invlogit_draw = invlogit(draw))

print(paste0('posterior extracted at ',Sys.time()))

## move at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

#### calculate log cumulative odds ####
prop <- table(move_no_na$moving_direction) / nrow(move_no_na)
cum_prop <- cumsum(prop)
log_cum_odds <- logit(cum_prop)

#### plot marginal effects ####
## extract marginal effects
marg <- conditional_effects(mom1_fit,
                            categorical = TRUE,
                            method = 'posterior_epred')
names(marg)
# "age_combo:cats__" "stim_type:cats__" "f_age_num:cats__" "move_tminus1_num:cats__" "after_stim:cats__"
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
    ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('move away directly',
                                      'move away at an angle',
                                      'side on',
                                      'approach at an angle',
                                      'approach directly'))+
    scale_fill_viridis_d(name = 'movement direction:',
                         breaks = c('1','2','3','4','5'),
                         labels = c('move away directly',
                                    'move away at an angle',
                                    'side on',
                                    'approach at an angle',
                                    'approach directly'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)))
ggsave(plot = focal_age_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_focalage_agecombo.png', device = 'png',
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
      colour = as.factor(cats__), # movement direction?
      ymax = upper__, ymin = lower__),
      linewidth = 1,
      width = 0.4)+
    geom_point(aes(#x = agecombo,
      x = partner_age,
      colour = as.factor(cats__),    # movement direction?
      #shape = focal_age,
      y = estimate__),
      size = 3)+
    # geom_ribbon(aes(#x = agecombo,
    #                 x = as.numeric(partner_age),
    #                 fill = as.factor(cats__),     # movement direction?
    #                 ymax = upper__, ymin = lower__),
    #             alpha = 0.4)+
    # geom_line(aes(#x = agecombo,
    #               x = as.numeric(partner_age),
    #               colour = as.factor(cats__),     # movement direction?
    #               y = estimate__),
    #           linewidth = 1)+
    facet_wrap(. ~ focal_age,
               labeller = labeller(focal_age = focal_age_labels))+
    ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('move away directly',
                                      'move away at an angle',
                                      'side on',
                                      'approach at an angle',
                                      'approach directly'))+
    # scale_fill_viridis_d(name = 'movement direction:',
    #                      breaks = c('1','2','3','4','5'),
    #                      labels = c('move away directly',
    #                                 'move away at an angle',
    #                                 'side on',
    #                                 'approach at an angle',
    #                                 'approach directly'))+
    scale_x_discrete(name = 'partner age category')+
    #scale_x_continuous(name = 'partner age category')+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = agecombo_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_agepartner_agecombo.png', device = 'png',
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
    xlab(label = 'stimulus type') + ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('move away directly',
                                      'move away at an angle',
                                      'side on',
                                      'approach at an angle',
                                      'approach directly'))+
    scale_x_discrete(breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = stim_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + agecombo_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(mom1_fit, ndraws = 100) # really good fit

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
                          "bsp_momove_tminus1_num",
                          # "simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]",
                          # "simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "simo_momove_tminus1_num1[1]","simo_momove_tminus1_num1[2]"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing doesn't move brilliant, esp. for bsp_mofocal_age, but only horrendous one is playback ID

## move at intercepts (estimates of cutpoints between categories on linear model scale)
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

## age interaction -- come back to this!

## movement direction in previous second
prevsec1 <- draws_cut %>% filter(parameter == 'bsp_momove_tminus1_num')
prevsec2 <- draws_cut %>% filter(parameter == 'simo_momove_tminus1_num1[1]')
prevsec3 <- draws_cut %>% filter(parameter == 'simo_momove_tminus1_num1[2]')
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

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
save.image('movement_direction/movement_ordinal_model1_run.RData')

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
ggplot(move_no_na, aes(x = f_age_num, y = move_index,
                       colour = age_difference))+
  geom_jitter(alpha = 0.1)+
  facet_wrap(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_y_discrete(name = 'focal movement direction relative to target',
                   breaks = c(1,2,3,4,5),
                   labels = c('move away directly',
                              'move away at an angle',
                              'side on',
                              'approach at an angle',
                              'approach directly'))+
  labs(colour = 'age difference')
print('plot complete')

## plot control data
move_no_na %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = after_stim, y = move_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started')
print('plot complete')

## plot lion data
move_no_na %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = after_stim, y = move_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started')
print('plot complete')

## plot human data
move_no_na %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = after_stim, y = move_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started')

print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('movement_direction/movement_ordinal_model1_run.RData') # save.image('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')
dev.off()

#### predict from model ####
pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelpredictions.pdf')
load('movement_direction/movement_ordinal_model1_run.RData')
rm(list = ls()[! ls() %in% c('mom1_fit','move_no_na')]) ; gc()

pred <- posterior_epred(object = mom1_fit,
                        newdata = move_no_na)
save.image('movement_direction/movement_ordinal_model1_predictions.RData')

## convert to data frame
extract_predictions <- function(prediction_array, layer, df){
  predictions <- as.data.frame(prediction_array[,,layer])
  colnames(predictions) <- 1:nrow(df)
  predictions <- predictions %>%
    pivot_longer(cols = everything(),
                 names_to = 'data_row', values_to = 'epred') %>%
    mutate(data_row = as.integer(data_row)) %>%
    left_join(df, by = 'data_row') %>%
    mutate(pred_type = ifelse(layer == 1, 'move directly away',
                              ifelse(layer == 2, 'move away at an angle',
                                     ifelse(layer == 3, 'move neither towards or away',
                                            ifelse(layer == 4, 'approach at an angle',
                                                   ifelse(layer == 5, 'approach directly',
                                                          'CHECK -- PROBLEM IN DATA'))))),
           pred_type_num = layer)
  return(predictions)
}

# pred1 <- as.data.frame(pred[,,1])
# colnames(pred1) <- 1:nrow(move_no_na)

move_no_na$data_row <- 1:nrow(move_no_na)
# pred1 <- pred1 %>%
#   pivot_longer(cols = everything(),
#                names_to = 'data_row', values_to = 'epred') %>%
#   left_join(move_no_na, by = 'data_row') %>%
#   mutate(pred_type = 'not_moving',
#          pred_type_num = 0)

pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = move_no_na)
pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = move_no_na)
pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = move_no_na)
pred4 <- extract_predictions(prediction_array = pred, layer = 4, df = move_no_na)
pred5 <- extract_predictions(prediction_array = pred, layer = 5, df = move_no_na)

pred <- rbind(pred1, pred2, pred3, pred4, pred5)
save.image('movement_direction/movement_ordinal_model1_predictions.RData')
rm(pred1, pred2, pred3, pred4, pred5) ; gc()

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions ####
load('movement_direction/movement_ordinal_model1_predictions.RData')

## make labels for movement in previous second
prevsec_labels <- c('direct away at t-1',
                    'angle away at t-1',
                    'neither at t-1',
                    'angle approach at t-1',
                    'direct approach at t-1')
names(prevsec_labels) <- 1:5

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
(ctd_plot <- pred %>%
    filter(stim_type == 'ctd',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    fill = factor(pred_type, levels = c('move directly away',
                                                        'move away at an angle',
                                                        'move neither towards or away',
                                                        'approach at an angle',
                                                        'approach directly')),
                    colour = factor(pred_type, levels = c('move directly away',
                                                          'move away at an angle',
                                                          'move neither towards or away',
                                                          'approach at an angle',
                                                          'approach directly'))
                    )) +
    facet_grid(move_tminus1_num ~ after_stim,
               labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'cape turtle dove (control)')+
    theme(legend.position = 'bottom'))
print('ctd_plot complete')

(lion_plot <- pred %>%
    filter(stim_type == 'l',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    fill = factor(pred_type, levels = c('move directly away',
                                                        'move away at an angle',
                                                        'move neither towards or away',
                                                        'approach at an angle',
                                                        'approach directly')),
                    colour = factor(pred_type, levels = c('move directly away',
                                                          'move away at an angle',
                                                          'move neither towards or away',
                                                          'approach at an angle',
                                                          'approach directly'))
                    )) +
    facet_grid(move_tminus1_num ~ after_stim,
               labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'lion')+
    theme(legend.position = 'bottom'))
print('lion_plot complete')

(human_plot <- pred %>%
    filter(stim_type == 'h',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    fill = factor(pred_type, levels = c('move directly away',
                                                        'move away at an angle',
                                                        'move neither towards or away',
                                                        'approach at an angle',
                                                        'approach directly')),
                    colour = factor(pred_type, levels = c('move directly away',
                                                          'move away at an angle',
                                                          'move neither towards or away',
                                                          'approach at an angle',
                                                          'approach directly'))
                    )) +
    facet_grid(move_tminus1_num ~ after_stim,
               labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'human')+
    theme(legend.position = 'bottom'))
print('human_plot complete')

(ctd_plot + lion_plot + human_plot) +
  plot_layout(guides = "collect") +# & theme(legend.position = 'bottom') +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(), file = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_predictions_violin.png',
       device = 'png', height = 32, width = 16)

## reset plotting
dev.off()
pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelcontrasts.pdf')

#### calculate posterior contrasts from predictions ####
rm(list = ls()) ; gc()
load('movement_direction/movement_ordinal_model1_predictions.RData')

## stim type ####
move_new <- move_no_na %>%
  dplyr::select(f_age_num, age_combo, stim_type, after_stim, move_tminus1_num,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))

## redo predictions with different stimulus types: all doves
ctd_move <- move_new %>%
  mutate(stim_type = 'ctd')
ctd_mtx <- posterior_epred(object = mom1_fit, newdata = ctd_move)
colnames(ctd_mtx) <- ctd_move$unique_data_combo
ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all lions
lion_move <- move_new %>%
  mutate(stim_type = 'l')
lion_mtx <- posterior_epred(object = mom1_fit, newdata = lion_move)
colnames(lion_mtx) <- lion_move$unique_data_combo
lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all humans
human_move <- move_new %>%
  mutate(stim_type = 'h')
human_mtx <- posterior_epred(object = mom1_fit, newdata = human_move)
colnames(human_mtx) <- human_move$unique_data_combo
human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('movement_direction/movement_ordinal_model1_stimuluscontrasts.RData')

## probability of each prediction
# load('movement_direction/movement_ordinal_model1_stimuluscontrasts.RData')
# count_values <- function(vector, levels = c(1,2,3)) {
#   x <- tabulate(factor(vector, levels), length(levels))
#   return(list(x))
# }
stim_pred <- ctd_move %>%
  dplyr::select(-stim_type) %>%
  # mutate(ctd_count = apply(ctd_mtx, 2, count_values),
  #        lion_count = apply(lion_mtx, 2, count_values),
  #        human_count = apply(human_mtx, 2, count_values)) %>%
  # unnest(c(ctd_count, lion_count, human_count)) %>% # I've done something weird with the count_values function so for now this needs unnesting twice, but should probably fix it at some point! For now this works!
  # unnest(c(ctd_count, lion_count, human_count)) %>%
  mutate(ctd_prop1_mu = apply(ctd_mtx[,,1], 2, mean),
         ctd_prop2_mu = apply(ctd_mtx[,,2], 2, mean),
         ctd_prop3_mu = apply(ctd_mtx[,,3], 2, mean),
         ctd_prop4_mu = apply(ctd_mtx[,,4], 2, mean),
         ctd_prop5_mu = apply(ctd_mtx[,,5], 2, mean),
         ctd_prop1_sd = apply(ctd_mtx[,,1], 2, sd),
         ctd_prop2_sd = apply(ctd_mtx[,,2], 2, sd),
         ctd_prop3_sd = apply(ctd_mtx[,,3], 2, sd),
         ctd_prop4_sd = apply(ctd_mtx[,,4], 2, sd),
         ctd_prop5_sd = apply(ctd_mtx[,,5], 2, sd),
         lion_prop1_mu = apply(lion_mtx[,,1], 2, mean),
         lion_prop2_mu = apply(lion_mtx[,,2], 2, mean),
         lion_prop3_mu = apply(lion_mtx[,,3], 2, mean),
         lion_prop4_mu = apply(lion_mtx[,,4], 2, mean),
         lion_prop5_mu = apply(lion_mtx[,,5], 2, mean),
         lion_prop1_sd = apply(lion_mtx[,,1], 2, sd),
         lion_prop2_sd = apply(lion_mtx[,,2], 2, sd),
         lion_prop3_sd = apply(lion_mtx[,,3], 2, sd),
         lion_prop4_sd = apply(lion_mtx[,,4], 2, sd),
         lion_prop5_sd = apply(lion_mtx[,,5], 2, sd),
         human_prop1_mu = apply(human_mtx[,,1], 2, mean),
         human_prop2_mu = apply(human_mtx[,,2], 2, mean),
         human_prop3_mu = apply(human_mtx[,,3], 2, mean),
         human_prop4_mu = apply(human_mtx[,,4], 2, mean),
         human_prop5_mu = apply(human_mtx[,,5], 2, mean),
         human_prop1_sd = apply(human_mtx[,,1], 2, sd),
         human_prop2_sd = apply(human_mtx[,,2], 2, sd),
         human_prop3_sd = apply(human_mtx[,,3], 2, sd),
         human_prop4_sd = apply(human_mtx[,,4], 2, sd),
         human_prop5_sd = apply(human_mtx[,,5], 2, sd)) %>%
  pivot_longer(cols = c(ctd_prop1_mu,ctd_prop2_mu,ctd_prop3_mu,ctd_prop4_mu,ctd_prop5_mu,
                        lion_prop1_mu,lion_prop2_mu,lion_prop3_mu,lion_prop4_mu,lion_prop5_mu,
                        human_prop1_mu,human_prop2_mu,human_prop3_mu,human_prop4_mu,human_prop5_mu),
               names_to = 'stim_proptype_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(ctd_prop1_sd,ctd_prop2_sd,ctd_prop3_sd,ctd_prop4_sd,ctd_prop5_sd,
                        lion_prop1_sd,lion_prop2_sd,lion_prop3_sd,lion_prop4_sd,lion_prop5_sd,
                        human_prop1_sd,human_prop2_sd,human_prop3_sd,human_prop4_sd,human_prop5_sd),
               names_to = 'stim_proptype_sd', values_to = 'stdv_propn') %>%
  separate(col = stim_proptype_mu, into = c('stim_proptype_mu','mu'),
           sep = '_m', remove = T) %>%
  separate(col = stim_proptype_sd, into = c('stim_proptype_sd','sd'),
           sep = '_s', remove = T) %>%
  filter(stim_proptype_mu == stim_proptype_sd) %>%
  select(-mu, -sd, -stim_proptype_sd) %>%
  separate(col = stim_proptype_mu, into = c('stim_type', 'move_pred'),
           sep = '_prop', remove = T) %>%
  mutate(move_pred = as.numeric(move_pred)) %>%
  mutate(pred_type = ifelse(move_pred == 1, 'directly away',
                            ifelse(move_pred == 2, 'away angle',
                                   ifelse(move_pred == 3, 'neither',
                                          ifelse(move_pred == 4, 'approach angle',
                                                 'directly approach')))))

## calculate contrasts
ctd_vs_lion_ad <- lion_mtx[,,1] - ctd_mtx[,,1]
ctd_vs_lion_aa <- lion_mtx[,,2] - ctd_mtx[,,2]
ctd_vs_lion_n  <- lion_mtx[,,3] - ctd_mtx[,,3]
ctd_vs_lion_ta <- lion_mtx[,,4] - ctd_mtx[,,4]
ctd_vs_lion_td <- lion_mtx[,,5] - ctd_mtx[,,5]

ctd_vs_human_ad <- human_mtx[,,1] - ctd_mtx[,,1]
ctd_vs_human_aa <- human_mtx[,,2] - ctd_mtx[,,2]
ctd_vs_human_n  <- human_mtx[,,3] - ctd_mtx[,,3]
ctd_vs_human_ta <- human_mtx[,,4] - ctd_mtx[,,4]
ctd_vs_human_td <- human_mtx[,,5] - ctd_mtx[,,5]

lion_vs_human_ad <- human_mtx[,,1] - lion_mtx[,,1]
lion_vs_human_aa <- human_mtx[,,2] - lion_mtx[,,2]
lion_vs_human_n  <- human_mtx[,,3] - lion_mtx[,,3]
lion_vs_human_ta <- human_mtx[,,4] - lion_mtx[,,4]
lion_vs_human_td <- human_mtx[,,5] - lion_mtx[,,5]

## summarise contrasts
contrasts <- move_no_na %>%
  select(-stim_type) %>%
  mutate(ctd_vs_lion_ad_mu = apply(ctd_vs_lion_ad, 2, mean),
         ctd_vs_lion_ad_sd = apply(ctd_vs_lion_ad, 2, sd),
         ctd_vs_lion_aa_mu = apply(ctd_vs_lion_aa, 2, mean),
         ctd_vs_lion_aa_sd = apply(ctd_vs_lion_aa, 2, sd),
         ctd_vs_lion_n_mu  = apply(ctd_vs_lion_n, 2, mean),
         ctd_vs_lion_n_sd  = apply(ctd_vs_lion_n, 2, sd),
         ctd_vs_lion_ta_mu = apply(ctd_vs_lion_ta, 2, mean),
         ctd_vs_lion_ta_sd = apply(ctd_vs_lion_ta, 2, sd),
         ctd_vs_lion_td_mu = apply(ctd_vs_lion_td, 2, mean),
         ctd_vs_lion_td_sd = apply(ctd_vs_lion_td, 2, sd),

         ctd_vs_human_ad_mu = apply(ctd_vs_human_ad, 2, mean),
         ctd_vs_human_ad_sd = apply(ctd_vs_human_ad, 2, sd),
         ctd_vs_human_aa_mu = apply(ctd_vs_human_aa, 2, mean),
         ctd_vs_human_aa_sd = apply(ctd_vs_human_aa, 2, sd),
         ctd_vs_human_n_mu  = apply(ctd_vs_human_n, 2, mean),
         ctd_vs_human_n_sd  = apply(ctd_vs_human_n, 2, sd),
         ctd_vs_human_ta_mu = apply(ctd_vs_human_ta, 2, mean),
         ctd_vs_human_ta_sd = apply(ctd_vs_human_ta, 2, sd),
         ctd_vs_human_td_mu = apply(ctd_vs_human_td, 2, mean),
         ctd_vs_human_td_sd = apply(ctd_vs_human_td, 2, sd),

         lion_vs_human_ad_mu = apply(lion_vs_human_ad, 2, mean),
         lion_vs_human_ad_sd = apply(lion_vs_human_ad, 2, sd),
         lion_vs_human_aa_mu = apply(lion_vs_human_aa, 2, mean),
         lion_vs_human_aa_sd = apply(lion_vs_human_aa, 2, sd),
         lion_vs_human_n_mu  = apply(lion_vs_human_n, 2, mean),
         lion_vs_human_n_sd  = apply(lion_vs_human_n, 2, sd),
         lion_vs_human_ta_mu = apply(lion_vs_human_ta, 2, mean),
         lion_vs_human_ta_sd = apply(lion_vs_human_ta, 2, sd),
         lion_vs_human_td_mu = apply(lion_vs_human_td, 2, mean),
         lion_vs_human_td_sd = apply(lion_vs_human_td, 2, sd)
  )
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(ctd_vs_lion_ad_mu, ctd_vs_lion_aa_mu,
                        ctd_vs_lion_n_mu,
                        ctd_vs_lion_td_mu, ctd_vs_lion_ta_mu,
                        ctd_vs_human_ad_mu, ctd_vs_human_aa_mu,
                        ctd_vs_human_n_mu,
                        ctd_vs_human_td_mu, ctd_vs_human_ta_mu,
                        lion_vs_human_ad_mu, lion_vs_human_aa_mu,
                        lion_vs_human_n_mu,
                        lion_vs_human_td_mu, lion_vs_human_ta_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('contrast','move_pred'),
           sep = '_age', remove = T) %>%
  separate(move_pred, into = c('move_pred','mu'),
           sep = '_', remove = T) %>%
  mutate(move_pred = as.numeric(move_pred)) %>%
  mutate(pred_type = ifelse(move_pred == 1, 'directly away',
                            ifelse(move_pred == 2, 'away at an angle',
                                   ifelse(move_pred == 3, 'neither towards or away',
                                          ifelse(move_pred == 4, 'approach at an angle',
                                                 'directly approach'))))) %>%
  select(-mu, -ctd_vs_lion_ad_sd, -ctd_vs_lion_aa_sd, -ctd_vs_lion_n_sd, -ctd_vs_lion_ta_sd, -ctd_vs_lion_td_sd,
         -ctd_vs_human_ad_sd, -ctd_vs_human_aa_sd, -ctd_vs_human_n_sd, -ctd_vs_human_ta_sd, -ctd_vs_human_td_sd,
         -lion_vs_human_ad_sd, -lion_vs_human_aa_sd, -lion_vs_human_n_sd, -lion_vs_human_ta_sd, -lion_vs_human_td_sd)

## plot contrasts
# stim_pred %>%
#   dplyr::select(ctd_lion, ctd_human, lion_human, pred_type) %>%
#   pivot_longer(cols = c('ctd_lion', 'ctd_human', 'lion_human'),
#                names_to = 'contrast') %>%
#   ggplot()+
#   geom_density(aes(x = value, colour = contrast))+
#   facet_wrap(. ~ pred_type)

stim_pred %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = pred_type))+
  facet_wrap(. ~ stim_type)
contrasts_long %>%
  ggplot()+
  geom_density(aes(x = difference))+
  facet_grid(pred_type ~ contrast)

save.image('movement_direction/movement_ordinal_model1_stimuluscontrasts.RData')

## focal age ####
# load('movement_direction/movement_ordinal_model1_stimuluscontrasts.RData')
rm(list = ls()[!ls() %in% c('mom1_fit','move_no_na')]) ; gc()

## create new dataframe to predict from
age_new <- move_no_na %>%
  dplyr::select(f_age_num, p_age_num, age_combo, stim_type, move_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))

## predict with original ages
age_move_org <- age_new
age_mtx_org <- posterior_epred(object = mom1_fit, newdata = age_move_org)
colnames(age_mtx_org) <- age_move_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with altered ages
age_move_alt <- age_new %>%
  mutate(f_age_num_original = f_age_num) %>%
  mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
  mutate(age_combo_original = age_combo) %>%
  mutate(age_combo = paste0(f_age_num,'_',p_age_num)) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%  # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(f_age_num_original)
age_mtx_alt <- posterior_epred(object = mom1_fit, newdata = age_move_alt)
colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
save.image('movement_direction/movement_ordinal_model1_agecontrasts.RData')

## summarise and convert to long format
# rm(list = ls()) ; gc() ; load('movement_direction/movement_ordinal_model1_agecontrasts.RData')
age_pred <- age_move_org %>%
  #dplyr::select(-f_age_num) %>%
  mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
         age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
         age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
         age_org_prop4_mu = apply(age_mtx_org[,,4], 2, mean),
         age_org_prop5_mu = apply(age_mtx_org[,,5], 2, mean),
         age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
         age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
         age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
         age_org_prop4_sd = apply(age_mtx_org[,,4], 2, sd),
         age_org_prop5_sd = apply(age_mtx_org[,,5], 2, sd),
         age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
         age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
         age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
         age_alt_prop4_mu = apply(age_mtx_alt[,,4], 2, mean),
         age_alt_prop5_mu = apply(age_mtx_alt[,,5], 2, mean),
         age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
         age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
         age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd),
         age_alt_prop4_sd = apply(age_mtx_alt[,,4], 2, sd),
         age_alt_prop5_sd = apply(age_mtx_alt[,,5], 2, sd)) %>%
  pivot_longer(cols = c(age_org_prop1_mu, age_org_prop2_mu, age_org_prop3_mu,
                        age_org_prop4_mu, age_org_prop5_mu,
                        age_alt_prop1_mu, age_alt_prop2_mu, age_alt_prop3_mu,
                        age_alt_prop4_mu, age_alt_prop5_mu),
               names_to = 'focal_agemove_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(age_org_prop1_sd, age_org_prop2_sd, age_org_prop3_sd,
                        age_org_prop4_sd, age_org_prop5_sd,
                        age_alt_prop1_sd, age_alt_prop2_sd, age_alt_prop3_sd,
                        age_alt_prop4_sd, age_alt_prop5_sd),
               names_to = 'focal_agemove_sd', values_to = 'stdv_propn') %>%
  separate(col = focal_agemove_mu, into = c('focal_agemove_mu','mu'),
           sep = '_m', remove = T) %>%
  separate(col = focal_agemove_sd, into = c('focal_agemove_sd','sd'),
           sep = '_s', remove = T) %>%
  select(-mu, -sd) %>%
  filter(focal_agemove_mu == focal_agemove_sd) %>%
  separate(col = focal_agemove_mu, into = c('original_altered', 'move_pred'),
           sep = '_prop', remove = T) %>%
  select(-focal_agemove_sd) %>%
  mutate(move_pred = as.numeric(move_pred),
         f_age_num = ifelse(original_altered == 'age_org',
                            f_age_num,
                            ifelse(original_altered == 'age_alt' & f_age_num == 4,
                                   1, f_age_num + 1))) %>%
  mutate(pred_type = ifelse(move_pred == 1, 'move away directly',
                            ifelse(move_pred == 2, 'move away at an angle',
                                   ifelse(move_pred == 3, 'neither approach or retreat',
                                          ifelse(move_pred == 4, 'approach at an angle',
                                                 'approach directly')))))

## calculate contrasts
alt_vs_org_awaydirect <- age_mtx_alt[,,1] - age_mtx_org[,,1]
alt_vs_org_awayangle  <- age_mtx_alt[,,2] - age_mtx_org[,,2]
alt_vs_org_neither    <- age_mtx_alt[,,3] - age_mtx_org[,,3]
alt_vs_org_twdsangle  <- age_mtx_alt[,,4] - age_mtx_org[,,4]
alt_vs_org_twdsdirect <- age_mtx_alt[,,5] - age_mtx_org[,,5]

## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
mean(alt_vs_org_awaydirect); sd(alt_vs_org_awaydirect)
#        -0.0002071437     ±     0.005378194
mean(alt_vs_org_awayangle) ; sd(alt_vs_org_awayangle)
#         0.0003584539     ±     0.008211262
mean(alt_vs_org_neither)   ; sd(alt_vs_org_neither)
#          5.19391e-05     ±     0.006590041
mean(alt_vs_org_twdsangle) ; sd(alt_vs_org_twdsangle)
#        -5.684145e-05     ±      0.00796264
mean(alt_vs_org_twdsdirect); sd(alt_vs_org_twdsdirect)
#        -0.0001464078     ±     0.006000429

## repeat excluding age category 4 because different contrast
mean(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     # 0.0001407432
sd(  alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     # 0.005066347
mean(alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])      # 0.0003296887
sd(  alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])      # 0.008527676
mean(alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])        # -6.212857e-06
sd(  alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])        # 0.006209619
mean(alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])      # 0.000144165
sd(  alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])      # 0.008230058
mean(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # -0.0006083841
sd(  alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # 0.005444133

## split contrasts by original age category
age1v2_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 1)]
age2v3_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 2)]
age3v4_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 3)]
age1v4_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 4)] * (-1)
age1v2_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 1)]
age2v3_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 2)]
age3v4_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 3)]
age1v4_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 4)] * (-1)
age1v2_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 1)]
age2v3_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 2)]
age3v4_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 3)]
age1v4_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 4)] * (-1)
age1v2_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 1)]
age2v3_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 2)]
age3v4_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 3)]
age1v4_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 4)] * (-1)
age1v2_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 1)]
age2v3_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 2)]
age3v4_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 3)]
age1v4_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 4)] * (-1)

## calculate contrast values
mean(age1v2_ad) ; sd(age1v2_ad) #   0.001304186 ± 0.004696869
mean(age2v3_ad) ; sd(age2v3_ad) #   0.002283829 ± 0.004453934
mean(age3v4_ad) ; sd(age3v4_ad) #  -0.002026502 ± 0.004746689
mean(age1v4_ad) ; sd(age1v4_ad) #   0.002155958 ± 0.006534138
mean(age1v2_aa) ; sd(age1v2_aa) #  0.0004473055 ± 0.008646302
mean(age2v3_aa) ; sd(age2v3_aa) #  0.0004431893 ± 0.007971412
mean(age3v4_aa) ; sd(age3v4_aa) #  0.0002039822 ± 0.008977904
mean(age1v4_aa) ; sd(age1v4_aa) # -0.0005195924 ± 0.006142011
mean(age1v2_n)  ; sd(age1v2_n)  # -0.0004030411 ± 0.006850727
mean(age2v3_n)  ; sd(age2v3_n)  #  0.0001901263 ± 0.006156954
mean(age3v4_n)  ; sd(age3v4_n)  # -0.0001065396 ± 0.006118789
mean(age1v4_n)  ; sd(age1v4_n)  # -0.0003776982 ± 0.008401226
mean(age1v2_ta) ; sd(age1v2_ta) #   8.82818e-05 ± 0.009274127
mean(age2v3_ta) ; sd(age2v3_ta) # -4.280603e-05 ± 0.007600419
mean(age3v4_ta) ; sd(age3v4_ta) #  0.0003243425 ± 0.008551451
mean(age1v4_ta) ; sd(age1v4_ta) #   0.001182852 ± 0.006136129
mean(age1v2_td) ; sd(age1v2_td) #  -0.001436732 ± 0.006333778
mean(age2v3_td) ; sd(age2v3_td) #  -0.002874339 ± 0.004793162
mean(age3v4_td) ; sd(age3v4_td) #   0.001604717 ± 0.004893596
mean(age1v4_td) ; sd(age1v4_td) #  -0.002441519 ± 0.007986037

## summarise contrasts
contrasts <- move_no_na %>%
  mutate(alt_vs_org_awaydirect_mu = apply(alt_vs_org_awaydirect, 2, mean),
         alt_vs_org_awaydirect_sd = apply(alt_vs_org_awaydirect, 2, sd),
         alt_vs_org_awayangle_mu = apply(alt_vs_org_awayangle, 2, mean),
         alt_vs_org_awayangle_sd = apply(alt_vs_org_awayangle, 2, sd),
         alt_vs_org_neither_mu = apply(alt_vs_org_neither, 2, mean),
         alt_vs_org_neither_sd = apply(alt_vs_org_neither, 2, sd),
         alt_vs_org_twdsangle_mu = apply(alt_vs_org_twdsangle, 2, mean),
         alt_vs_org_twdsangle_sd = apply(alt_vs_org_twdsangle, 2, sd),
         alt_vs_org_twdsdirect_mu = apply(alt_vs_org_twdsdirect, 2, mean),
         alt_vs_org_twdsdirect_sd = apply(alt_vs_org_twdsdirect, 2, sd)
         # age1v2_ad_mu = apply(age1v2_ad, 2, mean), age1v2_ad_sd = apply(age1v2_ad, 2, sd),
         # age2v3_ad_mu = apply(age2v3_ad, 2, mean), age2v3_ad_sd = apply(age2v3_ad, 2, sd),
         # age3v4_ad_mu = apply(age3v4_ad, 2, mean), age3v4_ad_sd = apply(age3v4_ad, 2, sd),
         # age1v4_ad_mu = apply(age1v4_ad, 2, mean), age1v4_ad_sd = apply(age1v4_ad, 2, sd),
         # age1v2_aa_mu = apply(age1v2_aa, 2, mean), age1v2_aa_sd = apply(age1v2_aa, 2, sd),
         # age2v3_aa_mu = apply(age2v3_aa, 2, mean), age2v3_aa_sd = apply(age2v3_aa, 2, sd),
         # age3v4_aa_mu = apply(age3v4_aa, 2, mean), age3v4_aa_sd = apply(age3v4_aa, 2, sd),
         # age1v4_aa_mu = apply(age1v4_aa, 2, mean), age1v4_aa_sd = apply(age1v4_aa, 2, sd),
         # age1v2_n_mu = apply(age1v2_n, 2, mean), age1v2_n_sd = apply(age1v2_n, 2, sd),
         # age2v3_n_mu = apply(age2v3_n, 2, mean), age2v3_n_sd = apply(age2v3_n, 2, sd),
         # age3v4_n_mu = apply(age3v4_n, 2, mean), age3v4_n_sd = apply(age3v4_n, 2, sd),
         # age1v4_n_mu = apply(age1v4_n, 2, mean), age1v4_n_sd = apply(age1v4_n, 2, sd),
         # age1v2_ta_mu = apply(age1v2_ta, 2, mean), age1v2_ta_sd = apply(age1v2_ta, 2, sd),
         # age2v3_ta_mu = apply(age2v3_ta, 2, mean), age2v3_ta_sd = apply(age2v3_ta, 2, sd),
         # age3v4_ta_mu = apply(age3v4_ta, 2, mean), age3v4_ta_sd = apply(age3v4_ta, 2, sd),
         # age1v4_ta_mu = apply(age1v4_ta, 2, mean), age1v4_ta_sd = apply(age1v4_ta, 2, sd),
         # age1v2_td_mu = apply(age1v2_td, 2, mean), age1v2_td_sd = apply(age1v2_td, 2, sd),
         # age2v3_td_mu = apply(age2v3_td, 2, mean), age2v3_td_sd = apply(age2v3_td, 2, sd),
         # age3v4_td_mu = apply(age3v4_td, 2, mean), age3v4_td_sd = apply(age3v4_td, 2, sd),
         # age1v4_td_mu = apply(age1v4_td, 2, mean), age1v4_td_sd = apply(age1v4_td, 2, sd)
         ) %>%
  mutate(categories_different = ifelse(f_age_num == 4,
                                       '3 categories different',
                                       '1 category different'))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(alt_vs_org_awaydirect_mu, alt_vs_org_awayangle_mu,
                        alt_vs_org_neither_mu, alt_vs_org_twdsangle_mu,
                        alt_vs_org_twdsdirect_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','vs','org','move_pred','mu'),
           sep = '_', remove = T) %>%
  select(-alt_vs_org_awaydirect_sd, -alt_vs_org_awayangle_sd, -alt_vs_org_neither_sd,
         -alt_vs_org_twdsangle_sd, -alt_vs_org_twdsdirect_sd, -alt, -vs, -org, -mu)

## plot contrasts
# age_pred %>%
#   dplyr::select(ctd_lion, ctd_human, lion_human, pred_type) %>%
#   pivot_longer(cols = c('ctd_lion', 'ctd_human', 'lion_human'),
#                names_to = 'contrast') %>%
#   ggplot()+
#   geom_density(aes(x = value, colour = contrast))+
#   facet_wrap(. ~ pred_type)

age_pred %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('move away directly',
                                       'move away at an angle',
                                       'neither approach or retreat',
                                       'approach at an angle',
                                       'approach directly'))) %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = pred_type, fill = pred_type),
               alpha = 0.5)+
  facet_grid(pred_type ~ stim_type)
contrasts_long %>%
  mutate(pred_type = ifelse(move_pred == 'awaydirect',
                            'move away directly',
                            ifelse(move_pred == 'awayangle',
                                   'move away at an angle',
                                   ifelse(move_pred == 'neither',
                                          'neither approach or retreat',
                                          ifelse(move_pred == 'twdsangle',
                                                 'approach at an angle',
                                                 'approach directly'))))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('move away directly',
                                       'move away at an angle',
                                       'neither approach or retreat',
                                       'approach at an angle',
                                       'approach directly'))) %>%
  mutate(f_age_new = ifelse(f_age_num == 4,
                            'youngest to oldest',
                            paste0('category ',f_age_num,' to ',f_age_num+1))) %>%
  mutate(difference = ifelse(f_age_num == 4,
                             difference * (-1),
                             difference)) %>%
  ggplot()+
  geom_density(aes(x = difference,
                   colour = categories_different,
                   fill = categories_different),
               alpha = 0.5)+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_grid(pred_type ~ f_age_new, scales = 'free_y')+
  scale_colour_viridis_d(begin = 0.5, end = 0)+
  scale_fill_viridis_d(begin = 0.5, end = 0)+
  labs(colour = 'categories\ndifferent',
       fill = 'categories\ndifferent')
save.image('movement_direction/movement_ordinal_model1_agecontrasts.RData')

## clean up a bit
rm(list = ls()[! ls() %in% c('alt_vs_org_awaydirect','alt_vs_org_awayangle',
                             'alt_vs_org_neither','alt_vs_org_twdsangle',
                             'alt_vs_org_twdsdirect','move_no_na','mom1_fit')]) ; gc()

## plot full density instead of means
colnames(alt_vs_org_awaydirect) <- move_no_na$data_row
colnames(alt_vs_org_awayangle)  <- move_no_na$data_row
colnames(alt_vs_org_neither)    <- move_no_na$data_row
colnames(alt_vs_org_twdsangle)  <- move_no_na$data_row
colnames(alt_vs_org_twdsdirect) <- move_no_na$data_row

mtx_to_df <- function(mtx, pred_type){
  df <- mtx %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(),
                 names_to = 'data_row',
                 values_to = 'contrast') %>%
    mutate(data_row = as.integer(data_row)) %>%
    left_join(move_no_na, by = 'data_row') %>%
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
ad <- mtx_to_df(alt_vs_org_awaydirect, pred_type = 'move directly away')
aa <- mtx_to_df(alt_vs_org_awayangle, pred_type = 'move away at an angle')
n <- mtx_to_df(alt_vs_org_neither, pred_type = 'neither approach or retreat')
ta <- mtx_to_df(alt_vs_org_twdsangle, pred_type = 'approach at an angle')
td <- mtx_to_df(alt_vs_org_twdsdirect, pred_type = 'approach directly')

plot_contrasts <- rbind(aa, ad, n, ta, td) %>%
  mutate(prediction_type = factor(prediction_type,
                                  levels = c('move directly away',
                                             'move away at an angle',
                                             'neither approach or retreat',
                                             'approach at an angle',
                                             'approach directly')))

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
       filename = 'movement_ordinal1_agecontrasts.png',
       path = '../outputs/movement_ordinal_model_1/',
       width = 2400, height = 3200, unit = 'px')
ggsave(plot = last_plot(), device = 'svg',
       filename = 'movement_ordinal1_agecontrasts.svg',
       path = '../outputs/movement_ordinal_model_1/',
       width = 2400, height = 3200, unit = 'px')

## time since stimulus ####
load('movement_direction/movement_ordinal_model1_agecontrasts.RData')
rm(list = ls()[!ls() %in% c('mom1_fit','move_no_na')]) ; gc()

## create new data frame to calculate from
time_new <- move_no_na %>%
  dplyr::select(f_age_num, age_combo, stim_type, move_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, stim_type, move_tminus1_num, after_stim, focal_id, stim_id, playback_id))))

## predict with original times
time_move_org <- time_new
time_mtx_org <- posterior_epred(object = mom1_fit, newdata = time_move_org)
colnames(time_mtx_org) <- time_move_org$unique_data_combo
time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +15 seconds
time_move_alt_0.25 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1/4) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%  # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.25 <- posterior_epred(object = mom1_fit, newdata = time_move_alt_0.25)
colnames(time_mtx_alt_0.25) <- time_move_alt_0.25$unique_data_combo
time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +30 seconds
time_move_alt_0.50 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1/2) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%  # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.50 <- posterior_epred(object = mom1_fit, newdata = time_move_alt_0.50)
colnames(time_mtx_alt_0.50) <- time_move_alt_0.50$unique_data_combo
time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +45 seconds
time_move_alt_0.75 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 3/4) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%  # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_0.75 <- posterior_epred(object = mom1_fit, newdata = time_move_alt_0.75)
colnames(time_mtx_alt_0.75) <- time_move_alt_0.75$unique_data_combo
time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +60 seconds
time_move_alt_1.00 <- time_new %>%
  mutate(after_stim_org = after_stim) %>%
  mutate(after_stim = after_stim + 1) %>%
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%  # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(after_stim_org)
time_mtx_alt_1.00 <- posterior_epred(object = mom1_fit, newdata = time_move_alt_1.00)
colnames(time_mtx_alt_1.00) <- time_move_alt_1.00$unique_data_combo
time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('movement_direction/movement_ordinal_model1_timecontrasts.RData')

## summarise and convert to long format
rm(list = ls()) ; gc() ; load('movement_direction/movement_ordinal_model1_timecontrasts.RData')
time_pred <- time_move_org %>%
  mutate(time_org_0.00_prop1_mu = apply(time_mtx_org[,,1], 2, mean),
         time_org_0.00_prop2_mu = apply(time_mtx_org[,,2], 2, mean),
         time_org_0.00_prop3_mu = apply(time_mtx_org[,,3], 2, mean),
         time_org_0.00_prop4_mu = apply(time_mtx_org[,,4], 2, mean),
         time_org_0.00_prop5_mu = apply(time_mtx_org[,,5], 2, mean),
         time_org_0.00_prop1_sd = apply(time_mtx_org[,,1], 2, sd),
         time_org_0.00_prop2_sd = apply(time_mtx_org[,,2], 2, sd),
         time_org_0.00_prop3_sd = apply(time_mtx_org[,,3], 2, sd),
         time_org_0.00_prop4_sd = apply(time_mtx_org[,,4], 2, sd),
         time_org_0.00_prop5_sd = apply(time_mtx_org[,,5], 2, sd),
         time_alt_0.25_prop1_mu = apply(time_mtx_alt_0.25[,,1], 2, mean),
         time_alt_0.25_prop2_mu = apply(time_mtx_alt_0.25[,,2], 2, mean),
         time_alt_0.25_prop3_mu = apply(time_mtx_alt_0.25[,,3], 2, mean),
         time_alt_0.25_prop4_mu = apply(time_mtx_alt_0.25[,,4], 2, mean),
         time_alt_0.25_prop5_mu = apply(time_mtx_alt_0.25[,,5], 2, mean),
         time_alt_0.25_prop1_sd = apply(time_mtx_alt_0.25[,,1], 2, sd),
         time_alt_0.25_prop2_sd = apply(time_mtx_alt_0.25[,,2], 2, sd),
         time_alt_0.25_prop3_sd = apply(time_mtx_alt_0.25[,,3], 2, sd),
         time_alt_0.25_prop4_sd = apply(time_mtx_alt_0.25[,,4], 2, sd),
         time_alt_0.25_prop5_sd = apply(time_mtx_alt_0.25[,,5], 2, sd),

         time_alt_0.50_prop1_mu = apply(time_mtx_alt_0.50[,,1], 2, mean),
         time_alt_0.50_prop2_mu = apply(time_mtx_alt_0.50[,,2], 2, mean),
         time_alt_0.50_prop3_mu = apply(time_mtx_alt_0.50[,,3], 2, mean),
         time_alt_0.50_prop4_mu = apply(time_mtx_alt_0.50[,,4], 2, mean),
         time_alt_0.50_prop5_mu = apply(time_mtx_alt_0.50[,,5], 2, mean),
         time_alt_0.50_prop1_sd = apply(time_mtx_alt_0.50[,,1], 2, sd),
         time_alt_0.50_prop2_sd = apply(time_mtx_alt_0.50[,,2], 2, sd),
         time_alt_0.50_prop3_sd = apply(time_mtx_alt_0.50[,,3], 2, sd),
         time_alt_0.50_prop4_sd = apply(time_mtx_alt_0.50[,,4], 2, sd),
         time_alt_0.50_prop5_sd = apply(time_mtx_alt_0.50[,,5], 2, sd),
         time_alt_0.75_prop1_mu = apply(time_mtx_alt_0.75[,,1], 2, mean),
         time_alt_0.75_prop2_mu = apply(time_mtx_alt_0.75[,,2], 2, mean),
         time_alt_0.75_prop3_mu = apply(time_mtx_alt_0.75[,,3], 2, mean),
         time_alt_0.75_prop4_mu = apply(time_mtx_alt_0.75[,,4], 2, mean),
         time_alt_0.75_prop5_mu = apply(time_mtx_alt_0.75[,,5], 2, mean),
         time_alt_0.75_prop1_sd = apply(time_mtx_alt_0.75[,,1], 2, sd),
         time_alt_0.75_prop2_sd = apply(time_mtx_alt_0.75[,,2], 2, sd),
         time_alt_0.75_prop3_sd = apply(time_mtx_alt_0.75[,,3], 2, sd),
         time_alt_0.75_prop4_sd = apply(time_mtx_alt_0.75[,,4], 2, sd),
         time_alt_0.75_prop5_sd = apply(time_mtx_alt_0.75[,,5], 2, sd),
         time_alt_1.00_prop1_mu = apply(time_mtx_alt_1.00[,,1], 2, mean),
         time_alt_1.00_prop2_mu = apply(time_mtx_alt_1.00[,,2], 2, mean),
         time_alt_1.00_prop3_mu = apply(time_mtx_alt_1.00[,,3], 2, mean),
         time_alt_1.00_prop4_mu = apply(time_mtx_alt_1.00[,,4], 2, mean),
         time_alt_1.00_prop5_mu = apply(time_mtx_alt_1.00[,,5], 2, mean),
         time_alt_1.00_prop1_sd = apply(time_mtx_alt_1.00[,,1], 2, sd),
         time_alt_1.00_prop2_sd = apply(time_mtx_alt_1.00[,,2], 2, sd),
         time_alt_1.00_prop3_sd = apply(time_mtx_alt_1.00[,,3], 2, sd),
         time_alt_1.00_prop4_sd = apply(time_mtx_alt_1.00[,,4], 2, sd),
         time_alt_1.00_prop5_sd = apply(time_mtx_alt_1.00[,,5], 2, sd)) %>%
  pivot_longer(cols = c(time_org_0.00_prop1_mu,time_org_0.00_prop2_mu,time_org_0.00_prop3_mu,
                        time_org_0.00_prop4_mu,time_org_0.00_prop5_mu,
                        time_alt_0.25_prop1_mu,time_alt_0.25_prop2_mu,time_alt_0.25_prop3_mu,
                        time_alt_0.25_prop4_mu,time_alt_0.25_prop5_mu,
                        time_alt_0.50_prop1_mu,time_alt_0.50_prop2_mu,time_alt_0.50_prop3_mu,
                        time_alt_0.50_prop4_mu,time_alt_0.50_prop5_mu,
                        time_alt_0.75_prop1_mu,time_alt_0.75_prop2_mu,time_alt_0.75_prop3_mu,
                        time_alt_0.75_prop4_mu,time_alt_0.75_prop5_mu,
                        time_alt_1.00_prop1_mu,time_alt_1.00_prop2_mu,time_alt_1.00_prop3_mu,
                        time_alt_1.00_prop4_mu,time_alt_1.00_prop5_mu),
               names_to = 'time_org_alt_prop_agemove_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(time_org_0.00_prop1_sd,time_org_0.00_prop2_sd,time_org_0.00_prop3_sd,
                        time_org_0.00_prop4_sd,time_org_0.00_prop5_sd,
                        time_alt_0.25_prop1_sd,time_alt_0.25_prop2_sd,time_alt_0.25_prop3_sd,
                        time_alt_0.25_prop4_sd,time_alt_0.25_prop5_sd,
                        time_alt_0.50_prop1_sd,time_alt_0.50_prop2_sd,time_alt_0.50_prop3_sd,
                        time_alt_0.50_prop4_sd,time_alt_0.50_prop5_sd,
                        time_alt_0.75_prop1_sd,time_alt_0.75_prop2_sd,time_alt_0.75_prop3_sd,
                        time_alt_0.75_prop4_sd,time_alt_0.75_prop5_sd,
                        time_alt_1.00_prop1_sd,time_alt_1.00_prop2_sd,time_alt_1.00_prop3_sd,
                        time_alt_1.00_prop4_sd,time_alt_1.00_prop5_sd),
               names_to = 'time_org_alt_prop_agemove_sd', values_to = 'stdv_propn') %>%
  separate(col = time_org_alt_prop_agemove_mu,
           into = c('time_mu','org_mu','alt_mu','prop_agemove_mu','mu'),
           sep = '_', remove = T) %>%
  separate(col = time_org_alt_prop_agemove_sd,
           into = c('time_sd','org_sd','alt_sd','prop_agemove_sd','sd'),
           sep = '_', remove = T) %>%
  select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>%
  filter(alt_mu == alt_sd & prop_agemove_sd == prop_agemove_mu) %>%
  mutate(move_pred = ifelse(prop_agemove_mu == 'prop1', 1,
                          ifelse(prop_agemove_mu == 'prop2', 2,
                                 ifelse(prop_agemove_mu == 'prop3', 3,
                                        ifelse(prop_agemove_mu == 'prop4', 4, 5))))) %>%
  select(-alt_sd, -prop_agemove_mu, -prop_agemove_sd) %>%
  rename(mins_added = alt_mu) %>%
  mutate(pred_type = ifelse(move_pred == 1, 'directly away',
                            ifelse(move_pred == 2, 'away at an angle',
                                   ifelse(move_pred == 3, 'neither towards or away',
                                          ifelse(move_pred == 4, 'approach at an angle',
                                                 'approach directly')))))

## calculate contrasts
alt0.25_vs_0.00_da <- time_mtx_alt_0.25[,,1] - time_mtx_org[,,1]
alt0.25_vs_0.00_aa <- time_mtx_alt_0.25[,,2] - time_mtx_org[,,2]
alt0.25_vs_0.00_n  <- time_mtx_alt_0.25[,,3] - time_mtx_org[,,3]
alt0.25_vs_0.00_at <- time_mtx_alt_0.25[,,4] - time_mtx_org[,,4]
alt0.25_vs_0.00_dt <- time_mtx_alt_0.25[,,5] - time_mtx_org[,,5]

alt0.50_vs_0.25_da <- time_mtx_alt_0.50[,,1] - time_mtx_alt_0.25[,,1]
alt0.50_vs_0.25_aa <- time_mtx_alt_0.50[,,2] - time_mtx_alt_0.25[,,2]
alt0.50_vs_0.25_n  <- time_mtx_alt_0.50[,,3] - time_mtx_alt_0.25[,,3]
alt0.50_vs_0.25_at <- time_mtx_alt_0.50[,,4] - time_mtx_alt_0.25[,,4]
alt0.50_vs_0.25_dt <- time_mtx_alt_0.50[,,5] - time_mtx_alt_0.25[,,5]

alt0.75_vs_0.50_da <- time_mtx_alt_0.75[,,1] - time_mtx_alt_0.50[,,1]
alt0.75_vs_0.50_aa <- time_mtx_alt_0.75[,,2] - time_mtx_alt_0.50[,,2]
alt0.75_vs_0.50_n  <- time_mtx_alt_0.75[,,3] - time_mtx_alt_0.50[,,3]
alt0.75_vs_0.50_at <- time_mtx_alt_0.75[,,4] - time_mtx_alt_0.50[,,4]
alt0.75_vs_0.50_dt <- time_mtx_alt_0.75[,,5] - time_mtx_alt_0.50[,,5]

alt1.00_vs_0.75_da <- time_mtx_alt_1.00[,,1] - time_mtx_alt_0.75[,,1]
alt1.00_vs_0.75_aa <- time_mtx_alt_1.00[,,2] - time_mtx_alt_0.75[,,2]
alt1.00_vs_0.75_n  <- time_mtx_alt_1.00[,,3] - time_mtx_alt_0.75[,,3]
alt1.00_vs_0.75_at <- time_mtx_alt_1.00[,,4] - time_mtx_alt_0.75[,,4]
alt1.00_vs_0.75_dt <- time_mtx_alt_1.00[,,5] - time_mtx_alt_0.75[,,5]

## summarise contrasts
contrasts <- move_no_na %>%
  mutate(alt0.25_vs_0.00_da_mu = apply(alt0.25_vs_0.00_da, 2, mean),
         alt0.25_vs_0.00_da_sd = apply(alt0.25_vs_0.00_da, 2, sd),
         alt0.25_vs_0.00_aa_mu = apply(alt0.25_vs_0.00_aa, 2, mean),
         alt0.25_vs_0.00_aa_sd = apply(alt0.25_vs_0.00_aa, 2, sd),
         alt0.25_vs_0.00_n_mu  = apply(alt0.25_vs_0.00_n, 2, mean),
         alt0.25_vs_0.00_n_sd  = apply(alt0.25_vs_0.00_n, 2, sd),
         alt0.25_vs_0.00_at_mu = apply(alt0.25_vs_0.00_at, 2, mean),
         alt0.25_vs_0.00_at_sd = apply(alt0.25_vs_0.00_at, 2, sd),
         alt0.25_vs_0.00_dt_mu = apply(alt0.25_vs_0.00_dt, 2, mean),
         alt0.25_vs_0.00_dt_sd = apply(alt0.25_vs_0.00_dt, 2, sd),

         alt0.50_vs_0.25_da_mu = apply(alt0.50_vs_0.25_da, 2, mean),
         alt0.50_vs_0.25_da_sd = apply(alt0.50_vs_0.25_da, 2, sd),
         alt0.50_vs_0.25_aa_mu = apply(alt0.50_vs_0.25_aa, 2, mean),
         alt0.50_vs_0.25_aa_sd = apply(alt0.50_vs_0.25_aa, 2, sd),
         alt0.50_vs_0.25_n_mu  = apply(alt0.50_vs_0.25_n, 2, mean),
         alt0.50_vs_0.25_n_sd  = apply(alt0.50_vs_0.25_n, 2, sd),
         alt0.50_vs_0.25_at_mu = apply(alt0.50_vs_0.25_at, 2, mean),
         alt0.50_vs_0.25_at_sd = apply(alt0.50_vs_0.25_at, 2, sd),
         alt0.50_vs_0.25_dt_mu = apply(alt0.50_vs_0.25_dt, 2, mean),
         alt0.50_vs_0.25_dt_sd = apply(alt0.50_vs_0.25_dt, 2, sd),

         alt0.75_vs_0.50_da_mu = apply(alt0.75_vs_0.50_da, 2, mean),
         alt0.75_vs_0.50_da_sd = apply(alt0.75_vs_0.50_da, 2, sd),
         alt0.75_vs_0.50_aa_mu = apply(alt0.75_vs_0.50_aa, 2, mean),
         alt0.75_vs_0.50_aa_sd = apply(alt0.75_vs_0.50_aa, 2, sd),
         alt0.75_vs_0.50_n_mu  = apply(alt0.75_vs_0.50_n, 2, mean),
         alt0.75_vs_0.50_n_sd  = apply(alt0.75_vs_0.50_n, 2, sd),
         alt0.75_vs_0.50_at_mu = apply(alt0.75_vs_0.50_at, 2, mean),
         alt0.75_vs_0.50_at_sd = apply(alt0.75_vs_0.50_at, 2, sd),
         alt0.75_vs_0.50_dt_mu = apply(alt0.75_vs_0.50_dt, 2, mean),
         alt0.75_vs_0.50_dt_sd = apply(alt0.75_vs_0.50_dt, 2, sd),

         alt1.00_vs_0.75_da_mu = apply(alt1.00_vs_0.75_da, 2, mean),
         alt1.00_vs_0.75_da_sd = apply(alt1.00_vs_0.75_da, 2, sd),
         alt1.00_vs_0.75_aa_mu = apply(alt1.00_vs_0.75_aa, 2, mean),
         alt1.00_vs_0.75_aa_sd = apply(alt1.00_vs_0.75_aa, 2, sd),
         alt1.00_vs_0.75_n_mu  = apply(alt1.00_vs_0.75_n, 2, mean),
         alt1.00_vs_0.75_n_sd  = apply(alt1.00_vs_0.75_n, 2, sd),
         alt1.00_vs_0.75_at_mu = apply(alt1.00_vs_0.75_at, 2, mean),
         alt1.00_vs_0.75_at_sd = apply(alt1.00_vs_0.75_at, 2, sd),
         alt1.00_vs_0.75_dt_mu = apply(alt1.00_vs_0.75_dt, 2, mean),
         alt1.00_vs_0.75_dt_sd = apply(alt1.00_vs_0.75_dt, 2, sd))
contrasts_long <- contrasts %>%
  select(-alt0.25_vs_0.00_da_sd,-alt0.25_vs_0.00_aa_sd,-alt0.25_vs_0.00_n_sd,
         -alt0.25_vs_0.00_at_sd,-alt0.25_vs_0.00_dt_sd,
         -alt0.50_vs_0.25_da_sd,-alt0.50_vs_0.25_aa_sd,-alt0.50_vs_0.25_n_sd,
         -alt0.50_vs_0.25_at_sd,-alt0.50_vs_0.25_dt_sd,
         -alt0.75_vs_0.50_da_sd,-alt0.75_vs_0.50_aa_sd,-alt0.75_vs_0.50_n_sd,
         -alt0.75_vs_0.50_dt_sd,-alt0.75_vs_0.50_at_sd,
         -alt1.00_vs_0.75_da_sd,-alt1.00_vs_0.75_aa_sd,-alt1.00_vs_0.75_n_sd,
         -alt1.00_vs_0.75_dt_sd,-alt1.00_vs_0.75_at_sd) %>%
  pivot_longer(cols = c(alt0.25_vs_0.00_da_mu,alt0.25_vs_0.00_aa_mu,alt0.25_vs_0.00_n_mu,
                        alt0.25_vs_0.00_at_mu,alt0.25_vs_0.00_dt_mu,
                        alt0.50_vs_0.25_da_mu,alt0.50_vs_0.25_aa_mu,alt0.50_vs_0.25_n_mu,
                        alt0.50_vs_0.25_at_mu,alt0.50_vs_0.25_dt_mu,
                        alt0.75_vs_0.50_da_mu,alt0.75_vs_0.50_aa_mu,alt0.75_vs_0.50_n_mu,
                        alt0.75_vs_0.50_at_mu,alt0.75_vs_0.50_dt_mu,
                        alt1.00_vs_0.75_da_mu,alt1.00_vs_0.75_aa_mu,alt1.00_vs_0.75_n_mu,
                        alt1.00_vs_0.75_at_mu,alt1.00_vs_0.75_dt_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','contrast'), sep = 3) %>%
  separate(contrast, into = c('later','vs','earlier','pred_type','mu'),
           sep = '_', remove = T) %>%
  select(-alt, -vs, -mu) %>%
  mutate(later = as.numeric(later),
         earlier = as.numeric(earlier),
         contrast = paste0(later,'_',earlier),
         pred_type = ifelse(pred_type == 'da', 'directly away',
                            ifelse(pred_type == 'aa', 'away at an angle',
                                   ifelse(pred_type == 'n', 'neither towards or away',
                                          ifelse(pred_type == 'at', 'approach at an angle',
                                                 'directly approach'))))) %>%
  mutate(pred_type = factor(pred_type, levels = c('directly away',
                                                  'away at an angle',
                                                  'neither towards or away',
                                                  'approach at an angle',
                                                  'directly approach')))

## plot contrasts
time_pred %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = pred_type))+
  facet_wrap(. ~ stim_type)
contrasts_long %>%
  ggplot()+
  geom_density(aes(x = difference))+
  facet_grid(pred_type ~ contrast, scales = 'free')
save.image('movement_direction/movement_ordinal_model1_timecontrasts.RData')
dev.off()
