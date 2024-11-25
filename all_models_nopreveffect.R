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
## read in age data
ages <- readRDS('../data_processed/elephant_behaviour_proportions.RDS') %>%
  select(pb_num, subject, targeted_elephant,    # random effects
         stim_type,age_category,partner_age_category,age_difference, # exposures
         age,partner_age,focal,dyad_partner,    # reference info
         group_size                             # remove any with only 2
  ) %>%
  distinct()

## read in stimulus start time data
stim_starts <- readRDS('../data_processed/stimuli.RDS') %>%
  filter(status == 'START' & behavior == 'STIMULUS') %>%
  select(pb_num,time,stim_num,comment,stim_type)

## remove false starts
table(stim_starts$pb_num)
# 1  3  4  6  7 10 11 13 14 15 16 17 18 19 21 22 23 24 25 28 29 30 31 32 33 34 35 36 37 38 41 42 43 44 45 46 47 48 50 51 52 53 55 56 58 59 60 61
# 1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  2  1  1  2  1  1  3  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  5  1  1  1  1  1  1
multiple_starts <- c(10, 24, 29, 32, 46, 53)
check <- stim_starts %>%
  filter(pb_num %in% multiple_starts) # for stim 10+46+53 take first time, for 24+29+32 use second.
for(i in multiple_starts){
  x <- check %>% filter(pb_num == i)
  check <- anti_join(check, x)
  if(i %in% c(10,46,53)){
    x <- x[1,]
  }
  if(i %in% c(24,29,32)){
    x <- x[2,]
  }
  check <- rbind(check, x)
}
stim_starts <- stim_starts %>%
  filter(! pb_num %in% multiple_starts) %>%
  rbind(check) %>%
  mutate(time = as.numeric(time)) %>%
  mutate(stim_start = round(time, 0)) %>%
  select(pb_num, stim_start,stim_num,stim_type)
rm(check, i, multiple_starts, x) ; gc()

## read in stop time data
stim_stops <- readRDS('../data_processed/stimuli.RDS') %>%
  filter(status == 'STOP' & behavior == 'STIMULUS') %>%
  select(pb_num,time,stim_num,comment)

## remove false starts
(multiple_stops <- table(stim_stops$pb_num) %>%
    as.data.frame() %>%
    filter(Freq > 1))
#   Var1 Freq
# 1   10    2
# 2   24    2
# 3   29    2
# 4   32    3
# 5   46    2
# 6   53    5
multiple_stops <- c(10, 24, 29, 32, 46, 53)
check <- stim_stops %>%
  filter(pb_num %in% multiple_stops) # 10+46 = take first time, 24+29+32+53 = take last time
for(i in multiple_stops){
  x <- check %>% filter(pb_num == i)
  check <- anti_join(check, x)
  if(i %in% c(10,46)){
    x <- x[1,]
  }
  if(i %in% c(24,29,32,53)){
    x <- x[nrow(x),]
  }
  check <- rbind(check, x)
}
stim_stops <- stim_stops %>%
  filter(! pb_num %in% multiple_stops) %>%
  rbind(check) %>%
  mutate(time = as.numeric(time)) %>%
  mutate(stim_stop = round(time, 0)) %>%
  select(pb_num, stim_stop)
rm(check, i, multiple_stops, x) ; gc()

######## combine with behaviour data to identify before/during/after ####
cols_of_interest <- c('b1_nn','b2_nn','b3_nn','b4_nn',
                      'b5_nn','b6_nn','b7_nn','b8_nn',
                      'b1_move','b2_move','b3_move','b4_move',
                      'b5_move','b6_move','b7_move','b8_move',
                      'b1_look','b2_look','b3_look','b4_look',
                      'b5_look','b6_look','b7_look','b8_look')
cols_of_interest_name <- c('b1_nn_name','b2_nn_name','b3_nn_name','b4_nn_name',
                           'b5_nn_name','b6_nn_name','b7_nn_name','b8_nn_name',
                           'b1_move_name','b2_move_name','b3_move_name','b4_move_name',
                           'b5_move_name','b6_move_name','b7_move_name','b8_move_name',
                           'b1_look_name','b2_look_name','b3_look_name','b4_look_name',
                           'b5_look_name','b6_look_name','b7_look_name','b8_look_name')
cols_of_interest_index <- c('b1_nn_index','b2_nn_index','b3_nn_index','b4_nn_index',
                            'b5_nn_index','b6_nn_index','b7_nn_index','b8_nn_index',
                            'b1_move_index','b2_move_index','b3_move_index','b4_move_index',
                            'b5_move_index','b6_move_index','b7_move_index','b8_move_index',
                            'b1_look_index','b2_look_index','b3_look_index','b4_look_index',
                            'b5_look_index','b6_look_index','b7_look_index','b8_look_index')
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
  select(subject,pb_num,second,out_frame_name,
         all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
  # convert to tidy format
  rename(b1_nn = b1_nn_name, b2_nn = b2_nn_name,
         b3_nn = b3_nn_name, b4_nn = b4_nn_name,
         b5_nn = b5_nn_name, b6_nn = b6_nn_name,
         b7_nn = b7_nn_name, b8_nn = b8_nn_name,
         b1_move = b1_move_name, b2_move = b2_move_name,
         b3_move = b3_move_name, b4_move = b4_move_name,
         b5_move = b5_move_name, b6_move = b6_move_name,
         b7_move = b7_move_name, b8_move = b8_move_name,
         b1_look = b1_look_name, b2_look = b2_look_name,
         b3_look = b3_look_name, b4_look = b4_look_name,
         b5_look = b5_look_name, b6_look = b6_look_name,
         b7_look = b7_look_name, b8_look = b8_look_name) %>%
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_name', values_to = 'action_name') %>%
  rename(b1_nn = b1_nn_index, b2_nn = b2_nn_index,
         b3_nn = b3_nn_index, b4_nn = b4_nn_index,
         b5_nn = b5_nn_index, b6_nn = b6_nn_index,
         b7_nn = b7_nn_index, b8_nn = b8_nn_index,
         b1_move = b1_move_index, b2_move = b2_move_index,
         b3_move = b3_move_index, b4_move = b4_move_index,
         b5_move = b5_move_index, b6_move = b6_move_index,
         b7_move = b7_move_index, b8_move = b8_move_index,
         b1_look = b1_look_index, b2_look = b2_look_index,
         b3_look = b3_look_index, b4_look = b4_look_index,
         b5_look = b5_look_index, b6_look = b6_look_index,
         b7_look = b7_look_index, b8_look = b8_look_index) %>%
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_index', values_to = 'action_index') %>%
  filter(elephant_activity_name == elephant_activity_index) %>%
  select(-elephant_activity_index) %>%
  # clean up
  rename(elephant_activity = elephant_activity_name) %>%
  filter(action_name != 'impossible_partner') %>%
  # fix out of sight variable
  mutate(action_name = ifelse(out_frame_name == 'out_of_sight',
                              'out_of_sight', action_name),
         action_index = ifelse(out_frame_name == 'out_of_sight',
                               9, action_index)) %>%
  # join on explanatory and random variables
  separate(elephant_activity, into = c('targeted_elephant','activity'), sep = '_', remove = T) %>%
  mutate(targeted_elephant = paste0(targeted_elephant, '_e', pb_num)) %>%
  left_join(ages[,c('subject','targeted_elephant','age_category','partner_age_category','age','partner_age')],
            by = c('subject','targeted_elephant')) %>%
  rename(focal = subject,
         partner = targeted_elephant,
         f_age_num = age_category,
         f_age_cat = age,
         p_age_num = partner_age_category,
         p_age_cat = partner_age) %>%
  mutate(pb_num = as.numeric(pb_num)) %>%
  left_join(stim_starts, by = 'pb_num') %>%
  left_join(stim_stops, by = 'pb_num') %>%
  # clean up
  mutate(bda = ifelse(second < stim_start, 'before',
                      ifelse(second < stim_stop, 'during','after'))) %>%
  dplyr::select(pb_num,focal,partner,activity,action_name,action_index,
                stim_num,stim_type,stim_start,stim_stop,second,bda,
                f_age_cat,p_age_cat,f_age_num,p_age_num) %>%
  mutate(f_age_num = as.factor(f_age_num),
         p_age_num = as.factor(p_age_num),
         age_combo = paste0(f_age_num,'_',p_age_num))
rm(cols_of_interest, cols_of_interest_name, cols_of_interest_index, stim_starts, stim_stops, ages) ; gc()

saveRDS(behav, '../data_processed/behaviour_by_second_indexvariables_bda.RDS')
# behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')

## remove individuals where ages are unknown
behav <- behav %>%
  filter(!is.na(f_age_num))

######## nearest neighbour                                           ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelchecks.pdf')

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
num_iter <- 2000
nbm_prior <- brm(
  formula = action ~ 0 + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(nbm_prior)

#### fit model                                                       ####
nbm_fit <- brm(
  formula = action ~ 0 + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)
# Warning messages:
# 1: There were 226 divergent transitions after warmup. See https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup # to find out why this is a problem and how to eliminate them.
# 2: There were 3770 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 3: Examine the pairs() plot to diagnose sampling problems
# 4: The largest R-hat is 1.07, indicating chains have not mixed. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#r-hat
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#bulk-ess
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable. Running the chains for more iterations may help. See https://mc-stan.org/misc/warnings.html#tail-ess
save.image('nearest_neighbour/neighbour_noprev_run.RData')

## check model fit
# load('nearest_neighbour/neighbour_noprev_run.RData')
(summary <- summary(nbm_fit))
# Family: bernoulli
# Links: mu = logit
# Formula: action ~ 0 + age_combo + stim_type + bda + (1 | focal) + (1 | stim_num) + (1 | pb_num)
# Data: nn (Number of observations: 170652)
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
#
# Group-Level Effects:
#   ~focal (Number of levels: 176)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.84      0.07     0.72     0.99 1.00      954     1625
#
# ~pb_num (Number of levels: 48)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     6.31      1.04     4.56     8.60 1.01      236      158
#
# ~stim_num (Number of levels: 30)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.51      1.01     0.06     3.73 1.07       46       41
#
# Population-Level Effects:
#              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# age_combo1_1     0.56      0.34    -0.13     1.19 1.00      490      852
# age_combo1_2    -0.45      0.33    -1.12     0.19 1.00      487      888
# age_combo1_3    -3.18      0.34    -3.87    -2.53 1.00      507      908
# age_combo1_4    -1.19      0.34    -1.89    -0.54 1.00      488      965
# age_combo2_1     0.06      0.28    -0.50     0.59 1.00      354      721
# age_combo2_2    -0.73      0.27    -1.28    -0.20 1.00      349      728
# age_combo2_3    -0.53      0.27    -1.09    -0.01 1.00      350      695
# age_combo2_4    -0.64      0.27    -1.20    -0.12 1.00      354      714
# age_combo3_1    -1.85      0.28    -2.43    -1.31 1.01      380      642
# age_combo3_2    -0.13      0.27    -0.69     0.40 1.01      357      625
# age_combo3_3    -0.55      0.27    -1.10    -0.02 1.01      356      629
# age_combo3_4    -0.89      0.28    -1.44    -0.36 1.01      357      637
# age_combo4_1    -1.25      0.30    -1.85    -0.68 1.00      388      802
# age_combo4_2    -0.48      0.29    -1.06     0.09 1.01      371      751
# age_combo4_3    -0.97      0.29    -1.54    -0.41 1.01      373      699
# age_combo4_4    -0.16      0.29    -0.74     0.40 1.01      375      642
# stim_typeh       0.02      0.89    -1.76     1.69 1.00     1074     1505
# stim_typel      -0.06      0.92    -1.85     1.75 1.00     1177     2169
# bdabefore        0.08      0.01     0.05     0.10 1.00     2784     1976
# bdaduring        0.07      0.02     0.02     0.12 1.00     3063     2114
#
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning messages:
# 1: Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors.
# 2: There were 226 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
par(mfrow = c(3,1))
hist(summary$fixed$Rhat, breaks = 50)
hist(summary$fixed$Bulk_ESS, breaks = 50)
hist(summary$fixed$Tail_ESS, breaks = 50)
par(mfrow = c(1,1))

## extract posterior distribution
draws <- as_draws_df(nbm_fit) %>%
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

# extract marginal effects
marg <- conditional_effects(nbm_fit,
                            effects = c('age_combo','stim_type','bda'),
                            categorical = FALSE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg) # "age_combo" "stim_type" "bda"
age_effect <- marg[[1]]
stim_effect <- marg[[2]]
bda_effect <- marg[[3]]

#### plot marginal effects                                           ####
neighbour_labels <- c('neighbour age category 1',
                      'neighbour age category 2',
                      'neighbour age category 3',
                      'neighbour age category 4')
names(neighbour_labels) <- c(1:4)
(focal_age_plot <- age_effect %>%
    separate(col = age_combo, sep = '_', remove = F,
             into = c('focal_age','neighbour_age')) %>%
    mutate(agecombo = paste0(focal_age,'-',neighbour_age)) %>%
    ggplot()+
    geom_errorbar(aes(x = focal_age,
                      colour = focal_age,
                      ymax = upper__, ymin = lower__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = focal_age,
                   colour = focal_age,
                   y = estimate__),
               cex = 3)+
    xlab(label = 'focal age category')+
    ylab('probability of being nearest neighbours after dove stimulus')+
    scale_colour_viridis_d(name = 'focal age:')+
    facet_wrap(. ~ neighbour_age,
               labeller = labeller(neighbour_age = neighbour_labels))+
    theme(legend.direction = 'horizontal',
          legend.position = 'bottom',
          legend.box = 'vertical',
          legend.spacing.x = unit(0.2, 'cm'),
          legend.spacing.y = unit(2, 'mm'),
          axis.title = element_text(size = 16),
          axis.text.x = element_text(size = 12,
                                     #angle = 70,
                                     vjust = 0.5),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = focal_age_plot, filename = '../outputs/neighbour_binomial_model_bda/neighbour_noprev_marginaleffects_focalage.png',
       device = 'png', width = 8.3, height = 5.8)

(stim_plot <- stim_effect %>%
    ggplot()+
    geom_errorbar(aes(x = stim_type,
                      colour = stim_type,
                      ymax = upper__, ymin = lower__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type,
                   colour = stim_type,
                   shape = stim_type,
                   y = estimate__),
               cex = 3)+
    ylab('probability of being nearest neighbours after stimulus, age combo 1_1')+
    scale_colour_viridis_d(name = 'stimulus type:')+
    scale_shape_manual(name = 'stimulus type:', values = c(15:18))+
    scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h'))+
    theme(legend.position = 'none',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = stim_plot, filename = '../outputs/neighbour_binomial_model_bda/neighbour_noprev_marginaleffects_stimtype.png', device = 'png',
       width = 8.3, height = 5.8)
rm(focal_age_plot,stim_plot,age_effect,prev_effect,stim_effect,bda_effect) ;gc()

#### posterior predictive check                                      ####
pp_check(nbm_fit, ndraws = 100)

#### plot traces                                                     ####
parameters_of_interest <- parameters[1:which(parameters == 'b_stim_typel')]
draws %>%
  filter(parameter %in% parameters_of_interest) %>%
  ggplot(aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

#### plot density curves                                             ####
draws %>%
  filter(parameter %in% parameters_of_interest) %>%
  ggplot(aes(x = draw, colour = as.factor(chain)))+
  geom_density()+
  facet_wrap(. ~ parameter, scales = 'free')+
  theme(legend.position = 'none')

save.image('nearest_neighbour/neighbour_noprev_run.RData')

## reset plotting
dev.off()

#### predict from model                                              ####
# load('nearest_neighbour/neighbour_noprev_run.RData')
rm(list = ls()[! ls() %in% c('nbm_fit','nn','behav','num_iter','num_chains')])

pred <- posterior_epred(object = nbm_fit,
                        newdata = nn)
save.image('nearest_neighbour/neighbour_noprev_predictions.RData')

## convert to data frame
nn$data_row <- 1:nrow(nn)
predictions <- as.data.frame(pred)
colnames(predictions) <- 1:nrow(nn)
predictions <- predictions %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row', values_to = 'epred') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(nn, by = 'data_row')

save.image('nearest_neighbour/neighbour_noprev_predictions.RData')
rm(pred) ; gc()

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions                                                ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelpredictions.pdf')
load('nearest_neighbour/neighbour_noprev_predictions.RData')

predictions %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('dove (control)', 'lion', 'human')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
    ggplot()+
    geom_violin(aes(x = f_age_cat,
                    y = epred,
                    colour = bda)) +
    facet_grid(p_age_cat ~ stim_type)+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'time relative to stimulus:',
         fill = 'time relative to stimulus:',
         x = 'age category of focal elephant',
         y = 'predicted probability')+
    theme(legend.position = 'bottom')
ggsave(path = '../outputs/neighbour_binomial_model_bda/',
       filename = 'neighbour_noprev_modelpredictions.png',
       device = 'png', height = 1200, width = 1600, units = 'px')

## reset plotting
dev.off()

#### calculate posterior contrasts from predictions                  ####
load('nearest_neighbour/neighbour_noprev_predictions.RData')
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelcontrasts.pdf')

## stim type                                                         ####
stim_new <- nn %>%
  dplyr::select(age_combo, stim_type, bda,
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(age_combo, bda,
                                                         focal, stim_num, pb_num))))

## redo predictions with different stimulus types: all doves
ctd_nn <- stim_new %>%
  mutate(stim_type = 'ctd')
ctd_mtx <- posterior_epred(object = nbm_fit, newdata = ctd_nn)
colnames(ctd_mtx) <- ctd_nn$unique_data_combo
ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all lions
lion_nn <- stim_new %>%
  mutate(stim_type = 'l')
lion_mtx <- posterior_epred(object = nbm_fit, newdata = lion_nn)
colnames(lion_mtx) <- lion_nn$unique_data_combo
lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all humans
human_nn <- stim_new %>%
  mutate(stim_type = 'h')
human_mtx <- posterior_epred(object = nbm_fit, newdata = human_nn)
colnames(human_mtx) <- human_nn$unique_data_combo
human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

save.image('nearest_neighbour/neighbour_noprev_stimuluscontrasts.RData')

## calculate contrasts
ctd_vs_lion <- lion_mtx - ctd_mtx
ctd_vs_human <- human_mtx - ctd_mtx
lion_vs_human <- human_mtx - lion_mtx

## summarise contrasts
contrasts <- nn %>%
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

## produce values for reporting
print('dove vs lion')
mean(ctd_vs_lion)  ; sd(ctd_vs_lion)
quantile(ctd_vs_lion, prob = c(0.025, 0.5, 0.975))
# "dove vs lion"
# -0.004485089
# 0.1501311
#          2.5%           50%         97.5%
# -0.3007870932 -0.0001918324  0.3300206064
print('dove vs human')
mean(ctd_vs_human) ; sd(ctd_vs_human)
quantile(ctd_vs_human, prob = c(0.025, 0.5, 0.975))
# "dove vs human"
# -0.0003306431
# 0.1407757
#          2.5%           50%         97.5%
# -2.965974e-01  3.358137e-06  3.067035e-01
print('lion vs human')
mean(lion_vs_human); sd(lion_vs_human)
quantile(lion_vs_human, prob = c(0.025, 0.5, 0.975))
# "lion vs human"
# 0.004154446
# 0.1881756
#          2.5%           50%         97.5%
# -0.3977546312  0.0001321291  0.4079827226

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

save.image('nearest_neighbour/neighbour_noprev_stimuluscontrasts.RData')

## age combo                                                         ####
# load('nearest_neighbour/neighbour_noprev_stimuluscontrasts.RData')
rm(list = ls()[!ls() %in% c('nbm_fit','nn','behav','num_iter','num_chains')]) ; gc()

## create new dataframe to predict from
age_new <- nn %>%
  dplyr::select(age_combo, stim_type, bda,
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(age_combo, bda,
                                                         focal, stim_num, pb_num))))
## predict with original ages
age_nn_org <- age_new
age_mtx_org <- posterior_epred(object = nbm_fit, newdata = age_nn_org)
colnames(age_mtx_org) <- age_nn_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with altered age combinations
age_predict <- function(df, age_combination, model){
  df_new <- df %>%
    mutate(age_combo = age_combination)
  mtx <- posterior_epred(object = model, newdata = df_new)
  colnames(mtx) <- df_new$unique_data_combo
  mtx <- mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
  return(mtx)
}
mtx_11 <- age_predict(df = age_new, age_combination = '1_1', model = nbm_fit)
mtx_12 <- age_predict(df = age_new, age_combination = '1_2', model = nbm_fit)
mtx_13 <- age_predict(df = age_new, age_combination = '1_3', model = nbm_fit)
mtx_14 <- age_predict(df = age_new, age_combination = '1_4', model = nbm_fit)
mtx_21 <- age_predict(df = age_new, age_combination = '2_1', model = nbm_fit)
mtx_22 <- age_predict(df = age_new, age_combination = '2_2', model = nbm_fit)
mtx_23 <- age_predict(df = age_new, age_combination = '2_3', model = nbm_fit)
mtx_24 <- age_predict(df = age_new, age_combination = '2_4', model = nbm_fit)
mtx_31 <- age_predict(df = age_new, age_combination = '3_1', model = nbm_fit)
mtx_32 <- age_predict(df = age_new, age_combination = '3_2', model = nbm_fit)
mtx_33 <- age_predict(df = age_new, age_combination = '3_3', model = nbm_fit)
mtx_34 <- age_predict(df = age_new, age_combination = '3_4', model = nbm_fit)
mtx_41 <- age_predict(df = age_new, age_combination = '4_1', model = nbm_fit)
mtx_42 <- age_predict(df = age_new, age_combination = '4_2', model = nbm_fit)
mtx_43 <- age_predict(df = age_new, age_combination = '4_3', model = nbm_fit)
mtx_44 <- age_predict(df = age_new, age_combination = '4_4', model = nbm_fit)

save.image('nearest_neighbour/neighbour_noprev_agecontrasts.RData')

## calculate contrasts
contrast_11v12 <- mtx_11 - mtx_12
contrast_11v13 <- mtx_11 - mtx_13
contrast_11v14 <- mtx_11 - mtx_14
contrast_11v21 <- mtx_11 - mtx_21
contrast_11v31 <- mtx_11 - mtx_31
contrast_11v41 <- mtx_11 - mtx_41
contrast_12v13 <- mtx_12 - mtx_13
contrast_12v14 <- mtx_12 - mtx_14
contrast_12v22 <- mtx_12 - mtx_22
contrast_12v32 <- mtx_12 - mtx_32
contrast_12v42 <- mtx_12 - mtx_42
contrast_13v14 <- mtx_13 - mtx_14
contrast_13v23 <- mtx_13 - mtx_23
contrast_13v33 <- mtx_13 - mtx_33
contrast_13v43 <- mtx_13 - mtx_43
contrast_14v24 <- mtx_14 - mtx_24
contrast_14v34 <- mtx_14 - mtx_34
contrast_14v44 <- mtx_14 - mtx_44

contrast_21v22 <- mtx_21 - mtx_22
contrast_21v23 <- mtx_21 - mtx_23
contrast_21v24 <- mtx_21 - mtx_24
contrast_21v31 <- mtx_21 - mtx_31
contrast_21v41 <- mtx_21 - mtx_41
contrast_22v23 <- mtx_22 - mtx_23
contrast_22v24 <- mtx_22 - mtx_24
contrast_22v32 <- mtx_22 - mtx_32
contrast_22v42 <- mtx_22 - mtx_42
contrast_23v24 <- mtx_23 - mtx_24
contrast_23v33 <- mtx_23 - mtx_33
contrast_23v43 <- mtx_23 - mtx_43
contrast_24v34 <- mtx_24 - mtx_34
contrast_24v44 <- mtx_24 - mtx_44

contrast_31v32 <- mtx_31 - mtx_32
contrast_31v33 <- mtx_31 - mtx_33
contrast_31v34 <- mtx_31 - mtx_34
contrast_31v41 <- mtx_31 - mtx_41
contrast_32v33 <- mtx_32 - mtx_33
contrast_32v34 <- mtx_32 - mtx_34
contrast_32v42 <- mtx_32 - mtx_42
contrast_33v34 <- mtx_33 - mtx_34
contrast_33v43 <- mtx_33 - mtx_43
contrast_34v44 <- mtx_34 - mtx_44

contrast_41v42 <- mtx_41 - mtx_42
contrast_41v43 <- mtx_41 - mtx_43
contrast_41v44 <- mtx_41 - mtx_44
contrast_42v43 <- mtx_42 - mtx_43
contrast_42v44 <- mtx_42 - mtx_44
contrast_43v44 <- mtx_43 - mtx_44

## summarise contrasts
contrasts <- nn %>%
  select(-stim_type) %>%
  mutate(mu_11v12 = apply(contrast_11v12, 2, mean),
         sd_11v12 = apply(contrast_11v12, 2, sd),
         mu_11v13 = apply(contrast_11v13, 2, mean),
         sd_11v13 = apply(contrast_11v13, 2, sd),
         mu_11v14 = apply(contrast_11v14, 2, mean),
         sd_11v14 = apply(contrast_11v14, 2, sd),
         mu_11v21 = apply(contrast_11v21, 2, mean),
         sd_11v21 = apply(contrast_11v21, 2, sd),
         mu_11v31 = apply(contrast_11v31, 2, mean),
         sd_11v31 = apply(contrast_11v31, 2, sd),
         mu_11v41 = apply(contrast_11v41, 2, mean),
         sd_11v41 = apply(contrast_11v41, 2, sd),

         mu_12v13 = apply(contrast_12v13, 2, mean),
         sd_12v13 = apply(contrast_12v13, 2, sd),
         mu_12v14 = apply(contrast_12v14, 2, mean),
         sd_12v14 = apply(contrast_12v14, 2, sd),
         mu_12v22 = apply(contrast_12v22, 2, mean),
         sd_12v22 = apply(contrast_12v22, 2, sd),
         mu_12v32 = apply(contrast_12v32, 2, mean),
         sd_12v32 = apply(contrast_12v32, 2, sd),
         mu_12v42 = apply(contrast_12v42, 2, mean),
         sd_12v42 = apply(contrast_12v42, 2, sd),

         mu_13v14 = apply(contrast_13v14, 2, mean),
         sd_13v14 = apply(contrast_13v14, 2, sd),
         mu_13v23 = apply(contrast_13v23, 2, mean),
         sd_13v23 = apply(contrast_13v23, 2, sd),
         mu_13v33 = apply(contrast_13v33, 2, mean),
         sd_13v33 = apply(contrast_13v33, 2, sd),
         mu_13v43 = apply(contrast_13v43, 2, mean),
         sd_13v43 = apply(contrast_13v43, 2, sd),

         mu_14v24 = apply(contrast_14v24, 2, mean),
         sd_14v24 = apply(contrast_14v24, 2, sd),
         mu_14v34 = apply(contrast_14v34, 2, mean),
         sd_14v34 = apply(contrast_14v34, 2, sd),
         mu_14v44 = apply(contrast_14v44, 2, mean),
         sd_14v44 = apply(contrast_14v44, 2, sd),

         mu_21v22 = apply(contrast_21v22, 2, mean),
         sd_21v22 = apply(contrast_21v22, 2, sd),
         mu_21v23 = apply(contrast_21v23, 2, mean),
         sd_21v23 = apply(contrast_21v23, 2, sd),
         mu_21v24 = apply(contrast_21v24, 2, mean),
         sd_21v24 = apply(contrast_21v24, 2, sd),
         mu_21v31 = apply(contrast_21v31, 2, mean),
         sd_21v31 = apply(contrast_21v31, 2, sd),
         mu_21v41 = apply(contrast_21v41, 2, mean),
         sd_21v41 = apply(contrast_21v41, 2, sd),

         mu_22v23 = apply(contrast_22v23, 2, mean),
         sd_22v23 = apply(contrast_22v23, 2, sd),
         mu_22v24 = apply(contrast_22v24, 2, mean),
         sd_22v24 = apply(contrast_22v24, 2, sd),
         mu_22v32 = apply(contrast_22v32, 2, mean),
         sd_22v32 = apply(contrast_22v32, 2, sd),
         mu_22v42 = apply(contrast_22v42, 2, mean),
         sd_22v42 = apply(contrast_22v42, 2, sd),

         mu_23v24 = apply(contrast_23v24, 2, mean),
         sd_23v24 = apply(contrast_23v24, 2, sd),
         mu_23v33 = apply(contrast_23v33, 2, mean),
         sd_23v33 = apply(contrast_23v33, 2, sd),
         mu_23v43 = apply(contrast_23v43, 2, mean),
         sd_23v43 = apply(contrast_23v43, 2, sd),

         mu_24v34 = apply(contrast_24v34, 2, mean),
         sd_24v34 = apply(contrast_24v34, 2, sd),
         mu_24v44 = apply(contrast_24v44, 2, mean),
         sd_24v44 = apply(contrast_24v44, 2, sd),

         mu_31v32 = apply(contrast_31v32, 2, mean),
         sd_31v32 = apply(contrast_31v32, 2, sd),
         mu_31v33 = apply(contrast_31v33, 2, mean),
         sd_31v33 = apply(contrast_31v33, 2, sd),
         mu_31v34 = apply(contrast_31v34, 2, mean),
         sd_31v34 = apply(contrast_31v34, 2, sd),
         mu_31v41 = apply(contrast_31v41, 2, mean),
         sd_31v41 = apply(contrast_31v41, 2, sd),

         mu_32v33 = apply(contrast_32v33, 2, mean),
         sd_32v33 = apply(contrast_32v33, 2, sd),
         mu_32v34 = apply(contrast_32v34, 2, mean),
         sd_32v34 = apply(contrast_32v34, 2, sd),
         mu_32v42 = apply(contrast_32v42, 2, mean),
         sd_32v42 = apply(contrast_32v42, 2, sd),

         mu_33v34 = apply(contrast_33v34, 2, mean),
         sd_33v34 = apply(contrast_33v34, 2, sd),
         mu_33v43 = apply(contrast_33v43, 2, mean),
         sd_33v43 = apply(contrast_33v43, 2, sd),

         mu_34v44 = apply(contrast_34v44, 2, mean),
         sd_34v44 = apply(contrast_34v44, 2, sd),

         mu_41v42 = apply(contrast_41v42, 2, mean),
         sd_41v42 = apply(contrast_41v42, 2, sd),
         mu_41v43 = apply(contrast_41v43, 2, mean),
         sd_41v43 = apply(contrast_41v43, 2, sd),
         mu_41v44 = apply(contrast_41v44, 2, mean),
         sd_41v44 = apply(contrast_41v44, 2, sd),

         mu_42v43 = apply(contrast_42v43, 2, mean),
         sd_42v43 = apply(contrast_42v43, 2, sd),
         mu_42v44 = apply(contrast_42v44, 2, mean),
         sd_42v44 = apply(contrast_42v44, 2, sd),

         mu_43v44 = apply(contrast_43v44, 2, mean),
         sd_43v44 = apply(contrast_43v44, 2, sd)
  )

contrasts_long <- contrasts %>%
  pivot_longer(cols = c(mu_11v12,mu_11v13,mu_11v14,mu_11v21,mu_11v31,mu_11v41,
                        mu_12v13,mu_12v14,mu_12v22,mu_12v32,mu_12v42,
                        mu_13v14,mu_13v23,mu_13v33,mu_13v43,
                        mu_14v24,mu_14v34,mu_14v44,
                        mu_21v22,mu_21v23,mu_21v24,mu_21v31,mu_21v41,
                        mu_22v23,mu_22v24,mu_22v32,mu_22v42,
                        mu_23v24,mu_23v33,mu_23v43,
                        mu_24v34,mu_24v44,
                        mu_31v32,mu_31v33,mu_31v34,mu_31v41,
                        mu_32v33,mu_32v34,mu_32v42,
                        mu_33v34,mu_33v43,
                        mu_34v44,
                        mu_41v42,mu_41v43,mu_41v44,
                        mu_42v43,mu_42v44,
                        mu_43v44),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('mu','combo1','v','combo2'),
           sep = c(3,5,6), remove = T) %>%
  select(-sd_11v12,-sd_11v13,-sd_11v14,-sd_11v21,-sd_11v31,-sd_11v41,
         -sd_12v13,-sd_12v14,-sd_12v22,-sd_12v32,-sd_12v42,
         -sd_13v14,-sd_13v23,-sd_13v33,-sd_13v43,
         -sd_14v24,-sd_14v34,-sd_14v44,
         -sd_21v22,-sd_21v23,-sd_21v24,-sd_21v31,-sd_21v41,
         -sd_22v23,-sd_22v24,-sd_22v32,-sd_22v42,
         -sd_23v24,-sd_23v33,-sd_23v43,
         -sd_24v34,-sd_24v44,
         -sd_31v32,-sd_31v33,-sd_31v34,-sd_31v41,
         -sd_32v33,-sd_32v34,-sd_32v42,
         -sd_33v34,-sd_33v43,
         -sd_34v44,
         -sd_41v42,-sd_41v43,-sd_41v44,
         -sd_42v43,-sd_42v44,
         -sd_43v44,
         -mu)

## save contrasts
save.image('nearest_neighbour/neighbour_noprev_agecontrasts.RData')

## produce values for reporting
print('11v12')
mean(contrast_11v12); sd(contrast_11v12)
quantile(contrast_11v12, prob = c(0.025, 0.5, 0.975))
# "11v12"
# 0.1907554
# 0.07355961
#         2.5%          50%        97.5%
# 1.474336e-05 2.131843e-01 2.674389e-01
print('11v13')
mean(contrast_11v13); sd(contrast_11v13)
quantile(contrast_11v13, prob = c(0.025, 0.5, 0.975))
# "11v13"
# 0.4135505
# 0.1865288
#         2.5%         50%       97.5%
# 0.000347739 0.432317332 0.704206570
print('11v14')
mean(contrast_11v14); sd(contrast_11v14)
quantile(contrast_11v14, prob = c(0.025, 0.5, 0.975))
# "11v14"
# 0.2940887
# 0.1199198
#         2.5%          50%        97.5%
# 4.042153e-05 3.238599e-01 4.383125e-01
print('11v21')
mean(contrast_11v21); sd(contrast_11v21)
quantile(contrast_11v21, prob = c(0.025, 0.5, 0.975))
# "11v21"
# 0.09797386
# 0.07267368
#         2.5%         50%       97.5%
# -0.01907567  0.09807823  0.25471420
print('11v31')
mean(contrast_11v31); sd(contrast_11v31)
quantile(contrast_11v31, prob = c(0.025, 0.5, 0.975))
# "11v31"
# 0.3567232
# 0.1582444
#         2.5%          50%        97.5%
# 8.529905e-05 3.793389e-01 5.967685e-01
print('11v41')
mean(contrast_11v41); sd(contrast_11v41)
quantile(contrast_11v41, prob = c(0.025, 0.5, 0.975))
# "11v41"
# 0.3028858
# 0.135422
#         2.5%          50%        97.5%
# 4.393867e-05 3.227396e-01 5.150554e-01
print('12v13')
mean(contrast_12v13); sd(contrast_12v13)
quantile(contrast_12v13, prob = c(0.025, 0.5, 0.975))
# "12v13"
# 0.2227952
# 0.1252813
#         2.5%         50%       97.5%
# 0.000331207 0.212493891 0.487353870
print('12v14')
mean(contrast_12v14); sd(contrast_12v14)
quantile(contrast_12v14, prob = c(0.025, 0.5, 0.975))
# "12v14"
# 0.1033333
# 0.05195249
#         2.5%          50%        97.5%
# 0.0000253545 0.1048126978 0.1970168915
print('12v22')
mean(contrast_12v22); sd(contrast_12v22)
quantile(contrast_12v22, prob = c(0.025, 0.5, 0.975))
# "12v22"
# 0.04445598
# 0.05522511
#         2.5%         50%       97.5%
# -0.03976662  0.03705341  0.18120759
print('12v32')
mean(contrast_12v32); sd(contrast_12v32)
quantile(contrast_12v32, prob = c(0.025, 0.5, 0.975))
# "12v32"
# -0.05443373
# 0.06044366
#         2.5%         50%       97.5%
# -0.17487497 -0.05749970  0.06415536
print('12v42')
mean(contrast_12v42); sd(contrast_12v42)
quantile(contrast_12v42, prob = c(0.025, 0.5, 0.975))
# "12v42"
# 0.007403856
# 0.05982049
#         2.5%           50%         97.5%
# -1.041463e-01  1.658985e-05  1.444455e-01
print('13v14')
mean(contrast_13v14); sd(contrast_13v14)
quantile(contrast_13v14, prob = c(0.025, 0.5, 0.975))
# "13v14"
# -0.1194619
# 0.07809974
#         2.5%           50%         97.5%
# -0.3079157948 -0.1061168970 -0.0002853226
print('13v23')
mean(contrast_13v23); sd(contrast_13v23)
quantile(contrast_13v23, prob = c(0.025, 0.5, 0.975))
# "13v23"
# -0.2080729
# 0.1153636
#         2.5%           50%         97.5%
# -0.4547014480 -0.1981099592 -0.0003307762
print('13v33')
mean(contrast_13v33); sd(contrast_13v33)
quantile(contrast_13v33, prob = c(0.025, 0.5, 0.975))
# "13v33"
# -0.2042007
# 0.1135994
#         2.5%           50%         97.5%
# -0.4466470284 -0.1933618566 -0.0003301032
print('13v43')
mean(contrast_13v43); sd(contrast_13v43)
quantile(contrast_13v43, prob = c(0.025, 0.5, 0.975))
# "13v43"
# -0.1442887
# 0.08921022
#         2.5%           50%         97.5%
# -0.3600575261 -0.1310368649 -0.0003052864
print('14v24')
mean(contrast_14v24); sd(contrast_14v24)
quantile(contrast_14v24, prob = c(0.025, 0.5, 0.975))
# "14v24"
# -0.07140298
# 0.05434339
#         2.5%         50%       97.5%
# -0.19207241 -0.06866198  0.01410108
print('14v34')
mean(contrast_14v34); sd(contrast_14v34)
quantile(contrast_14v34, prob = c(0.025, 0.5, 0.975))
# "14v34"
# -0.03499634
# 0.04650559
#         2.5%         50%       97.5%
# -0.13580854 -0.03392572  0.05665805
print('14v44')
mean(contrast_14v44); sd(contrast_14v44)
quantile(contrast_14v44, prob = c(0.025, 0.5, 0.975))
# "14v44"
# -0.1509054
# 0.0846154
#          2.5%           50%         97.5%
# -3.180956e-01 -1.530877e-01 -2.028654e-05

print('21v22')
mean(contrast_21v22); sd(contrast_21v22)
quantile(contrast_21v22, prob = c(0.025, 0.5, 0.975))
# "21v22"
# 0.1372375
# 0.0564042
#         2.5%          50%        97.5%
# 1.662795e-05 1.494410e-01 2.072854e-01
print('21v23')
mean(contrast_21v23); sd(contrast_21v23)
quantile(contrast_21v23, prob = c(0.025, 0.5, 0.975))
# "21v23"
# 0.1075037
# 0.04372262
#         2.5%          50%        97.5%
# 1.130633e-05 1.176269e-01 1.623386e-01
print('21v24')
mean(contrast_21v24); sd(contrast_21v24)
quantile(contrast_21v24, prob = c(0.025, 0.5, 0.975))
# "21v24"
# 0.1247118
# 0.05114132
#         2.5%          50%        97.5%
# 1.427966e-05 1.361052e-01 1.900821e-01
print('21v31')
mean(contrast_21v31); sd(contrast_21v31)
quantile(contrast_21v31, prob = c(0.025, 0.5, 0.975))
# "21v31"
# 0.2587493
# 0.12012
#         2.5%          50%        97.5%
# 0.0000801562 0.2667607455 0.4624373862
print('21v41')
mean(contrast_21v41); sd(contrast_21v41)
quantile(contrast_21v41, prob = c(0.025, 0.5, 0.975))
# "21v41"
# 0.204912
# 0.09633482
#         2.5%          50%        97.5%
# 3.810106e-05 2.132453e-01 3.729324e-01
print('22v23')
mean(contrast_22v23); sd(contrast_22v23)
quantile(contrast_22v23, prob = c(0.025, 0.5, 0.975))
# "22v23"
# -0.02973376
# 0.01408272
#         2.5%           50%         97.5%
# -5.430017e-02 -3.076915e-02 -5.290126e-06
print('22v24')
mean(contrast_22v24); sd(contrast_22v24)
quantile(contrast_22v24, prob = c(0.025, 0.5, 0.975))
# "22v24"
# -0.01252568
# 0.008232991
#         2.5%           50%         97.5%
# -3.000978e-02 -1.223876e-02 -5.582645e-08
print('22v32')
mean(contrast_22v32); sd(contrast_22v32)
quantile(contrast_22v32, prob = c(0.025, 0.5, 0.975))
# "22v32"
# -0.09888971
# 0.05289369
#         2.5%           50%         97.5%
# -1.958879e-01 -1.025223e-01 -1.269338e-05
print('22v42')
mean(contrast_22v42); sd(contrast_22v42)
quantile(contrast_22v42, prob = c(0.025, 0.5, 0.975))
# "22v42"
# -0.03705212
# 0.04575221
#         2.5%         50%       97.5%
# -0.13511130 -0.03455667  0.04574463
print('23v24')
mean(contrast_23v24); sd(contrast_23v24)
quantile(contrast_23v24, prob = c(0.025, 0.5, 0.975))
# "23v24"
# 0.01720808
# 0.009406388
#         2.5%          50%        97.5%
#         2.789241e-06 1.731086e-02 3.633478e-02
print('23v33')
mean(contrast_23v33); sd(contrast_23v33)
quantile(contrast_23v33, prob = c(0.025, 0.5, 0.975))
# "23v33"
# 0.003872205
# 0.03255564
#         2.5%           50%         97.5%
# -0.0561397638  0.0000551845  0.0789983368
print('23v43')
mean(contrast_23v43); sd(contrast_23v43)
quantile(contrast_23v43, prob = c(0.025, 0.5, 0.975))
# "23v43"
# 0.06378428
# 0.04886848
#         2.5%           50%         97.5%
# -0.0002493421  0.0584598159  0.1772400575
print('24v34')
mean(contrast_24v34); sd(contrast_24v34)
quantile(contrast_24v34, prob = c(0.025, 0.5, 0.975))
# "24v34"
# 0.03640664
# 0.0347608
#         2.5%         50%       97.5%
# -0.01085940  0.03153682  0.12020474
print('24v44')
mean(contrast_24v44); sd(contrast_24v44)
quantile(contrast_24v44, prob = c(0.025, 0.5, 0.975))
# "24v44"
# -0.07950238
# 0.05691474
#         2.5%          50%        97.5%
# -0.192499064 -0.080503227  0.005980926

print('31v32')
mean(contrast_31v32); sd(contrast_31v32)
quantile(contrast_31v32, prob = c(0.025, 0.5, 0.975))
# "31v32"
# -0.2204015
# 0.1043572
#         2.5%           50%         97.5%
# -3.956745e-01 -2.239751e-01 -7.703904e-05
print('31v33')
mean(contrast_31v33); sd(contrast_31v33)
quantile(contrast_31v33, prob = c(0.025, 0.5, 0.975))
# "31v33"
# -0.1473734
# 0.0760045
#         2.5%           50%         97.5%
# -2.933390e-01 -1.444086e-01 -6.831489e-05
print('31v34')
mean(contrast_31v34); sd(contrast_31v34)
quantile(contrast_31v34, prob = c(0.025, 0.5, 0.975))
# "31v34"
# -0.09763088
# 0.05387945
#         2.5%           50%         97.5%
# -2.102394e-01 -9.269844e-02 -5.790641e-05
print('31v41')
mean(contrast_31v41); sd(contrast_31v41)
quantile(contrast_31v41, prob = c(0.025, 0.5, 0.975))
# "31v41"
# -0.05383738
# 0.04224571
#         2.5%           50%         97.5%
# -1.603287e-01 -4.735385e-02 -1.046405e-06
print('32v33')
mean(contrast_32v33); sd(contrast_32v33)
quantile(contrast_32v33, prob = c(0.025, 0.5, 0.975))
# "32v33"
# 0.07302815
# 0.03018785
#         2.5%          50%        97.5%
# 8.829657e-06 7.894146e-02 1.106361e-01
print('32v34')
mean(contrast_32v34); sd(contrast_32v34)
quantile(contrast_32v34, prob = c(0.025, 0.5, 0.975))
# "32v34"
# 0.1227707
# 0.05260935
#         2.5%          50%        97.5%
# 1.933158e-05 1.305745e-01 1.927930e-01
print('32v42')
mean(contrast_32v42); sd(contrast_32v42)
quantile(contrast_32v42, prob = c(0.025, 0.5, 0.975))
# "32v42"
# 0.06183759
# 0.05134167
#         2.5%         50%       97.5%
# -0.01481436  0.05998304  0.17117818
print('33v34')
mean(contrast_33v34); sd(contrast_33v34)
quantile(contrast_33v34, prob = c(0.025, 0.5, 0.975))
# "33v34"
# 0.04974251
# 0.02343468
#         2.5%          50%        97.5%
# 1.045525e-05 5.106214e-02 8.899195e-02
print('33v43')
mean(contrast_33v43); sd(contrast_33v43)
quantile(contrast_33v43, prob = c(0.025, 0.5, 0.975))
# "33v43"
# 0.05991208
# 0.0469499
#         2.5%          50%        97.5%
# -0.003713618  0.056633662  0.165151746
print('34v44')
mean(contrast_34v44); sd(contrast_34v44)
quantile(contrast_34v44, prob = c(0.025, 0.5, 0.975))
# "34v44"
# -0.115909
# 0.0661464
#         2.5%           50%         97.5%
# -2.466809e-01 -1.172738e-01 -8.421238e-06

print('41v42')
mean(contrast_41v42); sd(contrast_41v42)
quantile(contrast_41v42, prob = c(0.025, 0.5, 0.975))
# "41v42"
# -0.1047266
# 0.05121073
#         2.5%           50%         97.5%
# -1.956581e-01 -1.059004e-01 -2.822182e-05
print('41v43')
mean(contrast_41v43); sd(contrast_41v43)
quantile(contrast_41v43, prob = c(0.025, 0.5, 0.975))
# "41v43"
# -0.03362393
# 0.02036506
#         2.5%           50%         97.5%
# -7.921471e-02 -3.209365e-02 -1.215938e-05
print('41v44')
mean(contrast_41v44); sd(contrast_41v44)
quantile(contrast_41v44, prob = c(0.025, 0.5, 0.975))
# "41v44"
# -0.1597025
# 0.0732269
#         2.5%           50%         97.5%
# -2.752608e-01 -1.659695e-01 -3.509224e-05
print('42v43')
mean(contrast_42v43); sd(contrast_42v43)
quantile(contrast_42v43, prob = c(0.025, 0.5, 0.975))
# "42v43"
# 0.07110264
# 0.03328796
#         2.5%          50%        97.5%
# 1.537439e-05 7.319678e-02 1.249125e-01
print('42v44')
mean(contrast_42v44); sd(contrast_42v44)
quantile(contrast_42v44, prob = c(0.025, 0.5, 0.975))
# "42v44"
# -0.05497594
# 0.02426358
#         2.5%           50%         97.5%
# -9.213177e-02 -5.868879e-02 -6.565019e-06
print('43v44')
mean(contrast_43v44); sd(contrast_43v44)
quantile(contrast_43v44, prob = c(0.025, 0.5, 0.975))
# "43v44"
# -0.1260786
# 0.0555912
#         2.5%           50%         97.5%
# -2.042773e-01 -1.333537e-01 -2.211352e-05

## plot contrasts
(focal_plot <- contrasts_long %>%
    separate(combo1, remove = F, into = c('f1', 't1'), sep = 1) %>%
    separate(combo2, remove = F, into = c('f2', 't2'), sep = 1) %>%
    filter(t1 == t2) %>%
    mutate(contrast = paste0(combo1, ' -> ', combo2),
           cats_diff = as.factor(as.numeric(f2) - as.numeric(f1))) %>%
    mutate(contrast = factor(contrast,
                             levels = c('11 -> 21','13 -> 23','21 -> 31','24 -> 34',
                                        '11 -> 31','13 -> 33','21 -> 41','24 -> 44',
                                        '11 -> 41','13 -> 43','22 -> 32','31 -> 41',
                                        '12 -> 22','14 -> 24','22 -> 42','32 -> 42',
                                        '12 -> 32','14 -> 34','23 -> 33','33 -> 43',
                                        '12 -> 42','14 -> 44','23 -> 43','34 -> 44'
                             ))) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = cats_diff, fill = cats_diff),
                 alpha = 0.4)+
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_wrap(. ~ contrast, scales = 'free_y', ncol = 4)+
    #scale_x_continuous(limits = c(-0.02,0.02))+ # UNCOMMENT THIS ONE IN FINAL GRAPHS, BUT FIRST NEED TO SEE WHAT THE FULL RANGE IS BEFORE CONSTRAINING IT!
    labs(colour = 'difference in\nage category',
         fill = 'difference in\nage category',
         title = 'changing focal age',
         x = 'contrast'))
ggsave(plot = focal_plot, device = 'png',
       filename = 'nbm_noprev_focalage_contrasts.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       width = 2200, height = 2400, units = 'px')
ggsave(plot = focal_plot, device = 'svg',
       filename = 'nbm_noprev_focalage_contrasts.svg',
       path = '../outputs/neighbour_binomial_model_bda/',
       width = 2200, height = 2400, units = 'px')
(partner_plot <- contrasts_long %>%
    separate(combo1, remove = F, into = c('f1', 't1'), sep = 1) %>%
    separate(combo2, remove = F, into = c('f2', 't2'), sep = 1) %>%
    filter(f1 == f2) %>%
    mutate(contrast = paste0(combo1, ' -> ', combo2),
           cats_diff = as.factor(as.numeric(t2) - as.numeric(t1))) %>%
    mutate(contrast = factor(contrast,
                             levels = c('11 -> 12','21 -> 22','31 -> 32','41 -> 42',
                                        '11 -> 13','21 -> 23','31 -> 33','41 -> 43',
                                        '11 -> 14','21 -> 24','31 -> 34','41 -> 44',
                                        '12 -> 13','22 -> 23','32 -> 33','42 -> 43',
                                        '12 -> 14','22 -> 24','32 -> 34','42 -> 44',
                                        '13 -> 14','23 -> 24','33 -> 34','43 -> 44'))) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = cats_diff, fill = cats_diff),
                 alpha = 0.4)+
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    #scale_x_continuous(limits = c(-0.0125,0.025))+ # UNCOMMENT THIS ONE IN FINAL GRAPHS, BUT FIRST NEED TO SEE WHAT THE FULL RANGE IS BEFORE CONSTRAINING IT!
    facet_wrap(contrast ~ ., scales = 'free_y', ncol = 4)+
    labs(colour = 'difference in\nage category',
         fill = 'difference in\nage category',
         title = 'changing partner age',
         x = 'contrast'))
ggsave(plot = partner_plot, device = 'png',
       filename = 'nbm_noprev_partnerage_contrasts.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       width = 2200, height = 2400, units = 'px')
ggsave(plot = partner_plot, device = 'svg',
       filename = 'nbm_noprev_partnerage_contrasts.svg',
       path = '../outputs/neighbour_binomial_model_bda/',
       width = 2200, height = 2400, units = 'px')
# (focal_plot + partner_plot)+
#   plot_layout(guides = 'collect')+
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'nbm_noprev_age_contrasts.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        width = 4400, height = 2400, units = 'px')
# ggsave(plot = last_plot(), device = 'svg',
#        filename = 'nbm_noprev_age_contrasts.svg',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        width = 4400, height = 2400, units = 'px')

save.image('nearest_neighbour/neighbour_noprev_agecontrasts.RData')

# ############ movement proportion                                     ############
# pdf('../outputs/movement_binomial_model/movement_noprev_dataprep.pdf')
# set.seed(12345)
# rm(list = ls()[! ls() %in% c('behav','move','num_iter','num_chains')]) ; gc()
# move <- behav %>%
#   filter(activity == 'move')
# 
# #### filter data                                                     ####
# move_no_na <- move %>%
#   # remove out of sight observations
#   filter(action_index != 9) %>%
#   # convert to binary move or no move
#   mutate(move_index = ifelse(action_index == 0, 0, 1),
#          moving_direction = ifelse(action_name == 'not_moving',
#                                    'not_moving', 'moving')) %>%
#   # clean up
#   mutate(f_age_num = as.integer(f_age_num)) %>%
#   mutate(focal_id = as.integer(as.factor(focal)),
#          stim_num = as.integer(as.factor(stim_num))) %>%
#   rename(stim_id = stim_num,
#          playback_id = pb_num) %>%
#   select(focal, moving_direction, move_index,
#          f_age_cat, f_age_num,
#          stim_type, bda,
#          focal_id, stim_id, playback_id) %>%
#   filter(!is.na(f_age_num))
# str(move_no_na)
# 
# #### plot raw                                                        ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# ## plot overall
# move_no_na %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h')),
#          bda = factor(bda, levels = c('before','during','after'))) %>%
#   ggplot()+
#   geom_bar(aes(x = moving_direction,
#                fill = f_age_cat),
#            position = 'dodge')+
#   facet_grid(bda ~ stim_type,
#              labeller = labeller(stim_type = stim_labels),
#              scales = 'free_y')+
#   labs(fill = 'age category (years)',
#        y = 'seconds moving',
#        x = '')+
#   scale_fill_viridis_d()
# print(paste0('raw data plotted at ',Sys.time()))
# 
# ## reset plotting
# save.image('movement_direction/binomial_noprev/movement_noprev_run.RData')
# 
# #### set priors                                                      ####
# # set priors
# get_prior(formula = move_index ~ 0 + mo(f_age_num) + stim_type * bda +
#             (1|focal_id) + (1|stim_id) + (1|playback_id),
#           data = move_no_na,
#           family = bernoulli("logit"))
# #                prior class         coef       group resp dpar nlpar lb ub       source
# #               (flat)     b                                                     default
# #               (flat)     b    bdabefore                                   (vectorized)
# #               (flat)     b    bdaduring                                   (vectorized)
# #               (flat)     b  mof_age_num                                   (vectorized)
# #               (flat)     b stim_typectd                                   (vectorized)
# #               (flat)     b   stim_typeh                                   (vectorized)
# #               (flat)     b   stim_typel                                   (vectorized)
# # student_t(3, 0, 2.5)    sd                                           0         default
# # student_t(3, 0, 2.5)    sd                 focal_id                  0    (vectorized)
# # student_t(3, 0, 2.5)    sd    Intercept    focal_id                  0    (vectorized)
# # student_t(3, 0, 2.5)    sd              playback_id                  0    (vectorized)
# # student_t(3, 0, 2.5)    sd    Intercept playback_id                  0    (vectorized)
# # student_t(3, 0, 2.5)    sd                  stim_id                  0    (vectorized)
# # student_t(3, 0, 2.5)    sd    Intercept     stim_id                  0    (vectorized)
# #         dirichlet(1)  simo mof_age_num1                                        default
# 
# # set priors
# priors <- c(
#   # focal age
#   prior(normal(-1,1),      class = b,    coef = mof_age_num),
#   prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num1),
#   # stimulus type
#   prior(normal(-1,1),      class = b,    coef = stim_typectd),
#   prior(normal(-1,1),      class = b,    coef = stim_typel),
#   prior(normal(-1,1),      class = b,    coef = stim_typeh),
#   # time
#   prior(normal(-1,1),      class = b,    coef = bdabefore),
#   prior(normal(-1,1),      class = b,    coef = bdaduring)
# )
# 
# #### prior predictive check                                          ####
# prop_prior <- brm(
#   formula = move_index ~ 0 + mo(f_age_num) + stim_type * bda +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),
#   data = move_no_na,
#   family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(prop_prior)
# 
# print(paste0('priors set and checked at ', Sys.time()))
# 
# #### fit model                                                       ####
# prop_fit <- brm(
#   formula = move_index ~ 0 + mo(f_age_num) + stim_type * bda +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),
#   data = move_no_na,
#   family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   control = list(adapt_delta = 0.9))
# 
# # save workspace
# save.image('movement_direction/binomial_noprev/movement_noprev_run.RData')
# 
# # inspect model
# summary(prop_fit)
# 
# print(paste0('model fitted at ', Sys.time()))
# dev.off()
# 
# #### check outputs                                                   ####
# pdf('../outputs/movement_binomial_model/movement_noprev_modelchecks.pdf')
# load('movement_direction/binomial_noprev/movement_noprev_run.RData') # load('ele_playbacks/movement_direction/movement_noprev_run.RData')
# 
# ## check Stan code
# prop_fit$model
# prop_fit$formula
# 
# ## extract posterior distribution
# draws <- as_draws_df(prop_fit) %>%
#   select(-lprior, -`lp__`)
# parameters <- colnames(draws)[1:(ncol(draws)-3)]
# draws <- draws  %>%
#   pivot_longer(cols = all_of(parameters),
#                names_to = 'parameter',
#                values_to = 'draw') %>%
#   rename(chain = `.chain`,
#          position = `.iteration`,
#          draw_id = `.draw`) %>%
#   mutate(invlogit_draw = invlogit(draw))
# 
# print(paste0('posterior extracted at ',Sys.time()))
# 
# #### plot marginal effects                                           ####
# ## extract marginal effects
# marg <- conditional_effects(prop_fit,
#                             method = 'posterior_epred')
# names(marg)
# # "stim_type" "bda" "f_age_num"
# stim_effect <- marg[['stim_type']]
# time_effect <- marg[['bda']]
# agef_effect <- marg[['f_age_num']]
# 
# ## plot marginal effects
# (focal_age_plot <- ggplot(agef_effect)+
#     geom_errorbar(aes(x = f_age_num,
#                       ymax = upper__, ymin = lower__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = f_age_num,
#                    #colour = cats__,
#                    y = estimate__),
#                size = 3)+ # cex = 3?
#     xlab(label = 'focal age')+
#     ylab('probability of movement direction')+
#     theme(axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)))
# ggsave(plot = focal_age_plot, filename = '../outputs/movement_binomial_model/movement_noprev_marginaleffects_focalage.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# focal_age_labels <- c('focal age category 1',
#                       'focal age category 2',
#                       'focal age category 3',
#                       'focal age category 4')
# names(focal_age_labels) <- 1:4
# 
# (stim_plot <- ggplot(stim_effect)+
#     geom_errorbar(aes(x = stim_type,
#                       ymin = lower__, ymax = upper__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type,
#                    #colour = cats__,
#                    y = estimate__),
#                cex = 3)+ # size = 3?
#     xlab(label = 'stimulus type') + ylab('probability of movement')+
#     scale_x_discrete(breaks = c('ctd','l','h'),
#                      labels = c('dove (control)', 'lion', 'human'),
#                      limits = c('ctd','l','h'))+
#     theme(axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = stim_plot, filename = '../outputs/movement_binomial_model/movement_noprev_marginaleffects_stimtype_agecombo.png', device = 'png',
#        width = 8.3, height = 5.8)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check                                      ####
# pp_check(prop_fit, ndraws = 100)
# 
# #### plot traces and density curves                                  ####
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_stim_typectd","b_stim_typeh","b_stim_typel",
#                           "b_bdabefore","b_bdaduring",
#                           "bsp_mof_age_num",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "sigma"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none')
# 
# ## stim type
# dove <- draws_cut %>%
#   filter(parameter == 'b_stim_typectd') %>%
#   rename(dove = draw) %>%
#   select(-parameter)
# lion <- draws_cut %>%
#   filter(parameter == 'b_stim_typel') %>%
#   rename(lion = draw) %>%
#   select(-parameter)
# human <- draws_cut %>%
#   filter(parameter == 'b_stim_typeh') %>%
#   rename(human = draw) %>%
#   select(-parameter)
# 
# dove_lion <- dove %>%
#   left_join(lion, by = c('chain','position','draw_id')) %>%
#   mutate(difference = lion - dove)
# dove_human <- dove %>%
#   left_join(human, by = c('chain','position','draw_id')) %>%
#   mutate(difference = human - dove)
# lion_human <- lion %>%
#   left_join(human, by = c('chain','position','draw_id')) %>%
#   mutate(difference = human - lion)
# 
# par(mfrow = c(3,1))
# plot(density(dove_lion$difference), main = 'lion vs dove') ; abline(v = 0, lty = 2)
# plot(density(dove_human$difference), main = 'dove vs human') ; abline(v = 0, lty = 2)
# plot(density(lion_human$difference), main = 'lion vs human') ; abline(v = 0, lty = 2)
# par(mfrow = c(1,1))
# 
# ## focal age
# age1 <- draws_cut %>% filter(parameter == 'bsp_mof_age_num')
# age2 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[1]')
# age3 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[2]')
# age4 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[3]')
# par(mfrow = c(2,2))
# plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
# plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)
# 
# ## difference section
# section <- draws_cut %>% filter(parameter %in% c('b_bdabefore','b_bdaduring'))
# par(mfrow = c(2,1))
# plot(density(section$draw[section$parameter == 'b_bdabefore']),
#      main = 'before vs after slope', xlim = c(-1,0)) ; abline(v = 0, lty = 2)
# plot(density(section$draw[section$parameter == 'b_bdaduring']),
#      main = 'during vs after slope') ; abline(v = 0, lty = 2)
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# save.image('movement_direction/binomial_noprev/movement_noprev_run.RData')
# dev.off()
# 
# #### predict from model                                              ####
# load('movement_direction/binomial_noprev/movement_noprev_run.RData')
# rm(list = ls()[! ls() %in% c('prop_fit','move_no_na','behav','num_iter','num_chains')]) ; gc()
# 
# str(move_no_na)
# 
# pred <- posterior_epred(object = prop_fit,
#                         newdata = move_no_na) # including newdata = move_no_na was causing error of "Error in if (sum(invalid)) { : argument is not interpretable as logical". Without specifying, it's dropping ~4000 data rows -- RAW DATA CONTAINED A SET OF NA VALUES IN F_AGE_NUM FROM UNKNOWN AGE ELEPHANTS
# save.image('movement_direction/binomial_noprev/movement_noprev_predictions.RData')
# 
# ## convert to data frame
# move_no_na$data_row <- 1:nrow(move_no_na)
# pred <- as.data.frame(pred)
# colnames(pred) <- 1:nrow(move_no_na)
# pred <- pred %>%
#   pivot_longer(cols = everything(),
#                names_to = 'data_row', values_to = 'epred') %>%
#   mutate(data_row = as.integer(data_row)) %>%
#   left_join(move_no_na, by = 'data_row')
# 
# save.image('movement_direction/binomial_noprev/movement_noprev_predictions.RData')
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions                                                ####
# pdf('../outputs/movement_binomial_model/movement_noprev_modelpredictions.pdf')
# load('movement_direction/binomial_noprev/movement_noprev_predictions.RData')
# 
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# pred[1:(nrow(move_no_na)*1000),] %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
#   ggplot()+
#   geom_boxplot(aes(x = as.factor(f_age_num), y = epred,
#                    fill = bda)) +
#   facet_grid(. ~ stim_type,
#              labeller = labeller(stim_type = stim_labels))+
#   scale_fill_viridis_d()+
#   scale_colour_viridis_d()+
#   labs(fill = 'time relative to stimulus:',
#        x = 'age category of focal elephant',
#        y = 'predicted probability of moving')+
#   theme(legend.position = 'bottom')
# 
# pred[1:(nrow(move_no_na)*1000),] %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
#   ggplot()+
#   geom_density_ridges(aes(x = epred,
#                           y = f_age_cat,
#                           #colour = bda,
#                           #alpha = bda,
#                           #linetype = bda,
#                           fill = f_age_cat)
#   )+
#   labs(fill = 'focal age category',
#        x = 'predicted probability of moving',
#        y = 'probability density',
#        linetype = 'time relative\nto stimulus',
#        colour = 'time relative\nto stimulus',
#        alpha = 'time relative\nto stimulus')+
#   facet_grid(stim_type ~ bda,
#              scales = 'free_x',
#              labeller = labeller(stim_type = stim_labels))+
#   scale_fill_viridis_d()+
#   scale_colour_manual(values = c('transparent','black','black'),
#                       breaks = c('before','during','after'))+
#   scale_alpha_manual(values = c(0,0.6,0.6),
#                      breaks = c('before','during','after'))+
#   scale_linetype_manual(values = c(1,1,2),
#                         breaks = c('before','during','after'))+
#   theme(legend.position = 'bottom')
# ggsave(filename = 'prop_binomial_noprev_predicted_ridges.png',
#        path = '../outputs/movement_binomial_model/',
#        device = 'png', height = 1800, width = 1500, units = 'px')
# 
# ## plot against raw data
# pred[1:(nrow(move_no_na)*2),] %>%
#   # group_by(data_row) %>%
#   # mutate(epred = mean(epred)) %>%
#   # ungroup() %>%
#   group_by(focal, stim_type, bda) %>%
#   summarise(epred = mean(epred),
#             total_move = length(which(move_index == 1)),
#             total_still = length(which(move_index == 0)),
#             total_view = length(move_index),
#             prop_move = total_move / total_view) %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd','l','h')),
#          bda = factor(bda, levels = c('before','during','after'))) %>%
#   left_join(distinct(move_no_na[,c('focal','f_age_cat')]), by = 'focal') %>%
#   ggplot()+
#   geom_point(aes(x = prop_move,
#                  y = epred,
#                  colour = f_age_cat,
#                  size = total_view),
#              alpha = 0.5)+
#   facet_grid(stim_type ~ bda,
#              labeller = labeller(stim_type = stim_labels))+
#   scale_colour_viridis_d()+
#   geom_abline(slope = 1, intercept = 0)+
#   labs(colour = 'focal age category',
#        size = 'number of seconds visible',
#        x = 'proportion of time moving\n(NOT THE X VARIABLE FOR THE MODEL,\nBUT THE BEST OPTION FOR PLOTTING\nAGAINST PROBABILITY!)',
#        y = 'predicted probability of moving')
# 
# pred2 <- pred[1:(nrow(move_no_na)*1000),] %>%
#   #dplyr::select(-moving_direction,-time_since_stim,-after_stim) %>%
#   pivot_longer(cols = c(epred, move_index),
#                names_to = 'raw_pred', values_to = 'move') %>%
#   mutate(raw_pred = ifelse(raw_pred == 'epred', 'predicted', 'observed')) %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd','l','h')))
# pred2 %>%
#   ggplot()+
#   geom_density_ridges(aes(x = move,
#                           y = f_age_cat,
#                           fill = f_age_cat,
#                           colour = raw_pred,
#                           alpha = raw_pred)
#   )+
#   labs(fill = 'focal age category',
#        x = 'predicted probability of moving',
#        y = 'probability density',
#        colour = 'time relative\nto stimulus',
#        alpha = 'time relative\nto stimulus')+
#   facet_grid(bda ~ stim_type,
#              scales = 'free_x',
#              labeller = labeller(stim_type = stim_labels))+
#   scale_fill_viridis_d()+
#   scale_colour_manual(values = c('black','transparent'),
#                       breaks = c('predicted','observed'))+
#   scale_alpha_manual(values = c(0,0.6),
#                      breaks = c('predicted','observed'))+
#   theme(legend.position = 'bottom')
# ggsave(filename = 'predicted_vs_raw_ridges_noprev.png',
#        path = '../outputs/movement_binomial_model/',
#        device = 'png', height = 1800, width = 1500, units = 'px')
# 
# counts <- move_no_na %>%
#   group_by(f_age_cat, stim_type, bda, move_index) %>%
#   summarise(n = length(focal))
# counts$total <- NA
# for(i in 1:nrow(counts)){
#   counts$total[i] <- sum(counts$n[which(counts$f_age_cat == counts$f_age_cat[i] &
#                                           counts$stim_type == counts$stim_type[i] &
#                                           counts$bda == counts$bda[i])])
# }
# counts$prop <- counts$n / counts$total
# 
# props <- pred[1:(nrow(move_no_na)*1000),] %>%
#   group_by(f_age_cat, stim_type, bda, move_index) %>%
#   summarise(pred_mean = mean(epred))
# 
# counts <- counts %>%
#   left_join(props, by = c('f_age_cat', 'stim_type', 'bda', 'move_index'))
# 
# counts %>%
#   filter(move_index == 1) %>%
#   ggplot() +
#   geom_point(aes(x = prop, y = pred_mean, colour = bda, shape = f_age_cat))+
#   scale_colour_viridis_d()+
#   facet_grid(. ~ stim_type)+
#   geom_abline(slope = 1, intercept = 0)
# 
# ggplot()+
#   geom_col(data = counts,
#            aes(x = f_age_cat,
#                y = prop,
#                fill = as.factor(move_index)))+
#   geom_violin(data = pred[1:(nrow(move_no_na)*1000),],
#               aes(x = f_age_cat,
#                   y = epred),
#               fill = 'transparent')+
#   scale_fill_viridis_d(begin = 1, end = 0.5)+
#   facet_grid(bda ~ stim_type,
#              labeller = labeller(stim_type = stim_labels))+
#   labs(x = 'focal age category',
#        y = 'proportion of time spent moving',
#        fill = 'moving')
# 
# ggplot() +
#   geom_boxplot(data = counts[counts$bda == 'after',],
#                aes(x = prop, y = 5,
#                    fill = f_age_cat),
#                width = 5, notch = F) +
#   geom_density(data = pred[1:nrow(move_no_na),],
#                aes(x = epred), inherit.aes = FALSE) +
#   facet_grid(stim_type ~ f_age_cat,
#              labeller = labeller(stim_type = stim_labels)) +
#   scale_fill_viridis_d()
# 
# ggplot() +
#   geom_col(data = counts,
#            aes(x = move_index,
#                y = prop,
#                fill = f_age_cat),
#            width = 0.2, alpha = 0.6) +
#   geom_density(data = pred[1:nrow(move_no_na),],
#                aes(x = epred), inherit.aes = FALSE) +
#   facet_grid(stim_type ~ f_age_cat,
#              labeller = labeller(stim_type = stim_labels)) +
#   scale_fill_viridis_d()+
#   scale_x_continuous(name = 'probability of moving',
#                      limits = c(-0.1,1.1),
#                      breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))+
#   scale_y_continuous()
# 
# ## reset plotting
# dev.off()
# 
# #### calculate posterior contrasts from predictions                  ####
# pdf('../outputs/movement_binomial_model/movement_noprev_modelcontrasts.pdf')
# # load('movement_direction/binomial_noprev/movement_noprev_predictions.RData')
# 
# ## stim type                                                         ####
# move_new <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, bda,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, bda,focal_id, stim_id, playback_id))))
# 
# ## redo predictions with different stimulus types: all doves
# ctd_before <- move_new %>%
#   mutate(stim_type = 'ctd',
#          bda = 'before')
# ctd_before_mtx <- posterior_epred(object = prop_fit, newdata = ctd_before)
# colnames(ctd_before_mtx) <- ctd_before$unique_data_combo
# rows_to_include <- c(1:100,1001:1100,2001:2100,3001:3100)
# ctd_before_mtx <- ctd_before_mtx[rows_to_include,]
# 
# ctd_during <- move_new %>%
#   mutate(stim_type = 'ctd',
#          bda = 'during')
# ctd_during_mtx <- posterior_epred(object = prop_fit, newdata = ctd_during)
# colnames(ctd_during_mtx) <- ctd_during$unique_data_combo
# ctd_during_mtx <- ctd_during_mtx[rows_to_include,]
# 
# ctd_after <- move_new %>%
#   mutate(stim_type = 'ctd',
#          after_after = 'after')
# ctd_after_mtx <- posterior_epred(object = prop_fit, newdata = ctd_after)
# colnames(ctd_after_mtx) <- ctd_after$unique_data_combo
# ctd_after_mtx <- ctd_after_mtx[rows_to_include,]
# 
# ## redo predictions with different stimulus types: all lions
# lion_before <- move_new %>%
#   mutate(stim_type = 'l',
#          bda = 'before')
# lion_before_mtx <- posterior_epred(object = prop_fit, newdata = lion_before)
# colnames(lion_before_mtx) <- lion_before$unique_data_combo
# lion_before_mtx <- lion_before_mtx[rows_to_include,]
# 
# lion_during <- move_new %>%
#   mutate(stim_type = 'l',
#          bda = 'during')
# lion_during_mtx <- posterior_epred(object = prop_fit, newdata = lion_during)
# colnames(lion_during_mtx) <- lion_during$unique_data_combo
# lion_during_mtx <- lion_during_mtx[rows_to_include,]
# 
# lion_after <- move_new %>%
#   mutate(stim_type = 'l',
#          after_after = 'after')
# lion_after_mtx <- posterior_epred(object = prop_fit, newdata = lion_after)
# colnames(lion_after_mtx) <- lion_after$unique_data_combo
# lion_after_mtx <- lion_after_mtx[rows_to_include,]
# 
# ## redo predictions with different stimulus types: all humans
# human_before <- move_new %>%
#   mutate(stim_type = 'h',
#          bda = 'before')
# human_before_mtx <- posterior_epred(object = prop_fit, newdata = human_before)
# colnames(human_before_mtx) <- human_before$unique_data_combo
# human_before_mtx <- human_before_mtx[rows_to_include,]
# 
# human_during <- move_new %>%
#   mutate(stim_type = 'h',
#          bda = 'during')
# human_during_mtx <- posterior_epred(object = prop_fit, newdata = human_during)
# colnames(human_during_mtx) <- human_during$unique_data_combo
# human_during_mtx <- human_during_mtx[rows_to_include,]
# 
# human_after <- move_new %>%
#   mutate(stim_type = 'h',
#          after_after = 'after')
# human_after_mtx <- posterior_epred(object = prop_fit, newdata = human_after)
# colnames(human_after_mtx) <- human_after$unique_data_combo
# human_after_mtx <- human_after_mtx[rows_to_include,]
# 
# ## calculate contrasts
# ctd_vs_lion_before <- lion_before_mtx - ctd_before_mtx
# ctd_vs_human_before <- human_before_mtx - ctd_before_mtx
# lion_vs_human_before <- human_before_mtx - lion_before_mtx
# 
# ctd_vs_lion_during <- lion_during_mtx - ctd_during_mtx
# ctd_vs_human_during <- human_during_mtx - ctd_during_mtx
# lion_vs_human_during <- human_during_mtx - lion_during_mtx
# 
# ctd_vs_lion_after <- lion_after_mtx - ctd_after_mtx
# ctd_vs_human_after <- human_after_mtx - ctd_after_mtx
# lion_vs_human_after <- human_after_mtx - lion_after_mtx
# 
# ctd_before_vs_during <- ctd_during_mtx - ctd_before_mtx
# lion_before_vs_during <- lion_during_mtx - lion_before_mtx
# human_before_vs_during <- human_during_mtx - human_before_mtx
# 
# ctd_before_vs_after <- ctd_after_mtx - ctd_before_mtx
# lion_before_vs_after <- lion_after_mtx - lion_before_mtx
# human_before_vs_after <- human_after_mtx - human_before_mtx
# 
# ctd_during_vs_after <- ctd_after_mtx - ctd_during_mtx
# lion_during_vs_after <- lion_after_mtx - lion_during_mtx
# human_during_vs_after <- human_after_mtx - human_during_mtx
# 
# ## summarise contrasts
# contrasts <- move_no_na %>%
#   select(-stim_type) %>%
#   mutate(ctd_vs_lion_before_mu = apply(ctd_vs_lion_before, 2, mean),
#          ctd_vs_lion_before_sd = apply(ctd_vs_lion_before, 2, sd),
#          ctd_vs_human_before_mu = apply(ctd_vs_human_before, 2, mean),
#          ctd_vs_human_before_sd = apply(ctd_vs_human_before, 2, sd),
#          lion_vs_human_before_mu = apply(lion_vs_human_before, 2, mean),
#          lion_vs_human_before_sd = apply(lion_vs_human_before, 2, sd),
# 
#          ctd_vs_lion_during_mu = apply(ctd_vs_lion_during, 2, mean),
#          ctd_vs_lion_during_sd = apply(ctd_vs_lion_during, 2, sd),
#          ctd_vs_human_during_mu = apply(ctd_vs_human_during, 2, mean),
#          ctd_vs_human_during_sd = apply(ctd_vs_human_during, 2, sd),
#          lion_vs_human_during_mu = apply(lion_vs_human_during, 2, mean),
#          lion_vs_human_during_sd = apply(lion_vs_human_during, 2, sd),
# 
#          ctd_vs_lion_after_mu = apply(ctd_vs_lion_after, 2, mean),
#          ctd_vs_lion_after_sd = apply(ctd_vs_lion_after, 2, sd),
#          ctd_vs_human_after_mu = apply(ctd_vs_human_after, 2, mean),
#          ctd_vs_human_after_sd = apply(ctd_vs_human_after, 2, sd),
#          lion_vs_human_after_mu = apply(lion_vs_human_after, 2, mean),
#          lion_vs_human_after_sd = apply(lion_vs_human_after, 2, sd),
# 
#          ctd_before_vs_during_mu = apply(ctd_before_vs_during, 2, mean),
#          ctd_before_vs_during_sd = apply(ctd_before_vs_during, 2, sd),
#          lion_before_vs_during_mu = apply(lion_before_vs_during, 2, mean),
#          lion_before_vs_during_sd = apply(lion_before_vs_during, 2, sd),
#          human_before_vs_during_mu = apply(human_before_vs_during, 2, mean),
#          human_before_vs_during_sd = apply(human_before_vs_during, 2, sd),
# 
#          ctd_before_vs_after_mu = apply(ctd_before_vs_after, 2, mean),
#          ctd_before_vs_after_sd = apply(ctd_before_vs_after, 2, sd),
#          lion_before_vs_after_mu = apply(lion_before_vs_after, 2, mean),
#          lion_before_vs_after_sd = apply(lion_before_vs_after, 2, sd),
#          human_before_vs_after_mu = apply(human_before_vs_after, 2, mean),
#          human_before_vs_after_sd = apply(human_before_vs_after, 2, sd),
# 
#          ctd_during_vs_after_mu = apply(ctd_during_vs_after, 2, mean),
#          ctd_during_vs_after_sd = apply(ctd_during_vs_after, 2, sd),
#          lion_during_vs_after_mu = apply(lion_during_vs_after, 2, mean),
#          lion_during_vs_after_sd = apply(lion_during_vs_after, 2, sd),
#          human_during_vs_after_mu = apply(human_during_vs_after, 2, mean),
#          human_during_vs_after_sd = apply(human_during_vs_after, 2, sd),
#   )
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(ctd_vs_lion_before_mu, ctd_vs_human_before_mu, lion_vs_human_before_mu,
#                         ctd_vs_lion_during_mu, ctd_vs_human_during_mu, lion_vs_human_during_mu,
#                         ctd_vs_lion_after_mu, ctd_vs_human_after_mu, lion_vs_human_after_mu,
#                         ctd_before_vs_during_mu, lion_before_vs_during_mu, human_before_vs_during_mu,
#                         ctd_before_vs_after_mu, lion_before_vs_after_mu, human_before_vs_after_mu,
#                         ctd_during_vs_after_mu, lion_during_vs_after_mu, human_during_vs_after_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','mu'),
#            sep = -3, remove = T) %>%
#   select(-mu, -ctd_vs_lion_before_sd, -ctd_vs_human_before_sd, -lion_vs_human_before_sd,
#          ctd_vs_lion_during_sd, -ctd_vs_human_during_sd, -lion_vs_human_during_sd,
#          ctd_vs_lion_after_sd, -ctd_vs_human_after_sd, -lion_vs_human_after_sd,
#          ctd_before_vs_during_sd, -lion_before_vs_during_sd, -human_before_vs_during_sd,
#          ctd_before_vs_after_sd, -lion_before_vs_after_sd, -human_before_vs_after_sd,
#          ctd_during_vs_after_sd, -lion_during_vs_after_sd, -human_during_vs_after_sd)
# 
# ## save contrasts
# save.image('movement_direction/binomial_noprev/movement_noprev_stimuluscontrasts.RData')
# load('movement_direction/binomial_noprev/movement_noprev_stimuluscontrasts.RData')
# 
# ## produce values for reporting
# print('ctd_vs_lion_before')
# mean(ctd_vs_lion_before)   ; sd(ctd_vs_lion_before)
# quantile(ctd_vs_lion_before, prob = c(0.025, 0.5, 0.975))
# # "ctd_vs_lion_before"
# # 0.09997586
# # 0.112192
# #         2.5%          50%        97.5%
# # -0.002885296  0.062505956  0.402750958
# print('ctd_vs_human_before')
# mean(ctd_vs_human_before)  ; sd(ctd_vs_human_before)
# quantile(ctd_vs_human_before, prob = c(0.025, 0.5, 0.975))
# # "ctd_vs_human_before"
# # 0.1070293
# # 0.1112871
# #          2.5%           50%         97.5%
# # -0.0001412521  0.0754436061  0.3985617737
# print('lion_vs_human_before')
# mean(lion_vs_human_before) ; sd(lion_vs_human_before)
# quantile(lion_vs_human_before, prob = c(0.025, 0.5, 0.975))
# # "lion_vs_human_before"
# # 0.007053401
# # 0.0998597
# #        2.5%          50%        97.5%
# # -0.231873753  0.001865493  0.218730992
# 
# print('ctd_vs_lion_during')
# mean(ctd_vs_lion_during)   ; sd(ctd_vs_lion_during)
# quantile(ctd_vs_lion_during, prob = c(0.025, 0.5, 0.975))
# # "ctd_vs_lion_during"
# # 0.1610598
# # 0.1430077
# #         2.5%          50%        97.5%
# # -0.006989969  0.133194579  0.482573457
# print('ctd_vs_human_during')
# mean(ctd_vs_human_during)  ; sd(ctd_vs_human_during)
# quantile(ctd_vs_human_during, prob = c(0.025, 0.5, 0.975))
# # "ctd_vs_human_during"
# # 0.1744427
# # 0.1418212
# #          2.5%           50%         97.5%
# # -0.0003757976  0.1558683227  0.4762806373
# print('lion_vs_human_during')
# mean(lion_vs_human_during) ; sd(lion_vs_human_during)
# quantile(lion_vs_human_during, prob = c(0.025, 0.5, 0.975))
# # "lion_vs_human_during"
# # 0.01338294
# # 0.1294452
# #         2.5%          50%        97.5%
# # -0.277740066  0.004123949  0.273176854
# 
# print('ctd_vs_lion_after')
# mean(ctd_vs_lion_after)    ; sd(ctd_vs_lion_after)
# quantile(ctd_vs_lion_after, prob = c(0.025, 0.5, 0.975))
# # "ctd_vs_lion_after"
# # 0.1183215
# # 0.1234973
# #         2.5%          50%        97.5%
# # -0.003577652  0.080971172  0.433507130
# print('ctd_vs_human_after')
# mean(ctd_vs_human_after)   ; sd(ctd_vs_human_after)
# quantile(ctd_vs_human_after, prob = c(0.025, 0.5, 0.975))
# # "ctd_vs_human_after"
# # 0.1270625
# # 0.122473
# #          2.5%           50%         97.5%
# # -0.0002041351  0.0957623656  0.4285620372
# print('lion_vs_human_after')
# mean(lion_vs_human_after)  ; sd(lion_vs_human_after)
# quantile(lion_vs_human_after, prob = c(0.025, 0.5, 0.975))
# # "lion_vs_human_after"
# # 0.008740988
# # 0.1100775
# #         2.5%          50%        97.5%
# # -0.252259975  0.002421806  0.238769251
# 
# print('ctd_before_vs_during')
# mean(ctd_before_vs_during)  ; sd(ctd_before_vs_during)
# quantile(ctd_before_vs_during, prob = c(0.025, 0.5, 0.975))
# # "ctd_before_vs_during"
# # 0.1043842
# # 0.08828833
# #         2.5%          50%        97.5%
# # 0.0002734141 0.0818090154 0.2974717269
# print('lion_before_vs_during')
# mean(lion_before_vs_during) ; sd(lion_before_vs_during)
# quantile(lion_before_vs_during, prob = c(0.025, 0.5, 0.975))
# # "lion_before_vs_during"
# # 0.1654681
# # 0.1028053
# #         2.5%          50%        97.5%
# # 0.0007336299 0.1777316319 0.3044997908
# print('human_before_vs_during')
# mean(human_before_vs_during); sd(human_before_vs_during)
# quantile(human_before_vs_during, prob = c(0.025, 0.5, 0.975))
# # "human_before_vs_during"
# # 0.1717977
# # 0.1024429
# #         2.5%          50%        97.5%
# # 0.0008107847 0.1901273551 0.3049890262
# 
# print('ctd_before_vs_after')
# mean(ctd_before_vs_after)  ; sd(ctd_before_vs_after)
# quantile(ctd_before_vs_after, prob = c(0.025, 0.5, 0.975))
# # "ctd_before_vs_after"
# # 0.02914146
# # 0.05295342
# #      2.5%       50%     97.5%
# # 0.0000000 0.0000000 0.1749114
# print('lion_before_vs_after')
# mean(lion_before_vs_after) ; sd(lion_before_vs_after)
# quantile(lion_before_vs_after, prob = c(0.025, 0.5, 0.975))
# # "lion_before_vs_after"
# # 0.0474871
# # 0.07088116
# #      2.5%       50%     97.5%
# # 0.0000000 0.0000000 0.2370297
# print('human_before_vs_after')
# mean(human_before_vs_after); sd(human_before_vs_after)
# quantile(human_before_vs_after, prob = c(0.025, 0.5, 0.975))
# # "human_before_vs_after"
# # 0.04917469
# # 0.07205638
# #      2.5%       50%     97.5%
# # 0.0000000 0.0000000 0.2443526
# 
# print('ctd_during_vs_after')
# mean(ctd_during_vs_after)  ; sd(ctd_during_vs_after)
# quantile(ctd_during_vs_after, prob = c(0.025, 0.5, 0.975))
# # "ctd_during_vs_after"
# # -0.07524277
# # 0.07547698
# #        2.5%         50%       97.5%
# # -0.27613516 -0.05505516  0.00000000
# print('lion_during_vs_after')
# mean(lion_during_vs_after) ; sd(lion_during_vs_after)
# quantile(lion_during_vs_after, prob = c(0.025, 0.5, 0.975))
# # "lion_during_vs_after"
# # -0.117981
# # 0.09645446
# #       2.5%        50%      97.5%
# # -0.3010719 -0.1064389  0.0000000
# print('human_during_vs_after')
# mean(human_during_vs_after); sd(human_during_vs_after)
# quantile(human_during_vs_after, prob = c(0.025, 0.5, 0.975))
# # "human_during_vs_after"
# # -0.122623
# # 0.09785694
# #       2.5%        50%      97.5%
# # -0.3017001 -0.1124629  0.0000000
# 
# ## plot contrasts
# contrasts_long <- contrasts_long %>%
#   mutate(contrast_stim = case_when(contrast == 'ctd_vs_lion_before' ~ 'dove -> lion',
#                                    contrast == 'ctd_vs_human_before' ~ 'dove -> human',
#                                    contrast == 'lion_vs_human_before' ~ 'lion -> human',
# 
#                                    contrast == 'ctd_vs_lion_during' ~ 'dove -> lion',
#                                    contrast == 'ctd_vs_human_during' ~ 'dove -> human',
#                                    contrast == 'lion_vs_human_during' ~ 'lion -> human',
# 
#                                    contrast == 'ctd_vs_lion_after' ~ 'dove -> lion',
#                                    contrast == 'ctd_vs_human_after' ~ 'dove -> human',
#                                    contrast == 'lion_vs_human_after' ~ 'lion -> human',
# 
#                                    contrast == 'ctd_before_vs_during' ~ 'dove -> dove',
#                                    contrast == 'lion_before_vs_during' ~ 'lion -> lion',
#                                    contrast == 'human_before_vs_during' ~ 'human -> human',
# 
#                                    contrast == 'ctd_before_vs_after' ~ 'dove -> dove',
#                                    contrast == 'lion_before_vs_after' ~ 'lion -> lion',
#                                    contrast == 'human_before_vs_after' ~ 'human -> human',
# 
#                                    contrast == 'ctd_during_vs_after' ~ 'dove -> dove',
#                                    contrast == 'lion_during_vs_after' ~ 'lion -> lion',
#                                    contrast == 'human_during_vs_after' ~ 'human -> human'),
# 
#          contrast_sect = case_when(contrast == 'ctd_vs_lion_before' ~ 'before -> before',
#                                    contrast == 'ctd_vs_human_before' ~ 'before -> before',
#                                    contrast == 'lion_vs_human_before' ~ 'before -> before',
# 
#                                    contrast == 'ctd_vs_lion_during' ~ 'during -> during',
#                                    contrast == 'ctd_vs_human_during' ~ 'during -> during',
#                                    contrast == 'lion_vs_human_during' ~ 'during -> during',
# 
#                                    contrast == 'ctd_vs_lion_after' ~ 'after -> after',
#                                    contrast == 'ctd_vs_human_after' ~ 'after -> after',
#                                    contrast == 'lion_vs_human_after' ~ 'after -> after',
# 
#                                    contrast == 'ctd_before_vs_during' ~ 'before -> during',
#                                    contrast == 'lion_before_vs_during' ~ 'before -> during',
#                                    contrast == 'human_before_vs_during' ~ 'before -> during',
# 
#                                    contrast == 'ctd_before_vs_after' ~ 'before -> after',
#                                    contrast == 'lion_before_vs_after' ~ 'before -> after',
#                                    contrast == 'human_before_vs_after' ~ 'before -> after',
# 
#                                    contrast == 'ctd_during_vs_after' ~ 'during -> after',
#                                    contrast == 'lion_during_vs_after' ~ 'during -> after',
#                                    contrast == 'human_during_vs_after' ~ 'during -> after')
#   )
# 
# contrasts_long %>%
#   filter(contrast_stim %in% c('dove -> lion','dove -> human','lion -> human')) %>%
#   mutate(contrast_sect = ifelse(contrast_sect == 'before -> before', 'before',
#                                 ifelse(contrast_sect == 'during -> during', 'during', 'after'))) %>%
#   mutate(contrast_sect = factor(contrast_sect, levels = c('before','during','after'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast_stim, fill = contrast_stim),
#                alpha = 0.4)+
#   facet_wrap(f_age_cat ~ contrast_sect, scales = 'free_y')+
#   scale_colour_viridis_d()+
#   scale_fill_viridis_d()+
#   labs(colour = 'effect of changing stimulus',
#        fill = 'effect of changing stimulus')+
#   theme(legend.position = 'bottom')
# contrasts_long %>%
#   filter(contrast_stim %in% c('dove -> lion','dove -> human','lion -> human')) %>%
#   mutate(contrast_sect = ifelse(contrast_sect == 'before -> before', 'before',
#                                 ifelse(contrast_sect == 'during -> during', 'during', 'after'))) %>%
#   mutate(contrast_sect = factor(contrast_sect, levels = c('before','during','after'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = f_age_cat, fill = f_age_cat),
#                alpha = 0.4)+
#   facet_wrap(contrast_stim ~ contrast_sect, scales = 'free_y')+
#   scale_colour_viridis_d()+
#   scale_fill_viridis_d()+
#   labs(colour = 'effect of changing stimulus',
#        fill = 'effect of changing stimulus')+
#   theme(legend.position = 'bottom')
# 
# contrasts_long %>%
#   filter(contrast_sect %in% c('before -> during','before -> after','during -> after')) %>%
#   mutate(contrast_stim = ifelse(contrast_stim == 'dove -> dove', 'dove (control)',
#                                 ifelse(contrast_stim == 'lion -> lion', 'lion', 'human'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast_stim, fill = contrast_stim),
#                alpha = 0.4)+
#   facet_wrap(f_age_cat ~ contrast_sect, scales = 'free_y')+
#   scale_colour_viridis_d()+
#   scale_fill_viridis_d()+
#   labs(colour = 'effect of changing stimulus',
#        fill = 'effect of changing stimulus')+
#   theme(legend.position = 'bottom')
# contrasts_long %>%
#   filter(contrast_sect %in% c('before -> during','before -> after','during -> after')) %>%
#   mutate(contrast_stim = ifelse(contrast_stim == 'dove -> dove', 'dove (control)',
#                                 ifelse(contrast_stim == 'lion -> lion', 'lion', 'human'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = f_age_cat, fill = f_age_cat),
#                alpha = 0.4)+
#   facet_wrap(contrast_stim ~ contrast_sect, scales = 'free_y')+
#   scale_colour_viridis_d()+
#   scale_fill_viridis_d()+
#   labs(colour = 'effect of changing stimulus',
#        fill = 'effect of changing stimulus')+
#   theme(legend.position = 'bottom')
# 
# save.image('movement_direction/binomial_noprev/movement_noprev_stimuluscontrasts.RData')
# rm(ctd_vs_lion_before,ctd_vs_human_before,lion_vs_human_before,
#    ctd_vs_lion_after,ctd_vs_human_after,lion_vs_human_after,
#    ctd_before_vs_after,lion_before_vs_after,human_before_vs_after,
#    ctd_before,ctd_before_mtx,ctd_after,ctd_after_mtx,
#    lion_before,lion_before_mtx,lion_after,lion_after_mtx,
#    human_before,human_before_mtx,human_after,human_after_mtx) ; gc()
# 
# ## focal age                                                         ####
# load('movement_direction/binomial_noprev/movement_noprev_stimuluscontrasts.RData')
# move_new <- move_new %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(stim_type, bda,
#                                                          focal_id, stim_id, playback_id))))
# 
# ## predict with original ages
# age_move_org <- move_new
# age_mtx_org <- posterior_epred(object = prop_fit, newdata = age_move_org)
# colnames(age_mtx_org) <- age_move_org$unique_data_combo
# 
# ## redo predictions with altered ages
# age_move_alt <- move_new %>%
#   mutate(f_age_num_original = f_age_num) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
#   relocate(f_age_num_original)
# age_mtx_alt <- posterior_epred(object = prop_fit, newdata = age_move_alt)
# colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
# 
# save.image('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')
# # load('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')
# 
# ## calculate contrasts
# age_contrast <- age_mtx_alt - age_mtx_org
# 
# ## plot overall effect
# plot(density(age_contrast))
# 
# ## plot effect per category
# age1_vs_age2 <- age_contrast[,which(age_move_org$f_age_num == 1)]
# age2_vs_age3 <- age_contrast[,which(age_move_org$f_age_num == 2)]
# age3_vs_age4 <- age_contrast[,which(age_move_org$f_age_num == 3)]
# age1_vs_age4 <- age_contrast[,which(age_move_org$f_age_num == 4)]*-1
# 
# ## plot category effects
# plot(density(age1_vs_age2), col = 'blue',
#      xlim = c(-0.2,0), ylim = c(0,30), las = 1,
#      main = 'contrasts between age categories:\nblue = 1->2, red = 2->3,\ngreen = 3->4, purple = 1->4')
# lines(density(age2_vs_age3), col = 'red')
# lines(density(age3_vs_age4), col = 'green')
# lines(density(age1_vs_age4), col = 'purple')
# 
# ## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
# print('age1 vs age2')
# mean(age1_vs_age2) ; sd(age1_vs_age2) # -0.05072095 ;  0.06024068
# quantile(age1_vs_age2, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.2150803273 -0.0295625030 -0.0000226048
# print('age2 vs age3')
# mean(age2_vs_age3) ; sd(age2_vs_age3) # -0.03553748 ; 0.03827389
# quantile(age2_vs_age3, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.1371146152 -0.0224629018 -0.0000398404
# print('age3 vs age4')
# mean(age3_vs_age4) ; sd(age3_vs_age4) # -0.0437991 ; 0.04795357
# quantile(age3_vs_age4, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.1723784623 -0.0282496427 -0.0000334704
# print('age1 vs age4')
# mean(age1_vs_age4) ; sd(age1_vs_age4) # -0.1519467 ; 0.1330916
# quantile(age1_vs_age4, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.4551524263 -0.1241281505 -0.0001654837
# 
# ## save output
# save.image('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')   #load('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')
# 
# ## plot predictions
# pred <- pred %>%
#   mutate(draw_id = rep(1:4000, each = nrow(age_move_org)),
#          stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
#                                  ifelse(stim_type == 'l','lion','human'))) %>%
#   mutate(stim_type_long = factor(stim_type_long,
#                                  levels = c('dove (control)','lion','human')))
# 
# pred %>%
#   ggplot()+
#   geom_boxplot(aes(x = f_age_cat,
#                    fill = bda,
#                    y = epred))+
#   labs(x = 'focal age category',
#        y = 'predicted probability of moving',
#        fill = 'moving in previous second')+
#   facet_wrap(. ~ stim_type_long)+
#   scale_fill_viridis_d()+
#   theme(legend.position = 'bottom')
# print('plot1 done')
# 
# pred %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   ggplot()+
#   geom_density_ridges(aes(x = epred,
#                           y = f_age_cat,
#                           fill = f_age_cat),
#                       alpha = 0.4)+
#   labs(fill = 'focal age\ncategory',
#        x = 'predicted probability of moving',
#        y = 'probability density')+
#   facet_grid(bda ~ stim_type_long,
#              scales = 'free')+
#   scale_fill_viridis_d()+
#   theme(legend.position = 'bottom')
# print('plot2 done')
# 
# ## plot contrasts
# colnames(age_contrast) <- move_no_na$data_row
# plot_contrasts <- age_contrast %>%
#   as.data.frame() %>%
#   pivot_longer(cols = everything(),
#                names_to = 'data_row',
#                values_to = 'contrast') %>%
#   mutate(data_row = as.integer(data_row)) %>%
#   left_join(move_no_na, by = 'data_row') %>%
#   mutate(categories = factor(ifelse(f_age_num == 1,
#                                     "10-15 to 16-20",
#                                     ifelse(f_age_num == 2,
#                                            "16-20 to 21-25",
#                                            ifelse(f_age_num == 3,
#                                                   "21-25 to 26-35",
#                                                   "10-15 to 26-35"))),
#                              levels = c("10-15 to 16-20", "16-20 to 21-25",
#                                         "21-25 to 26-35","10-15 to 26-35"))) %>%
#   mutate(contrast = ifelse(f_age_num == 4,
#                            contrast * (-1), # age_contrast shows 4 -> 1 not 1-> 4
#                            contrast),
#          diff_cats = ifelse(f_age_num == 4,
#                             'youngest to oldest', 'increase by one'))
# ggplot(plot_contrasts)+
#   geom_density(aes(x = contrast,
#                    fill = diff_cats, # fill = f_age_cat,
#                    colour = diff_cats # colour = f_age_cat
#   ),
#   #fill = '#21918c', colour = '#21918c',
#   alpha = 0.4)+
#   scale_colour_viridis_d(begin = 0, end = 0.5)+
#   scale_fill_viridis_d(begin = 0, end = 0.5)+
#   facet_wrap(. ~ categories, scales = 'free_y')+
#   labs(x = 'contrast between age categories',
#        fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
#        colour = 'change in age\ncategory'  # colour = 'original\nage category'
#   )+
#   theme(legend.position = 'none')+ #c(0.8, 0.9))+
#   theme_bw()
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'prop_density_agecontrasts_noprev.png',
#        path = '../outputs/movement_binomial_model/',
#        width = 2400, height = 1800, unit = 'px')
# ggsave(plot = last_plot(), device = 'svg',
#        filename = 'prop_density_agecontrasts_noprev.svg',
#        path = '../outputs/movement_binomial_model/',
#        width = 2400, height = 1800, unit = 'px')
# print('plot3 done')
# 
# # ## replot contrasts and check that it matches -- 2 different scripts, wrote plots independently -- same data seems to produce different graphs
# # data <- age_contrast %>%
# #   as.data.frame() %>%
# #   pivot_longer(cols = everything(),
# #                names_to = 'unique_data_combo',
# #                values_to = 'contrast') %>%
# #   mutate(unique_data_combo = as.integer(unique_data_combo)) %>%
# #   left_join(distinct(age_move_org), by = 'unique_data_combo') %>%
# #   rename(f_age_num_org = f_age_num) %>%
# #   mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1, f_age_num_org + 1)) %>%
# #   mutate(f_age_cat_org = ifelse(f_age_num_org == 1, '10-15 yrs',
# #                                 ifelse(f_age_num_org == 2, '16-20 yrs',
# #                                        ifelse(f_age_num_org == 3, '21-25 yrs',
# #                                               ifelse(f_age_num_org == 4, '26-35 yrs',
# #                                                      '>36 yrs')))),
# #          f_age_cat_alt = ifelse(f_age_num_alt == 1, '10-15 yrs',
# #                                 ifelse(f_age_num_alt == 2, '16-20 yrs',
# #                                        ifelse(f_age_num_alt == 3, '21-25 yrs',
# #                                               ifelse(f_age_num_alt == 4, '26-35 yrs',
# #                                                      '>36 yrs')))),
# #          contrast = ifelse(f_age_num_org == 4,
# #                            contrast*(-1),
# #                            contrast)) %>%
# #   relocate(f_age_num_alt, .after = (f_age_num_org)) %>%
# #   relocate(f_age_cat_org, .after = (f_age_num_alt)) %>%
# #   relocate(f_age_cat_alt, .after = (f_age_cat_org)) %>%
# #   mutate(comparison = ifelse(f_age_num_org == 4,
# #                              paste0(f_age_cat_alt, ' to ', f_age_cat_org),
# #                              paste0(f_age_cat_org, ' to ', f_age_cat_alt))) %>%
# #   mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
# #                             ifelse(stim_type == 'l', 'lion',
# #                                    'human'))) %>%
# #   mutate(stim_type = factor(stim_type,
# #                             levels = c('dove (control)', 'lion', 'human'))) %>%
# #   mutate(comparison = factor(comparison,
# #                              levels = c('10-15 yrs to 16-20 yrs',
# #                                         '16-20 yrs to 21-25 yrs',
# #                                         '21-25 yrs to 26-35 yrs',
# #                                         '10-15 yrs to 26-35 yrs')))
# # saveRDS(data, '../data_processed/plot_contrasts_noprev_moveprob_checkdata.RDS') # data <- readRDS('../data_processed/plot_contrasts_noprev_moveprob_checkdata.RDS')
# 
# plot_contrasts %>%
#   mutate(stim_type = ifelse(stim_type == 'ctd','dove (control)',
#                             ifelse(stim_type == 'l','lion','human'))) %>%
#   mutate(bda = factor(bda, levels = c('before','during','after')),
#          stim_type = factor(stim_type, levels = c('dove (control)','lion','human'))) %>%
#   ggplot()+
#   geom_violin(aes(x = categories,
#                   y = contrast,
#                   fill = categories),
#               position = position_dodge(0.5))+
#   geom_hline(yintercept = 0, lty = 2)+
#   scale_fill_viridis_d()+
#   facet_grid(stim_type ~ bda)+
#   labs(fill = 'age comparison')+
#   theme(legend.position = 'bottom',
#         axis.text.x = element_text(angle = 90, vjust = 0.5))
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'prop_violin_agecontrasts_noprev.png',
#        path = '../outputs/movement_binomial_model/',
#        height = 1600, width = 1600, unit = 'px')
# print('plot4 done')
# 
# ### save and clean up
# save.image('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')
# dev.off()
# 
# ############ movement direction                                      ####
# rm(list = ls()[! ls() %in% c('behav','move','num_iter','num_chains')]) ; gc()
# set.seed(12345)
# pdf('../outputs/movement_ordinal_model_2bda/movement_noprev_2bda_modelprep.pdf')
# 
# #### create data                                                     ####
# ## remove observations where not moving or out of sight
# move <- move %>%
#   filter(action_name != 'not_moving') %>%
#   filter(action_name != 'out_of_sight')
# 
# ## rename columns
# move <- move %>%
#   rename(move_index = action_index,
#          moving_direction = action_name)
# 
# ## remove NA
# move <- move %>%
#   filter(!is.na(p_age_num)) %>%
#   filter(!is.na(f_age_num))
# 
# ## ensure correct data format
# move <- move %>%
#   mutate(f_age_num = as.integer(f_age_num))
# str(move)
# 
# #### set prior                                                       ####
# get_prior(formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda +
#             (1|focal) + (1|stim_num) + (1|pb_num),
#           data = move, family = cumulative("logit"))
# #                prior     class         coef    group resp dpar nlpar lb ub        source
# #               (flat)         b                                                   default
# #               (flat)         b age_combo1_2                                 (vectorized)
# #               (flat)         b age_combo1_3                                 (vectorized)
# #               (flat)         b age_combo1_4                                 (vectorized)
# #               (flat)         b age_combo2_1                                 (vectorized)
# #               (flat)         b age_combo2_2                                 (vectorized)
# #               (flat)         b age_combo2_3                                 (vectorized)
# #               (flat)         b age_combo2_4                                 (vectorized)
# #               (flat)         b age_combo3_1                                 (vectorized)
# #               (flat)         b age_combo3_2                                 (vectorized)
# #               (flat)         b age_combo3_3                                 (vectorized)
# #               (flat)         b age_combo3_4                                 (vectorized)
# #               (flat)         b age_combo4_1                                 (vectorized)
# #               (flat)         b age_combo4_2                                 (vectorized)
# #               (flat)         b age_combo4_3                                 (vectorized)
# #               (flat)         b age_combo4_4                                 (vectorized)
# #               (flat)         b    bdabefore                                 (vectorized)
# #               (flat)         b    bdaduring                                 (vectorized)
# #               (flat)         b  mof_age_num                                 (vectorized)
# #               (flat)         b   stim_typeh                                 (vectorized)
# #               (flat)         b   stim_typel                                 (vectorized)
# # student_t(3, 0, 2.5) Intercept                                                   default
# # student_t(3, 0, 2.5) Intercept            1                                 (vectorized)
# # student_t(3, 0, 2.5) Intercept            2                                 (vectorized)
# # student_t(3, 0, 2.5) Intercept            3                                 (vectorized)
# # student_t(3, 0, 2.5) Intercept            4                                 (vectorized)
# # student_t(3, 0, 2.5)        sd                                        0          default
# # student_t(3, 0, 2.5)        sd                 focal                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd    Intercept    focal                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd                pb_num                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd    Intercept   pb_num                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd              stim_num                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd    Intercept stim_num                  0     (vectorized)
# #         dirichlet(1)      simo mof_age_num1                                      default
# 
# priors <- c(
#   # focal age
#   prior(normal(0,1),      class = b,    coef = mof_age_num),
#   prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
#   # interaction
#   prior(normal(0,1),      class = b,    coef = age_combo1_2),
#   prior(normal(0,1),      class = b,    coef = age_combo1_3),
#   prior(normal(0,1),      class = b,    coef = age_combo1_4),
#   prior(normal(0,1),      class = b,    coef = age_combo2_1),
#   prior(normal(0,1),      class = b,    coef = age_combo2_2),
#   prior(normal(0,1),      class = b,    coef = age_combo2_3),
#   prior(normal(0,1),      class = b,    coef = age_combo2_4),
#   prior(normal(0,1),      class = b,    coef = age_combo3_1),
#   prior(normal(0,1),      class = b,    coef = age_combo3_2),
#   prior(normal(0,1),      class = b,    coef = age_combo3_3),
#   prior(normal(0,1),      class = b,    coef = age_combo3_4),
#   prior(normal(0,1),      class = b,    coef = age_combo4_1),
#   prior(normal(0,1),      class = b,    coef = age_combo4_2),
#   prior(normal(0,1),      class = b,    coef = age_combo4_3),
#   prior(normal(0,1),      class = b,    coef = age_combo4_4),
#   # stim type
#   prior(normal(0,1),      class = b,    coef = stim_typeh),
#   prior(normal(0,1),      class = b,    coef = stim_typel),
#   # before/during/after
#   prior(normal(0,1),      class = b,    coef = bdabefore),
#   prior(normal(0,1),      class = b,    coef = bdaduring))
# 
# ## prior predictive check
# mom2_prior <- brm(
#   formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = move, family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(mom2_prior) # huge variation in prior, but fairly on both sides so good
# 
# ## reset plotting
# dev.off()
# pdf('../outputs/movement_ordinal_model_2bda/movement_noprev_2bda_modelchecks.pdf')
# 
# #### fit model                                                       ####
# mom2_fit <- brm(
#   formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = move, family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')
# 
# #### extract draws                                                   ####
# # load('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')
# 
# ## check model diagnostics -- moves very good
# (summary <- summary(mom2_fit))
# # Family: cumulative
# # Links: mu = logit; disc = identity
# # Formula: move_index ~ mo(f_age_num) + age_combo + stim_type + bda + (1 |+ (1 | stim_num) + (1 | pb_num)
# # Data: move (Number of observations: 31907)
# # Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; total post-warmup draws = 4000
# #
# # Group-Level Effects:
# # ~focal (Number of levels: 155)
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     1.47      0.10     1.29     1.68 1.00     1034     1706
# #
# # ~pb_num (Number of levels: 45)
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     0.15      0.11     0.01     0.41 1.01      524      948
# #
# # ~stim_num (Number of levels: 29)
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     0.15      0.12     0.01     0.46 1.01      553      977
# #
# # Population-Level Effects:
# #              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # Intercept[1]    -0.75      0.42    -1.56     0.06 1.00     1150     1513
# # Intercept[2]     0.26      0.42    -0.56     1.07 1.00     1150     1490
# # Intercept[3]     0.56      0.42    -0.26     1.37 1.00     1150     1445
# # Intercept[4]     1.53      0.42     0.70     2.33 1.00     1151     1439
# # age_combo1_2     0.03      0.11    -0.18     0.24 1.00     5245     3367
# # age_combo1_3     0.22      0.12    -0.03     0.47 1.00     4720     3359
# # age_combo1_4     0.20      0.14    -0.08     0.48 1.00     5377     3460
# # age_combo2_1     0.19      0.32    -0.44     0.83 1.00     1104     1373
# # age_combo2_2    -0.09      0.31    -0.71     0.56 1.00     1090     1412
# # age_combo2_3     0.49      0.31    -0.14     1.13 1.00     1086     1492
# # age_combo2_4    -0.63      0.32    -1.26     0.00 1.00     1099     1454
# # age_combo3_1     0.12      0.33    -0.52     0.76 1.01     1067     1863
# # age_combo3_2    -0.04      0.32    -0.66     0.57 1.01     1056     1912
# # age_combo3_3     0.16      0.32    -0.46     0.78 1.01     1070     2021
# # age_combo3_4    -1.23      0.32    -1.87    -0.63 1.01     1050     2002
# # age_combo4_1     0.30      0.43    -0.52     1.14 1.00     1542     2464
# # age_combo4_2     0.35      0.41    -0.45     1.16 1.00     1462     2267
# # age_combo4_3     0.50      0.41    -0.30     1.31 1.00     1472     2286
# # age_combo4_4    -0.44      0.42    -1.26     0.37 1.00     1516     2414
# # stim_typeh       0.13      0.28    -0.40     0.68 1.00      674     1734
# # stim_typel       0.41      0.34    -0.28     1.05 1.00      887     1494
# # bdabefore       -0.02      0.03    -0.07     0.03 1.00     9635     3174
# # bdaduring       -0.05      0.04    -0.12     0.02 1.00     9795     3346
# # mof_age_num     -0.05      0.22    -0.46     0.39 1.00     1149     1838
# #
# # Simplex Parameters:
# #                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # mof_age_num1[1]     0.33      0.18     0.05     0.73 1.00     7192     3124
# # mof_age_num1[2]     0.32      0.17     0.05     0.70 1.00     6703     2800
# # mof_age_num1[3]     0.34      0.18     0.06     0.72 1.00     5885     2947
# #
# # Family Specific Parameters:
# #      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # disc     1.00      0.00     1.00     1.00   NA       NA       NA
# #
# # Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# 
# par(mfrow = c(3,1))
# hist(summary$fixed$Rhat, breaks = 50)
# hist(summary$fixed$Bulk_ESS, breaks = 50)
# hist(summary$fixed$Tail_ESS, breaks = 50)
# par(mfrow = c(1,1))
# 
# ## extract posterior distribution
# draws <- as_draws_df(mom2_fit) %>%
#   select(-lprior, -`lp__`)
# parameters <- colnames(draws)[1:(ncol(draws)-3)]
# draws <- draws  %>%
#   pivot_longer(cols = all_of(parameters),
#                names_to = 'parameter',
#                values_to = 'draw') %>%
#   rename(chain = `.chain`,
#          position = `.iteration`,
#          draw_id = `.draw`) %>%
#   mutate(invlogit_draw = invlogit(draw))
# 
# ## extract marginal effects
# marg <- conditional_effects(mom2_fit,
#                             effects = c('f_age_num','age_combo','stim_type',
#                                         'bda'),
#                             categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg) # "f_age_num:cats__" "age_combo:cats__" "stim_type:cats__" "bda:cats__"
# agefocal_effect <- marg[[1]]
# agecombo_effect <- marg[[2]]
# stim_effect <- marg[[3]]
# bda_effect <- marg[[4]]
# 
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,2))
# hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
# par(mfrow = c(1,1))
# 
# #### plot marginal effects                                           ####
# (f_age_num_plot <- ggplot(agefocal_effect)+
#    geom_errorbar(aes(x = f_age_num,
#                      ymax = upper__, ymin = lower__,
#                      colour = cats__),
#                  linewidth = 1, width = 0.2)+
#    geom_point(aes(x = f_age_num,
#                   y = estimate__,
#                   colour = cats__),
#               size = 3)+ # cex = 3?
#    xlab(label = 'focal age')+
#    ylab('probability of moving direction')+
#    scale_colour_viridis_d(name = 'moving direction:',
#                           breaks = c('1','2','3','4','5'),
#                           labels = c('move away directly',
#                                      'move away at an angle',
#                                      'move neither towards or away',
#                                      'approach at an angle',
#                                      'approach directly'))+
#    scale_fill_viridis_d(name = 'moving direction:',
#                         breaks = c('1','2','3','4','5'),
#                         labels = c('move away directly',
#                                    'move away at an angle',
#                                    'move neither towards or away',
#                                    'approach at an angle',
#                                    'approach directly'))+
#    theme(legend.position = 'bottom',
#          axis.title = element_text(size = 16),
#          axis.text = element_text(size = 12),
#          legend.title = element_text(size = 12),
#          legend.text = element_text(size = 10)))
# ggsave(plot = f_age_num_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_noprev_2bda_marginaleffects_agefocal.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# f_age_num_labels <- c('focal age category 1',
#                       'focal age category 2',
#                       'focal age category 3',
#                       'focal age category 4')
# names(f_age_num_labels) <- 1:4
# (agecombo_plot <- agecombo_effect %>%
#     separate(col = age_combo, sep = '_', remove = F,
#              into = c('f_age_num','partner_age')) %>%
#     mutate(agecombo = paste0(f_age_num,'-',partner_age)) %>%
#     ggplot()+
#     geom_errorbar(aes(#x = agecombo,
#       x = partner_age,
#       colour = as.factor(cats__), # moving direction?
#       ymax = upper__, ymin = lower__),
#       linewidth = 1,
#       width = 0.4)+
#     geom_point(aes(#x = agecombo,
#       x = partner_age,
#       colour = as.factor(cats__),    # moving direction?
#       y = estimate__),
#       size = 3)+
#     facet_wrap(. ~ f_age_num,
#                labeller = labeller(f_age_num = f_age_num_labels))+
#     ylab('probability of moving direction')+
#     scale_colour_viridis_d(name = 'moving direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'move neither towards or away',
#                                       'approach at an angle',
#                                       'approach directly'))+
#     scale_x_discrete(name = 'partner age category')+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = agecombo_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_noprev_2bda_marginaleffects_agepartner.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# (stim_plot <- ggplot(stim_effect)+
#     geom_errorbar(aes(x = stim_type,
#                       ymin = lower__, ymax = upper__,
#                       colour = cats__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type,
#                    y = estimate__,
#                    colour = cats__),
#                cex = 3)+ # size = 3?
#     xlab(label = 'stimulus type') + ylab('probability of moving direction')+
#     scale_colour_viridis_d(name = 'moving direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'move neither towards or away',
#                                       'approach at an angle',
#                                       'approach directly'))+
#     scale_x_discrete(breaks = c('ctd','l','h'),
#                      labels = c('dove (control)', 'lion', 'human'),
#                      limits = c('ctd','l','h'))+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = stim_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_noprev_2bda_marginaleffects_stimtype.png',
#        device = 'png', width = 8.3, height = 5.8)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check                                      ####
# pp_check(mom2_fit, ndraws = 100) # really good fit
# 
# #### plot traces and density curves                                  ####
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
#                           "b_stim_typeh","b_stim_typel",
#                           "b_bdabefore","b_bdaduring",
#                           "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
#                           "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
#                           "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
#                           "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "bsp_mof_age_num",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none')
# 
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,1))
# hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
# hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')
# 
# ## stim type
# lion <- draws_cut %>% filter(parameter == 'b_stim_typel')
# human <- draws_cut %>% filter(parameter == 'b_stim_typeh')
# plot(density(lion$draw), main = 'lion vs dove') ; abline(v = 0, lty = 2)
# plot(density(human$draw), main = 'human vs dove') ; abline(v = 0, lty = 2)
# 
# ## focal age
# age1 <- draws_cut %>% filter(parameter == 'bsp_mof_age_num')
# age2 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[1]')
# age3 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[2]')
# age4 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[3]')
# par(mfrow = c(2,2))
# plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
# plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# 
# #### plot raw                                                        ####
# ## define labels for plotting
# f_age_labels <- c('F: 10-15 years','F: 16-20 years','F: 21-25 years','F: 26-35 years')
# names(f_age_labels) <- c(1,2,3,4)
# p_age_labels <- c('T: 10-15 years','T: 16-20 years','T: 21-25 years','T: 26-35 years')
# names(p_age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# ## plot control data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(p_age_num ~ f_age_num,
#              labeller = labeller(f_age_num = f_age_labels,
#                                  p_age_num = p_age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')+
#   labs(title = 'cape turtle dove (control)')
# 
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(p_age_num ~ f_age_num,
#              labeller = labeller(f_age_num = f_age_labels,
#                                  p_age_num = p_age_labels),
#              drop = F)+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
#   labs(title = 'cape turtle dove (control)')
# 
# ## plot lion data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(p_age_num ~ f_age_num,
#              labeller = labeller(f_age_num = f_age_labels,
#                                  p_age_num = p_age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')+
#   labs(title = 'lion')
# 
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(p_age_num ~ f_age_num,
#              labeller = labeller(f_age_num = f_age_labels,
#                                  p_age_num = p_age_labels),
#              drop = F)+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
#   labs(title = 'lion')
# 
# ## plot human data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(p_age_num ~ f_age_num,
#              labeller = labeller(f_age_num = f_age_labels,
#                                  p_age_num = p_age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')+
#   labs(title = 'human')
# 
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(p_age_num ~ f_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels),
#              drop = F)+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
#   labs(title = 'human')
# 
# print(paste0('raw data plotted at ',Sys.time()))
# 
# ## reset plotting
# save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')
# dev.off()
# 
# #### predict from model                                              ####
# # load('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')
# rm(list = ls()[! ls() %in% c('mom2_fit','move','behav','num_chains','num_iter')]) ; gc()
# pdf('../outputs/movement_ordinal_model_2bda/moving_noprev_2bda_modelpredictions.pdf')
# 
# pred <- posterior_epred(object = mom2_fit,
#                         newdata = move)
# save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')
# 
# ## convert to data frame
# move$data_row <- 1:nrow(move)
# extract_predictions <- function(prediction_array, layer, df){
#   predictions <- as.data.frame(prediction_array[,,layer])
#   colnames(predictions) <- 1:nrow(df)
#   predictions <- predictions %>%
#     pivot_longer(cols = everything(),
#                  names_to = 'data_row', values_to = 'epred') %>%
#     mutate(data_row = as.integer(data_row)) %>%
#     left_join(df, by = 'data_row') %>%
#     mutate(pred_type = ifelse(layer == 1, 'move directly away',
#                               ifelse(layer == 2, 'move away at an angle',
#                                      ifelse(layer == 3, 'move neither towards or away',
#                                             ifelse(layer == 4, 'approach at an angle',
#                                                    ifelse(layer == 5, 'approach directly',
#                                                           'CHECK -- PROBLEM IN DATA'))))),
#            pred_type_num = layer)
#   return(predictions)
# }
# pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = move)
# pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = move)
# pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = move)
# pred4 <- extract_predictions(prediction_array = pred, layer = 4, df = move)
# pred5 <- extract_predictions(prediction_array = pred, layer = 5, df = move)
# 
# saveRDS(pred1, '../data_processed/move_dir_noprev_predictions_awaydirect.RDS')
# saveRDS(pred2, '../data_processed/move_dir_noprev_predictions_awayangle.RDS')
# saveRDS(pred3, '../data_processed/move_dir_noprev_predictions_neitherdir.RDS')
# saveRDS(pred4, '../data_processed/move_dir_noprev_predictions_towardangle.RDS')
# saveRDS(pred5, '../data_processed/move_dir_noprev_predictions_towarddirect.RDS')
# 
# pred <- rbind(pred1, pred2, pred3, pred4, pred5)
# saveRDS(pred, '../data_processed/move_dir_noprev_predictions.RDS')
# 
# rm(pred1, pred2, pred3, pred4, pred5) ; gc()
# save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions                                                ####
# # load('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')
# pred %>%
#   mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                             ifelse(stim_type == 'l', 'lion','human'))) %>%
#   mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
#          bda = factor(bda, levels = c('before','during','after'))) %>%
#   ggplot()+
#   geom_violin(aes(x = f_age_cat, y = epred,
#                   fill = factor(pred_type, levels = c('move directly away',
#                                                       'move away at an angle',
#                                                       'move neither towards or away',
#                                                       'approach at an angle',
#                                                       'approach directly')),
#                   colour = factor(pred_type, levels = c('move directly away',
#                                                         'move away at an angle',
#                                                         'move neither towards or away',
#                                                         'approach at an angle',
#                                                         'approach directly'))
#   )) +
#   facet_grid(stim_type ~ bda)+
#   scale_fill_viridis_d()+
#   scale_colour_viridis_d()+
#   labs(colour = 'predicted direction of movement relative to focal:',
#        fill = 'predicted direction of movement relative to focal:',
#        x = 'age category of focal elephant',
#        y = 'proportion of predictions')
# 
# ## reset plotting
# dev.off()
# 
# #### calculate posterior contrasts from predictions                  ####
# load('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')
# pdf('../outputs/movement_ordinal_model_2bda/movement_noprev_model2_modelcontrasts.pdf')
# 
# ## stim type                                                         ####
# stim_new <- move %>%
#   dplyr::select(f_age_num, age_combo, stim_type, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, bda,
#                                                          focal, stim_num, pb_num))))
# 
# ## redo predictions with different stimulus types: all doves
# ctd_move <- stim_new %>%
#   mutate(stim_type = 'ctd')
# ctd_mtx <- posterior_epred(object = mom2_fit, newdata = ctd_move)
# colnames(ctd_mtx) <- ctd_move$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all lions
# lion_move <- stim_new %>%
#   mutate(stim_type = 'l')
# lion_mtx <- posterior_epred(object = mom2_fit, newdata = lion_move)
# colnames(lion_mtx) <- lion_move$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all humans
# human_move <- stim_new %>%
#   mutate(stim_type = 'h')
# human_mtx <- posterior_epred(object = mom2_fit, newdata = human_move)
# colnames(human_mtx) <- human_move$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_stimuluscontrasts.RData')
# 
# ## calculate contrasts
# ctd_vs_lion <- lion_mtx - ctd_mtx
# ctd_vs_human <- human_mtx - ctd_mtx
# lion_vs_human <- human_mtx - lion_mtx
# 
# ## summarise contrasts
# contrasts <- move %>%
#   select(-stim_type) %>%
#   mutate(ctd_vs_lion_mu = apply(ctd_vs_lion, 2, mean),
#          ctd_vs_lion_sd = apply(ctd_vs_lion, 2, sd),
#          ctd_vs_human_mu = apply(ctd_vs_human, 2, mean),
#          ctd_vs_human_sd = apply(ctd_vs_human, 2, sd),
#          lion_vs_human_mu = apply(lion_vs_human, 2, mean),
#          lion_vs_human_sd = apply(lion_vs_human, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(ctd_vs_lion_mu, ctd_vs_human_mu, lion_vs_human_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','mu'),
#            sep = -3, remove = T) %>%
#   select(-mu, -ctd_vs_lion_sd, -ctd_vs_human_sd, -lion_vs_human_sd)
# 
# ## produce values for reporting
# print('dove vs lion')
# median(ctd_vs_lion)  ; mean(ctd_vs_lion)  ; sd(ctd_vs_lion)
# # 0.0002728995 ; 5.993599e-18 ; 0.06148847
# quantile(ctd_vs_lion, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.1403551551  0.0002728995  0.1430214731
# 
# print('dove vs human')
# median(ctd_vs_human) ; mean(ctd_vs_human) ; sd(ctd_vs_human)
# # 0.0002103324 ; 3.418759e-17 ; 0.03647513
# quantile(ctd_vs_human, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.0880580932  0.0002103324  0.0820228850
# 
# print('lion vs human')
# median(lion_vs_human); mean(lion_vs_human); sd(lion_vs_human)
# # 2.206352e-05 ; -8.98969e-18 ; 0.05611132
# quantile(lion_vs_human, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -1.339580e-01  2.206352e-05  1.262522e-01
# 
# ## plot contrasts
# contrasts_long %>%
#   mutate(contrast = ifelse(contrast == 'ctd_vs_human',
#                            'dove -> human',
#                            ifelse(contrast == 'ctd_vs_lion',
#                                   'dove -> lion', 'lion -> human')),
#          bda = factor(bda, levels = c('before','during','after'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast))+
#   facet_grid(f_age_cat ~ bda)+
#   scale_colour_viridis_d()+
#   labs(colour = 'effect of changing stimulus')
# 
# save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_stimuluscontrasts.RData')
# 
# ## focal age                                                         ####
# # load('movement_direction/ordinal_noprev/moving_noprev_2bda_stimuluscontrasts.RData')
# rm(list = ls()[!ls() %in% c('mom2_fit','move','behav','num_iter','num_chains')]) ; gc()
# 
# ## create new dataframe to predict from
# age_new <- move %>%
#   dplyr::select(f_age_num, age_combo, stim_type, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, bda,
#                                                          focal, stim_num, pb_num))))
# 
# ## predict with original ages
# age_move_org <- age_new
# age_mtx_org <- posterior_epred(object = mom2_fit, newdata = age_move_org)
# colnames(age_mtx_org) <- age_move_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with altered ages
# age_move_alt <- age_new %>%
#   mutate(f_age_num_original = f_age_num,
#          age_combo_original = age_combo) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
#   separate(age_combo, into = c('f_age_old','p_age'), sep = '_') %>%
#   mutate(age_combo = paste0(f_age_num, '_', p_age)) %>%
#   dplyr::select(f_age_num_original, f_age_num,
#                 age_combo_original, age_combo,
#                 stim_type, bda,
#                 focal, stim_num, pb_num,
#                 unique_data_combo)
# age_mtx_alt <- posterior_epred(object = mom2_fit, newdata = age_move_alt)
# colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_agecontrasts.RData')
# 
# ## summarise and convert to long format
# # rm(list = ls()) ; gc() ; load('movement_direction/ordinal_noprev/moving_noprev_2bda_agecontrasts.RData')
# age_pred <- age_move_org %>%
#   #dplyr::select(-f_age_num) %>%
#   mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
#          age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
#          age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
#          age_org_prop4_mu = apply(age_mtx_org[,,4], 2, mean),
#          age_org_prop5_mu = apply(age_mtx_org[,,5], 2, mean),
#          age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
#          age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
#          age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
#          age_org_prop4_sd = apply(age_mtx_org[,,4], 2, sd),
#          age_org_prop5_sd = apply(age_mtx_org[,,5], 2, sd),
#          age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
#          age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
#          age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
#          age_alt_prop4_mu = apply(age_mtx_alt[,,4], 2, mean),
#          age_alt_prop5_mu = apply(age_mtx_alt[,,5], 2, mean),
#          age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
#          age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
#          age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd),
#          age_alt_prop4_sd = apply(age_mtx_alt[,,4], 2, sd),
#          age_alt_prop5_sd = apply(age_mtx_alt[,,5], 2, sd)) %>%
#   pivot_longer(cols = c(age_org_prop1_mu, age_org_prop2_mu, age_org_prop3_mu,
#                         age_org_prop4_mu, age_org_prop5_mu,
#                         age_alt_prop1_mu, age_alt_prop2_mu, age_alt_prop3_mu,
#                         age_alt_prop4_mu, age_alt_prop5_mu),
#                names_to = 'focal_agemove_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(age_org_prop1_sd, age_org_prop2_sd, age_org_prop3_sd,
#                         age_org_prop4_sd, age_org_prop5_sd,
#                         age_alt_prop1_sd, age_alt_prop2_sd, age_alt_prop3_sd,
#                         age_alt_prop4_sd, age_alt_prop5_sd),
#                names_to = 'focal_agemove_sd', values_to = 'stdv_propn') %>%
#   separate(col = focal_agemove_mu, into = c('focal_agemove_mu','mu'),
#            sep = '_m', remove = T) %>%
#   separate(col = focal_agemove_sd, into = c('focal_agemove_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-mu, -sd) %>%
#   filter(focal_agemove_mu == focal_agemove_sd) %>%
#   separate(col = focal_agemove_mu, into = c('original_altered', 'move_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-focal_agemove_sd) %>%
#   mutate(move_pred = as.numeric(move_pred),
#          f_age_num = ifelse(original_altered == 'age_org',
#                             f_age_num,
#                             ifelse(original_altered == 'age_alt' & f_age_num == 4,
#                                    1, f_age_num + 1))) %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'move away directly',
#                             ifelse(move_pred == 2, 'move away at an angle',
#                                    ifelse(move_pred == 3, 'neither approach or retreat',
#                                           ifelse(move_pred == 4, 'approach at an angle',
#                                                  'approach directly')))))
# 
# ## calculate contrasts
# alt_vs_org_awaydirect <- age_mtx_alt[,,1] - age_mtx_org[,,1]
# alt_vs_org_awayangle  <- age_mtx_alt[,,2] - age_mtx_org[,,2]
# alt_vs_org_neither    <- age_mtx_alt[,,3] - age_mtx_org[,,3]
# alt_vs_org_twdsangle  <- age_mtx_alt[,,4] - age_mtx_org[,,4]
# alt_vs_org_twdsdirect <- age_mtx_alt[,,5] - age_mtx_org[,,5]
# 
# ## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
# print('directly away -- including all age categories')
# mean(alt_vs_org_awaydirect); sd(alt_vs_org_awaydirect)
# # -0.003147928 ; 0.09476731
# quantile(alt_vs_org_awaydirect, prob = c(0.025, 0.5, 0.975))
# #       2.5%        50%      97.5%
# # -0.1958869 -0.0020165  0.1975206
# 
# print('angle away -- including all age categories')
# mean(alt_vs_org_awayangle) ; sd(alt_vs_org_awayangle)
# # 0.0002188393 ; 0.03418267
# quantile(alt_vs_org_awayangle, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -7.072522e-02 -9.174213e-05  6.857230e-02
# 
# print('neither -- including all age categories')
# mean(alt_vs_org_neither)   ; sd(alt_vs_org_neither)
# # -7.595488e-05 ; 0.01010672
# quantile(alt_vs_org_neither, prob = c(0.025, 0.5, 0.975))
# #         2.5%          50%        97.5%
# # -0.022299270  0.000104712  0.021216900
# 
# print('angle towards -- including all age categories')
# mean(alt_vs_org_twdsangle) ; sd(alt_vs_org_twdsangle)
# # -1.717405e-05 ; 0.0327647
# quantile(alt_vs_org_twdsangle, prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.0712463345  0.0002091753  0.0650423023
# 
# print('directly towards -- including all age categories')
# mean(alt_vs_org_twdsdirect); sd(alt_vs_org_twdsdirect)
# # 0.003022218 ; 0.08624364
# quantile(alt_vs_org_twdsdirect, prob = c(0.025, 0.5, 0.975))
# #         2.5%          50%        97.5%
# # -0.164832934  0.001003156  0.189712966
# 
# ## repeat excluding age category 4 because different contrast
# print('directly away -- excluding category 4')
# mean(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     # -0.002859873
# sd(  alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     # 0.09226089
# quantile(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)],
#          prob = c(0.025, 0.5, 0.975))
# #         2.5%          50%        97.5%
# # -0.187451904 -0.002451885  0.192654459
# 
# print('angle away -- excluding category 4')
# mean(alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])      # 0.001396191
# sd(  alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])      # 0.03286692
# quantile(alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)],
#          prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -6.479614e-02  7.298083e-05  6.777950e-02
# 
# print('neither -- excluding category 4')
# mean(alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])        # 8.724056e-05
# sd(  alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])        # 0.009639349
# quantile(alt_vs_org_neither[,which(age_move_org$f_age_num != 4)],
#          prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.0211205843  0.0001737518  0.0201308295
# 
# print('angle towards -- excluding category 4')
# mean(alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])      # -0.0002542673
# sd(  alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])      # 0.03117361
# quantile(alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)],
#          prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.0687492032  0.0001404286  0.0609348000
# 
# print('directly towards -- excluding category 4')
# mean(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # 0.001630709
# sd(  alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # 0.08068363
# quantile(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)],
#          prob = c(0.025, 0.5, 0.975))
# #          2.5%           50%         97.5%
# # -0.1575351296  0.0009805663  0.1729106325
# 
# ## split contrasts by original age category
# age1v2_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 1)]
# age2v3_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 2)]
# age3v4_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 3)]
# age1v4_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 4)] * (-1)
# age1v2_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 1)]
# age2v3_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 2)]
# age3v4_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 3)]
# age1v4_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 4)] * (-1)
# age1v2_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 1)]
# age2v3_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 2)]
# age3v4_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 3)]
# age1v4_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 4)] * (-1)
# age1v2_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 1)]
# age2v3_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 2)]
# age3v4_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 3)]
# age1v4_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 4)] * (-1)
# age1v2_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 1)]
# age2v3_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 2)]
# age3v4_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 3)]
# age1v4_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 4)] * (-1)
# 
# ## calculate contrast values
# print('away direct, 1v2')
# mean(age1v2_ad) ; sd(age1v2_ad)
# # 0.01466836 ± 0.1020192
# quantile(age1v2_ad, prob = c(0.025,0.5,0.975))
# #          2.5%           50%         97.5%
# # -0.1653743808 -0.0009097382  0.2534695712
# print('away direct, 2v3')
# mean(age2v3_ad) ; sd(age2v3_ad)
# # 0.05988455 ± 0.06641943
# quantile(age2v3_ad, prob = c(0.025,0.5,0.975))
# #        2.5%         50%       97.5%
# # -0.05096865  0.04989613  0.21376198
# print('away direct, 3v4')
# mean(age3v4_ad) ; sd(age3v4_ad)
# # -0.06397345 ± 0.06696429
# quantile(age3v4_ad, prob = c(0.025,0.5,0.975))
# #        2.5%         50%       97.5%
# # -0.21418866 -0.05437035  0.04793656
# print('away direct, 1v4')
# mean(age1v4_ad) ; sd(age1v4_ad)
# # 0.004760013 ± 0.107709
# quantile(age1v4_ad, prob = c(0.025,0.5,0.975))
# #          2.5%           50%         97.5%
# # -0.2284701207  0.0005932804  0.2348608415
# 
# print('away angle, 1v2')
# mean(age1v2_aa) ; sd(age1v2_aa)
# # -0.005702401 ± 0.03489941
# quantile(age1v2_aa, prob = c(0.025,0.5,0.975))
# #         2.5%          50%        97.5%
# # -0.078477076 -0.004090745  0.070805899
# print('away angle, 2v3')
# mean(age2v3_aa) ; sd(age2v3_aa)
# # 0.009306459 ± 0.02838815
# quantile(age2v3_aa, prob = c(0.025,0.5,0.975))
# #         2.5%          50%        97.5%
# # -0.048011653  0.006773324  0.064544789
# print('away angle, 3v4')
# mean(age3v4_aa) ; sd(age3v4_aa)
# # -0.004435865 ± 0.03468061
# quantile(age3v4_aa, prob = c(0.025,0.5,0.975))
# #         2.5%          50%        97.5%
# # -0.070127674 -0.004320332  0.070103520
# print('away angle, 1v4')
# mean(age1v4_aa) ; sd(age1v4_aa)
# # 0.006370142 ± 0.04013736
# quantile(age1v4_aa, prob = c(0.025,0.5,0.975))
# #         2.5%          50%        97.5%
# # -0.077360946  0.002226321  0.098466593
# 
# print('neither, 1v2')
# mean(age1v2_n)  ; sd(age1v2_n)
# # -0.002662211 ± 0.01026003
# quantile(age1v2_n, prob = c(0.025,0.5,0.975))
# #         2.5%          50%        97.5%
# # -0.026236179 -0.001142348  0.018779494
# print('neither, 2v3')
# mean(age2v3_n)  ; sd(age2v3_n)
# # -0.00116685 ± 0.009111935
# quantile(age2v3_n, prob = c(0.025,0.5,0.975))
# #          2.5%           50%         97.5%
# # -2.248178e-02 -2.216049e-05  1.556407e-02
# print('neither, 3v4')
# mean(age3v4_n)  ; sd(age3v4_n)
# # 0.001791374 ± 0.009676823
# quantile(age3v4_n, prob = c(0.025,0.5,0.975))
# #          2.5%           50%         97.5%
# # -0.0181166127  0.0006563865  0.0232327395
# print('neither, 1v4')
# mean(age1v4_n)  ; sd(age1v4_n)
# # 0.0009892691 ± 0.01236169
# quantile(age1v4_n, prob = c(0.025,0.5,0.975))
# #          2.5%           50%         97.5%
# # -0.0267482125  0.0003869161  0.0278160110
# 
# print('angle towards, 1v2')
# mean(age1v2_ta) ; sd(age1v2_ta)
# # -0.007486576 ± 0.03455452
# quantile(age1v2_ta, prob = c(0.025,0.5,0.975))
# #         2.5%          50%        97.5%
# # -0.090053435 -0.001857262  0.053409252
# print('angle towards, 2v3')
# mean(age2v3_ta) ; sd(age2v3_ta)
# # -0.01469701 ± 0.02700237
# quantile(age2v3_ta, prob = c(0.025,0.5,0.975))
# #        2.5%         50%       97.5%
# # -0.07436599 -0.01069289  0.03872520
# print('angle towards, 3v4')
# mean(age3v4_ta) ; sd(age3v4_ta)
# # 0.01445636 ± 0.02704508
# quantile(age3v4_ta, prob = c(0.025,0.5,0.975))
# #        2.5%         50%       97.5%
# # -0.04165238  0.01262254  0.06976480
# print('angle towards, 1v4')
# mean(age1v4_ta) ; sd(age1v4_ta)
# # -0.001309705 ± 0.04050663
# quantile(age1v4_ta, prob = c(0.025,0.5,0.975))
# #         2.5%          50%        97.5%
# # -0.075550251 -0.001352119  0.083720209
# 
# print('direct towards, 1v2')
# mean(age1v2_td) ; sd(age1v2_td)
# # 0.001182832 ± 0.09346676
# quantile(age1v2_td, prob = c(0.025,0.5,0.975))
# #          2.5%           50%         97.5%
# # -0.1977650099  0.0007191657  0.1991887275
# print('direct towards, 2v3')
# mean(age2v3_td) ; sd(age2v3_td)
# # -0.05332715 ± 0.05809341
# quantile(age2v3_td, prob = c(0.025,0.5,0.975))
# #        2.5%         50%       97.5%
# # -0.17832862 -0.05069826  0.05233635
# print('direct towards, 3v4')
# mean(age3v4_td) ; sd(age3v4_td)
# # 0.05216158 ± 0.06011493
# quantile(age3v4_td, prob = c(0.025,0.5,0.975))
# #        2.5%         50%       97.5%
# # -0.03861648  0.03694627  0.19566562
# print('direct towards, 1v4')
# mean(age1v4_td) ; sd(age1v4_td)
# # -0.01080972 ± 0.1120743
# quantile(age1v4_td, prob = c(0.025,0.5,0.975))
# #         2.5%          50%        97.5%
# # -0.268241878 -0.001140115  0.206904010
# 
# ## summarise contrasts
# contrasts <- move %>%
#   mutate(alt_vs_org_awaydirect_mu = apply(alt_vs_org_awaydirect, 2, mean),
#          alt_vs_org_awaydirect_sd = apply(alt_vs_org_awaydirect, 2, sd),
#          alt_vs_org_awayangle_mu  = apply(alt_vs_org_awayangle,  2, mean),
#          alt_vs_org_awayangle_sd  = apply(alt_vs_org_awayangle,  2, sd),
#          alt_vs_org_neither_mu    = apply(alt_vs_org_neither,    2, mean),
#          alt_vs_org_neither_sd    = apply(alt_vs_org_neither,    2, sd),
#          alt_vs_org_twdsangle_mu  = apply(alt_vs_org_twdsangle,  2, mean),
#          alt_vs_org_twdsangle_sd  = apply(alt_vs_org_twdsangle,  2, sd),
#          alt_vs_org_twdsdirect_mu = apply(alt_vs_org_twdsdirect, 2, mean),
#          alt_vs_org_twdsdirect_sd = apply(alt_vs_org_twdsdirect, 2, sd)
#   ) %>%
#   mutate(categories_different = ifelse(f_age_num == 4,
#                                        '3 categories different',
#                                        '1 category different'))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(alt_vs_org_awaydirect_mu, alt_vs_org_awayangle_mu,
#                         alt_vs_org_neither_mu, alt_vs_org_twdsangle_mu,
#                         alt_vs_org_twdsdirect_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('alt','vs','org','move_pred','mu'),
#            sep = '_', remove = T) %>%
#   select(-alt_vs_org_awaydirect_sd, -alt_vs_org_awayangle_sd, -alt_vs_org_neither_sd,
#          -alt_vs_org_twdsangle_sd, -alt_vs_org_twdsdirect_sd, -alt, -vs, -org, -mu)
# 
# ## plot contrasts
# str(age_pred)
# # tibble [319,070 × 13] (S3: tbl_df/tbl/data.frame)
# # $ f_age_num        : num [1:319070] 4 4 4 4 4 1 1 1 1 1 ...
# # $ age_combo        : chr [1:319070] "4_4" "4_4" "4_4" "4_4" ...
# # $ stim_type        : chr [1:319070] "ctd" "ctd" "ctd" "ctd" ...
# # $ bda              : chr [1:319070] "before" "before" "before" "before" ...
# # $ focal            : chr [1:319070] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# # $ stim_num         : chr [1:319070] "14" "14" "14" "14" ...
# # $ pb_num           : num [1:319070] 1 1 1 1 1 1 1 1 1 1 ...
# # $ unique_data_combo: int [1:319070] 725 725 725 725 725 725 725 725 725 725
# # $ original_altered : chr [1:319070] "age_org" "age_org" "age_org" "age_org"
# # $ move_pred        : num [1:319070] 1 2 3 4 5 1 2 3 4 5 ...
# # $ mean_propn       : Named num [1:319070] 0.7832 0.1241 0.0224 0.0422 0.0281.
# # ..- attr(*, "names")= chr [1:319070] "725" "725" "725" "725" ...
# # $ stdv_propn       : Named num [1:319070] 0.04247 0.02148 0.00477 0.00952 0.84 ...
# # ..- attr(*, "names")= chr [1:319070] "725" "725" "725" "725" ...
# # $ pred_type        : chr [1:319070] "move away directly" "move away at an an" "neither approach or retreat" "approach at an angle" ...
# age_pred %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('move away directly',
#                                        'move away at an angle',
#                                        'neither approach or retreat',
#                                        'approach at an angle',
#                                        'approach directly')),
#          stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                             ifelse(stim_type == 'l','lion','human'))) %>%
#   mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human'))) %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = stim_type, fill = stim_type),
#                alpha = 0.5)+
#   facet_grid(pred_type ~ .)
# save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_agecontrasts.RData')
# 
# ## clean up a bit
# rm(list = ls()[! ls() %in% c('alt_vs_org_awaydirect','alt_vs_org_awayangle',
#                              'alt_vs_org_neither','alt_vs_org_twdsangle',
#                              'alt_vs_org_twdsdirect','move','mom2_fit','behav','num_iter','num_chains')]) ; gc()
# 
# ## plot full density instead of means
# colnames(alt_vs_org_awaydirect) <- move$data_row
# colnames(alt_vs_org_awayangle)  <- move$data_row
# colnames(alt_vs_org_neither)    <- move$data_row
# colnames(alt_vs_org_twdsangle)  <- move$data_row
# colnames(alt_vs_org_twdsdirect) <- move$data_row
# 
# mtx_to_df <- function(mtx, pred_type){
#   df <- mtx %>%
#     as.data.frame() %>%
#     pivot_longer(cols = everything(),
#                  names_to = 'data_row',
#                  values_to = 'contrast') %>%
#     mutate(data_row = as.integer(data_row)) %>%
#     left_join(move, by = 'data_row') %>%
#     mutate(categories = factor(ifelse(f_age_num == 1,
#                                       "10-15 to 16-20",
#                                       ifelse(f_age_num == 2,
#                                              "16-20 to 21-25",
#                                              ifelse(f_age_num == 3,
#                                                     "21-25 to 26-35",
#                                                     "10-15 to 26-35"))),
#                                levels = c("10-15 to 16-20", "16-20 to 21-25",
#                                           "21-25 to 26-35","10-15 to 26-35"))) %>%
#     mutate(contrast = ifelse(f_age_num == 4,
#                              contrast * (-1), # age_contrast shows 4 -> 1 not 1-> 4
#                              contrast),
#            diff_cats = ifelse(f_age_num == 4,
#                               'youngest to oldest', 'increase by one'),
#            prediction_type = pred_type)
#   return(df)
# }
# ad <- mtx_to_df(alt_vs_org_awaydirect, pred_type = 'move directly away')
# aa <- mtx_to_df(alt_vs_org_awayangle, pred_type = 'move away at an angle')
# n <- mtx_to_df(alt_vs_org_neither, pred_type = 'neither approach or retreat')
# ta <- mtx_to_df(alt_vs_org_twdsangle, pred_type = 'approach at an angle')
# td <- mtx_to_df(alt_vs_org_twdsdirect, pred_type = 'approach directly')
# 
# plot_contrasts <- rbind(aa, ad, n, ta, td) %>%
#   mutate(prediction_type = factor(prediction_type,
#                                   levels = c('move directly away',
#                                              'move away at an angle',
#                                              'neither approach or retreat',
#                                              'approach at an angle',
#                                              'approach directly')))
# 
# ggplot(plot_contrasts)+
#   geom_density(aes(x = contrast,
#                    fill = diff_cats, # fill = f_age_cat,
#                    colour = diff_cats # colour = f_age_cat
#   ),
#   #fill = '#21918c', colour = '#21918c',
#   alpha = 0.4)+
#   scale_colour_viridis_d(begin = 0, end = 0.5)+
#   scale_fill_viridis_d(begin = 0, end = 0.5)+
#   geom_vline(xintercept = 0, linetype = 2)+
#   facet_grid(prediction_type ~ categories, scales = 'free_y')+#, nrow = 3, ncol = 4)+
#   labs(x = 'contrast between age categories',
#        fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
#        colour = 'change in age\ncategory'  # colour = 'original\nage category'
#   )+
#   theme(legend.position = 'none')+ #c(0.8, 0.9))+
#   theme_bw()
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'movement_noprev2bda_agecontrasts.png',
#        path = '../outputs/movement_ordinal_model_2bda/',
#        width = 2400, height = 3200, unit = 'px')
# ggsave(plot = last_plot(), device = 'svg',
#        filename = 'movement_noprev2bda_agecontrasts.svg',
#        path = '../outputs/movement_ordinal_model_2bda/',
#        width = 2400, height = 3200, unit = 'px')
# 
# ############ looking direction                                       ####
# rm(list = ls()[! ls() %in% c('behav','num_iter','num_chains')]) ; gc()
# set.seed(12345)
# pdf('../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_modelchecks.pdf')
# 
# #### create data                                                     ####
# ## select specific data
# look <- behav %>%
#   filter(activity == 'look') %>%
#   select(-activity, -stim_start, -stim_stop) %>%
#   rename(look_index = action_index) %>%
#   mutate(action = ifelse(action_name == 'out_of_sight', 9,
#                          look_index),
#          f_age_num = as.numeric(f_age_num),
#          p_age_num = as.numeric(p_age_num)) %>%
#   filter(!is.na(f_age_num)) %>%
#   filter(!is.na(p_age_num)) %>%
#   filter(look_index != 9)
# 
# #### set prior                                                       ####
# get_prior(formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda +
#             (1|focal) + (1|stim_num) + (1|pb_num),
#           data = look, family = cumulative("logit"))
# #                prior     class         coef    group resp dpar nlpar lb ub        source
# #               (flat)         b                                                   default
# #               (flat)         b age_combo1_2                                 (vectorized)
# #               (flat)         b age_combo1_3                                 (vectorized)
# #               (flat)         b age_combo1_4                                 (vectorized)
# #               (flat)         b age_combo2_1                                 (vectorized)
# #               (flat)         b age_combo2_2                                 (vectorized)
# #               (flat)         b age_combo2_3                                 (vectorized)
# #               (flat)         b age_combo2_4                                 (vectorized)
# #               (flat)         b age_combo3_1                                 (vectorized)
# #               (flat)         b age_combo3_2                                 (vectorized)
# #               (flat)         b age_combo3_3                                 (vectorized)
# #               (flat)         b age_combo3_4                                 (vectorized)
# #               (flat)         b age_combo4_1                                 (vectorized)
# #               (flat)         b age_combo4_2                                 (vectorized)
# #               (flat)         b age_combo4_3                                 (vectorized)
# #               (flat)         b age_combo4_4                                 (vectorized)
# #               (flat)         b    bdabefore                                 (vectorized)
# #               (flat)         b    bdaduring                                 (vectorized)
# #               (flat)         b  mof_age_num                                 (vectorized)
# #               (flat)         b   stim_typeh                                 (vectorized)
# #               (flat)         b   stim_typel                                 (vectorized)
# # student_t(3, 0, 2.5) Intercept                                                   default
# # student_t(3, 0, 2.5) Intercept            1                                 (vectorized)
# # student_t(3, 0, 2.5) Intercept            2                                 (vectorized)
# # student_t(3, 0, 2.5)        sd                                        0          default
# # student_t(3, 0, 2.5)        sd                 focal                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd    Intercept    focal                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd                pb_num                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd    Intercept   pb_num                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd              stim_num                  0     (vectorized)
# # student_t(3, 0, 2.5)        sd    Intercept stim_num                  0     (vectorized)
# #         dirichlet(1)      simo mof_age_num1                                      default
# 
# priors <- c(
#   # focal age
#   prior(normal(0,1),      class = b,    coef = mof_age_num),
#   prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
#   # interaction
#   prior(normal(0,1),      class = b,    coef = age_combo1_2),
#   prior(normal(0,1),      class = b,    coef = age_combo1_3),
#   prior(normal(0,1),      class = b,    coef = age_combo1_4),
#   prior(normal(0,1),      class = b,    coef = age_combo2_1),
#   prior(normal(0,1),      class = b,    coef = age_combo2_2),
#   prior(normal(0,1),      class = b,    coef = age_combo2_3),
#   prior(normal(0,1),      class = b,    coef = age_combo2_4),
#   prior(normal(0,1),      class = b,    coef = age_combo3_1),
#   prior(normal(0,1),      class = b,    coef = age_combo3_2),
#   prior(normal(0,1),      class = b,    coef = age_combo3_3),
#   prior(normal(0,1),      class = b,    coef = age_combo3_4),
#   prior(normal(0,1),      class = b,    coef = age_combo4_1),
#   prior(normal(0,1),      class = b,    coef = age_combo4_2),
#   prior(normal(0,1),      class = b,    coef = age_combo4_3),
#   prior(normal(0,1),      class = b,    coef = age_combo4_4),
#   # stim type
#   prior(normal(0,1),      class = b,    coef = stim_typeh),
#   prior(normal(0,1),      class = b,    coef = stim_typel),
#   # before/during/after
#   prior(normal(0,1),      class = b,    coef = bdabefore),
#   prior(normal(0,1),      class = b,    coef = bdaduring))
# 
# ## prior predictive check
# lom_noprev_prior <- brm(
#   formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = look, family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(lom_noprev_prior) # huge variation in prior, but fairly on both sides so good
# 
# #### fit model                                                       ####
# lom_noprev_fit <- brm(
#   formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = look, family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# save.image('looking_direction/looking_noprev_2bda_run.RData')
# 
# #### extract draws                                                   ####
# # load('looking_direction/looking_noprev_2bda_run.RData')
# ## check model diagnostics -- looks very good
# (summary <- summary(lom_noprev_fit))
# 
# par(mfrow = c(3,1))
# hist(summary$fixed$Rhat, breaks = 50)
# hist(summary$fixed$Bulk_ESS, breaks = 50)
# hist(summary$fixed$Tail_ESS, breaks = 50)
# par(mfrow = c(1,1))
# 
# ## extract posterior distribution
# draws <- as_draws_df(lom_noprev_fit) %>%
#   select(-lprior, -`lp__`)
# parameters <- colnames(draws)[1:(ncol(draws)-3)]
# draws <- draws  %>%
#   pivot_longer(cols = all_of(parameters),
#                names_to = 'parameter',
#                values_to = 'draw') %>%
#   rename(chain = `.chain`,
#          position = `.iteration`,
#          draw_id = `.draw`) %>%
#   mutate(invlogit_draw = invlogit(draw))
# 
# ## extract marginal effects
# marg <- conditional_effects(lom_noprev_fit,
#                             effects = c('f_age_num','age_combo','stim_type',
#                                         'bda'),
#                             categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg) # "f_age_num:cats__" "age_combo:cats__" "stim_type:cats__" "bda:cats__"
# agefocal_effect <- marg[[1]]
# agecombo_effect <- marg[[2]]
# stim_effect <- marg[[3]]
# bda_effect <- marg[[4]]
# 
# ## look at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,2))
# hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
# par(mfrow = c(1,1))
# 
# #### plot marginal effects                                           ####
# (f_age_num_plot <- ggplot(agefocal_effect)+
#    geom_errorbar(aes(x = f_age_num,
#                      ymax = upper__, ymin = lower__,
#                      colour = cats__),
#                  linewidth = 1, width = 0.2)+
#    geom_point(aes(x = f_age_num,
#                   y = estimate__,
#                   colour = cats__),
#               size = 3)+ # cex = 3?
#    xlab(label = 'focal age')+
#    ylab('probability of looking direction')+
#    scale_colour_viridis_d(name = 'looking direction:',
#                           breaks = c('1','2','3'),
#                           labels = c('look towards',
#                                      'side on',
#                                      'look away'))+
#    scale_fill_viridis_d(name = 'looking direction:',
#                         breaks = c('1','2','3'),
#                         labels = c('look towards',
#                                    'side on',
#                                    'look away'))+
#    theme(legend.position = 'bottom',
#          axis.title = element_text(size = 16),
#          axis.text = element_text(size = 12),
#          legend.title = element_text(size = 12),
#          legend.text = element_text(size = 10)))
# ggsave(plot = f_age_num_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_marginaleffects_agefocal.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# f_age_num_labels <- c('focal age category 1',
#                       'focal age category 2',
#                       'focal age category 3',
#                       'focal age category 4')
# names(f_age_num_labels) <- 1:4
# (agecombo_plot <- agecombo_effect %>%
#     separate(col = age_combo, sep = '_', remove = F,
#              into = c('f_age_num','partner_age')) %>%
#     mutate(agecombo = paste0(f_age_num,'-',partner_age)) %>%
#     ggplot()+
#     geom_errorbar(aes(#x = agecombo,
#       x = partner_age,
#       colour = as.factor(cats__), # looking direction?
#       ymax = upper__, ymin = lower__),
#       linewidth = 1,
#       width = 0.4)+
#     geom_point(aes(#x = agecombo,
#       x = partner_age,
#       colour = as.factor(cats__),    # looking direction?
#       #shape = f_age_num,
#       y = estimate__),
#       size = 3)+
#     facet_wrap(. ~ f_age_num,
#                labeller = labeller(f_age_num = f_age_num_labels))+
#     ylab('probability of looking direction')+
#     scale_colour_viridis_d(name = 'looking direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('look towards',
#                                       'side on',
#                                       'look away'))+
#     scale_x_discrete(name = 'partner age category')+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = agecombo_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_marginaleffects_agepartner.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# (stim_plot <- ggplot(stim_effect)+
#     geom_errorbar(aes(x = stim_type,
#                       ymin = lower__, ymax = upper__,
#                       colour = cats__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type,
#                    y = estimate__,
#                    colour = cats__),
#                cex = 3)+ # size = 3?
#     xlab(label = 'stimulus type') + ylab('probability of looking direction')+
#     scale_colour_viridis_d(name = 'looking direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('look towards', 'side on', 'look away'))+
#     scale_x_discrete(breaks = c('ctd','l','h'),
#                      labels = c('dove (control)', 'lion', 'human'),
#                      limits = c('ctd','l','h'))+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = stim_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_marginaleffects_stimtype.png',
#        device = 'png', width = 8.3, height = 5.8)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check                                      ####
# pp_check(lom_noprev_fit, ndraws = 100) # really good fit
# 
# #### plot traces and density curves                                  ####
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
#                           "b_stim_typeh","b_stim_typel",
#                           "b_bdabefore","b_bdaduring",
#                           "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
#                           "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
#                           "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
#                           "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "bsp_mof_age_num",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mixing generally looks very good, though a couple of age combo ones are a touch wandery
# 
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,1))
# hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
# hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')
# 
# ## stim type
# lion <- draws_cut %>% filter(parameter == 'b_stim_typel')
# human <- draws_cut %>% filter(parameter == 'b_stim_typeh')
# plot(density(lion$draw), main = 'lion vs dove') ; abline(v = 0, lty = 2)
# plot(density(human$draw), main = 'human vs dove') ; abline(v = 0, lty = 2)
# 
# ## focal age
# age1 <- draws_cut %>% filter(parameter == 'bsp_mof_age_num')
# age2 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[1]')
# age3 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[2]')
# age4 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[3]')
# par(mfrow = c(2,2))
# plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
# plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# 
# #### plot raw                                                        ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# ## plot control data
# look %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = bda, y = look_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.01))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal looking direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('look directly at','side-on','look directly away'))+
#   scale_x_discrete(name = 'time relative to stimulus')
# 
# look %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = look_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal looking direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('look directly at','side-on','look directly away'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 
# ## plot lion data
# look %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = bda, y = look_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.01))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal looking direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('look directly at','side-on','look directly away'))+
#   scale_x_discrete(name = 'time relative to stimulus')
# 
# look %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = look_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal looking direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('look directly at','side-on','look directly away'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 
# ## plot human data
# look %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = bda, y = look_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.01))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal looking direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('look directly at','side-on','look directly away'))+
#   scale_x_discrete(name = 'time relative to stimulus')
# 
# look %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = look_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal looking direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('look directly at','side-on','look directly away'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 
# print(paste0('raw data plotted at ', Sys.time()))
# 
# ## reset plotting
# save.image('looking_direction/looking_noprev_2bda_run.RData')
# dev.off()
# pdf('../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_modelpredictions.pdf')
# 
# #### predict from model                                              ####
# # load('looking_direction/looking_noprev_2bda_run.RData')
# rm(list = ls()[! ls() %in% c('lom_noprev_fit','look','behav','num_iter','num_chains')]) ; gc()
# 
# pred <- posterior_epred(object = lom_noprev_fit,
#                         newdata = look)
# save.image('looking_direction/looking_noprev_model2bda_predictions.RData')
# print('looking direction predictions completed')
# 
# ## convert to data frame
# # load('looking_direction/looking_noprev_model2bda_predictions.RData')
# look$data_row <- 1:nrow(look)
# extract_predictions <- function(prediction_array, layer, df){
#   predictions <- as.data.frame(prediction_array[,,layer])
#   colnames(predictions) <- 1:nrow(df)
#   predictions <- predictions %>%
#     pivot_longer(cols = everything(),
#                  names_to = 'data_row', values_to = 'epred') %>%
#     mutate(data_row = as.integer(data_row)) %>%
#     left_join(df, by = 'data_row') %>%
#     mutate(pred_type = ifelse(layer == 1, 'look directly away',
#                               ifelse(layer == 2, 'side-on',
#                                      ifelse(layer == 3, 'look at directly',
#                                             'CHECK -- PROBLEM IN DATA'))),
#            pred_type_num = layer)
#   return(predictions)
# }
# pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = look)
# pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = look)
# pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = look)
# 
# ## combine data frames
# pred <- rbind(pred1, pred2, pred3)
# save.image('looking_direction/looking_noprev_model2bda_predictions.RData')
# rm(pred1, pred2, pred3) ; gc()
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions                                                ####
# # load('looking_direction/looking_noprev_model2bda_predictions.RData')
# pred %>%
#   mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                             ifelse(stim_type == 'l', 'lion','human'))) %>%
#   mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human'))) %>%
#   ggplot()+
#   geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                   fill = factor(pred_type, levels = c('look directly away',
#                                                       'side-on',
#                                                       'look at directly')),
#                   colour = factor(pred_type, levels = c('look directly away',
#                                                         'side-on',
#                                                         'look at directly'))
#   )) +
#   facet_grid(stim_type ~ bda)+
#   scale_fill_viridis_d()+
#   scale_colour_viridis_d()+
#   labs(colour = 'predicted looking direction relative to focal:',
#        fill = 'predicted looking direction relative to focal:',
#        x = 'age category of focal elephant',
#        y = 'proportion of predictions',
#        title = 'cape turtle dove (control)')+
#   theme(legend.position = 'bottom')
# 
# ## reset plotting
# dev.off()
# 
# print('looking direction predictions completed')
# 
# #### calculate posterior contrasts from predictions                  ####
# load('looking_direction/looking_noprev_model2bda_predictions.RData')
# pdf('../outputs/looking_ordinal_model_2bda/looking_noprev_modelcontrasts.pdf')
# 
# ## stim type                                                         ####
# stim_new <- look %>%
#   dplyr::select(f_age_num, age_combo, stim_type, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, bda,
#                                                          focal, stim_num, pb_num))))
# 
# ## redo predictions with different stimulus types: all doves
# ctd_look <- stim_new %>%
#   mutate(stim_type = 'ctd')
# ctd_mtx <- posterior_epred(object = lom_noprev_fit, newdata = ctd_look)
# colnames(ctd_mtx) <- ctd_look$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all lions
# lion_look <- stim_new %>%
#   mutate(stim_type = 'l')
# lion_mtx <- posterior_epred(object = lom_noprev_fit, newdata = lion_look)
# colnames(lion_mtx) <- lion_look$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all humans
# human_look <- stim_new %>%
#   mutate(stim_type = 'h')
# human_mtx <- posterior_epred(object = lom_noprev_fit, newdata = human_look)
# colnames(human_mtx) <- human_look$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('looking_direction/looking_noprev_stimuluscontrasts.RData')
# 
# ## focal age                                                         ####
# # load('looking_direction/looking_noprev_stimuluscontrasts.RData')
# rm(list = ls()[!ls() %in% c('lom_noprev_fit','look','behav','num_iter','num_chains')]) ; gc()
# 
# ## create new dataframe to predict from
# age_new <- look %>%
#   dplyr::select(f_age_num, age_combo, stim_type, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, bda,
#                                                          focal, stim_num, pb_num))))
# 
# ## predict with original ages
# age_look_org <- age_new
# age_mtx_org <- posterior_epred(object = lom_noprev_fit, newdata = age_look_org)
# colnames(age_mtx_org) <- age_look_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with altered ages
# age_look_alt <- age_new %>%
#   mutate(f_age_num_original = f_age_num,
#          age_combo_original = age_combo) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
#   separate(age_combo, into = c('f_age_old','p_age'), sep = '_') %>%
#   mutate(age_combo = paste0(f_age_num, '_', p_age)) %>%
#   dplyr::select(f_age_num_original, f_age_num,
#                 age_combo_original, age_combo,
#                 stim_type, bda,
#                 focal, stim_num, pb_num,
#                 unique_data_combo)
# age_mtx_alt <- posterior_epred(object = lom_noprev_fit, newdata = age_look_alt)
# colnames(age_mtx_alt) <- age_look_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('looking_direction/looking_noprev_agecontrasts.RData')
# 
# ## summarise and convert to long format
# # rm(list = ls()) ; gc()
# # load('looking_direction/looking_noprev_agecontrasts.RData')
# age_pred <- age_look_org %>%
#   #dplyr::select(-f_age_num) %>%
#   mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
#          age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
#          age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
#          age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
#          age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
#          age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
#          age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
#          age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
#          age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
#          age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
#          age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
#          age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd)) %>%
#   pivot_longer(cols = c(age_org_prop1_mu, age_org_prop2_mu, age_org_prop3_mu,
#                         age_alt_prop1_mu, age_alt_prop2_mu, age_alt_prop3_mu),
#                names_to = 'focal_agelook_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(age_org_prop1_sd, age_org_prop2_sd, age_org_prop3_sd,
#                         age_alt_prop1_sd, age_alt_prop2_sd, age_alt_prop3_sd),
#                names_to = 'focal_agelook_sd', values_to = 'stdv_propn') %>%
#   separate(col = focal_agelook_mu, into = c('focal_agelook_mu','mu'),
#            sep = '_m', remove = T) %>%
#   separate(col = focal_agelook_sd, into = c('focal_agelook_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-mu, -sd) %>%
#   filter(focal_agelook_mu == focal_agelook_sd) %>%
#   separate(col = focal_agelook_mu, into = c('original_altered', 'look_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-focal_agelook_sd) %>%
#   mutate(look_pred = as.numeric(look_pred),
#          f_age_num = ifelse(original_altered == 'age_org',
#                             f_age_num,
#                             ifelse(original_altered == 'age_alt' & f_age_num == 4,
#                                    1, f_age_num + 1))) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'look away directly',
#                             ifelse(look_pred == 2, 'side on',
#                                    'look at directly')))
# 
# ## calculate contrasts
# alt_vs_org_away <- age_mtx_alt[,,1] - age_mtx_org[,,1]
# alt_vs_org_side <- age_mtx_alt[,,2] - age_mtx_org[,,2]
# alt_vs_org_twds <- age_mtx_alt[,,3] - age_mtx_org[,,3]
# 
# # ## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
# # mean(alt_vs_org_away) ; sd(alt_vs_org_away)
# # mean(alt_vs_org_side) ; sd(alt_vs_org_side)
# # mean(alt_vs_org_twds) ; sd(alt_vs_org_twds)
# 
# ## repeat excluding age category 4 because different contrast
# mean(alt_vs_org_away[,which(age_look_org$f_age_num != 4)])
# sd(alt_vs_org_away[,which(age_look_org$f_age_num != 4)])
# quantile(alt_vs_org_away[,which(age_look_org$f_age_num != 4)], prob = c(0.025, 0.5, 0.975))
# # -0.0006109111
# # 0.0990351
# #         2.5%          50%        97.5%
# # -0.186487304 -0.007682002  0.212870522
# mean(alt_vs_org_side[,which(age_look_org$f_age_num != 4)])
# sd(alt_vs_org_side[,which(age_look_org$f_age_num != 4)])
# quantile(alt_vs_org_side[,which(age_look_org$f_age_num != 4)], prob = c(0.025, 0.5, 0.975))
# # -0.005720087
# # 0.06798424
# #         2.5%          50%        97.5%
# # -0.151817473 -0.002579296  0.129303391
# mean(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)])
# sd(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)])
# quantile(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)], prob = c(0.025, 0.5, 0.975))
# # 0.006330998
# # 0.0686627
# #         2.5%          50%        97.5%
# # -0.153380674  0.003328229  0.150520221
# 
# ## split contrasts by original age category
# age1v2_away <- alt_vs_org_away[,which(age_look_org$f_age_num == 1)]
# age2v3_away <- alt_vs_org_away[,which(age_look_org$f_age_num == 2)]
# age3v4_away <- alt_vs_org_away[,which(age_look_org$f_age_num == 3)]
# age1v4_away <- alt_vs_org_away[,which(age_look_org$f_age_num == 4)] * (-1)
# age1v2_side <- alt_vs_org_side[,which(age_look_org$f_age_num == 1)]
# age2v3_side <- alt_vs_org_side[,which(age_look_org$f_age_num == 2)]
# age3v4_side <- alt_vs_org_side[,which(age_look_org$f_age_num == 3)]
# age1v4_side <- alt_vs_org_side[,which(age_look_org$f_age_num == 4)] * (-1)
# age1v2_twds <- alt_vs_org_twds[,which(age_look_org$f_age_num == 1)]
# age2v3_twds <- alt_vs_org_twds[,which(age_look_org$f_age_num == 2)]
# age3v4_twds <- alt_vs_org_twds[,which(age_look_org$f_age_num == 3)]
# age1v4_twds <- alt_vs_org_twds[,which(age_look_org$f_age_num == 4)] * (-1)
# 
# ## calculate contrast values
# mean(age1v2_away) ; sd(age1v2_away)
# quantile(age1v2_away, prob = c(0.025, 0.5, 0.975))
# # 0.00897345 ; 0.1018764
# #         2.5%          50%        97.5%
# # -0.194314197  0.008778455  0.222728647
# mean(age2v3_away) ; sd(age2v3_away)
# quantile(age2v3_away, prob = c(0.025, 0.5, 0.975))
# # -0.02052369 ; 0.09354632
# #        2.5%         50%       97.5%
# # -0.16651997 -0.03478611  0.23882551
# mean(age3v4_away) ; sd(age3v4_away)
# quantile(age3v4_away, prob = c(0.025, 0.5, 0.975))
# # 0.01669119 ; 0.1001175
# #        2.5%         50%       97.5%
# # -0.21973909  0.01836717  0.19622306
# mean(age1v4_away) ; sd(age1v4_away)
# quantile(age1v4_away, prob = c(0.025, 0.5, 0.975))
# # 0.03813279 ; 0.1368261
# #        2.5%         50%       97.5%
# # -0.24437345  0.03916427  0.30081575
# mean(age1v2_side) ; sd(age1v2_side)
# quantile(age1v2_side, prob = c(0.025, 0.5, 0.975))
# # -0.01079317 ; 0.06653553
# #         2.5%          50%        97.5%
# # -0.156600723 -0.005392002  0.128210379
# mean(age2v3_side) ; sd(age2v3_side)
# quantile(age2v3_side, prob = c(0.025, 0.5, 0.975))
# # 0.004477254 ; 0.06618873
# #         2.5%          50%        97.5%
# # -0.162657489  0.003959147  0.126923640
# mean(age3v4_side) ; sd(age3v4_side)
# quantile(age3v4_side, prob = c(0.025, 0.5, 0.975))
# # -0.01455228 ; 0.06857869
# #        2.5%         50%       97.5%
# # -0.14635576 -0.01303003  0.13195648
# mean(age1v4_side) ; sd(age1v4_side)
# quantile(age1v4_side, prob = c(0.025, 0.5, 0.975))
# # 0.003970471 ; 0.1034045
# #         2.5%          50%        97.5%
# # -0.214769443  0.002041956  0.206082118
# mean(age1v2_twds) ; sd(age1v2_twds)
# quantile(age1v2_twds, prob = c(0.025, 0.5, 0.975))
# # 0.001819724 ; 0.06086906
# #         2.5%          50%        97.5%
# # -0.115022052 -0.003456768  0.147930538
# mean(age2v3_twds) ; sd(age2v3_twds)
# quantile(age2v3_twds, prob = c(0.025, 0.5, 0.975))
# # 0.01604644 ; 0.07179094
# #        2.5%         50%       97.5%
# # -0.19944737  0.01530257  0.14748622
# mean(age3v4_twds) ; sd(age3v4_twds)
# quantile(age3v4_twds, prob = c(0.025, 0.5, 0.975))
# # -0.002138914 ; 0.06560742
# #         2.5%          50%        97.5%
# # -0.129179581 -0.005993718  0.154994481
# mean(age1v4_twds) ; sd(age1v4_twds)
# quantile(age1v4_twds, prob = c(0.025, 0.5, 0.975))
# # -0.04210326 ; 0.1131008
# #        2.5%         50%       97.5%
# # -0.31250431 -0.01337271  0.17458661
# 
# ## summarise contrasts
# contrasts <- look %>%
#   mutate(alt_vs_org_away_mu = apply(alt_vs_org_away, 2, mean),
#          alt_vs_org_away_sd = apply(alt_vs_org_away, 2, sd),
#          alt_vs_org_side_mu = apply(alt_vs_org_side, 2, mean),
#          alt_vs_org_side_sd = apply(alt_vs_org_side, 2, sd),
#          alt_vs_org_twds_mu = apply(alt_vs_org_twds, 2, mean),
#          alt_vs_org_twds_sd = apply(alt_vs_org_twds, 2, sd)
#   ) %>%
#   mutate(categories_different = ifelse(f_age_num == 4,
#                                        '3 categories different',
#                                        '1 category different'),
#          alt_vs_org_away_mu = ifelse(f_age_num == 4, alt_vs_org_away_mu*(-1), alt_vs_org_away_mu),
#          alt_vs_org_side_mu = ifelse(f_age_num == 4, alt_vs_org_side_mu*(-1), alt_vs_org_side_mu),
#          alt_vs_org_twds_mu = ifelse(f_age_num == 4, alt_vs_org_twds_mu*(-1), alt_vs_org_twds_mu))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(alt_vs_org_away_mu,
#                         alt_vs_org_side_mu,
#                         alt_vs_org_twds_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('alt','vs','org','look_pred','mu'),
#            sep = '_', remove = T) %>%
#   select(-alt_vs_org_away_sd,-alt_vs_org_side_sd,-alt_vs_org_twds_sd,
#          -alt, -vs, -org, -mu)
# 
# ## plot contrasts
# age_pred %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('look away directly',
#                                        'side on',
#                                        'look at directly'))) %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = pred_type, fill = pred_type),
#                alpha = 0.5)+
#   facet_grid(pred_type ~ stim_type)
# contrasts_long %>%
#   mutate(pred_type = ifelse(look_pred == 'away',
#                             'look away directly',
#                             ifelse(look_pred == 'side',
#                                    'side on',
#                                    'look at directly'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('look away directly',
#                                        'side on',
#                                        'look at directly'))) %>%
#   mutate(f_age_new = ifelse(f_age_num == 4,
#                             'youngest to oldest',
#                             paste0('category ',f_age_num,' to ',f_age_num+1))) %>%
#   ggplot()+
#   geom_density(aes(x = difference,
#                    colour = categories_different,
#                    fill = categories_different),
#                alpha = 0.5)+
#   geom_vline(xintercept = 0, linetype = 2)+
#   facet_grid(pred_type ~ f_age_new, scales = 'free_y')+
#   scale_colour_viridis_d(begin = 0.5, end = 0)+
#   scale_fill_viridis_d(begin = 0.5, end = 0)+
#   labs(colour = 'categories\ndifferent',
#        fill = 'categories\ndifferent')
# save.image('looking_direction/looking_noprev_agecontrasts.RData')
# 
#   ## clean up a bit
# rm(list = ls()[! ls() %in% c('alt_vs_org_away','alt_vs_org_side',
#                              'alt_vs_org_twds','look','lom_noprev_fit',
#                              'behav','num_iter','num_chains')]) ; gc()
# 
# ## plot full density instead of means
# look$data_row <- 1:nrow(look)
# colnames(alt_vs_org_away) <- look$data_row
# colnames(alt_vs_org_side) <- look$data_row
# colnames(alt_vs_org_twds) <- look$data_row
# 
# mtx_to_df <- function(mtx, pred_type){
#   df <- mtx %>%
#     as.data.frame() %>%
#     pivot_longer(cols = everything(),
#                  names_to = 'data_row',
#                  values_to = 'contrast') %>%
#     mutate(data_row = as.integer(data_row)) %>%
#     left_join(look, by = 'data_row') %>%
#     mutate(categories = factor(ifelse(f_age_num == 1,
#                                       "10-15 to 16-20",
#                                       ifelse(f_age_num == 2,
#                                              "16-20 to 21-25",
#                                              ifelse(f_age_num == 3,
#                                                     "21-25 to 26-35",
#                                                     "10-15 to 26-35"))),
#                                levels = c("10-15 to 16-20", "16-20 to 21-25",
#                                           "21-25 to 26-35","10-15 to 26-35"))) %>%
#     mutate(contrast = ifelse(f_age_num == 4,
#                              contrast * (-1), # age_contrast shows 4 -> 1 not 1-> 4
#                              contrast),
#            diff_cats = ifelse(f_age_num == 4,
#                               'youngest to oldest', 'increase by one'),
#            prediction_type = pred_type)
#   return(df)
# }
# away <- mtx_to_df(alt_vs_org_away, pred_type = 'look away directly')
# side <- mtx_to_df(alt_vs_org_side, pred_type = 'side on')
# twds <- mtx_to_df(alt_vs_org_twds, pred_type = 'look at directly')
# 
# plot_contrasts <- rbind(away, side, twds) %>%
#   mutate(prediction_type = factor(prediction_type,
#                                   levels = c('look away directly',
#                                              'side on',
#                                              'look at directly')))
# 
# ggplot(plot_contrasts)+
#   geom_density(aes(x = contrast,
#                    fill = diff_cats, # fill = f_age_cat,
#                    colour = diff_cats # colour = f_age_cat
#   ),
#   #fill = '#21918c', colour = '#21918c',
#   alpha = 0.4)+
#   scale_colour_viridis_d(begin = 0, end = 0.5)+
#   scale_fill_viridis_d(begin = 0, end = 0.5)+
#   geom_vline(xintercept = 0, linetype = 2)+
#   facet_grid(prediction_type ~ categories, scales = 'free_y')+#, nrow = 3, ncol = 4)+
#   labs(x = 'contrast between age categories',
#        fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
#        colour = 'change in age\ncategory'  # colour = 'original\nage category'
#   )+
#   theme(legend.position = 'none')+ #c(0.8, 0.9))+
#   theme_bw()
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'looking_noprev_ordinal2bda_agecontrasts.png',
#        path = '../outputs/looking_ordinal_model_2bda/',
#        width = 2400, height = 3200, unit = 'px')
# ggsave(plot = last_plot(), device = 'svg',
#        filename = 'looking_noprev_ordinal2bda_agecontrasts.svg',
#        path = '../outputs/looking_ordinal_model_2bda/',
#        width = 2400, height = 3200, unit = 'px')
# 
# dev.off()
# save.image('looking_direction/looking_noprev_agecontrasts.RData')
