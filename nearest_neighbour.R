#### information ####
# script for basic analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
library(tidyverse)
#library(cmdstanr) ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/') # library(cmdstanr, lib.loc = '../../packages/')
library(brms)

theme_set(theme_classic())

#### NEAREST NEIGHBOUR: ordinal logistic regression ####
# https://dagitty.net/dags.html?id=fQrEyF#
#library(brms, lib.loc = '../../packages/') # library(brms)

# read in data
ages <- readRDS('../data_processed/elephant_behaviour_proportions.RDS') %>% 
  select(pb_num, subject, targeted_elephant,    # random effects
         stim_type,age_category,partner_age_category,age_difference, # exposures
         age,partner_age,focal,dyad_partner,    # reference info
         group_size                             # remove any with only 2
  ) %>% 
  distinct()

stim_starts <- readRDS('../data_processed/stimuli.RDS') %>% 
  filter(status == 'START' & behavior == 'STIMULUS') %>% 
  select(pb_num,time,stim_num,comment)
table(stim_starts$pb_num)
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
  select(pb_num, stim_start,stim_num)

nn_all <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>% 
  select(subject,bull,pb_num,second,out_frame_name,out_frame_index,
         b1_nn_name,#b1_nn_index,
         b2_nn_name,#b2_nn_index,
         b3_nn_name,#b3_nn_index,
         b4_nn_name,#b4_nn_index,
         b5_nn_name,#b5_nn_index,
         b6_nn_name,#b6_nn_index,
         b7_nn_name,#b7_nn_index,
         b8_nn_name#,b8_nn_index,
  ) %>% 
  pivot_longer(cols = c('b1_nn_name','b2_nn_name','b3_nn_name','b4_nn_name',
                        'b5_nn_name','b6_nn_name','b7_nn_name','b8_nn_name'),
               names_to = 'elephant_nn', values_to = 'neighbour') %>% 
  filter(is.na(neighbour) == FALSE) %>% 
  filter(neighbour != 'impossible_partner') %>% 
  filter(neighbour != 'out_of_sight') %>% 
  separate(elephant_nn, into = c('partner_bull','nn_name'), sep = 2, remove = F) %>% 
  select(-nn_name) %>% 
  mutate(targeted_elephant = paste0(partner_bull, '_e', pb_num)) %>% 
  left_join(ages[,c('subject','targeted_elephant','stim_type','age_category','partner_age_category','age','partner_age','age_difference','group_size')],
            by = c('subject','targeted_elephant')) %>% 
  rename(f_subject = subject,
         f_bull = bull,
         p_subject = targeted_elephant,
         p_bull = partner_bull,
         nn_binom = neighbour,
         f_age_num = age_category,
         f_age_cat = age,
         p_age_num = partner_age_category,
         p_age_cat = partner_age,
         age_diff_cat = age_difference) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(stim_starts, by = 'pb_num') %>% 
  mutate(time_since_stim = second - stim_start)

nn <- nn_all %>% 
  filter(nn_binom == 1) %>% 
  filter(group_size > 2) %>% 
  filter(out_frame_name == 'in_frame') %>% 
  select(-elephant_nn,-nn_binom,-group_size) %>% 
  mutate(nn_tminus1 = NA)

# create variable for nearest neighbour at time t-1
playbacks <- unique(nn$pb_num)
for(i in 1:length(playbacks)){
  pb <- nn %>% filter(pb_num == playbacks[i])
  nn <- nn %>% anti_join(pb)
  for(j in 2:nrow(pb)){
    pb$nn_tminus1[j] <- pb$age_diff_cat[j-1]
  }
  nn <- rbind(nn, pb)
}
rm(check, x, i, multiple_starts) ; gc()

# filter to remove elephants with unknown ages
nn_no_na <- nn %>% 
  mutate(age_diff_cat = factor(age_diff_cat,
                               levels = c('partner younger',
                                          'matched',
                                          'partner older'))) %>% 
  mutate(age_diff_num = as.integer(age_diff_cat),
         f_age_num = as.integer(f_age_num),
         p_age_num = as.integer(p_age_num)) %>% 
  filter(is.na(age_diff_num) == FALSE) %>%
  filter(is.na(f_age_num) == FALSE) %>% 
  filter(is.na(p_age_num) == FALSE) %>% 
  filter(is.na(nn_tminus1) == FALSE) %>% 
  mutate(focal_id = as.integer(as.factor(f_subject)),
         stim_num = as.integer(as.factor(stim_num))) %>% 
  rename(stim_id = stim_num,
         playback_id = pb_num)
str(nn_no_na)

# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
nn_no_na <- nn_no_na %>% 
  mutate(nn_tminus1_num = ifelse(nn_tminus1 == 'partner younger', 1,
                                 ifelse(nn_tminus1 == 'matched', 2, 
                                        ifelse(nn_tminus1 == 'partner older', 3, NA)))) %>% 
  mutate(nn_tminus1_num = as.integer(nn_tminus1_num))

get_prior(formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +   # fixed effects -- no longer contains partner age, otherwise it's essentially abs(x-z) ~ x*z rather than y ~ x*z
            time_since_stim + mo(nn_tminus1_num) +                                   # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),                            # random effects
          data = nn_no_na,
          family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,0.25),   class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(0,1),      class = b,    coef = stim_typeh),
  prior(normal(0,1),      class = b,    coef = stim_typel),
  # controls
  prior(normal(0,1),      class = b,    coef = time_since_stim),
  prior(normal(0,0.333),  class = b,    coef = monn_tminus1_num),
  prior(dirichlet(2),     class = simo, coef = monn_tminus1_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
nn_prior <- brm(
  formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type + 
    time_since_stim + mo(nn_tminus1_num) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = nn_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(nn_prior) # huge variation in prior, but fairly on both sides so good

## fit model
nn_fit <- brm(
  formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type + 
    time_since_stim + mo(nn_tminus1_num) + 
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = nn_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# inspect model
summary(nn_fit)

# save workspace
save.image('nearest_neighbour/neighbour_model_run.RData')
#load('nearest_neighbour/nearest_neighbour/neighbour_model_run.RData') ; rm(biologylibs, homedrive, homelibs, homelibsprofile,rlibs,Rversion)

## check outputs ####
library(LaplacesDemon)
#load('nearest_neighbour/nearest_neighbour/neighbour_model_run.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()

## check Stan code
nn_fit$model

## calculate log cumulative odds
prop <- table(nn_no_na$age_diff_num) / nrow(nn_no_na)
cum_prop <- cumsum(prop)
log_cum_odds <- logit(cum_prop)

## extract posterior distribution
draws <- as_draws_df(nn_fit) %>% 
  select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>% 
  pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>% 
  mutate(iteration = rep(rep(1:(num_iter/2),
                             each = length(unique(parameter))),
                         num_chains),
         chain = rep(1:num_chains,
                     each = length(unique(parameter))*(num_iter/2)),
         invlogit_draw = invlogit(draw))

## move at intercepts (estimates of cutpoints between categories on linear model scale)
unique(draws$parameter)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

## extract marginal effects
marg <- conditional_effects(nn_fit,
                            effects = c('f_age_num','stim_type',
                                        'time_since_stim','nn_tminus1_num'),
                            categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
f_age_effect <- marg[[1]]
stim_effect <- marg[[2]]
time_effect <- marg[[3]]
prevsec_effect <- marg[[4]]

## plot marginal effects
conditional_effects(nn_fit, effects = 'f_age_num',
                    categorical = TRUE,
                    spaghetti = TRUE,
                    #conditions = c('stim_type'),
                    method = 'posterior_epred')
(focal_age_plot <- ggplot(f_age_effect)+
    geom_ribbon(aes(x = f_age_num, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
    geom_line(aes(x = f_age_num, y = estimate__, colour = cats__), linewidth = 1)+
    xlab(label = 'focal age') + ylab('probability')+
    scale_colour_viridis_d(name = 'age difference:',
                           breaks = c('1','2','3'),
                           labels = c('partner younger',
                                      'age matched',
                                      'partner older'))+
    scale_fill_viridis_d(name = 'age difference:',
                         breaks = c('1','2','3'),
                         labels = c('partner younger',
                                    'age matched',
                                    'partner older')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = focal_age_plot, filename = '../outputs/nn_marginaleffects_focalage.png', device = 'png',
       width = 8.3, height = 5.8)

conditional_effects(nn_fit, 'stim_type',
                    categorical = TRUE,
                    method = 'posterior_epred')
(stim_plot <- ggplot(stim_effect)+
    geom_errorbar(aes(x = stim_type, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type, y = estimate__, colour = cats__),cex = 3)+
    xlab(label = 'stimulus type') + ylab('probability')+
    scale_colour_viridis_d(name = 'neighbour age:',
                           breaks = c('1','2','3'),
                           labels = c('younger', 'matched', 'older'))+
    scale_fill_viridis_d(name = 'neighbour age:',
                         breaks = c('1','2','3'),
                         labels = c('younger', 'matched', 'older'))+
    scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = stim_plot, filename = '../outputs/nn_marginaleffects_stimtype.png', device = 'png',
       width = 8.3, height = 5.8)

library(ggpubr)
(all_plots <- ggarrange(focal_age_plot, stim_plot, ncol=2, nrow=1, common.legend = TRUE, legend = "bottom"))
ggsave(plot = all_plots, filename = '../outputs/nn_marginaleffects.png', device = 'png',
       width = (5.8*2), height = 8.3)

## posterior predictive check
pp_check(nn_fit, ndraws = 100) # perfect fit

## plot traces
draws %>% 
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "b_time_since_stim",
                          "bsp_mof_age_num","bsp_mop_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "simo_mop_age_num1[1]","simo_mop_age_num1[2]","simo_mop_age_num1[3]",
                          "bsp_monn_tminus1_num",
                          "simo_monn_tminus1_num1[1]","simo_monn_tminus1_num1[2]",
                          "bsp_mof_age_num:mop_age_num",
                          "simo_mof_age_num:mop_age_num1[1]","simo_mof_age_num:mop_age_num1[2]",
                          "simo_mof_age_num:mop_age_num1[3]","simo_mof_age_num:mop_age_num2[1]",
                          "simo_mof_age_num:mop_age_num2[2]","simo_mof_age_num:mop_age_num2[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept"
                          )) %>% 
  ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # looks like a lot of poor mixing, though there were no warnings from the model run about Rhat being dodgy (369 divergent transitions only)

## plot raw
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

ggplot(nn_no_na, aes(x = f_age_num, y = age_diff_num,
                       colour = as.factor(nn_tminus1_num)))+
  geom_jitter(alpha = 0.1)+
  facet_wrap(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_y_continuous(name = 'neighbour age relative to focal',
                     breaks = c(1,2,3),
                     labels = c('younger','matched','older'))+
  labs(colour = 'neigbour age at t-1:')

nn_no_na %>% 
  ggplot(aes(x = time_since_stim, y = age_diff_num))+
  geom_vline(aes(xintercept = 0))+
  geom_point(aes(colour = as.factor(f_age_num)#rgb(0,0,1,0.01)
                 ), alpha = 0.01)+
  facet_grid(f_age_num ~ stim_type,
             labeller = labeller(f_age_num = age_labels,
                                 stim_type = stim_labels))+
  scale_y_continuous(name = 'neighbour age relative to focal',
                     breaks = c(1,2,3),
                     labels = c('younger',
                                'matched',
                                'older'))+
  scale_x_continuous(name = 'time since stimulus started (s)') # no particular effect of time since stimulus -- all seem pretty similar before and after. possible differences between stimuli: youngest are more likely to age match during control but be near older during lion/human (but that is also the case beforethe stimulus); 21-25 more likely to age match during lion than other 2; oldest switch from being near younger to age matching during lion stimuli; oldest do not age match for human stimuli; 16-20 more likely to be near youngest after lion and human than before

## predict from model ####
rm(list = ls()[! ls() %in% c('nn_fit','nn_no_na')]) ; gc()
subjects <- sample(unique(nn_no_na$focal_id), 5, replace = F)
stimuli <- sample(unique(nn_no_na$stim_id), 5, replace = F)
pbs <- sample(unique(nn_no_na$playback_id), 5, replace = F)
predict_data <- data.frame(f_age_num = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
                           p_age_num = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
                           stim_type = rep(c('ctd','h','l'),
                                           each = 26*3*length(subjects)*length(stimuli)*length(pbs)),
                           time_since_stim = rep(rep(seq(from = -200, to = 300, by = 20),
                                                     each = 3*length(subjects)*length(stimuli)*length(pbs)),
                                                 3),
                           nn_tminus1_num = rep(rep(1:3,
                                                      each = length(subjects)*length(stimuli)*length(pbs)),
                                                  3*26),
                           focal_id = rep(rep(subjects,
                                              each = length(stimuli)*length(pbs)),
                                          3*26*3),
                           stim_id = rep(rep(stimuli,
                                             each = length(pbs)),
                                         3*26*3*length(subjects)),
                           playback_id = rep(pbs, 3*26*3*length(subjects)*length(stimuli)))
pred <- posterior_predict(object = nn_fit,
                          newdata = predict_data)
age_types <- c('foc1_part1','foc1_part2','foc1_part3','foc1_part4',
               'foc2_part1','foc2_part2','foc2_part3','foc2_part4',
               'foc3_part1','foc3_part2','foc3_part3','foc3_part4',
               'foc4_part1','foc4_part2','foc4_part3','foc4_part4')
pred_all <- array(data = NA, dim = c(nrow(pred), ncol(pred), length(age_types)),
                  dimnames = list(rownames(pred), colnames(pred),
                                  age_types))
pred_all[,,1] <- pred
save.image('nearest_neighbour/neighbour_model_predictions.RData')
for(i in 2:length(age_types)){
  predict_data$f_age_num <- ifelse(i <= 4, 1,
                                   ifelse(i <= 8, 2,
                                          ifelse(i <= 12, 3 , 4)))
  predict_data$p_age_num <- ifelse(i %in% c(1,5,9,13), 1,
                                     ifelse(i %in% c(2,6,10,14), 2, 
                                            ifelse(i %in% c(3,7,11,15), 3, 4)))
  # predict_data$focal_age <- ifelse(i < 5, 1,
  #                                  ifelse(i < 9, 2, 3))
  # predict_data$age_diff_num <- ifelse(i %in% c(1,5,9), 1,
  #                                     ifelse(i %in% c(2,6,10), 2,
  #                                            ifelse(i %in% c(3,7,11), 3, 4)))
  pred <- posterior_predict(object = nn_fit,
                            newdata = predict_data)
  pred_all[,,i] <- pred
  save.image('nearest_neighbour/neighbour_model_predictions.RData')
}

load('nearest_neighbour/neighbour_model_predictions.RData')
predict_data$num <- row_number(predict_data)
predictions <- pred_all[,,age_types[1]] %>% 
  as.data.frame()
predictions <- predictions[1:100,] %>% 
  pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
  separate(Vnum, into = c('v','num'), sep = 1) %>% 
  select(-v) %>% 
  mutate(age_type = age_types[1],
         num = as.numeric(num)) %>% 
  left_join(predict_data[,3:ncol(predict_data)], by = 'num')
for(i in 2:length(age_types)){
  pred <- pred_all[,,age_types[i]] %>% 
    as.data.frame()
  pred <- pred[1:100,] %>% 
    pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
    separate(Vnum, into = c('v','num'), sep = 1) %>% 
    select(-v) %>% 
    mutate(age_type = age_types[i],
           num = as.numeric(num)) %>% 
    left_join(predict_data[,3:ncol(predict_data)], by = 'num')
  predictions <- rbind(predictions, pred)
}
save.image('nearest_neighbour/neighbour_model_predictions.RData')

age_types <- data.frame(age_type = age_types) %>% 
  separate(age_type, into = c('focal_age','partner_age'),
           remove = F, sep = '_part') %>% 
  mutate(focal_age = ifelse(focal_age == 'foc1', 1,
                            ifelse(focal_age == 'foc2', 2,
                                   ifelse(focal_age == 'foc3', 3, 4))),
         partner_age = as.numeric(partner_age))

rm(pred, pbs, stimuli, subjects, i) ; gc()
predictions <- left_join(predictions, age_types, by = 'age_type')
rm(age_types) ; gc()
save.image('nearest_neighbour/neighbour_model_predictions.RData')

## plot outputs ####
load('nearest_neighbour/neighbour_model_predictions.RData')
ggplot(predictions, aes(x = time_since_stim, y = prediction))+
  geom_line()+
  geom_point()+
  facet_wrap(.~stim_type)

## plot raw data -- this looks a lot more exciting at first glance than it is: basically just shows that far and away the strongest effect is neighbour age at second t-1
stimuli <- c('dove (control)','lion','human')
names(stimuli) <- c('ctd','l','h')
nn_no_na %>% 
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>% 
  ggplot(aes(x = time_since_stim, y = age_diff_num,
             colour = stim_type, shape = nn_tminus1))+
  geom_vline(aes(xintercept = 0))+
  geom_point(alpha = 0.1)+ # colour = rgb(0,0,1,0.01)
  facet_grid(stim_type ~ factor(nn_tminus1,
                                levels = c('partner younger',
                                           'matched',
                                           'partner older')),
             labeller = labeller(stim_type = stimuli))

## plot predictions
summary(nn_fit)
# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: age_diff_num ~ 1 + mo(f_age_num) * mo(p_age_num) + stim_type + time_since_stim + mo(nn_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id) 
# Data: nn_no_na (Number of observations: 40120) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~focal_id (Number of levels: 140) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)    21.13      2.33    17.17    25.97 1.02      351      471
# 
# ~playback_id (Number of levels: 33) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.75      1.36     0.08     5.13 1.02      532      608
# 
# ~stim_id (Number of levels: 23) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.82      1.48     0.08     5.75 1.02      237       65
# 
# Population-Level Effects: 
#                         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]               11.81      2.55     6.52    16.68 1.01      339      673
# Intercept[2]               35.39      3.10    29.61    41.69 1.01      403     1075
# stim_typeh                  0.26      0.99    -1.71     2.12 1.01      564      517
# stim_typel                 -0.26      0.95    -2.12     1.55 1.00     3005     3107
# time_since_stim            -0.01      0.00    -0.01    -0.00 1.00     4550     3277
# mof_age_num                -0.35      0.26    -0.85     0.17 1.00     4806     2784
# mop_age_num                 1.78      0.17     1.43     2.11 1.00     1941     2988
# monn_tminus1_num            1.55      0.22     1.13     1.98 1.00     1581     2843
# mof_age_num:mop_age_num     4.60      0.26     4.10     5.11 1.01      334       81
# 
# Simplex Parameters: 
#                             Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mof_age_num1[1]                 0.29      0.17     0.04     0.69 1.00     1579     2640
# mof_age_num1[2]                 0.39      0.20     0.07     0.78 1.00      887     2276
# mof_age_num1[3]                 0.31      0.17     0.05     0.71 1.00     4973     2781
# mop_age_num1[1]                 0.91      0.04     0.81     0.97 1.00     1027     2571
# mop_age_num1[2]                 0.05      0.03     0.01     0.13 1.01      590      732
# mop_age_num1[3]                 0.05      0.03     0.01     0.12 1.01     4793     2503
# monn_tminus1_num1[1]            0.13      0.08     0.02     0.32 1.00     6017     2901
# monn_tminus1_num1[2]            0.87      0.08     0.68     0.98 1.00     6017     2901
# mof_age_num:mop_age_num1[1]     0.96      0.02     0.92     0.99 1.00     1218     2472
# mof_age_num:mop_age_num1[2]     0.02      0.02     0.00     0.06 1.00     3760     2859
# mof_age_num:mop_age_num1[3]     0.02      0.01     0.00     0.05 1.01      650      325
# mof_age_num:mop_age_num2[1]     0.03      0.02     0.00     0.08 1.01      323       70
# mof_age_num:mop_age_num2[2]     0.62      0.03     0.55     0.68 1.01     1408     2381
# mof_age_num:mop_age_num2[3]     0.35      0.03     0.29     0.41 1.00     1927     3096
# 
# Family Specific Parameters: 
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message: There were 222 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 

## want to extract an estimated probability of each value at every time point, split across the different types
## take predictions from model and mean across random effects? so mean+/-stdev prediction for seconds = -120, age = 10-15, age_diff = 1, stim = ctd  -- pretty sure this would be the equivalent of treating it like a continuous variable again which is definitely not right, but right now I have no other ideas!! I think I probably need to extract it directly from the model draws for each parameter, but I don't think I know how to do that...

age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
ggplot(nn_no_na)+
  annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
  geom_point(mapping = aes(x = time_since_stim, y = age_diff_num),
             alpha = 0.01, shape = 19)+
  # geom_violin(data = nn_no_na,
  #             mapping = aes(y = time_since_stim, x = as.factor(age_diff_num)))+
  # coord_flip()+
  facet_wrap(. ~ f_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'age of nearest neighbour relative to focal',
                     breaks = c(1,2,3),
                     labels = c('younger','matched','older'),
                     expand = c(0,0))+
  scale_x_continuous(name = 'time since stimulus (s)')

predict_means <- predictions %>% 
  select(focal_age, partner_age, stim_type, time_since_stim, nn_tminus1_num) %>% 
  distinct() %>% 
  mutate(mean_pred = NA, stdv_pred = NA)
for(i in 1:nrow(predict_means)){
  x <- predictions$prediction[predictions$focal_age == predict_means$focal_age[i] &
                                predictions$partner_age == predict_means$partner_age[i] &
                                predictions$stim_type == predict_means$stim_type[i] &
                                predictions$time_since_stim == predict_means$time_since_stim[i] &
                                predictions$nn_tminus1_num == predict_means$nn_tminus1_num[i]]
  predict_means$mean_pred[i] <- mean(x)
  predict_means$stdv_pred[i] <- sd(x)
}
predict_means$lwr <- predict_means$mean_pred - predict_means$stdv_pred
predict_means$upr <- predict_means$mean_pred + predict_means$stdv_pred

save.image('nearest_neighbour/neighbour_model_predictions.RData')

age_labels <- c('younger at t-1','matched at t-1','older at t-1')
names(age_labels) <- c(1,2,3)
stim_labels <- c('dove (control)','lion','human')
names(stim_labels) <- c('ctd','l','h')
predict_means <- predict_means %>% 
  mutate(f_age_cat = ifelse(focal_age == 1, '10-15',
                            ifelse(focal_age == 2, '16-20',
                                   ifelse(focal_age == 3, '21-25',
                                          ifelse(focal_age == 4, '26-35',
                                                 '36+')))),
         p_age_cat = ifelse(partner_age == 1, '10-15',
                            ifelse(partner_age == 2, '16-20',
                                   ifelse(partner_age == 3, '21-25',
                                          ifelse(partner_age == 4, '26-35',
                                                 '36+')))))
ggplot()+
  annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
  geom_ribbon(data = predict_means,
              mapping = aes(x = time_since_stim, ymax = upr, ymin = lwr,
                            fill = f_age_cat, linetype = p_age_cat),
              alpha = 0.2)+
  geom_point(data = nn_no_na,
             mapping = aes(x = time_since_stim, y = age_diff_num,
                           colour = f_age_cat),
             alpha = 0.1, shape = 19)+
  facet_grid(stim_type ~ nn_tminus1_num,
             labeller = labeller(nn_tminus1_num = age_labels,
                                 stim_type = stim_labels))+
  geom_line(data = predict_means,
            mapping = aes(x = time_since_stim, y = mean_pred,
                          colour = f_age_cat,
                          linetype = p_age_cat))+
  scale_y_continuous(name = 'age of nearest neighbour relative to focal',
                     breaks = c(1,2,3), labels = c('younger','matched','older'),
                     expand = c(0,0))+
  scale_x_continuous(name = 'time since stimulus (s)')+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  labs(colour = 'focal age category',
       linetype = 'neighbour age category',
       fill = 'focal age category')
ggsave(plot = last_plot(),
       filename = '../outputs/nn_predictions_withribbon.png', device = 'png',
       width = 8.3, height = 5.8)

f_age_labels <- c('focal: 10-15','focal: 16-20','focal: 21-25','focal: 26-35')
names(f_age_labels) <- c(1,2,3,4)
p_age_labels <- c('partner: 10-15','partner: 16-20','partner: 21-25','partner: 26-35')
names(p_age_labels) <- c(1,2,3,4)
# predict_means <- predict_means %>% 
#   mutate(f_age_cat = ifelse(focal_age == 1, '10-15',
#                             ifelse(focal_age == 2, '16-20',
#                                    ifelse(focal_age == 3, '21-25',
#                                           ifelse(focal_age == 4, '26-35',
#                                                  '36+')))),
#          p_age_cat = ifelse(partner_age == 1, '10-15',
#                             ifelse(partner_age == 2, '16-20',
#                                    ifelse(partner_age == 3, '21-25',
#                                           ifelse(partner_age == 4, '26-35',
#                                                  '36+')))))
ggplot()+
  annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
  # geom_ribbon(data = predict_means,
  #             mapping = aes(x = time_since_stim, ymax = upr, ymin = lwr,
  #                           fill = f_age_cat, linetype = p_age_cat),
  #             alpha = 0.2)+
  geom_point(data = nn_no_na,
             mapping = aes(x = time_since_stim, y = age_diff_num,
                           colour = stim_type, shape = nn_tminus1),
             alpha = 0.1)+
  facet_grid(f_age_cat ~ p_age_cat,
             labeller = labeller(f_age_cat = f_age_labels,
                                 p_age_cat = p_age_labels))+
  geom_line(data = predict_means,
            mapping = aes(x = time_since_stim, y = mean_pred,
                          colour = stim_type,
                          linetype = nn_tminus1))+
  scale_y_continuous(name = 'age of nearest neighbour relative to focal',
                     breaks = c(1,2,3), labels = c('younger','matched','older'),
                     expand = c(0,0))+
  scale_x_continuous(name = 'time since stimulus (s)')+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  labs(colour = 'focal age category',
       linetype = 'neighbour age category',
       fill = 'focal age category')
ggsave(plot = last_plot(),
       filename = '../outputs/nn_predictions_withribbon.png', device = 'png',
       width = 8.3, height = 5.8)

save.image('nearest_neighbour/neighbour_model_predictions.RData')

