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
         playback_id = pb_num) %>% 
  mutate(after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60)) # currently this is in seconds not minutes -- minutes might work better since on a scale clsoer to y (so just do time_since_stim/60)
str(nn_no_na)

# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
nn_no_na <- nn_no_na %>% 
  mutate(nn_tminus1_num = ifelse(nn_tminus1 == 'partner younger', 1,
                                 ifelse(nn_tminus1 == 'matched', 2, 
                                        ifelse(nn_tminus1 == 'partner older', 3, NA)))) %>% 
  mutate(nn_tminus1_num = as.integer(nn_tminus1_num))

get_prior(formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
            s(after_stim) + mo(nn_tminus1_num) +                       # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),              # random effects
          data = nn_no_na,
          family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,0.25),     class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2),   class = simo, coef = mof_age_num1),
  # stim type
  prior(normal(0,1),        class = b,    coef = stim_typeh),
  prior(normal(0,1),        class = b,    coef = stim_typel),
  # time spline
  prior(normal(0,1),        class = b,    coef = safter_stim_1),
  prior(student_t(3,0,2.5), class = sds,  coef = s(after_stim)),
  # action in previous second
  prior(normal(0,0.333),    class = b,    coef = monn_tminus1_num),
  prior(dirichlet(2,2),     class = simo, coef = monn_tminus1_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
nn_prior <- brm(
  formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
    s(after_stim) + mo(nn_tminus1_num) +                       # controls
    (1|focal_id) + (1|stim_id) + (1|playback_id),              # random effects
  data = nn_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(nn_prior) # huge variation in prior, but fairly on both sides so good

## fit model
nn_fit <- brm(
  formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
    s(after_stim) + mo(nn_tminus1_num) +                       # controls
    (1|focal_id) + (1|stim_id) + (1|playback_id),              # random effects
  data = nn_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# inspect model
summary(nn_fit)

# save workspace
save.image('nearest_neighbour/neighbour_model_run_time_spline.RData')
#load('nearest_neighbour/nearest_neighbour/neighbour_model_run_time_spline.RData') ; rm(biologylibs, homedrive, homelibs, homelibsprofile,rlibs,Rversion)

#### check outputs ####
library(LaplacesDemon)
#load('nearest_neighbour/neighbour_model_run_time_spline.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()

## check Stan code
nn_fit$model

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

## extract marginal effects
marg <- conditional_effects(nn_fit,
                            effects = c('f_age_num','stim_type',
                                        'after_stim','nn_tminus1_num'),
                            categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
f_age_effect <- marg[[1]]
stim_effect <- marg[[2]]
time_effect <- marg[[3]]
prevsec_effect <- marg[[4]]

#### plot marginal effects ####
# conditional_effects(nn_fit, effects = 'f_age_num',
#                     categorical = TRUE,
#                     spaghetti = TRUE,
#                     #conditions = c('stim_type'),
#                     method = 'posterior_epred')
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

# conditional_effects(nn_fit, 'stim_type',
#                     categorical = TRUE,
#                     method = 'posterior_epred')
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

rm(f_age_effect,prevsec_effect, stim_effect, time_effect) ;gc()

#### posterior predictive check ####
pp_check(nn_fit, ndraws = 100) # perfect fit

#### plot traces ####
draws %>% 
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "bs_safter_stim_1","sds_safter_stim_1",
                          "s_safter_stim_1[1]","s_safter_stim_1[2]",
                          "s_safter_stim_1[3]","s_safter_stim_1[4]",
                          "s_safter_stim_1[5]","s_safter_stim_1[6]",
                          "s_safter_stim_1[7]","s_safter_stim_1[8]",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "bsp_monn_tminus1_num",
                          "simo_monn_tminus1_num1[1]","simo_monn_tminus1_num1[2]"
  )) %>% 
  ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mostly fine, but playback ID intercept has a weird unmixed bit

#### plot density curves ####
unique(draws$parameter)
draws_cut <- draws %>% 
  filter(parameter %in% c('b_Intercept[1]','b_Intercept[2]',
                          'b_stim_typel','b_stim_typeh',
                          'bsp_mof_age_num','simo_mof_age_num1[1]',
                          'simo_mof_age_num1[2]','simo_mof_age_num1[3]',
                          'bsp_monn_tminus1_num',
                          'simo_monn_tminus1_num1[1]','simo_monn_tminus1_num1[2]',
                          'bs_safter_stim_1','sds_safter_stim_1',
                          's_safter_stim_1[1]','s_safter_stim_1[2]',
                          's_safter_stim_1[3]','s_safter_stim_1[4]',
                          's_safter_stim_1[5]','s_safter_stim_1[6]',
                          's_safter_stim_1[7]','s_safter_stim_1[8]'))

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

## neighbour in previous second
prevsec1 <- draws_cut %>% filter(parameter == 'bsp_monn_tminus1_num')
prevsec2 <- draws_cut %>% filter(parameter == 'simo_monn_tminus1_num1[1]')
prevsec3 <- draws_cut %>% filter(parameter == 'simo_monn_tminus1_num1[2]')
par(mfrow = c(3,1))
plot(density(prevsec1$draw), main = 't-1 younger') ; abline(v = 0, lty = 2)
plot(density(prevsec2$draw), main = 't-1 matched') ; abline(v = 0, lty = 2)
plot(density(prevsec3$draw), main = 't-1 older') ; abline(v = 0, lty = 2)

## time since stimulus
timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
par(mfrow = c(5,2))
plot(density(timeb$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
plot(density(times$draw), main = 'time spline 0') ; abline(v = 0, lty = 2)
plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)

#### plot raw ####
par(mfrow = c(1,1))
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

#### predict from model ####
#load('nearest_neighbour/neighbour_model_run_time_spline.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()

## check Stan code
rm(list = ls()[! ls() %in% c('nn_fit','nn_no_na')]) ; gc()
subjects <- sample(unique(nn_no_na$focal_id), 5, replace = F)
stimuli <- sample(unique(nn_no_na$stim_id), 5, replace = F)
pbs <- sample(unique(nn_no_na$playback_id), 5, replace = F)
predict_data <- data.frame(f_age_num = rep(1, 3*16*3*length(subjects)*length(stimuli)*length(pbs)),
                           stim_type = rep(c('ctd','h','l'),
                                           each = 16*3*length(subjects)*length(stimuli)*length(pbs)),
                           after_stim = rep(rep(seq(from = 0, to = 300, by = 20),
                                                     each = 3*length(subjects)*length(stimuli)*length(pbs)),
                                                 3),
                           nn_tminus1_num = rep(rep(1:3,
                                                    each = length(subjects)*length(stimuli)*length(pbs)),
                                                3*16),
                           focal_id = rep(rep(subjects,
                                              each = length(stimuli)*length(pbs)),
                                          3*16*3),
                           stim_id = rep(rep(stimuli,
                                             each = length(pbs)),
                                         3*16*3*length(subjects)),
                           playback_id = rep(pbs, 3*16*3*length(subjects)*length(stimuli)))
pred <- posterior_predict(object = nn_fit,
                          newdata = predict_data)
age_types <- 1:4
pred_all <- array(data = NA, dim = c(nrow(pred), ncol(pred), length(age_types)),
                  dimnames = list(rownames(pred), colnames(pred),
                                  age_types))
pred_all[,,1] <- pred
save.image('nearest_neighbour/neighbour_model_predictions_time_spline.RData')
for(i in 2:length(age_types)){
  predict_data$f_age_num <- age_types[i]
  pred <- posterior_predict(object = nn_fit,
                            newdata = predict_data)
  pred_all[,,i] <- pred
  save.image('nearest_neighbour/neighbour_model_predictions_time_spline.RData')
}

load('nearest_neighbour/neighbour_model_predictions_time_spline.RData')
predict_data$num <- row_number(predict_data)
predictions <- pred_all[,,age_types[1]] %>% 
  as.data.frame()
predictions <- predictions[1:100,] %>% 
  pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
  separate(Vnum, into = c('v','num'), sep = 1) %>% 
  select(-v) %>% 
  mutate(focal_age = age_types[1],
         num = as.numeric(num)) %>% 
  left_join(predict_data[,2:ncol(predict_data)], by = 'num')
for(i in 2:length(age_types)){
  pred <- pred_all[,,age_types[i]] %>% 
    as.data.frame()
  pred <- pred[1:100,] %>% 
    pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
    separate(Vnum, into = c('v','num'), sep = 1) %>% 
    select(-v) %>% 
    mutate(focal_age = age_types[i],
           num = as.numeric(num)) %>% 
    left_join(predict_data[,2:ncol(predict_data)], by = 'num')
  predictions <- rbind(predictions, pred)
}
save.image('nearest_neighbour/neighbour_model_predictions_time_spline.RData')

#### compare to log cumulative odds of data ####
## raw log cumulative odds
prop_data <- table(nn_no_na$age_diff_num) / nrow(nn_no_na)
cum_prop_data <- cumsum(prop_data)
log_cum_odds_data <- logit(cum_prop_data)

## predicted log cumulative odds
prop_pred <- table(predictions$prediction) / nrow(predictions)
cum_prop_pred <- cumsum(prop_pred)
log_cum_odds_pred <- logit(cum_prop_pred)

## compare
prop_data ; prop_pred                   # are quite different, but there are a lot more time=0 points in the data than in the predictions
cum_prop_data ; cum_prop_pred
log_cum_odds_data ; log_cum_odds_pred

#### plot outputs ####
load('nearest_neighbour/neighbour_model_predictions_time_spline.RData')
ggplot(predictions, aes(x = after_stim, y = prediction))+
  geom_line()+
  geom_point()+
  facet_wrap(. ~ stim_type)

## plot raw data -- this looks a lot more exciting at first glance than it is: basically just shows that far and away the strongest effect is neighbour age at second t-1
stimuli <- c('dove (control)','lion','human')
names(stimuli) <- c('ctd','l','h')
nn_no_na %>% 
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>% 
  ggplot(aes(x = after_stim, y = age_diff_num,
             colour = stim_type, shape = nn_tminus1))+
  geom_vline(aes(xintercept = 0))+
  geom_point(alpha = 0.1)+ # colour = rgb(0,0,1,0.01)
  facet_grid(stim_type ~ factor(nn_tminus1,
                                levels = c('partner younger',
                                           'matched',
                                           'partner older')),
             labeller = labeller(stim_type = stimuli))

## plot predictions
summary(nn_fit) # CAN I SEE THE CUTPOINTS OUTPUT FROM THE MODEL LIKE THIS?? WHAT VALUES ON THE CUMULATIVE LOG ODDS SCALE MAKE IT TRANSITION TO PREDICTING A 2 INSTEAD OF A 1 OR 3 INSTEAD OF 2??
# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: age_diff_num ~ 1 + mo(f_age_num) + stim_type + s(after_stim) + mo(nn_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id) 
# Data: nn_no_na (Number of observations: 40010) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Smooth Terms: 
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(safter_stim_1)     0.57      0.53     0.02     1.95 1.01      682     2194
# 
# Group-Level Effects: 
#   ~focal_id (Number of levels: 140) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     6.19      0.66     4.97     7.46 1.02      217     1398
# 
#   ~playback_id (Number of levels: 33) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.86      0.71     0.03     2.85 1.05       67       14
# 
#   ~stim_id (Number of levels: 23) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.70      0.52     0.03     1.94 1.01      334      632
# 
# Population-Level Effects: 
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]         2.78      0.88     1.02     4.47 1.01      620     1615
# Intercept[2]        12.61      0.89    10.82    14.34 1.01      716     1679
# stim_typeh           0.26      0.80    -1.37     1.77 1.02     1348     2135
# stim_typel          -0.01      0.81    -1.55     1.56 1.01      445     2153
# safter_stim_1       -0.28      0.76    -1.57     1.36 1.01      687     3193
# mof_age_num         -0.75      0.25    -1.23    -0.23 1.01     1001     1263
# monn_tminus1_num     7.98      0.09     7.80     8.16 1.01     3183     2322
# 
# Simplex Parameters: 
#                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mof_age_num1[1]          0.20      0.14     0.03     0.52 1.01      374     2712
# mof_age_num1[2]          0.59      0.18     0.18     0.88 1.02     1518     2643
# mof_age_num1[3]          0.21      0.15     0.03     0.57 1.03      126     2466
# monn_tminus1_num1[1]     0.51      0.01     0.49     0.53 1.01      410     2636
# monn_tminus1_num1[2]     0.49      0.01     0.47     0.51 1.01      410     2636
# 
# Family Specific Parameters: 
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning messages:
# 1: Parts of the model have not converged (some Rhats are > 1.05). Be careful when analysing the results! We recommend running more iterations and/or setting stronger priors. 
# 2: There were 371 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 

fixed <- summary(nn_fit)$fixed
focal_id <- summary(nn_fit)$random$focal_id %>% 
  as.data.frame()
pb_id <- summary(nn_fit)$random$playback_id %>% 
  as.data.frame()
stim_id <- summary(nn_fit)$random$stim_id %>% 
  as.data.frame()
mo <- summary(nn_fit)$mo %>% 
  as.data.frame()
coef <- rbind(fixed, mo, focal_id, pb_id, stim_id) %>% 
  janitor::clean_names()
rm(fixed, mo, focal_id, pb_id, stim_id)
coef$coef <- rownames(coef)

# With a 1 unit increase in focal age (i.e., changing from one level to the next of the categorical predictor), the predicted odds of observing Y = 3 versus Y = 1 or 2 change by a factor of exp(beta) which, for diagram, is exp(-0.758) = 0.469
coef_exp <- coef %>% 
  mutate(estimate = exp(estimate),
         est_error = exp(est_error),
         l_95_percent_ci = exp(l_95_percent_ci),
         u_95_percent_ci = exp(u_95_percent_ci),
         rhat = round(rhat, 2),
         bulk_ess = round(bulk_ess),
         tail_ess = round(tail_ess)) %>% 
  rename(lwr = l_95_percent_ci,
         upr = u_95_percent_ci) %>% 
  relocate(coef)

## NEED TO DOUBLE CHECK TO SEE WHETHER ENGINE IS SUBTRACTING OR ADDING THE SLOPE VALUE: ON LOG ODDS SCALE, A NEGATIVE CHANGE INDUCES AN INCREASE IN THE OBSERVED CATEGORY VALUE, SO NORMALLY SUBTRACT THE SLOPE NOT ADD.

















#### plot predictions ####
## take predictions from model. Determine from predictions the probability of each output depending on each set of input = 0 seconds + 10-15 years + ctd stimulus + younger partner at previous time step.
head(predictions)

## remove individual variation
pred_prop <- predictions %>% 
  select(-num,-focal_id,-stim_id,-playback_id) %>% 
  distinct()

## create proportional data frame
ages <- unique(predictions$focal_age) ; stims <- unique(predictions$stim_type) ; after_stims <- unique(predictions$after_stim) ; prevsecs <- unique(predictions$nn_tminus1_num) ; predcns <- unique(predictions$prediction)
pred_prop <- data.frame(focal_age = rep(ages,
                                        each = length(stims)*length(after_stims)*length(prevsecs)*length(predcns)),
                        stim_type = rep(rep(stims, length(ages)),
                                        each = length(after_stims)*length(prevsecs)*length(predcns)),
                        after_stim = rep(rep(after_stims, length(ages)*length(stims)),
                                         each = length(prevsecs)*length(predcns)),
                        nn_tminus1_num = rep(rep(prevsecs, length(ages)*length(stims)*length(after_stims)),
                                             each = length(predcns)),
                        prediction = rep(predcns, length(ages)*length(stims)*length(after_stims)*length(prevsecs))) %>% 
  mutate(fixed = paste0(focal_age,'_',stim_type,'_',after_stim,'_',nn_tminus1_num),
         count_predictions = NA,
         count_total = NA,
         proportion = NA)
predictions <- predictions %>% 
  mutate(fixed = paste0(focal_age,'_',stim_type,'_',after_stim,'_',nn_tminus1_num))
for(i in 1:length(unique(pred_prop$fixed))){
  x <- predictions %>%
    filter(fixed == unique(pred_prop$fixed)[i])
  for(j in 1:length(predcns)){
    pred_prop$count_predictions[which(pred_prop$fixed == unique(pred_prop$fixed)[i] &
                                 pred_prop$prediction == predcns[j])] <- length(which(x$prediction == predcns[j]))
    pred_prop$count_total[which(pred_prop$fixed == unique(pred_prop$fixed)[i] &
                                  pred_prop$prediction == predcns[j])] <- nrow(x)
  }
  if(i %% 10 == 0){ rm(x) ; gc() }
  if(i == length(unique(pred_prop$fixed))) { rm(x) ; gc() }
}
pred_prop$proportion <- pred_prop$count_predictions / pred_prop$count_total

## plot proportions, one plot per stim type
prevsec_labels <- c('neighbour younger at t-1',
                    'neighbour same age at t-1',
                    'neighbour older at t-1')
names(prevsec_labels) <- 1:3
pred_prop_plot <- pred_prop %>% 
  filter(after_stim %in% c(0, 10, 20, 40, 80, 100, 150, 200, 300)) %>% 
  mutate(pred_label = ifelse(prediction == 1, 'neighbour younger',
                             ifelse(prediction == 2, 'age matched',
                                    ifelse(prediction == 3, 'neighbour older',NA)))) %>% 
  mutate(pred_label = factor(pred_label,
                             levels = c('neighbour younger',
                                        'age matched',
                                        'neighbour older')))
ctd_plot <- pred_prop_plot %>% 
  filter(stim_type == 'ctd') %>% 
  ggplot(aes(x = focal_age, y = proportion, fill = as.factor(pred_label)))+
  geom_col()+
  facet_grid(nn_tminus1_num ~ as.factor(after_stim),
             labeller = labeller(nn_tminus1_num = prevsec_labels))+
  scale_fill_viridis_d()+
  labs(fill = 'predicted age of neighbour relative to focal:',
       x = 'age category of focal elephant',
       y = 'proportion of predictions')+
  theme(legend.position = 'bottom')
lion_plot <- pred_prop_plot %>% 
  filter(stim_type == 'l') %>% 
  ggplot(aes(x = focal_age, y = proportion, fill = as.factor(pred_label)))+
  geom_col()+
  facet_grid(nn_tminus1_num ~ as.factor(after_stim),
             labeller = labeller(nn_tminus1_num = prevsec_labels))+
  scale_fill_viridis_d()+
  labs(fill = 'predicted age of neighbour relative to focal:',
       x = 'age category of focal elephant',
       y = 'proportion of predictions')+
  theme(legend.position = 'bottom')
human_plot <- pred_prop_plot %>% 
  filter(stim_type == 'h') %>% 
  ggplot(aes(x = focal_age, y = proportion, fill = as.factor(pred_label)))+
  geom_col()+
  facet_grid(nn_tminus1_num ~ as.factor(after_stim),
             labeller = labeller(nn_tminus1_num = prevsec_labels))+
  scale_fill_viridis_d()+
  labs(fill = 'predicted age of neighbour relative to focal:',
       x = 'age category of focal elephant',
       y = 'proportion of predictions')+
  theme(legend.position = 'bottom')
(all_plots <- ggarrange(ctd_plot, lion_plot, human_plot, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom"))
ggsave(plot = all_plots, filename = '../outputs/nn_posteriorpredictions_stimtype.png', device = 'png',
       width = (5.8*3), height = 8.3)

## plot proportions, one plot per action in previous second
stim_labels <- c('dove (control)', 'lion', 'human')
names(stim_labels) <- c('ctd','l','h')
(plot1 <- pred_prop_plot %>% 
  filter(nn_tminus1_num == 1) %>% 
  ggplot(aes(x = focal_age, y = proportion, fill = as.factor(pred_label)))+
  geom_col()+
  facet_grid(stim_type ~ as.factor(after_stim),
             labeller = labeller(stim_type = stim_labels))+
  scale_fill_viridis_d()+
  labs(fill = 'predicted age of neighbour relative to focal:',
       x = 'age category of focal elephant',
       y = 'proportion of predictions')+
  theme(legend.position = 'bottom'))
(plot2 <- pred_prop_plot %>% 
    filter(nn_tminus1_num == 2) %>% 
    ggplot(aes(x = focal_age, y = proportion, fill = as.factor(pred_label)))+
    geom_col()+
    facet_grid(stim_type ~ as.factor(after_stim),
               labeller = labeller(stim_type = stim_labels))+
    scale_fill_viridis_d()+
    labs(fill = 'predicted age of neighbour relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions')+
    theme(legend.position = 'bottom'))
(plot3 <- pred_prop_plot %>% 
    filter(nn_tminus1_num == 3) %>% 
    ggplot(aes(x = focal_age, y = proportion, fill = as.factor(pred_label)))+
    geom_col()+
    facet_grid(stim_type ~ as.factor(after_stim),
               labeller = labeller(stim_type = stim_labels))+
    scale_fill_viridis_d()+
    labs(fill = 'predicted age of neighbour relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions')+
    theme(legend.position = 'bottom'))
all_plots <- ggarrange(plot1, plot2, plot3, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")
ggsave(plot = all_plots, filename = '../outputs/nn_posteriorpredictions_prevsec.png', device = 'png',
       width = (5.8*3), height = 8.3)
save.image('nearest_neighbour/neighbour_model_predictions_time_spline.RData')

## calculate max and min probability at each time
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)

(plot1 <- pred_prop %>% 
  filter(nn_tminus1_num == 1) %>% 
  mutate(predict_label = ifelse(prediction == 1, 'younger',
                                ifelse(prediction == 2, 'age matched',
                                       'older'))) %>% 
  mutate(predict_label = factor(predict_label,
                                levels = c('younger','age matched','older'))) %>% 
  ggplot(aes(x = after_stim, y = proportion,
             #lty = as.factor(nn_tminus1_num),
             colour = as.factor(predict_label)))+
  geom_line(linewidth = 1)+
  facet_grid(focal_age ~ stim_type,
             labeller = labeller(focal_age = age_labels,
                                 stim_type = stim_labels))+
  scale_colour_viridis_d()+
  labs(colour = 'neighbour age:',
       x = 'seconds since stimulus started',
       y = 'predicted probability of neighbour age',
       title = 'nearest neighbour younger in previous second')+
  scale_y_continuous(limits = c(0,1)) )
(plot2 <- pred_prop %>% 
    filter(nn_tminus1_num == 2) %>% 
    mutate(predict_label = ifelse(prediction == 1, 'younger',
                                  ifelse(prediction == 2, 'age matched',
                                         'older'))) %>% 
    mutate(predict_label = factor(predict_label,
                                  levels = c('younger','age matched','older'))) %>% 
    ggplot(aes(x = after_stim, y = proportion,
               #lty = as.factor(nn_tminus1_num),
               colour = as.factor(predict_label)))+
    geom_line(linewidth = 1)+
    facet_grid(focal_age ~ stim_type,
               labeller = labeller(focal_age = age_labels,
                                   stim_type = stim_labels))+
    scale_colour_viridis_d()+
    labs(colour = 'neighbour age:',
         x = 'seconds since stimulus started',
         y = 'predicted probability of neighbour age',
         title = 'nearest neighbour age matched in previous second')+
    scale_y_continuous(limits = c(0,1)) )
(plot3 <- pred_prop %>% 
    filter(nn_tminus1_num == 3) %>% 
    mutate(predict_label = ifelse(prediction == 1, 'younger',
                                  ifelse(prediction == 2, 'age matched',
                                         'older'))) %>% 
    mutate(predict_label = factor(predict_label,
                                  levels = c('younger','age matched','older'))) %>% 
    ggplot(aes(x = after_stim, y = proportion,
               #lty = as.factor(nn_tminus1_num),
               colour = as.factor(predict_label)))+
    geom_line(linewidth = 1)+
    facet_grid(focal_age ~ stim_type,
               labeller = labeller(focal_age = age_labels,
                                   stim_type = stim_labels))+
    scale_colour_viridis_d()+
    labs(colour = 'neighbour age:',
         x = 'seconds since stimulus started',
         y = 'predicted probability of neighbour age',
         title = 'nearest neighbour older in previous second')+
    scale_y_continuous(limits = c(0,1)) )
all_plots <- ggarrange(plot1, plot2, plot3, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")
ggsave(plot = all_plots, filename = '../outputs/nn_posteriorpredictions_prevsec_line.png', device = 'png',
       width = (5.8*3), height = 8.3)
save.image('nearest_neighbour/neighbour_model_predictions_time_spline.RData')

#### extract outputs from predictions ####
#CALCULATE POSTERIOR CONTRASTS FROM PREDICTIONS
# load('nearest_neighbour/neighbour_model_predictions_time_spline.RData') ; rm(all_plots, ctd_plot, human_plot, lion_plot, plot1, plot2, plot3, pred_prop_plot, pred, after_stims, age_labels, ages, i, j, predcns, prevsec_labels, prevsecs, stim_labels, stims)

## stim type
ctd <- predictions %>% filter(stim_type == 'ctd')
lion <- predictions %>% filter(stim_type == 'l')
human <- predictions %>% filter(stim_type == 'h')
par(mfrow = c(3,1))
barplot(table(ctd$prediction), main = 'dove')
barplot(table(lion$prediction), main = 'lion')
barplot(table(human$prediction), main = 'human')

lion <- pred_prop %>% filter(stim_type == 'l')
human <- pred_prop %>% filter(stim_type == 'h')

pred_stim <- pred_prop %>% 
  filter(stim_type == 'ctd') %>% 
  rename(count_ctd = count_predictions,
         prop_ctd = proportion) %>% 
  select(focal_age, after_stim, nn_tminus1_num, prediction, count_ctd, prop_ctd) %>% 
  left_join(lion[,c('focal_age','after_stim','nn_tminus1_num','prediction','count_predictions','proportion')],
            by = c('focal_age','after_stim','nn_tminus1_num','prediction')) %>% 
  rename(count_lion = count_predictions,
         prop_lion = proportion) %>% 
  left_join(human[,c('focal_age','after_stim','nn_tminus1_num','prediction','count_predictions','proportion')],
            by = c('focal_age','after_stim','nn_tminus1_num','prediction')) %>% 
  rename(count_human = count_predictions,
         prop_human = proportion) %>% 
  mutate(ctd_lion = prop_ctd - prop_lion,
         ctd_human = prop_ctd - prop_human,
         lion_human = prop_lion - prop_human)

plot(density(pred_stim$ctd_lion), main = 'dove - lion') ; abline(v = 0, lty = 2)
plot(density(pred_stim$ctd_human), main = 'dove - human') ; abline(v = 0, lty = 2)
plot(density(pred_stim$lion_human), main = 'lion - human') ; abline(v = 0, lty = 2)

## focal age
age1 <- predictions %>% filter(focal_age == 1)
age2 <- predictions %>% filter(focal_age == 2)
age3 <- predictions %>% filter(focal_age == 3)
age4 <- predictions %>% filter(focal_age == 4)
par(mfrow = c(2,2))
barplot(table(age1$prediction), main = 'age1')
barplot(table(age2$prediction), main = 'age2')
barplot(table(age3$prediction), main = 'age3')
barplot(table(age4$prediction), main = 'age4')

age2 <- pred_prop %>% filter(focal_age == 2)
age3 <- pred_prop %>% filter(focal_age == 3)
age4 <- pred_prop %>% filter(focal_age == 4)

pred_stim <- pred_prop %>% 
  filter(focal_age == 1) %>% 
  rename(count_1 = count_predictions,
         prop_1 = proportion) %>% 
  select(stim_type, after_stim, nn_tminus1_num, prediction, count_1, prop_1) %>% 
  left_join(age2[,c('stim_type','after_stim','nn_tminus1_num','prediction','count_predictions','proportion')],
            by = c('stim_type','after_stim','nn_tminus1_num','prediction')) %>% 
  rename(count_2 = count_predictions,
         prop_2 = proportion) %>% 
  left_join(age3[,c('stim_type','after_stim','nn_tminus1_num','prediction','count_predictions','proportion')],
            by = c('stim_type','after_stim','nn_tminus1_num','prediction')) %>% 
  rename(count_3 = count_predictions,
         prop_3 = proportion) %>% 
  left_join(age4[,c('stim_type','after_stim','nn_tminus1_num','prediction','count_predictions','proportion')],
            by = c('stim_type','after_stim','nn_tminus1_num','prediction')) %>% 
  rename(count_4 = count_predictions,
         prop_4 = proportion) %>% 
  mutate(age1_2 = prop_1 - prop_2,
         age1_3 = prop_1 - prop_3,
         age1_4 = prop_1 - prop_4,
         age2_3 = prop_2 - prop_3,
         age2_4 = prop_2 - prop_4,
         age3_4 = prop_3 - prop_4)

par(mfrow = c(3,2))
plot(density(pred_stim$age1_2), main = 'age 1 - age 2') ; abline(v = 0, lty = 2)
plot(density(pred_stim$age1_3), main = 'age 1 - age 3') ; abline(v = 0, lty = 2)
plot(density(pred_stim$age1_4), main = 'age 1 - age 4') ; abline(v = 0, lty = 2)
plot(density(pred_stim$age2_3), main = 'age 2 - age 3') ; abline(v = 0, lty = 2)
plot(density(pred_stim$age2_4), main = 'age 2 - age 4') ; abline(v = 0, lty = 2)
plot(density(pred_stim$age3_4), main = 'age 3 - age 4') ; abline(v = 0, lty = 2)

## neighbour in previous second
prevsec1 <- predictions %>% filter(nn_tminus1_num == 1)
prevsec2 <- predictions %>% filter(nn_tminus1_num == 2)
prevsec3 <- predictions %>% filter(nn_tminus1_num == 3)
par(mfrow = c(3,1))
barplot(table(prevsec1$prediction), main = 't-1=1')
barplot(table(prevsec2$prediction), main = 't-1=2')
barplot(table(prevsec3$prediction), main = 't-1=3')

prevsec2 <- pred_prop %>% filter(nn_tminus1_num == 2)
prevsec3 <- pred_prop %>% filter(nn_tminus1_num == 3)

pred_stim <- pred_prop %>% 
  filter(nn_tminus1_num == 1) %>% 
  rename(count_1 = count_predictions,
         prop_1 = proportion) %>% 
  select(focal_age, after_stim, stim_type, prediction, count_1, prop_1) %>% 
  left_join(prevsec2[,c('focal_age','after_stim','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','after_stim','stim_type','prediction')) %>% 
  rename(count_2 = count_predictions,
         prop_2 = proportion) %>% 
  left_join(prevsec3[,c('focal_age','after_stim','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','after_stim','stim_type','prediction')) %>% 
  rename(count_3 = count_predictions,
         prop_3 = proportion) %>% 
  mutate(nn1_2 = prop_1 - prop_2,
         nn1_3 = prop_1 - prop_3,
         nn2_3 = prop_2 - prop_3)

plot(density(pred_stim$nn1_2), main = '(t-1=1) - (t-1=2)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$nn1_3), main = '(t-1=1) - (t-1=3)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$nn2_3), main = '(t-1=2) - (t-1=3)') ; abline(v = 0, lty = 2)

## time since stimulus
time0 <- predictions %>% filter(after_stim == 0)
time20 <- predictions %>% filter(after_stim == 20)
time40 <- predictions %>% filter(after_stim == 40)
time60 <- predictions %>% filter(after_stim == 60)
time80 <- predictions %>% filter(after_stim == 80)
time100 <- predictions %>% filter(after_stim == 100)
time120 <- predictions %>% filter(after_stim == 120)
time140 <- predictions %>% filter(after_stim == 140)
time160 <- predictions %>% filter(after_stim == 160)
time180 <- predictions %>% filter(after_stim == 180)
time200 <- predictions %>% filter(after_stim == 200)
time220 <- predictions %>% filter(after_stim == 220)
time240 <- predictions %>% filter(after_stim == 240)
time260 <- predictions %>% filter(after_stim == 260)
time280 <- predictions %>% filter(after_stim == 280)
time300 <- predictions %>% filter(after_stim == 300)
par(mfrow = c(4,4))
barplot(table(time0$prediction), main = 't=0')
barplot(table(time20$prediction), main = 't=20')
barplot(table(time40$prediction), main = 't=40')
barplot(table(time60$prediction), main = 't=60')
barplot(table(time80$prediction), main = 't=80')
barplot(table(time100$prediction), main = 't=100')
barplot(table(time120$prediction), main = 't=120')
barplot(table(time140$prediction), main = 't=140')
barplot(table(time160$prediction), main = 't=160')
barplot(table(time180$prediction), main = 't=180')
barplot(table(time200$prediction), main = 't=200')
barplot(table(time220$prediction), main = 't=220')
barplot(table(time240$prediction), main = 't=240')
barplot(table(time260$prediction), main = 't=260')
barplot(table(time280$prediction), main = 't=280')
barplot(table(time300$prediction), main = 't=300')

time20 <- pred_prop %>% filter(after_stim == 20)
time40 <- pred_prop %>% filter(after_stim == 40)
time60 <- pred_prop %>% filter(after_stim == 60)
time80 <- pred_prop %>% filter(after_stim == 80)
time100 <- pred_prop %>% filter(after_stim == 100)
time120 <- pred_prop %>% filter(after_stim == 120)
time140 <- pred_prop %>% filter(after_stim == 140)
time160 <- pred_prop %>% filter(after_stim == 160)
time180 <- pred_prop %>% filter(after_stim == 180)
time200 <- pred_prop %>% filter(after_stim == 200)
time220 <- pred_prop %>% filter(after_stim == 220)
time240 <- pred_prop %>% filter(after_stim == 240)
time260 <- pred_prop %>% filter(after_stim == 260)
time280 <- pred_prop %>% filter(after_stim == 280)
time300 <- pred_prop %>% filter(after_stim == 300)

pred_stim <- pred_prop %>% 
  filter(after_stim == 0) %>% 
  rename(count_0 = count_predictions,
         prop_0 = proportion) %>% 
  select(focal_age, nn_tminus1_num, stim_type, prediction, count_0, prop_0) %>% 
  left_join(time20[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_1 = count_predictions,
         prop_1 = proportion) %>% 
  left_join(time40[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_2 = count_predictions,
         prop_2 = proportion) %>% 
  left_join(time60[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_3 = count_predictions,
         prop_3 = proportion) %>% 
  left_join(time80[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_4 = count_predictions,
         prop_4 = proportion) %>% 
  left_join(time100[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_5 = count_predictions,
         prop_5 = proportion) %>% 
  left_join(time120[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_6 = count_predictions,
         prop_6 = proportion) %>% 
  left_join(time140[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_7 = count_predictions,
         prop_7 = proportion) %>% 
  left_join(time160[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_8 = count_predictions,
         prop_8 = proportion) %>% 
  left_join(time180[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_9 = count_predictions,
         prop_9 = proportion) %>% 
  left_join(time200[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_10 = count_predictions,
         prop_10 = proportion) %>% 
  left_join(time220[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_11 = count_predictions,
         prop_11 = proportion) %>% 
  left_join(time240[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_12 = count_predictions,
         prop_12 = proportion) %>% 
  left_join(time260[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_13 = count_predictions,
         prop_13 = proportion) %>% 
  left_join(time280[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_14 = count_predictions,
         prop_14 = proportion) %>% 
  left_join(time300[,c('focal_age','nn_tminus1_num','stim_type','prediction','count_predictions','proportion')],
            by = c('focal_age','nn_tminus1_num','stim_type','prediction')) %>% 
  rename(count_15 = count_predictions,
         prop_15 = proportion) %>% 
  mutate(t0_1 = prop_0 - prop_1,
         t1_2 = prop_1 - prop_2,
         t2_3 = prop_2 - prop_3,
         t3_4 = prop_3 - prop_4,
         t4_5 = prop_4 - prop_5,
         t5_6 = prop_5 - prop_6,
         t6_7 = prop_6 - prop_7,
         t7_8 = prop_7 - prop_8,
         t8_9 = prop_8 - prop_9,
         t9_10 = prop_9 - prop_10,
         t10_11 = prop_10 - prop_11,
         t11_12 = prop_11 - prop_12,
         t12_13 = prop_12 - prop_13,
         t13_14 = prop_13 - prop_14,
         t14_15 = prop_14 - prop_15)

plot(density(pred_stim$t0_1), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t1_2), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t2_3), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t3_4), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t4_5), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t5_6), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t6_7), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t7_8), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t8_9), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t9_10), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t10_11), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t11_12), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t12_13), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t13_14), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
plot(density(pred_stim$t14_15), main = '(t=0) - (t=1)') ; abline(v = 0, lty = 2)
