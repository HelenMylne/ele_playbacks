#### information ####
# script for basic analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
#library(cmdstanr) ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/') ; library(brms) ; library(tidyverse) ; library(LaplacesDemon)
library(StanHeaders, lib.loc = '../../packages/')
library(rstan, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')
library(cmdstanr, lib.loc = '../../packages/') # library(cmdstanr)
set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/')
library(tidyverse, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')

theme_set(theme_classic())

pdf('../../outputs/looking_direction_pt1.pdf')

#### descriptive stats ####
# read in data
prop <- readRDS('../data_processed/proportions_of_time_per_behaviour.RDS') %>% 
  mutate(stim_type_full = ifelse(stim_type == 'l', 'lion',
                                 ifelse(stim_type == 'ctd', 'control', 'human')))

# per behaviour, mean and stdev
behav <- prop %>% 
  select(section, stim_type, behaviour, behavioural_category, type, age_difference) %>% 
  distinct() %>% 
  mutate(seconds_mean = NA, seconds_stdv = NA,
         prop_mean = NA, prop_stdv = NA) %>% 
  filter(!(type == 'elephant' & is.na(age_difference) == TRUE)) %>% 
  filter(!(type == 'neighbour' & is.na(age_difference) == TRUE))

for(i in 1:nrow(behav)){
  x <- prop %>%
    filter(behaviour == behav$behaviour[i] &
             stim_type == behav$stim_type[i] &
             section == behav$section[i])
  if(behav$type[i] == 'elephant' | behav$type[i] == 'neighbour'){
    x <- x %>% filter(age_difference == behav$age_difference[i])
  }
  behav$seconds_mean[i] <- mean(x$behav_seconds, na.rm = T)
  behav$seconds_stdv[i] <- sd(x$behav_seconds, na.rm = T)
  behav$prop_mean[i] <- mean(x$propn, na.rm = T)
  behav$prop_stdv[i] <- sd(x$propn, na.rm = T)
}
rm(i, x) ; gc()

write_csv(behav, '../data_processed/proportions_descriptivestats.csv')
rm(behav, prop) ; gc()

print(paste0('proportional descriptions completed at ',Sys.time()))

#### LOOKING DIRECTION: ordinal logistic regression -- USE BRMS METHOD DESCRIBED IN https://journals.sagepub.com/doi/full/10.1177/2515245918823199 ####
# https://dagitty.net/dags.html?id=dw8twK
#library(brms, lib.loc = '../../packages/') # library(brms)

# read in data
ages <- readRDS('../data_processed/elephant_behaviour_proportions.RDS') %>%
  select(pb_num, subject, targeted_elephant,    # random effects
         #stim_num,                             # unnecessary random effects
         stim_type,age_category,partner_age_category,age_difference, # exposures
         age,partner_age,focal, dyad_partner,   # helpful for reference, don't include
         #section, # need to know when stimulus was, but don't want as categorical
         group_size # control for this
  ) %>%
  mutate(group_size = ifelse(pb_num == 44, 7, group_size)) %>%
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

look <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
  select(subject,bull,pb_num,second,out_frame_name,out_frame_index,
         b1_look_name,#b1_look_index,
         b2_look_name,#b2_look_index,
         b3_look_name,#b3_look_index,
         b4_look_name,#b4_look_index,
         b5_look_name,#b5_look_index,
         b6_look_name,#b6_look_index,
         b7_look_name,#b7_look_index,
         b8_look_name#,b8_look_index,
         #b1_present_index,b2_present_index,b3_present_index,b4_present_index,
         #b5_present_index,b6_present_index,b7_present_index,b8_present_index
  ) %>%
  #pivot_longer(cols = c('b1_present_index','b2_present_index','b3_present_index','b4_present_index',
  #                      'b5_present_index','b6_present_index','b7_present_index','b8_present_index'),
  #             names_to = 'name_present', values_to = 'present') %>%
  pivot_longer(cols = c('b1_look_name','b2_look_name','b3_look_name','b4_look_name',
                        'b5_look_name','b6_look_name','b7_look_name','b8_look_name'),
               names_to = 'elephant_look', values_to = 'direction_look') %>%
  filter(is.na(direction_look) == FALSE) %>%
  filter(direction_look != 'impossible_partner') %>%
  filter(direction_look != 'out_of_sight') %>%
  mutate(look_index = as.integer(factor(direction_look,
                                        levels = c('look at directly','side-on','look directly away')))) %>%
  separate(elephant_look, into = c('dyad_partner','_look_name'), sep = 2) %>%
  select(-`_look_name`) %>%
  mutate(pb_num = as.numeric(pb_num)) %>%
  left_join(distinct(ages[,c('subject', 'age_category')]),
            by = 'subject') %>%
  left_join(distinct(ages[,c('pb_num','partner_age_category','dyad_partner')]),
            by = c('dyad_partner','pb_num')) %>%
  left_join(distinct(ages[,c('pb_num','stim_type')]),
            by = 'pb_num') %>%
  left_join(distinct(ages[,c('subject','dyad_partner','age_difference')]),
            by = c('subject','dyad_partner')) %>%
  left_join(stim_starts, by = 'pb_num') %>%
  mutate(time_since_stim = second - stim_start) %>%
  mutate(look_tminus1 = NA)

# create variable for looking direction at time t-1
subjects <- unique(look$subject)
for(i in 1:length(subjects)){
  focal <- look %>% filter(subject == subjects[i])
  look <- look %>% anti_join(focal, by = 'subject')
  for(j in 2:nrow(focal)){
    focal$look_tminus1[j] <- focal$look_index[j-1]
  }
  look <- rbind(look, focal)
}
rm(check, x, i, multiple_starts) ; gc()

# filter to remove elephants with unknown ages
look_no_na <- look %>%
  filter(is.na(age_category) == FALSE) %>%
  filter(is.na(partner_age_category) == FALSE) %>%
  filter(is.na(look_tminus1) == FALSE) %>%
  mutate(focal_id = as.integer(as.factor(subject)),
         stim_num = as.integer(as.factor(stim_num)),
         age_category = as.factor(age_category),
         partner_age_category = as.factor(partner_age_category)) %>%
  rename(looking_direction = look_index,
         focal_age = age_category,
         partner_age = partner_age_category,
         stim_id = stim_num,
         playback_id = pb_num) %>%
  mutate(after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60)) # time now in minutes, and all 0 before stimulus starts
str(look_no_na)

print(paste0('data created at ',Sys.time()))

# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
look_no_na <- look_no_na %>%
  mutate(#age_diff_num = ifelse(age_difference == 'partner younger', 1,
         #                      ifelse(age_difference == 'matched', 2, 3)),
         look_tminus1_num = ifelse(look_tminus1 == 'look at directly', 1,
                                   ifelse(look_tminus1 == 'side-on', 2, 3))) %>%
  mutate(focal_age = as.integer(focal_age), #as.factor(focal_age),
         partner_age = as.integer(partner_age),
         #age_diff_num = as.integer(age_diff_num), #as.factor(age_diff_num),
         look_tminus1_num = as.integer(look_tminus1_num)) #as.factor(look_tminus1_num))

get_prior(formula = looking_direction ~ 1 + mo(focal_age) + mo(partner_age) + stim_type +   # fixed effects
            s(after_stim) + mo(look_tminus1_num) +                                          # controls, treat time as a spline
            (1|focal_id) + (1|stim_id) + (1|playback_id),                                   # random effects
          data = look_no_na,
          family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,0.25),     class = b,    coef = mofocal_age),
  prior(dirichlet(2,2,2),   class = simo, coef = mofocal_age1),
  # partner age
  prior(normal(0,0.25),     class = b,    coef = mopartner_age),
  prior(dirichlet(2,2,2),   class = simo, coef = mopartner_age1),
  # age interaction
  #prior(normal(0,0.5),      class = b,    coef = mofocal_age:mopartner_age),
  #prior(dirichlet(2),       class = simo, coef = mofocal_age:mopartner_age1),
  #prior(dirichlet(2),       class = simo, coef = mofocal_age:mopartner_age2),
  # stim type
  prior(normal(0,1),        class = b,    coef = stim_typeh),
  prior(normal(0,1),        class = b,    coef = stim_typel),
  # time spline
  prior(normal(0,1),        class = b,    coef = safter_stim_1), #prior(student_t(3,0,2.5), class = sds, coef = s(after_stim)),
  # action in previous second
  prior(normal(0,0.333),    class = b,    coef = molook_tminus1_num),
  prior(dirichlet(2),       class = simo, coef = molook_tminus1_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
direction_look_prior <- brm(
  formula = looking_direction ~ 1 + mo(focal_age) + mo(partner_age) + stim_type + # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                                        # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                                 # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

make_stancode(formula = looking_direction ~ 1 + mo(focal_age) + mo(partner_age) + stim_type + # fixed effects
                s(after_stim) + mo(look_tminus1_num) +                                        # controls, treat time as a spline
                (1|focal_id) + (1|stim_id) + (1|playback_id),                                 # random effects
              data = look_no_na,
              family = cumulative("logit"),
              prior = priors, chains = num_chains, cores = num_chains)

pp_check(direction_look_prior) # prior expects 1 and 3 most likely, 2 least likely. data show 1 least likely, 2 middle, 3 most.

print(paste0('priors set at ',Sys.time()))

## fit model
direction_look_fit <- brm(
  formula = looking_direction ~ 1 + mo(focal_age) + mo(partner_age) + stim_type +   # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                                        # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                                 # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# inspect model
summary(direction_look_fit)

# save workspace
save.image('looking_direction/looking_direction_model_run_time_spline.RData')
#load('looking_direction/looking_direction_model_run_time_spline.RData')# ; rm(biologylibs, homedrive, homelibs, homelibsprofile,rlibs,Rversion)

# ## notify model is done!!
# library(audio, lib.loc = '../../packages')
# fname = "../../dove_sound.wav"
# sfx <- load.wave(fname)
# play(sfx)

print(paste0('model run at ',Sys.time()))

## check outputs ####
#load('looking_direction_model_run_time_spline.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()
summary(direction_look_fit)

## check Stan code
direction_look_fit$model

## calculate log cumulative odds
prop <- table(look_no_na$looking_direction) / nrow(look_no_na)
cum_prop <- cumsum(prop)
log_cum_odds <- logit(cum_prop)

## extract posterior distribution
draws <- as_draws_df(direction_look_fit) %>% 
  select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>% 
  pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>% 
  mutate(iteration = rep(rep(1:(num_iter/2),
                             each = length(unique(parameter))),
                         num_chains),
         chain = rep(1:num_chains,
                     each = length(unique(parameter))*(num_iter/2)),
         invlogit_draw = invlogit(draw))

print(paste0('posterior extracted at ',Sys.time()))

## look at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

## extract marginal effects
marg <- conditional_effects(direction_look_fit,
                            categorical = TRUE,
                            #conditions = data.frame(time_since_stim = 0,
                            #                        look_tminus1_num = 1,
                            #                        stim_type = 'ctd',
                            #                        focal_age = 1,
                            #                        partner_age = 1),
                            method = 'posterior_epred')
names(marg)
stim_effect <- marg[[1]]
time_effect <- marg[[2]]
focal_age_effect <- marg[[3]]
partner_age_effect <- marg[[4]] #agediff_effect <- marg[[4]]
prevsec_effect <- marg[[5]]

## plot marginal effects
conditional_effects(direction_look_fit, effects = 'focal_age', categorical = TRUE,
                    spaghetti = TRUE,
                    method = 'posterior_epred')
(focal_age_plot <- ggplot(focal_age_effect)+
  geom_ribbon(aes(x = focal_age, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
  geom_line(aes(x = focal_age, y = estimate__, colour = cats__), linewidth = 1)+
  xlab(label = 'focal age') + ylab('probability')+
  scale_colour_viridis_d(name = 'looking direction:',
                         breaks = c('1','2','3'),
                         labels = c('look towards', 'side on', 'look away'))+
  scale_fill_viridis_d(name = 'looking direction:',
                       breaks = c('1','2','3'),
                       labels = c('look towards', 'side on', 'look away'))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)))
ggsave(plot = focal_age_plot, filename = '../outputs/looking_marginaleffects_focalage_time_spline.png', device = 'png',
       width = 8.3, height = 5.8)

conditional_effects(direction_look_fit, effects = 'partner_age', categorical = TRUE,
                    spaghetti = TRUE,
                    method = 'posterior_epred')
(age_part_plot <- ggplot(partner_age_effect)+
  geom_ribbon(aes(x = partner_age, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
  geom_line(aes(x = partner_age, y = estimate__, colour = cats__), linewidth = 1)+
  #geom_errorbar(aes(x = partner_age, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
  #geom_point(aes(x = partner_age, y = estimate__, colour = cats__),cex = 3)+
  ylab('probability')+
  scale_colour_viridis_d(name = 'looking direction:',
                         breaks = c('1','2','3'),
                         labels = c('look towards', 'side on', 'look away'))+
  scale_fill_viridis_d(name = 'looking direction:',
                       breaks = c('1','2','3'),
                       labels = c('look towards', 'side on', 'look away'))+
  scale_x_continuous(name = 'partner age'#,
                     #breaks = c(1,2,3),
                     #labels = c('partner younger', 'same age', 'partner older'))
                     )+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
)
ggsave(plot = age_part_plot, filename = '../outputs/looking_marginaleffects_agepartner_time_spline.png', device = 'png',
       width = 8.3, height = 5.8)

conditional_effects(direction_look_fit, 'stim_type', categorical = TRUE,
                    method = 'posterior_epred')
(stim_plot <- ggplot(stim_effect)+
  geom_errorbar(aes(x = stim_type, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
  geom_point(aes(x = stim_type, y = estimate__, colour = cats__),cex = 3)+
  ylab('probability')+
  scale_colour_viridis_d(name = 'looking direction:',
                         breaks = c('1','2','3'),
                         labels = c('look towards', 'side on', 'look away'))+
  scale_fill_viridis_d(name = 'looking direction:',
                       breaks = c('1','2','3'),
                       labels = c('look towards', 'side on', 'look away'))+
  scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
                   labels = c('dove (control)', 'lion', 'human'), limits = c('ctd','l','h'))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)))
ggsave(plot = stim_plot, filename = '../outputs/looking_marginaleffects_stimtype_time_spline.png', device = 'png',
       width = 8.3, height = 5.8)

#conditional_effects(direction_look_fit, 'time_since_stim', categorical = TRUE)
#conditional_effects(direction_look_fit, 'look_tminus1_num', categorical = TRUE)

library(ggpubr)
(all_plots <- ggarrange(focal_age_plot, age_part_plot, stim_plot, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom"))
ggsave(plot = all_plots, filename = '../outputs/looking_marginaleffects_time_spline.png', device = 'png',
       width = (5.8*3), height = 8.3)

print(paste0('marginal effects plotted at ',Sys.time()))

## posterior predictive check
pp_check(direction_look_fit, ndraws = 100) # really good fit

## reset plotting because I don't trust my coding
dev.off()
# pdf('../../outputs/looking_direction_pt2.pdf')
# 
# ## plot traces
# draws %>% 
#   filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
#                           "b_stim_typeh","b_stim_typel",
#                           "safter_stim_1","sds_s(after_stim)",
#                           "bsp_mofocal_age","bsp_mopartner_age",
#                           "bsp_molook_tminus1_num",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]",
#                           "simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
#                           "simo_molook_tminus1_num1[1]","simo_molook_tminus1_num1[2]")) %>% 
#   ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mixing doesn't look brilliant, esp. for bsp_mofocal_age, but only horrendous one is playback ID
# 
# # ## plot raw
# # ggplot(look_no_na, aes(x = focal_age, y = looking_direction,
# #                        colour = age_difference))+
# #   geom_jitter(alpha = 0.1)+
# #   facet_wrap(. ~ stim_type)
# # 
# # look_no_na %>% 
# #   filter(stim_type == 'ctd') %>% 
# #   ggplot(aes(x = time_since_stim, y = looking_direction,
# #              group = focal_id))+
# #   geom_vline(aes(xintercept = 0))+
# #   geom_point(colour = rgb(0,0,1,0.01))+
# #   #geom_line()+
# #   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# # look_no_na %>% 
# #   filter(stim_type == 'h') %>% 
# #   ggplot(aes(x = time_since_stim, y = looking_direction,
# #              group = focal_id))+
# #   geom_vline(aes(xintercept = 0))+
# #   geom_point(colour = rgb(0,0,1,0.01))+
# #   #geom_line()+
# #   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# # look_no_na %>% 
# #   filter(stim_type == 'l') %>% 
# #   ggplot(aes(x = time_since_stim, y = looking_direction,
# #              group = focal_id))+
# #   geom_vline(aes(xintercept = 0))+
# #   geom_point(colour = rgb(0,0,1,0.01))+
# #   #geom_line()+
# #   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# 
# ## predict from model ####
# rm(list = ls()[! ls() %in% c('direction_look_fit','look_no_na')]) ; gc()
# subjects <- sample(unique(look_no_na$focal_id), 5, replace = F)
# stimuli <- sample(unique(look_no_na$stim_id), 5, replace = F)
# pbs <- sample(unique(look_no_na$playback_id), 5, replace = F)
# predict_data <- data.frame(focal_age = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
#                            partner_age = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
#                            stim_type = rep(c('ctd','h','l'),
#                                            each = 26*3*length(subjects)*length(stimuli)*length(pbs)),
#                            after_stim = rep(rep(seq(from = -200, to = 300, by = 20),
#                                                      each = 3*length(subjects)*length(stimuli)*length(pbs)),
#                                                  3),
#                            look_tminus1_num = rep(rep(1:3,#c('look at directly','side-on','look directly away'),
#                                                       each = length(subjects)*length(stimuli)*length(pbs)),
#                                                   3*26),
#                            focal_id = rep(rep(subjects,
#                                               each = length(stimuli)*length(pbs)),
#                                           3*26*3),
#                            stim_id = rep(rep(stimuli,
#                                              each = length(pbs)),
#                                          3*26*3*length(subjects)),
#                            playback_id = rep(pbs, 3*26*3*length(subjects)*length(stimuli)))
# pred <- posterior_predict(object = direction_look_fit,
#                           newdata = predict_data)
# age_types <- c('foc1_part1','foc1_part2','foc1_part3','foc1_part4',
#                'foc2_part1','foc2_part2','foc2_part3','foc2_part4',
#                'foc3_part1','foc3_part2','foc3_part3','foc3_part4',
#                'foc4_part1','foc4_part2','foc4_part3','foc4_part4')
# pred_all <- array(data = NA, dim = c(nrow(pred), ncol(pred), length(age_types)),
#                   dimnames = list(rownames(pred), colnames(pred),
#                                   age_types))
# pred_all[,,1] <- pred
# save.image('looking_direction_time_spline_predictions.RData')
# for(i in 2:length(age_types)){
#   predict_data$focal_age <- ifelse(i <= 4, 1,
#                                    ifelse(i <= 8, 2,
#                                           ifelse(i <= 12, 3 , 4)))
#   predict_data$partner_age <- ifelse(i %in% c(1,5,9,13), 1,
#                                       ifelse(i %in% c(2,6,10,14), 2, 
#                                              ifelse(i %in% c(3,7,11,15), 3, 4)))
#   # predict_data$focal_age <- ifelse(i < 5, 1,
#   #                                  ifelse(i < 9, 2, 3))
#   # predict_data$age_diff_num <- ifelse(i %in% c(1,5,9), 1,
#   #                                     ifelse(i %in% c(2,6,10), 2,
#   #                                            ifelse(i %in% c(3,7,11), 3, 4)))
#   pred <- posterior_predict(object = direction_look_fit,
#                             newdata = predict_data)
#   pred_all[,,i] <- pred
#   save.image('looking_direction_time_spline_predictions.RData')
# }
# 
# load('looking_direction_time_spline_predictions.RData')
# predict_data$num <- row_number(predict_data)
# predictions <- pred_all[,,age_types[1]] %>% 
#   as.data.frame()
# predictions <- predictions[1:100,] %>% 
#   pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
#   separate(Vnum, into = c('v','num'), sep = 1) %>% 
#   select(-v) %>% 
#   mutate(age_type = age_types[1],
#          num = as.numeric(num)) %>% 
#   left_join(predict_data[,3:ncol(predict_data)], by = 'num')
# for(i in 2:length(age_types)){
#   pred <- pred_all[,,age_types[i]] %>% 
#     as.data.frame()
#   pred <- pred[1:100,] %>% 
#     pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
#     separate(Vnum, into = c('v','num'), sep = 1) %>% 
#     select(-v) %>% 
#     mutate(age_type = age_types[i],
#            num = as.numeric(num)) %>% 
#     left_join(predict_data[,3:ncol(predict_data)], by = 'num')
#   predictions <- rbind(predictions, pred)
# }
# save.image('looking_direction_time_spline_predictions.RData')
# 
# age_types <- data.frame(age_type = age_types) %>% 
#   separate(age_type, into = c('focal_age','partner_age'), remove = F, sep = '_part') %>% 
#   mutate(focal_age = ifelse(focal_age == 'foc1', 1,
#                             ifelse(focal_age == 'foc2', 2,
#                                    ifelse(focal_age == 'foc3', 3, 4))),
#          partner_age = as.numeric(partner_age)) %>% 
#   select(age_type, focal_age, partner_age)
# 
# rm(pred, pbs, stimuli, subjects, i) ; gc()
# predictions <- left_join(predictions, age_types, by = 'age_type')
# rm(age_types) ; gc()
# save.image('looking_direction_time_spline_predictions.RData')
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# ## plot outputs ####
# #load('looking_direction_time_spline_predictions.RData')
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# 
# predictions <- predictions %>%
#   mutate(stimulus = ifelse(stim_type == 'ctd','dove (control)',
#                            ifelse(stim_type == 'l', 'lion', 'human')),
#          prediction_adjusted = ifelse(stim_type == 'ctd', prediction - 0.1,
#                                       ifelse(stim_type == 'h', prediction + 0.1, prediction)))
# predict_mean <- predictions %>%
#   select(-num,-age_type,-prediction, -prediction_adjusted, -focal_id, -stim_id, -playback_id) %>%
#   distinct() %>%
#   mutate(prediction_mu = NA)
# for(i in 1:nrow(predict_mean)){
#   predict_mean$prediction_mu[i] <- mean(predictions$prediction[which(predictions$stim_type == predict_mean$stim_type[i] &
#                                                                        predictions$time_since_stim == predict_mean$time_since_stim[i] &
#                                                                        predictions$look_tminus1_num == predict_mean$look_tminus1_num[i] &
#                                                                        predictions$focal_age == predict_mean$focal_age[i] &
#                                                                        predictions$partner_age == predict_mean$partner_age[i])])
# }
# save.image('looking_direction_time_spline_predictions.RData')
# 
# ggplot(predict_mean, aes(x = time_since_stim,                          # no effect of time in any graph
#                          y = prediction_mu,                            # all around 2, mean is never close to 1 or 3
#                          colour = stimulus,                            # most likely to look away for dove, then human, then lion (most likely to look at for lion, then human, then dove)
#                          shape = as.factor(look_tminus1_num)))+        # most likely to look away when previously looking away, most likely to look at when previously looking at
#   geom_line()+
#   geom_point()+
#   facet_grid(partner_age ~ focal_age,                                  # partner = rows, focal = columns
#              labeller = labeller(focal_age = age_labels,               # no effect of focal age on looking direction
#                                  partner_age = age_labels))+           # increasingly likely to look away from other individuals as they age
#   scale_x_continuous(name = 'time since stimulus started (s)')+
#   scale_y_continuous(name = 'looking direction',
#                      breaks = c(1,2,3), labels = c('look at','side on','look away'),
#                      expand = c(0,0), limits = c(0.9,3.1))
# ggsave(plot = last_plot(), filename = '../outputs/looking_predictions_time_spline.png', device = 'png', width = 8.27, height = 5.83)
# 
# ## plot raw data
# # look_no_na %>% 
# #   filter(stim_type == 'ctd') %>% 
# #   ggplot(aes(x = time_since_stim, y = looking_direction,
# #              group = focal_id))+
# #   geom_vline(aes(xintercept = 0))+
# #   geom_point(colour = rgb(0,0,1,0.01))+
# #   #geom_line()+
# #   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# # 
# # look_no_na %>% 
# #   filter(stim_type == 'h') %>% 
# #   ggplot(aes(x = time_since_stim, y = looking_direction,
# #              group = focal_id))+
# #   geom_vline(aes(xintercept = 0))+
# #   geom_point(colour = rgb(0,0,1,0.01))+
# #   #geom_line()+
# #   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# # 
# # look_no_na %>% 
# #   filter(stim_type == 'l') %>% 
# #   ggplot(aes(x = time_since_stim, y = looking_direction,
# #              group = focal_id))+
# #   geom_vline(aes(xintercept = 0))+
# #   geom_point(colour = rgb(0,0,1,0.01))+
# #   #geom_line()+
# #   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# 
# print(paste0('raw data plotted at ',Sys.time()))
# 
# ## plot predictions
# summary(direction_look_fit)
# # Family: cumulative 
# # Links: mu = logit; disc = identity 
# # Formula: looking_direction ~ 1 + mo(focal_age) + mo(partner_age) + stim_type + time_since_stim + mo(look_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id) 
# # Data: look_no_na (Number of observations: 171820) 
# # Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; total post-warmup draws = 4000
# # 
# # Group-Level Effects: 
# # ~focal_id (Number of levels: 176) 
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     1.17      0.07     1.04     1.31 1.00      843     1441
# # 
# # ~playback_id (Number of levels: 48) 
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     0.26      0.17     0.01     0.65 1.01      191      459
# # 
# # ~stim_id (Number of levels: 30) 
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     0.18      0.13     0.01     0.48 1.01      524     1201
# # 
# # Population-Level Effects: 
# #                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # Intercept[1]          -1.25      0.26    -1.77    -0.71 1.00     1376     1918
# # Intercept[2]           1.20      0.26     0.68     1.74 1.00     1378     1844
# # stim_typeh            -0.22      0.27    -0.77     0.30 1.00     1630     2373
# # stim_typel            -0.42      0.26    -0.93     0.10 1.00     1479     2052
# # time_since_stim        0.00      0.00    -0.00     0.00 1.00     3934     3124
# # mofocal_age            0.00      0.11    -0.20     0.21 1.00     1200     1638
# # mopartner_age          0.27      0.01     0.25     0.29 1.00     6206     2783
# # molook_tminus1_num     0.61      0.01     0.60     0.63 1.00     5622     2441
# # 
# # Simplex Parameters: 
# #                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # mofocal_age1[1]            0.35      0.18     0.06     0.73 1.00     6146     2964
# # mofocal_age1[2]            0.31      0.17     0.05     0.69 1.00     5656     2885
# # mofocal_age1[3]            0.33      0.18     0.05     0.71 1.00     5503     2757
# # mopartner_age1[1]          0.32      0.02     0.27     0.36 1.00     6868     3220
# # mopartner_age1[2]          0.01      0.01     0.00     0.02 1.00     4669     2236
# # mopartner_age1[3]          0.68      0.02     0.63     0.72 1.00     6565     3012
# # molook_tminus1_num1[1]     0.46      0.01     0.45     0.48 1.00     6128     2514
# # molook_tminus1_num1[2]     0.54      0.01     0.52     0.55 1.00     6128     2514
# # 
# # Family Specific Parameters: 
# #      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # disc     1.00      0.00     1.00     1.00   NA       NA       NA
# 
# # Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# # Warning message: There were 45 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html
# 
# ## want to extract an estimated probability of each value at every time point, split across the different types
# ## take predictions from model and mean across random effects? so mean+/-stdev prediction for seconds = -120, age = 10-15, age_diff = 1, stim = ctd  -- pretty sure this would be the equivalent of treating it like a continuous variable again which is definitely not right, but right now I have no other ideas!! I think I probably need to extract it directly from the model draws for each parameter, but I don't think I know how to do that...
# 
# ggplot()+
#   annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
#   geom_point(data = look_no_na,
#              mapping = aes(x = time_since_stim, y = looking_direction),
#              alpha = 0.01, shape = 19)+
#   facet_grid(partner_age ~ focal_age,
#              labeller = labeller(focal_age = age_labels, partner_age = age_labels))+
#   scale_y_continuous(name = 'looking direction',
#                      breaks = c(1,2,3), labels = c('look at','side on','look away'),
#                      expand = c(0,0))+
#   scale_x_continuous(name = 'time since stimulus (s)')
# 
# predict_means <- predictions[1:4680,] %>% 
#   select(focal_age, partner_age, stim_type, after_stim, look_tminus1_num) %>% 
#   distinct() %>% 
#   mutate(mean_pred = NA, stdv_pred = NA)
# for(i in 1:nrow(predict_means)){
#   x <- predictions$prediction[predictions$focal_age == predict_means$focal_age[i] &
#                                 predictions$stim_type == predict_means$stim_type[i] &
#                                 predictions$partner_age == predict_means$partner_age[i] &
#                                 predictions$after_stim == predict_means$after_stim[i] &
#                                 predictions$look_tminus1_num == predict_means$look_tminus1_num[i]]
#   predict_means$mean_pred[i] <- mean(x)
#   predict_means$stdv_pred[i] <- sd(x)
# }
# predict_means$lwr <- predict_means$mean_pred - predict_means$stdv_pred
# predict_means$upr <- predict_means$mean_pred + predict_means$stdv_pred
# 
# save.image('looking_direction_time_spline_predictions.RData')
# 
# age_labels <- c('partner younger','age matched','partner older')
# names(age_labels) <- c(1,2,3)
# stim_labels <- c('dove (control)','lion','human')
# names(stim_labels) <- c('ctd','l','h')
# ggplot()+
#   annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
#   geom_point(data = look_no_na,
#              mapping = aes(x = after_stim, y = looking_direction),
#              alpha = 0.01, shape = 19)+
#   facet_grid(stim_type ~ age_diff_num,
#              labeller = labeller(age_diff_num = age_labels,
#                                  stim_type = stim_labels))+
#   # geom_ribbon(data = predict_means,
#   #             mapping = aes(x = after_stim, ymax = upr, ymin = lwr,
#   #                           fill = as.factor(focal_age)),
#   #             alpha = 0.2)+
#   geom_line(data = predict_means,
#             mapping = aes(x = after_stim, y = mean_pred,
#                           colour = as.factor(focal_age),
#                           linetype = as.factor(look_tminus1_num)))+
#   scale_y_continuous(name = 'looking direction',
#                      breaks = c(1,2,3), labels = c('look at','side on','look away'),
#                      expand = c(0,0))+
#   scale_x_continuous(name = 'time since stimulus (s)')
# 
# dev.off()
# save.image('looking_direction_time_spline_predictions.RData')
# print(paste0('completed at ',Sys.time()))
# 
# 
# 
# 




