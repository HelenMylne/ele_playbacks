#### information ####
# script for basic analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
# library(tidyverse) ; library(cmdstanr) ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/') ; library(brms) ; library(LaplacesDemon)
library(StanHeaders, lib.loc = '../../packages/')
library(rstan, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')
library(tidyverse, lib.loc = '../../packages/')
#library(cmdstanr, lib.loc = '../../packages/') ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/')
library(LaplacesDemon, lib.loc = '../../packages/')

theme_set(theme_classic())

pdf('../outputs/movement_direction.pdf')

#### import data ####
# https://dagitty.net/dags.html?id=dw8twK

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

move <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>% 
  select(subject,bull,pb_num,second,out_frame_name,out_frame_index,
         b1_move_name,#b1_move_index,
         b2_move_name,#b2_move_index,
         b3_move_name,#b3_move_index,
         b4_move_name,#b4_move_index,
         b5_move_name,#b5_move_index,
         b6_move_name,#b6_move_index,
         b7_move_name,#b7_move_index,
         b8_move_name#,b8_move_index,
         #b1_present_index,b2_present_index,b3_present_index,b4_present_index,
         #b5_present_index,b6_present_index,b7_present_index,b8_present_index
  ) %>% 
  #pivot_longer(cols = c('b1_present_index','b2_present_index','b3_present_index','b4_present_index',
  #                      'b5_present_index','b6_present_index','b7_present_index','b8_present_index'),
  #             names_to = 'name_present', values_to = 'present') %>% 
  pivot_longer(cols = c('b1_move_name','b2_move_name','b3_move_name','b4_move_name',
                        'b5_move_name','b6_move_name','b7_move_name','b8_move_name'),
               names_to = 'elephant_move', values_to = 'direction_move') %>% 
  filter(is.na(direction_move) == FALSE) %>% 
  filter(direction_move != 'impossible_partner') %>% 
  filter(direction_move != 'out_of_sight') %>% 
  mutate(direction_move = ifelse(direction_move == 'approach directly',
                                 'approach_directly',
                                 ifelse(direction_move == 'approach at an angle',
                                        'approach_angle',
                                        ifelse(direction_move == 'move directly with',
                                               'move_with',
                                               ifelse(direction_move == 'move away at an angle',
                                                      'away_angle',
                                                      ifelse(direction_move == 'move away directly',
                                                             'away_directly',direction_move)))))
  ) %>%
  mutate(move_index = as.integer(factor(direction_move,
                                        levels = c('not_moving',
                                                   'approach_directly','approach_angle',
                                                   'move_with',
                                                   'away_angle','away_directly')))) %>%
  separate(elephant_move, into = c('dyad_partner','_move_name'), sep = 2) %>%
  select(-`_move_name`) %>%
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
  mutate(move_tminus1 = NA)

# create variable for movement direction at time t-1
subjects <- unique(move$subject)
for(i in 1:length(subjects)){
  focal <- move %>% filter(subject == subjects[i])
  move <- move %>% anti_join(focal, by = 'subject')
  for(j in 2:nrow(focal)){
    focal$move_tminus1[j] <- focal$move_index[j-1]
  }
  move <- rbind(move, focal)
}
rm(check, x, i, multiple_starts) ; gc()

# filter to remove elephants with unknown ages
move_no_na <- move %>% 
  filter(is.na(move_index) == FALSE) %>%   # all seconds where not moving -- feel like there should be something here, but where would it fit in the scale relative to other categories
  filter(is.na(age_category) == FALSE) %>% 
  filter(is.na(partner_age_category) == FALSE) %>% 
  filter(is.na(move_tminus1) == FALSE) %>% 
  mutate(#stim_type = ifelse(stim_type == 'h', 3,
    #                   ifelse(stim_type == 'l', 2, 1)),
    focal_id = as.integer(as.factor(subject)),
    stim_num = as.integer(as.factor(stim_num)),
    age_category = as.factor(age_category),
    partner_age_category = as.factor(partner_age_category)) %>% 
  rename(moving_direction = move_index,
         focal_age = age_category,
         partner_age = partner_age_category,
         stim_id = stim_num,
         playback_id = pb_num)
str(move_no_na)
# n_obs <- nrow(move_no_na)  # number of observations
# n_direct <- 3              # number of moving directions + 1
# move_ls <- list(           # generate data list
#   n_obs = n_obs,
#   n_direct = n_direct,
#   moving_direction = move_no_na$move_index,
#   focal_age = move_no_na$age_category,
#   partner_age = move_no_na$partner_age_category,
#   #age_difference = move_no_na$age_difference,
#   stim_type = move_no_na$stim_type,
#   time_since_stim = move_no_na$time_since_stim,
#   focal_id = move_no_na$focal_id,
#   stim_id = move_no_na$stim_num,
#   playback_id = move_no_na$pb_num)

print(paste0('data imported at ', Sys.time()))

#### set priors ####
# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
move_model_data <- move_no_na %>% 
  # This is definitely not right! not moving either needs to be excluded or combined with move_with -- mutate(move_tminus1_num = as.integer(factor(move_tminus1,
                                        levels = c('not_moving',
                                                   'approach_directly','approach_angle',
                                                   'move_with',
                                                   'away_angle','away_directly')))) %>% 
  mutate(focal_age = as.integer(focal_age),
         partner_age = as.integer(partner_age),
         move_tminus1_num = as.integer(move_tminus1_num)) %>% 
  filter(!is.na(move_tminus1_num)) %>% 
  mutate(after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60)) # time now in minutes, and all 0 before stimulus starts

get_prior(formula = moving_direction ~ 1 + mo(focal_age) + mo(partner_age) + stim_type +   # fixed effects
            s(after_stim) + mo(move_tminus1_num) +                                       # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),                                  # random effects
          data = move_model_data,
          family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,0.25),   class = b,    coef = mofocal_age),
  prior(dirichlet(2,2,2), class = simo, coef = mofocal_age1),
  # partner age
  prior(normal(0,0.25),   class = b,    coef = mopartner_age),
  prior(dirichlet(2,2,2), class = simo, coef = mopartner_age1),
  # interaction age
  prior(normal(0,0.5),    class = b,    coef = mofocal_age:mopartner_age),
  prior(dirichlet(2),     class = simo, coef = mofocal_age:mopartner_age1),
  prior(dirichlet(2),     class = simo, coef = mofocal_age:mopartner_age2),
  # stimulus type
  prior(normal(0,1),      class = b,    coef = stim_typeh),
  prior(normal(0,1),      class = b,    coef = stim_typel),
  # time spline
  prior(normal(0,1),        class = b,    coef = safter_stim_1),
  prior(student_t(3,0,2.5), class = sds,  coef = s(after_stim)),
  # action in previous second
  prior(normal(0,0.333),  class = b,    coef = momove_tminus1_num),
  prior(dirichlet(2),     class = simo, coef = momove_tminus1_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
direction_move_prior <- brm(
  formula = moving_direction ~ 1 + mo(focal_age) + mo(partner_age) + stim_type + 
    s(after_stim) + mo(move_tminus1_num) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_model_data,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(direction_move_prior) # prior expects 1 and 3 most likely, 2 least likely. data show 1 least likely, 2 middle, 3 most.

print(paste0('priors set and checked at ', Sys.time()))

#### fit model ####
direction_move_fit <- brm(
  formula = moving_direction ~ 1 + mo(focal_age) + mo(partner_age) + stim_type + 
    s(after_stim) + mo(move_tminus1_num) + 
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_model_data,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# inspect model
summary(direction_move_fit)

# save workspace
save.image('moving_direction_model_run.RData')
#load('moving_direction_model_run.RData') ; rm(biologylibs, homedrive, homelibs, homelibsprofile,rlibs,Rversion)

print(paste0('model fitted at ', Sys.time()))

# #### check outputs ####
# #load('moving_direction_model_run.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()
# summary(direction_move_fit)
# 
# ## check Stan code
# direction_move_fit$model
# 
# ## calculate log cumulative odds
# prop <- table(move_model_data$moving_direction) / nrow(move_model_data)
# cum_prop <- cumsum(prop)
# log_cum_odds <- logit(cum_prop)
# 
# print(paste0('cumulative log odds calculated at ',Sys.time()))
# 
# ## extract posterior distribution
# draws <- as_draws_df(direction_move_fit) %>% 
#   select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>% 
#   pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>% 
#   mutate(iteration = rep(rep(1:(num_iter/2),
#                              each = length(unique(parameter))),
#                          num_chains),
#          chain = rep(1:num_chains,
#                      each = length(unique(parameter))*(num_iter/2)),
#          invlogit_draw = invlogit(draw))
# 
# print(paste0('posterior draws extracted at ',Sys.time()))
# 
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,2))
# hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
# par(mfrow = c(1,1))
# 
# ## extract marginal effects
# marg <- conditional_effects(direction_move_fit, categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# stim_effect <- marg[[1]]
# time_effect <- marg[[2]]
# focal_age_effect <- marg[[3]]
# partner_age_effect <- marg[[4]] #agediff_effect <- marg[[4]]
# prevsec_effect <- marg[[5]]
# # marg <- conditional_effects(direction_move_fit, effects = 'focal_age', categorical = TRUE,
# #                             #spaghetti = TRUE,
# #                             method = 'posterior_epred')
# # names(marg)
# # age_effect <- marg[[1]]
# 
# 
# ## plot marginal effects
# conditional_effects(direction_move_fit, effects = 'focal_age',
#                     categorical = TRUE,
#                     spaghetti = TRUE,
#                     #conditions = c('partner_age','stim_type'),
#                     #int_conditions = list(focal_age = c(1:4), age_diff_num = c(1:3), stim_type = c('ctd','l','h'),
#                     method = 'posterior_epred')
# (focal_age_plot <- ggplot(focal_age_effect)+
#     geom_ribbon(aes(x = focal_age, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
#     geom_line(aes(x = focal_age, y = estimate__, colour = cats__), linewidth = 1)+
#     xlab(label = 'focal age') + ylab('probability')+
#     scale_colour_viridis_d(name = 'moving direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('move towards', 'side on', 'move away'))+
#     scale_fill_viridis_d(name = 'moving direction:',
#                          breaks = c('1','2','3'),
#                          labels = c('move towards', 'side on', 'move away')))+
#   theme(legend.position = 'bottom',
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 10))
# ggsave(plot = focal_age_plot, filename = '../outputs/moving_marginaleffects_focalage.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# conditional_effects(direction_move_fit, effects = 'partner_age', categorical = TRUE,
#                     spaghetti = TRUE,
#                     method = 'posterior_epred')
# (age_part_plot <- ggplot(partner_age_effect)+
#     geom_ribbon(aes(x = partner_age, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
#     geom_line(aes(x = partner_age, y = estimate__, colour = cats__), linewidth = 1)+
#     #geom_errorbar(aes(x = partner_age, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
#     #geom_point(aes(x = partner_age, y = estimate__, colour = cats__),cex = 3)+
#     ylab('probability')+
#     scale_colour_viridis_d(name = 'moving direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('move towards', 'side on', 'move away'))+
#     scale_fill_viridis_d(name = 'moving direction:',
#                          breaks = c('1','2','3'),
#                          labels = c('move towards', 'side on', 'move away'))+
#     scale_x_continuous(name = 'partner age'#,
#                        #breaks = c(1,2,3),
#                        #labels = c('partner younger', 'same age', 'partner older'))
#     )+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10))
# )
# ggsave(plot = age_part_plot, filename = '../outputs/moving_marginaleffects_agepartner.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# conditional_effects(direction_move_fit, 'stim_type', categorical = TRUE,
#                     method = 'posterior_epred')
# (stim_plot <- ggplot(stim_effect)+
#     geom_errorbar(aes(x = stim_type, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type, y = estimate__, colour = cats__),cex = 3)+
#     ylab('probability')+
#     scale_colour_viridis_d(name = 'moving direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('move towards', 'side on', 'move away'))+
#     scale_fill_viridis_d(name = 'moving direction:',
#                          breaks = c('1','2','3'),
#                          labels = c('move towards', 'side on', 'move away'))+
#     scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
#                      labels = c('dove (control)', 'lion', 'human'), limits = c('ctd','l','h')))+
#   theme(legend.position = 'bottom',
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 10))
# ggsave(plot = stim_plot, filename = '../outputs/moving_marginaleffects_stimtype.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# #conditional_effects(direction_move_fit, 'time_since_stim', categorical = TRUE)
# #conditional_effects(direction_move_fit, 'move_tminus1_num', categorical = TRUE)
# 
# library(ggpubr)
# (all_plots <- ggarrange(focal_age_plot, age_part_plot, stim_plot, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom"))
# ggsave(plot = all_plots, filename = '../outputs/moving_marginaleffects.png', device = 'png',
#        width = (5.8*3), height = 8.3)
# 
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# ## posterior predictive check
# pp_check(direction_move_fit, ndraws = 100)
# 
# print(paste0('posterior predictive check completed at ',Sys.time()))
# 
# ## plot traces
# draws %>% 
#   filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]","b_stim_typeh","b_stim_typel","b_time_since_stim","bsp_mofocal_age","bsp_moage_diff_num","bsp_momove_tminus1_num","sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept","simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]","simo_moage_diff_num1[1]","simo_moage_diff_num1[2]","simo_momove_tminus1_num1[1]","simo_momove_tminus1_num1[2]")) %>% 
#   ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # time since stim has ENORMOUS range (main body approx. -5000 to 10000), sd_playback_id poorly mixed
# 
# print(paste0('traceplots drawn at ',Sys.time()))
# 
# ## plot raw
# ggplot(move_model_data, aes(x = focal_age, y = moving_direction,
#                        colour = age_difference))+
#   geom_jitter(alpha = 0.1)+
#   facet_wrap(. ~ stim_type)
# 
# move_model_data %>% 
#   filter(stim_type == 'ctd') %>% 
#   ggplot(aes(x = time_since_stim, y = moving_direction,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# move_model_data %>% 
#   filter(stim_type == 'h') %>% 
#   ggplot(aes(x = time_since_stim, y = moving_direction,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# move_model_data %>% 
#   filter(stim_type == 'l') %>% 
#   ggplot(aes(x = time_since_stim, y = moving_direction,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# 
# #### predict from model ####
# rm(list = ls()[! ls() %in% c('direction_move_fit','move_model_data')]) ; gc()
# subjects <- sample(unique(move_model_data$focal_id), 5, replace = F)
# stimuli <- sample(unique(move_model_data$stim_id), 5, replace = F)
# pbs <- sample(unique(move_model_data$playback_id), 5, replace = F)
# predict_data <- data.frame(focal_age = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
#                            partner_age = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
#                            stim_type = rep(c('ctd','h','l'),
#                                            each = 26*3*length(subjects)*length(stimuli)*length(pbs)),
#                            time_since_stim = rep(rep(seq(from = -200, to = 300, by = 20),
#                                                      each = 3*length(subjects)*length(stimuli)*length(pbs)),
#                                                  3),
#                            move_tminus1_num = rep(rep(1:3,#c('move at directly','side-on','move directly away'),
#                                                       each = length(subjects)*length(stimuli)*length(pbs)),
#                                                   3*26),
#                            focal_id = rep(rep(subjects,
#                                               each = length(stimuli)*length(pbs)),
#                                           3*26*3),
#                            stim_id = rep(rep(stimuli,
#                                              each = length(pbs)),
#                                          3*26*3*length(subjects)),
#                            playback_id = rep(pbs, 3*26*3*length(subjects)*length(stimuli)))
# pred <- posterior_predict(object = direction_move_fit,
#                           newdata = predict_data)
# age_types <- c('foc1_part1','foc1_part2','foc1_part3','foc1_part4',
#                'foc2_part1','foc2_part2','foc2_part3','foc2_part4',
#                'foc3_part1','foc3_part2','foc3_part3','foc3_part4',
#                'foc4_part1','foc4_part2','foc4_part3','foc4_part4')
# pred_all <- array(data = NA, dim = c(nrow(pred), ncol(pred), length(age_types)),
#                   dimnames = list(rownames(pred), colnames(pred),
#                                   age_types))
# pred_all[,,1] <- pred
# save.image('moving_direction_predictions.RData')
# for(i in 2:length(age_types)){
#   predict_data$focal_age <- ifelse(i <= 4, 1,
#                                    ifelse(i <= 8, 2,
#                                           ifelse(i <= 12, 3 , 4)))
#   predict_data$partner_age <- ifelse(i %in% c(1,5,9,13), 1,
#                                      ifelse(i %in% c(2,6,10,14), 2, 
#                                             ifelse(i %in% c(3,7,11,15), 3, 4)))
#   # predict_data$focal_age <- ifelse(i < 5, 1,
#   #                                  ifelse(i < 9, 2, 3))
#   # predict_data$age_diff_num <- ifelse(i %in% c(1,5,9), 1,
#   #                                     ifelse(i %in% c(2,6,10), 2,
#   #                                            ifelse(i %in% c(3,7,11), 3, 4)))
#   pred <- posterior_predict(object = direction_move_fit,
#                             newdata = predict_data)
#   pred_all[,,i] <- pred
#   save.image('moving_direction_predictions.RData')
# }
# 
# load('moving_direction_predictions.RData')
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
# save.image('moving_direction_predictions.RData')
# 
# age_types <- data.frame(age_type = age_types) %>% 
#   separate(age_type, into = c('age','age_diff_num'), remove = F, sep = '_diff') %>% 
#   mutate(focal_age = ifelse(age == 'age1', 1,
#                             ifelse(age == 'age2', 2,
#                                    ifelse(age == 'age3', 3, 4))),
#          age_diff_num = as.numeric(age_diff_num)) %>% 
#   select(age_type, focal_age, age_diff_num)
# 
# rm(pred, pbs, stimuli, subjects, i) ; gc()
# predictions <- left_join(predictions, age_types, by = 'age_type')
# rm(age_types) ; gc()
# save.image('moving_direction_predictions.RData')
# 
# print(paste0('predictions made at ',Sys.time()))
# 
# #### plot outputs ####
# load('moving_direction_predictions.RData')
# ggplot(predictions, aes(x = time_since_stim, y = prediction))+
#   geom_line()+
#   geom_point()+
#   facet_wrap(.~stim_type)
# 
# ## plot raw data
# move_model_data %>% 
#   filter(stim_type == 'ctd') %>% 
#   ggplot(aes(x = time_since_stim, y = moving_direction,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# 
# move_model_data %>% 
#   filter(stim_type == 'h') %>% 
#   ggplot(aes(x = time_since_stim, y = moving_direction,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# 
# move_model_data %>% 
#   filter(stim_type == 'l') %>% 
#   ggplot(aes(x = time_since_stim, y = moving_direction,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
# 
# ## plot predictions
# summary(direction_move_fit)
# # Family: cumulative 
# # Links: mu = logit; disc = identity 
# # Formula: 
# 
# ## want to extract an estimated probability of each value at every time point, split across the different types
# ## take predictions from model and mean across random effects? so mean+/-stdev prediction for seconds = -120, age = 10-15, age_diff = 1, stim = ctd  -- pretty sure this would be the equivalent of treating it like a continuous variable again which is definitely not right, but right now I have no other ideas!! I think I probably need to extract it directly from the model draws for each parameter, but I don't think I know how to do that...
# 
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# ggplot()+
#   annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
#   geom_point(data = move_model_data,
#              mapping = aes(x = time_since_stim, y = moving_direction),
#              alpha = 0.01, shape = 19)+
#   # geom_violin(data = move_model_data,
#   #             mapping = aes(y = time_since_stim, x = as.factor(moving_direction)))+
#   # coord_flip()+
#   facet_wrap(. ~ focal_age,
#              labeller = labeller(focal_age = age_labels))+
#   scale_y_continuous(name = 'moving direction',
#                      breaks = c(1,2,3), labels = c('move at','side on','move away'),
#                      expand = c(0,0))+
#   scale_x_continuous(name = 'time since stimulus (s)')
# 
# predict_means <- predictions %>% 
#   select(focal_age, age_diff_num, stim_type, time_since_stim, move_tminus1_num) %>% 
#   distinct() %>% 
#   mutate(mean_pred = NA, stdv_pred = NA)
# for(i in 1:nrow(predict_means)){
#   x <- predictions$prediction[predictions$focal_age == predict_means$focal_age[i] &
#                                 predictions$stim_type == predict_means$stim_type[i] &
#                                 predictions$age_diff_num == predict_means$age_diff_num[i] &
#                                 predictions$time_since_stim == predict_means$time_since_stim[i] &
#                                 predictions$move_tminus1_num == predict_means$move_tminus1_num[i]]
#   predict_means$mean_pred[i] <- mean(x)
#   predict_means$stdv_pred[i] <- sd(x)
# }
# predict_means$lwr <- predict_means$mean_pred - predict_means$stdv_pred
# predict_means$upr <- predict_means$mean_pred + predict_means$stdv_pred
# 
# save.image('moving_direction_predictions.RData')
# 
# age_labels <- c('partner younger','age matched','partner older')
# names(age_labels) <- c(1,2,3)
# stim_labels <- c('dove (control)','lion','human')
# names(stim_labels) <- c('ctd','l','h')
# ggplot()+
#   annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
#   geom_point(data = move_model_data,
#              mapping = aes(x = time_since_stim, y = moving_direction),
#              alpha = 0.01, shape = 19)+
#   facet_grid(stim_type ~ age_diff_num,
#              labeller = labeller(age_diff_num = age_labels,
#                                  stim_type = stim_labels))+
#   # geom_ribbon(data = predict_means,
#   #             mapping = aes(x = time_since_stim, ymax = upr, ymin = lwr,
#   #                           fill = as.factor(focal_age)),
#   #             alpha = 0.2)+
#   geom_line(data = predict_means,
#             mapping = aes(x = time_since_stim, y = mean_pred,
#                           colour = as.factor(focal_age),
#                           linetype = as.factor(move_tminus1_num)))+
#   scale_y_continuous(name = 'moving direction',
#                      breaks = c(1,2,3), labels = c('move at','side on','move away'),
#                      expand = c(0,0))+
#   scale_x_continuous(name = 'time since stimulus (s)')
# 
# 
# 
