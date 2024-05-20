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

theme_set(theme_classic())
set.seed(12345)

# #### import data for both models ####
# # https://dagitty.net/dags.html?id=dw8twK
# # read in data
# ages <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS') %>%
#   select(focal, f_age_cat, f_age_num) %>%
#   distinct() %>%
#   filter(!is.na(f_age_cat)) %>%
#   mutate(partner = focal,
#          p_age_cat = f_age_cat,
#          p_age_num = f_age_num)
# 
# stim_starts <- readRDS('../data_processed/stimuli.RDS') %>%
#   filter(status == 'START' & behavior == 'STIMULUS') %>%
#   select(pb_num,time,stim_num,stim_type,group_size,comment)
# table(stim_starts$pb_num)
# multiple_starts <- c(10, 24, 29, 32, 46, 53)
# check <- stim_starts %>%
#   filter(pb_num %in% multiple_starts) # for stim 10+29+46+53 take first time, for 24+32 use second.
# for(i in multiple_starts){
#   x <- check %>% filter(pb_num == i)
#   check <- anti_join(check, x)
#   if(i %in% c(10,29,46,53)){
#     x <- x[1,]
#   }
#   if(i %in% c(24,32)){
#     x <- x[2,]
#   }
#   check <- rbind(check, x)
# }
# stim_starts <- stim_starts %>%
#   filter(! pb_num %in% multiple_starts) %>%
#   rbind(check) %>%
#   mutate(time = as.numeric(time)) %>%
#   mutate(stim_start = round(time, 0)) %>%
#   select(pb_num,stim_start,stim_num,stim_type,group_size)
# 
# ## movement data
# cols_of_interest <- c('b1_move','b2_move','b3_move','b4_move',
#                       'b5_move','b6_move','b7_move','b8_move')
# cols_of_interest_name <- c('b1_move_name','b2_move_name','b3_move_name','b4_move_name',
#                            'b5_move_name','b6_move_name','b7_move_name','b8_move_name')
# cols_of_interest_index <- c('b1_move_index','b2_move_index','b3_move_index','b4_move_index',
#                             'b5_move_index','b6_move_index','b7_move_index','b8_move_index')
# move <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
#   # select relevant variables
#   select(subject,pb_num,second,out_frame_name,
#          all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
#   # convert to tidy format
#   rename(b1_move = b1_move_name, b2_move = b2_move_name,
#          b3_move = b3_move_name, b4_move = b4_move_name,
#          b5_move = b5_move_name, b6_move = b6_move_name,
#          b7_move = b7_move_name, b8_move = b8_move_name) %>%
#   pivot_longer(cols = all_of(cols_of_interest),
#                names_to = 'elephant_activity_name', values_to = 'moving_direction') %>%
#   rename(b1_move = b1_move_index, b2_move = b2_move_index,
#          b3_move = b3_move_index, b4_move = b4_move_index,
#          b5_move = b5_move_index, b6_move = b6_move_index,
#          b7_move = b7_move_index, b8_move = b8_move_index) %>%
#   pivot_longer(cols = all_of(cols_of_interest),
#                names_to = 'elephant_activity_index', values_to = 'move_index') %>%
#   filter(elephant_activity_name == elephant_activity_index) %>%
#   select(-elephant_activity_index) %>%
#   # clean up
#   rename(elephant_activity = elephant_activity_name,
#          focal = subject) %>%
#   # remove non-existent elephants (e.g. elephants 5-8 in a 4-elephant group)
#   mutate(moving_direction = ifelse(out_frame_name == 'out_of_sight' &
#                                             is.na(moving_direction) == FALSE,
#                                    ifelse(moving_direction == 'impossible_partner',
#                                           'impossible_partner','out_of_sight'),
#                                    moving_direction),
#          move_index = ifelse(out_frame_name == 'out_of_sight' &
#                                is.na(moving_direction) == FALSE,
#                            9, move_index)) %>% 
#   filter(is.na(move_index) == FALSE) %>%
#   # remove movement relative to self
#   filter(moving_direction != 'impossible_partner') %>% 
#   # join with explanatory variables
#   separate(elephant_activity, into = c('partner','activity'),
#            sep = '_', remove = T) %>%
#   mutate(partner = paste0(partner, '_e', pb_num),
#          pb_num = as.numeric(pb_num)) %>%
#   left_join(ages[,c('focal','f_age_cat','f_age_num')], by = 'focal') %>%
#   left_join(ages[,c('partner','p_age_cat','p_age_num')], by = 'partner') %>%
#   left_join(stim_starts, by = 'pb_num') %>%
#   # remove elephants with unknown ages
#   filter(!is.na(f_age_num)) %>%  # b2_e13 + b2_e34 + b6_e7 = unknown age
#   # create additional variables
#   mutate(time_since_stim = second - stim_start,
#          after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60),
#          age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num),
#                                  'partner_younger',
#                                  ifelse(as.numeric(f_age_num) == as.numeric(p_age_num),
#                                         'matched',
#                                         'partner_older'))) %>%
#   # clean up
#   select(pb_num,focal,partner,
#          activity,moving_direction,move_index,
#          stim_num,stim_type,
#          time_since_stim, after_stim,
#          f_age_cat,p_age_cat,f_age_num,p_age_num,
#          age_difference) %>%
#   mutate(f_age_num = as.factor(f_age_num),
#          p_age_num = as.factor(p_age_num),
#          age_combo = paste0(f_age_num,'_',p_age_num),
#          move_tminus1 = NA,
#          move_tminus1_num = NA)
# rm(list = ls() [ ! ls() %in% 'move']) ; gc()
# length(which(is.na(move$moving_direction) == TRUE))
# 
# ## create variable for moving_direction at time t-1
# focals <- unique(move$focal)
# for(f in 1:length(focals)){
#   focal <- move %>% filter(focal == focals[f])
#   move <- move %>% anti_join(focal, by = 'focal')
#   partners <- unique(focal$partner)
#   for(p in 1:length(partners)){
#     focal_partner <- focal %>% filter(partner == partners[p])
#     focal <- focal %>% anti_join(focal_partner, by = 'partner')
#     for(i in 2:nrow(focal_partner)){
#       focal_partner$move_tminus1[i] <- focal_partner$moving_direction[i-1]
#       focal_partner$move_tminus1_num[i] <- focal_partner$move_index[i-1]
#     }
#     focal <- rbind(focal, focal_partner)
#   }
#   move <- rbind(move, focal)
# }
# rm(list = ls()[ ! ls() %in% c('move', 'focals')]) ; gc()
# 
# ############### Probability of moving ###############
# pdf('../outputs/movement_binomial_model/movement_binomial_modelprep.pdf')
# 
# #### filter data ####
# move_no_na <- move %>%
#   # remove out of sight observations
#   filter(move_index != 9) %>%
#   filter(move_tminus1_num != 9) %>%
#   # remove first observation so can't see what they were doing in previous second
#   filter(is.na(move_tminus1) == FALSE) %>%
#   # convert to binary move or no move
#   mutate(move_index = ifelse(move_index == 0, 0, 1),
#          moving_direction = ifelse(moving_direction == 'not_moving',
#                                    'not_moving', 'moving'),
#          move_tminus1_num = ifelse(move_tminus1_num == 0, 0, 1),
#          move_tminus1 = ifelse(move_tminus1 == 'not_moving',
#                                    'not_moving', 'moving')) %>%
#   # clean up
#   mutate(f_age_num = as.integer(f_age_num)) %>%
#   mutate(focal_id = as.integer(as.factor(focal)),
#          stim_num = as.integer(as.factor(stim_num))) %>%
#   rename(stim_id = stim_num,
#          playback_id = pb_num) %>%
#   select(focal, moving_direction, move_index,
#          f_age_cat, f_age_num,
#          time_since_stim, after_stim, stim_type,
#          move_tminus1, move_tminus1_num,
#          focal_id, stim_id, playback_id) %>%
#   distinct()
# str(move_no_na)
# 
# #### set priors ####
# # set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
# get_prior(formula = move_index ~ 0 + mo(f_age_num) + stim_type +   # fixed effects
#             s(after_stim) + move_tminus1_num +                     # controls
#             (1|focal_id) + (1|stim_id) + (1|playback_id),          # random effects
#           data = move_no_na,
#           family = bernoulli("logit"))
# 
# # centre on -1 (logit(0.25) approx. = -1, and more likely to be not moving than moving)
# priors <- c(
#   # focal age
#   prior(normal(-1,1),     class = b,    coef = mof_age_num),
#   prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
#   # stimulus type
#   prior(normal(-1,1),     class = b,    coef = stim_typectd),
#   prior(normal(-1,1),     class = b,    coef = stim_typel),
#   prior(normal(-1,1),     class = b,    coef = stim_typeh),
#   # time spline
#   prior(normal(-1,1),     class = b,    coef = safter_stim_1),
#   # action in previous second
#   prior(normal(-1,1),     class = b,    coef = move_tminus1_num))
# 
# #### prior predictive check ####
# num_chains <- 4
# num_iter <- 2000
# mbm_prior <- brm(
#   formula = move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),
#   data = move_no_na,
#   family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(mbm_prior)
# 
# print(paste0('priors set and checked at ', Sys.time()))
# 
# ## reset plotting
# dev.off()
# pdf('../outputs/movement_binomial_model/movement_binomial_modelchecks.pdf')
# 
# #### fit model ####
# mbm_fit <- brm(
#   formula = move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),
#   data = move_no_na,
#   family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# 
# # save workspace
# save.image('movement_direction/movement_binomial_run.RData') # save.image('ele_playbacks/movement_direction/movement_binomial_run.RData')
# 
# # inspect model
# summary(mbm_fit)
# print(paste0('model fitted at ', Sys.time()))
# 
# #### check outputs ####
# # load('movement_direction/movement_binomial_run.RData') # load('ele_playbacks/movement_direction/movement_binomial_run.RData')
# 
# ## check Stan code
# mbm_fit$model
# mbm_fit$formula
# 
# ## extract posterior distribution
# draws <- as_draws_df(mbm_fit) %>%
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
# #### calculate log cumulative odds ####
# (prop <- table(move_no_na$moving_direction) / nrow(move_no_na))
# (cum_prop <- cumsum(prop))
# (log_cum_odds <- logit(cum_prop))
# 
# #### plot marginal effects ####
# ## extract marginal effects
# marg <- conditional_effects(mbm_fit,
#                             effects = c('f_age_num', 'stim_type', 'after_stim', 'move_tminus1_num'),
#                             #categorical = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# agefocal_effect <- marg[[1]]
# stim_effect <- marg[[2]]
# time_effect <- marg[[3]]
# prevsec_effect <- marg[[4]]
# 
# ## plot marginal effects
# (focal_age_plot <- ggplot(agefocal_effect)+
#     geom_errorbar(aes(x = f_age_num,
#                       #colour = cats__,
#                       ymax = upper__, ymin = lower__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = f_age_num,
#                    #colour = cats__,
#                    y = estimate__),
#                size = 3)+ # cex = 3?
#     xlab(label = 'focal age')+
#     ylab('probability of movement direction')+
#     scale_colour_viridis_d(name = 'movement direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'side on',
#                                       'approach at an angle',
#                                       'approach directly'))+
#     scale_fill_viridis_d(name = 'movement direction:',
#                          breaks = c('1','2','3','4','5'),
#                          labels = c('move away directly',
#                                     'move away at an angle',
#                                     'side on',
#                                     'approach at an angle',
#                                     'approach directly'))+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)))
# ggsave(plot = focal_age_plot, filename = '../outputs/movement_binomial_model/movement_binomial_marginaleffects_focalage.png', device = 'png',
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
#                       #colour = cats__,
#                       ymin = lower__, ymax = upper__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type,
#                    #colour = cats__,
#                    y = estimate__),
#                cex = 3)+ # size = 3?
#     xlab(label = 'stimulus type') + ylab('probability of movement direction')+
#     scale_colour_viridis_d(name = 'movement direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'side on',
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
# ggsave(plot = stim_plot, filename = '../outputs/movement_binomial_model/movement_marginaleffects_stimtype_agecombo.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# (focal_age_plot + stim_plot) +
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(),
#        filename = '../outputs/movement_binomial_model/movement_marginaleffects.png',
#        device = 'png', width = (5.8*3), height = 8.3)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check ####
# pp_check(mbm_fit, ndraws = 100) # really good fit
# 
# #### plot traces and density curves ####
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_stim_typectd","b_stim_typeh","b_stim_typel",
#                           "b_move_tminus1_num","bsp_mof_age_num",
#                           "bs_safter_stim_1","sds_safter_stim_1",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mixing doesn't move brilliant, esp. for bsp_mofocal_age, but only horrendous one is playback ID
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
# plot(density(dove_human$difference), main = 'lion vs human') ; abline(v = 0, lty = 2)
# plot(density(lion_human$difference), main = 'human vs lion') ; abline(v = 0, lty = 2)
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
# ## movement direction in previous second
# prevsec <- draws_cut %>% filter(parameter == 'b_move_tminus1_num')
# plot(density(prevsec$draw), main = 't-1 slope') ; abline(v = 0, lty = 2)
# 
# # ## time since stimulus -- come back to this!
# # timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
# # times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
# # time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
# # time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
# # time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
# # time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
# # time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
# # time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
# # time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
# # time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
# # par(mfrow = c(5,2))
# # plot(density(timeb$draw), main = 'time slope') ; abline(v = 0, lty = 2)
# # plot(density(times$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
# # plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
# # plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
# # plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
# # plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
# # plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
# # plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
# # plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
# # plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# save.image('movement_direction/movement_binomial_run.RData')
# 
# #### plot raw ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# ## plot overall
# move_no_na %>%
#   ggplot(aes(x = after_stim, y = move_index,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(f_age_num ~ stim_type,
#              labeller = labeller(f_age_num = age_labels,
#                                  stim_type = stim_labels))+
#   scale_x_continuous(name = 'time since stimulus started')
# print(paste0('raw data plotted at ',Sys.time()))
# 
# ## reset plotting
# save.image('movement_direction/movement_binomial_run.RData') # save.image('ele_playbacks/movement_direction/movement_binomial_run.RData')
# dev.off()
# 
# #### predict from model ####
# pdf('../outputs/movement_binomial_model/movement_binomial_modelpredictions.pdf')
# # load('movement_direction/movement_binomial_run.RData')
# rm(list = ls()[! ls() %in% c('mbm_fit','move_no_na','move','focals')]) ; gc()
# 
# pred <- posterior_epred(object = mbm_fit,
#                         newdata = move_no_na)
# save.image('movement_direction/movement_binomial_predictions.RData')
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
# save.image('movement_direction/movement_binomial_predictions.RData')
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions ####
# # load('movement_direction/movement_binomial_predictions.RData')
# 
# ## make labels for movement in previous second
# prevsec_labels <- c('not moving at t-1',
#                     'moving at t-1')
# names(prevsec_labels) <- c(0,1)
# 
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# (ctd_plot <- pred %>%
#     filter(stim_type == 'ctd',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_boxplot(aes(x = as.factor(f_age_num), y = epred,
#                      fill = as.factor(move_tminus1_num))) +
#     facet_grid(. ~ after_stim)+#,
#                #labeller = labeller(move_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'cape turtle dove (control)')+
#     theme(legend.position = 'bottom'))
# (lion_plot <- pred %>%
#     filter(stim_type == 'l',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     # colour = factor(pred_type, levels = c('not moving', 'moving')),
#                     # fill = factor(pred_type, levels = c('not moving', 'moving'))
#                     fill = as.factor(move_tminus1_num)
#                     )) +
#     facet_grid(. ~ after_stim)+#,
#                #labeller = labeller(move_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'lion')+
#     theme(legend.position = 'bottom'))
# (human_plot <- pred %>%
#     filter(stim_type == 'h',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     # colour = factor(pred_type, levels = c('not moving', 'moving')),
#                     # fill = factor(pred_type, levels = c('not moving', 'moving'))
#                     fill = as.factor(move_tminus1_num)
#                     )) +
#     facet_grid(. ~ after_stim)+#,
#                #labeller = labeller(move_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# (ctd_plot + lion_plot + human_plot)+
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(), file = '../outputs/movement_binomial_model/movement_binomial_predictions_violin.png',
#        device = 'png', height = 8, width = 48)
# 
# ## reset plotting
# dev.off()
# #pdf('../outputs/movement_binomial_model/movement_binomial_modelcontrasts.pdf')
# 
# rm(list = ls()[! ls() %in% 'move']) ; gc()
# 
# #### calculate posterior contrasts from predictions ####
# load('movement_direction/movement_binomial_predictions.RData')
# 
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

# plot contrasts
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
move_new <- move_new %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(stim_type, move_tminus1_num, after_stim,
                                                         focal_id, stim_id, playback_id))))

## predict with original ages
age_move_org <- move_new
age_mtx_org <- posterior_epred(object = move_fit, newdata = age_move_org)
colnames(age_mtx_org) <- age_move_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with altered ages
age_move_alt <- move_new %>%
  mutate(f_age_num_original = f_age_num) %>%
  mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
  relocate(f_age_num_original)
age_mtx_alt <- posterior_epred(object = move_fit, newdata = age_move_alt)
colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('movement_direction/movement_binomial_agecontrasts.RData')

# ############# Probability of different directions once moving ###############
# pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelprep.pdf')
# 
# #### filter data ####
# move_no_na <- move %>%
#   # remove out of sight observations
#   filter(move_index != 9) %>%
#   filter(move_tminus1_num != 9) %>%
#   # remove all times when elephant was not moving so only about direction
#   filter(moving_direction != 'not_moving') %>%
#   filter(move_tminus1 != 'not_moving') %>%
#   filter(is.na(move_tminus1) == FALSE) %>%
#   # remove any occasions with missing age data
#   filter(is.na(f_age_num) == FALSE) %>%
#   filter(is.na(p_age_num) == FALSE) %>%
#   filter(is.na(age_difference) == FALSE) %>%
#   # clean up
#   mutate(age_difference = factor(age_difference,
#                                  levels = c('partner_younger',
#                                             'matched',
#                                             'partner_older')),
#          moving_direction = factor(moving_direction,
#                                    levels = c('move away directly',
#                                               'move away at an angle',
#                                               'move directly with',
#                                               'approach at an angle',
#                                               'approach directly')),
#          move_tminus1 = factor(move_tminus1,
#                                    levels = c('move away directly',
#                                               'move away at an angle',
#                                               'move directly with',
#                                               'approach at an angle',
#                                               'approach directly'))) %>%
#   mutate(age_diff_num = as.integer(age_difference),
#          f_age_num = as.integer(f_age_num),
#          p_age_num = as.integer(p_age_num)) %>%
#   mutate(age_combo = paste0(f_age_num, '_', p_age_num),
#          focal_id = as.integer(as.factor(focal)),
#          stim_num = as.integer(as.factor(stim_num))) %>%
#   rename(stim_id = stim_num,
#          playback_id = pb_num) %>%
#   select(focal, partner, moving_direction, move_index,
#          f_age_cat, p_age_cat, f_age_num, p_age_num,
#          age_difference, age_diff_num, age_combo,
#          time_since_stim, after_stim, stim_type,
#          move_tminus1, move_tminus1_num,
#          focal_id, stim_id, playback_id)
# str(move_no_na)
# 
# #### set priors ####
# # set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
# get_prior(formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
#             s(after_stim) + mo(move_tminus1_num) +                             # controls
#             (1|focal_id) + (1|stim_id) + (1|playback_id),                      # random effects
#           data = move_no_na,
#           family = cumulative("logit"))
# 
# priors <- c(
#   # focal age
#   prior(normal(0,0.25),   class = b,    coef = mof_age_num),
#   prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
#   # interaction age
#   prior(normal(0,1),     class = b,    coef = age_combo1_2),
#   prior(normal(0,1),     class = b,    coef = age_combo1_3),
#   prior(normal(0,1),     class = b,    coef = age_combo1_4),
#   prior(normal(0,1),     class = b,    coef = age_combo2_1),
#   prior(normal(0,1),     class = b,    coef = age_combo2_2),
#   prior(normal(0,1),     class = b,    coef = age_combo2_3),
#   prior(normal(0,1),     class = b,    coef = age_combo2_4),
#   prior(normal(0,1),     class = b,    coef = age_combo3_1),
#   prior(normal(0,1),     class = b,    coef = age_combo3_2),
#   prior(normal(0,1),     class = b,    coef = age_combo3_3),
#   prior(normal(0,1),     class = b,    coef = age_combo3_4),
#   prior(normal(0,1),     class = b,    coef = age_combo4_1),
#   prior(normal(0,1),     class = b,    coef = age_combo4_2),
#   prior(normal(0,1),     class = b,    coef = age_combo4_3),
#   prior(normal(0,1),     class = b,    coef = age_combo4_4),
#   # stimulus type
#   prior(normal(0,1),      class = b,    coef = stim_typeh),
#   prior(normal(0,1),      class = b,    coef = stim_typel),
#   # time spline
#   prior(normal(0,1),        class = b,    coef = safter_stim_1),
#   #prior(student_t(3,0,2.5), class = sds,  coef = s(after_stim)), # included in move, wasn't in movement direction but not sure why?
#   # action in previous second
#   prior(normal(0,0.333),  class = b,    coef = momove_tminus1_num),
#   prior(dirichlet(2,2,2,2),     class = simo, coef = momove_tminus1_num1))
# 
# #### prior predictive check ####
# num_chains <- 4
# num_iter <- 2000
# mom1_prior <- brm(
#   formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +
#     s(after_stim) + mo(move_tminus1_num) +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),
#   data = move_no_na,
#   family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# 
# pp_check(mom1_prior)
# 
# print(paste0('priors set and checked at ', Sys.time()))
# 
# ## reset plotting
# dev.off()
# pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelchecks.pdf')
# 
# #### fit model ####
# mom1_fit <- brm(
#   formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +
#     s(after_stim) + mo(move_tminus1_num) +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),
#   data = move_no_na,
#   family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# 
# # save workspace
# save.image('movement_direction/movement_ordinal_model1_run.RData') # save.image('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')
# 
# # inspect model
# summary(mom1_fit)
# print(paste0('model fitted at ', Sys.time()))
# 
# #### check outputs ####
# # load('movement_direction/movement_ordinal_model1_run.RData') # load('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')
# ## check Stan code
# mom1_fit$model
# mom1_fit$formula
# 
# ## extract posterior distribution
# draws <- as_draws_df(mom1_fit) %>%
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
# # nearest neighbour version -- run with movement direction first but if it throws an error then come back to the nearest neighbour code to fix it
# # draws <- as_draws_df(move_fit) %>%
# #   select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>%
# #   pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>%
# #   mutate(iteration = rep(rep(1:(num_iter/2),
# #                              each = length(unique(parameter))),
# #                          num_chains),
# #          chain = rep(1:num_chains,
# #                      each = length(unique(parameter))*(num_iter/2)),
# #          invlogit_draw = invlogit(draw))
# 
# print(paste0('posterior extracted at ',Sys.time()))
# 
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,2))
# hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
# par(mfrow = c(1,1))
# 
# #### calculate log cumulative odds ####
# prop <- table(move_no_na$moving_direction) / nrow(move_no_na)
# cum_prop <- cumsum(prop)
# log_cum_odds <- logit(cum_prop)
# 
# #### plot marginal effects ####
# ## extract marginal effects
# marg <- conditional_effects(mom1_fit,
#                             categorical = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# agecombo_effect <- marg[[1]]
# stim_effect <- marg[[2]]
# agefocal_effect <- marg[[3]]
# prevsec_effect <- marg[[4]]
# time_effect <- marg[[5]]
# 
# ## plot marginal effects
# (focal_age_plot <- ggplot(agefocal_effect)+
#     # geom_ribbon(aes(x = f_age_num,
#     #                 ymax = upper__, ymin = lower__,
#     #                 fill = cats__),
#     #             alpha = 0.4)+
#     # geom_line(aes(x = f_age_num,
#     #               y = estimate__,
#     #               colour = cats__),
#     #           linewidth = 1)+
#     geom_errorbar(aes(x = f_age_num,
#                       ymax = upper__, ymin = lower__,
#                       colour = cats__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = f_age_num,
#                    y = estimate__,
#                    colour = cats__),
#                size = 3)+ # cex = 3?
#     xlab(label = 'focal age')+
#     ylab('probability of movement direction')+
#     scale_colour_viridis_d(name = 'movement direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'side on',
#                                       'approach at an angle',
#                                       'approach directly'))+
#     scale_fill_viridis_d(name = 'movement direction:',
#                          breaks = c('1','2','3','4','5'),
#                          labels = c('move away directly',
#                                     'move away at an angle',
#                                     'side on',
#                                     'approach at an angle',
#                                     'approach directly'))+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)))
# ggsave(plot = focal_age_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_focalage_agecombo.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# focal_age_labels <- c('focal age category 1',
#                       'focal age category 2',
#                       'focal age category 3',
#                       'focal age category 4')
# names(focal_age_labels) <- 1:4
# (agecombo_plot <- agecombo_effect %>%
#     separate(col = age_combo, sep = '_', remove = F,
#              into = c('focal_age','partner_age')) %>%
#     mutate(agecombo = paste0(focal_age,'-',partner_age)) %>%
#     ggplot()+
#     geom_errorbar(aes(#x = agecombo,
#       x = partner_age,
#       colour = as.factor(cats__), # movement direction?
#       ymax = upper__, ymin = lower__),
#       linewidth = 1,
#       width = 0.4)+
#     geom_point(aes(#x = agecombo,
#       x = partner_age,
#       colour = as.factor(cats__),    # movement direction?
#       #shape = focal_age,
#       y = estimate__),
#       size = 3)+
#     # geom_ribbon(aes(#x = agecombo,
#     #                 x = as.numeric(partner_age),
#     #                 fill = as.factor(cats__),     # movement direction?
#     #                 ymax = upper__, ymin = lower__),
#     #             alpha = 0.4)+
#     # geom_line(aes(#x = agecombo,
#     #               x = as.numeric(partner_age),
#     #               colour = as.factor(cats__),     # movement direction?
#     #               y = estimate__),
#     #           linewidth = 1)+
#     facet_wrap(. ~ focal_age,
#                labeller = labeller(focal_age = focal_age_labels))+
#     ylab('probability of movement direction')+
#     scale_colour_viridis_d(name = 'movement direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'side on',
#                                       'approach at an angle',
#                                       'approach directly'))+
#     # scale_fill_viridis_d(name = 'movement direction:',
#     #                      breaks = c('1','2','3','4','5'),
#     #                      labels = c('move away directly',
#     #                                 'move away at an angle',
#     #                                 'side on',
#     #                                 'approach at an angle',
#     #                                 'approach directly'))+
#     scale_x_discrete(name = 'partner age category')+
#     #scale_x_continuous(name = 'partner age category')+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = agecombo_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_agepartner_agecombo.png', device = 'png',
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
#     xlab(label = 'stimulus type') + ylab('probability of movement direction')+
#     scale_colour_viridis_d(name = 'movement direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'side on',
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
# ggsave(plot = stim_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_stimtype_agecombo.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# (focal_age_plot + agecombo_plot + stim_plot) +
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(),
#        filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects.png',
#        device = 'png', width = (5.8*3), height = 8.3)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check ####
# pp_check(mom1_fit, ndraws = 100) # really good fit
# 
# #### plot traces and density curves ####
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
#                           "b_stim_typeh","b_stim_typel",
#                           "safter_stim_1","sds_s(after_stim)",
#                           "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
#                           "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
#                           "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
#                           "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "bsp_mof_age_num",#"bsp_mopartner_age",
#                           "bsp_momove_tminus1_num",
#                           # "simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]",
#                           # "simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           "simo_momove_tminus1_num1[1]","simo_momove_tminus1_num1[2]"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mixing doesn't move brilliant, esp. for bsp_mofocal_age, but only horrendous one is playback ID
# 
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,1))
# hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
# #hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')
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
# ## age interaction -- come back to this!
# 
# ## movement direction in previous second
# prevsec1 <- draws_cut %>% filter(parameter == 'bsp_momove_tminus1_num')
# prevsec2 <- draws_cut %>% filter(parameter == 'simo_momove_tminus1_num1[1]')
# prevsec3 <- draws_cut %>% filter(parameter == 'simo_momove_tminus1_num1[2]')
# par(mfrow = c(3,1))
# plot(density(prevsec1$draw), main = 't-1 younger') ; abline(v = 0, lty = 2)
# plot(density(prevsec2$draw), main = 't-1 matched') ; abline(v = 0, lty = 2)
# plot(density(prevsec3$draw), main = 't-1 older') ; abline(v = 0, lty = 2)
# 
# # ## time since stimulus -- come back to this!
# # timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
# # times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
# # time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
# # time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
# # time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
# # time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
# # time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
# # time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
# # time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
# # time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
# # par(mfrow = c(5,2))
# # plot(density(timeb$draw), main = 'time slope') ; abline(v = 0, lty = 2)
# # plot(density(times$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
# # plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
# # plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
# # plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
# # plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
# # plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
# # plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
# # plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
# # plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# save.image('movement_direction/movement_ordinal_model1_run.RData')
# 
# #### plot raw ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# ## plot overall
# ggplot(move_no_na, aes(x = f_age_num, y = move_index,
#                        colour = age_difference))+
#   geom_jitter(alpha = 0.1)+
#   facet_wrap(. ~ stim_type,
#              labeller = labeller(stim_type = stim_labels))+
#   scale_y_discrete(name = 'focal movement direction relative to target',
#                    breaks = c(1,2,3,4,5),
#                    labels = c('move away directly',
#                               'move away at an angle',
#                               'side on',
#                               'approach at an angle',
#                               'approach directly'))+
#   labs(colour = 'age difference')
# print('plot complete')
# 
# ## plot control data
# move_no_na %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = after_stim, y = move_index,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(f_age_num ~ factor(age_difference,
#                                 levels = c('partner younger','matched','partner older')),
#              labeller = labeller(f_age_num = age_labels))+
#   scale_x_continuous(name = 'time since stimulus started')
# print('plot complete')
# 
# ## plot lion data
# move_no_na %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = after_stim, y = move_index,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(f_age_num ~ factor(age_difference,
#                                 levels = c('partner younger','matched','partner older')),
#              labeller = labeller(f_age_num = age_labels))+
#   scale_x_continuous(name = 'time since stimulus started')
# print('plot complete')
# 
# ## plot human data
# move_no_na %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = after_stim, y = move_index,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(f_age_num ~ factor(age_difference,
#                                 levels = c('partner younger','matched','partner older')),
#              labeller = labeller(f_age_num = age_labels))+
#   scale_x_continuous(name = 'time since stimulus started')
# 
# print(paste0('raw data plotted at ',Sys.time()))
# 
# ## reset plotting
# save.image('movement_direction/movement_ordinal_model1_run.RData') # save.image('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')
# dev.off()
# 
# #### predict from model ####
# pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelpredictions.pdf')
# load('movement_direction/movement_ordinal_model1_run.RData')
# rm(list = ls()[! ls() %in% c('mom1_fit','move_no_na')]) ; gc()
# 
# pred <- posterior_epred(object = mom1_fit,
#                         newdata = move_no_na)
# save.image('movement_direction/movement_ordinal_model1_predictions.RData')
# 
# ## convert to data frame
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
# 
# # pred1 <- as.data.frame(pred[,,1])
# # colnames(pred1) <- 1:nrow(move_no_na)
# 
# move_no_na$data_row <- 1:nrow(move_no_na)
# # pred1 <- pred1 %>%
# #   pivot_longer(cols = everything(),
# #                names_to = 'data_row', values_to = 'epred') %>%
# #   left_join(move_no_na, by = 'data_row') %>%
# #   mutate(pred_type = 'not_moving',
# #          pred_type_num = 0)
# 
# pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = move_no_na)
# pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = move_no_na)
# pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = move_no_na)
# pred4 <- extract_predictions(prediction_array = pred, layer = 4, df = move_no_na)
# pred5 <- extract_predictions(prediction_array = pred, layer = 5, df = move_no_na)
# 
# pred <- rbind(pred1, pred2, pred3, pred4, pred5)
# save.image('movement_direction/movement_ordinal_model1_predictions.RData')
# rm(pred1, pred2, pred3, pred4, pred5) ; gc()
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions ####
# load('movement_direction/movement_ordinal_model1_predictions.RData')
# 
# ## make labels for movement in previous second
# prevsec_labels <- c('direct away at t-1',
#                     'angle away at t-1',
#                     'neither at t-1',
#                     'angle approach at t-1',
#                     'direct approach at t-1')
# names(prevsec_labels) <- 1:5
# 
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# (ctd_plot <- pred %>%
#     filter(stim_type == 'ctd',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('move directly away',
#                                                         'move away at an angle',
#                                                         'move neither towards or away',
#                                                         'approach at an angle',
#                                                         'approach directly')),
#                     colour = factor(pred_type, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))
#                     )) +
#     facet_grid(move_tminus1_num ~ after_stim,
#                labeller = labeller(move_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'cape turtle dove (control)')+
#     theme(legend.position = 'bottom'))
# print('ctd_plot complete')
# 
# (lion_plot <- pred %>%
#     filter(stim_type == 'l',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('move directly away',
#                                                         'move away at an angle',
#                                                         'move neither towards or away',
#                                                         'approach at an angle',
#                                                         'approach directly')),
#                     colour = factor(pred_type, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))
#                     )) +
#     facet_grid(move_tminus1_num ~ after_stim,
#                labeller = labeller(move_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'lion')+
#     theme(legend.position = 'bottom'))
# print('lion_plot complete')
# 
# (human_plot <- pred %>%
#     filter(stim_type == 'h',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('move directly away',
#                                                         'move away at an angle',
#                                                         'move neither towards or away',
#                                                         'approach at an angle',
#                                                         'approach directly')),
#                     colour = factor(pred_type, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))
#                     )) +
#     facet_grid(move_tminus1_num ~ after_stim,
#                labeller = labeller(move_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# print('human_plot complete')
# 
# (ctd_plot + lion_plot + human_plot) +
#   plot_layout(guides = "collect") +
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(), file = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_predictions_violin.png',
#        device = 'png', height = 8, width = 24)
# 
# ## reset plotting
# dev.off()
pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelcontrasts.pdf')
# 
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

# ## count types of each prediction
# #load('movement_direction/movement_model_stimuluscontrasts.RData')
# # count_values <- function(vector, levels = c(1,2,3)) {
# #   x <- tabulate(factor(vector, levels), length(levels))
# #   return(list(x))
# # }
# stim_pred <- ctd_move %>%
#   dplyr::select(-stim_type) %>%
#   # mutate(ctd_count = apply(ctd_mtx, 2, count_values),
#   #        lion_count = apply(lion_mtx, 2, count_values),
#   #        human_count = apply(human_mtx, 2, count_values)) %>%
#   # unnest(c(ctd_count, lion_count, human_count)) %>% # I've done something weird with the count_values function so for now this needs unnesting twice, but should probably fix it at some point! For now this works!
#   # unnest(c(ctd_count, lion_count, human_count)) %>%
#   mutate(ctd_prop1_mu = apply(ctd_mtx[,,1], 2, mean),
#          ctd_prop2_mu = apply(ctd_mtx[,,2], 2, mean),
#          ctd_prop3_mu = apply(ctd_mtx[,,3], 2, mean),
#          ctd_prop1_sd = apply(ctd_mtx[,,1], 2, sd),
#          ctd_prop2_sd = apply(ctd_mtx[,,2], 2, sd),
#          ctd_prop3_sd = apply(ctd_mtx[,,3], 2, sd),
#          lion_prop1_mu = apply(lion_mtx[,,1], 2, mean),
#          lion_prop2_mu = apply(lion_mtx[,,2], 2, mean),
#          lion_prop3_mu = apply(lion_mtx[,,3], 2, mean),
#          lion_prop1_sd = apply(lion_mtx[,,1], 2, sd),
#          lion_prop2_sd = apply(lion_mtx[,,2], 2, sd),
#          lion_prop3_sd = apply(lion_mtx[,,3], 2, sd),
#          human_prop1_mu = apply(human_mtx[,,1], 2, mean),
#          human_prop2_mu = apply(human_mtx[,,2], 2, mean),
#          human_prop3_mu = apply(human_mtx[,,3], 2, mean),
#          human_prop1_sd = apply(human_mtx[,,1], 2, sd),
#          human_prop2_sd = apply(human_mtx[,,2], 2, sd),
#          human_prop3_sd = apply(human_mtx[,,3], 2, sd)) %>%
#   pivot_longer(cols = c(ctd_prop1_mu,ctd_prop2_mu,ctd_prop3_mu,
#                         lion_prop1_mu,lion_prop2_mu,lion_prop3_mu,
#                         human_prop1_mu,human_prop2_mu,human_prop3_mu),
#                names_to = 'stim_propage_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(ctd_prop1_sd,ctd_prop2_sd,ctd_prop3_sd,
#                         lion_prop1_sd,lion_prop2_sd,lion_prop3_sd,
#                         human_prop1_sd,human_prop2_sd,human_prop3_sd),
#                names_to = 'stim_propage_sd', values_to = 'stdv_propn') %>%
#   separate(col = stim_propage_mu, into = c('stim_propage_mu','mu'),
#            sep = '_m', remove = T) %>%
#   select(-mu) %>%
#   separate(col = stim_propage_sd, into = c('stim_propage_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-sd) %>%
#   filter(stim_propage_mu == stim_propage_sd) %>%
#   separate(col = stim_propage_mu, into = c('stim_type', 'move_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-stim_propage_sd) %>%
#   mutate(move_pred = as.numeric(move_pred)) %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'younger',
#                             ifelse(move_pred == 2, 'matched', 'older')))
# 
# ## calculate contrasts
# ctd_vs_lion_age1 <- lion_mtx[,,1] - ctd_mtx[,,1]
# ctd_vs_lion_age2 <- lion_mtx[,,2] - ctd_mtx[,,2]
# ctd_vs_lion_age3 <- lion_mtx[,,3] - ctd_mtx[,,3]
# ctd_vs_human_age1 <- human_mtx[,,1] - ctd_mtx[,,1]
# ctd_vs_human_age2 <- human_mtx[,,2] - ctd_mtx[,,2]
# ctd_vs_human_age3 <- human_mtx[,,3] - ctd_mtx[,,3]
# lion_vs_human_age1 <- human_mtx[,,1] - lion_mtx[,,1]
# lion_vs_human_age2 <- human_mtx[,,2] - lion_mtx[,,2]
# lion_vs_human_age3 <- human_mtx[,,3] - lion_mtx[,,3]
# 
# ## summarise contrasts
# contrasts <- move_no_na %>%
#   select(-stim_type) %>%
#   mutate(ctd_vs_lion_age1_mu = apply(ctd_vs_lion_age1, 2, mean),
#          ctd_vs_lion_age1_sd = apply(ctd_vs_lion_age1, 2, sd),
#          ctd_vs_lion_age2_mu = apply(ctd_vs_lion_age2, 2, mean),
#          ctd_vs_lion_age2_sd = apply(ctd_vs_lion_age2, 2, sd),
#          ctd_vs_lion_age3_mu = apply(ctd_vs_lion_age3, 2, mean),
#          ctd_vs_lion_age3_sd = apply(ctd_vs_lion_age3, 2, sd),
#          ctd_vs_human_age1_mu = apply(ctd_vs_human_age1, 2, mean),
#          ctd_vs_human_age1_sd = apply(ctd_vs_human_age1, 2, sd),
#          ctd_vs_human_age2_mu = apply(ctd_vs_human_age2, 2, mean),
#          ctd_vs_human_age2_sd = apply(ctd_vs_human_age2, 2, sd),
#          ctd_vs_human_age3_mu = apply(ctd_vs_human_age3, 2, mean),
#          ctd_vs_human_age3_sd = apply(ctd_vs_human_age3, 2, sd),
#          lion_vs_human_age1_mu = apply(lion_vs_human_age1, 2, mean),
#          lion_vs_human_age1_sd = apply(lion_vs_human_age1, 2, sd),
#          lion_vs_human_age2_mu = apply(lion_vs_human_age2, 2, mean),
#          lion_vs_human_age2_sd = apply(lion_vs_human_age2, 2, sd),
#          lion_vs_human_age3_mu = apply(lion_vs_human_age3, 2, mean),
#          lion_vs_human_age3_sd = apply(lion_vs_human_age3, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(ctd_vs_lion_age1_mu,ctd_vs_lion_age2_mu,ctd_vs_lion_age3_mu,
#                         ctd_vs_human_age1_mu,ctd_vs_human_age2_mu,ctd_vs_human_age3_mu,
#                         lion_vs_human_age1_mu,lion_vs_human_age2_mu,lion_vs_human_age3_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','move_pred'),
#            sep = '_age', remove = T) %>%
#   separate(move_pred, into = c('move_pred','mu'),
#            sep = '_', remove = T) %>%
#   mutate(move_pred = as.numeric(move_pred)) %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'younger',
#                             ifelse(move_pred == 2, 'matched', 'older'))) %>%
#   select(-mu, -ctd_vs_lion_age1_sd, -ctd_vs_lion_age2_sd, -ctd_vs_lion_age3_sd,
#          -ctd_vs_human_age1_sd, -ctd_vs_human_age2_sd, -ctd_vs_human_age3_sd,
#          -lion_vs_human_age1_sd, -lion_vs_human_age2_sd, -lion_vs_human_age3_sd)
# 
# ## plot contrasts
# # stim_pred %>%
# #   dplyr::select(ctd_lion, ctd_human, lion_human, pred_type) %>%
# #   pivot_longer(cols = c('ctd_lion', 'ctd_human', 'lion_human'),
# #                names_to = 'contrast') %>%
# #   ggplot()+
# #   geom_density(aes(x = value, colour = contrast))+
# #   facet_wrap(. ~ pred_type)
# 
# stim_pred %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = pred_type))+
#   facet_wrap(. ~ stim_type)
# contrasts_long %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'younger',
#                             ifelse(move_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast)
# 
# save.image('movement_direction/movement_model_stimuluscontrasts_epred.RData')
# 
# # focal age -- move ####
# # load('movement_direction/movement_model_stimuluscontrasts_epred.RData')
# rm(ctd_move, ctd_mtx, human_move, human_mtx, lion_move, lion_mtx,
#    contrasts, contrasts_long, stim_pred,
#    ctd_vs_human_age1, ctd_vs_human_age2, ctd_vs_human_age3,
#    ctd_vs_lion_age1, ctd_vs_lion_age2, ctd_vs_lion_age3,
#    lion_vs_human_age1, lion_vs_human_age2, lion_vs_human_age3) ; gc()
# 
# ## predict with original ages
# age_move_org <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# age_mtx_org <- posterior_epred(object = move_fit, newdata = age_move_org)
# colnames(age_mtx_org) <- age_move_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with altered ages
# age_move_alt <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(f_age_num_original = f_age_num) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1),
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(f_age_num_original)
# age_mtx_alt <- posterior_epred(object = move_fit, newdata = age_move_alt)
# colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# save.image('movement_direction/movement_model_agecontrasts_epred.RData')
# 
# ## summarise and convert to long format
# age_pred <- age_move_org %>%
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
#   pivot_longer(cols = c(age_org_prop1_mu,age_org_prop2_mu,age_org_prop3_mu,
#                         age_alt_prop1_mu,age_alt_prop2_mu,age_alt_prop3_mu),
#                names_to = 'focal_agemove_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(age_org_prop1_sd,age_org_prop2_sd,age_org_prop3_sd,
#                         age_alt_prop1_sd,age_alt_prop2_sd,age_alt_prop3_sd),
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
#   mutate(pred_type = ifelse(move_pred == 1, 'younger',
#                             ifelse(move_pred == 2, 'matched', 'older')))
# 
# ## calculate contrasts
# alt_vs_org_young <- age_mtx_alt[,,1] - age_mtx_org[,,1]
# alt_vs_org_match <- age_mtx_alt[,,2] - age_mtx_org[,,2]
# alt_vs_org_older <- age_mtx_alt[,,3] - age_mtx_org[,,3]
# 
# ## summarise contrasts
# contrasts <- move_no_na %>%
#   mutate(alt_vs_org_young_mu = apply(alt_vs_org_young, 2, mean),
#          alt_vs_org_young_sd = apply(alt_vs_org_young, 2, sd),
#          alt_vs_org_match_mu = apply(alt_vs_org_match, 2, mean),
#          alt_vs_org_match_sd = apply(alt_vs_org_match, 2, sd),
#          alt_vs_org_older_mu = apply(alt_vs_org_older, 2, mean),
#          alt_vs_org_older_sd = apply(alt_vs_org_older, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(alt_vs_org_young_mu,alt_vs_org_match_mu,alt_vs_org_older_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('alt','vs','org','move_pred','mu'),
#            sep = '_', remove = T) %>%
#   select(-alt_vs_org_young_sd, -alt_vs_org_match_sd, -alt_vs_org_older_sd, -alt, -vs, -org, -mu)
# 
# ## plot contrasts
# # age_pred %>%
# #   dplyr::select(ctd_lion, ctd_human, lion_human, pred_type) %>%
# #   pivot_longer(cols = c('ctd_lion', 'ctd_human', 'lion_human'),
# #                names_to = 'contrast') %>%
# #   ggplot()+
# #   geom_density(aes(x = value, colour = contrast))+
# #   facet_wrap(. ~ pred_type)
# 
# age_pred %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = pred_type))+
#   facet_wrap(. ~ stim_type)
# contrasts_long %>%
#   mutate(pred_type = ifelse(move_pred == 'young', 'younger',
#                             ifelse(move_pred == 'match', 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   mutate(f_age_new = ifelse(f_age_num == 4, 1, f_age_num+1)) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ f_age_new, scales = 'free')
# save.image('movement_direction/movement_model_agecontrasts_epred.RData')
# 
# # movement in previous second -- move ####
# #load('movement_direction/movement_model_agecontrasts_epred.RData')
# rm(age_move_org, age_mtx_org, age_move_alt, age_mtx_alt, age_pred, alt_vs_org_young, alt_vs_org_match, alt_vs_org_older, contrasts, contrasts_long) ; gc()
# 
# ## redo predictions with different previous movements: all younger -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 1, move AT T-1 YOUNGER
# young_move <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(move_tminus1_num = 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# young_mtx <- posterior_epred(object = move_fit, newdata = young_move)
# colnames(young_mtx) <- young_move$unique_data_combo
# young_mtx <- young_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different previous movements: all matching
# match_move <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(move_tminus1_num = 2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# match_mtx <- posterior_epred(object = move_fit, newdata = match_move)
# colnames(match_mtx) <- match_move$unique_data_combo
# match_mtx <- match_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different previous movements: all older -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 4, move AT T-1 OLDER
# older_move <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(move_tminus1_num = 3,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# older_mtx <- posterior_epred(object = move_fit, newdata = older_move)
# colnames(older_mtx) <- older_move$unique_data_combo
# older_mtx <- older_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('movement_direction/movement_model_tminus1contrasts_epred.RData')
# 
# ## count types of each prediction
# #load('movement_direction/movement_model_tminus1contrasts_epred.RData')
# prevsec_pred <- young_move %>%
#   dplyr::select(-move_tminus1_num) %>%
#   mutate(young_prop1_mu = apply(young_mtx[,,1], 2, mean),
#          young_prop2_mu = apply(young_mtx[,,2], 2, mean),
#          young_prop3_mu = apply(young_mtx[,,3], 2, mean),
#          young_prop1_sd = apply(young_mtx[,,1], 2, sd),
#          young_prop2_sd = apply(young_mtx[,,2], 2, sd),
#          young_prop3_sd = apply(young_mtx[,,3], 2, sd),
#          match_prop1_mu = apply(match_mtx[,,1], 2, mean),
#          match_prop2_mu = apply(match_mtx[,,2], 2, mean),
#          match_prop3_mu = apply(match_mtx[,,3], 2, mean),
#          match_prop1_sd = apply(match_mtx[,,1], 2, sd),
#          match_prop2_sd = apply(match_mtx[,,2], 2, sd),
#          match_prop3_sd = apply(match_mtx[,,3], 2, sd),
#          older_prop1_mu = apply(older_mtx[,,1], 2, mean),
#          older_prop2_mu = apply(older_mtx[,,2], 2, mean),
#          older_prop3_mu = apply(older_mtx[,,3], 2, mean),
#          older_prop1_sd = apply(older_mtx[,,1], 2, sd),
#          older_prop2_sd = apply(older_mtx[,,2], 2, sd),
#          older_prop3_sd = apply(older_mtx[,,3], 2, sd)) %>%
#   pivot_longer(cols = c(young_prop1_mu,young_prop2_mu,young_prop3_mu,
#                         match_prop1_mu,match_prop2_mu,match_prop3_mu,
#                         older_prop1_mu,older_prop2_mu,older_prop3_mu),
#                names_to = 'prevsec_propage_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(young_prop1_sd,young_prop2_sd,young_prop3_sd,
#                         match_prop1_sd,match_prop2_sd,match_prop3_sd,
#                         older_prop1_sd,older_prop2_sd,older_prop3_sd),
#                names_to = 'prevsec_propage_sd', values_to = 'stdv_propn') %>%
#   separate(col = prevsec_propage_mu, into = c('prevsec_propage_mu','mu'),
#            sep = '_m', remove = T) %>%
#   separate(col = prevsec_propage_sd, into = c('prevsec_propage_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-mu, -sd) %>%
#   filter(prevsec_propage_mu == prevsec_propage_sd) %>%
#   separate(col = prevsec_propage_mu, into = c('prevsec_type', 'move_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-prevsec_propage_sd) %>%
#   mutate(move_pred = as.numeric(move_pred)) %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'younger',
#                             ifelse(move_pred == 2, 'matched', 'older')))
# 
# ## calculate contrasts
# young_vs_match_age1 <- match_mtx[,,1] - young_mtx[,,1]
# young_vs_match_age2 <- match_mtx[,,2] - young_mtx[,,2]
# young_vs_match_age3 <- match_mtx[,,3] - young_mtx[,,3]
# young_vs_older_age1 <- older_mtx[,,1] - young_mtx[,,1]
# young_vs_older_age2 <- older_mtx[,,2] - young_mtx[,,2]
# young_vs_older_age3 <- older_mtx[,,3] - young_mtx[,,3]
# match_vs_older_age1 <- older_mtx[,,1] - match_mtx[,,1]
# match_vs_older_age2 <- older_mtx[,,2] - match_mtx[,,2]
# match_vs_older_age3 <- older_mtx[,,3] - match_mtx[,,3]
# 
# ## summarise contrasts
# contrasts <- move_no_na %>%
#   select(-move_tminus1_num) %>%
#   mutate(young_vs_match_age1_mu = apply(young_vs_match_age1, 2, mean),
#          young_vs_match_age1_sd = apply(young_vs_match_age1, 2, sd),
#          young_vs_match_age2_mu = apply(young_vs_match_age2, 2, mean),
#          young_vs_match_age2_sd = apply(young_vs_match_age2, 2, sd),
#          young_vs_match_age3_mu = apply(young_vs_match_age3, 2, mean),
#          young_vs_match_age3_sd = apply(young_vs_match_age3, 2, sd),
#          young_vs_older_age1_mu = apply(young_vs_older_age1, 2, mean),
#          young_vs_older_age1_sd = apply(young_vs_older_age1, 2, sd),
#          young_vs_older_age2_mu = apply(young_vs_older_age2, 2, mean),
#          young_vs_older_age2_sd = apply(young_vs_older_age2, 2, sd),
#          young_vs_older_age3_mu = apply(young_vs_older_age3, 2, mean),
#          young_vs_older_age3_sd = apply(young_vs_older_age3, 2, sd),
#          match_vs_older_age1_mu = apply(match_vs_older_age1, 2, mean),
#          match_vs_older_age1_sd = apply(match_vs_older_age1, 2, sd),
#          match_vs_older_age2_mu = apply(match_vs_older_age2, 2, mean),
#          match_vs_older_age2_sd = apply(match_vs_older_age2, 2, sd),
#          match_vs_older_age3_mu = apply(match_vs_older_age3, 2, mean),
#          match_vs_older_age3_sd = apply(match_vs_older_age3, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(young_vs_match_age1_mu,young_vs_match_age2_mu,young_vs_match_age3_mu,
#                         young_vs_older_age1_mu,young_vs_older_age2_mu,young_vs_older_age3_mu,
#                         match_vs_older_age1_mu,match_vs_older_age2_mu,match_vs_older_age3_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','move_pred'),
#            sep = '_age', remove = T) %>%
#   separate(move_pred, into = c('move_pred','mu'),
#            sep = '_', remove = T) %>%
#   mutate(move_pred = as.numeric(move_pred)) %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'younger',
#                             ifelse(move_pred == 2, 'matched', 'older'))) %>%
#   select(-mu, -young_vs_match_age1_sd, -young_vs_match_age2_sd, -young_vs_match_age3_sd,
#          -young_vs_older_age1_sd, -young_vs_older_age2_sd, -young_vs_older_age3_sd,
#          -match_vs_older_age1_sd, -match_vs_older_age2_sd, -match_vs_older_age3_sd)
# 
# ## plot contrasts
# prevsec_pred %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = pred_type))+
#   facet_wrap(. ~ stim_type)
# contrasts_long %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'younger',
#                             ifelse(move_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast)
# 
# save.image('movement_direction/movement_model_prevseccontrasts_epred.RData')
# 
# # predictions %>%
# #   mutate(previous = ifelse(move_tminus1_num == 1, 'younger',
# #                            ifelse(move_tminus1_num == 2, 'same age', 'older')),
# #          prediction = ifelse(prediction == 1, 'younger',
# #                              ifelse(prediction == 2, 'same age', 'older'))) %>%
# #   mutate(previous = factor(previous, levels = c('younger','same age','older')),
# #          prediction = factor(prediction, levels = c('younger','same age','older'))) %>%
# #   ggplot()+
# #   geom_bar(aes(x = prediction, fill = as.factor(previous)),
# #            position = 'dodge')+
# #   scale_y_continuous(expand = c(0,0))+
# #   labs(colour = 'previous second')+
# #   scale_fill_viridis_d()
# #
# # prevsec2 <- pred_prop %>% filter(move_tminus1_num == 2)
# # prevsec3 <- pred_prop %>% filter(move_tminus1_num == 3)
# # pred_prev <- pred_prop %>%
# #   filter(move_tminus1_num == 1) %>%
# #   rename(count_1 = count_predictions,
# #          prop_1 = proportion) %>%
# #   select(f_age_num, after_stim, stim_type, prediction, count_1, prop_1) %>%
# #   left_join(prevsec2[,c('f_age_num','after_stim','stim_type','prediction','count_predictions','proportion')],
# #             by = c('f_age_num','after_stim','stim_type','prediction')) %>%
# #   rename(count_2 = count_predictions,
# #          prop_2 = proportion) %>%
# #   left_join(prevsec3[,c('f_age_num','after_stim','stim_type','prediction','count_predictions','proportion')],
# #             by = c('f_age_num','after_stim','stim_type','prediction')) %>%
# #   rename(count_3 = count_predictions,
# #          prop_3 = proportion) %>%
# #   mutate(move1_2 = prop_1 - prop_2,
# #          move1_3 = prop_1 - prop_3,
# #          move2_3 = prop_2 - prop_3)
# # pred_prev %>%
# #   select(stim_type, after_stim, f_age_num, prediction,
# #          move1_2,move1_3,move2_3) %>%
# #   pivot_longer(cols = c('move1_2', 'move1_3', 'move2_3'),
# #                names_to = 'contrast', values_to = 'value') %>%
# #   mutate(contrast = ifelse(contrast == 'move1_2', 'younger vs same',
# #                            ifelse(contrast == 'move1_3', 'younger vs older',
# #                                   'same vs older'))) %>%
# #   ggplot()+
# #   geom_density(aes(x = value, colour = contrast), linewidth = 1)+
# #   scale_colour_viridis_d()+
# #   labs(colour = 't-1 pair',
# #        x = 'difference between movements at previous second')+
# #   geom_vline(xintercept = 0, linetype = 2)+
# #   scale_x_continuous(limits = c(-2,2))
# 
# # time since stimulus -- move ####
# # load('movement_direction/movement_model_prevseccontrasts_epred.RData')
# rm(young_move, young_mtx, match_move, match_mtx, older_move, older_mtx,
#    contrasts, contrasts_long, prevsec_pred,
#    young_vs_match_age1, young_vs_match_age2, young_vs_match_age3,
#    young_vs_older_age1, young_vs_older_age2, young_vs_older_age3,
#    match_vs_older_age1, match_vs_older_age2, match_vs_older_age3) ; gc()
# 
# ## predict with original times
# time_move_org <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# time_mtx_org <- posterior_epred(object = move_fit, newdata = time_move_org)
# colnames(time_mtx_org) <- time_move_org$unique_data_combo
# time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +15 seconds
# time_move_alt_0.25 <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.25 <- posterior_epred(object = move_fit, newdata = time_move_alt_0.25)
# colnames(time_mtx_alt_0.25) <- time_move_alt_0.25$unique_data_combo
# time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +30 seconds
# time_move_alt_0.50 <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1/2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.50 <- posterior_epred(object = move_fit, newdata = time_move_alt_0.50)
# colnames(time_mtx_alt_0.50) <- time_move_alt_0.50$unique_data_combo
# time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +45 seconds
# time_move_alt_0.75 <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 3/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.75 <- posterior_epred(object = move_fit, newdata = time_move_alt_0.75)
# colnames(time_mtx_alt_0.75) <- time_move_alt_0.75$unique_data_combo
# time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +60 seconds
# time_move_alt_1.00 <- move_no_na %>%
#   dplyr::select(f_age_num, stim_type, move_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_1.00 <- posterior_epred(object = move_fit, newdata = time_move_alt_1.00)
# colnames(time_mtx_alt_1.00) <- time_move_alt_1.00$unique_data_combo
# time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('movement_direction/movement_model_timecontrasts_epred.RData')
# 
# ## summarise and convert to long format
# time_pred <- time_move_org %>%
#   mutate(time_org_0.00_prop1_mu = apply(time_mtx_org[,,1], 2, mean),
#          time_org_0.00_prop2_mu = apply(time_mtx_org[,,2], 2, mean),
#          time_org_0.00_prop3_mu = apply(time_mtx_org[,,3], 2, mean),
#          time_org_0.00_prop1_sd = apply(time_mtx_org[,,1], 2, sd),
#          time_org_0.00_prop2_sd = apply(time_mtx_org[,,2], 2, sd),
#          time_org_0.00_prop3_sd = apply(time_mtx_org[,,3], 2, sd),
#          time_alt_0.25_prop1_mu = apply(time_mtx_alt_0.25[,,1], 2, mean),
#          time_alt_0.25_prop2_mu = apply(time_mtx_alt_0.25[,,2], 2, mean),
#          time_alt_0.25_prop3_mu = apply(time_mtx_alt_0.25[,,3], 2, mean),
#          time_alt_0.25_prop1_sd = apply(time_mtx_alt_0.25[,,1], 2, sd),
#          time_alt_0.25_prop2_sd = apply(time_mtx_alt_0.25[,,2], 2, sd),
#          time_alt_0.25_prop3_sd = apply(time_mtx_alt_0.25[,,3], 2, sd),
#          time_alt_0.50_prop1_mu = apply(time_mtx_alt_0.50[,,1], 2, mean),
#          time_alt_0.50_prop2_mu = apply(time_mtx_alt_0.50[,,2], 2, mean),
#          time_alt_0.50_prop3_mu = apply(time_mtx_alt_0.50[,,3], 2, mean),
#          time_alt_0.50_prop1_sd = apply(time_mtx_alt_0.50[,,1], 2, sd),
#          time_alt_0.50_prop2_sd = apply(time_mtx_alt_0.50[,,2], 2, sd),
#          time_alt_0.50_prop3_sd = apply(time_mtx_alt_0.50[,,3], 2, sd),
#          time_alt_0.75_prop1_mu = apply(time_mtx_alt_0.75[,,1], 2, mean),
#          time_alt_0.75_prop2_mu = apply(time_mtx_alt_0.75[,,2], 2, mean),
#          time_alt_0.75_prop3_mu = apply(time_mtx_alt_0.75[,,3], 2, mean),
#          time_alt_0.75_prop1_sd = apply(time_mtx_alt_0.75[,,1], 2, sd),
#          time_alt_0.75_prop2_sd = apply(time_mtx_alt_0.75[,,2], 2, sd),
#          time_alt_0.75_prop3_sd = apply(time_mtx_alt_0.75[,,3], 2, sd),
#          time_alt_1.00_prop1_mu = apply(time_mtx_alt_1.00[,,1], 2, mean),
#          time_alt_1.00_prop2_mu = apply(time_mtx_alt_1.00[,,2], 2, mean),
#          time_alt_1.00_prop3_mu = apply(time_mtx_alt_1.00[,,3], 2, mean),
#          time_alt_1.00_prop1_sd = apply(time_mtx_alt_1.00[,,1], 2, sd),
#          time_alt_1.00_prop2_sd = apply(time_mtx_alt_1.00[,,2], 2, sd),
#          time_alt_1.00_prop3_sd = apply(time_mtx_alt_1.00[,,3], 2, sd)) %>%
#   pivot_longer(cols = c(time_org_0.00_prop1_mu,time_org_0.00_prop2_mu,time_org_0.00_prop3_mu,
#                         time_alt_0.25_prop1_mu,time_alt_0.25_prop2_mu,time_alt_0.25_prop3_mu,
#                         time_alt_0.50_prop1_mu,time_alt_0.50_prop2_mu,time_alt_0.50_prop3_mu,
#                         time_alt_0.75_prop1_mu,time_alt_0.75_prop2_mu,time_alt_0.75_prop3_mu,
#                         time_alt_1.00_prop1_mu,time_alt_1.00_prop2_mu,time_alt_1.00_prop3_mu),
#                names_to = 'time_org_alt_prop_agemove_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(time_org_0.00_prop1_sd,time_org_0.00_prop2_sd,time_org_0.00_prop3_sd,
#                         time_alt_0.25_prop1_sd,time_alt_0.25_prop2_sd,time_alt_0.25_prop3_sd,
#                         time_alt_0.50_prop1_sd,time_alt_0.50_prop2_sd,time_alt_0.50_prop3_sd,
#                         time_alt_0.75_prop1_sd,time_alt_0.75_prop2_sd,time_alt_0.75_prop3_sd,
#                         time_alt_1.00_prop1_sd,time_alt_1.00_prop2_sd,time_alt_1.00_prop3_sd),
#                names_to = 'time_org_alt_prop_agemove_sd', values_to = 'stdv_propn') %>%
#   separate(col = time_org_alt_prop_agemove_mu,
#            into = c('time_mu','org_mu','alt_mu','prop_agemove_mu','mu'),
#            sep = '_', remove = T) %>%
#   separate(col = time_org_alt_prop_agemove_sd,
#            into = c('time_sd','org_sd','alt_sd','prop_agemove_sd','sd'),
#            sep = '_', remove = T) %>%
#   select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>%
#   filter(alt_mu == alt_sd & prop_agemove_sd == prop_agemove_mu) %>%
#   mutate(move_pred = ifelse(prop_agemove_mu == 'prop1', 1,
#                           ifelse(prop_agemove_mu == 'prop2', 2,
#                                  ifelse(prop_agemove_mu == 'prop3', 3, 4)))) %>%
#   select(-alt_sd, -prop_agemove_mu, -prop_agemove_sd) %>%
#   rename(mins_added = alt_mu) %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'younger',
#                             ifelse(move_pred == 2, 'matched', 'older')))
# 
# ## calculate contrasts
# alt0.25_vs_0.00_young <- time_mtx_alt_0.25[,,1] - time_mtx_org[,,1]
# alt0.25_vs_0.00_match <- time_mtx_alt_0.25[,,2] - time_mtx_org[,,2]
# alt0.25_vs_0.00_older <- time_mtx_alt_0.25[,,3] - time_mtx_org[,,3]
# 
# alt0.50_vs_0.25_young <- time_mtx_alt_0.50[,,1] - time_mtx_alt_0.25[,,1]
# alt0.50_vs_0.25_match <- time_mtx_alt_0.50[,,2] - time_mtx_alt_0.25[,,2]
# alt0.50_vs_0.25_older <- time_mtx_alt_0.50[,,3] - time_mtx_alt_0.25[,,3]
# 
# alt0.75_vs_0.50_young <- time_mtx_alt_0.75[,,1] - time_mtx_alt_0.50[,,1]
# alt0.75_vs_0.50_match <- time_mtx_alt_0.75[,,2] - time_mtx_alt_0.50[,,2]
# alt0.75_vs_0.50_older <- time_mtx_alt_0.75[,,3] - time_mtx_alt_0.50[,,3]
# 
# alt1.00_vs_0.75_young <- time_mtx_alt_1.00[,,1] - time_mtx_alt_0.75[,,1]
# alt1.00_vs_0.75_match <- time_mtx_alt_1.00[,,2] - time_mtx_alt_0.75[,,2]
# alt1.00_vs_0.75_older <- time_mtx_alt_1.00[,,3] - time_mtx_alt_0.75[,,3]
# 
# ## summarise contrasts
# contrasts <- move_no_na %>%
#   mutate(alt0.25_vs_0.00_young_mu = apply(alt0.25_vs_0.00_young, 2, mean),
#          alt0.25_vs_0.00_young_sd = apply(alt0.25_vs_0.00_young, 2, sd),
#          alt0.25_vs_0.00_match_mu = apply(alt0.25_vs_0.00_match, 2, mean),
#          alt0.25_vs_0.00_match_sd = apply(alt0.25_vs_0.00_match, 2, sd),
#          alt0.25_vs_0.00_older_mu = apply(alt0.25_vs_0.00_older, 2, mean),
#          alt0.25_vs_0.00_older_sd = apply(alt0.25_vs_0.00_older, 2, sd),
#          alt0.50_vs_0.25_young_mu = apply(alt0.50_vs_0.25_young, 2, mean),
#          alt0.50_vs_0.25_young_sd = apply(alt0.50_vs_0.25_young, 2, sd),
#          alt0.50_vs_0.25_match_mu = apply(alt0.50_vs_0.25_match, 2, mean),
#          alt0.50_vs_0.25_match_sd = apply(alt0.50_vs_0.25_match, 2, sd),
#          alt0.50_vs_0.25_older_mu = apply(alt0.50_vs_0.25_older, 2, mean),
#          alt0.50_vs_0.25_older_sd = apply(alt0.50_vs_0.25_older, 2, sd),
#          alt0.75_vs_0.50_young_mu = apply(alt0.75_vs_0.50_young, 2, mean),
#          alt0.75_vs_0.50_young_sd = apply(alt0.75_vs_0.50_young, 2, sd),
#          alt0.75_vs_0.50_match_mu = apply(alt0.75_vs_0.50_match, 2, mean),
#          alt0.75_vs_0.50_match_sd = apply(alt0.75_vs_0.50_match, 2, sd),
#          alt0.75_vs_0.50_older_mu = apply(alt0.75_vs_0.50_older, 2, mean),
#          alt0.75_vs_0.50_older_sd = apply(alt0.75_vs_0.50_older, 2, sd),
#          alt1.00_vs_0.75_young_mu = apply(alt1.00_vs_0.75_young, 2, mean),
#          alt1.00_vs_0.75_young_sd = apply(alt1.00_vs_0.75_young, 2, sd),
#          alt1.00_vs_0.75_match_mu = apply(alt1.00_vs_0.75_match, 2, mean),
#          alt1.00_vs_0.75_match_sd = apply(alt1.00_vs_0.75_match, 2, sd),
#          alt1.00_vs_0.75_older_mu = apply(alt1.00_vs_0.75_older, 2, mean),
#          alt1.00_vs_0.75_older_sd = apply(alt1.00_vs_0.75_older, 2, sd))
# contrasts_long <- contrasts %>%
#   select(-alt0.25_vs_0.00_young_sd,-alt0.25_vs_0.00_match_sd,-alt0.25_vs_0.00_older_sd,
#          -alt0.50_vs_0.25_young_sd,-alt0.50_vs_0.25_match_sd,-alt0.50_vs_0.25_older_sd,
#          -alt0.75_vs_0.50_young_sd,-alt0.75_vs_0.50_match_sd,-alt0.75_vs_0.50_older_sd,
#          -alt1.00_vs_0.75_young_sd,-alt1.00_vs_0.75_match_sd,-alt1.00_vs_0.75_older_sd) %>%
#   pivot_longer(cols = c(alt0.25_vs_0.00_young_mu,alt0.25_vs_0.00_match_mu,alt0.25_vs_0.00_older_mu,
#                         alt0.50_vs_0.25_young_mu,alt0.50_vs_0.25_match_mu,alt0.50_vs_0.25_older_mu,
#                         alt0.75_vs_0.50_young_mu,alt0.75_vs_0.50_match_mu,alt0.75_vs_0.50_older_mu,
#                         alt1.00_vs_0.75_young_mu,alt1.00_vs_0.75_match_mu,alt1.00_vs_0.75_older_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('alt','contrast'), sep = 3) %>%
#   separate(contrast, into = c('later','vs','earlier','pred_type','mu'),
#            sep = '_', remove = T) %>%
#   select(-alt, -vs, -mu) %>%
#   mutate(later = as.numeric(later),
#          earlier = as.numeric(earlier),
#          contrast = paste0(later,'_',earlier))
# 
# ## plot contrasts
# time_pred %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = pred_type))+
#   facet_wrap(. ~ stim_type)
# contrasts_long %>%
#   mutate(pred_type = ifelse(pred_type == 'young', 'younger',
#                             ifelse(pred_type == 'match', 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast, scales = 'free')
# save.image('movement_direction/movement_model_timecontrasts_epred.RData')
# 
