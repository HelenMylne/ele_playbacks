######## information ####
# simple models to compare before/during/after for each measure of social behaviour

######## set up ####
#library(tidyverse); library(brms) ; library(LaplacesDemon) ; library(ggpubr) ; library(patchwork)
library(tidyverse, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(ggpubr, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')

theme_set(theme_classic())

# ######## import data about playbacks ####
# ## read in age data
# ages <- readRDS('../data_processed/elephant_behaviour_proportions.RDS') %>%
#   select(pb_num, subject, targeted_elephant,    # random effects
#          stim_type,age_category,partner_age_category,age_difference, # exposures
#          age,partner_age,focal,dyad_partner,    # reference info
#          group_size                             # remove any with only 2
#   ) %>%
#   distinct()
# 
# ## read in stimulus start time data
# stim_starts <- readRDS('../data_processed/stimuli.RDS') %>%
#   filter(status == 'START' & behavior == 'STIMULUS') %>%
#   select(pb_num,time,stim_num,comment,stim_type)
# 
# ## remove false starts
# table(stim_starts$pb_num)
# multiple_starts <- c(10, 24, 29, 32, 46, 53)
# check <- stim_starts %>%
#   filter(pb_num %in% multiple_starts) # for stim 10+46+53 take first time, for 24+29+32 use second.
# for(i in multiple_starts){
#   x <- check %>% filter(pb_num == i)
#   check <- anti_join(check, x)
#   if(i %in% c(10,46,53)){
#     x <- x[1,]
#   }
#   if(i %in% c(24,29,32)){
#     x <- x[2,]
#   }
#   check <- rbind(check, x)
# }
# stim_starts <- stim_starts %>%
#   filter(! pb_num %in% multiple_starts) %>%
#   rbind(check) %>%
#   mutate(time = as.numeric(time)) %>%
#   mutate(stim_start = round(time, 0)) %>%
#   select(pb_num, stim_start,stim_num,stim_type)
# rm(check, i, multiple_starts, x) ; gc()
# 
# ## read in stop time data
# stim_stops <- readRDS('../data_processed/stimuli.RDS') %>%
#   filter(status == 'STOP' & behavior == 'STIMULUS') %>%
#   select(pb_num,time,stim_num,comment)
# 
# ## remove false starts
# (multiple_stops <- table(stim_stops$pb_num) %>%
#   as.data.frame() %>%
#   filter(Freq > 1))
# multiple_stops <- c(10, 24, 29, 32, 46, 53)
# check <- stim_stops %>%
#   filter(pb_num %in% multiple_stops) # 10+46 = take first time, 24+29+32+53 = take last time
# for(i in multiple_stops){
#   x <- check %>% filter(pb_num == i)
#   check <- anti_join(check, x)
#   if(i %in% c(10,46)){
#     x <- x[1,]
#   }
#   if(i %in% c(24,29,32,53)){
#     x <- x[nrow(x),]
#   }
#   check <- rbind(check, x)
# }
# stim_stops <- stim_stops %>%
#   filter(! pb_num %in% multiple_stops) %>%
#   rbind(check) %>%
#   mutate(time = as.numeric(time)) %>%
#   mutate(stim_stop = round(time, 0)) %>%
#   select(pb_num, stim_stop)
# rm(check, i, multiple_stops, x) ; gc()
# 
# ######## combine with behaviour data to identify before/during/after ####
# cols_of_interest <- c('b1_nn','b2_nn','b3_nn','b4_nn',
#                       'b5_nn','b6_nn','b7_nn','b8_nn',
#                       'b1_move','b2_move','b3_move','b4_move',
#                       'b5_move','b6_move','b7_move','b8_move',
#                       'b1_look','b2_look','b3_look','b4_look',
#                       'b5_look','b6_look','b7_look','b8_look')
# cols_of_interest_name <- c('b1_nn_name','b2_nn_name','b3_nn_name','b4_nn_name',
#                            'b5_nn_name','b6_nn_name','b7_nn_name','b8_nn_name',
#                            'b1_move_name','b2_move_name','b3_move_name','b4_move_name',
#                            'b5_move_name','b6_move_name','b7_move_name','b8_move_name',
#                            'b1_look_name','b2_look_name','b3_look_name','b4_look_name',
#                            'b5_look_name','b6_look_name','b7_look_name','b8_look_name')
# cols_of_interest_index <- c('b1_nn_index','b2_nn_index','b3_nn_index','b4_nn_index',
#                             'b5_nn_index','b6_nn_index','b7_nn_index','b8_nn_index',
#                             'b1_move_index','b2_move_index','b3_move_index','b4_move_index',
#                             'b5_move_index','b6_move_index','b7_move_index','b8_move_index',
#                             'b1_look_index','b2_look_index','b3_look_index','b4_look_index',
#                             'b5_look_index','b6_look_index','b7_look_index','b8_look_index')
# behav <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
#   select(subject,pb_num,second,out_frame_name,
#          all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
#   # convert to tidy format
#   rename(b1_nn = b1_nn_name, b2_nn = b2_nn_name,
#          b3_nn = b3_nn_name, b4_nn = b4_nn_name,
#          b5_nn = b5_nn_name, b6_nn = b6_nn_name,
#          b7_nn = b7_nn_name, b8_nn = b8_nn_name,
#          b1_move = b1_move_name, b2_move = b2_move_name,
#          b3_move = b3_move_name, b4_move = b4_move_name,
#          b5_move = b5_move_name, b6_move = b6_move_name,
#          b7_move = b7_move_name, b8_move = b8_move_name,
#          b1_look = b1_look_name, b2_look = b2_look_name,
#          b3_look = b3_look_name, b4_look = b4_look_name,
#          b5_look = b5_look_name, b6_look = b6_look_name,
#          b7_look = b7_look_name, b8_look = b8_look_name) %>%
#   pivot_longer(cols = all_of(cols_of_interest),
#                names_to = 'elephant_activity_name', values_to = 'action_name') %>%
#   rename(b1_nn = b1_nn_index, b2_nn = b2_nn_index,
#          b3_nn = b3_nn_index, b4_nn = b4_nn_index,
#          b5_nn = b5_nn_index, b6_nn = b6_nn_index,
#          b7_nn = b7_nn_index, b8_nn = b8_nn_index,
#          b1_move = b1_move_index, b2_move = b2_move_index,
#          b3_move = b3_move_index, b4_move = b4_move_index,
#          b5_move = b5_move_index, b6_move = b6_move_index,
#          b7_move = b7_move_index, b8_move = b8_move_index,
#          b1_look = b1_look_index, b2_look = b2_look_index,
#          b3_look = b3_look_index, b4_look = b4_look_index,
#          b5_look = b5_look_index, b6_look = b6_look_index,
#          b7_look = b7_look_index, b8_look = b8_look_index) %>%
#   pivot_longer(cols = all_of(cols_of_interest),
#                names_to = 'elephant_activity_index', values_to = 'action_index') %>%
#   filter(elephant_activity_name == elephant_activity_index) %>%
#   select(-elephant_activity_index) %>%
#   # clean up
#   rename(elephant_activity = elephant_activity_name) %>%
#   filter(action_name != 'impossible_partner') %>%
#   # fix out of sight variable
#   mutate(action_name = ifelse(out_frame_name == 'out_of_sight',
#                               'out_of_sight', action_name),
#          action_index = ifelse(out_frame_name == 'out_of_sight',
#                                9, action_index)) %>%
#   # join on explanatory and random variables
#   separate(elephant_activity, into = c('targeted_elephant','activity'), sep = '_', remove = T) %>%
#   mutate(targeted_elephant = paste0(targeted_elephant, '_e', pb_num)) %>%
#   left_join(ages[,c('subject','targeted_elephant','age_category','partner_age_category','age','partner_age')],
#             by = c('subject','targeted_elephant')) %>%
#   rename(focal = subject,
#          partner = targeted_elephant,
#          f_age_num = age_category,
#          f_age_cat = age,
#          p_age_num = partner_age_category,
#          p_age_cat = partner_age) %>%
#   mutate(pb_num = as.numeric(pb_num)) %>%
#   left_join(stim_starts, by = 'pb_num') %>%
#   left_join(stim_stops, by = 'pb_num') %>%
#   # clean up
#   mutate(bda = ifelse(second < stim_start, 'before',
#                       ifelse(second < stim_stop, 'during','after'))) %>%
#   dplyr::select(pb_num,focal,partner,activity,action_name,action_index,
#                 stim_num,stim_type,stim_start,stim_stop,second,bda,
#                 f_age_cat,p_age_cat,f_age_num,p_age_num) %>%
#   mutate(f_age_num = as.factor(f_age_num),
#          p_age_num = as.factor(p_age_num),
#          age_combo = paste0(f_age_num,'_',p_age_num))
# rm(cols_of_interest, cols_of_interest_name, cols_of_interest_index, stim_starts, stim_stops, ages) ; gc()
# 
# saveRDS(behav, '../data_processed/behaviour_by_second_indexvariables_bda.RDS')
# 
# ## remove individuals where ages are unknown
# behav <- behav %>%
#   filter(!is.na(f_age_num))
# 
# ######## nearest neighbour ####
# pdf('../outputs/neighbour_binomial_model_bda/neighbour_binomial_modelchecks.pdf')
# 
# #### create data ####
# ## select specific data
# nn <- behav %>%
#   filter(activity == 'nn') %>%
#   select(-activity, -stim_start, -stim_stop) %>%
#   mutate(prev = NA,
#          action = ifelse(action_name == 'out_of_sight', 9,
#                          action_index - 1),
#          f_age_num = as.factor(as.numeric(f_age_num)),
#          p_age_num = as.factor(as.numeric(p_age_num))) %>%
#   filter(!is.na(p_age_num)) %>%
#   relocate(action, .after = action_index)
# 
# # create variable for nearest neighbour at time t-1
# focals <- unique(nn$focal)
# for(f in 1:length(focals)){
#   focal <- nn %>% filter(focal == focals[f])
#   nn <- nn %>% anti_join(focal, by = 'focal')
#   partners <- unique(focal$partner)
#   for(p in 1:length(partners)){
#     focal_partner <- focal %>% filter(partner == partners[p])
#     focal <- focal %>% anti_join(focal_partner, by = 'partner')
#     for(i in 2:nrow(focal_partner)){
#       focal_partner$prev[i] <- focal_partner$action[i-1]
#     }
#     focal <- rbind(focal, focal_partner)
#   }
#   nn <- rbind(nn, focal)
# }
# rm(focal, focals, focal_partner, f, p, i, partners) ; gc()
# 
# ## remove observations with missing data
# nn <- nn %>%
#   filter(action != 9) %>%
#   filter(prev != 9) %>%
#   filter(!is.na(prev))
# 
# ## check numbers for current vs previous second
# table(nn$action_name, nn$prev)
# 
# #### set prior ####
# get_prior(formula = action ~ 1 + age_combo + stim_type + bda + prev +
#             (1|focal) + (1|stim_num) + (1|pb_num),
#           data = nn, family = bernoulli("logit"))
# priors <- c(
# #  # focal age
# #  prior(normal(0,1),      class = b,    coef = mof_age_num),
# #  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
# #  # partner age
# #  prior(normal(0,1),      class = b,    coef = mop_age_num),
# #  prior(dirichlet(2,2,2), class = simo, coef = mop_age_num1),
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
#   prior(normal(0,1),      class = b,    coef = bdaduring),
#   # action in previous second
#   # prior(normal(0,1),      class = b,    coef = moprev),
#   # prior(dirichlet(2),   class = simo, coef = moprev1))
#   prior(normal(1,1),      class = b,    coef = prev))
# 
# ## prior predictive check
# num_chains <- 4
# num_iter <- 2000
# nbm_prior <- brm(
#   formula = action ~ 1 + age_combo + stim_type + bda + prev +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = nn, family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# 
# pp_check(nbm_prior) # y is quite skewed, prior is mostly symmetrical, but data still fall within it
# 
# #### fit model ####
# nbm_fit <- brm(
#   formula = action ~ 1 + age_combo + stim_type + bda + prev +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = nn, family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# save.image('nearest_neighbour/neighbour_binomial_run.RData')
# 
# ## check model fit -- Rhat very good, ESS a bit crap, may need to run for more iterations
# # load('nearest_neighbour/neighbour_binomial_run.RData')
# (summary <- summary(nbm_fit))
# par(mfrow = c(3,1))
# hist(summary$fixed$Rhat, breaks = 50)
# hist(summary$fixed$Bulk_ESS, breaks = 50)
# hist(summary$fixed$Tail_ESS, breaks = 50)
# par(mfrow = c(1,1))
# 
# ## extract posterior distribution
# draws <- as_draws_df(nbm_fit) %>%
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
# # extract marginal effects
# marg <- conditional_effects(nbm_fit,
#                             effects = c('age_combo','stim_type',
#                                         'bda','prev'),
#                             categorical = FALSE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# age_effect <- marg[[1]]
# stim_effect <- marg[[2]]
# bda_effect <- marg[[3]]
# prev_effect <- marg[[4]]
# 
# #### plot marginal effects ####
# neighbour_labels <- c('neighbour age category 1',
#                       'neighbour age category 2',
#                       'neighbour age category 3',
#                       'neighbour age category 4')
# names(neighbour_labels) <- c(1:4)
# (focal_age_plot <- age_effect %>%
#    separate(col = age_combo, sep = '_', remove = F,
#             into = c('focal_age','neighbour_age')) %>%
#    mutate(agecombo = paste0(focal_age,'-',neighbour_age)) %>%
#    ggplot()+
#    geom_errorbar(aes(#x = agecombo,
#                      x = focal_age,
#                      colour = focal_age,
#                      #linetype = neighbour_age,
#                      ymax = upper__, ymin = lower__),
#                  linewidth = 1, width = 0.2)+
#    geom_point(aes(#x = agecombo,
#                   x = focal_age,
#                   colour = focal_age,
#                   #shape = neighbour_age,
#                   y = estimate__),
#               cex = 3)+
#    #xlab(label = 'combined age categories')+
#    xlab(label = 'focal age category')+
#    ylab('probability of being nearest neighbours:\nafter dove stimulus, not nearest neighbours in previous second')+
#    scale_colour_viridis_d(name = 'focal age:')+
#    #scale_linetype(name = 'neighbour age line type:')+
#    #scale_shape_manual(name = 'neighbour age shape:', values = c(15:18))+
#    facet_wrap(. ~ neighbour_age,
#               labeller = labeller(neighbour_age = neighbour_labels))+
#    theme(legend.direction = 'horizontal',
#          legend.position = 'bottom',
#          legend.box = 'vertical',
#          legend.spacing.x = unit(0.2, 'cm'),
#          legend.spacing.y = unit(2, 'mm'),
#          axis.title = element_text(size = 16),
#          axis.text.x = element_text(size = 12,
#                                     #angle = 70,
#                                     vjust = 0.5),
#          axis.text.y = element_text(size = 12),
#          legend.title = element_text(size = 12),
#          legend.text = element_text(size = 10)) )
# ggsave(plot = focal_age_plot, filename = '../outputs/neighbour_binomial_model_bda/neighbour_binomial_marginaleffects_focalage.png',
#        device = 'png', width = 8.3, height = 5.8)
# 
# (stim_plot <- stim_effect %>%
#     ggplot()+
#     geom_errorbar(aes(x = stim_type,
#                       colour = stim_type,
#                       ymax = upper__, ymin = lower__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type,
#                    colour = stim_type,
#                    shape = stim_type,
#                    y = estimate__),
#               cex = 3)+
#     ylab('probability of being nearest neighbours after stimulus:\nage 1 with age 1, not neighbours in previous second')+
#     scale_colour_viridis_d(name = 'stimulus type:')+
#     scale_shape_manual(name = 'stimulus type:', values = c(15:18))+
#     scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
#                      labels = c('dove (control)', 'lion', 'human'),
#                      limits = c('ctd','l','h'))+
#     theme(legend.position = 'none',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = stim_plot, filename = '../outputs/neighbour_binomial_model_bda/neighbour_binomial_marginaleffects_stimtype.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# (all_plots <- ggarrange(focal_age_plot, stim_plot, ncol=2, nrow=1, common.legend = FALSE, legend = "bottom"))
# ggsave(plot = all_plots, filename = '../outputs/neighbour_binomial_model_bda/neighbour_binomial_marginaleffects.png', device = 'png',
#        width = (5.8*2), height = 8.3)
# 
# rm(all_plots,focal_age_plot,stim_plot,age_effect,prev_effect,stim_effect,bda_effect) ;gc()
# 
# #### posterior predictive check ####
# pp_check(nbm_fit, ndraws = 100) # perfect fit
# 
# #### plot traces ####
# parameters_of_interest <- parameters[1:which(parameters == 'b_prev')]
# draws %>%
#   filter(parameter %in% parameters_of_interest) %>%
#   ggplot(aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mostly fine, but playback ID intercept has a weird unmixed bit
# 
# #### plot density curves ####
# draws %>%
#   filter(parameter %in% parameters_of_interest) %>%
#   ggplot(aes(x = draw, colour = as.factor(chain)))+
#   geom_density()+
#   facet_wrap(. ~ parameter, scales = 'free')+
#   theme(legend.position = 'none')
# 
# save.image('nearest_neighbour/neighbour_binomial_run.RData')
# 
# ## reset plotting
# dev.off()
# pdf('../outputs/neighbour_binomial_model_bda/neighbour_binomial_modelpredictions.pdf')
# 
# #### predict from model ####
# # load('nearest_neighbour/neighbour_binomial_run.RData')
# rm(list = ls()[! ls() %in% c('nbm_fit','nn')])
# 
# pred <- posterior_epred(object = nbm_fit,
#                         newdata = nn)
# save.image('nearest_neighbour/neighbour_binomial_predictions.RData')
# 
# ## convert to data frame
# nn$data_row <- 1:nrow(nn)
# predictions <- as.data.frame(pred)
# colnames(predictions) <- 1:nrow(nn)
# predictions <- predictions %>%
#   pivot_longer(cols = everything(),
#                names_to = 'data_row', values_to = 'epred') %>%
#   mutate(data_row = as.integer(data_row)) %>%
#   left_join(nn, by = 'data_row')
# 
# save.image('nearest_neighbour/neighbour_binomial_predictions.RData')
# rm(pred) ; gc()
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions ####
# #load('nearest_neighbour/neighbour_binomial_predictions.RData')
# 
# ## make labels for neighbour in previous second
# prevsec_labels <- c('not neighbour at t-1',
#                     'neighbour at t-1')
# names(prevsec_labels) <- c(0,1)
# 
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# (ctd_plot <- predictions %>%
#     filter(stim_type == 'ctd') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     # fill = factor(pred_type, levels = c('not neighbour', 'neighbour')),
#                     # colour = factor(pred_type, levels = c('not neighbour', 'neighbour'))
#                     colour = bda
#                     )) +
#     facet_grid(prev ~ .,
#                labeller = labeller(prev = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of neighbour relative to focal:',
#          fill = 'predicted direction of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'cape turtle dove (control)')+
#     theme(legend.position = 'bottom'))
# print('ctd_plot completed')
# 
# (lion_plot <- predictions %>%
#     filter(stim_type == 'l') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     # fill = factor(pred_type, levels = c('not neighbour', 'neighbour')),
#                     # colour = factor(pred_type, levels = c('not neighbour', 'neighbour'))
#                     colour = bda
#                     )) +
#     facet_grid(prev ~ .,
#                labeller = labeller(prev = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of neighbour relative to focal:',
#          fill = 'predicted direction of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'lion')+
#     theme(legend.position = 'bottom'))
# print('lion_plot completed')
# 
# (human_plot <- predictions %>%
#     filter(stim_type == 'h') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     # fill = factor(pred_type, levels = c('not neighbour', 'neighbour')),
#                     # colour = factor(pred_type, levels = c('not neighbour', 'neighbour'))
#                     colour = bda
#                     )) +
#     facet_grid(prev ~ .,
#                labeller = labeller(prev = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of neighbour relative to focal:',
#          fill = 'predicted direction of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# print('human_plot completed')
# 
# # (ctd_plot + lion_plot + human_plot)+
# #   plot_annotation(tag_levels = 'a')
# # ggsave(plot = last_plot(), file = '../outputs/neighbour_binomial_model_bda/neighbour_binomial_predictions_violin.png',
# #        device = 'png', height = 8, width = 48)
# 
# ## reset plotting
# dev.off()
# 
# #### calculate posterior contrasts from predictions ####
# load('nearest_neighbour/neighbour_binomial_predictions.RData')
# pdf('../outputs/neighbour_binomial_model_bda/neighbour_binomial_modelcontrasts.pdf')
# 
# ## stim type ####
# stim_new <- nn %>%
#   dplyr::select(age_combo, stim_type, prev, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(age_combo, prev, bda,
#                                                          focal, stim_num, pb_num))))
# 
# ## redo predictions with different stimulus types: all doves
# ctd_nn <- stim_new %>%
#   mutate(stim_type = 'ctd')
# ctd_mtx <- posterior_epred(object = nbm_fit, newdata = ctd_nn)
# colnames(ctd_mtx) <- ctd_nn$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
# 
# ## redo predictions with different stimulus types: all lions
# lion_nn <- stim_new %>%
#   mutate(stim_type = 'l')
# lion_mtx <- posterior_epred(object = nbm_fit, newdata = lion_nn)
# colnames(lion_mtx) <- lion_nn$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
# 
# ## redo predictions with different stimulus types: all humans
# human_nn <- stim_new %>%
#   mutate(stim_type = 'h')
# human_mtx <- posterior_epred(object = nbm_fit, newdata = human_nn)
# colnames(human_mtx) <- human_nn$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
# 
# save.image('nearest_neighbour/neighbour_binomial_stimuluscontrasts.RData')
# 
# ## calculate contrasts
# ctd_vs_lion <- lion_mtx - ctd_mtx
# ctd_vs_human <- human_mtx - ctd_mtx
# lion_vs_human <- human_mtx - lion_mtx
# 
# ## summarise contrasts
# contrasts <- nn %>%
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
# median(ctd_vs_lion)  ; mean(ctd_vs_lion)  ; sd(ctd_vs_lion)   #
# median(ctd_vs_human) ; mean(ctd_vs_human) ; sd(ctd_vs_human)  #
# median(lion_vs_human); mean(lion_vs_human); sd(lion_vs_human) #
# 
# ## plot contrasts
# contrasts_long %>%
#   mutate(contrast = ifelse(contrast == 'ctd_vs_human',
#                            'dove -> human',
#                            ifelse(contrast == 'ctd_vs_lion',
#                                   'dove -> lion', 'lion -> human'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast))+
#   scale_colour_viridis_d()+
#   labs(colour = 'effect of changing stimulus')
# 
# save.image('nearest_neighbour/neighbour_binomial_stimuluscontrasts.RData')
# 
# ## age combo ####
# # load('nearest_neighbour/neighbour_binomial_stimuluscontrasts.RData')
# rm(list = ls()[!ls() %in% c('nbm_fit','nn')]) ; gc()
# 
# ## create new dataframe to predict from
# age_new <- nn %>% 
#   dplyr::select(age_combo, stim_type, prev, bda, 
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(age_combo, prev, bda,
#                                                          focal, stim_num, pb_num))))
# ## predict with original ages
# age_nn_org <- age_new
# age_mtx_org <- posterior_epred(object = nbm_fit, newdata = age_nn_org)
# colnames(age_mtx_org) <- age_nn_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),]
# 
# ## redo predictions with altered age combinations
# age_predict <- function(df, age_combination, model){
#   df_new <- df %>% 
#     mutate(age_combo = age_combination)
#   mtx <- posterior_epred(object = model, newdata = df_new)
#   colnames(mtx) <- df_new$unique_data_combo
#   mtx <- mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
#   return(mtx)
# }
# mtx_11 <- age_predict(df = age_new, age_combination = '1_1', model = nbm_fit)
# mtx_12 <- age_predict(df = age_new, age_combination = '1_2', model = nbm_fit)
# mtx_13 <- age_predict(df = age_new, age_combination = '1_3', model = nbm_fit)
# mtx_14 <- age_predict(df = age_new, age_combination = '1_4', model = nbm_fit)
# mtx_21 <- age_predict(df = age_new, age_combination = '2_1', model = nbm_fit)
# mtx_22 <- age_predict(df = age_new, age_combination = '2_2', model = nbm_fit)
# mtx_23 <- age_predict(df = age_new, age_combination = '2_3', model = nbm_fit)
# mtx_24 <- age_predict(df = age_new, age_combination = '2_4', model = nbm_fit)
# mtx_31 <- age_predict(df = age_new, age_combination = '3_1', model = nbm_fit)
# mtx_32 <- age_predict(df = age_new, age_combination = '3_2', model = nbm_fit)
# mtx_33 <- age_predict(df = age_new, age_combination = '3_3', model = nbm_fit)
# mtx_34 <- age_predict(df = age_new, age_combination = '3_4', model = nbm_fit)
# mtx_41 <- age_predict(df = age_new, age_combination = '4_1', model = nbm_fit)
# mtx_42 <- age_predict(df = age_new, age_combination = '4_2', model = nbm_fit)
# mtx_43 <- age_predict(df = age_new, age_combination = '4_3', model = nbm_fit)
# mtx_44 <- age_predict(df = age_new, age_combination = '4_4', model = nbm_fit)
# 
# save.image('nearest_neighbour/neighbour_binomial_agecontrasts.RData')
# 
# ## calculate contrasts
# contrast_11v12 <- mtx_11 - mtx_12
# contrast_11v13 <- mtx_11 - mtx_13
# contrast_11v14 <- mtx_11 - mtx_14
# contrast_11v21 <- mtx_11 - mtx_21
# contrast_11v31 <- mtx_11 - mtx_31
# contrast_11v41 <- mtx_11 - mtx_41
# contrast_12v13 <- mtx_12 - mtx_13
# contrast_12v14 <- mtx_12 - mtx_14
# contrast_12v22 <- mtx_12 - mtx_22
# contrast_12v32 <- mtx_12 - mtx_32
# contrast_12v42 <- mtx_12 - mtx_42
# contrast_13v14 <- mtx_13 - mtx_14
# contrast_13v23 <- mtx_13 - mtx_23
# contrast_13v33 <- mtx_13 - mtx_33
# contrast_13v43 <- mtx_13 - mtx_43
# contrast_14v24 <- mtx_14 - mtx_24
# contrast_14v34 <- mtx_14 - mtx_34
# contrast_14v44 <- mtx_14 - mtx_44
# 
# contrast_21v22 <- mtx_21 - mtx_22
# contrast_21v23 <- mtx_21 - mtx_23
# contrast_21v24 <- mtx_21 - mtx_24
# contrast_21v31 <- mtx_21 - mtx_31
# contrast_21v41 <- mtx_21 - mtx_41
# contrast_22v23 <- mtx_22 - mtx_23
# contrast_22v24 <- mtx_22 - mtx_24
# contrast_22v32 <- mtx_22 - mtx_32
# contrast_22v42 <- mtx_22 - mtx_42
# contrast_23v24 <- mtx_23 - mtx_24
# contrast_23v33 <- mtx_23 - mtx_33
# contrast_23v43 <- mtx_23 - mtx_43
# contrast_24v34 <- mtx_24 - mtx_34
# contrast_24v44 <- mtx_24 - mtx_44
# 
# contrast_31v32 <- mtx_31 - mtx_32
# contrast_31v33 <- mtx_31 - mtx_33
# contrast_31v34 <- mtx_31 - mtx_34
# contrast_31v41 <- mtx_31 - mtx_41
# contrast_32v33 <- mtx_32 - mtx_33
# contrast_32v34 <- mtx_32 - mtx_34
# contrast_32v42 <- mtx_32 - mtx_42
# contrast_33v34 <- mtx_33 - mtx_34
# contrast_33v43 <- mtx_33 - mtx_43
# contrast_34v44 <- mtx_34 - mtx_44
# 
# contrast_41v42 <- mtx_41 - mtx_42
# contrast_41v43 <- mtx_41 - mtx_43
# contrast_41v44 <- mtx_41 - mtx_44
# contrast_42v43 <- mtx_42 - mtx_43
# contrast_42v44 <- mtx_42 - mtx_44
# contrast_43v44 <- mtx_43 - mtx_44
# 
# ## summarise contrasts
# contrasts <- nn %>%
#   select(-stim_type) %>%
#   mutate(mu_11v12 = apply(contrast_11v12, 2, mean),
#          sd_11v12 = apply(contrast_11v12, 2, sd),
#          mu_11v13 = apply(contrast_11v13, 2, mean),
#          sd_11v13 = apply(contrast_11v13, 2, sd),
#          mu_11v14 = apply(contrast_11v14, 2, mean),
#          sd_11v14 = apply(contrast_11v14, 2, sd),
#          mu_11v21 = apply(contrast_11v21, 2, mean),
#          sd_11v21 = apply(contrast_11v21, 2, sd),
#          mu_11v31 = apply(contrast_11v31, 2, mean),
#          sd_11v31 = apply(contrast_11v31, 2, sd),
#          mu_11v41 = apply(contrast_11v41, 2, mean),
#          sd_11v41 = apply(contrast_11v41, 2, sd),
#          
#          mu_12v13 = apply(contrast_12v13, 2, mean),
#          sd_12v13 = apply(contrast_12v13, 2, sd),
#          mu_12v14 = apply(contrast_12v14, 2, mean),
#          sd_12v14 = apply(contrast_12v14, 2, sd),
#          mu_12v22 = apply(contrast_12v22, 2, mean),
#          sd_12v22 = apply(contrast_12v22, 2, sd),
#          mu_12v32 = apply(contrast_12v32, 2, mean),
#          sd_12v32 = apply(contrast_12v32, 2, sd),
#          mu_12v42 = apply(contrast_12v42, 2, mean),
#          sd_12v42 = apply(contrast_12v42, 2, sd),
#          
#          mu_13v14 = apply(contrast_13v14, 2, mean),
#          sd_13v14 = apply(contrast_13v14, 2, sd),
#          mu_13v23 = apply(contrast_13v23, 2, mean),
#          sd_13v23 = apply(contrast_13v23, 2, sd),
#          mu_13v33 = apply(contrast_13v33, 2, mean),
#          sd_13v33 = apply(contrast_13v33, 2, sd),
#          mu_13v43 = apply(contrast_13v43, 2, mean),
#          sd_13v43 = apply(contrast_13v43, 2, sd),
#          
#          mu_14v24 = apply(contrast_14v24, 2, mean),
#          sd_14v24 = apply(contrast_14v24, 2, sd),
#          mu_14v34 = apply(contrast_14v34, 2, mean),
#          sd_14v34 = apply(contrast_14v34, 2, sd),
#          mu_14v44 = apply(contrast_14v44, 2, mean),
#          sd_14v44 = apply(contrast_14v44, 2, sd),
#          
#          mu_21v22 = apply(contrast_21v22, 2, mean),
#          sd_21v22 = apply(contrast_21v22, 2, sd),
#          mu_21v23 = apply(contrast_21v23, 2, mean),
#          sd_21v23 = apply(contrast_21v23, 2, sd),
#          mu_21v24 = apply(contrast_21v24, 2, mean),
#          sd_21v24 = apply(contrast_21v24, 2, sd),
#          mu_21v31 = apply(contrast_21v31, 2, mean),
#          sd_21v31 = apply(contrast_21v31, 2, sd),
#          mu_21v41 = apply(contrast_21v41, 2, mean),
#          sd_21v41 = apply(contrast_21v41, 2, sd),
#          
#          mu_22v23 = apply(contrast_22v23, 2, mean),
#          sd_22v23 = apply(contrast_22v23, 2, sd),
#          mu_22v24 = apply(contrast_22v24, 2, mean),
#          sd_22v24 = apply(contrast_22v24, 2, sd),
#          mu_22v32 = apply(contrast_22v32, 2, mean),
#          sd_22v32 = apply(contrast_22v32, 2, sd),
#          mu_22v42 = apply(contrast_22v42, 2, mean),
#          sd_22v42 = apply(contrast_22v42, 2, sd),
#          
#          mu_23v24 = apply(contrast_23v24, 2, mean),
#          sd_23v24 = apply(contrast_23v24, 2, sd),
#          mu_23v33 = apply(contrast_23v33, 2, mean),
#          sd_23v33 = apply(contrast_23v33, 2, sd),
#          mu_23v43 = apply(contrast_23v43, 2, mean),
#          sd_23v43 = apply(contrast_23v43, 2, sd),
#          
#          mu_24v34 = apply(contrast_24v34, 2, mean),
#          sd_24v34 = apply(contrast_24v34, 2, sd),
#          mu_24v44 = apply(contrast_24v44, 2, mean),
#          sd_24v44 = apply(contrast_24v44, 2, sd),
#          
#          mu_31v32 = apply(contrast_31v32, 2, mean),
#          sd_31v32 = apply(contrast_31v32, 2, sd),
#          mu_31v33 = apply(contrast_31v33, 2, mean),
#          sd_31v33 = apply(contrast_31v33, 2, sd),
#          mu_31v34 = apply(contrast_31v34, 2, mean),
#          sd_31v34 = apply(contrast_31v34, 2, sd),
#          mu_31v41 = apply(contrast_31v41, 2, mean),
#          sd_31v41 = apply(contrast_31v41, 2, sd),
#          
#          mu_32v33 = apply(contrast_32v33, 2, mean),
#          sd_32v33 = apply(contrast_32v33, 2, sd),
#          mu_32v34 = apply(contrast_32v34, 2, mean),
#          sd_32v34 = apply(contrast_32v34, 2, sd),
#          mu_32v42 = apply(contrast_32v42, 2, mean),
#          sd_32v42 = apply(contrast_32v42, 2, sd),
#          
#          mu_33v34 = apply(contrast_33v34, 2, mean),
#          sd_33v34 = apply(contrast_33v34, 2, sd),
#          mu_33v43 = apply(contrast_33v43, 2, mean),
#          sd_33v43 = apply(contrast_33v43, 2, sd),
#          
#          mu_34v44 = apply(contrast_34v44, 2, mean),
#          sd_34v44 = apply(contrast_34v44, 2, sd),
#          
#          mu_41v42 = apply(contrast_41v42, 2, mean),
#          sd_41v42 = apply(contrast_41v42, 2, sd),
#          mu_41v43 = apply(contrast_41v43, 2, mean),
#          sd_41v43 = apply(contrast_41v43, 2, sd),
#          mu_41v44 = apply(contrast_41v44, 2, mean),
#          sd_41v44 = apply(contrast_41v44, 2, sd),
#          
#          mu_42v43 = apply(contrast_42v43, 2, mean),
#          sd_42v43 = apply(contrast_42v43, 2, sd),
#          mu_42v44 = apply(contrast_42v44, 2, mean),
#          sd_42v44 = apply(contrast_42v44, 2, sd),
#          
#          mu_43v44 = apply(contrast_43v44, 2, mean),
#          sd_43v44 = apply(contrast_43v44, 2, sd)
#   )
# 
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(mu_11v12,mu_11v13,mu_11v14,mu_11v21,mu_11v31,mu_11v41,
#                         mu_12v13,mu_12v14,mu_12v22,mu_12v32,mu_12v42,
#                         mu_13v14,mu_13v23,mu_13v33,mu_13v43,
#                         mu_14v24,mu_14v34,mu_14v44,
#                         mu_21v22,mu_21v23,mu_21v24,mu_21v31,mu_21v41,
#                         mu_22v23,mu_22v24,mu_22v32,mu_22v42,
#                         mu_23v24,mu_23v33,mu_23v43,
#                         mu_24v34,mu_24v44,
#                         mu_31v32,mu_31v33,mu_31v34,mu_31v41,
#                         mu_32v33,mu_32v34,mu_32v42,
#                         mu_33v34,mu_33v43,
#                         mu_34v44,
#                         mu_41v42,mu_41v43,mu_41v44,
#                         mu_42v43,mu_42v44,
#                         mu_43v44),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('mu','combo1','v','combo2'),
#            sep = c(3,5,6), remove = T) %>%
#   select(-sd_11v12,-sd_11v13,-sd_11v14,-sd_11v21,-sd_11v31,-sd_11v41,
#          -sd_12v13,-sd_12v14,-sd_12v22,-sd_12v32,-sd_12v42,
#          -sd_13v14,-sd_13v23,-sd_13v33,-sd_13v43,
#          -sd_14v24,-sd_14v34,-sd_14v44,
#          -sd_21v22,-sd_21v23,-sd_21v24,-sd_21v31,-sd_21v41,
#          -sd_22v23,-sd_22v24,-sd_22v32,-sd_22v42,
#          -sd_23v24,-sd_23v33,-sd_23v43,
#          -sd_24v34,-sd_24v44,
#          -sd_31v32,-sd_31v33,-sd_31v34,-sd_31v41,
#          -sd_32v33,-sd_32v34,-sd_32v42,
#          -sd_33v34,-sd_33v43,
#          -sd_34v44,
#          -sd_41v42,-sd_41v43,-sd_41v44,
#          -sd_42v43,-sd_42v44,
#          -sd_43v44,
#          -mu)
# 
# ## save contrasts
# save.image('nearest_neighbour/neighbour_binomial_agecontrasts.RData')
# 
# ## produce values for reporting
# median(contrast_11v12); mean(contrast_11v12); sd(contrast_11v12)  # 
# median(contrast_11v13); mean(contrast_11v13); sd(contrast_11v13)  # 
# median(contrast_11v14); mean(contrast_11v14); sd(contrast_11v14)  # 
# median(contrast_11v21); mean(contrast_11v21); sd(contrast_11v21)  # 
# median(contrast_11v31); mean(contrast_11v31); sd(contrast_11v31)  # 
# median(contrast_11v41); mean(contrast_11v41); sd(contrast_11v41)  # 
# median(contrast_12v13); mean(contrast_12v13); sd(contrast_12v13)  # 
# median(contrast_12v14); mean(contrast_12v14); sd(contrast_12v14)  # 
# median(contrast_12v22); mean(contrast_12v22); sd(contrast_12v22)  # 
# median(contrast_12v32); mean(contrast_12v32); sd(contrast_12v32)  # 
# median(contrast_12v42); mean(contrast_12v42); sd(contrast_12v42)  # 
# median(contrast_13v14); mean(contrast_13v14); sd(contrast_13v14)  # 
# median(contrast_13v23); mean(contrast_13v23); sd(contrast_13v23)  # 
# median(contrast_13v33); mean(contrast_13v33); sd(contrast_13v33)  # 
# median(contrast_13v43); mean(contrast_13v43); sd(contrast_13v43)  # 
# median(contrast_14v24); mean(contrast_14v24); sd(contrast_14v24)  # 
# median(contrast_14v34); mean(contrast_14v34); sd(contrast_14v34)  # 
# median(contrast_14v44); mean(contrast_14v44); sd(contrast_14v44)  # 
# 
# median(contrast_21v22); mean(contrast_21v22); sd(contrast_21v22)  # 
# median(contrast_21v23); mean(contrast_21v23); sd(contrast_21v23)  # 
# median(contrast_21v24); mean(contrast_21v24); sd(contrast_21v24)  # 
# median(contrast_21v31); mean(contrast_21v31); sd(contrast_21v31)  # 
# median(contrast_21v41); mean(contrast_21v41); sd(contrast_21v41)  # 
# median(contrast_22v23); mean(contrast_22v23); sd(contrast_22v23)  # 
# median(contrast_22v24); mean(contrast_22v24); sd(contrast_22v24)  # 
# median(contrast_22v32); mean(contrast_22v32); sd(contrast_22v32)  # 
# median(contrast_22v42); mean(contrast_22v42); sd(contrast_22v42)  # 
# median(contrast_23v24); mean(contrast_23v24); sd(contrast_23v24)  # 
# median(contrast_23v33); mean(contrast_23v33); sd(contrast_23v33)  # 
# median(contrast_23v43); mean(contrast_23v43); sd(contrast_23v43)  # 
# median(contrast_24v34); mean(contrast_24v34); sd(contrast_24v34)  # 
# median(contrast_24v44); mean(contrast_24v44); sd(contrast_24v44)  # 
# 
# median(contrast_31v32); mean(contrast_31v32); sd(contrast_31v32)  # 
# median(contrast_31v33); mean(contrast_31v33); sd(contrast_31v33)  # 
# median(contrast_31v34); mean(contrast_31v34); sd(contrast_31v34)  # 
# median(contrast_31v41); mean(contrast_31v41); sd(contrast_31v41)  # 
# median(contrast_32v33); mean(contrast_32v33); sd(contrast_32v33)  # 
# median(contrast_32v34); mean(contrast_32v34); sd(contrast_32v34)  # 
# median(contrast_32v42); mean(contrast_32v42); sd(contrast_32v42)  # 
# median(contrast_33v34); mean(contrast_33v34); sd(contrast_33v34)  # 
# median(contrast_33v43); mean(contrast_33v43); sd(contrast_33v43)  # 
# median(contrast_34v44); mean(contrast_34v44); sd(contrast_34v44)  # 
# 
# median(contrast_41v42); mean(contrast_41v42); sd(contrast_41v42)  # 
# median(contrast_41v43); mean(contrast_41v43); sd(contrast_41v43)  # 
# median(contrast_41v44); mean(contrast_41v44); sd(contrast_41v44)  # 
# median(contrast_42v43); mean(contrast_42v43); sd(contrast_42v43)  # 
# median(contrast_42v44); mean(contrast_42v44); sd(contrast_42v44)  # 
# median(contrast_43v44); mean(contrast_43v44); sd(contrast_43v44)  # 
# 
# ## plot contrasts
# contrasts_long %>%
#   mutate(contrast = paste0(combo1, ' -> ', combo2)) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast))+
#   scale_colour_viridis_d()+
#   labs(colour = 'effect of changing age combination')
# 
# save.image('nearest_neighbour/neighbour_binomial_agecontrasts.RData')
# 
# ## time since stimulus ####
# # load('nearest_neighbour/neighbour_binomial_agecontrasts.RData')
# rm(list = ls()[!ls() %in% c('nbm_fit','nn')]) ; gc()
# 
# ## create new dataframe to predict from
# time_new <- nn %>% 
#   dplyr::select(age_combo, stim_type, prev, bda, 
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(age_combo, prev, bda,
#                                                          focal, stim_num, pb_num))))
# 
# # ## predict with original times
# # time_nn_org <- time_new
# # time_mtx_org <- posterior_epred(object = nbm_fit, newdata = time_nn_org)
# # colnames(time_mtx_org) <- time_nn_org$unique_data_combo
# # time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),]
# 
# ## redo predictions: all before
# nn_before <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'before') %>% 
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, bda, focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# nn_before_mtx <- posterior_epred(object = nbm_fit, newdata = nn_before)
# colnames(nn_before_mtx) <- nn_before$unique_data_combo
# nn_before_mtx <- nn_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
# 
# ## redo predictions: all during
# nn_during <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'during') %>% 
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, bda,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# nn_during_mtx <- posterior_epred(object = nbm_fit, newdata = nn_during)
# colnames(nn_during_mtx) <- nn_during$unique_data_combo
# nn_during_mtx <- nn_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
# 
# ## redo predictions: all after
# nn_after <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'after') %>% 
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, bda,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# nn_after_mtx <- posterior_epred(object = nbm_fit, newdata = nn_after)
# colnames(nn_after_mtx) <- nn_after$unique_data_combo
# nn_after_mtx <- nn_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
# 
# save.image('nearest_neighbour/neighbour_binomial_timecontrasts.RData')
# 
# ## calculate contrasts
# before_vs_during <- nn_during_mtx - nn_before_mtx
# before_vs_after  <- nn_after_mtx  - nn_before_mtx
# during_vs_after  <- nn_after_mtx  - nn_during_mtx
# 
# ## summarise contrasts
# contrasts <- nn %>%
#   select(-stim_type) %>%
#   mutate(before_vs_during_mu = apply(before_vs_during, 2, mean),
#          before_vs_during_sd = apply(before_vs_during, 2, sd),
#          before_vs_after_mu  = apply(before_vs_after,  2, mean),
#          before_vs_after_sd  = apply(before_vs_after,  2, sd),
#          during_vs_after_mu  = apply(during_vs_after,  2, mean),
#          during_vs_after_sd  = apply(during_vs_after,  2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(before_vs_during_mu, before_vs_after_mu, during_vs_after_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','mu'),
#            sep = -3, remove = T) %>%
#   select(-mu, -before_vs_during_sd, -before_vs_after_sd, -during_vs_after_sd)
# 
# ## produce values for reporting
# median(before_vs_during); mean(before_vs_during); sd(before_vs_during) #
# median(before_vs_after) ; mean(before_vs_after) ; sd(before_vs_after)  #
# median(during_vs_after) ; mean(during_vs_after) ; sd(during_vs_after)  #
# 
# ## plot contrasts
# contrasts_long %>%
#   mutate(contrast = ifelse(contrast == 'before_vs_during',
#                            'before -> during',
#                            ifelse(contrast == 'before_vs_after',
#                                   'before -> after', 'during -> after'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast))+
#   scale_colour_viridis_d()+
#   labs(colour = 'effect of changing stimulus')
# 
# save.image('nearest_neighbour/neighbour_binomial_timecontrasts.RData')
# 
# dev.off()
# 
# rm(list = ls()) ; gc()
# print('neighbour completed')
# 
# ######## looking direction ####
# rm(list = ls()[! ls() %in% 'behav']) ; gc()
# set.seed(12345)
# pdf('../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_modelchecks.pdf')
# 
# #### create data ####
# ## select specific data
# look <- behav %>%
#   filter(activity == 'look') %>%
#   select(-activity, -stim_start, -stim_stop) %>%
#   rename(look_index = action_index) %>%
#   mutate(action = ifelse(action_name == 'out_of_sight', 9,
#                          look_index - 1),
#          f_age_num = as.numeric(f_age_num),
#          p_age_num = as.numeric(p_age_num),
#          prev_action = NA,
#          prev_num = NA) %>%
#   filter(!is.na(f_age_num)) %>%
#   filter(!is.na(p_age_num))
# 
# # create variable for looking direction at time t-1
# focals <- unique(look$focal)
# for(f in 1:length(focals)){
#   focal <- look %>% filter(focal == focals[f])
#   look <- look %>% anti_join(focal, by = 'focal')
#   partners <- unique(focal$partner)
#   for(p in 1:length(partners)){
#     focal_partner <- focal %>% filter(partner == partners[p])
#     focal <- focal %>% anti_join(focal_partner, by = 'partner')
#     for(i in 2:nrow(focal_partner)){
#       focal_partner$prev_action[i] <- focal_partner$action_name[i-1]
#       focal_partner$prev_num[i] <- focal_partner$look_index[i-1]
#     }
#     focal <- rbind(focal, focal_partner)
#   }
#   look <- rbind(look, focal)
# }
# rm(focal, focals, focal_partner, f, p, i, partners) ; gc()
# 
# ## remove observations with missing data
# look <- look %>%
#   filter(look_index != 9) %>%
#   filter(prev_num != 9) %>%
#   filter(!is.na(prev_action))
# 
# ## check numbers for current vs previous second
# table(look$action_name, look$prev_action)
# 
# #### set prior ####
# get_prior(formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
#             (1|focal) + (1|stim_num) + (1|pb_num),
#           data = look, family = cumulative("logit"))
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
#   prior(normal(0,1),      class = b,    coef = bdaduring),
#   # action in previous second
#   prior(normal(1,1),      class = b,    coef = moprev_num),
#   prior(dirichlet(2),     class = simo, coef = moprev_num1))
# 
# ## prior predictive check
# num_chains <- 4
# num_iter <- 2000
# lom2_prior <- brm(
#   formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = look, family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(lom2_prior) # huge variation in prior, but fairly on both sides so good
# 
# #### fit model ####
# lom2_fit <- brm(
#   formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = look, family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# save.image('looking_direction/looking_ordinal_2bda_run.RData')
# 
# #### extract draws ####
# # load('looking_direction/looking_ordinal_2bda_run.RData')
# ## check model diagnostics -- looks very good
# (summary <- summary(lom2_fit))
# par(mfrow = c(3,1))
# hist(summary$fixed$Rhat, breaks = 50)
# hist(summary$fixed$Bulk_ESS, breaks = 50)
# hist(summary$fixed$Tail_ESS, breaks = 50)
# par(mfrow = c(1,1))
# 
# ## extract posterior distribution
# draws <- as_draws_df(lom2_fit) %>%
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
# marg <- conditional_effects(lom2_fit,
#                             effects = c('f_age_num','age_combo','stim_type',
#                                         'bda','prev_num'),
#                             categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# agefocal_effect <- marg[[1]]
# agecombo_effect <- marg[[2]]
# stim_effect <- marg[[3]]
# bda_effect <- marg[[4]]
# prev_effect <- marg[[5]]
# 
# ## look at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,2))
# hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
# par(mfrow = c(1,1))
# 
# #### calculate log cumulative odds ####
# (prop <- table(look$look_index) / nrow(look))
# (cum_prop <- cumsum(prop))
# (log_cum_odds <- logit(cum_prop))
# 
# #### plot marginal effects ####
# (f_age_num_plot <- ggplot(agefocal_effect)+
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
#     ylab('probability of looking direction')+
#     scale_colour_viridis_d(name = 'looking direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('look towards',
#                                       'side on',
#                                       'look away'))+
#     scale_fill_viridis_d(name = 'looking direction:',
#                          breaks = c('1','2','3'),
#                          labels = c('look towards',
#                                     'side on',
#                                     'look away'))+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)))
# ggsave(plot = f_age_num_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_marginaleffects_agefocal.png', device = 'png',
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
#     # geom_ribbon(aes(#x = agecombo,
#     #                 x = as.numeric(partner_age),
#     #                 fill = as.factor(cats__),     # looking direction?
#     #                 ymax = upper__, ymin = lower__),
#     #             alpha = 0.4)+
#     # geom_line(aes(#x = agecombo,
#     #               x = as.numeric(partner_age),
#     #               colour = as.factor(cats__),     # looking direction?
#     #               y = estimate__),
#     #           linewidth = 1)+
#     facet_wrap(. ~ f_age_num,
#                labeller = labeller(f_age_num = f_age_num_labels))+
#     ylab('probability of looking direction')+
#     scale_colour_viridis_d(name = 'looking direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('look towards',
#                                       'side on',
#                                       'look away'))+
#     # scale_fill_viridis_d(name = 'looking direction:',
#     #                      breaks = c('1','2','3'),
#     #                      labels = c('look towards',
#     #                                 'side on',
#     #                                 'look away'))+
#     scale_x_discrete(name = 'partner age category')+
#     #scale_x_continuous(name = 'partner age category')+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = agecombo_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_marginaleffects_agepartner.png', device = 'png',
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
# ggsave(plot = stim_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_marginaleffects_stimtype.png',
#        device = 'png', width = 8.3, height = 5.8)
# 
# # (f_age_num_plot + agecombo_plot + stim_plot) +
# #   plot_annotation(tag_levels = 'a')
# # ggsave(plot = last_plot(),
# #        filename = '../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_marginaleffects.png',
# #        device = 'png', width = (5.8*3), height = 8.3)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check ####
# pp_check(lom2_fit, ndraws = 100) # really good fit
# 
# #### plot traces and density curves ####
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
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           "bsp_moprev_num","simo_moprev_num1[1]","simo_moprev_num1[2]"))
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
# ## looking direction in previous second
# prevsec_slope <- draws_cut %>% filter(parameter == 'bsp_moprev_num')
# prevsec2 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[1]')
# prevsec3 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[2]')
# par(mfrow = c(3,1))
# plot(density(prevsec_slope$draw), main = 'slope of prevsec') ; abline(v = 0, lty = 2)
# plot(density(prevsec2$draw), main = 't-1 matched vs younger') ; abline(v = 0, lty = 2)
# plot(density(prevsec3$draw), main = 't-1 older vs younger') ; abline(v = 0, lty = 2)
# 
# ## time since stimulus -- come back to this!
# before <- draws_cut %>% filter(parameter == 'b_bdabefore')
# during <- draws_cut %>% filter(parameter == 'b_bdaduring')
# par(mfrow = c(2,1))
# plot(density(before$draw), main = 'after --> before') ; abline(v = 0, lty = 2)
# plot(density(during$draw), main = 'after --> during') ; abline(v = 0, lty = 2)
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# 
# #### plot raw ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# # ## plot overall
# # ggplot(look,aes(x = f_age_num, y = look_index,
# #                 colour = age_combo))+
# #   geom_jitter(alpha = 0.1)+
# #   facet_wrap(. ~ stim_type,
# #              labeller = labeller(stim_type = stim_labels))+
# #   scale_y_continuous(name = 'focal looking direction relative to target',
# #                      breaks = c(1,2,3),
# #                      labels = c('look directly at','side-on','look directly away'))+
# #   labs(colour = 'age difference')
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
# 
# print(paste0('raw data plotted at ', Sys.time()))
# 
# ## reset plotting
# save.image('looking_direction/looking_ordinal_2bda_run.RData')
# dev.off()
# pdf('../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_modelpredictions.pdf')
# 
# #### predict from model ####
# # load('looking_direction/looking_ordinal_2bda_run.RData')
# rm(list = ls()[! ls() %in% c('lom2_fit','look')]) ; gc()
# 
# pred <- posterior_epred(object = lom2_fit,
#                         newdata = look)
# save.image('looking_direction/looking_ordinal_model2bda_predictions.RData')
# print('looking direction predictions completed')
# 
# ## convert to data frame
# # load('looking_direction/looking_ordinal_model2bda_predictions.RData')
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
# save.image('looking_direction/looking_ordinal_model2bda_predictions.RData')
# rm(pred1, pred2, pred3) ; gc()
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions ####
# load('looking_direction/looking_ordinal_model2bda_predictions.RData')
# 
# ## make labels for looking in previous second
# prevsec_labels <- c('directly away at t-1',
#                     'side-on at t-1',
#                     'directly towards at t-1')
# names(prevsec_labels) <- 1:3
# 
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# (ctd_plot <- pred %>%
#     filter(stim_type == 'ctd') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('look directly away',
#                                                         'side-on',
#                                                         'look at directly')),
#                     colour = factor(pred_type, levels = c('look directly away',
#                                                           'side-on',
#                                                           'look at directly'))
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted looking direction relative to focal:',
#          fill = 'predicted looking direction relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'cape turtle dove (control)')+
#     theme(legend.position = 'bottom'))
# (lion_plot <- pred %>%
#     filter(stim_type == 'l') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('look directly away',
#                                                         'side-on',
#                                                         'look at directly')),
#                     colour = factor(pred_type, levels = c('look directly away',
#                                                           'side-on',
#                                                           'look at directly'))
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted looking direction relative to focal:',
#          fill = 'predicted looking direction relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'lion')+
#     theme(legend.position = 'bottom'))
# (human_plot <- pred %>%
#     filter(stim_type == 'h') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('look directly away',
#                                                         'side-on',
#                                                         'look at directly')),
#                     colour = factor(pred_type, levels = c('look directly away',
#                                                           'side-on',
#                                                           'look at directly'))
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted looking direction relative to focal:',
#          fill = 'predicted looking direction relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# # (ctd_plot + lion_plot + human_plot)+
# #   plot_alookotation(tag_levels = 'a')
# # ggsave(plot = last_plot(), file = '../outputs/looking_ordinal_model_2/looking_ordinal_model2_predictions_violin.png',
# #        device = 'png', height = 8, width = 24)
# 
# ## reset plotting
# dev.off()
# 
# print('looking direction completed')
# 
# #### calculate posterior contrasts from predictions ####
# load('looking_direction/looking_ordinal_model2bda_predictions.RData')
# pdf('../outputs/looking_ordinal_model_2bda/looking_ordinal_model2_modelcontrasts.pdf')
# 
# ## stim type ####
# stim_new <- look %>% 
#   dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda, 
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
#                                                          focal, stim_num, pb_num))))
# 
# ## redo predictions with different stimulus types: all doves
# ctd_look <- stim_new %>%
#   mutate(stim_type = 'ctd')
# ctd_mtx <- posterior_epred(object = lom2_fit, newdata = ctd_look)
# colnames(ctd_mtx) <- ctd_look$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all lions
# lion_look <- stim_new %>%
#   mutate(stim_type = 'l')
# lion_mtx <- posterior_epred(object = lom2_fit, newdata = lion_look)
# colnames(lion_mtx) <- lion_look$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all humans
# human_look <- stim_new %>%
#   mutate(stim_type = 'h')
# human_mtx <- posterior_epred(object = lom2_fit, newdata = human_look)
# colnames(human_mtx) <- human_look$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('looking_direction/looking_ordinal_model2bda_stimuluscontrasts.RData')
# 
# ## focal age ####
# # load('looking_direction/looking_ordinal_model2bda_stimuluscontrasts.RData')
# rm(list = ls()[!ls() %in% c('lom2_fit','look')]) ; gc()
# 
# ## create new dataframe to predict from
# age_new <- look %>% 
#   dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
#                                                          focal, stim_num, pb_num))))
# 
# ## predict with original ages
# age_look_org <- age_new
# age_mtx_org <- posterior_epred(object = lom2_fit, newdata = age_look_org)
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
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%  # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   dplyr::select(f_age_num_original, f_age_num,
#                 age_combo_original, age_combo,
#                 stim_type, prev_num, bda,
#                 focal, stim_num, pb_num,
#                 unique_data_combo)
# age_mtx_alt <- posterior_epred(object = lom2_fit, newdata = age_look_alt)
# colnames(age_mtx_alt) <- age_look_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('looking_direction/looking_ordinal_model2bda_agecontrasts.RData')
# 
# ## time since stimulus ####
# # load('looking_direction/looking_ordinal_model2bda_agecontrasts.RData')
# rm(list = ls()[!ls() %in% c('lom2_fit','look')]) ; gc()
# 
# ## create new dataframe to predict from
# time_new <- look %>% 
#   dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda, 
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
#                                                          focal, stim_num, pb_num))))
# 
# # ## predict with original times
# # time_look_org <- time_new
# # time_mtx_org <- posterior_epred(object = lom2_fit, newdata = time_look_org)
# # colnames(time_mtx_org) <- time_look_org$unique_data_combo
# # time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions: all before
# look_before <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'before') %>% 
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, bda, focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# look_before_mtx <- posterior_epred(object = lom2_fit, newdata = look_before)
# colnames(look_before_mtx) <- look_before$unique_data_combo
# look_before_mtx <- look_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions: all during
# look_during <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'during') %>% 
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, bda,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# look_during_mtx <- posterior_epred(object = lom2_fit, newdata = look_during)
# colnames(look_during_mtx) <- look_during$unique_data_combo
# look_during_mtx <- look_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions: all after
# look_after <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'after') %>% 
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, bda,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# look_after_mtx <- posterior_epred(object = lom2_fit, newdata = look_after)
# colnames(look_after_mtx) <- look_after$unique_data_combo
# look_after_mtx <- look_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('looking_direction/looking_ordinal_2bda_timecontrasts.RData')
# 
# ## calculate contrasts
# before_vs_during <- look_during_mtx - look_before_mtx
# before_vs_after  <- look_after_mtx  - look_before_mtx
# during_vs_after  <- look_after_mtx  - look_during_mtx
# 
# ## summarise contrasts
# contrasts <- look %>%
#   select(-stim_type) %>%
#   mutate(before_vs_during_mu = apply(before_vs_during, 2, mean),
#          before_vs_during_sd = apply(before_vs_during, 2, sd),
#          before_vs_after_mu  = apply(before_vs_after,  2, mean),
#          before_vs_after_sd  = apply(before_vs_after,  2, sd),
#          during_vs_after_mu  = apply(during_vs_after,  2, mean),
#          during_vs_after_sd  = apply(during_vs_after,  2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(before_vs_during_mu, before_vs_after_mu, during_vs_after_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','mu'),
#            sep = -3, remove = T) %>%
#   select(-mu, -before_vs_during_sd, -before_vs_after_sd, -during_vs_after_sd)
# 
# ## produce values for reporting
# median(before_vs_during); mean(before_vs_during); sd(before_vs_during) #
# median(before_vs_after) ; mean(before_vs_after) ; sd(before_vs_after)  #
# median(during_vs_after) ; mean(during_vs_after) ; sd(during_vs_after)  #
# 
# ## plot contrasts
# contrasts_long %>%
#   mutate(contrast = ifelse(contrast == 'before_vs_during',
#                            'before -> during',
#                            ifelse(contrast == 'before_vs_after',
#                                   'before -> after', 'during -> after'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast))+
#   scale_colour_viridis_d()+
#   labs(colour = 'effect of changing stimulus')
# 
# save.image('looking_direction/looking_ordinal_model2bda_timecontrasts.RData')
# dev.off()
# 
# rm(list = ls()) ; gc()
# print('looking direction completed')
# 
# ######## movement direction ####
# rm(list = ls()[! ls() %in% 'behav']) ; gc()
# set.seed(12345)
# pdf('../outputs/movement_ordinal_model_2bda/movement_ordinal_2bda_modelprep.pdf')
# 
# #### create data ####
# ## select specific data
# move <- behav %>%
#   filter(activity == 'move') %>%
#   rename(move_index = action_index) %>%
#   select(-activity, -stim_start, -stim_stop) %>%
#   mutate(prev_action = NA,
#          prev_num = NA) %>%
#   filter(!is.na(f_age_num)) %>%
#   filter(!is.na(p_age_num)) %>%
#   mutate(f_age_num = as.integer(f_age_num),
#          p_age_num = as.integer(p_age_num))
# 
# # create variable for movement direction at time t-1
# focals <- unique(move$focal)
# for(f in 1:length(focals)){
#   focal <- move %>% filter(focal == focals[f])
#   move <- move %>% anti_join(focal, by = 'focal')
#   partners <- unique(focal$partner)
#   for(p in 1:length(partners)){
#     focal_partner <- focal %>% filter(partner == partners[p])
#     focal <- focal %>% anti_join(focal_partner, by = 'partner')
#     for(i in 2:nrow(focal_partner)){
#       focal_partner$prev_action[i] <- focal_partner$action_name[i-1]
#       focal_partner$prev_num[i] <- focal_partner$move_index[i-1]
#     }
#     focal <- rbind(focal, focal_partner)
#   }
#   move <- rbind(move, focal)
# }
# rm(focal, focals, focal_partner, f, p, i, partners) ; gc()
# 
# ## remove observations where not moving currently or in previous second
# move <- move %>%
#   filter(action_name != 'not_moving') %>%
#   filter(prev_action != 'not_moving')
# 
# ## remove observations with missing data
# move <- move %>%
#   filter(action_name != 'out_of_sight') %>%
#   filter(prev_action != 'out_of_sight') %>%
#   filter(!is.na(prev_action))
# 
# ## check numbers for current vs previous second
# table(move$action_name, move$prev_action)
# 
# #### set prior ####
# get_prior(formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
#             (1|focal) + (1|stim_num) + (1|pb_num),
#           data = move, family = cumulative("logit"))
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
#   prior(normal(0,1),      class = b,    coef = bdaduring),
#   # action in previous second
#   prior(normal(1,1),      class = b,    coef = moprev_num),
#   prior(dirichlet(2),     class = simo, coef = moprev_num1))
# 
# ## prior predictive check
# num_chains <- 4
# num_iter <- 2000
# mom2_prior <- brm(
#   formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = move, family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(mom2_prior) # huge variation in prior, but fairly on both sides so good
# 
# ## reset plotting
# dev.off()
# pdf('../outputs/movement_ordinal_model_2bda/movement_ordinal_2bda_modelchecks.pdf')
# 
# #### fit model ####
# mom2_fit <- brm(
#   formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = move, family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# save.image('movement_direction/moving_ordinal_2bda_run.RData')
# 
# #### extract draws ####
# load('movement_direction/moving_ordinal_2bda_run.RData')
# ## check model diagnostics -- moves very good
# (summary <- summary(mom2_fit))
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
#                                         'bda','prev_num'),
#                             categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# agefocal_effect <- marg[[1]]
# agecombo_effect <- marg[[2]]
# stim_effect <- marg[[3]]
# bda_effect <- marg[[4]]
# prev_effect <- marg[[5]]
# 
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,2))
# hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
# par(mfrow = c(1,1))
# 
# #### calculate log cumulative odds ####
# (prop <- table(move$move_index) / nrow(move))
# (cum_prop <- cumsum(prop))
# (log_cum_odds <- logit(cum_prop))
# 
# #### plot marginal effects ####
# (f_age_num_plot <- ggplot(agefocal_effect)+
#    # geom_ribbon(aes(x = f_age_num,
#    #                 ymax = upper__, ymin = lower__,
#    #                 fill = cats__),
#    #             alpha = 0.4)+
#    # geom_line(aes(x = f_age_num,
#    #               y = estimate__,
#    #               colour = cats__),
#    #           linewidth = 1)+
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
# ggsave(plot = f_age_num_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_marginaleffects_agefocal.png', device = 'png',
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
#       #shape = f_age_num,
#       y = estimate__),
#       size = 3)+
#     # geom_ribbon(aes(#x = agecombo,
#     #                 x = as.numeric(partner_age),
#     #                 fill = as.factor(cats__),     # moving direction?
#     #                 ymax = upper__, ymin = lower__),
#     #             alpha = 0.4)+
#     # geom_line(aes(#x = agecombo,
#     #               x = as.numeric(partner_age),
#     #               colour = as.factor(cats__),     # moving direction?
#     #               y = estimate__),
#     #           linewidth = 1)+
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
#     # scale_fill_viridis_d(name = 'moving direction:',
#     #                      breaks = c('1','2','3','4','5'),
#     #                      labels = c('move away directly',
#     #                                 'move away at an angle',
#     #                                 'move neither towards or away',
#     #                                 'approach at an angle',
#     #                                 'approach directly'))+
#     scale_x_discrete(name = 'partner age category')+
#     #scale_x_continuous(name = 'partner age category')+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = agecombo_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_marginaleffects_agepartner.png', device = 'png',
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
# ggsave(plot = stim_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_marginaleffects_stimtype.png',
#        device = 'png', width = 8.3, height = 5.8)
# 
# # (f_age_num_plot + agecombo_plot + stim_plot) +
# #   plot_annotation(tag_levels = 'a')
# # ggsave(plot = last_plot(),
# #        filename = '../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_marginaleffects.png',
# #        device = 'png', width = (5.8*3), height = 8.3)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check ####
# pp_check(mom2_fit, ndraws = 100) # really good fit
# 
# #### plot traces and density curves ####
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
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           "bsp_moprev_num","simo_moprev_num1[1]","simo_moprev_num1[2]"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mixing generally moves very good, though a couple of age combo ones are a touch wandery
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
# ## moving direction in previous second
# prevsec_slope <- draws_cut %>% filter(parameter == 'bsp_moprev_num')
# prevsec2 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[1]')
# prevsec3 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[2]')
# par(mfrow = c(3,1))
# plot(density(prevsec_slope$draw), main = 'slope of prevsec') ; abline(v = 0, lty = 2)
# plot(density(prevsec2$draw), main = 't-1 matched vs younger') ; abline(v = 0, lty = 2)
# plot(density(prevsec3$draw), main = 't-1 older vs younger') ; abline(v = 0, lty = 2)
# 
# ## time since stimulus -- come back to this!
# before <- draws_cut %>% filter(parameter == 'b_bdabefore')
# during <- draws_cut %>% filter(parameter == 'b_bdaduring')
# par(mfrow = c(2,1))
# plot(density(before$draw), main = 'after --> before') ; abline(v = 0, lty = 2)
# plot(density(during$draw), main = 'after --> during') ; abline(v = 0, lty = 2)
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# 
# #### plot raw ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# # ## plot overall
# # ggplot(move,aes(x = f_age_num, y = move_index,
# #                 colour = age_combo))+
# #   geom_jitter(alpha = 0.1)+
# #   facet_wrap(. ~ stim_type,
# #              labeller = labeller(stim_type = stim_labels))+
# #   scale_y_continuous(name = 'focal moving direction relative to target',
# #                      breaks = c(1,2,3),
# #                      labels = c('move directly at','side-on','move directly away'))+
# #   labs(colour = 'age difference')
# 
# ## plot control data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')
# 
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 
# ## plot lion data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')
# 
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 
# ## plot human data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')
# 
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 
# print(paste0('raw data plotted at ',Sys.time()))
# 
# ## reset plotting
# save.image('movement_direction/moving_ordinal_2bda_run.RData')
# dev.off()
# pdf('../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_modelpredictions.pdf')
# 
# #### predict from model ####
# load('movement_direction/moving_ordinal_2bda_run.RData')
# rm(list = ls()[! ls() %in% c('mom2_fit','move')]) ;
# 
# pred <- posterior_epred(object = mom2_fit,
#                           newdata = move)
# save.image('movement_direction/moving_ordinal_2bda_modelpredictions.RData')
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
# pred <- rbind(pred1, pred2, pred3, pred4, pred5)
# save.image('movement_direction/moving_ordinal_2bda_modelpredictions.RData')
# rm(pred1, pred2, pred3, pred4, pred5) ; gc()
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions ####
# load('movement_direction/moving_ordinal_2bda_modelpredictions.RData')
# 
# ## make labels for movement in previous second
# prevsec_labels <- c('directly away at t-1',
#                     'angle away at t-1',
#                     'neither at t-1',
#                     'angle approach at t-1',
#                     'directly approach at t-1')
# names(prevsec_labels) <- 1:5
# 
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# (ctd_plot <- pred %>%
#     filter(stim_type == 'ctd') %>%
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
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'cape turtle dove (control)')+
#     theme(legend.position = 'bottom'))
# (lion_plot <- pred %>%
#     filter(stim_type == 'l') %>%
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
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'lion')+
#     theme(legend.position = 'bottom'))
# (human_plot <- pred %>%
#     filter(stim_type == 'h') %>%
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
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# # (ctd_plot + lion_plot + human_plot)+
# #   plot_amoveotation(tag_levels = 'a')
# # ggsave(plot = last_plot(), file = '../outputs/movement_ordinal_model_2/movement_ordinal_model2_predictions_violin.png',
# #        device = 'png', height = 8, width = 24)
# 
# ## reset plotting
# dev.off()
# 
#### calculate posterior contrasts from predictions ####
load('movement_direction/moving_ordinal_2bda_modelpredictions.RData')
pdf('../outputs/movement_ordinal_model_2bda/movement_ordinal_model2_modelcontrasts.pdf')

## stim type ####
# stim_new <- move %>% 
#   dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda, 
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
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
# save.image('movement_direction/moving_ordinal_2bda_stimuluscontrasts.RData')

load('movement_direction/moving_ordinal_2bda_stimuluscontrasts.RData')

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
median(ctd_vs_lion)  ; mean(ctd_vs_lion)  ; sd(ctd_vs_lion)
#
median(ctd_vs_human) ; mean(ctd_vs_human) ; sd(ctd_vs_human)
#
median(lion_vs_human); mean(lion_vs_human); sd(lion_vs_human)
#

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

save.image('nearest_neighbour/neighbour_binomial_stimuluscontrasts.RData')

## focal age ####
# # load('movement_direction/moving_ordinal_2bda_stimuluscontrasts.RData')
# rm(list = ls()[!ls() %in% c('mom2_fit','move')]) ; gc()
# 
# ## create new dataframe to predict from
# age_new <- move %>% 
#   dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda, 
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
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
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%  # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   dplyr::select(f_age_num_original, f_age_num,
#                 age_combo_original, age_combo,
#                 stim_type, prev_num, bda,
#                 focal, stim_num, pb_num,
#                 unique_data_combo)
# age_mtx_alt <- posterior_epred(object = mom2_fit, newdata = age_move_alt)
# colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('movement_direction/moving_ordinal_2bda_agecontrasts.RData')

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
#             ±     
mean(alt_vs_org_awayangle) ; sd(alt_vs_org_awayangle)
#             ±     
mean(alt_vs_org_neither)   ; sd(alt_vs_org_neither)
#             ±     
mean(alt_vs_org_twdsangle) ; sd(alt_vs_org_twdsangle)
#             ±      
mean(alt_vs_org_twdsdirect); sd(alt_vs_org_twdsdirect)
#             ±     

## repeat excluding age category 4 because different contrast
mean(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     # 
sd(  alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     # 
mean(alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])      # 
sd(  alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])      # 
mean(alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])        # 
sd(  alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])        # 
mean(alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])      # 
sd(  alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])      # 
mean(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # 
sd(  alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # 

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
mean(age1v2_ad) ; sd(age1v2_ad) #  ± 
mean(age2v3_ad) ; sd(age2v3_ad) #  ± 
mean(age3v4_ad) ; sd(age3v4_ad) #  ± 
mean(age1v4_ad) ; sd(age1v4_ad) #  ± 
mean(age1v2_aa) ; sd(age1v2_aa) #  ± 
mean(age2v3_aa) ; sd(age2v3_aa) #  ± 
mean(age3v4_aa) ; sd(age3v4_aa) #  ± 
mean(age1v4_aa) ; sd(age1v4_aa) #  ± 
mean(age1v2_n)  ; sd(age1v2_n)  #  ± 
mean(age2v3_n)  ; sd(age2v3_n)  #  ± 
mean(age3v4_n)  ; sd(age3v4_n)  #  ± 
mean(age1v4_n)  ; sd(age1v4_n)  #  ± 
mean(age1v2_ta) ; sd(age1v2_ta) #  ± 
mean(age2v3_ta) ; sd(age2v3_ta) #  ± 
mean(age3v4_ta) ; sd(age3v4_ta) #  ± 
mean(age1v4_ta) ; sd(age1v4_ta) #  ± 
mean(age1v2_td) ; sd(age1v2_td) #  ± 
mean(age2v3_td) ; sd(age2v3_td) #  ± 
mean(age3v4_td) ; sd(age3v4_td) #  ± 
mean(age1v4_td) ; sd(age1v4_td) #  ± 

## summarise contrasts
contrasts <- move_no_na %>%
  mutate(alt_vs_org_awaydirect_mu = apply(alt_vs_org_awaydirect, 2, mean),
         alt_vs_org_awaydirect_sd = apply(alt_vs_org_awaydirect, 2, sd),
         alt_vs_org_awayangle_mu  = apply(alt_vs_org_awayangle,  2, mean),
         alt_vs_org_awayangle_sd  = apply(alt_vs_org_awayangle,  2, sd),
         alt_vs_org_neither_mu    = apply(alt_vs_org_neither,    2, mean),
         alt_vs_org_neither_sd    = apply(alt_vs_org_neither,    2, sd),
         alt_vs_org_twdsangle_mu  = apply(alt_vs_org_twdsangle,  2, mean),
         alt_vs_org_twdsangle_sd  = apply(alt_vs_org_twdsangle,  2, sd),
         alt_vs_org_twdsdirect_mu = apply(alt_vs_org_twdsdirect, 2, mean),
         alt_vs_org_twdsdirect_sd = apply(alt_vs_org_twdsdirect, 2, sd)
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
# load('movement_direction/moving_ordinal_2bda_agecontrasts.RData')
rm(list = ls()[!ls() %in% c('mom2_fit','move')]) ; gc()

## create new dataframe to predict from
time_new <- move %>% 
  dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda, 
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
                                                         focal, stim_num, pb_num))))

# ## predict with original times
# time_move_org <- time_new
# time_mtx_org <- posterior_epred(object = mom2_fit, newdata = time_move_org)
# colnames(time_mtx_org) <- time_move_org$unique_data_combo
# time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions: all before
move_before <- time_new %>%
  mutate(bda_org = bda) %>%
  mutate(bda = 'before') %>% 
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, bda, focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(bda_org)
move_before_mtx <- posterior_epred(object = mom2_fit, newdata = move_before)
colnames(move_before_mtx) <- move_before$unique_data_combo
move_before_mtx <- move_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions: all during
move_during <- time_new %>%
  mutate(bda_org = bda) %>%
  mutate(bda = 'during') %>% 
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, bda,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(bda_org)
move_during_mtx <- posterior_epred(object = mom2_fit, newdata = move_during)
colnames(move_during_mtx) <- move_during$unique_data_combo
move_during_mtx <- move_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions: all after
move_after <- time_new %>%
  mutate(bda_org = bda) %>%
  mutate(bda = 'after') %>% 
  #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, bda,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
  relocate(bda_org)
move_after_mtx <- posterior_epred(object = lom2_fit, newdata = move_after)
colnames(move_after_mtx) <- move_after$unique_data_combo
move_after_mtx <- move_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

save.image('movement_direction/moving_ordinal_2bda_timecontrasts.RData')

## calculate contrasts
before_vs_during <- move_during_mtx - move_before_mtx
before_vs_after  <- move_after_mtx  - move_before_mtx
during_vs_after  <- move_after_mtx  - move_during_mtx

## summarise contrasts
contrasts <- nn %>%
  select(-stim_type) %>%
  mutate(before_vs_during_mu = apply(before_vs_during, 2, mean),
         before_vs_during_sd = apply(before_vs_during, 2, sd),
         before_vs_after_mu  = apply(before_vs_after,  2, mean),
         before_vs_after_sd  = apply(before_vs_after,  2, sd),
         during_vs_after_mu  = apply(during_vs_after,  2, mean),
         during_vs_after_sd  = apply(during_vs_after,  2, sd))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(before_vs_during_mu, before_vs_after_mu, during_vs_after_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('contrast','mu'),
           sep = -3, remove = T) %>%
  select(-mu, -before_vs_during_sd, -before_vs_after_sd, -during_vs_after_sd)

## produce values for reporting
median(before_vs_during); mean(before_vs_during); sd(before_vs_during) #
median(before_vs_after) ; mean(before_vs_after) ; sd(before_vs_after)  #
median(during_vs_after) ; mean(during_vs_after) ; sd(during_vs_after)  #

## plot contrasts
contrasts_long %>%
  mutate(contrast = ifelse(contrast == 'before_vs_during',
                           'before -> during',
                           ifelse(contrast == 'before_vs_after',
                                  'before -> after', 'during -> after'))) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = contrast))+
  scale_colour_viridis_d()+
  labs(colour = 'effect of changing stimulus')

save.image('movement_direction/moving_ordinal_2bda_timecontrasts.RData')
dev.off()

rm(list = ls()) ; gc()
print('movement direction completed')
