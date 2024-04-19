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

######## import data about playbacks ####
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
  filter(out_frame_name == 'in_frame') %>%
  select(subject,pb_num,second,
         all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
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
  rename(elephant_activity = elephant_activity_name) %>%
  filter(is.na(action_index) == FALSE) %>%
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

## remove individuals where ages are unknown
behav <- behav %>%
  filter(!is.na(f_age_num))

# ######## nearest neighbour ####
# pdf('../outputs/neighbour_binomial_model_bda/neighbour_binomial_modelchecks.pdf')
# 
# #### create data ####
# ## select specific data
# nn <- behav %>%
#   filter(activity == 'nn') %>%
#   select(-activity, -stim_start, -stim_stop) %>%
#   mutate(prev = NA,
#          action = as.numeric(action_name),
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
# ## remove observations where nearest neighbour in previous second was unknown
# nn <- nn %>%
#   filter(!is.na(prev))
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
pdf('../outputs/neighbour_binomial_model_bda/neighbour_binomial_modelpredictions.pdf')
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
#### plot predictions ####
load('nearest_neighbour/neighbour_binomial_predictions.RData')

## make labels for neighbour in previous second
prevsec_labels <- c('not neighbour at t-1',
                    'neighbour at t-1')
names(prevsec_labels) <- c(0,1)

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
(ctd_plot <- predictions %>%
    filter(stim_type == 'ctd') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    # fill = factor(pred_type, levels = c('not neighbour', 'neighbour')),
                    # colour = factor(pred_type, levels = c('not neighbour', 'neighbour'))
                    colour = bda
                    )) +
    facet_grid(prev ~ .,
               labeller = labeller(prev = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of neighbour relative to focal:',
         fill = 'predicted direction of neighbour relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'cape turtle dove (control)')+
    theme(legend.position = 'bottom'))
(lion_plot <- predictions %>%
    filter(stim_type == 'l') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    # fill = factor(pred_type, levels = c('not neighbour', 'neighbour')),
                    # colour = factor(pred_type, levels = c('not neighbour', 'neighbour'))
                    colour = bda
                    )) +
    facet_grid(prev ~ .,
               labeller = labeller(prev = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of neighbour relative to focal:',
         fill = 'predicted direction of neighbour relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'lion')+
    theme(legend.position = 'bottom'))
(human_plot <- predictions %>%
    filter(stim_type == 'h') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = epred,
                    # fill = factor(pred_type, levels = c('not neighbour', 'neighbour')),
                    # colour = factor(pred_type, levels = c('not neighbour', 'neighbour'))
                    colour = bda
                    )) +
    facet_grid(prev ~ .,
               labeller = labeller(prev = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of neighbour relative to focal:',
         fill = 'predicted direction of neighbour relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'human')+
    theme(legend.position = 'bottom'))
(ctd_plot + lion_plot + human_plot)+
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(), file = '../outputs/neighbour_binomial_model/neighbour_binomial_predictions_violin.png',
       device = 'png', height = 8, width = 48)

## reset plotting
dev.off()
#pdf('../outputs/neighbour_binomial_model/neighbour_binomial_modelcontrasts.pdf')

rm(list = ls()) ; gc()

# ######## looking direction ####
# rm(list = ls()[! ls() %in% 'behav']) ; gc()
# pdf('../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_modelchecks.pdf')
# 
# #### create data ####
# ## select specific data
# look <- behav %>%
#   filter(activity == 'look') %>%
#   select(-activity, -stim_start, -stim_stop) %>%
#   rename(action = action_name,
#          look_index = action_index) %>% 
#   mutate(prev_action = NA,
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
#       focal_partner$prev_action[i] <- focal_partner$action[i-1]
#       focal_partner$prev_num[i] <- focal_partner$look_index[i-1]
#     }
#     focal <- rbind(focal, focal_partner)
#   }
#   look <- rbind(look, focal)
# }
# rm(focal, focals, focal_partner, f, p, i, partners) ; gc()
# 
# ## remove observations where look in previous second was unknown
# look <- look %>%
#   filter(!is.na(prev_action)) %>%
#   mutate(f_age_num = as.integer(f_age_num))
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
# print(paste0('raw data plotted at ',Sys.time()))
# 
# ## reset plotting
# save.image('looking_direction/looking_ordinal_2bda_run.RData') # save.image('ele_playbacks/looking_direction/looking_ordinal_2bda_run.RData')
# dev.off()
# #pdf('../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_modelpredictions.pdf')
# 
#### predict from model ####
# load('ele_playbacks/looking_direction/looking_ordinal_model2_run.RData') # load('looking_direction/looking_ordinal_model2_run.RData')
rm(list = ls()[! ls() %in% c('lom2_fit','look_no_na')]) ; gc()

pred <- posterior_epred(object = lom2_fit,
                        newdata = look_no_na)
save.image('ele_playbacks/looking_direction/looking_ordinal_model2_predictions.RData')

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
save.image('looking_direction/looking_ordinal_model2_predictions.RData')
rm(pred1, pred2, pred3) ; gc()

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions ####
#load('looking_direction/looking_ordinal_model2_predictions.RData')  #load('ele_playbacks/looking_direction/looking_ordinal_model2_predictions.RData')

## make labels for looking in previous second
prevsec_labels <- c('directly away at t-1',
                    'side-on at t-1',
                    'directly towards at t-1')
names(prevsec_labels) <- 1:5

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
(ctd_plot <- pred %>%
    filter(stim_type == 'ctd') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ bda,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'cape turtle dove (control)')+
    theme(legend.position = 'bottom'))
(lion_plot <- predictions_all %>%
    filter(stim_type == 'l') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ bda,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'lion')+
    theme(legend.position = 'bottom'))
(human_plot <- predictions_all %>%
    filter(stim_type == 'h') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ bda,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'human')+
    theme(legend.position = 'bottom'))
(ctd_plot + lion_plot + human_plot)+
  plot_alookotation(tag_levels = 'a')
ggsave(plot = last_plot(), file = '../outputs/looking_ordinal_model_2/looking_ordinal_model2_predictions_violin.png',
       device = 'png', height = 8, width = 24)

## reset plotting
dev.off()
# pdf('../outputs/looking_ordinal_model_2/looking_ordinal_model2_modelcontrasts.pdf')

# ######## movement direction ####
# rm(list = ls()[! ls() %in% 'behav']) ; gc()
# pdf('../outputs/movement_ordinal_model_2bda/movement_ordinal_2bda_modelprep.pdf')
# 
# #### create data ####
# ## select specific data
# move <- behav %>%
#   filter(activity == 'move') %>%
#   rename(action = action_name,
#          move_index = action_index) %>%
#   select(-activity, -stim_start, -stim_stop) %>%
#   mutate(prev_action = NA,
#          prev_num = NA) %>%
#   filter(!is.na(f_age_num)) %>%
#   filter(!is.na(p_age_num))
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
#       focal_partner$prev_action[i] <- focal_partner$action[i-1]
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
#   filter(action != 'not_moving') %>%
#   filter(prev_action != 'not_moving') %>%
#   filter(!is.na(prev_action)) %>%
#   mutate(f_age_num = as.integer(f_age_num))
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
# #pdf('../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_modelpredictions.pdf')
# 
# 
#### predict from model ####
load('movement_direction/moving_ordinal_2bda_run.RData')
rm(list = ls()[! ls() %in% c('mom2_fit','move')]) ;

pred <- posterior_epred(object = mom2_fit,
                          newdata = move)
save.image('movement_direction/moving_ordinal_2bda_modelpredictions.RData')

## convert to data frame
move_no_na$data_row <- 1:nrow(move_no_na)
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
#load('movement_direction/movement_ordinal_model2_predictions.RData')  #load('ele_playbacks/movement_direction/movement_ordinal_model2_predictions.RData')

## make labels for movement in previous second
prevsec_labels <- c('directly away at t-1',
                    'angle away at t-1',
                    'neither at t-1',
                    'angle approach at t-1',
                    'directly approach at t-1')
names(prevsec_labels) <- 1:5

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
(ctd_plot <- pred %>%
    filter(stim_type == 'ctd') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
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
    facet_grid(move_tminus1_num ~ bda,
               labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'cape turtle dove (control)')+
    theme(legend.position = 'bottom'))
(lion_plot <- predictions_all %>%
    filter(stim_type == 'l') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
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
    facet_grid(move_tminus1_num ~ bda,
               labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'lion')+
    theme(legend.position = 'bottom'))
(human_plot <- predictions_all %>%
    filter(stim_type == 'h') %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
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
    facet_grid(move_tminus1_num ~ bda,
               labeller = labeller(move_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted direction of movement relative to focal:',
         fill = 'predicted direction of movement relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'human')+
    theme(legend.position = 'bottom'))
(ctd_plot + lion_plot + human_plot)+
  plot_amoveotation(tag_levels = 'a')
ggsave(plot = last_plot(), file = '../outputs/movement_ordinal_model_2/movement_ordinal_model2_predictions_violin.png',
       device = 'png', height = 8, width = 24)

## reset plotting
dev.off()
# pdf('../outputs/movement_ordinal_model_2/movement_ordinal_model2_modelcontrasts.pdf')

