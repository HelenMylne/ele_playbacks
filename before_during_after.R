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
# save.image('nearest_neighbour/neighbour_model_run_bda.RData')
# 
# ## reset plotting
# dev.off()
# #pdf('../outputs/neighbour_binomial_model_bda/neighbour_binomial_modelpredictions.pdf')
# 
# # #### predict from model ####
# # rm(list = ls()[! ls() %in% c('nbm_fit','nn')])
# #
# # pred <- posterior_predict(object = nbm_fit,
# #                           newdata = nn)
# # save.image('nearest_neighbour/neighbour_model_predictions_bda.RData')
# #
######## looking direction ####
rm(list = ls()[! ls() %in% 'behav']) ; gc()
pdf('../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_modelchecks.pdf')

#### create data ####
## select specific data
look <- behav %>%
  filter(activity == 'look') %>%
  select(-activity, -stim_start, -stim_stop) %>%
  rename(action = action_name,
         look_index = action_index) %>% 
  mutate(prev_action = NA,
         prev_num = NA) %>%
  filter(!is.na(f_age_num)) %>%
  filter(!is.na(p_age_num))

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
      focal_partner$prev_action[i] <- focal_partner$action[i-1]
      focal_partner$prev_num[i] <- focal_partner$look_index[i-1]
    }
    focal <- rbind(focal, focal_partner)
  }
  look <- rbind(look, focal)
}
rm(focal, focals, focal_partner, f, p, i, partners) ; gc()

## remove observations where look in previous second was unknown
look <- look %>%
  filter(!is.na(prev_action)) %>%
  mutate(f_age_num = as.integer(f_age_num))

#### set prior ####
get_prior(formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
            (1|focal) + (1|stim_num) + (1|pb_num),
          data = look, family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # interaction
  prior(normal(0,1),      class = b,    coef = age_combo1_2),
  prior(normal(0,1),      class = b,    coef = age_combo1_3),
  prior(normal(0,1),      class = b,    coef = age_combo1_4),
  prior(normal(0,1),      class = b,    coef = age_combo2_1),
  prior(normal(0,1),      class = b,    coef = age_combo2_2),
  prior(normal(0,1),      class = b,    coef = age_combo2_3),
  prior(normal(0,1),      class = b,    coef = age_combo2_4),
  prior(normal(0,1),      class = b,    coef = age_combo3_1),
  prior(normal(0,1),      class = b,    coef = age_combo3_2),
  prior(normal(0,1),      class = b,    coef = age_combo3_3),
  prior(normal(0,1),      class = b,    coef = age_combo3_4),
  prior(normal(0,1),      class = b,    coef = age_combo4_1),
  prior(normal(0,1),      class = b,    coef = age_combo4_2),
  prior(normal(0,1),      class = b,    coef = age_combo4_3),
  prior(normal(0,1),      class = b,    coef = age_combo4_4),
  # stim type
  prior(normal(0,1),      class = b,    coef = stim_typeh),
  prior(normal(0,1),      class = b,    coef = stim_typel),
  # before/during/after
  prior(normal(0,1),      class = b,    coef = bdabefore),
  prior(normal(0,1),      class = b,    coef = bdaduring),
  # action in previous second
  prior(normal(1,1),      class = b,    coef = moprev_num),
  prior(dirichlet(2),     class = simo, coef = moprev_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
lom2_prior <- brm(
  formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = look, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(lom2_prior) # huge variation in prior, but fairly on both sides so good

#### fit model ####
lom2_fit <- brm(
  formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = look, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)
save.image('looking_direction/looking_ordinal_2bda_run.RData')

#### extract draws ####
# load('looking_direction/looking_ordinal_2bda_run.RData')
## check model diagnostics -- looks very good
(summary <- summary(lom2_fit))
par(mfrow = c(3,1))
hist(summary$fixed$Rhat, breaks = 50)
hist(summary$fixed$Bulk_ESS, breaks = 50)
hist(summary$fixed$Tail_ESS, breaks = 50)
par(mfrow = c(1,1))

## extract posterior distribution
draws <- as_draws_df(lom2_fit) %>%
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

## extract marginal effects
marg <- conditional_effects(lom2_fit,
                            effects = c('f_age_num','age_combo','stim_type',
                                        'bda','prev_num'),
                            categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
agefocal_effect <- marg[[1]]
agecombo_effect <- marg[[2]]
stim_effect <- marg[[3]]
bda_effect <- marg[[4]]
prev_effect <- marg[[5]]

## look at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

#### calculate log cumulative odds ####
(prop <- table(look$look_index) / nrow(look))
(cum_prop <- cumsum(prop))
(log_cum_odds <- logit(cum_prop))

#### plot marginal effects ####
(f_age_num_plot <- ggplot(agefocal_effect)+
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
ggsave(plot = f_age_num_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_marginaleffects_agefocal.png', device = 'png',
       width = 8.3, height = 5.8)

f_age_num_labels <- c('focal age category 1',
                      'focal age category 2',
                      'focal age category 3',
                      'focal age category 4')
names(f_age_num_labels) <- 1:4
(agecombo_plot <- agecombo_effect %>%
    separate(col = age_combo, sep = '_', remove = F,
             into = c('f_age_num','partner_age')) %>%
    mutate(agecombo = paste0(f_age_num,'-',partner_age)) %>%
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
      #shape = f_age_num,
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
    facet_wrap(. ~ f_age_num,
               labeller = labeller(f_age_num = f_age_num_labels))+
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
ggsave(plot = agecombo_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_marginaleffects_agepartner.png', device = 'png',
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
ggsave(plot = stim_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_marginaleffects_stimtype.png',
       device = 'png', width = 8.3, height = 5.8)

# (f_age_num_plot + agecombo_plot + stim_plot) +
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(),
#        filename = '../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_marginaleffects.png',
#        device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(lom2_fit, ndraws = 100) # really good fit

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "b_bdabefore","b_bdaduring",
                          "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
                          "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
                          "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
                          "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "bsp_moprev_num","simo_moprev_num1[1]","simo_moprev_num1[2]"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing generally looks very good, though a couple of age combo ones are a touch wandery

## move at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,1))
hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')

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
prevsec_slope <- draws_cut %>% filter(parameter == 'bsp_moprev_num')
prevsec2 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[1]')
prevsec3 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[2]')
par(mfrow = c(3,1))
plot(density(prevsec_slope$draw), main = 'slope of prevsec') ; abline(v = 0, lty = 2)
plot(density(prevsec2$draw), main = 't-1 matched vs younger') ; abline(v = 0, lty = 2)
plot(density(prevsec3$draw), main = 't-1 older vs younger') ; abline(v = 0, lty = 2)

## time since stimulus -- come back to this!
before <- draws_cut %>% filter(parameter == 'b_bdabefore')
during <- draws_cut %>% filter(parameter == 'b_bdaduring')
par(mfrow = c(2,1))
plot(density(before$draw), main = 'after --> before') ; abline(v = 0, lty = 2)
plot(density(during$draw), main = 'after --> during') ; abline(v = 0, lty = 2)

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

# ## plot overall
# ggplot(look,aes(x = f_age_num, y = look_index,
#                 colour = age_combo))+
#   geom_jitter(alpha = 0.1)+
#   facet_wrap(. ~ stim_type,
#              labeller = labeller(stim_type = stim_labels))+
#   scale_y_continuous(name = 'focal looking direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('look directly at','side-on','look directly away'))+
#   labs(colour = 'age difference')

## plot control data
look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = bda, y = look_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  scale_x_discrete(name = 'time relative to stimulus')

look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = look_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## plot lion data
look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'l') %>%
  ggplot(aes(x = bda, y = look_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  scale_x_discrete(name = 'time relative to stimulus')

look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'l') %>%
  ggplot(aes(x = look_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## plot human data
look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'h') %>%
  ggplot(aes(x = bda, y = look_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  scale_x_discrete(name = 'time relative to stimulus')

look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'h') %>%
  ggplot(aes(x = look_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('looking_direction/looking_ordinal_2bda_run.RData') # save.image('ele_playbacks/looking_direction/looking_ordinal_2bda_run.RData')
dev.off()
#pdf('../outputs/looking_ordinal_model_2bda/looking_ordinal_2bda_modelpredictions.pdf')

#### predict from model -- raw data -- NOT ACTUALLY RUN AND CHECKED YET ####
# load('looking_direction/look_model_run_bda.RData')
rm(list = ls()[ ! ls() %in% c('behav','draws','look','lom2_fit','priors','summary','num_chains','num_iter','age_labels','stim_labels') ]) ; gc()

## predict from raw data
pred_mtx <- posterior_epred(object = lom2_fit, newdata = look)
look$unique_data_combo <- 1:nrow(look)
colnames(pred_mtx) <- look$unique_data_combo
save.image('looking_direction/looking_ordinal_2bda_predictions.RData')

## convert predictions to long format data set, using only first 100 values per chain
#load('looking_direction/looking_ordinal_2bda_predictions.RData')
predictions1 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,1] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
  mutate(unique_data_combo = as.integer(unique_data_combo),
         pred_type = 1) %>%
  left_join(look, by = 'unique_data_combo')
predictions2 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,2] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
  mutate(unique_data_combo = as.integer(unique_data_combo),
         pred_type = 2) %>%
  left_join(look, by = 'unique_data_combo')
predictions3 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,3] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
  mutate(unique_data_combo = as.integer(unique_data_combo),
         pred_type = 3) %>%
  left_join(look, by = 'unique_data_combo')
save.image('looking_direction/looking_ordinal_2bda_predictions.RData')

predictions_all <- rbind(predictions1, predictions2, predictions3)
save.image('looking_direction/looking_ordinal_2bda_predictions.RData')

# ## plot predictions -- posterior_epred() -- nn ####
# #load('looking_direction/looking_ordinal_2bda_predictions.RData')  #load('ele_playbacks/looking_direction/looking_ordinal_2bda_predictions.RData')
# rm(pred_mtx, predictions1, predictions2, predictions3) ; gc()
#
# ## make labels for prediction type look nice
# predictions_all <- predictions_all %>%
#   mutate(pred_label = ifelse(pred_type == 1, 'younger',
#                              ifelse(pred_type == 2, 'age matched', 'older')))
#
# ## make labels for age of neighbour in previous second look nice
# prevsec_labels <- c('neighbour younger at t-1',
#                     'neighbour same age at t-1',
#                     'neighbour older at t-1')
# names(prevsec_labels) <- 1:3
#
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# (ctd_plot <- predictions_all %>%
#     filter(stim_type == 'ctd',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = prediction,
#                     fill = as.factor(pred_label),
#                     colour = as.factor(pred_label))) +
#     # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
#     #                 colour = as.factor(pred_label)))+
#     facet_grid(look_tminus1_num ~ after_stim,
#                labeller = labeller(look_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted age of neighbour relative to focal:',
#          fill = 'predicted age of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'cape turtle dove (control)')+
#     theme(legend.position = 'bottom'))
# (lion_plot <- predictions_all %>%
#     filter(stim_type == 'l',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = prediction,
#                     fill = as.factor(pred_label),
#                     colour = as.factor(pred_label))) +
#     # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
#     #                 colour = as.factor(pred_label)))+
#     facet_grid(look_tminus1_num ~ after_stim,
#                labeller = labeller(look_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted age of neighbour relative to focal:',
#          fill = 'predicted age of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'lion')+
#     theme(legend.position = 'bottom'))
# (human_plot <- predictions_all %>%
#     filter(stim_type == 'h',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = prediction,
#                     fill = as.factor(pred_label),
#                     colour = as.factor(pred_label))) +
#     # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
#     #                 colour = as.factor(pred_label)))+
#     facet_grid(look_tminus1_num ~ after_stim,
#                labeller = labeller(look_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted age of neighbour relative to focal:',
#          fill = 'predicted age of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# (ctd_plot + lion_plot + human_plot)+
#   plot_alookotation(tag_levels = 'a')
# ggsave(plot = last_plot(), file = '../outputs/looking_ordinal_model_2bda/look_predictions_violin.png',
#        device = 'png', height = 8, width = 24)
#
# ## graph contrasts from predictions and extract coefficients -- nn ####
# #CALCULATE POSTERIOR CONTRASTS FROM PREDICTIONS
# # load('looking_direction/looking_bda_predictions.RData') # load('ele_playbacks/looking_direction/looking_bda_predictions.RData')
# rm(prevsec_labels, ctd_plot, human_plot, lion_plot, predictions_all) ; gc()
#
# # stim type -- nn ####
# ## redo predictions with different stimulus types: all doves
# ctd_look <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'ctd',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# ctd_mtx <- posterior_epred(object = lom2_fit, newdata = ctd_look)
# colnames(ctd_mtx) <- ctd_look$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with different stimulus types: all lions
# lion_look <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'l',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# lion_mtx <- posterior_epred(object = lom2_fit, newdata = lion_look)
# colnames(lion_mtx) <- lion_look$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with different stimulus types: all humans
# human_look <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'h',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# human_mtx <- posterior_epred(object = lom2_fit, newdata = human_look)
# colnames(human_mtx) <- human_look$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# save.image('looking_direction/looking_bda_stimuluscontrasts_epred.RData')
#
# ## count types of each prediction
# #load('looking_direction/looking_bda_stimuluscontrasts_epred.RData')
# # count_values <- function(vector, levels = c(1,2,3)) {
# #   x <- tabulate(factor(vector, levels), length(levels))
# #   return(list(x))
# # }
# stim_pred <- ctd_look %>%
#   dplyr::select(-stim_type) %>%
#   # mutate(ctd_count = apply(ctd_mtx, 2, count_values),
#   #        lion_count = apply(lion_mtx, 2, count_values),
#   #        human_count = apply(human_mtx, 2, count_values)) %>%
#   # ulookest(c(ctd_count, lion_count, human_count)) %>% # I've done something weird with the count_values function so for now this needs ulookesting twice, but should probably fix it at some point! For now this works!
#   # ulookest(c(ctd_count, lion_count, human_count)) %>%
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
#            sep = '_m', relook = T) %>%
#   select(-mu) %>%
#   separate(col = stim_propage_sd, into = c('stim_propage_sd','sd'),
#            sep = '_s', relook = T) %>%
#   select(-sd) %>%
#   filter(stim_propage_mu == stim_propage_sd) %>%
#   separate(col = stim_propage_mu, into = c('stim_type', 'look_pred'),
#            sep = '_prop', relook = T) %>%
#   select(-stim_propage_sd) %>%
#   mutate(look_pred = as.numeric(look_pred)) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older')))
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
# contrasts <- look %>%
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
#   separate(contrast, into = c('contrast','look_pred'),
#            sep = '_age', relook = T) %>%
#   separate(look_pred, into = c('look_pred','mu'),
#            sep = '_', relook = T) %>%
#   mutate(look_pred = as.numeric(look_pred)) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older'))) %>%
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
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast)
#
# save.image('looking_direction/looking_bda_stimuluscontrasts_epred.RData')
#
# # focal age -- nn ####
# # load('looking_direction/looking_bda_stimuluscontrasts_epred.RData')
# rm(ctd_look, ctd_mtx, human_look, human_mtx, lion_look, lion_mtx,
#    contrasts, contrasts_long, stim_pred,
#    ctd_vs_human_age1, ctd_vs_human_age2, ctd_vs_human_age3,
#    ctd_vs_lion_age1, ctd_vs_lion_age2, ctd_vs_lion_age3,
#    lion_vs_human_age1, lion_vs_human_age2, lion_vs_human_age3) ; gc()
#
# ## predict with original ages
# age_look_org <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# age_mtx_org <- posterior_epred(object = lom2_fit, newdata = age_look_org)
# colnames(age_mtx_org) <- age_look_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with altered ages
# age_look_alt <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(f_age_num_original = f_age_num) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1),
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(f_age_num_original)
# age_mtx_alt <- posterior_epred(object = lom2_fit, newdata = age_look_alt)
# colnames(age_mtx_alt) <- age_look_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# save.image('looking_direction/looking_bda_agecontrasts_epred.RData')
#
# ## summarise and convert to long format
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
#   pivot_longer(cols = c(age_org_prop1_mu,age_org_prop2_mu,age_org_prop3_mu,
#                         age_alt_prop1_mu,age_alt_prop2_mu,age_alt_prop3_mu),
#                names_to = 'focal_agelook_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(age_org_prop1_sd,age_org_prop2_sd,age_org_prop3_sd,
#                         age_alt_prop1_sd,age_alt_prop2_sd,age_alt_prop3_sd),
#                names_to = 'focal_agelook_sd', values_to = 'stdv_propn') %>%
#   separate(col = focal_agelook_mu, into = c('focal_agelook_mu','mu'),
#            sep = '_m', relook = T) %>%
#   separate(col = focal_agelook_sd, into = c('focal_agelook_sd','sd'),
#            sep = '_s', relook = T) %>%
#   select(-mu, -sd) %>%
#   filter(focal_agelook_mu == focal_agelook_sd) %>%
#   separate(col = focal_agelook_mu, into = c('original_altered', 'look_pred'),
#            sep = '_prop', relook = T) %>%
#   select(-focal_agelook_sd) %>%
#   mutate(look_pred = as.numeric(look_pred),
#          f_age_num = ifelse(original_altered == 'age_org',
#                             f_age_num,
#                             ifelse(original_altered == 'age_alt' & f_age_num == 4,
#                                    1, f_age_num + 1))) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older')))
#
# ## calculate contrasts
# alt_vs_org_young <- age_mtx_alt[,,1] - age_mtx_org[,,1]
# alt_vs_org_match <- age_mtx_alt[,,2] - age_mtx_org[,,2]
# alt_vs_org_older <- age_mtx_alt[,,3] - age_mtx_org[,,3]
#
# ## summarise contrasts
# contrasts <- look %>%
#   mutate(alt_vs_org_young_mu = apply(alt_vs_org_young, 2, mean),
#          alt_vs_org_young_sd = apply(alt_vs_org_young, 2, sd),
#          alt_vs_org_match_mu = apply(alt_vs_org_match, 2, mean),
#          alt_vs_org_match_sd = apply(alt_vs_org_match, 2, sd),
#          alt_vs_org_older_mu = apply(alt_vs_org_older, 2, mean),
#          alt_vs_org_older_sd = apply(alt_vs_org_older, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(alt_vs_org_young_mu,alt_vs_org_match_mu,alt_vs_org_older_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('alt','vs','org','look_pred','mu'),
#            sep = '_', relook = T) %>%
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
#   mutate(pred_type = ifelse(look_pred == 'young', 'younger',
#                             ifelse(look_pred == 'match', 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   mutate(f_age_new = ifelse(f_age_num == 4, 1, f_age_num+1)) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ f_age_new, scales = 'free')
# save.image('looking_direction/looking_bda_agecontrasts_epred.RData')
#
# # neighbour in previous second -- nn ####
# #load('looking_direction/looking_bda_agecontrasts_epred.RData')
# rm(age_look_org, age_mtx_org, age_look_alt, age_mtx_alt, age_pred, alt_vs_org_young, alt_vs_org_match, alt_vs_org_older, contrasts, contrasts_long) ; gc()
#
# ## redo predictions with different previous neighbours: all younger -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 1, look AT T-1 YOUNGER
# young_look <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(look_tminus1_num = 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# young_mtx <- posterior_epred(object = lom2_fit, newdata = young_look)
# colnames(young_mtx) <- young_look$unique_data_combo
# young_mtx <- young_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with different previous neighbours: all matching
# match_look <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(look_tminus1_num = 2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# match_mtx <- posterior_epred(object = lom2_fit, newdata = match_look)
# colnames(match_mtx) <- match_look$unique_data_combo
# match_mtx <- match_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with different previous neighbours: all older -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 4, look AT T-1 OLDER
# older_look <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(look_tminus1_num = 3,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# older_mtx <- posterior_epred(object = lom2_fit, newdata = older_look)
# colnames(older_mtx) <- older_look$unique_data_combo
# older_mtx <- older_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# save.image('looking_direction/looking_bda_tminus1contrasts_epred.RData')
#
# ## count types of each prediction
# #load('looking_direction/looking_bda_tminus1contrasts_epred.RData')
# prevsec_pred <- young_look %>%
#   dplyr::select(-look_tminus1_num) %>%
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
#            sep = '_m', relook = T) %>%
#   separate(col = prevsec_propage_sd, into = c('prevsec_propage_sd','sd'),
#            sep = '_s', relook = T) %>%
#   select(-mu, -sd) %>%
#   filter(prevsec_propage_mu == prevsec_propage_sd) %>%
#   separate(col = prevsec_propage_mu, into = c('prevsec_type', 'look_pred'),
#            sep = '_prop', relook = T) %>%
#   select(-prevsec_propage_sd) %>%
#   mutate(look_pred = as.numeric(look_pred)) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older')))
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
# contrasts <- look %>%
#   select(-look_tminus1_num) %>%
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
#   separate(contrast, into = c('contrast','look_pred'),
#            sep = '_age', relook = T) %>%
#   separate(look_pred, into = c('look_pred','mu'),
#            sep = '_', relook = T) %>%
#   mutate(look_pred = as.numeric(look_pred)) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older'))) %>%
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
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast)
#
# save.image('looking_direction/looking_bda_prevseccontrasts_epred.RData')
#
# # predictions %>%
# #   mutate(previous = ifelse(look_tminus1_num == 1, 'younger',
# #                            ifelse(look_tminus1_num == 2, 'same age', 'older')),
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
# # prevsec2 <- pred_prop %>% filter(look_tminus1_num == 2)
# # prevsec3 <- pred_prop %>% filter(look_tminus1_num == 3)
# # pred_prev <- pred_prop %>%
# #   filter(look_tminus1_num == 1) %>%
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
# #   mutate(look1_2 = prop_1 - prop_2,
# #          look1_3 = prop_1 - prop_3,
# #          look2_3 = prop_2 - prop_3)
# # pred_prev %>%
# #   select(stim_type, after_stim, f_age_num, prediction,
# #          look1_2,look1_3,look2_3) %>%
# #   pivot_longer(cols = c('look1_2', 'look1_3', 'look2_3'),
# #                names_to = 'contrast', values_to = 'value') %>%
# #   mutate(contrast = ifelse(contrast == 'look1_2', 'younger vs same',
# #                            ifelse(contrast == 'look1_3', 'younger vs older',
# #                                   'same vs older'))) %>%
# #   ggplot()+
# #   geom_density(aes(x = value, colour = contrast), linewidth = 1)+
# #   scale_colour_viridis_d()+
# #   labs(colour = 't-1 pair',
# #        x = 'difference between neighbours at previous second')+
# #   geom_vline(xintercept = 0, linetype = 2)+
# #   scale_x_continuous(limits = c(-2,2))
#
# # time since stimulus -- nn ####
# # load('looking_direction/looking_bda_prevseccontrasts_epred.RData')
# rm(young_look, young_mtx, match_look, match_mtx, older_look, older_mtx,
#    contrasts, contrasts_long, prevsec_pred,
#    young_vs_match_age1, young_vs_match_age2, young_vs_match_age3,
#    young_vs_older_age1, young_vs_older_age2, young_vs_older_age3,
#    match_vs_older_age1, match_vs_older_age2, match_vs_older_age3) ; gc()
#
# ## predict with original times
# time_look_org <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# time_mtx_org <- posterior_epred(object = lom2_fit, newdata = time_look_org)
# colnames(time_mtx_org) <- time_look_org$unique_data_combo
# time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with shifted times: +15 seconds
# time_look_alt_0.25 <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.25 <- posterior_epred(object = lom2_fit, newdata = time_look_alt_0.25)
# colnames(time_mtx_alt_0.25) <- time_look_alt_0.25$unique_data_combo
# time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with shifted times: +30 seconds
# time_look_alt_0.50 <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1/2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.50 <- posterior_epred(object = lom2_fit, newdata = time_look_alt_0.50)
# colnames(time_mtx_alt_0.50) <- time_look_alt_0.50$unique_data_combo
# time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with shifted times: +45 seconds
# time_look_alt_0.75 <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 3/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.75 <- posterior_epred(object = lom2_fit, newdata = time_look_alt_0.75)
# colnames(time_mtx_alt_0.75) <- time_look_alt_0.75$unique_data_combo
# time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with shifted times: +60 seconds
# time_look_alt_1.00 <- look %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_1.00 <- posterior_epred(object = lom2_fit, newdata = time_look_alt_1.00)
# colnames(time_mtx_alt_1.00) <- time_look_alt_1.00$unique_data_combo
# time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# save.image('looking_direction/looking_bda_timecontrasts_epred.RData')
#
# ## summarise and convert to long format
# time_pred <- time_look_org %>%
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
#                names_to = 'time_org_alt_prop_agelook_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(time_org_0.00_prop1_sd,time_org_0.00_prop2_sd,time_org_0.00_prop3_sd,
#                         time_alt_0.25_prop1_sd,time_alt_0.25_prop2_sd,time_alt_0.25_prop3_sd,
#                         time_alt_0.50_prop1_sd,time_alt_0.50_prop2_sd,time_alt_0.50_prop3_sd,
#                         time_alt_0.75_prop1_sd,time_alt_0.75_prop2_sd,time_alt_0.75_prop3_sd,
#                         time_alt_1.00_prop1_sd,time_alt_1.00_prop2_sd,time_alt_1.00_prop3_sd),
#                names_to = 'time_org_alt_prop_agelook_sd', values_to = 'stdv_propn') %>%
#   separate(col = time_org_alt_prop_agelook_mu,
#            into = c('time_mu','org_mu','alt_mu','prop_agelook_mu','mu'),
#            sep = '_', relook = T) %>%
#   separate(col = time_org_alt_prop_agelook_sd,
#            into = c('time_sd','org_sd','alt_sd','prop_agelook_sd','sd'),
#            sep = '_', relook = T) %>%
#   select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>%
#   filter(alt_mu == alt_sd & prop_agelook_sd == prop_agelook_mu) %>%
#   mutate(look_pred = ifelse(prop_agelook_mu == 'prop1', 1,
#                           ifelse(prop_agelook_mu == 'prop2', 2,
#                                  ifelse(prop_agelook_mu == 'prop3', 3, 4)))) %>%
#   select(-alt_sd, -prop_agelook_mu, -prop_agelook_sd) %>%
#   rename(mins_added = alt_mu) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older')))
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
# contrasts <- look %>%
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
#            sep = '_', relook = T) %>%
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
# save.image('looking_direction/looking_bda_timecontrasts_epred.RData')
#
######## movement direction ####
rm(list = ls()[! ls() %in% 'behav']) ; gc()
pdf('../outputs/movement_ordinal_model_2bda/movement_ordinal_2bda_modelchecks.pdf')

#### create data ####
## select specific data
move <- behav %>%
  filter(activity == 'move') %>%
  rename(action = action_name,
         move_index = action_index) %>% 
  select(-activity, -stim_start, -stim_stop) %>%
  mutate(prev_action = NA,
         prev_num = NA) %>%
  filter(!is.na(f_age_num)) %>%
  filter(!is.na(p_age_num))

# create variable for movement direction at time t-1
focals <- unique(move$focal)
for(f in 1:length(focals)){
  focal <- move %>% filter(focal == focals[f])
  move <- move %>% anti_join(focal, by = 'focal')
  partners <- unique(focal$partner)
  for(p in 1:length(partners)){
    focal_partner <- focal %>% filter(partner == partners[p])
    focal <- focal %>% anti_join(focal_partner, by = 'partner')
    for(i in 2:nrow(focal_partner)){
      focal_partner$prev_action[i] <- focal_partner$action[i-1]
      focal_partner$prev_num[i] <- focal_partner$move_index[i-1]
    }
    focal <- rbind(focal, focal_partner)
  }
  move <- rbind(move, focal)
}
rm(focal, focals, focal_partner, f, p, i, partners) ; gc()

## remove observations where not moving currently or in previous second
move <- move %>%
  filter(action != 'not_moving') %>%
  filter(prev_action != 'not_moving') %>% 
  filter(!is.na(prev_action)) %>%
  mutate(f_age_num = as.integer(f_age_num))

#### set prior ####
get_prior(formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
            (1|focal) + (1|stim_num) + (1|pb_num),
          data = move, family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # interaction
  prior(normal(0,1),      class = b,    coef = age_combo1_2),
  prior(normal(0,1),      class = b,    coef = age_combo1_3),
  prior(normal(0,1),      class = b,    coef = age_combo1_4),
  prior(normal(0,1),      class = b,    coef = age_combo2_1),
  prior(normal(0,1),      class = b,    coef = age_combo2_2),
  prior(normal(0,1),      class = b,    coef = age_combo2_3),
  prior(normal(0,1),      class = b,    coef = age_combo2_4),
  prior(normal(0,1),      class = b,    coef = age_combo3_1),
  prior(normal(0,1),      class = b,    coef = age_combo3_2),
  prior(normal(0,1),      class = b,    coef = age_combo3_3),
  prior(normal(0,1),      class = b,    coef = age_combo3_4),
  prior(normal(0,1),      class = b,    coef = age_combo4_1),
  prior(normal(0,1),      class = b,    coef = age_combo4_2),
  prior(normal(0,1),      class = b,    coef = age_combo4_3),
  prior(normal(0,1),      class = b,    coef = age_combo4_4),
  # stim type
  prior(normal(0,1),      class = b,    coef = stim_typeh),
  prior(normal(0,1),      class = b,    coef = stim_typel),
  # before/during/after
  prior(normal(0,1),      class = b,    coef = bdabefore),
  prior(normal(0,1),      class = b,    coef = bdaduring),
  # action in previous second
  prior(normal(1,1),      class = b,    coef = moprev_num),
  prior(dirichlet(2),     class = simo, coef = moprev_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
mom2_prior <- brm(
  formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = move, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(mom2_prior) # huge variation in prior, but fairly on both sides so good

#### fit model ####
mom2_fit <- brm(
  formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = move, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)
save.image('movement_direction/moving_ordinal_2bda_run.RData')

#### extract draws ####
# load('moving_direction/moving_ordinal_2bda_run.RData')
## check model diagnostics -- moves very good
(summary <- summary(mom2_fit))
par(mfrow = c(3,1))
hist(summary$fixed$Rhat, breaks = 50)
hist(summary$fixed$Bulk_ESS, breaks = 50)
hist(summary$fixed$Tail_ESS, breaks = 50)
par(mfrow = c(1,1))

## extract posterior distribution
draws <- as_draws_df(mom2_fit) %>%
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

## extract marginal effects
marg <- conditional_effects(mom2_fit,
                            effects = c('f_age_num','age_combo','stim_type',
                                        'bda','prev_num'),
                            categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
agefocal_effect <- marg[[1]]
agecombo_effect <- marg[[2]]
stim_effect <- marg[[3]]
bda_effect <- marg[[4]]
prev_effect <- marg[[5]]

## move at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

#### calculate log cumulative odds ####
(prop <- table(move$move_index) / nrow(move))
(cum_prop <- cumsum(prop))
(log_cum_odds <- logit(cum_prop))

#### plot marginal effects ####
(f_age_num_plot <- ggplot(agefocal_effect)+
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
   ylab('probability of moving direction')+
   scale_colour_viridis_d(name = 'moving direction:',
                          breaks = c('1','2','3','4','5'),
                          labels = c('move away directly',
                                     'move away at an angle',
                                     'move neither towards or away',
                                     'approach at an angle',
                                     'approach directly'))+
   scale_fill_viridis_d(name = 'moving direction:',
                        breaks = c('1','2','3','4','5'),
                        labels = c('move away directly',
                                   'move away at an angle',
                                   'move neither towards or away',
                                   'approach at an angle',
                                   'approach directly'))+
   theme(legend.position = 'bottom',
         axis.title = element_text(size = 16),
         axis.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10)))
ggsave(plot = f_age_num_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_marginaleffects_agefocal.png', device = 'png',
       width = 8.3, height = 5.8)

f_age_num_labels <- c('focal age category 1',
                      'focal age category 2',
                      'focal age category 3',
                      'focal age category 4')
names(f_age_num_labels) <- 1:4
(agecombo_plot <- agecombo_effect %>%
    separate(col = age_combo, sep = '_', remove = F,
             into = c('f_age_num','partner_age')) %>%
    mutate(agecombo = paste0(f_age_num,'-',partner_age)) %>%
    ggplot()+
    geom_errorbar(aes(#x = agecombo,
      x = partner_age,
      colour = as.factor(cats__), # moving direction?
      ymax = upper__, ymin = lower__),
      linewidth = 1,
      width = 0.4)+
    geom_point(aes(#x = agecombo,
      x = partner_age,
      colour = as.factor(cats__),    # moving direction?
      #shape = f_age_num,
      y = estimate__),
      size = 3)+
    # geom_ribbon(aes(#x = agecombo,
    #                 x = as.numeric(partner_age),
    #                 fill = as.factor(cats__),     # moving direction?
    #                 ymax = upper__, ymin = lower__),
    #             alpha = 0.4)+
    # geom_line(aes(#x = agecombo,
    #               x = as.numeric(partner_age),
    #               colour = as.factor(cats__),     # moving direction?
    #               y = estimate__),
    #           linewidth = 1)+
    facet_wrap(. ~ f_age_num,
               labeller = labeller(f_age_num = f_age_num_labels))+
    ylab('probability of moving direction')+
    scale_colour_viridis_d(name = 'moving direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('move away directly',
                                      'move away at an angle',
                                      'move neither towards or away',
                                      'approach at an angle',
                                      'approach directly'))+
    # scale_fill_viridis_d(name = 'moving direction:',
    #                      breaks = c('1','2','3','4','5'),
    #                      labels = c('move away directly',
    #                                 'move away at an angle',
    #                                 'move neither towards or away',
    #                                 'approach at an angle',
    #                                 'approach directly'))+
    scale_x_discrete(name = 'partner age category')+
    #scale_x_continuous(name = 'partner age category')+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = agecombo_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_marginaleffects_agepartner.png', device = 'png',
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
    xlab(label = 'stimulus type') + ylab('probability of moving direction')+
    scale_colour_viridis_d(name = 'moving direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('move away directly',
                                      'move away at an angle',
                                      'move neither towards or away',
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
ggsave(plot = stim_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_marginaleffects_stimtype.png',
       device = 'png', width = 8.3, height = 5.8)

(f_age_num_plot + agecombo_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = '../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(mom2_fit, ndraws = 100) # really good fit

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "b_bdabefore","b_bdaduring",
                          "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
                          "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
                          "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
                          "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "bsp_moprev_num","simo_moprev_num1[1]","simo_moprev_num1[2]"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing generally moves very good, though a couple of age combo ones are a touch wandery

## move at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,1))
hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')

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

## moving direction in previous second
prevsec_slope <- draws_cut %>% filter(parameter == 'bsp_moprev_num')
prevsec2 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[1]')
prevsec3 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[2]')
par(mfrow = c(3,1))
plot(density(prevsec_slope$draw), main = 'slope of prevsec') ; abline(v = 0, lty = 2)
plot(density(prevsec2$draw), main = 't-1 matched vs younger') ; abline(v = 0, lty = 2)
plot(density(prevsec3$draw), main = 't-1 older vs younger') ; abline(v = 0, lty = 2)

## time since stimulus -- come back to this!
before <- draws_cut %>% filter(parameter == 'b_bdabefore')
during <- draws_cut %>% filter(parameter == 'b_bdaduring')
par(mfrow = c(2,1))
plot(density(before$draw), main = 'after --> before') ; abline(v = 0, lty = 2)
plot(density(during$draw), main = 'after --> during') ; abline(v = 0, lty = 2)

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

# ## plot overall
# ggplot(move,aes(x = f_age_num, y = move_index,
#                 colour = age_combo))+
#   geom_jitter(alpha = 0.1)+
#   facet_wrap(. ~ stim_type,
#              labeller = labeller(stim_type = stim_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('move directly at','side-on','move directly away'))+
#   labs(colour = 'age difference')

## plot control data
move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = bda, y = move_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal moving direction relative to target',
                     breaks = c('1','2','3','4','5'),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  scale_x_discrete(name = 'time relative to stimulus')

move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = move_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal moving direction relative to target',
                     breaks = c('1','2','3','4','5'),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## plot lion data
move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'l') %>%
  ggplot(aes(x = bda, y = move_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal moving direction relative to target',
                     breaks = c('1','2','3','4','5'),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  scale_x_discrete(name = 'time relative to stimulus')

move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'l') %>%
  ggplot(aes(x = move_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal moving direction relative to target',
                     breaks = c('1','2','3','4','5'),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## plot human data
move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'h') %>%
  ggplot(aes(x = bda, y = move_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal moving direction relative to target',
                     breaks = c('1','2','3','4','5'),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  scale_x_discrete(name = 'time relative to stimulus')

move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>% 
  filter(stim_type == 'h') %>%
  ggplot(aes(x = move_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal moving direction relative to target',
                     breaks = c('1','2','3','4','5'),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('ele_playbacks/moving_direction/moving_ordinal_2bda_run.RData') # save.image('moving_direction/moving_ordinal_2bda_run.RData')
dev.off()
#pdf('../outputs/movement_ordinal_model_2bda/moving_ordinal_2bda_modelpredictions.pdf')

