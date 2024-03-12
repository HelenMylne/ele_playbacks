#### information ####
# simple models to compare before/during/after for each measure of social behaviour

#### set up ####
#library(tidyverse); library(brms) ; library(LaplacesDemon) ; library(ggpubr)
library(tidyverse, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(ggpubr, lib.loc = '../../packages/')

theme_set(theme_classic())

#### import data about playbacks ####
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

#### combine with behaviour data to identify before/during/after ####
cols_of_interest <- c('b1_nn_name','b2_nn_name','b3_nn_name','b4_nn_name',
  'b5_nn_name','b6_nn_name','b7_nn_name','b8_nn_name',
  'b1_move_name','b2_move_name','b3_move_name','b4_move_name',
  'b5_move_name','b6_move_name','b7_move_name','b8_move_name',
  'b1_look_name','b2_look_name','b3_look_name','b4_look_name',
  'b5_look_name','b6_look_name','b7_look_name','b8_look_name')
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>% 
  select(subject,pb_num,second,out_frame_name,
         all_of(cols_of_interest)) %>%
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity', values_to = 'action') %>% 
  filter(is.na(action) == FALSE) %>% 
  filter(action != 'impossible_partner') %>% 
  filter(out_frame_name != 'out_of_sight') %>% 
  separate(elephant_activity, into = c('targeted_elephant','activity','name'), sep = '_', remove = T) %>% 
  dplyr::select(-name,-out_frame_name) %>% 
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
  dplyr::select(pb_num,focal,partner,activity,action,
                stim_num,stim_type,stim_start,stim_stop,second,bda,
                f_age_cat,p_age_cat,f_age_num,p_age_num) %>% 
  mutate(f_age_num = as.factor(f_age_num),
         p_age_num = as.factor(p_age_num),
         age_combo = paste0(f_age_num,'_',p_age_num))
rm(cols_of_interest, stim_starts, stim_stops, ages) ; gc()

## remove individuals where ages are unknown
behav <- behav %>% 
  filter(!is.na(f_age_num))

# #### nearest neighbour ####
# ## select specific data
# nn <- behav %>% 
#   filter(activity == 'nn') %>% 
#   select(-activity, -stim_start, -stim_stop, -second) %>% 
#   mutate(prev = NA,
#          action = as.numeric(action),
#          f_age_num = as.factor(as.numeric(f_age_num)),
#          p_age_num = as.factor(as.numeric(p_age_num))) %>% 
#   filter(!is.na(p_age_num))
# 
# ## fill in behaviour in previous second
# subjects <- unique(nn$focal)
# for(i in 1:length(subjects)){
#   focal <- nn %>% filter(focal == subjects[i])
#   nn <- nn %>% anti_join(focal, by = 'focal')
#   for(j in 2:nrow(focal)){
#     focal$prev[j] <- focal$action[j-1]
#   }
#   nn <- rbind(nn, focal)
# }
# rm(focal, i, j, subjects) ; gc()
# 
# ## remove observations where nearest neighbour in previous second was unknown
# nn <- nn %>% 
#   filter(!is.na(prev))
# 
# ## set prior ####
# get_prior(formula = action ~ 1 + age_combo + stim_type + bda + mo(prev) +  
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
#   prior(normal(0,1),      class = b,    coef = moprev),
#   prior(dirichlet(2),   class = simo, coef = moprev1))
# 
# ## prior predictive check
# num_chains <- 4
# num_iter <- 2000
# nn_prior <- brm(
#   formula = action ~ 1 + age_combo + stim_type + bda + mo(prev) +  
#     (1|focal) + (1|stim_num) + (1|pb_num), 
#   data = nn, family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(nn_prior) # huge variation in prior, but fairly on both sides so good
# 
# ## fit model ####
# nn_fit <- brm(
#   formula = action ~ 1 + age_combo + stim_type + bda + mo(prev) +  
#     (1|focal) + (1|stim_num) + (1|pb_num), 
#   data = nn, family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# save.image('nearest_neighbour/neighbour_model_run_bda.RData')
# 
# # ## check model fit
# # (summary <- summary(nn_fit))
# # par(mfrow = c(3,1))
# # hist(summary$fixed$Rhat, breaks = 50)
# # hist(summary$fixed$Bulk_ESS, breaks = 50)
# # hist(summary$fixed$Tail_ESS, breaks = 50)
# # par(mfrow = c(1,1))
# # 
# # ## extract posterior distribution
# # draws <- as_draws_df(nn_fit) %>% 
# #   select(-lprior, -`lp__`)
# # parameters <- colnames(draws)[1:(ncol(draws)-3)]
# # draws <- draws  %>% 
# #   pivot_longer(cols = all_of(parameters),
# #                names_to = 'parameter',
# #                values_to = 'draw') %>% 
# #   rename(chain = `.chain`,
# #          position = `.iteration`,
# #          draw_id = `.draw`) %>% 
# #   mutate(invlogit_draw = invlogit(draw))
# # 
# # ## extract marginal effects
# # marg <- conditional_effects(nn_fit,
# #                             effects = c('age_combo','stim_type',
# #                                         'bda','prev'),
# #                             categorical = FALSE,
# #                             #spaghetti = TRUE,
# #                             method = 'posterior_epred')
# # names(marg)
# # age_effect <- marg[[1]]
# # stim_effect <- marg[[2]]
# # bda_effect <- marg[[3]]
# # prev_effect <- marg[[4]]
# # 
# # ## plot marginal effects ####
# # neighbour_labels <- c('neighbour age category 1',
# #                       'neighbour age category 2',
# #                       'neighbour age category 3',
# #                       'neighbour age category 4')
# # names(neighbour_labels) <- c(1:4)
# # (focal_age_plot <- age_effect %>% 
# #    separate(col = age_combo, sep = '_', remove = F,
# #             into = c('focal_age','neighbour_age')) %>% 
# #    mutate(agecombo = paste0(focal_age,'-',neighbour_age)) %>% 
# #    ggplot()+
# #    geom_errorbar(aes(#x = agecombo,
# #                      x = focal_age,
# #                      colour = focal_age,
# #                      #linetype = neighbour_age,
# #                      ymax = upper__, ymin = lower__),
# #                  linewidth = 1, width = 0.2)+
# #    geom_point(aes(#x = agecombo,
# #                   x = focal_age,
# #                   colour = focal_age,
# #                   #shape = neighbour_age,
# #                   y = estimate__),
# #               cex = 3)+
# #    #xlab(label = 'combined age categories')+
# #    xlab(label = 'focal age category')+
# #    ylab('probability of being nearest neighbours:\nafter dove stimulus, not nearest neighbours in previous second')+
# #    scale_colour_viridis_d(name = 'focal age:')+
# #    #scale_linetype(name = 'neighbour age line type:')+
# #    #scale_shape_manual(name = 'neighbour age shape:', values = c(15:18))+
# #    facet_wrap(. ~ neighbour_age,
# #               labeller = labeller(neighbour_age = neighbour_labels))+
# #    theme(legend.direction = 'horizontal',
# #          legend.position = 'bottom',
# #          legend.box = 'vertical',
# #          legend.spacing.x = unit(0.2, 'cm'),
# #          legend.spacing.y = unit(2, 'mm'),
# #          axis.title = element_text(size = 16),
# #          axis.text.x = element_text(size = 12,
# #                                     #angle = 70,
# #                                     vjust = 0.5),
# #          axis.text.y = element_text(size = 12),
# #          legend.title = element_text(size = 12),
# #          legend.text = element_text(size = 10)) )
# # ggsave(plot = focal_age_plot, filename = '../outputs/nn_marginaleffects_focalage_bda.png', device = 'png',
# #        width = 8.3, height = 5.8)
# # 
# # (stim_plot <- stim_effect %>% 
# #     ggplot()+
# #     geom_errorbar(aes(x = stim_type,
# #                       colour = stim_type,
# #                       ymax = upper__, ymin = lower__),
# #                   linewidth = 1, width = 0.2)+
# #     geom_point(aes(x = stim_type,
# #                    colour = stim_type,
# #                    shape = stim_type,
# #                    y = estimate__),
# #               cex = 3)+
# #     ylab('probability of being nearest neighbours after stimulus:\nage 1 with age 1, not neighbours in previous second')+
# #     scale_colour_viridis_d(name = 'stimulus type:')+
# #     scale_shape_manual(name = 'stimulus type:', values = c(15:18))+
# #     scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
# #                      labels = c('dove (control)', 'lion', 'human'),
# #                      limits = c('ctd','l','h'))+
# #     theme(legend.position = 'none',
# #           axis.title = element_text(size = 16),
# #           axis.text = element_text(size = 12),
# #           legend.title = element_text(size = 12),
# #           legend.text = element_text(size = 10)) )
# # ggsave(plot = stim_plot, filename = '../outputs/nn_marginaleffects_stimtype_bda.png', device = 'png',
# #        width = 8.3, height = 5.8)
# # 
# # (all_plots <- ggarrange(focal_age_plot, stim_plot, ncol=2, nrow=1, common.legend = FALSE, legend = "bottom"))
# # ggsave(plot = all_plots, filename = '../outputs/nn_marginaleffects_bda.png', device = 'png',
# #        width = (5.8*2), height = 8.3)
# # 
# # rm(all_plots,focal_age_plot,stim_plot,age_effect,prev_effect,stim_effect,bda_effect) ;gc()
# # 
# # ## posterior predictive check ####
# # pp_check(nn_fit, ndraws = 100) # perfect fit
# # 
# # ## plot traces ####
# # parameters_of_interest <- parameters[1:which(parameters == 'simo_moprev1[1]')]
# # draws %>% 
# #   filter(parameter %in% parameters_of_interest) %>% 
# #   ggplot(aes(x = position, y = draw, colour = as.factor(chain)))+
# #   geom_line()+
# #   facet_wrap(. ~ parameter, scales = 'free_y')+
# #   theme(legend.position = 'none') # mostly fine, but playback ID intercept has a weird unmixed bit
# # 
# # ## plot density curves ####
# # draws %>% 
# #   filter(parameter %in% parameters_of_interest) %>% 
# #   ggplot(aes(x = draw, colour = as.factor(chain)))+
# #   geom_density()+
# #   facet_wrap(. ~ parameter, scales = 'free')+
# #   theme(legend.position = 'none')
# # 
# # save.image('nearest_neighbour/neighbour_model_run_bda.RData')
# # 
# # ## predict from model ####
# # rm(list = ls()[! ls() %in% c('nn_fit','nn')])
# # 
# # pred <- posterior_predict(object = nn_fit,
# #                           newdata = nn)
# # save.image('nearest_neighbour/neighbour_model_predictions_bda.RData')
# # 
#### looking direction ####
rm(list = ls()[! ls() %in% 'behav'])

## select specific data
look <- behav %>% 
  filter(activity == 'look') %>% 
  select(-activity, -stim_start, -stim_stop, -second) %>% 
  mutate(look_index = ifelse(action == 'look at directly', 1,
                             ifelse(action == 'side-on', 2, 3)),
         prev_action = NA,
         prev_num = NA) %>% 
  filter(!is.na(f_age_num)) %>% 
  filter(!is.na(p_age_num))

## fill in behaviour in previous second
subjects <- unique(look$focal)
for(i in 1:length(subjects)){
  focal <- look %>% filter(focal == subjects[i])
  look <- look %>% anti_join(focal, by = 'focal')
  for(j in 2:nrow(focal)){
    focal$prev_action[j] <- focal$action[j-1]
    focal$prev_num[j] <- focal$look_index[j-1]
  }
  look <- rbind(look, focal)
}
rm(focal, i, j, subjects) ; gc()

## remove observations where look in previous second was unknown
look <- look %>% 
  filter(!is.na(prev_action)) %>% 
  mutate(f_age_num = as.integer(f_age_num))

## set prior ####
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
  prior(normal(0,1),      class = b,    coef = moprev_num),
  prior(dirichlet(2),     class = simo, coef = moprev_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
look_prior <- brm(
  formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +  
    (1|focal) + (1|stim_num) + (1|pb_num), 
  data = look, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(look_prior) # huge variation in prior, but fairly on both sides so good

## fit model ####
look_fit <- brm(
  formula = look_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +  
    (1|focal) + (1|stim_num) + (1|pb_num), 
  data = look, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)
save.image('looking_direction/look_model_run_bda.RData')

# ## check model fit
# (summary <- summary(look_fit))
# par(mfrow = c(3,1))
# hist(summary$fixed$Rhat, breaks = 50)
# hist(summary$fixed$Bulk_ESS, breaks = 50)
# hist(summary$fixed$Tail_ESS, breaks = 50)
# par(mfrow = c(1,1))
# 
# ## extract posterior distribution
# draws <- as_draws_df(look_fit) %>% 
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
# marg <- conditional_effects(look_fit,
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
#### movement direction ####
rm(list = ls()[! ls() %in% 'behav'])

## select specific data
move <- behav %>% 
  filter(activity == 'move') %>% 
  filter(action != 'not_moving') %>% 
  select(-activity, -stim_start, -stim_stop, -second) %>% 
  mutate(move_index = ifelse(action == 'approach directly', 1,
                             ifelse(action == 'approach at an angle', 2,
                                    ifelse(action == 'move directly with', 3,
                                           ifelse(action == 'move away at an angle', 4, 5)))),
         prev_action = NA,
         prev_num = NA) %>% 
  filter(!is.na(f_age_num)) %>% 
  filter(!is.na(p_age_num))

## fill in behaviour in previous second
subjects <- unique(move$focal)
for(i in 1:length(subjects)){
  focal <- move %>% filter(focal == subjects[i])
  move <- move %>% anti_join(focal, by = 'focal')
  for(j in 2:nrow(focal)){
    focal$prev_action[j] <- focal$action[j-1]
    focal$prev_num[j] <- focal$move_index[j-1]
  }
  move <- rbind(move, focal)
}
rm(focal, i, j, subjects) ; gc()

## remove observations where move in previous second was unknown
move <- move %>% 
  filter(!is.na(prev_action)) %>% 
  mutate(f_age_num = as.integer(f_age_num))

## set prior ####
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
  prior(normal(0,1),      class = b,    coef = moprev_num),
  prior(dirichlet(2),     class = simo, coef = moprev_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
move_prior <- brm(
  formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +  
    (1|focal) + (1|stim_num) + (1|pb_num), 
  data = move, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(move_prior) # huge variation in prior, but fairly on both sides so good

## fit model ####
move_fit <- brm(
  formula = move_index ~ mo(f_age_num) + age_combo + stim_type + bda + mo(prev_num) +  
    (1|focal) + (1|stim_num) + (1|pb_num), 
  data = move, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)
save.image('movement_direction/move_model_run_bda.RData')
