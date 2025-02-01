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

# ######## import data about playbacks                                 ####
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
# # 1  3  4  6  7 10 11 13 14 15 16 17 18 19 21 22 23 24 25 28 29 30 31 32 33 34 35 36 37 38 41 42 43 44 45 46 47 48 50 51 52 53 55 56 58 59 60 61
# # 1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  2  1  1  2  1  1  3  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  5  1  1  1  1  1  1
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
#     as.data.frame() %>%
#     filter(Freq > 1))
# #   Var1 Freq
# # 1   10    2
# # 2   24    2
# # 3   29    2
# # 4   32    3
# # 5   46    2
# # 6   53    5
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
######## nearest neighbour                                           ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_modelchecks_newstructure.pdf')
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')

## remove individuals where ages are unknown
behav <- behav %>%
  filter(!is.na(f_age_num))

## set parameters
num_chains <- 4
num_iter <- 2000

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

#### change age combo to reduce number of categories                 ####
nn <- nn %>%
  mutate(f_age_wide = ifelse(as.numeric(f_age_num) < 3, 'Y', 'O'),
         p_age_wide = ifelse(as.numeric(p_age_num) < 3, 'Y', 'O')) %>%
  mutate(age_rel = paste0(f_age_wide, p_age_wide)) %>%
  filter(f_age_cat != 'unkage')
sort(unique(nn$age_rel))

## plot raw data
ggplot(nn)+
  geom_bar(aes(x = age_rel, fill = action_name))+
  scale_x_discrete(name = 'relative ages')+
  scale_fill_viridis_d(end = 0.5)+
  labs(fill = 'neighbours')+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,65000))

nn_prop <- table(nn$age_rel, nn$action_name) %>% 
  as.data.frame() %>% 
  rename(age_rel = Var1,
         neighbours = Var2,
         count = Freq) %>% 
  mutate(neighbours = ifelse(neighbours == 0, 'no','yes'),
         prop = NA) %>% 
  mutate(neighbours = factor(neighbours, levels = c('yes','no')))
for(i in 1:nrow(nn_prop)){
  nn_prop$prop[i] <- nn_prop$count[i] / sum(nn_prop$count[nn_prop$age_rel == nn_prop$age_rel[i]])
}

ggplot(nn_prop)+
  geom_col(aes(x = age_rel, fill = neighbours, y = prop))+
  scale_x_discrete(name = 'relative ages')+
  scale_fill_viridis_d(end = 0.5)+
  labs(fill = 'neighbours')+
  scale_y_continuous(expand = c(0,0),
                     name = 'proportion')

dyads <- nn %>% 
  select(pb_num, focal, partner, f_age_wide, p_age_wide, age_rel) %>% 
  distinct()
table(dyads$age_rel)

nodes <- nn %>% 
  select(pb_num, focal, f_age_wide) %>% 
  distinct()
table(nodes$f_age_wide)

groups <- nn %>% 
  select(pb_num, f_age_wide) %>% 
  distinct()
table(groups$f_age_wide, groups$pb_num)

playbacks <- nn %>% 
  select(pb_num) %>% 
  distinct() %>% 
  mutate(prop_old = NA,
         group_size = NA)
for(i in 1:nrow(playbacks)){
  pb <- nodes %>% 
    filter(pb_num == playbacks$pb_num[i])
  playbacks$prop_old[i] <- length(which(pb$f_age_wide == 'O')) / nrow(pb)
  playbacks$group_size[i] <- nrow(pb)
}

ggplot(playbacks)+
  geom_point(aes(x = group_size, y = prop_old))

nodes <- nodes %>% 
  filter(pb_num %in% nn$pb_num) %>% 
  mutate(num_old_options = NA,
         prop_old_options = NA)
for(i in 1:nrow(nodes)){
  pb <- nodes %>% 
    filter(pb_num == nodes$pb_num[i]) %>% 
    filter(focal != nodes$focal[i])
  nodes$num_old_options[i] <- length(which(pb$f_age_wide == 'O'))
  nodes$prop_old_options[i] <- nodes$num_old_options[i] / nrow(pb)
}

nn <- nn %>% 
  left_join(nodes, by = c('pb_num','focal','f_age_wide')) %>% 
  left_join(playbacks, by = 'pb_num')

#### new format of model: neighbour age ~ focal age                  ####
rm(list = ls() [! ls() %in% c('nn','behav','num_chains','num_iter')]) ; gc()
neighbours <- nn %>%
  filter(action == 1) %>%
  dplyr::select(pb_num,focal,partner,stim_num,stim_type,bda,
                f_age_cat,p_age_cat,f_age_wide,p_age_wide,f_age_num,p_age_num,
                group_size,prop_old,prop_old_options) %>%
  rename(f_age = f_age_wide,
         n_age = p_age_wide,
         n_age_cat = p_age_cat,
         n_age_num = p_age_num) %>%
  mutate(n_old = ifelse(n_age == 'Y',0,1)) %>%
  mutate(prop_old_invl = LaplacesDemon::invlogit(prop_old_options))
str(neighbours)

#### set prior                                                       ####
get_prior(formula = n_old ~ 1 + f_age + stim_type * bda +
            (1|focal) + (1|stim_num) + (1|pb_num),
          data = neighbours, family = bernoulli("logit"))

priors <- c(
  # focal age
  prior(normal(0,1),          class = b,  coef = f_ageY),
  # stim type
  prior(normal(0,1),          class = b,  coef = stim_typeh),
  prior(normal(0,1),          class = b,  coef = stim_typel),
  # before/during/after
  prior(normal(0,1),          class = b,  coef = bdabefore),
  prior(normal(0,1),          class = b,  coef = bdaduring),
  # interaction
  prior(normal(0,1),          class = b,  coef = stim_typeh:bdabefore),
  prior(normal(0,1),          class = b,  coef = stim_typeh:bdaduring),
  prior(normal(0,1),          class = b,  coef = stim_typel:bdabefore),
  prior(normal(0,1),          class = b,  coef = stim_typel:bdaduring),
  # random effects / intercepts
  prior(student_t(3, 0, 0.5), class = sd, group = focal),
  prior(student_t(3, 0, 0.5), class = sd, group = pb_num),
  prior(student_t(3, 0, 0.5), class = sd, group = stim_num),
  prior(student_t(3, 0, 1),   class = Intercept))

## prior predictive check
nbm_prior <- brm(
  formula = n_old ~ 1 + f_age + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = neighbours, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(nbm_prior)

#### fit model                                                       ####
nbm_fit <- brm(
  formula = n_old ~ 1 + f_age + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = neighbours, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 10))

save.image('nearest_neighbour/neighbour_predicttarget_noprop_run.RData')

## check model fit
load('nearest_neighbour/neighbour_predicttarget_noprop_run.RData')
(summary <- summary(nbm_fit))
#
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
                            effects = c('f_age','stim_type','bda'),
                            categorical = FALSE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
age_effect <- marg[[1]]
stim_effect <- marg[[2]]
bda_effect <- marg[[3]]

#### plot marginal effects                                           ####
# neighbour_labels <- c('neighbour young',
#                       'neighbour old')
# names(neighbour_labels) <- c(1:2)
# (focal_age_plot <- age_effect %>%
#     mutate(agecombo = paste0(focal_age,'-',neighbour_age)) %>%
#     ggplot()+
#     geom_errorbar(aes(x = focal_age,
#                       colour = focal_age,
#                       ymax = upper__, ymin = lower__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = focal_age,
#                    colour = focal_age,
#                    y = estimate__),
#                cex = 3)+
#     xlab(label = 'focal age category')+
#     ylab('probability of being nearest neighbours after dove stimulus')+
#     scale_colour_viridis_d(name = 'focal age:')+
#     facet_wrap(. ~ neighbour_age,
#                labeller = labeller(neighbour_age = neighbour_labels))+
#     theme(legend.direction = 'horizontal',
#           legend.position = 'bottom',
#           legend.box = 'vertical',
#           legend.spacing.x = unit(0.2, 'cm'),
#           legend.spacing.y = unit(2, 'mm'),
#           axis.title = element_text(size = 16),
#           axis.text.x = element_text(size = 12,
#                                      #angle = 70,
#                                      vjust = 0.5),
#           axis.text.y = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = focal_age_plot, filename = '../outputs/neighbour_binomial_model_bda/neighbour_predicttarget_noprop_marginaleffects_focalage.png',
#        device = 'png', width = 8.3, height = 5.8)

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
ggsave(plot = stim_plot, filename = '../outputs/neighbour_binomial_model_bda/neighbour_predicttarget_noprop_marginaleffects_stimtype.png', device = 'png',
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

save.image('nearest_neighbour/neighbour_predicttarget_noprop_run.RData')

## reset plotting
dev.off()


