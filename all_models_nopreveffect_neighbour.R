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
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')

## remove individuals where ages are unknown
behav <- behav %>%
  filter(!is.na(f_age_num))

######## nearest neighbour                                           ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelchecks.pdf')

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

#### new model: change age combo to reduce number of categories      ####
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

#### set prior                                                       ####
get_prior(formula = action ~ 1 + age_rel + stim_type * bda +
            (1|focal) + (1|stim_num) + (1|pb_num),
          data = nn, family = bernoulli("logit"))
#                prior     class                 coef    group resp dpar nlpar lb ub       source
#               (flat)         b                                                          default
#               (flat)         b            age_relOY                                (vectorized)
#               (flat)         b            age_relYO                                (vectorized)
#               (flat)         b            age_relYY                                (vectorized)
#               (flat)         b            bdabefore                                (vectorized)
#               (flat)         b            bdaduring                                (vectorized)
#               (flat)         b           stim_typeh                                (vectorized)
#               (flat)         b stim_typeh:bdabefore                                (vectorized)
#               (flat)         b stim_typeh:bdaduring                                (vectorized)
#               (flat)         b           stim_typel                                (vectorized)
#               (flat)         b stim_typel:bdabefore                                (vectorized)
#               (flat)         b stim_typel:bdaduring                                (vectorized)
# student_t(3, 0, 2.5) Intercept                                                          default
# student_t(3, 0, 2.5)        sd                                                0         default
# student_t(3, 0, 2.5)        sd                         focal                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept    focal                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                        pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept   pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                      stim_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept stim_num                  0    (vectorized)

priors <- c(
  # age combination
  prior(normal(0,1),          class = b,  coef = age_relOY),
  prior(normal(0,1),          class = b,  coef = age_relYO),
  prior(normal(0,1),          class = b,  coef = age_relYY),
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
num_chains <- 4
num_iter <- 2000
nbm_prior <- brm(
  formula = action ~ 1 + age_rel + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(nbm_prior)

#### fit model                                                       ####
nbm_fit <- brm(
  formula = action ~ 1 + age_rel + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  control = list(adapt_delta = 0.9,
                 max_treedepth = 10))

save.image('nearest_neighbour/neighbour_noprev_run.RData')

## check model fit
# load('nearest_neighbour/neighbour_noprev_run.RData')
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
                            effects = c('age_rel','stim_type','bda'),
                            categorical = FALSE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
age_effect <- marg[[1]]
stim_effect <- marg[[2]]
bda_effect <- marg[[3]]

#### plot marginal effects                                           ####
neighbour_labels <- c('neighbour age category 1',
                      'neighbour age category 2',
                      'neighbour age category 3',
                      'neighbour age category 4')
names(neighbour_labels) <- c(1:4)
(focal_age_plot <- age_effect %>%
    separate(col = age_rel, sep = '_', remove = F,
             into = c('focal_age','neighbour_age')) %>%
    mutate(agecombo = paste0(focal_age,'-',neighbour_age)) %>%
    ggplot()+
    geom_errorbar(aes(x = focal_age,
                      colour = focal_age,
                      ymax = upper__, ymin = lower__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = focal_age,
                   colour = focal_age,
                   y = estimate__),
               cex = 3)+
    xlab(label = 'focal age category')+
    ylab('probability of being nearest neighbours after dove stimulus')+
    scale_colour_viridis_d(name = 'focal age:')+
    facet_wrap(. ~ neighbour_age,
               labeller = labeller(neighbour_age = neighbour_labels))+
    theme(legend.direction = 'horizontal',
          legend.position = 'bottom',
          legend.box = 'vertical',
          legend.spacing.x = unit(0.2, 'cm'),
          legend.spacing.y = unit(2, 'mm'),
          axis.title = element_text(size = 16),
          axis.text.x = element_text(size = 12,
                                     #angle = 70,
                                     vjust = 0.5),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = focal_age_plot, filename = '../outputs/neighbour_binomial_model_bda/neighbour_noprev_marginaleffects_focalage.png',
       device = 'png', width = 8.3, height = 5.8)

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
ggsave(plot = stim_plot, filename = '../outputs/neighbour_binomial_model_bda/neighbour_noprev_marginaleffects_stimtype.png', device = 'png',
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

save.image('nearest_neighbour/neighbour_noprev_run.RData')

## reset plotting
dev.off()

#### predict from model                                              ####
# load('nearest_neighbour/neighbour_noprev_run.RData')
rm(list = ls()[! ls() %in% c('nbm_fit','nn','behav','num_iter','num_chains')])

pred <- posterior_epred(object = nbm_fit,
                        newdata = nn)
save.image('nearest_neighbour/neighbour_noprev_predictions.RData')

## convert to data frame
nn$data_row <- 1:nrow(nn)
predictions <- as.data.frame(pred)
colnames(predictions) <- 1:nrow(nn)
predictions <- predictions %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row', values_to = 'epred') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(nn, by = 'data_row')

save.image('nearest_neighbour/neighbour_noprev_predictions.RData')
rm(pred) ; gc()

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions                                                ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelpredictions.pdf')
# load('nearest_neighbour/neighbour_noprev_predictions.RData')

predictions %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('dove (control)', 'lion', 'human')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_violin(aes(x = f_age_cat,
                  y = epred,
                  colour = bda)) +
  facet_grid(p_age_cat ~ stim_type)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(colour = 'time relative to stimulus:',
       fill = 'time relative to stimulus:',
       x = 'age category of focal elephant',
       y = 'predicted probability')+
  theme(legend.position = 'bottom')
ggsave(path = '../outputs/neighbour_binomial_model_bda/',
       filename = 'neighbour_noprev_modelpredictions.png',
       device = 'png', height = 1200, width = 1600, units = 'px')

## reset plotting
dev.off()

#### calculate posterior contrasts from predictions                  ####
load('nearest_neighbour/neighbour_noprev_predictions.RData')
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelcontrasts.pdf')

## stim type                                                         ####
stim_new <- nn %>%
  dplyr::select(age_rel, stim_type, bda,
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(age_rel, bda,
                                                         focal, stim_num, pb_num))))

## redo predictions with different stimulus types: all doves
ctd_before <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'before')
ctd_before_mtx <- posterior_epred(object = nbm_fit, newdata = ctd_before)
colnames(ctd_before_mtx) <- ctd_before$unique_data_combo
ctd_before_mtx <- ctd_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

ctd_during <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'during')
ctd_during_mtx <- posterior_epred(object = nbm_fit, newdata = ctd_during)
colnames(ctd_during_mtx) <- ctd_during$unique_data_combo
ctd_during_mtx <- ctd_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

ctd_after <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'after')
ctd_after_mtx <- posterior_epred(object = nbm_fit, newdata = ctd_after)
colnames(ctd_after_mtx) <- ctd_after$unique_data_combo
ctd_after_mtx <- ctd_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all lions
lion_before <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'before')
lion_before_mtx <- posterior_epred(object = nbm_fit, newdata = lion_before)
colnames(lion_before_mtx) <- lion_before$unique_data_combo
lion_before_mtx <- lion_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

lion_during <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'during')
lion_during_mtx <- posterior_epred(object = nbm_fit, newdata = lion_during)
colnames(lion_during_mtx) <- lion_during$unique_data_combo
lion_during_mtx <- lion_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

lion_after <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'after')
lion_after_mtx <- posterior_epred(object = nbm_fit, newdata = lion_after)
colnames(lion_after_mtx) <- lion_after$unique_data_combo
lion_after_mtx <- lion_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all humans
human_before <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'before')
human_before_mtx <- posterior_epred(object = nbm_fit, newdata = human_before)
colnames(human_before_mtx) <- human_before$unique_data_combo
human_before_mtx <- human_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

human_during <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'during')
human_during_mtx <- posterior_epred(object = nbm_fit, newdata = human_during)
colnames(human_during_mtx) <- human_during$unique_data_combo
human_during_mtx <- human_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

human_after <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'after')
human_after_mtx <- posterior_epred(object = nbm_fit, newdata = human_after)
colnames(human_after_mtx) <- human_after$unique_data_combo
human_after_mtx <- human_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

save.image('nearest_neighbour/neighbour_noprev_stimuluscontrasts.RData')
# load('nearest_neighbour/neighbour_noprev_stimuluscontrasts.RData')

## calculate contrasts
ctd_vs_lion_before <- lion_before_mtx - ctd_before_mtx
ctd_vs_human_before <- human_before_mtx - ctd_before_mtx
lion_vs_human_before <- human_before_mtx - lion_before_mtx

ctd_vs_lion_during <- lion_during_mtx - ctd_during_mtx
ctd_vs_human_during <- human_during_mtx - ctd_during_mtx
lion_vs_human_during <- human_during_mtx - lion_during_mtx

ctd_vs_lion_after <- lion_after_mtx - ctd_after_mtx
ctd_vs_human_after <- human_after_mtx - ctd_after_mtx
lion_vs_human_after <- human_after_mtx - lion_after_mtx

ctd_before_vs_during <- ctd_during_mtx - ctd_before_mtx
ctd_before_vs_after  <- ctd_after_mtx - ctd_before_mtx
ctd_during_vs_after  <- ctd_after_mtx - ctd_during_mtx

lion_before_vs_during <- lion_during_mtx - lion_before_mtx
lion_before_vs_after  <- lion_after_mtx - lion_before_mtx
lion_during_vs_after  <- lion_after_mtx - lion_during_mtx

human_before_vs_during <- human_during_mtx - human_before_mtx
human_before_vs_after  <- human_after_mtx - human_before_mtx
human_during_vs_after  <- human_after_mtx - human_during_mtx

## summarise contrasts
contrasts <- nn %>%
  select(-stim_type) %>%
  mutate(ctd_vs_lion_before_mu = apply(ctd_vs_lion_before, 2, mean),
         ctd_vs_lion_before_sd = apply(ctd_vs_lion_before, 2, sd),
         ctd_vs_human_before_mu = apply(ctd_vs_human_before, 2, mean),
         ctd_vs_human_before_sd = apply(ctd_vs_human_before, 2, sd),
         lion_vs_human_before_mu = apply(lion_vs_human_before, 2, mean),
         lion_vs_human_before_sd = apply(lion_vs_human_before, 2, sd),
         
         ctd_vs_lion_during_mu = apply(ctd_vs_lion_during, 2, mean),
         ctd_vs_lion_during_sd = apply(ctd_vs_lion_during, 2, sd),
         ctd_vs_human_during_mu = apply(ctd_vs_human_during, 2, mean),
         ctd_vs_human_during_sd = apply(ctd_vs_human_during, 2, sd),
         lion_vs_human_during_mu = apply(lion_vs_human_during, 2, mean),
         lion_vs_human_during_sd = apply(lion_vs_human_during, 2, sd),
         
         ctd_vs_lion_after_mu = apply(ctd_vs_lion_after, 2, mean),
         ctd_vs_lion_after_sd = apply(ctd_vs_lion_after, 2, sd),
         ctd_vs_human_after_mu = apply(ctd_vs_human_after, 2, mean),
         ctd_vs_human_after_sd = apply(ctd_vs_human_after, 2, sd),
         lion_vs_human_after_mu = apply(lion_vs_human_after, 2, mean),
         lion_vs_human_after_sd = apply(lion_vs_human_after, 2, sd),
         
         ctd_before_vs_during_mu = apply(ctd_before_vs_during, 2, mean),
         ctd_before_vs_during_sd = apply(ctd_before_vs_during, 2, sd),
         ctd_before_vs_after_mu = apply(ctd_before_vs_after, 2, mean),
         ctd_before_vs_after_sd = apply(ctd_before_vs_after, 2, sd),
         ctd_during_vs_after_mu = apply(ctd_during_vs_after, 2, mean),
         ctd_during_vs_after_sd = apply(ctd_during_vs_after, 2, sd),
         
         lion_before_vs_during_mu = apply(lion_before_vs_during, 2, mean),
         lion_before_vs_during_sd = apply(lion_before_vs_during, 2, sd),
         lion_before_vs_after_mu = apply(lion_before_vs_after, 2, mean),
         lion_before_vs_after_sd = apply(lion_before_vs_after, 2, sd),
         lion_during_vs_after_mu = apply(lion_during_vs_after, 2, mean),
         lion_during_vs_after_sd = apply(lion_during_vs_after, 2, sd),
         
         human_before_vs_during_mu = apply(human_before_vs_during, 2, mean),
         human_before_vs_during_sd = apply(human_before_vs_during, 2, sd),
         human_before_vs_after_mu = apply(human_before_vs_after, 2, mean),
         human_before_vs_after_sd = apply(human_before_vs_after, 2, sd),
         human_during_vs_after_mu = apply(human_during_vs_after, 2, mean),
         human_during_vs_after_sd = apply(human_during_vs_after, 2, sd))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(ctd_vs_lion_before_mu,    ctd_vs_human_before_mu,  lion_vs_human_before_mu,
                        ctd_vs_lion_during_mu,    ctd_vs_human_during_mu,  lion_vs_human_during_mu,
                        ctd_vs_lion_after_mu,     ctd_vs_human_after_mu,   lion_vs_human_after_mu,
                        ctd_before_vs_during_mu,  ctd_before_vs_after_mu,  ctd_during_vs_after_mu,
                        lion_before_vs_during_mu, lion_before_vs_after_mu, lion_during_vs_after_mu,
                        human_before_vs_during_mu,human_before_vs_after_mu,human_during_vs_after_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('contrast','mu'),
           sep = -3, remove = T) %>%
  select(-mu,
         -ctd_vs_lion_before_sd,    -ctd_vs_human_before_sd,  -lion_vs_human_before_sd,
         -ctd_vs_lion_during_sd,    -ctd_vs_human_during_sd,  -lion_vs_human_during_sd,
         -ctd_vs_lion_after_sd,     -ctd_vs_human_after_sd,   -lion_vs_human_after_sd,
         -ctd_before_vs_during_sd,  -ctd_before_vs_after_sd,  -ctd_during_vs_after_sd,
         -lion_before_vs_during_sd, -lion_before_vs_after_sd, -lion_during_vs_after_sd,
         -human_before_vs_during_sd,-human_before_vs_after_sd,-human_during_vs_after_sd)

## produce values for reporting
print('dove vs lion -- before')
mean(ctd_vs_lion_before)  ; sd(ctd_vs_lion_before)
quantile(ctd_vs_lion_before, prob = c(0.025, 0.5, 0.975))
length(which(ctd_vs_lion_before < 0)) / length(ctd_vs_lion_before)
# "dove vs lion -- before"
# 0.04187171
# 0.1646033
#       2.5%         50%       97.5%
# -0.28625580  0.02063874  0.40781070
# 0.3975
print('dove vs human -- before')
mean(ctd_vs_human_before) ; sd(ctd_vs_human_before)
quantile(ctd_vs_human_before, prob = c(0.025, 0.5, 0.975))
length(which(ctd_vs_human_before < 0)) / length(ctd_vs_human_before)
# "dove vs human -- before"
# -0.03836684
# 0.139189
#       2.5%         50%       97.5%
# -0.35991103 -0.01788765  0.24864070
# 0.6275
print('lion vs human -- before')
mean(lion_vs_human_before); sd(lion_vs_human_before)
quantile(lion_vs_human_before, prob = c(0.025, 0.5, 0.975))
length(which(lion_vs_human_before < 0)) / length(lion_vs_human_before)
# "lion vs human -- before"
# -0.08023855
# 0.2015114
#       2.5%         50%       97.5%
# -0.52425782 -0.05627844  0.31690555
# 0.66

print('dove vs lion -- during')
mean(ctd_vs_lion_during)  ; sd(ctd_vs_lion_during)
quantile(ctd_vs_lion_during, prob = c(0.025, 0.5, 0.975))
length(which(ctd_vs_lion_during < 0)) / length(ctd_vs_lion_during)
# "dove vs lion -- during"
# 0.04816623
# 0.1658911
#       2.5%         50%       97.5%
# -0.27969139  0.02570841  0.41296871
# 0.39
print('dove vs human -- during')
mean(ctd_vs_human_during) ; sd(ctd_vs_human_during)
quantile(ctd_vs_human_during, prob = c(0.025, 0.5, 0.975))
length(which(ctd_vs_human_during < 0)) / length(ctd_vs_human_during)
# "dove vs human -- during"
# -0.02656977
# 0.1391156
#       2.5%         50%       97.5%
# -0.34542515 -0.00704075  0.26326545
# 0.5825
print('lion vs human -- during')
mean(lion_vs_human_during); sd(lion_vs_human_during)
quantile(lion_vs_human_during, prob = c(0.025, 0.5, 0.975))
length(which(lion_vs_human_during < 0)) / length(lion_vs_human_during)
# "lion vs human -- during"
# -0.07473599
# 0.2021193
#       2.5%         50%       97.5%
# -0.51975652 -0.04987603  0.32660870
# 0.645

print('dove vs lion -- after')
mean(ctd_vs_lion_after)  ; sd(ctd_vs_lion_after)
quantile(ctd_vs_lion_after, prob = c(0.025, 0.5, 0.975))
length(which(ctd_vs_lion_after < 0)) / length(ctd_vs_lion_after)
# "dove vs lion -- after"
# 0.05580762
# 0.1644989
#       2.5%        50%      97.5%
# -0.2663957  0.0329688  0.4234324
# 0.375
print('dove vs human -- after')
mean(ctd_vs_human_after) ; sd(ctd_vs_human_after)
quantile(ctd_vs_human_after, prob = c(0.025, 0.5, 0.975))
length(which(ctd_vs_human_after < 0)) / length(ctd_vs_human_after)
# "dove vs human -- after"
# -0.03313247
# 0.1355853
#       2.5%         50%       97.5%
# -0.34888248 -0.01172908  0.25037198
# 0.61
print('lion vs human -- after')
mean(lion_vs_human_after); sd(lion_vs_human_after)
quantile(lion_vs_human_after, prob = c(0.025, 0.5, 0.975))
length(which(lion_vs_human_after < 0)) / length(lion_vs_human_after)
# "lion vs human -- after"
# -0.08894009
# 0.2008635
#       2.5%         50%       97.5%
# -0.53280077 -0.06397704  0.29852496
# 0.675

print('before vs during -- dove')
mean(ctd_before_vs_during)  ; sd(ctd_before_vs_during)
quantile(ctd_before_vs_during, prob = c(0.025, 0.5, 0.975))
length(which(ctd_before_vs_during < 0)) / length(ctd_before_vs_during)
# "before vs during -- dove"
# -0.007144311
# 0.006931377
#       2.5%          50%        97.5%
# -0.022563044 -0.006344822  0.004046757
# 0.9
print('before vs after -- dove')
mean(ctd_before_vs_after) ; sd(ctd_before_vs_after)
quantile(ctd_before_vs_after, prob = c(0.025, 0.5, 0.975))
length(which(ctd_before_vs_after < 0)) / length(ctd_before_vs_after)
# "before vs after -- dove"
# -0.01599424
# 0.008352686
#       2.5%           50%         97.5%
# -3.052482e-02 -1.675449e-02 -3.615598e-06
# 1
print('during vs after -- dove')
mean(ctd_during_vs_after); sd(ctd_during_vs_after)
quantile(ctd_during_vs_after, prob = c(0.025, 0.5, 0.975))
length(which(ctd_during_vs_after < 0)) / length(ctd_during_vs_after)
# "during vs after -- dove"
# -0.008849927
# 0.007447034
#       2.5%          50%        97.5%
# -0.025644424 -0.007855470  0.001244932
# 0.9475

print('before vs during -- lion')
mean(lion_before_vs_during)  ; sd(lion_before_vs_during)
quantile(lion_before_vs_during, prob = c(0.025, 0.5, 0.975))
length(which(lion_before_vs_during < 0)) / length(lion_before_vs_during)
# "before vs during -- lion"
# -0.0008497944
# 0.01010667
#       2.5%           50%         97.5%
# -0.0221474779 -0.0001679366  0.0211417519
# 0.5625
print('before vs after -- lion')
mean(lion_before_vs_after) ; sd(lion_before_vs_after)
quantile(lion_before_vs_after, prob = c(0.025, 0.5, 0.975))
length(which(lion_before_vs_after < 0)) / length(lion_before_vs_after)
# "before vs after -- lion"
# -0.002058326
# 0.005292747
#       2.5%          50%        97.5%
# -0.013711763 -0.001268267  0.007903963
# 0.6725
print('during vs after -- lion')
mean(lion_during_vs_after); sd(lion_during_vs_after)
quantile(lion_during_vs_after, prob = c(0.025, 0.5, 0.975))
length(which(lion_during_vs_after < 0)) / length(lion_during_vs_after)
# "during vs after -- lion"
# -0.001208532
# 0.0104395
#       2.5%           50%         97.5%
# -0.0248083203 -0.0001162283  0.0200427477
# 0.5475

print('before vs during -- human')
mean(human_before_vs_during)  ; sd(human_before_vs_during)
quantile(human_before_vs_during, prob = c(0.025, 0.5, 0.975))
length(which(human_before_vs_during < 0)) / length(human_before_vs_during)
# "before vs during -- human"
# 0.004652758
# 0.006872247
#       2.5%          50%        97.5%
# -0.007048669  0.003157956  0.020053161
# 0.2175
print('before vs after -- human')
mean(human_before_vs_after) ; sd(human_before_vs_after)
quantile(human_before_vs_after, prob = c(0.025, 0.5, 0.975))
length(which(human_before_vs_after < 0)) / length(human_before_vs_after)
# "before vs after -- human"
# -0.01075987
# 0.007020287
#       2.5%           50%         97.5%
# -2.547327e-02 -1.059186e-02 -3.159262e-06
# 0.9999976
print('during vs after -- human')
mean(human_during_vs_after); sd(human_during_vs_after)
quantile(human_during_vs_after, prob = c(0.025, 0.5, 0.975))
length(which(human_during_vs_after < 0)) / length(human_during_vs_after)
# "during vs after -- human"
# -0.01541263
# 0.0105628
#       2.5%           50%         97.5%
# -3.844217e-02 -1.473567e-02 -1.551257e-06
# 0.9924976

## plot contrasts
contrasts_long %>%
  mutate(contrast = case_when(contrast == 'ctd_vs_human_before' ~ 'dove -> human',
                              contrast == 'ctd_vs_human_during' ~ 'dove -> human',
                              contrast == 'ctd_vs_human_after'  ~ 'dove -> human',
                              
                              contrast == 'ctd_vs_lion_before' ~ 'dove -> lion',
                              contrast == 'ctd_vs_lion_during' ~ 'dove -> lion',
                              contrast == 'ctd_vs_lion_after'  ~ 'dove -> lion',
                              
                              contrast == 'lion_vs_human_before' ~ 'lion -> human',
                              contrast == 'lion_vs_human_during' ~ 'lion -> human',
                              contrast == 'lion_vs_human_after'  ~ 'lion -> human',
                              
                              contrast == 'ctd_before_vs_during'   ~ 'before -> after',
                              contrast == 'lion_before_vs_during'  ~ 'before -> after',
                              contrast == 'human_before_vs_during' ~ 'before -> after',
                              
                              contrast == 'ctd_before_vs_after'   ~ 'before -> after',
                              contrast == 'lion_before_vs_after'  ~ 'before -> after',
                              contrast == 'human_before_vs_after' ~ 'before -> after',
                              
                              contrast == 'ctd_after_vs_during'   ~ 'before -> after',
                              contrast == 'lion_after_vs_during'  ~ 'before -> after',
                              contrast == 'human_after_vs_during' ~ 'before -> after')) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = contrast))+
  scale_colour_viridis_d()+
  labs(colour = 'effect of changing stimulus')

save.image('nearest_neighbour/neighbour_noprev_stimuluscontrasts.RData')

## age combo                                                         ####
# load('nearest_neighbour/neighbour_noprev_stimuluscontrasts.RData')
rm(list = ls()[!ls() %in% c('nbm_fit','nn','behav','num_iter','num_chains')]) ; gc()

## create new dataframe to predict from
age_new <- nn %>%
  dplyr::select(age_rel, stim_type, bda,
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(age_rel, bda,
                                                         focal, stim_num, pb_num))))

## predict with original ages
age_nn_org <- age_new
age_mtx_org <- posterior_epred(object = nbm_fit, newdata = age_nn_org)
colnames(age_mtx_org) <- age_nn_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with altered age combinations
age_predict <- function(df, age_combination, model){
  df_new <- df %>%
    mutate(age_rel = age_combination)
  mtx <- posterior_epred(object = model, newdata = df_new)
  colnames(mtx) <- df_new$unique_data_combo
  mtx <- mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
  return(mtx)
}
mtx_OO <- age_predict(df = age_new, age_combination = 'OO', model = nbm_fit)
mtx_OY <- age_predict(df = age_new, age_combination = 'OY', model = nbm_fit)
mtx_YO <- age_predict(df = age_new, age_combination = 'YO', model = nbm_fit)
mtx_YY <- age_predict(df = age_new, age_combination = 'YY', model = nbm_fit)

save.image('nearest_neighbour/neighbour_noprev_agecontrasts.RData')

## calculate contrasts
contrast_YYvYO <- mtx_YY - mtx_YO
contrast_YYvOY <- mtx_YY - mtx_OY
contrast_YYvOO <- mtx_YY - mtx_OO
contrast_YOvOY <- mtx_YO - mtx_OY
contrast_YOvOO <- mtx_YO - mtx_OO
contrast_OYvOO <- mtx_OY - mtx_OO

## summarise contrasts
contrasts <- nn %>%
  select(-stim_type) %>%
  mutate(mu_YYvYO = apply(contrast_YYvYO, 2, mean),
         sd_YYvYO = apply(contrast_YYvYO, 2, sd),
         mu_YYvOY = apply(contrast_YYvYO, 2, mean),
         sd_YYvOY = apply(contrast_YYvYO, 2, sd),
         mu_YYvOO = apply(contrast_YYvOO, 2, mean),
         sd_YYvOO = apply(contrast_YYvOO, 2, sd),
         
         mu_YOvOY = apply(contrast_YOvOY, 2, mean),
         sd_YOvOY = apply(contrast_YOvOY, 2, sd),
         mu_YOvOO = apply(contrast_YOvOO, 2, mean),
         sd_YOvOO = apply(contrast_YOvOO, 2, sd),
         mu_OYvOO = apply(contrast_OYvOO, 2, mean),
         sd_OYvOO = apply(contrast_OYvOO, 2, sd)
  )

contrasts_long <- contrasts %>%
  pivot_longer(cols = c(mu_YYvYO,mu_YYvOY,mu_YYvOO,
                        mu_YOvOY,mu_YOvOO,
                        mu_OYvOO),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('mu','combo1','v','combo2'),
           sep = c(3,5,6), remove = T) %>%
  select(-sd_YYvYO,-sd_YYvOY,-sd_YYvOO,
         -sd_YOvOY,-sd_YOvOO,
         -sd_OYvOO,
         -mu, -v)

## save contrasts
save.image('nearest_neighbour/neighbour_noprev_agecontrasts.RData')

## produce values for reporting
print('YY vs YO')
mean(contrast_YYvYO); sd(contrast_YYvYO)
quantile(contrast_YYvYO, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YYvYO < 0)) / length(contrast_YYvYO)
# "YY vs YO"
# 0.03064035
# 0.0138561
#       2.5%          50%        97.5%
# 7.001155e-06 3.193930e-02 5.311109e-02
# 0
print('YY vs OY')
mean(contrast_YYvOY); sd(contrast_YYvOY)
quantile(contrast_YYvOY, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YYvOY < 0)) / length(contrast_YYvOY)
# "YY vs OY"
# -0.02389598
# 0.02875253
#       2.5%         50%       97.5%
# -0.08886406 -0.02035392  0.02466073
# 0.8449955
print('YY vs OO')
mean(contrast_YYvOO); sd(contrast_YYvOO)
quantile(contrast_YYvOO, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YYvOO < 0)) / length(contrast_YYvOO)
# "YY vs OO"
# 0.0389974
# 0.03066436
#       2.5%          50%        97.5%
# -0.009041779  0.037665009  0.105607743
# 0.055
print('YO vs OY')
mean(contrast_YOvOY); sd(contrast_YOvOY)
quantile(contrast_YOvOY, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YOvOY < 0)) / length(contrast_YOvOY)
# "YO vs OY"
# -0.05453633
# 0.03452563
#       2.5%           50%         97.5%
# -1.284194e-01 -5.357089e-02 -8.449191e-07
# 0.985
print('YO vs OO')
mean(contrast_YOvOO); sd(contrast_YOvOO)
quantile(contrast_YOvOO, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YOvOO < 0)) / length(contrast_YOvOO)
# "YO vs OO"
# 0.008357046
# 0.02480767
#       2.5%          50%        97.5%
# -0.041190534  0.006154465  0.061774433
# 0.3575
print('OY vs OO')
mean(contrast_OYvOO); sd(contrast_OYvOO)
quantile(contrast_OYvOO, prob = c(0.025, 0.5, 0.975))
length(which(contrast_OYvOO < 0)) / length(contrast_OYvOO)
# "OY vs OO"
# 0.06289338
# 0.02680199
#       2.5%          50%        97.5%
# 1.381864e-05 6.573617e-02 9.876693e-02
# 0

## plot contrasts
(colour_plot <- contrasts_long %>%
    mutate(contrast = paste0(combo1, ' -> ', combo2)) %>%
    mutate(contrast = factor(contrast,
                             levels = c('YY -> YO','YY -> OY','YY -> OO',
                                        'YO -> OY','YO -> OO',
                                        'OY -> OO'))) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = contrast, fill = contrast),
                 alpha = 0.4)+
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    #scale_x_continuous(limits = c(-0.02,0.02))+ # UNCOMMENT THIS ONE IN FINAL GRAPHS, BUT FIRST NEED TO SEE WHAT THE FULL RANGE IS BEFORE CONSTRAINING IT!
    labs(colour = 'difference in\nage category',
         fill = 'difference in\nage category',
         title = 'changing focal age',
         x = 'contrast'))
ggsave(plot = colour_plot, device = 'png',
       filename = 'nbm_noprev_focalage_contrasts.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       width = 2200, height = 2400, units = 'px')
ggsave(plot = colour_plot, device = 'svg',
       filename = 'nbm_noprev_focalage_contrasts.svg',
       path = '../outputs/neighbour_binomial_model_bda/',
       width = 2200, height = 2400, units = 'px')
(facet_plot <- contrasts_long %>%
    mutate(contrast = paste0(combo1, ' -> ', combo2)) %>%
    mutate(contrast = factor(contrast,
                             levels = c('YY -> YO','YY -> OY','YY -> OO',
                                        'YO -> OY','YO -> OO',
                                        'OY -> OO'))) %>%
    ggplot()+
    geom_density(aes(x = difference),
                 alpha = 0.4)+
    #scale_x_continuous(limits = c(-0.0125,0.025))+ # UNCOMMENT THIS ONE IN FINAL GRAPHS, BUT FIRST NEED TO SEE WHAT THE FULL RANGE IS BEFORE CONSTRAINING IT!
    facet_wrap(contrast ~ ., scales = 'free_y', ncol = 4)+
    labs(colour = 'difference in\nage category',
         fill = 'difference in\nage category',
         title = 'changing partner age',
         x = 'contrast'))
ggsave(plot = facet_plot, device = 'png',
       filename = 'nbm_noprev_partnerage_contrasts.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       width = 2200, height = 2400, units = 'px')
ggsave(plot = facet_plot, device = 'svg',
       filename = 'nbm_noprev_partnerage_contrasts.svg',
       path = '../outputs/neighbour_binomial_model_bda/',
       width = 2200, height = 2400, units = 'px')

save.image('nearest_neighbour/neighbour_noprev_agecontrasts.RData')

######## create new model to compare to null model                   ########
rm(list = ls()) ; gc()
load('nearest_neighbour/neighbour_noprev_predictions.RData')
ls()

#### extract random effect values for offsets                        ####
random <- as_draws_df(nbm_fit) %>%
  dplyr::select(-b_Intercept, -b_age_relOY, -b_age_relYO, -b_age_relYY,
                -b_stim_typeh, -b_stim_typel, -b_bdabefore, -b_bdaduring,
                -`b_stim_typeh:bdabefore`, -`b_stim_typel:bdabefore`,
                -`b_stim_typeh:bdaduring`, -`b_stim_typel:bdaduring`,
                -lprior, -`lp__`)
colnames(random)
stim_num <- random %>%
  dplyr::select(grep(pattern = 'stim_num', x = colnames(random)),
                `.chain`, `.iteration`, `.draw`) %>%
  pivot_longer(cols = 1:(length(unique(nn$stim_num)) + 1),
               names_to = 'parameter',
               values_to = 'mcmc')
rand_stim <- stim_num %>%
  group_by(parameter) %>%
  summarise(mean_stim = mean(mcmc),
            sd_stim = sd(mcmc)) %>%
  separate(parameter, into = c('r_stim','stim_intercept'),
           remove = F, sep = 11) %>%
  separate(stim_intercept, into = c('stim_num','intercept'),
           remove = T, sep = ',') %>%
  dplyr::select(-intercept)

pb_num <- random %>%
  dplyr::select(grep(pattern = 'pb_num', x = colnames(random)),
                `.chain`, `.iteration`, `.draw`) %>%
  pivot_longer(cols = 1:(length(unique(nn$pb_num)) + 1),
               names_to = 'parameter',
               values_to = 'mcmc')
rand_pb <- pb_num %>%
  group_by(parameter) %>%
  summarise(mean_pb = mean(mcmc),
            sd_pb = sd(mcmc)) %>%
  separate(parameter, into = c('r_pb','pb_intercept'),
           remove = F, sep = 9) %>%
  separate(pb_intercept, into = c('pb_num','intercept'),
           remove = T, sep = ',') %>%
  dplyr::select(-intercept)

focal <- random %>%
  dplyr::select(grep(pattern = 'focal', x = colnames(random)),
                `.chain`, `.iteration`, `.draw`) %>%
  pivot_longer(cols = 1:(length(unique(nn$focal)) + 1),
               names_to = 'parameter',
               values_to = 'mcmc')
rand_focal <- focal %>%
  group_by(parameter) %>%
  summarise(mean_focal = mean(mcmc),
            sd_focal = sd(mcmc)) %>%
  separate(parameter, into = c('r_focal','focal_intercept'),
           remove = F, sep = 8) %>%
  separate(focal_intercept, into = c('focal','intercept'),
           remove = T, sep = ',') %>%
  dplyr::select(-intercept)

## apply mean random effect values to data frame
nn <- nn %>%
  left_join(rand_stim[,c('stim_num','mean_stim','sd_stim')],
            by = 'stim_num') %>%
  mutate(pb_num = as.character(pb_num)) %>%
  left_join(rand_pb[,c('pb_num','mean_pb','sd_pb')],
            by = 'pb_num') %>%
  mutate(pb_num = as.numeric(pb_num)) %>%
  left_join(rand_focal[,c('focal','mean_focal','sd_focal')],
            by = 'focal')

#### reset priors (same as before but remove random effects)         ####
priors2 <- c(
  # age combination
  prior(normal(0,1), class = b,  coef = age_relOY),
  prior(normal(0,1), class = b,  coef = age_relYO),
  prior(normal(0,1), class = b,  coef = age_relYY),
  # stim type
  prior(normal(0,1), class = b,  coef = stim_typeh),
  prior(normal(0,1), class = b,  coef = stim_typel),
  # before/during/after
  prior(normal(0,1), class = b,  coef = bdabefore),
  prior(normal(0,1), class = b,  coef = bdaduring),
  # interaction
  prior(normal(0,1), class = b,  coef = stim_typeh:bdabefore),
  prior(normal(0,1), class = b,  coef = stim_typeh:bdaduring),
  prior(normal(0,1), class = b,  coef = stim_typel:bdabefore),
  prior(normal(0,1), class = b,  coef = stim_typel:bdaduring))

#### refit model using offsets instead of random effects             ####
nbm2_fit <- brm(
  formula = action ~ 1 + age_rel + stim_type * bda +
    offset(mean_focal) + offset(mean_stim) + offset(mean_pb),
  data = nn, family = bernoulli("logit"),
  prior = priors2, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 10))

save.image('nearest_neighbour/neighbour_noprev_offset_fullmodel.RData')

(summary2 <- summary(nbm2_fit))
#
par(mfrow = c(3,1))
hist(summary2$fixed$Rhat, breaks = 50)
hist(summary2$fixed$Bulk_ESS, breaks = 50)
hist(summary2$fixed$Tail_ESS, breaks = 50)
par(mfrow = c(1,1))

## extract posterior distribution
draws2 <- as_draws_df(nbm2_fit) %>%
  select(-lprior, -`lp__`)
parameters2 <- colnames(draws2)[1:(ncol(draws2)-3)]
draws2 <- draws2  %>%
  pivot_longer(cols = all_of(parameters2),
               names_to = 'parameter',
               values_to = 'draw') %>%
  rename(chain = `.chain`,
         position = `.iteration`,
         draw_id = `.draw`) %>%
  mutate(invlogit_draw = invlogit(draw))

#### posterior predictive check                                      ####
pp_check(nbm2_fit, ndraws = 100)

#### plot traces                                                     ####
draws2 %>%
  ggplot(aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

#### plot density curves                                             ####
draws2 %>%
  ggplot(aes(x = draw, colour = as.factor(chain)))+
  geom_density()+
  facet_wrap(. ~ parameter, scales = 'free')+
  theme(legend.position = 'none')

save.image('nearest_neighbour/neighbour_noprev_offset_fullmodel.RData')

#### predict from model                                              ####
# load('nearest_neighbour/neighbour_noprev_offset_fullmodel.RData')
rm(list = ls()[! ls() %in% c('nbm2_fit','nn','behav','num_iter','num_chains')])

pred2 <- posterior_epred(object = nbm2_fit,
                         newdata = nn)
save.image('nearest_neighbour/neighbour_noprev_offset_fullmodel_predictions.RData')

## convert to data frame
predictions2 <- as.data.frame(pred2)
colnames(predictions2) <- 1:nrow(nn)
predictions2 <- predictions2 %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row', values_to = 'epred') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(nn, by = 'data_row')

save.image('nearest_neighbour/neighbour_noprev_offset_fullmodel_predictions.RData')
rm(pred2) ; gc()

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions                                                ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_offset_fullmodel_predictions.pdf')
# load('nearest_neighbour/neighbour_noprev_offset_fullmodel_predictions.RData')

predictions2 %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('dove (control)', 'lion', 'human')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_violin(aes(x = f_age_cat,
                  y = epred,
                  colour = bda)) +
  facet_grid(p_age_cat ~ stim_type)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(colour = 'time relative to stimulus:',
       fill = 'time relative to stimulus:',
       x = 'age category of focal elephant',
       y = 'predicted probability')+
  theme(legend.position = 'bottom')
ggsave(path = '../outputs/neighbour_binomial_model_bda/',
       filename = 'neighbour_noprev_offset_fullmodel_predictions.png',
       device = 'png', height = 1200, width = 1600, units = 'px')

######## compare to null                                             ########
rm(list = ls()[! ls() %in% c('predictions2','nn','num_chains','num_iter')]) ; gc()

load('nearest_neighbour/neighbour_noprev_predictions_null.RData')
rm(list = ls()[! ls() %in% c('predictions_null',
                             'predictions2','nn',
                             'num_chains','num_iter')]) ; gc()

save.image('nearest_neighbour/neighbour_noprev_offset_nullcompare.RData')
# load('nearest_neighbour/neighbour_noprev_offset_nullcompare.RData')

predictions_null$model_type <- 'null'
predictions2$model_type <- 'full'

pred_all <- rbind(predictions2,predictions_null)
save.image('nearest_neighbour/neighbour_noprev_offset_nullcompare.RData')
# load('nearest_neighbour/neighbour_noprev_nullcompare.RData')

pred_all <- pred_all %>%
  mutate(iteration = rep(rep(1:num_iter, each = nrow(nn)), (num_chains*2) ) )

mid <- sample(x = (1:(num_chains*num_iter)), size = 1000, replace = F)
pred_all <- pred_all %>%
  filter(iteration %in% mid)

gc()
save.image('nearest_neighbour/neighbour_noprev_nullcompare_offset_smallfiles.RData')
# load('nearest_neighbour/neighbour_noprev_nullcompare_smallfiles.RData')
predictions <- pred_all %>% 
  filter(model_type == 'live')

predictions_null <- pred_all %>% 
  filter(model_type == 'null')

(null_lwr <- quantile(x = predictions_null$epred, probs = 0.025))
(null_upr <- quantile(x = predictions_null$epred, probs = 0.975))

(live_lwr <- quantile(x = predictions$epred, probs = 0.025))
(live_upr <- quantile(x = predictions$epred, probs = 0.975))

length(which(predictions$epred > null_lwr)) / nrow(predictions)
length(which(predictions$epred < null_upr)) / nrow(predictions)

length(which(predictions$epred > null_lwr & predictions$epred < null_upr)) / nrow(predictions)

(all <- pred_all %>% 
    mutate(model_type = ifelse(model_type == 'live',
                               'relative age + stimulus * time',
                               'stimulus * time'),
           stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                              ifelse(stim_type == 'l', 'lion', 'human')),
           f_age_cat = paste0('F: ',f_age_cat),
           p_age_cat = paste0('T: ',p_age_cat)) %>% 
    mutate(stim_type = factor(stim_type,
                              levels = c('dove (control)','lion','human')),
           bda = factor(bda, levels = c('before','during','after'))) %>% 
    ggplot()+
    geom_density(aes(x = epred,
                     fill = model_type,
                     colour = model_type),
                 alpha = 0.5)+
    scale_colour_viridis_d(end = 0.5)+
    scale_fill_viridis_d(end = 0.5)+
    labs(x = 'predicted probability of being neighbours',
         fill = 'model type',
         colour = 'model type'))
ggsave(plot = all, device = 'png',
       filename = 'compare_neighbour_null.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 1200, width = 1600, units = 'px')

(facet_stim <- all + 
    facet_grid(bda ~ stim_type)+
    theme(legend.position = 'bottom'))
ggsave(plot = facet_stim, device = 'png',
       filename = 'compare_neighbour_null_facetstim.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 1600, width = 2000, units = 'px')

(facet_age <- all + 
    facet_grid(p_age_cat ~ f_age_cat)+
    theme(legend.position = 'bottom'))
ggsave(plot = facet_age, device = 'png',
       filename = 'compare_neighbour_null_facetage.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 1600, width = 2400, units = 'px')

dev.off()
