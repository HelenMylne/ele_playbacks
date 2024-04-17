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

#### import data for both models ####
# https://dagitty.net/dags.html?id=dw8twK
# read in data
ages <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS') %>%
  select(focal, f_age_cat, f_age_num) %>%
  distinct() %>%
  filter(!is.na(f_age_cat)) %>%
  mutate(partner = focal,
         p_age_cat = f_age_cat,
         p_age_num = f_age_num)

stim_starts <- readRDS('../data_processed/stimuli.RDS') %>%
  filter(status == 'START' & behavior == 'STIMULUS') %>%
  select(pb_num,time,stim_num,stim_type,group_size,comment)
table(stim_starts$pb_num)
multiple_starts <- c(10, 24, 29, 32, 46, 53)
check <- stim_starts %>%
  filter(pb_num %in% multiple_starts) # for stim 10+29+46+53 take first time, for 24+32 use second.
for(i in multiple_starts){
  x <- check %>% filter(pb_num == i)
  check <- anti_join(check, x)
  if(i %in% c(10,29,46,53)){
    x <- x[1,]
  }
  if(i %in% c(24,32)){
    x <- x[2,]
  }
  check <- rbind(check, x)
}
stim_starts <- stim_starts %>%
  filter(! pb_num %in% multiple_starts) %>%
  rbind(check) %>%
  mutate(time = as.numeric(time)) %>%
  mutate(stim_start = round(time, 0)) %>%
  select(pb_num,stim_start,stim_num,stim_type,group_size)

## movement data
cols_of_interest <- c('b1_move','b2_move','b3_move','b4_move',
                      'b5_move','b6_move','b7_move','b8_move')
cols_of_interest_name <- c('b1_move_name','b2_move_name','b3_move_name','b4_move_name',
                           'b5_move_name','b6_move_name','b7_move_name','b8_move_name')
cols_of_interest_index <- c('b1_move_index','b2_move_index','b3_move_index','b4_move_index',
                            'b5_move_index','b6_move_index','b7_move_index','b8_move_index')
move <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
  filter(out_frame_name == 'in_frame') %>%
  select(subject,pb_num,second,
         all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
  rename(b1_move = b1_move_name, b2_move = b2_move_name,
         b3_move = b3_move_name, b4_move = b4_move_name,
         b5_move = b5_move_name, b6_move = b6_move_name,
         b7_move = b7_move_name, b8_move = b8_move_name) %>% 
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_name', values_to = 'moving_direction') %>% 
  rename(b1_move = b1_move_index, b2_move = b2_move_index,
         b3_move = b3_move_index, b4_move = b4_move_index,
         b5_move = b5_move_index, b6_move = b6_move_index,
         b7_move = b7_move_index, b8_move = b8_move_index) %>% 
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_index', values_to = 'move_index') %>% 
  filter(elephant_activity_name == elephant_activity_index) %>% 
  select(-elephant_activity_index) %>% 
  rename(elephant_activity = elephant_activity_name,
         focal = subject) %>% 
  filter(is.na(move_index) == FALSE) %>% 
  separate(elephant_activity, into = c('partner','activity'), sep = '_', remove = T) %>% 
  mutate(partner = paste0(partner, '_e', pb_num),
         pb_num = as.numeric(pb_num)) %>% 
  left_join(ages[,c('focal','f_age_cat','f_age_num')], by = 'focal') %>% 
  left_join(ages[,c('partner','p_age_cat','p_age_num')], by = 'partner') %>% 
  left_join(stim_starts, by = 'pb_num') %>% 
  mutate(time_since_stim = second - stim_start,
         after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60),
         age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num),
                                 'partner_younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num),
                                        'matched',
                                        'partner_older'))) %>%
  select(pb_num,focal,partner,
         activity,moving_direction,move_index,
         stim_num,stim_type,
         time_since_stim, after_stim,
         f_age_cat,p_age_cat,f_age_num,p_age_num,
         age_difference) %>% 
  mutate(f_age_num = as.factor(f_age_num),
         p_age_num = as.factor(p_age_num),
         age_combo = paste0(f_age_num,'_',p_age_num),
         move_tminus1 = NA,
         move_tminus1_num = NA)
rm(list = ls() [ ! ls() %in% 'move']) ; gc()

unique(move$focal[is.na(move$f_age_num)])   # b6_e7 = unknown age
unique(move$partner[is.na(move$p_age_num)]) # b2_e13 + b2_e34 = unknown age
length(which(is.na(move$moving_direction) == TRUE))

## remove all records of b2_e34 as never visible
move <- move %>%
  filter(focal != 'b2_e34') %>% 
  filter(partner != 'b2_e34')

# create variable for moving_direction at time t-1
focals <- unique(move$focal)
for(f in 1:length(focals)){
  focal <- move %>% filter(focal == focals[f])
  move <- move %>% anti_join(focal, by = 'focal')
  partners <- unique(focal$partner)
  for(p in 1:length(partners)){
    focal_partner <- focal %>% filter(partner == partners[p])
    focal <- focal %>% anti_join(focal_partner, by = 'partner')
    for(i in 2:nrow(focal_partner)){
      focal_partner$move_tminus1[i] <- focal_partner$moving_direction[i-1]
      focal_partner$move_tminus1_num[i] <- focal_partner$move_index[i-1]
    }
    focal <- rbind(focal, focal_partner)
  }
  move <- rbind(move, focal)
}
rm(list = ls()[ ! ls() %in% c('move', 'focals')]) ; gc()

############### Probability of moving ###############
pdf('../outputs/movement_binomial_model/movement_binomial_modelprep.pdf')

#### filter data ####
move_no_na <- move %>%
  filter(is.na(f_age_num) == FALSE) %>%
  filter(is.na(move_tminus1) == FALSE) %>%
  mutate(move_index = ifelse(move_index == 0, 0, 1),
         moving_direction = ifelse(moving_direction == 'not_moving',
                                   'not_moving', 'moving'),
         move_tminus1_num = ifelse(move_tminus1_num == 0, 0, 1),
         move_tminus1 = ifelse(move_tminus1 == 'not_moving',
                                   'not_moving', 'moving')) %>%
  mutate(f_age_num = as.integer(f_age_num)) %>%
  mutate(focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num) %>%
  select(focal, moving_direction, move_index,
         f_age_cat, f_age_num,
         time_since_stim, after_stim, stim_type,
         move_tminus1, move_tminus1_num,
         focal_id, stim_id, playback_id) %>%
  distinct()
str(move_no_na)

#### set priors ####
# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
get_prior(formula = move_index ~ 0 + mo(f_age_num) + stim_type +   # fixed effects
            s(after_stim) + move_tminus1_num +                     # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),          # random effects
          data = move_no_na,
          family = bernoulli("logit"))

# centre on -1 (logit(0.25) approx. = -1, and more likely to be not moving than moving)
priors <- c(
  # focal age
  prior(normal(-1,1),     class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(-1,1),     class = b,    coef = stim_typectd),
  prior(normal(-1,1),     class = b,    coef = stim_typel),
  prior(normal(-1,1),     class = b,    coef = stim_typeh),
  # time spline
  prior(normal(-1,1),     class = b,    coef = safter_stim_1),
  # action in previous second
  prior(normal(-1,1),     class = b,    coef = move_tminus1_num))

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
mbm_prior <- brm(
  formula = move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(mbm_prior)

print(paste0('priors set and checked at ', Sys.time()))

## reset plotting
dev.off()
pdf('../outputs/movement_binomial_model/movement_binomial_modelchecks.pdf')

#### fit model ####
direction_move_fit <- brm(
  formula = move_index ~ 0 + mo(f_age_num) + stim_type + s(after_stim) + move_tminus1_num +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# save workspace
save.image('movement_direction/movement_binomial_run.RData') # save.image('ele_playbacks/movement_direction/movement_binomial_run.RData')

# inspect model
summary(direction_move_fit)
print(paste0('model fitted at ', Sys.time()))

#### check outputs ####
load('movement_direction/movement_binomial_run.RData') # load('ele_playbacks/movement_direction/movement_binomial_run.RData')

## check Stan code
direction_move_fit$model
direction_move_fit$formula

## extract posterior distribution
draws <- as_draws_df(direction_move_fit) %>%
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

print(paste0('posterior extracted at ',Sys.time()))

#### calculate log cumulative odds ####
(prop <- table(move_no_na$moving_direction) / nrow(move_no_na))
(cum_prop <- cumsum(prop))
(log_cum_odds <- logit(cum_prop))

#### plot marginal effects ####
## extract marginal effects
marg <- conditional_effects(direction_move_fit,
                            effects = c('f_age_num', 'stim_type', 'after_stim', 'move_tminus1_num'),
                            #categorical = TRUE,
                            method = 'posterior_epred')
names(marg)
agefocal_effect <- marg[[1]]
stim_effect <- marg[[2]]
time_effect <- marg[[3]]
prevsec_effect <- marg[[4]]

## plot marginal effects
(focal_age_plot <- ggplot(agefocal_effect)+
    geom_errorbar(aes(x = f_age_num,
                      #colour = cats__,
                      ymax = upper__, ymin = lower__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = f_age_num,
                   #colour = cats__,
                   y = estimate__),
               size = 3)+ # cex = 3?
    xlab(label = 'focal age')+
    ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('approach directly',
                                      'approach at an angle',
                                      'side on',
                                      'move away at an angle',
                                      'move away directly'))+
    scale_fill_viridis_d(name = 'movement direction:',
                         breaks = c('1','2','3','4','5'),
                         labels = c('approach directly',
                                    'approach at an angle',
                                    'side on',
                                    'move away at an angle',
                                    'move away directly'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)))
ggsave(plot = focal_age_plot, filename = '../outputs/movement_binomial_model/movement_binomial_marginaleffects_focalage.png', device = 'png',
       width = 8.3, height = 5.8)

focal_age_labels <- c('focal age category 1',
                      'focal age category 2',
                      'focal age category 3',
                      'focal age category 4')
names(focal_age_labels) <- 1:4

(stim_plot <- ggplot(stim_effect)+
    geom_errorbar(aes(x = stim_type,
                      #colour = cats__,
                      ymin = lower__, ymax = upper__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type,
                   #colour = cats__,
                   y = estimate__),
               cex = 3)+ # size = 3?
    xlab(label = 'stimulus type') + ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('approach directly',
                                      'approach at an angle',
                                      'side on',
                                      'move away at an angle',
                                      'move away directly'))+
    scale_x_discrete(breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = stim_plot, filename = '../outputs/movement_binomial_model/movement_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = '../outputs/movement_binomial_model/movement_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(direction_move_fit, ndraws = 100) # really good fit

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_stim_typectd","b_stim_typeh","b_stim_typel",
                          "b_move_tminus1_num","bsp_mof_age_num",
                          "bs_safter_stim_1","sds_safter_stim_1",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing doesn't move brilliant, esp. for bsp_mofocal_age, but only horrendous one is playback ID

## stim type
dove <- draws_cut %>%
  filter(parameter == 'b_stim_typectd') %>%
  rename(dove = draw) %>%
  select(-parameter)
lion <- draws_cut %>%
  filter(parameter == 'b_stim_typel') %>%
  rename(lion = draw) %>%
  select(-parameter)
human <- draws_cut %>%
  filter(parameter == 'b_stim_typeh') %>%
  rename(human = draw) %>%
  select(-parameter)

dove_lion <- dove %>%
  left_join(lion, by = c('chain','position','draw_id')) %>%
  mutate(difference = lion - dove)
dove_human <- dove %>%
  left_join(human, by = c('chain','position','draw_id')) %>%
  mutate(difference = human - dove)
lion_human <- lion %>%
  left_join(human, by = c('chain','position','draw_id')) %>%
  mutate(difference = human - lion)

par(mfrow = c(3,1))
plot(density(dove_lion$difference), main = 'lion vs dove') ; abline(v = 0, lty = 2)
plot(density(dove_human$difference), main = 'lion vs human') ; abline(v = 0, lty = 2)
plot(density(lion_human$difference), main = 'human vs lion') ; abline(v = 0, lty = 2)
par(mfrow = c(1,1))

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

## movement direction in previous second
prevsec <- draws_cut %>% filter(parameter == 'b_move_tminus1_num')
plot(density(prevsec$draw), main = 't-1 slope') ; abline(v = 0, lty = 2)

# ## time since stimulus -- come back to this!
# timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
# times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
# time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
# time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
# time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
# time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
# time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
# time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
# time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
# time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
# par(mfrow = c(5,2))
# plot(density(timeb$draw), main = 'time slope') ; abline(v = 0, lty = 2)
# plot(density(times$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
# plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
# plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
# plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
# plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
# plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
# plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
# plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
# plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
save.image('movement_direction/movement_binomial_run.RData')

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
move_no_na %>%
  ggplot(aes(x = after_stim, y = move_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ stim_type,
             labeller = labeller(f_age_num = age_labels,
                                 stim_type = stim_labels))+
  scale_x_continuous(name = 'time since stimulus started')
print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('movement_direction/movement_binomial_run.RData') # save.image('ele_playbacks/movement_direction/movement_binomial_run.RData')
dev.off()

#### predict from model ####
pdf('../outputs/movement_binomial_model/movement_binomial_modelpredictions.pdf')
#load('movement_direction/movement_binomial_run.RData')
rm(list = ls()[! ls() %in% c('direction_move_fit','move_no_na')]) ; gc()

pred <- posterior_epred(object = direction_move_fit,
                        newdata = move_no_na)
save.image('movement_direction/movement_binomial_predictions.RData')

rm(list = ls()) ; gc()

############# Probability of different directions once moving ###############
pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelprep.pdf')

#### filter data ####
move_no_na <- move %>%
  filter(moving_direction != 'not_moving') %>%
  filter(move_tminus1 != 'not_moving') %>%
  filter(is.na(f_age_num) == FALSE) %>%
  filter(is.na(p_age_num) == FALSE) %>%
  filter(is.na(age_difference) == FALSE) %>%
  filter(is.na(move_tminus1) == FALSE) %>%
  mutate(age_difference = factor(age_difference,
                                 levels = c('partner_younger',
                                            'matched',
                                            'partner_older')),
         move_index = ifelse(moving_direction == 'approach directly', 1,
                             ifelse(moving_direction == 'approach at an angle', 2,
                                    ifelse(moving_direction == 'move directly with', 3,
                                           ifelse(moving_direction == 'move away at an angle', 4,
                                                  5)))),
         move_tminus1_num = ifelse(move_tminus1 == 'approach directly', 1,
                                   ifelse(move_tminus1 == 'approach at an angle', 2,
                                          ifelse(move_tminus1 == 'move directly with', 3,
                                                 ifelse(move_tminus1 == 'move away at an angle', 4,
                                                        5))))) %>%
  mutate(age_diff_num = as.integer(age_difference),
         f_age_num = as.integer(f_age_num),
         p_age_num = as.integer(p_age_num)) %>%
  mutate(age_combo = paste0(f_age_num, '_', p_age_num),
         focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num) %>%
  select(focal, partner, moving_direction, move_index,
         f_age_cat, p_age_cat, f_age_num, p_age_num,
         age_difference, age_diff_num, age_combo,
         time_since_stim, after_stim, stim_type,
         move_tminus1, move_tminus1_num,
         focal_id, stim_id, playback_id)
str(move_no_na)

#### set priors ####
# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
get_prior(formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
            s(after_stim) + mo(move_tminus1_num) +                             # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),                      # random effects
          data = move_no_na,
          family = cumulative("logit"))

priors <- c(
  # focal age
  prior(normal(0,0.25),   class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # interaction age
  prior(normal(0,1),     class = b,    coef = age_combo1_2),
  prior(normal(0,1),     class = b,    coef = age_combo1_3),
  prior(normal(0,1),     class = b,    coef = age_combo1_4),
  prior(normal(0,1),     class = b,    coef = age_combo2_1),
  prior(normal(0,1),     class = b,    coef = age_combo2_2),
  prior(normal(0,1),     class = b,    coef = age_combo2_3),
  prior(normal(0,1),     class = b,    coef = age_combo2_4),
  prior(normal(0,1),     class = b,    coef = age_combo3_1),
  prior(normal(0,1),     class = b,    coef = age_combo3_2),
  prior(normal(0,1),     class = b,    coef = age_combo3_3),
  prior(normal(0,1),     class = b,    coef = age_combo3_4),
  prior(normal(0,1),     class = b,    coef = age_combo4_1),
  prior(normal(0,1),     class = b,    coef = age_combo4_2),
  prior(normal(0,1),     class = b,    coef = age_combo4_3),
  prior(normal(0,1),     class = b,    coef = age_combo4_4),
  # stimulus type
  prior(normal(0,1),      class = b,    coef = stim_typeh),
  prior(normal(0,1),      class = b,    coef = stim_typel),
  # time spline
  prior(normal(0,1),        class = b,    coef = safter_stim_1),
  #prior(student_t(3,0,2.5), class = sds,  coef = s(after_stim)), # included in nn, wasn't in movement direction but not sure why?
  # action in previous second
  prior(normal(0,0.333),  class = b,    coef = momove_tminus1_num),
  prior(dirichlet(2,2,2,2),     class = simo, coef = momove_tminus1_num1))

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
direction_move_prior <- brm(
  formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +
    s(after_stim) + mo(move_tminus1_num) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

pp_check(direction_move_prior)

print(paste0('priors set and checked at ', Sys.time()))

## reset plotting
dev.off()
pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelchecks.pdf')

#### fit model ####
direction_move_fit <- brm(
  formula = move_index ~ 1 + mo(f_age_num) + age_combo + stim_type +
    s(after_stim) + mo(move_tminus1_num) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# save workspace
save.image('movement_direction/movement_ordinal_model1_run.RData') # save.image('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')

# inspect model
summary(direction_move_fit)
print(paste0('model fitted at ', Sys.time()))

#### check outputs ####
load('movement_direction/movement_ordinal_model1_run.RData') # load('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')
## check Stan code
direction_move_fit$model
direction_move_fit$formula

## extract posterior distribution
draws <- as_draws_df(direction_move_fit) %>%
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
# nearest neighbour version -- run with movement direction first but if it throws an error then come back to the nearest neighbour code to fix it
# draws <- as_draws_df(nn_fit) %>%
#   select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>%
#   pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>%
#   mutate(iteration = rep(rep(1:(num_iter/2),
#                              each = length(unique(parameter))),
#                          num_chains),
#          chain = rep(1:num_chains,
#                      each = length(unique(parameter))*(num_iter/2)),
#          invlogit_draw = invlogit(draw))

print(paste0('posterior extracted at ',Sys.time()))

## move at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

#### calculate log cumulative odds ####
prop <- table(move_no_na$moving_direction) / nrow(move_no_na)
cum_prop <- cumsum(prop)
log_cum_odds <- logit(cum_prop)

#### plot marginal effects ####
## extract marginal effects
marg <- conditional_effects(direction_move_fit,
                            categorical = TRUE,
                            method = 'posterior_epred')
names(marg)
agecombo_effect <- marg[[1]]
stim_effect <- marg[[2]]
agefocal_effect <- marg[[3]]
prevsec_effect <- marg[[4]]
time_effect <- marg[[5]]

## plot marginal effects
(focal_age_plot <- ggplot(agefocal_effect)+
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
    ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('approach directly',
                                      'approach at an angle',
                                      'side on',
                                      'move away at an angle',
                                      'move away directly'))+
    scale_fill_viridis_d(name = 'movement direction:',
                         breaks = c('1','2','3','4','5'),
                         labels = c('approach directly',
                                    'approach at an angle',
                                    'side on',
                                    'move away at an angle',
                                    'move away directly'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)))
ggsave(plot = focal_age_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_focalage_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

focal_age_labels <- c('focal age category 1',
                      'focal age category 2',
                      'focal age category 3',
                      'focal age category 4')
names(focal_age_labels) <- 1:4
(agecombo_plot <- agecombo_effect %>%
    separate(col = age_combo, sep = '_', remove = F,
             into = c('focal_age','partner_age')) %>%
    mutate(agecombo = paste0(focal_age,'-',partner_age)) %>%
    ggplot()+
    geom_errorbar(aes(#x = agecombo,
      x = partner_age,
      colour = as.factor(cats__), # movement direction?
      ymax = upper__, ymin = lower__),
      linewidth = 1,
      width = 0.4)+
    geom_point(aes(#x = agecombo,
      x = partner_age,
      colour = as.factor(cats__),    # movement direction?
      #shape = focal_age,
      y = estimate__),
      size = 3)+
    # geom_ribbon(aes(#x = agecombo,
    #                 x = as.numeric(partner_age),
    #                 fill = as.factor(cats__),     # movement direction?
    #                 ymax = upper__, ymin = lower__),
    #             alpha = 0.4)+
    # geom_line(aes(#x = agecombo,
    #               x = as.numeric(partner_age),
    #               colour = as.factor(cats__),     # movement direction?
    #               y = estimate__),
    #           linewidth = 1)+
    facet_wrap(. ~ focal_age,
               labeller = labeller(focal_age = focal_age_labels))+
    ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('approach directly',
                                      'approach at an angle',
                                      'side on',
                                      'move away at an angle',
                                      'move away directly'))+
    # scale_fill_viridis_d(name = 'movement direction:',
    #                      breaks = c('1','2','3','4','5'),
    #                      labels = c('approach directly',
    #                                 'approach at an angle',
    #                                 'side on',
    #                                 'move away at an angle',
    #                                 'move away directly'))+
    scale_x_discrete(name = 'partner age category')+
    #scale_x_continuous(name = 'partner age category')+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = agecombo_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_agepartner_agecombo.png', device = 'png',
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
    xlab(label = 'stimulus type') + ylab('probability of movement direction')+
    scale_colour_viridis_d(name = 'movement direction:',
                           breaks = c('1','2','3','4','5'),
                           labels = c('approach directly',
                                      'approach at an angle',
                                      'side on',
                                      'move away at an angle',
                                      'move away directly'))+
    scale_x_discrete(breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = stim_plot, filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + agecombo_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = '../outputs/movement_ordinal_model_1/movement_ordinal_model1_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(direction_move_fit, ndraws = 100) # really good fit

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "safter_stim_1","sds_s(after_stim)",
                          "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
                          "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
                          "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
                          "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "bsp_mof_age_num",#"bsp_mopartner_age",
                          "bsp_momove_tminus1_num",
                          # "simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]",
                          # "simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "simo_momove_tminus1_num1[1]","simo_momove_tminus1_num1[2]"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing doesn't move brilliant, esp. for bsp_mofocal_age, but only horrendous one is playback ID

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

## age interaction -- come back to this!

## movement direction in previous second
prevsec1 <- draws_cut %>% filter(parameter == 'bsp_momove_tminus1_num')
prevsec2 <- draws_cut %>% filter(parameter == 'simo_momove_tminus1_num1[1]')
prevsec3 <- draws_cut %>% filter(parameter == 'simo_momove_tminus1_num1[2]')
par(mfrow = c(3,1))
plot(density(prevsec1$draw), main = 't-1 younger') ; abline(v = 0, lty = 2)
plot(density(prevsec2$draw), main = 't-1 matched') ; abline(v = 0, lty = 2)
plot(density(prevsec3$draw), main = 't-1 older') ; abline(v = 0, lty = 2)

# ## time since stimulus -- come back to this!
# timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
# times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
# time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
# time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
# time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
# time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
# time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
# time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
# time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
# time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
# par(mfrow = c(5,2))
# plot(density(timeb$draw), main = 'time slope') ; abline(v = 0, lty = 2)
# plot(density(times$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
# plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
# plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
# plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
# plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
# plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
# plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
# plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
# plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
save.image('movement_direction/movement_ordinal_model1_run.RData')

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
ggplot(move_no_na, aes(x = f_age_num, y = move_index,
                       colour = age_difference))+
  geom_jitter(alpha = 0.1)+
  facet_wrap(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_y_discrete(name = 'focal movement direction relative to target',
                     breaks = c(1,2,3,4,5),
                     labels = c('approach directly','approach angle','move with',
                                'retreat angle','retreat directly'))+
  labs(colour = 'age difference')
print('plot complete')

## plot control data
move_no_na %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = after_stim, y = move_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started')
print('plot complete')

## plot lion data
move_no_na %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = after_stim, y = move_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started')
print('plot complete')

## plot human data
move_no_na %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = after_stim, y = move_index,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(f_age_num ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(f_age_num = age_labels))+
  scale_x_continuous(name = 'time since stimulus started')

print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('movement_direction/movement_ordinal_model1_run.RData') # save.image('ele_playbacks/movement_direction/movement_ordinal_model1_run.RData')
dev.off()

#### predict from model ####
pdf('../outputs/movement_ordinal_model_1/movement_ordinal_model1_modelpredictions.pdf')
load('movement_direction/movement_ordinal_model1_run.RData')
rm(list = ls()[! ls() %in% c('direction_move_fit','move_no_na')]) ; gc()

pred <- posterior_epred(object = direction_move_fit,
                        newdata = move_no_na)
save.image('movement_direction/movement_ordinal_model1_predictions.RData.RData')

# ## convert to data frame
# pred1 <- as.data.frame(pred[,,1])
# colnames(pred1) <- 1:nrow(move_no_na)
# pred2 <- as.data.frame(pred[,,2])
# colnames(pred2) <- 1:nrow(move_no_na)
# save.image('movement_direction/movement_ordinal_model1_predictions.RData.RData')
# 
# move_no_na$data_row <- 1:nrow(move_no_na)
# pred1 <- pred1 %>%
#   pivot_longer(cols = everything(),
#                names_to = 'data_row', values_to = 'epred') %>%
#   left_join(move_no_na, by = 'data_row') %>%
#   mutate(pred_type = 'not_moving',
#          pred_type_num = 0)
# pred2 <- pred2 %>%
#   pivot_longer(cols = everything(),
#                names_to = 'data_row', values_to = 'epred') %>%
#   left_join(move_no_na, by = 'data_row') %>%
#   mutate(pred_type = 'moving',
#          pred_type_num = 1)
# 
# pred <- rbind(pred1, pred2)
# save.image('movement_direction/movement_ordinal_model1_predictions.RData.RData')
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
