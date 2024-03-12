#### information ####
# script for movement direction analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
# library(tidyverse) ; library(cmdstanr) ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/') ; library(brms) ; library(LaplacesDemon)
library(StanHeaders, lib.loc = '../../packages/')
library(rstan, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')
library(tidyverse, lib.loc = '../../packages/')
#library(cmdstanr, lib.loc = '../../packages/') ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')

theme_set(theme_classic())
set.seed(12345)

pdf('../outputs/movement_direction_modelprep.pdf')

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
    focal$move_tminus1[j] <- focal$direction_move[j-1]
  }
  move <- rbind(move, focal)
}
rm(check, x, i, multiple_starts) ; gc()

# filter to remove elephants with unknown ages
move_no_na <- move %>% 
  filter(is.na(age_category) == FALSE) %>% 
  filter(is.na(partner_age_category) == FALSE) %>% 
  filter(is.na(move_tminus1) == FALSE) %>% 
  filter(is.na(move_index) == FALSE) %>%   # remove all seconds where not moving -- only considering the times focal is moving, what is the direction?
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

move_no_na <- move_no_na %>% 
  mutate(move_tminus1_num = as.integer(factor(move_tminus1,
                                              levels = c('not_moving',
                                                         'approach_directly','approach_angle',
                                                         'move_with',
                                                         'away_angle','away_directly')))) %>% 
  mutate(focal_age = as.integer(focal_age),
         partner_age = as.integer(partner_age),
         move_tminus1_num = as.integer(move_tminus1_num)) %>% 
  filter(!is.na(move_tminus1_num)) %>% 
  mutate(after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60)) %>%  # time now in minutes, and all 0 before stimulus starts
  mutate(age_combo = paste0(focal_age,'_',partner_age))

print(paste0('data imported at ', Sys.time()))

#### set priors ####
# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
get_prior(formula = moving_direction ~ 1 + mo(focal_age) + age_combo + stim_type +   # fixed effects
            s(after_stim) + mo(move_tminus1_num) +                                       # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),                                  # random effects
          data = move_no_na,
          family = cumulative("logit"))

priors <- c(
  # focal age
  prior(normal(0,0.25),   class = b,    coef = mofocal_age),
  prior(dirichlet(2,2,2), class = simo, coef = mofocal_age1),
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
  prior(dirichlet(2,2,2,2,2),     class = simo, coef = momove_tminus1_num1))

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
direction_move_prior <- brm(
  formula = moving_direction ~ 1 + mo(focal_age) + age_combo + stim_type + 
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
pdf('../../outputs/looking_direction_modelchecks.pdf')

#### fit model ####
direction_move_fit <- brm(
  formula = moving_direction ~ 1 + mo(focal_age) + age_combo + stim_type + 
    s(after_stim) + mo(move_tminus1_num) + 
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# save workspace
save.image('movement_direction/moving_direction_model_run.RData') # save.image('ele_playbacks/movement_direction/moving_direction_model_run.RData')

# inspect model
summary(direction_move_fit)
print(paste0('model fitted at ', Sys.time()))

#### check outputs ####
#load('movement_direction/moving_direction_model_run.RData') # load('ele_playbacks/movement_direction/moving_direction_model_run.RData')
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
prop <- table(move_no_na$movement_direction) / nrow(move_no_na)
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
    # geom_ribbon(aes(x = focal_age,
    #                 ymax = upper__, ymin = lower__,
    #                 fill = cats__),
    #             alpha = 0.4)+
    # geom_line(aes(x = focal_age,
    #               y = estimate__,
    #               colour = cats__),
    #           linewidth = 1)+
    geom_errorbar(aes(x = focal_age,
                      ymax = upper__, ymin = lower__,
                      colour = cats__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = focal_age,
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
ggsave(plot = focal_age_plot, filename = '../outputs/movement_marginaleffects_focalage_agecombo.png', device = 'png',
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
ggsave(plot = agecombo_plot, filename = '../outputs/movement_marginaleffects_agepartner_agecombo.png', device = 'png',
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
ggsave(plot = stim_plot, filename = '../outputs/movement_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + agecombo_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = '../outputs/movement_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(direction_move_fit, ndraws = 100) # really good fit

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "safter_stim_1","sds_s(after_stim)",
                          # "bs_safter_stim_1","sds_safter_stim_1",
                          # "s_safter_stim_1[1]","s_safter_stim_1[2]",
                          # "s_safter_stim_1[3]","s_safter_stim_1[4]",
                          # "s_safter_stim_1[5]","s_safter_stim_1[6]",
                          # "s_safter_stim_1[7]","s_safter_stim_1[8]",
                          "bsp_mofocal_age","bsp_mopartner_age",
                          "bsp_momove_tminus1_num",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]",
                          "simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
                          "simo_momove_tminus1_num1[1]","simo_momove_tminus1_num1[2]"))
ggplot(data = draws_cut,
       aes(x = iteration, y = draw, colour = as.factor(chain)))+
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
age1 <- draws_cut %>% filter(parameter == 'bsp_mofocal_age')
age2 <- draws_cut %>% filter(parameter == 'simo_mofocal_age1[1]')
age3 <- draws_cut %>% filter(parameter == 'simo_mofocal_age1[2]')
age4 <- draws_cut %>% filter(parameter == 'simo_mofocal_age1[3]')
par(mfrow = c(2,2))
plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)

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

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
ggplot(move_no_na, aes(x = focal_age, y = movement_direction,
                       colour = age_difference))+
  geom_jitter(alpha = 0.1)+
  facet_wrap(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_y_continuous(name = 'focal movement direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('move directly at','side-on','older'))+
  labs(colour = 'age difference')

## plot control data
move_no_na %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = time_since_stim, y = movement_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(focal_age = age_labels))+
  scale_x_continuous(name = 'time since stimulus started (s)')

## plot lion data
move_no_na %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = time_since_stim, y = movement_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(focal_age = age_labels))+
  scale_x_continuous(name = 'time since stimulus started (s)')

## plot human data
move_no_na %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = time_since_stim, y = movement_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(focal_age = age_labels))+
  scale_x_continuous(name = 'time since stimulus started (s)')

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))

## reset plotting
dev.off()
#pdf('../../outputs/movement_direction_modelpredictions.pdf')

#### predict from model -- TAKE THIS FROM movement MODEL ONCE YOU'VE WORKED IT OUT FOR THAT ####
