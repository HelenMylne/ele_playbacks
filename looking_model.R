#### information ####
# script for looking direction analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
#library(cmdstanr) ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/') ; library(brms) ; library(tidyverse) ; library(LaplacesDemon)
library(StanHeaders, lib.loc = '../../packages/')
library(rstan, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')
#library(cmdstanr, lib.loc = '../../packages/') # library(cmdstanr)
#set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/')
library(tidyverse, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')

theme_set(theme_classic())
set.seed(12345)

pdf('../../outputs/looking_direction_modelprep.pdf')

#### descriptive stats ####
# read in data
prop <- readRDS('../data_processed/proportions_of_time_per_behaviour.RDS') %>%
  mutate(stim_type_full = ifelse(stim_type == 'l', 'lion',
                                 ifelse(stim_type == 'ctd', 'control', 'human')))

# per behaviour, mean and stdev
behav <- prop %>%
  select(section, stim_type, behaviour, behavioural_category, type, age_difference) %>%
  distinct() %>%
  mutate(seconds_mean = NA, seconds_stdv = NA,
         prop_mean = NA, prop_stdv = NA) %>%
  filter(!(type == 'elephant' & is.na(age_difference) == TRUE)) %>%
  filter(!(type == 'neighbour' & is.na(age_difference) == TRUE))

for(i in 1:nrow(behav)){
  x <- prop %>%
    filter(behaviour == behav$behaviour[i] &
             stim_type == behav$stim_type[i] &
             section == behav$section[i])
  if(behav$type[i] == 'elephant' | behav$type[i] == 'neighbour'){
    x <- x %>% filter(age_difference == behav$age_difference[i])
  }
  behav$seconds_mean[i] <- mean(x$behav_seconds, na.rm = T)
  behav$seconds_stdv[i] <- sd(x$behav_seconds, na.rm = T)
  behav$prop_mean[i] <- mean(x$propn, na.rm = T)
  behav$prop_stdv[i] <- sd(x$propn, na.rm = T)
}
rm(i, x) ; gc()

write_csv(behav, '../data_processed/proportions_descriptivestats.csv')
rm(behav, prop) ; gc()

print(paste0('proportional descriptions completed at ',Sys.time()))

#### data prep ####
# https://dagitty.net/dags.html?id=dw8twK
#library(brms, lib.loc = '../../packages/') # library(brms)

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

look <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
  select(subject,bull,pb_num,second,out_frame_name,out_frame_index,
         b1_look_name,#b1_look_index,
         b2_look_name,#b2_look_index,
         b3_look_name,#b3_look_index,
         b4_look_name,#b4_look_index,
         b5_look_name,#b5_look_index,
         b6_look_name,#b6_look_index,
         b7_look_name,#b7_look_index,
         b8_look_name#,b8_look_index,
         #b1_present_index,b2_present_index,b3_present_index,b4_present_index,
         #b5_present_index,b6_present_index,b7_present_index,b8_present_index
  ) %>%
  #pivot_longer(cols = c('b1_present_index','b2_present_index','b3_present_index','b4_present_index',
  #                      'b5_present_index','b6_present_index','b7_present_index','b8_present_index'),
  #             names_to = 'name_present', values_to = 'present') %>%
  pivot_longer(cols = c('b1_look_name','b2_look_name','b3_look_name','b4_look_name',
                        'b5_look_name','b6_look_name','b7_look_name','b8_look_name'),
               names_to = 'elephant_look', values_to = 'direction_look') %>%
  filter(is.na(direction_look) == FALSE) %>%
  filter(direction_look != 'impossible_partner') %>%
  filter(direction_look != 'out_of_sight') %>%
  mutate(look_index = as.integer(factor(direction_look,
                                        levels = c('look at directly','side-on','look directly away')))) %>%
  separate(elephant_look, into = c('dyad_partner','_look_name'), sep = 2) %>%
  select(-`_look_name`) %>%
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
  mutate(look_tminus1 = NA)

# create variable for looking direction at time t-1
subjects <- unique(look$subject)
for(i in 1:length(subjects)){
  focal <- look %>% filter(subject == subjects[i])
  look <- look %>% anti_join(focal, by = 'subject')
  for(j in 2:nrow(focal)){
    focal$look_tminus1[j] <- focal$direction_look[j-1]
  }
  look <- rbind(look, focal)
}
rm(check, x, i, multiple_starts) ; gc()

# filter to remove elephants with unknown ages
look_no_na <- look %>%
  filter(is.na(age_category) == FALSE) %>%
  filter(is.na(partner_age_category) == FALSE) %>%
  filter(is.na(look_tminus1) == FALSE) %>%
  filter(out_frame_name == 'in_frame') %>% 
  mutate(focal_id = as.integer(as.factor(subject)),
         stim_num = as.integer(as.factor(stim_num)),
         age_category = as.factor(age_category),
         partner_age_category = as.factor(partner_age_category)) %>%
  rename(looking_direction = look_index,
         focal_age = age_category,
         partner_age = partner_age_category,
         stim_id = stim_num,
         playback_id = pb_num) %>%
  mutate(after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60)) # time now in minutes, and all 0 before stimulus starts
str(look_no_na)

## clean up final model variables
look_no_na <- look_no_na %>%
  mutate(#age_diff_num = ifelse(age_difference == 'partner younger', 1,
         #                      ifelse(age_difference == 'matched', 2, 3)),
         look_tminus1_num = ifelse(look_tminus1 == 'look at directly', 1,
                                   ifelse(look_tminus1 == 'side-on', 2, 3))) %>%
  mutate(focal_age = as.integer(focal_age), #as.factor(focal_age),
         partner_age = as.integer(partner_age),
         #age_diff_num = as.integer(age_diff_num), #as.factor(age_diff_num),
         look_tminus1_num = as.integer(look_tminus1_num)) %>%  #as.factor(look_tminus1_num))
  mutate(age_combo = paste0(focal_age,'_',partner_age))
print(paste0('data created at ',Sys.time()))

#### set priors ####
get_prior(formula = looking_direction ~ 1 + mo(focal_age) + age_combo + stim_type +   # fixed effects
            s(after_stim) + mo(look_tminus1_num) +                                          # controls, treat time as a spline
            (1|focal_id) + (1|stim_id) + (1|playback_id),                                   # random effects
          data = look_no_na,
          family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,1),      class = b,    coef = mofocal_age), # nn = normal(0,0.25)
  prior(dirichlet(2,2,2), class = simo, coef = mofocal_age1),
  # partner age
  #prior(normal(0,0.25),     class = b,    coef = mopartner_age),
  #prior(dirichlet(2,2,2),   class = simo, coef = mopartner_age1),
  # age interaction
  #prior(normal(0,0.25),     class = b,    coef = mofocal_age:mopartner_age),
  #prior(dirichlet(2,2,2),   class = simo, coef = mofocal_age:mopartner_age1),
  #prior(dirichlet(2,2,2),   class = simo, coef = mofocal_age:mopartner_age2),
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
  # stim type
  prior(normal(0,1),     class = b,    coef = stim_typeh),
  prior(normal(0,1),     class = b,    coef = stim_typel),
  # time spline
  prior(normal(0,1),     class = b,    coef = safter_stim_1), 
  prior(student_t(3,0,2.5), class = sds, coef = s(after_stim)), # included in nn, wasn't in looking direction but not sure why?
  # action in previous second
  prior(normal(0,0.333), class = b,    coef = molook_tminus1_num),
  prior(dirichlet(2,2),    class = simo, coef = molook_tminus1_num1))

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
direction_look_prior <- brm(
  formula = looking_direction ~ 1 + mo(focal_age) + age_combo + stim_type + # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                                        # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                                 # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

pp_check(direction_look_prior) # prior expects 1 and 3 most likely, 2 least likely. data show 1 least likely, 2 middle, 3 most.

print(paste0('priors set at ',Sys.time()))

## reset plotting
dev.off()
pdf('../../outputs/looking_direction_modelchecks.pdf')

#### fit model ####
direction_look_fit <- brm(
  formula = looking_direction ~ 1 + mo(focal_age) + age_combo + stim_type +   # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                                        # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                                 # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# save workspace
save.image('nearest_neighbour/neighbour_model_run.RData') # save.image('ele_playbacks/looking_direction/looking_direction_model_run_agecombo.RData')

# inspect model
summary(direction_look_fit)
print(paste0('model run at ',Sys.time()))

#### check outputs ####
#load('looking_direction/looking_direction_model_run_agecombo.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()
summary(direction_look_fit)

## check Stan code
direction_look_fit$model
direction_look_fit$formula

## extract posterior distribution
draws <- as_draws_df(direction_look_fit) %>%
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
# nearest neighbour version -- run with looking direction first but if it throws an error then come back to the nearest neighbour code to fix it
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

## look at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

#### calculate log cumulative odds ####
prop <- table(look_no_na$looking_direction) / nrow(look_no_na)
cum_prop <- cumsum(prop)
log_cum_odds <- logit(cum_prop)

#### plot marginal effects ####
## extract marginal effects
marg <- conditional_effects(direction_look_fit,
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
ggsave(plot = focal_age_plot, filename = '../outputs/looking_marginaleffects_focalage_agecombo.png', device = 'png',
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
                      colour = as.factor(cats__), # looking direction?
                      ymax = upper__, ymin = lower__),
                  linewidth = 1,
                  width = 0.4)+
    geom_point(aes(#x = agecombo,
                   x = partner_age,
                   colour = as.factor(cats__),    # looking direction?
                   #shape = focal_age,
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
    facet_wrap(. ~ focal_age,
               labeller = labeller(focal_age = focal_age_labels))+
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
ggsave(plot = agecombo_plot, filename = '../outputs/looking_marginaleffects_agepartner_agecombo.png', device = 'png',
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
ggsave(plot = stim_plot, filename = '../outputs/looking_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + agecombo_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = '../outputs/looking_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(direction_look_fit, ndraws = 100) # really good fit

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
                          "bsp_molook_tminus1_num",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]",
                          "simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
                          "simo_molook_tminus1_num1[1]","simo_molook_tminus1_num1[2]"))
ggplot(data = draws_cut,
       aes(x = iteration, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing doesn't look brilliant, esp. for bsp_mofocal_age, but only horrendous one is playback ID

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

## looking direction in previous second
prevsec1 <- draws_cut %>% filter(parameter == 'bsp_molook_tminus1_num')
prevsec2 <- draws_cut %>% filter(parameter == 'simo_molook_tminus1_num1[1]')
prevsec3 <- draws_cut %>% filter(parameter == 'simo_molook_tminus1_num1[2]')
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
ggplot(look_no_na, aes(x = focal_age, y = looking_direction,
                       colour = age_difference))+
  geom_jitter(alpha = 0.1)+
  facet_wrap(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_y_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','older'))+
  labs(colour = 'age difference')

## plot control data
look_no_na %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = time_since_stim, y = looking_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(focal_age = age_labels))+
  scale_x_continuous(name = 'time since stimulus started (s)')

## plot lion data
look_no_na %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = time_since_stim, y = looking_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference,
                                levels = c('partner younger','matched','partner older')),
             labeller = labeller(focal_age = age_labels))+
  scale_x_continuous(name = 'time since stimulus started (s)')

## plot human data
look_no_na %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = time_since_stim, y = looking_direction,
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
pdf('../../outputs/looking_direction_modelpredictions.pdf')

# #### predict from model ####
# #load('looking_direction/looking_direction_model_run_agecombo.RData')
# rm(list = ls()[! ls() %in% c('direction_look_fit','look_no_na')]) ; gc()
# 
# pred <- posterior_epred(object = direction_look_fit,
#                         newdata = look_no_na[1:(nrow(look_no_na)/2),])
# pred2 <- posterior_epred(object = direction_look_fit,
#                          newdata = look_no_na[((nrow(look_no_na)/2)+1):nrow(look_no_na),])
# save.image('looking_direction/looking_direction_model_predictions_agecombo.RData')
# 
# ## combine into single data frame -- DO THIS AGAIN AND CHECK DIMENSIONS BECAUSE PRED_ALL HAS FAR MORE COLUMNS THAN LOOK_NO_NA HAS ROWS -- is it 3 times the number? one for each prediction?
# pred_all <- pred %>%
#   as.data.frame() %>%
#   cbind(as.data.frame(pred2))
# colnames(pred_all) <- 1:nrow(look_no_na)
# look_no_na$data_row <- 1:nrow(look_no_na)
# save.image('looking_direction/looking_direction_model_predictions_agecombo.RData')
# rm(pred, pred2) ; gc()
# 
# pred_t <- pred_all %>%
#   as.matrix() %>%
#   t() %>%
#   as.data.frame()
# pred_t2 <- pred_t %>%
#   mutate(data_row = 1:nrow(look_no_na)) %>%
#   pivot_longer(cols = everything,
#                cols_vary = 'slowest',
#                names_to = 'posterior_position',
#                values_to = 'prediction')
# 
# pred_all <- pivot_longer(data = pred_all,
#                          cols = everything(),
#                names_to = 'data_row',
#                values_to = 'prediction')
# save.image('looking_direction/looking_direction_model_predictions_agecombo.RData')
# pred_all <- pred_all %>%
#   mutate(data_row = as.numeric(data_row))
# save.image('looking_direction/looking_direction_model_predictions_agecombo.RData')
# pred_all <- pred_all %>%
#   left_join(look_no_na, by = 'data_row')
# save.image('looking_direction/looking_direction_model_predictions_agecombo.RData')
# 
# print(paste0('predictions calculated at ',Sys.time()))
# 
# ## predict from model -- raw data -- nn ####
# 
# ## predict from raw data
# nn_no_na$unique_data_combo <- 1:nrow(nn_no_na)
# pred_mtx <- posterior_epred(object = nn_fit, newdata = nn_no_na)
# colnames(pred_mtx) <- nn_no_na$unique_data_combo
# save.image('nearest_neighbour/neighbour_model_predictions.RData')
# 
# ## convert predictions to long format data set, using only first 100 values per chain
# #load('nearest_neighbour/neighbour_model_predictions.RData')
# predictions1 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,1] %>%
#   as.data.frame() %>%
#   pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
#   mutate(unique_data_combo = as.integer(unique_data_combo),
#          pred_type = 1) %>%
#   left_join(nn_no_na, by = 'unique_data_combo')
# predictions2 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,2] %>%
#   as.data.frame() %>%
#   pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
#   mutate(unique_data_combo = as.integer(unique_data_combo),
#          pred_type = 2) %>%
#   left_join(nn_no_na, by = 'unique_data_combo')
# predictions3 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,3] %>%
#   as.data.frame() %>%
#   pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
#   mutate(unique_data_combo = as.integer(unique_data_combo),
#          pred_type = 3) %>%
#   left_join(nn_no_na, by = 'unique_data_combo')
# save.image('nearest_neighbour/neighbour_model_predictions.RData')
# 
# ## create long format data set with all predictions -- use for calculating some uncertainty measure (standard deviation/error amongst sets of predictions)
# # predictions_all <- predictions %>%
# #   mutate(chain_set100 = 1)
# # for(i in 2:10){
# #   j <- seq(from = 100*(i-1)+1, to = (100*i), by = 1)
# #   pred <- pred_mtx[c(j,1000+j,2000+j,3000+j),] %>%
# #     as.data.frame() %>%
# #     pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
# #     mutate(unique_data_combo = as.integer(unique_data_combo)) %>%
# #     left_join(nn_no_na, by = 'unique_data_combo') %>%
# #     mutate(chain_set100 = i)
# #   predictions_all <- rbind(predictions_all, pred)
# # }
# predictions_all <- rbind(predictions1, predictions2, predictions3)
# save.image('nearest_neighbour/neighbour_model_predictions.RData')
# 
# ## plot predictions -- posterior_epred() -- nn ####
# #load('nearest_neighbour/neighbour_model_predictions.RData')  #load('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData')
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
#     facet_grid(nn_tminus1_num ~ after_stim,
#                labeller = labeller(nn_tminus1_num = prevsec_labels))+
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
#     facet_grid(nn_tminus1_num ~ after_stim,
#                labeller = labeller(nn_tminus1_num = prevsec_labels))+
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
#     facet_grid(nn_tminus1_num ~ after_stim,
#                labeller = labeller(nn_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted age of neighbour relative to focal:',
#          fill = 'predicted age of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# (ctd_plot + lion_plot + human_plot)+
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(), file = '../outputs/nn_predictions_violin.png',
#        device = 'png', height = 8, width = 24)
# 
# ## graph contrasts from predictions and extract coefficients -- nn ####
# #CALCULATE POSTERIOR CONTRASTS FROM PREDICTIONS
# # load('nearest_neighbour/neighbour_model_predictions.RData') # load('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData')
# rm(prevsec_labels, ctd_plot, human_plot, lion_plot, predictions_all) ; gc()
# 
# # stim type -- nn ####
# ## redo predictions with different stimulus types: all doves
# ctd_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'ctd',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# ctd_mtx <- posterior_epred(object = nn_fit, newdata = ctd_nn)
# colnames(ctd_mtx) <- ctd_nn$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all lions
# lion_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'l',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# lion_mtx <- posterior_epred(object = nn_fit, newdata = lion_nn)
# colnames(lion_mtx) <- lion_nn$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all humans
# human_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'h',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# human_mtx <- posterior_epred(object = nn_fit, newdata = human_nn)
# colnames(human_mtx) <- human_nn$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('nearest_neighbour/neighbour_model_stimuluscontrasts_epred.RData')
# 
# ## count types of each prediction
# #load('nearest_neighbour/neighbour_model_stimuluscontrasts_epred.RData')
# # count_values <- function(vector, levels = c(1,2,3)) {
# #   x <- tabulate(factor(vector, levels), length(levels))
# #   return(list(x))
# # }
# stim_pred <- ctd_nn %>%
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
#   separate(col = stim_propage_mu, into = c('stim_type', 'nn_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-stim_propage_sd) %>%
#   mutate(nn_pred = as.numeric(nn_pred)) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older')))
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
# contrasts <- nn_no_na %>%
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
#   separate(contrast, into = c('contrast','nn_pred'),
#            sep = '_age', remove = T) %>%
#   separate(nn_pred, into = c('nn_pred','mu'),
#            sep = '_', remove = T) %>%
#   mutate(nn_pred = as.numeric(nn_pred)) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older'))) %>%
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
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast)
# 
# save.image('nearest_neighbour/neighbour_model_stimuluscontrasts_epred.RData')
# 
# # focal age -- nn ####
# # load('nearest_neighbour/neighbour_model_stimuluscontrasts_epred.RData')
# rm(ctd_nn, ctd_mtx, human_nn, human_mtx, lion_nn, lion_mtx,
#    contrasts, contrasts_long, stim_pred,
#    ctd_vs_human_age1, ctd_vs_human_age2, ctd_vs_human_age3,
#    ctd_vs_lion_age1, ctd_vs_lion_age2, ctd_vs_lion_age3,
#    lion_vs_human_age1, lion_vs_human_age2, lion_vs_human_age3) ; gc()
# 
# ## predict with original ages
# age_nn_org <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# age_mtx_org <- posterior_epred(object = nn_fit, newdata = age_nn_org)
# colnames(age_mtx_org) <- age_nn_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with altered ages
# age_nn_alt <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(f_age_num_original = f_age_num) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1),
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(f_age_num_original)
# age_mtx_alt <- posterior_epred(object = nn_fit, newdata = age_nn_alt)
# colnames(age_mtx_alt) <- age_nn_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# save.image('nearest_neighbour/neighbour_model_agecontrasts_epred.RData')
# 
# ## summarise and convert to long format
# age_pred <- age_nn_org %>%
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
#                names_to = 'focal_agenn_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(age_org_prop1_sd,age_org_prop2_sd,age_org_prop3_sd,
#                         age_alt_prop1_sd,age_alt_prop2_sd,age_alt_prop3_sd),
#                names_to = 'focal_agenn_sd', values_to = 'stdv_propn') %>%
#   separate(col = focal_agenn_mu, into = c('focal_agenn_mu','mu'),
#            sep = '_m', remove = T) %>%
#   separate(col = focal_agenn_sd, into = c('focal_agenn_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-mu, -sd) %>%
#   filter(focal_agenn_mu == focal_agenn_sd) %>%
#   separate(col = focal_agenn_mu, into = c('original_altered', 'nn_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-focal_agenn_sd) %>%
#   mutate(nn_pred = as.numeric(nn_pred),
#          f_age_num = ifelse(original_altered == 'age_org',
#                             f_age_num,
#                             ifelse(original_altered == 'age_alt' & f_age_num == 4,
#                                    1, f_age_num + 1))) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older')))
# 
# ## calculate contrasts
# alt_vs_org_young <- age_mtx_alt[,,1] - age_mtx_org[,,1]
# alt_vs_org_match <- age_mtx_alt[,,2] - age_mtx_org[,,2]
# alt_vs_org_older <- age_mtx_alt[,,3] - age_mtx_org[,,3]
# 
# ## summarise contrasts
# contrasts <- nn_no_na %>%
#   mutate(alt_vs_org_young_mu = apply(alt_vs_org_young, 2, mean),
#          alt_vs_org_young_sd = apply(alt_vs_org_young, 2, sd),
#          alt_vs_org_match_mu = apply(alt_vs_org_match, 2, mean),
#          alt_vs_org_match_sd = apply(alt_vs_org_match, 2, sd),
#          alt_vs_org_older_mu = apply(alt_vs_org_older, 2, mean),
#          alt_vs_org_older_sd = apply(alt_vs_org_older, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(alt_vs_org_young_mu,alt_vs_org_match_mu,alt_vs_org_older_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('alt','vs','org','nn_pred','mu'),
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
#   mutate(pred_type = ifelse(nn_pred == 'young', 'younger',
#                             ifelse(nn_pred == 'match', 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   mutate(f_age_new = ifelse(f_age_num == 4, 1, f_age_num+1)) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ f_age_new, scales = 'free')
# save.image('nearest_neighbour/neighbour_model_agecontrasts_epred.RData')
# 
# # neighbour in previous second -- nn ####
# #load('nearest_neighbour/neighbour_model_agecontrasts_epred.RData')
# rm(age_nn_org, age_mtx_org, age_nn_alt, age_mtx_alt, age_pred, alt_vs_org_young, alt_vs_org_match, alt_vs_org_older, contrasts, contrasts_long) ; gc()
# 
# ## redo predictions with different previous neighbours: all younger -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 1, NN AT T-1 YOUNGER
# young_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(nn_tminus1_num = 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# young_mtx <- posterior_epred(object = nn_fit, newdata = young_nn)
# colnames(young_mtx) <- young_nn$unique_data_combo
# young_mtx <- young_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different previous neighbours: all matching
# match_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(nn_tminus1_num = 2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# match_mtx <- posterior_epred(object = nn_fit, newdata = match_nn)
# colnames(match_mtx) <- match_nn$unique_data_combo
# match_mtx <- match_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different previous neighbours: all older -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 4, NN AT T-1 OLDER
# older_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(nn_tminus1_num = 3,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# older_mtx <- posterior_epred(object = nn_fit, newdata = older_nn)
# colnames(older_mtx) <- older_nn$unique_data_combo
# older_mtx <- older_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('nearest_neighbour/neighbour_model_tminus1contrasts_epred.RData')
# 
# ## count types of each prediction
# #load('nearest_neighbour/neighbour_model_tminus1contrasts_epred.RData')
# prevsec_pred <- young_nn %>%
#   dplyr::select(-nn_tminus1_num) %>%
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
#   separate(col = prevsec_propage_mu, into = c('prevsec_type', 'nn_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-prevsec_propage_sd) %>%
#   mutate(nn_pred = as.numeric(nn_pred)) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older')))
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
# contrasts <- nn_no_na %>%
#   select(-nn_tminus1_num) %>%
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
#   separate(contrast, into = c('contrast','nn_pred'),
#            sep = '_age', remove = T) %>%
#   separate(nn_pred, into = c('nn_pred','mu'),
#            sep = '_', remove = T) %>%
#   mutate(nn_pred = as.numeric(nn_pred)) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older'))) %>%
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
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast)
# 
# save.image('nearest_neighbour/neighbour_model_prevseccontrasts_epred.RData')
# 
# # predictions %>%
# #   mutate(previous = ifelse(nn_tminus1_num == 1, 'younger',
# #                            ifelse(nn_tminus1_num == 2, 'same age', 'older')),
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
# # prevsec2 <- pred_prop %>% filter(nn_tminus1_num == 2)
# # prevsec3 <- pred_prop %>% filter(nn_tminus1_num == 3)
# # pred_prev <- pred_prop %>%
# #   filter(nn_tminus1_num == 1) %>%
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
# #   mutate(nn1_2 = prop_1 - prop_2,
# #          nn1_3 = prop_1 - prop_3,
# #          nn2_3 = prop_2 - prop_3)
# # pred_prev %>%
# #   select(stim_type, after_stim, f_age_num, prediction,
# #          nn1_2,nn1_3,nn2_3) %>%
# #   pivot_longer(cols = c('nn1_2', 'nn1_3', 'nn2_3'),
# #                names_to = 'contrast', values_to = 'value') %>%
# #   mutate(contrast = ifelse(contrast == 'nn1_2', 'younger vs same',
# #                            ifelse(contrast == 'nn1_3', 'younger vs older',
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
# # load('nearest_neighbour/neighbour_model_prevseccontrasts_epred.RData')
# rm(young_nn, young_mtx, match_nn, match_mtx, older_nn, older_mtx,
#    contrasts, contrasts_long, prevsec_pred,
#    young_vs_match_age1, young_vs_match_age2, young_vs_match_age3,
#    young_vs_older_age1, young_vs_older_age2, young_vs_older_age3,
#    match_vs_older_age1, match_vs_older_age2, match_vs_older_age3) ; gc()
# 
# ## predict with original times
# time_nn_org <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# time_mtx_org <- posterior_epred(object = nn_fit, newdata = time_nn_org)
# colnames(time_mtx_org) <- time_nn_org$unique_data_combo
# time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +15 seconds
# time_nn_alt_0.25 <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.25 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.25)
# colnames(time_mtx_alt_0.25) <- time_nn_alt_0.25$unique_data_combo
# time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +30 seconds
# time_nn_alt_0.50 <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1/2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.50 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.50)
# colnames(time_mtx_alt_0.50) <- time_nn_alt_0.50$unique_data_combo
# time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +45 seconds
# time_nn_alt_0.75 <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 3/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_0.75 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.75)
# colnames(time_mtx_alt_0.75) <- time_nn_alt_0.75$unique_data_combo
# time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +60 seconds
# time_nn_alt_1.00 <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(after_stim_org = after_stim) %>%
#   mutate(after_stim = after_stim + 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(after_stim_org)
# time_mtx_alt_1.00 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_1.00)
# colnames(time_mtx_alt_1.00) <- time_nn_alt_1.00$unique_data_combo
# time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('nearest_neighbour/neighbour_model_timecontrasts_epred.RData')
# 
# ## summarise and convert to long format
# time_pred <- time_nn_org %>%
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
#                names_to = 'time_org_alt_prop_agenn_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(time_org_0.00_prop1_sd,time_org_0.00_prop2_sd,time_org_0.00_prop3_sd,
#                         time_alt_0.25_prop1_sd,time_alt_0.25_prop2_sd,time_alt_0.25_prop3_sd,
#                         time_alt_0.50_prop1_sd,time_alt_0.50_prop2_sd,time_alt_0.50_prop3_sd,
#                         time_alt_0.75_prop1_sd,time_alt_0.75_prop2_sd,time_alt_0.75_prop3_sd,
#                         time_alt_1.00_prop1_sd,time_alt_1.00_prop2_sd,time_alt_1.00_prop3_sd),
#                names_to = 'time_org_alt_prop_agenn_sd', values_to = 'stdv_propn') %>%
#   separate(col = time_org_alt_prop_agenn_mu,
#            into = c('time_mu','org_mu','alt_mu','prop_agenn_mu','mu'),
#            sep = '_', remove = T) %>%
#   separate(col = time_org_alt_prop_agenn_sd,
#            into = c('time_sd','org_sd','alt_sd','prop_agenn_sd','sd'),
#            sep = '_', remove = T) %>%
#   select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>%
#   filter(alt_mu == alt_sd & prop_agenn_sd == prop_agenn_mu) %>%
#   mutate(nn_pred = ifelse(prop_agenn_mu == 'prop1', 1,
#                           ifelse(prop_agenn_mu == 'prop2', 2,
#                                  ifelse(prop_agenn_mu == 'prop3', 3, 4)))) %>%
#   select(-alt_sd, -prop_agenn_mu, -prop_agenn_sd) %>%
#   rename(mins_added = alt_mu) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older')))
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
# contrasts <- nn_no_na %>%
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
# save.image('nearest_neighbour/neighbour_model_timecontrasts_epred.RData')
# 
