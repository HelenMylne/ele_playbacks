#### information ####
# script for looking direction analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
#library(cmdstanr) ; set_cmdstan_path('../packages/.cmdstan/cmdstan-2.31.0/') ; library(brms) ; library(tidyverse) ; library(LaplacesDemon)
library(StanHeaders, lib.loc = '../packages/')
library(rstan, lib.loc = '../packages/')
library(brms, lib.loc = '../packages/')
#library(cmdstanr, lib.loc = '../packages/') # library(cmdstanr)
#set_cmdstan_path('../packages/.cmdstan/cmdstan-2.31.0/')
library(tidyverse, lib.loc = '../packages/')
library(LaplacesDemon, lib.loc = '../packages/')
library(patchwork, lib.loc = '../packages/')

theme_set(theme_classic())
set.seed(12345)

pdf('outputs/looking_direction_modelprep.pdf')

#### data prep ####
# https://dagitty.net/dags.html?id=dw8twK
# read in data
ages <- readRDS('data_processed/behaviour_by_second_indexvariables_bda.RDS') %>% 
  select(focal, f_age_cat, f_age_num) %>% 
  distinct() %>% 
  filter(!is.na(f_age_cat)) %>% 
  mutate(partner = focal,
         p_age_cat = f_age_cat,
         p_age_num = f_age_num)

stim_starts <- readRDS('data_processed/stimuli.RDS') %>%
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

look <- readRDS('data_processed/behaviour_by_second_indexvariables.RDS') %>%
  filter(out_frame_name == 'in_frame') %>%
  select(subject,pb_num,second,
         b1_look_name,#b1_look_index,
         b2_look_name,#b2_look_index,
         b3_look_name,#b3_look_index,
         b4_look_name,#b4_look_index,
         b5_look_name,#b5_look_index,
         b6_look_name,#b6_look_index,
         b7_look_name,#b7_look_index,
         b8_look_name#,b8_look_index,
  ) %>%
  pivot_longer(cols = c('b1_look_name','b2_look_name','b3_look_name','b4_look_name',
                        'b5_look_name','b6_look_name','b7_look_name','b8_look_name'),
               names_to = 'elephant_look', values_to = 'looking_direction') %>%
  filter(is.na(looking_direction) == FALSE) %>%
  filter(looking_direction != 'impossible_partner') %>%
  filter(looking_direction != 'out_of_sight') %>%
  separate(elephant_look, into = c('partner_bull','look_name'), sep = 2, remove = F) %>%
  select(-look_name) %>%
  rename(focal = subject) %>% 
  mutate(partner = paste0(partner_bull, '_e', pb_num)) %>%
  left_join(ages[,c('focal','f_age_cat','f_age_num')], by = 'focal') %>%
  left_join(ages[,c('partner','p_age_cat','p_age_num')], by = 'partner') %>%
  # rename(f_subject = subject,
  #        f_bull = bull,
  #        p_subject = targeted_elephant,
  #        p_bull = partner_bull,
  #        look_binom = looking_direction,
  #        age_diff_cat = age_difference) %>%
  mutate(pb_num = as.numeric(pb_num),
         age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num),
                                 'partner_younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num),
                                        'matched',
                                        'partner_older'))) %>%
  left_join(stim_starts, by = 'pb_num') %>%
  mutate(time_since_stim = second - stim_start,
         after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60)) %>%
  select(-elephant_look,-partner_bull,-group_size,-stim_start) %>%
  mutate(look_tminus1 = NA)

unique(look$focal[is.na(look$f_age_num)])   # b6_e7 = unknown age
unique(look$partner[is.na(look$p_age_num)]) # b2_e13 = unknown age
length(which(is.na(look$looking_direction) == TRUE))

# create variable for nearest looking_direction at time t-1
subjects <- unique(look$focal)
for(i in 1:length(subjects)){
  focal <- look %>% filter(focal == subjects[i])
  look <- look %>% anti_join(focal, by = 'focal')
  for(j in 2:nrow(focal)){
    focal$look_tminus1[j] <- focal$looking_direction[j-1]
  }
  look <- rbind(look, focal)
}
rm(check, x, i, j, multiple_starts, focal) ; gc()

# filter to remove elephants with unknown ages
look_no_na <- look %>%
  mutate(age_difference = factor(age_difference,
                                 levels = c('partner_younger',
                                            'matched',
                                            'partner_older')),
         look_index = ifelse(looking_direction == 'look at directly', 1,
                             ifelse(looking_direction == 'side-on', 2, 3)),
         look_tminus1_num = ifelse(look_tminus1 == 'look at directly', 1,
                                   ifelse(look_tminus1 == 'side-on', 2, 3))) %>%
  mutate(age_diff_num = as.integer(age_difference),
         f_age_num = as.integer(f_age_num),
         p_age_num = as.integer(p_age_num)) %>%
  mutate(age_combo = paste0(f_age_num, '_', p_age_num)) %>% 
  filter(is.na(age_diff_num) == FALSE) %>%
  filter(is.na(f_age_num) == FALSE) %>%
  filter(is.na(p_age_num) == FALSE) %>%
  filter(is.na(look_tminus1) == FALSE) %>%
  mutate(focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num) %>% 
  select(focal, partner, looking_direction, look_index,
         f_age_cat, p_age_cat, f_age_num, p_age_num,
         age_difference, age_diff_num, age_combo,
         time_since_stim, after_stim, stim_type,
         look_tminus1, look_tminus1_num,
         focal_id, stim_id, playback_id)
str(look_no_na)

#### set priors ####
get_prior(formula = looking_direction ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
            s(after_stim) + mo(look_tminus1_num) +                                          # controls, treat time as a spline
            (1|focal_id) + (1|stim_id) + (1|playback_id),                                   # random effects
          data = look_no_na,
          family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,1),      class = b,    coef = mof_age_num), # nn = normal(0,0.25)
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
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
  #prior(student_t(3,0,2.5), class = sds, coef = s(after_stim)), # not sure why this is in get_prior() but not model but it's not needed
  # action in previous second
  prior(normal(0,0.333), class = b,    coef = molook_tminus1_num),
  prior(dirichlet(2,2),    class = simo, coef = molook_tminus1_num1))

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
direction_look_prior <- brm(
  formula = look_index ~ 1 + mo(f_age_num) + age_combo + stim_type + # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                           # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                    # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')

pp_check(direction_look_prior) # prior expects 1 and 3 most likely, 2 least likely. data show 1 least likely, 2 middle, 3 most.

print(paste0('priors set at ',Sys.time()))

## reset plotting
dev.off()
pdf('outputs/looking_direction_modelchecks.pdf')

#### fit model ####
direction_look_fit <- brm(
  formula = look_index ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
    s(after_stim) + mo(look_tminus1_num) +                             # controls, treat time as a spline
    (1|focal_id) + (1|stim_id) + (1|playback_id),                      # random effects
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# save workspace
save.image('ele_playbacks/looking_direction/looking_direction_model_run_agecombo.RData') # save.image('ele_playbacks/looking_direction/looking_direction_model_run_agecombo.RData')

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
ggsave(plot = focal_age_plot, filename = 'outputs/looking_marginaleffects_focalage_agecombo.png', device = 'png',
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
ggsave(plot = agecombo_plot, filename = 'outputs/looking_marginaleffects_agepartner_agecombo.png', device = 'png',
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
ggsave(plot = stim_plot, filename = 'outputs/looking_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + agecombo_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = 'outputs/looking_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(direction_look_fit, ndraws = 100) # really good fit

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
                          "bsp_molook_tminus1_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          #"simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
                          "simo_molook_tminus1_num1[1]","simo_molook_tminus1_num1[2]"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing doesn't look brilliant, esp. for bsp_mof_age_num, but only horrendous one is playback ID

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
ggplot(look_no_na, aes(x = f_age_num, y = looking_direction,
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

print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('ele_playbacks/looking_direction/looking_direction_model_run_agecombo.RData') # save.image('ele_playbacks/looking_direction/looking_direction_model_run_agecombo.RData')
dev.off()
#pdf('outputs/looking_direction_modelpredictions.pdf')

#### predict from model -- TAKE THIS FROM MOVEMENT MODEL ONCE YOU'VE WORKED IT OUT FOR THAT ####
