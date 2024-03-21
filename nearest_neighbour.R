#### information ####
# script for nearest neighbour analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
#library(tidyverse); library(brms) ; library(LaplacesDemon) ; library(patchwork)
library(StanHeaders, lib.loc = '../packages/')
library(rstan, lib.loc = '../packages/')
library(brms, lib.loc = '../packages/')
library(tidyverse, lib.loc = '../packages/')
library(LaplacesDemon, lib.loc = '../packages/')
library(patchwork, lib.loc = '../packages/')

theme_set(theme_classic())
set.seed(12345)

#### NEAREST NEIGHBOUR: ordinal logistic regression ####
# https://dagitty.net/dags.html?id=fQrEyF#
pdf('outputs/nn_modelprep.pdf')

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
# 1  3  4  6  7 10 11 13 14 15 16 17 18 19 21 22 23 24 25 28 29 30 31 32 33 34 35 36 37 38 41 42 43 44 45 46 47 48 50 51 52 53 55 56 58 59 60 61 
# 1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  2  1  1  2  1  1  3  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  5  1  1  1  1  1  1 
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

nn_all <- readRDS('data_processed/behaviour_by_second_indexvariables.RDS') %>%
  filter(out_frame_name != 'out_of_sight') %>%
  select(subject,pb_num,second,
         b1_nn_name,#b1_nn_index,
         b2_nn_name,#b2_nn_index,
         b3_nn_name,#b3_nn_index,
         b4_nn_name,#b4_nn_index,
         b5_nn_name,#b5_nn_index,
         b6_nn_name,#b6_nn_index,
         b7_nn_name,#b7_nn_index,
         b8_nn_name#,b8_nn_index,
  ) %>%
  pivot_longer(cols = c('b1_nn_name','b2_nn_name','b3_nn_name','b4_nn_name',
                        'b5_nn_name','b6_nn_name','b7_nn_name','b8_nn_name'),
               names_to = 'elephant_nn', values_to = 'neighbour') %>%
  filter(is.na(neighbour) == FALSE) %>%
  filter(neighbour != 'impossible_partner') %>%
  separate(elephant_nn, into = c('partner_bull','nn_name'), sep = 2, remove = F) %>%
  select(-nn_name) %>%
  rename(focal = subject) %>% 
  mutate(partner = paste0(partner_bull, '_e', pb_num)) %>%
  left_join(ages[,c('focal','f_age_cat','f_age_num')], by = 'focal') %>%
  left_join(ages[,c('partner','p_age_cat','p_age_num')], by = 'partner') %>%
  # rename(f_subject = subject,
  #        f_bull = bull,
  #        p_subject = targeted_elephant,
  #        p_bull = partner_bull,
  #        nn_binom = neighbour,
  #        age_diff_cat = age_difference) %>%
  mutate(pb_num = as.numeric(pb_num),
         age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num),
                                 'partner_younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num),
                                        'matched',
                                        'partner_older'))) %>%
  left_join(stim_starts, by = 'pb_num') %>%
  mutate(time_since_stim = second - stim_start,
         after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60))

nn <- nn_all %>%
  filter(neighbour == 1) %>%
  filter(group_size > 2) %>%
  select(-elephant_nn,-partner_bull,-neighbour,-group_size,-stim_start) %>%
  mutate(nn_tminus1 = NA)

# create variable for nearest neighbour at time t-1
subjects <- unique(nn$focal)
for(i in 1:length(subjects)){
  focal <- nn %>% filter(focal == subjects[i])
  nn <- nn %>% anti_join(focal, by = 'focal')
  for(j in 2:nrow(focal)){
    focal$nn_tminus1[j] <- focal$age_difference[j-1]
  }
  nn <- rbind(nn, focal)
}
rm(check, x, i, j, multiple_starts, focal) ; gc()

# filter to remove elephants with unknown ages
nn_no_na <- nn %>%
  mutate(age_difference = factor(age_difference,
                               levels = c('partner_younger',
                                          'matched',
                                          'partner_older'))) %>%
  mutate(age_diff_num = as.integer(age_difference),
         f_age_num = as.integer(f_age_num),
         p_age_num = as.integer(p_age_num)) %>%
  filter(is.na(age_diff_num) == FALSE) %>%
  filter(is.na(f_age_num) == FALSE) %>%
  filter(is.na(p_age_num) == FALSE) %>%
  filter(is.na(nn_tminus1) == FALSE) %>%
  mutate(focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num)
str(nn_no_na)
# $ focal          : chr [1:36838] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# $ playback_id    : num [1:36838] 1 1 1 1 1 1 1 1 1 1 ...
# $ second         : num [1:36838] 1 2 3 4 5 6 7 8 9 10 ...
# $ partner        : chr [1:36838] "b3_e1" "b3_e1" "b3_e1" "b3_e1" ...
# $ f_age_cat      : chr [1:36838] "26-35" "26-35" "26-35" "26-35" ...
# $ f_age_num      : int [1:36838] 4 4 4 4 4 4 4 4 4 4 ...
# $ p_age_cat      : chr [1:36838] "26-35" "26-35" "26-35" "26-35" ...
# $ p_age_num      : int [1:36838] 4 4 4 4 4 4 4 4 4 4 ...
# $ age_difference : Factor w/ 3 levels "partner_younger",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ stim_id        : int [1:36838] 4 4 4 4 4 4 4 4 4 4 ...
# $ stim_type      : chr [1:36838] "ctd" "ctd" "ctd" "ctd" ...
# $ time_since_stim: num [1:36838] -81 -80 -79 -78 -77 -76 -75 -74 -73 -72 ...
# $ after_stim     : num [1:36838] 0 0 0 0 0 0 0 0 0 0 ...
# $ nn_tminus1     : chr [1:36838] "matched" "matched" "matched" "matched" ...
# $ age_diff_num   : int [1:36838] 2 2 2 2 2 2 2 2 2 2 ...
# $ focal_id       : int [1:36838] 1 1 1 1 1 1 1 1 1 1 ...

## clean up
rm(ages, stim_starts, subjects) ; gc()

# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
nn_no_na <- nn_no_na %>%
  mutate(nn_tminus1_num = ifelse(nn_tminus1 == 'partner_younger', 1,
                                 ifelse(nn_tminus1 == 'matched', 2,
                                        ifelse(nn_tminus1 == 'partner_older', 3, NA)))) %>%
  mutate(nn_tminus1_num = as.integer(nn_tminus1_num))

get_prior(formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
            s(after_stim) + mo(nn_tminus1_num) +                       # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),              # random effects
          data = nn_no_na,
          family = cumulative("logit"))
priors <- c(
  # focal age
  prior(normal(0,0.25),     class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2),   class = simo, coef = mof_age_num1),
  # stim type
  prior(normal(0,1),        class = b,    coef = stim_typeh),
  prior(normal(0,1),        class = b,    coef = stim_typel),
  # time spline
  prior(normal(0,1),        class = b,    coef = safter_stim_1),
  #prior(student_t(3,0,2.5), class = sds,  coef = s(after_stim)),
  # action in previous second
  prior(normal(0,0.333),    class = b,    coef = monn_tminus1_num),
  prior(dirichlet(2,2),     class = simo, coef = monn_tminus1_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
nn_prior <- brm(
  formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
    s(after_stim) + mo(nn_tminus1_num) +                       # controls
    (1|focal_id) + (1|stim_id) + (1|playback_id),              # random effects
  data = nn_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(nn_prior) # huge variation in prior, but fairly on both sides so good

## fit model
nn_fit <- brm(
  formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
    s(after_stim) + mo(nn_tminus1_num) +                       # controls
    (1|stim_id) + (1|stim_id:playback_id) + (1|stim_id:playback_id:focal_id),  # nested random effects
  data = nn_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345)
save.image('ele_playbacks/nearest_neighbour/neighbour_model_run.RData')
dev.off()

#### check outputs ####
# load('ele_playbacks/nearest_neighbour/neighbour_model_run.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()
pdf('outputs/nn_modelchecks.pdf')

## check Stan code
nn_fit$model

## check model fit
summary(nn_fit)
# Formula: age_diff_num ~ 1 + mo(f_age_num) + stim_type + s(after_stim) + mo(nn_tminus1_num) + (1 | stim_id) + (1 | stim_id:playback_id) + (1 | stim_id:playback_id:focal_id) 
# Data: nn_no_na (Number of observations: 36838) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; total post-warmup draws = 4000
# 
# Smooth Terms: 
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(safter_stim_1)     0.52      0.53     0.02     1.96 1.00     1933     2225
# 
# Group-Level Effects: 
#   ~stim_id (Number of levels: 21) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.46      0.33     0.02     1.23 1.01      663     1126
# 
# ~stim_id:playback_id (Number of levels: 30) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.82      0.37     0.08     1.50 1.01      343      565
# 
# ~stim_id:playback_id:focal_id (Number of levels: 122) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.67      0.29     1.15     2.28 1.01      404     1160
# 
# Population-Level Effects: 
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]         1.58      0.52     0.55     2.58 1.00     1312     2171
# Intercept[2]        11.67      0.53    10.60    12.68 1.00     1250     1993
# stim_typeh           0.07      0.52    -0.96     1.08 1.00     2111     2753
# stim_typel           0.03      0.56    -1.08     1.13 1.00     1696     2553
# safter_stim_1       -0.18      0.75    -1.57     1.49 1.00     3386     2587
# mof_age_num         -1.20      0.18    -1.54    -0.85 1.01      683     1507
# monn_tminus1_num     8.51      0.11     8.29     8.73 1.00     2311     2575
# 
# Simplex Parameters: 
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mof_age_num1[1]        0.09      0.06     0.01     0.23 1.00     3258     1396
# mof_age_num1[2]        0.74      0.09     0.55     0.91 1.00     2843     2826
# mof_age_num1[3]        0.17      0.09     0.03     0.35 1.00     2012     2397
# monn_tminus1_num1[1]   0.51      0.01     0.49     0.53 1.00     3366     2723
# monn_tminus1_num1[2]   0.49      0.01     0.47     0.51 1.00     3366     2723
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message: There were 13 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 

## check chain mixing
summary <- summary(nn_fit)
hist(summary$fixed$Rhat, breaks = 50)
range(summary$fixed$Rhat)
# 1.000444 1.007834

## check effective sample size
hist(summary$fixed$Bulk_ESS, breaks = 50)
range(summary$fixed$Bulk_ESS)
# 683.3667 3385.9355
hist(summary$fixed$Tail_ESS, breaks = 50)
range(summary$fixed$Tail_ESS)
# 1507.037 2753.075

summary$random
# $stim_id
#                Estimate Est.Error   l-95% CI u-95% CI     Rhat  Bulk_ESS  Tail_ESS
# sd(Intercept) 0.4619142 0.3346158 0.02038823 1.229267 1.007116  663.3026  1125.687
# 
# $`stim_id:playback_id`
#                Estimate Est.Error   l-95% CI u-95% CI    Rhat  Bulk_ESS  Tail_ESS
# sd(Intercept) 0.8152322 0.3662419 0.07937348 1.496995 1.01456  343.0073  564.5211
# 
# $`stim_id:playback_id:focal_id`
#               Estimate Est.Error l-95% CI u-95% CI    Rhat  Bulk_ESS  Tail_ESS
# sd(Intercept) 1.669163 0.2921028 1.150674  2.28331 1.01373  403.7323  1160.486

## extract posterior distribution
draws <- as_draws_df(nn_fit) %>%
  select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>%
  pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>%
  mutate(iteration = rep(rep(1:(num_iter/2),
                             each = length(unique(parameter))),
                         num_chains),
         chain = rep(1:num_chains,
                     each = length(unique(parameter))*(num_iter/2)),
         invlogit_draw = invlogit(draw))

## extract marginal effects
marg <- conditional_effects(nn_fit,
                            effects = c('f_age_num','stim_type',
                                        'after_stim','nn_tminus1_num'),
                            categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
# "f_age_num:cats__"      "stim_type:cats__"      "after_stim:cats__"     "nn_tminus1_num:cats__"
f_age_effect <- marg[[1]]
stim_effect <- marg[[2]]
time_effect <- marg[[3]]
prevsec_effect <- marg[[4]]

#### plot marginal effects ####
# conditional_effects(nn_fit, effects = 'f_age_num',
#                     categorical = TRUE,
#                     spaghetti = TRUE,
#                     #conditions = c('stim_type'),
#                     method = 'posterior_epred')
(focal_age_plot <- ggplot(f_age_effect)+
   geom_errorbar(aes(x = f_age_num, ymax = upper__, ymin = lower__, colour = cats__), linewidth = 1, width = 0.2)+
   #geom_ribbon(aes(x = f_age_num, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
   geom_point(aes(x = f_age_num, y = estimate__, colour = cats__), cex = 3)+
   #geom_line(aes(x = f_age_num, y = estimate__, colour = cats__), linewidth = 1)+
   xlab(label = 'focal age') + ylab('probability')+
   scale_colour_viridis_d(name = 'age difference:',
                          breaks = c('1','2','3'),
                          labels = c('partner younger',
                                     'age matched',
                                     'partner older'))+
   scale_fill_viridis_d(name = 'age difference:',
                        breaks = c('1','2','3'),
                        labels = c('partner younger',
                                   'age matched',
                                   'partner older')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = focal_age_plot, filename = 'outputs/nn_marginaleffects_focalage.png', device = 'png',
       width = 8.3, height = 5.8)

# conditional_effects(nn_fit, 'stim_type',
#                     categorical = TRUE,
#                     method = 'posterior_epred')
(stim_plot <- ggplot(stim_effect)+
    geom_errorbar(aes(x = stim_type, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type, y = estimate__, colour = cats__),cex = 3)+
    xlab(label = 'stimulus type') + ylab('probability')+
    scale_colour_viridis_d(name = 'neighbour age:',
                           breaks = c('1','2','3'),
                           labels = c('younger', 'matched', 'older'))+
    scale_fill_viridis_d(name = 'neighbour age:',
                         breaks = c('1','2','3'),
                         labels = c('younger', 'matched', 'older'))+
    scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = stim_plot, filename = 'outputs/nn_marginaleffects_stimtype.png', device = 'png',
       width = 8.3, height = 5.8)

#(all_plots <- ggarrange(focal_age_plot, stim_plot, ncol=2, nrow=1, common.legend = TRUE, legend = "bottom"))
#ggsave(plot = all_plots, filename = '../outputs/nn_marginaleffects.png', device = 'png',
#       width = (5.8*2), height = 8.3)

ggsave(plot = focal_age_plot, filename = 'outputs/nn_marginaleffects_focalage.png', device = 'png',
       width = 5.8, height = 8.3)
ggsave(plot = stim_plot, filename = 'outputs/nn_marginaleffects_stimtype.png', device = 'png',
       width = 5.8, height = 8.3)

rm(f_age_effect,prevsec_effect, stim_effect, time_effect) ;gc()

#### posterior predictive check ####
pp_check(nn_fit, ndraws = 100) # perfect fit

#### plot traces ####
unique(draws$parameter)
draws_cut <- draws %>%
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "bs_safter_stim_1","sds_safter_stim_1",
                          "s_safter_stim_1[1]","s_safter_stim_1[2]",
                          "s_safter_stim_1[3]","s_safter_stim_1[4]",
                          "s_safter_stim_1[5]","s_safter_stim_1[6]",
                          "s_safter_stim_1[7]","s_safter_stim_1[8]",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "bsp_monn_tminus1_num",
                          "simo_monn_tminus1_num1[1]","simo_monn_tminus1_num1[2]"
  )) 
draws_cut %>%
  ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mostly fine, but playback ID intercept has a weird unmixed bit

#### plot density curves ####
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

## neighbour in previous second
prevsec1 <- draws_cut %>% filter(parameter == 'bsp_monn_tminus1_num')
prevsec2 <- draws_cut %>% filter(parameter == 'simo_monn_tminus1_num1[1]')
prevsec3 <- draws_cut %>% filter(parameter == 'simo_monn_tminus1_num1[2]')
par(mfrow = c(3,1))
plot(density(prevsec1$draw), main = 't-1 younger') ; abline(v = 0, lty = 2)
plot(density(prevsec2$draw), main = 't-1 matched') ; abline(v = 0, lty = 2)
plot(density(prevsec3$draw), main = 't-1 older') ; abline(v = 0, lty = 2)

## time since stimulus
timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
par(mfrow = c(5,2))
plot(density(timeb$draw), main = 'time slope') ; abline(v = 0, lty = 2)
plot(density(times$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)

#### plot raw ####
par(mfrow = c(1,1))
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

ggplot(nn_no_na, aes(x = f_age_num, y = age_diff_num,
                     colour = as.factor(nn_tminus1_num)))+
  geom_jitter(alpha = 0.1)+
  facet_wrap(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_y_continuous(name = 'neighbour age relative to focal',
                     breaks = c(1,2,3),
                     labels = c('younger','matched','older'))+
  labs(colour = 'neigbour age at t-1:')

nn_no_na %>%
  ggplot(aes(x = time_since_stim, y = age_diff_num))+
  geom_vline(aes(xintercept = 0))+
  geom_point(aes(colour = as.factor(f_age_num)#rgb(0,0,1,0.01)
  ), alpha = 0.01)+
  facet_grid(f_age_num ~ stim_type,
             labeller = labeller(f_age_num = age_labels,
                                 stim_type = stim_labels))+
  scale_y_continuous(name = 'neighbour age relative to focal',
                     breaks = c(1,2,3),
                     labels = c('younger',
                                'matched',
                                'older'))+
  scale_x_continuous(name = 'time since stimulus started (s)') # no particular effect of time since stimulus -- all seem pretty similar before and after. possible differences between stimuli: youngest are more likely to age match during control but be near older during lion/human (but that is also the case beforethe stimulus); 21-25 more likely to age match during lion than other 2; oldest switch from being near younger to age matching during lion stimuli; oldest do not age match for human stimuli; 16-20 more likely to be near youngest after lion and human than before

## plot raw data -- this looks a lot more exciting at first glance than it is: basically just shows that far and away the strongest effect is neighbour age at second t-1
stimuli <- c('dove (control)','lion','human')
names(stimuli) <- c('ctd','l','h')
nn_no_na %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
  ggplot(aes(x = after_stim, y = age_diff_num,
             colour = stim_type, shape = nn_tminus1))+
  geom_vline(aes(xintercept = 0))+
  geom_point(alpha = 0.1)+ # colour = rgb(0,0,1,0.01)
  facet_grid(stim_type ~ factor(nn_tminus1,
                                levels = c('partner younger',
                                           'matched',
                                           'partner older')),
             labeller = labeller(stim_type = stimuli))

## save output
save.image('ele_playbacks/nearest_neighbour/neighbour_model_run.RData')
dev.off()

#### predict from model -- raw data ####
#load('ele_playbacks/nearest_neighbour/neighbour_model_run.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()
rm(list = ls()[! ls() %in% c('nn_fit','nn_no_na')]) ; gc()
pdf('outputs/nn_modelpredictions.pdf')

## predict from raw data
nn_no_na$unique_data_combo <- 1:nrow(nn_no_na)
pred_mtx <- posterior_epred(object = nn_fit, newdata = nn_no_na)
colnames(pred_mtx) <- nn_no_na$unique_data_combo
save.image('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData')

## convert predictions to long format data set, using only first 100 values per chain
#load('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData')
predictions1 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,1] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
  mutate(unique_data_combo = as.integer(unique_data_combo),
         pred_type = 1) %>%
  left_join(nn_no_na, by = 'unique_data_combo')
predictions2 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,2] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
  mutate(unique_data_combo = as.integer(unique_data_combo),
         pred_type = 2) %>%
  left_join(nn_no_na, by = 'unique_data_combo')
predictions3 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,3] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
  mutate(unique_data_combo = as.integer(unique_data_combo),
         pred_type = 3) %>%
  left_join(nn_no_na, by = 'unique_data_combo')
save.image('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData')

## create long format data set with all predictions -- use for calculating some uncertainty measure (standard deviation/error amongst sets of predictions)
# predictions_all <- predictions %>%
#   mutate(chain_set100 = 1)
# for(i in 2:10){
#   j <- seq(from = 100*(i-1)+1, to = (100*i), by = 1)
#   pred <- pred_mtx[c(j,1000+j,2000+j,3000+j),] %>%
#     as.data.frame() %>%
#     pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
#     mutate(unique_data_combo = as.integer(unique_data_combo)) %>%
#     left_join(nn_no_na, by = 'unique_data_combo') %>%
#     mutate(chain_set100 = i)
#   predictions_all <- rbind(predictions_all, pred)
# }
predictions_all <- rbind(predictions1, predictions2, predictions3)
save.image('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData')

#### plot predictions -- posterior_epred() ####
#load('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData')  #load('nearest_neighbour/neighbour_model_predictions.RData')
rm(pred_mtx, predictions1, predictions2, predictions3) ; gc()

## make labels for prediction type look nice
predictions_all <- predictions_all %>%
  mutate(pred_label = ifelse(pred_type == 1, 'younger',
                             ifelse(pred_type == 2, 'age matched', 'older')))

## make labels for age of neighbour in previous second look nice
prevsec_labels <- c('neighbour younger at t-1',
                    'neighbour same age at t-1',
                    'neighbour older at t-1')
names(prevsec_labels) <- 1:3

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
(ctd_plot <- predictions_all %>%
    filter(stim_type == 'ctd',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = as.factor(pred_label),
                    colour = as.factor(pred_label))) +
    # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
    #                 colour = as.factor(pred_label)))+
    facet_grid(nn_tminus1_num ~ after_stim,
               labeller = labeller(nn_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted age of neighbour relative to focal:',
         fill = 'predicted age of neighbour relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'cape turtle dove (control)')+
    theme(legend.position = 'bottom'))
(lion_plot <- predictions_all %>%
    filter(stim_type == 'l',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = as.factor(pred_label),
                    colour = as.factor(pred_label))) +
    # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
    #                 colour = as.factor(pred_label)))+
    facet_grid(nn_tminus1_num ~ after_stim,
               labeller = labeller(nn_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted age of neighbour relative to focal:',
         fill = 'predicted age of neighbour relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'lion')+
    theme(legend.position = 'bottom'))
(human_plot <- predictions_all %>%
    filter(stim_type == 'h',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = as.factor(pred_label),
                    colour = as.factor(pred_label))) +
    # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
    #                 colour = as.factor(pred_label)))+
    facet_grid(nn_tminus1_num ~ after_stim,
               labeller = labeller(nn_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted age of neighbour relative to focal:',
         fill = 'predicted age of neighbour relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'human')+
    theme(legend.position = 'bottom'))
(ctd_plot + lion_plot + human_plot)+
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(), file = '../outputs/nn_predictions_violin.png',
       device = 'png', height = 8, width = 48)

## save output
save.image('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData')
dev.off()

#### graph contrasts from predictions and extract coefficients ####
#CALCULATE POSTERIOR CONTRASTS FROM PREDICTIONS
# load('ele_playbacks/nearest_neighbour/neighbour_model_predictions.RData') ; load('nearest_neighbour/neighbour_model_predictions.RData')
rm(prevsec_labels, ctd_plot, human_plot, lion_plot, predictions_all) ; gc()
pdf('outputs/nn_modelcontrasts.pdf')

## stim type ####
## redo predictions with different stimulus types: all doves
ctd_nn <- nn_no_na %>%
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(stim_type = 'ctd',
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
ctd_mtx <- posterior_epred(object = nn_fit, newdata = ctd_nn)
colnames(ctd_mtx) <- ctd_nn$unique_data_combo
ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all lions
lion_nn <- nn_no_na %>%
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(stim_type = 'l',
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
lion_mtx <- posterior_epred(object = nn_fit, newdata = lion_nn)
colnames(lion_mtx) <- lion_nn$unique_data_combo
lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all humans
human_nn <- nn_no_na %>%
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(stim_type = 'h',
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
human_mtx <- posterior_epred(object = nn_fit, newdata = human_nn)
colnames(human_mtx) <- human_nn$unique_data_combo
human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('ele_playbacks/nearest_neighbour/neighbour_model_stimuluscontrasts_epred.RData')

## count types of each prediction
#load('nearest_neighbour/neighbour_model_stimuluscontrasts_epred.RData')
# count_values <- function(vector, levels = c(1,2,3)) {
#   x <- tabulate(factor(vector, levels), length(levels))
#   return(list(x))
# }
stim_pred <- ctd_nn %>%
  dplyr::select(-stim_type) %>%
  # mutate(ctd_count = apply(ctd_mtx, 2, count_values),
  #        lion_count = apply(lion_mtx, 2, count_values),
  #        human_count = apply(human_mtx, 2, count_values)) %>%
  # unnest(c(ctd_count, lion_count, human_count)) %>% # I've done something weird with the count_values function so for now this needs unnesting twice, but should probably fix it at some point! For now this works!
  # unnest(c(ctd_count, lion_count, human_count)) %>%
  mutate(ctd_prop1_mu = apply(ctd_mtx[,,1], 2, mean),
         ctd_prop2_mu = apply(ctd_mtx[,,2], 2, mean),
         ctd_prop3_mu = apply(ctd_mtx[,,3], 2, mean),
         ctd_prop1_sd = apply(ctd_mtx[,,1], 2, sd),
         ctd_prop2_sd = apply(ctd_mtx[,,2], 2, sd),
         ctd_prop3_sd = apply(ctd_mtx[,,3], 2, sd),
         lion_prop1_mu = apply(lion_mtx[,,1], 2, mean),
         lion_prop2_mu = apply(lion_mtx[,,2], 2, mean),
         lion_prop3_mu = apply(lion_mtx[,,3], 2, mean),
         lion_prop1_sd = apply(lion_mtx[,,1], 2, sd),
         lion_prop2_sd = apply(lion_mtx[,,2], 2, sd),
         lion_prop3_sd = apply(lion_mtx[,,3], 2, sd),
         human_prop1_mu = apply(human_mtx[,,1], 2, mean),
         human_prop2_mu = apply(human_mtx[,,2], 2, mean),
         human_prop3_mu = apply(human_mtx[,,3], 2, mean),
         human_prop1_sd = apply(human_mtx[,,1], 2, sd),
         human_prop2_sd = apply(human_mtx[,,2], 2, sd),
         human_prop3_sd = apply(human_mtx[,,3], 2, sd)) %>%
  pivot_longer(cols = c(ctd_prop1_mu,ctd_prop2_mu,ctd_prop3_mu,
                        lion_prop1_mu,lion_prop2_mu,lion_prop3_mu,
                        human_prop1_mu,human_prop2_mu,human_prop3_mu),
               names_to = 'stim_propage_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(ctd_prop1_sd,ctd_prop2_sd,ctd_prop3_sd,
                        lion_prop1_sd,lion_prop2_sd,lion_prop3_sd,
                        human_prop1_sd,human_prop2_sd,human_prop3_sd),
               names_to = 'stim_propage_sd', values_to = 'stdv_propn') %>%
  separate(col = stim_propage_mu, into = c('stim_propage_mu','mu'),
           sep = '_m', remove = T) %>%
  select(-mu) %>%
  separate(col = stim_propage_sd, into = c('stim_propage_sd','sd'),
           sep = '_s', remove = T) %>%
  select(-sd) %>%
  filter(stim_propage_mu == stim_propage_sd) %>%
  separate(col = stim_propage_mu, into = c('stim_type', 'nn_pred'),
           sep = '_prop', remove = T) %>%
  select(-stim_propage_sd) %>%
  mutate(nn_pred = as.numeric(nn_pred)) %>%
  mutate(pred_type = ifelse(nn_pred == 1, 'younger',
                            ifelse(nn_pred == 2, 'matched', 'older')))

## convert full predictive distribution to long format
stim_pred_all <- ctd_mtx[,,1] %>%
  as.data.frame()
colnames(stim_pred_all) <- rownames(ctd_nn)
stim_pred_all <- pivot_longer(stim_pred_all, cols = everything(),
                              names_to = 'rownum', values_to = 'probability')
ctd_nn$rownum <- rownames(ctd_nn)
stim_pred_all <- stim_pred_all %>%
  left_join(ctd_nn, by = 'rownum') %>%
  mutate(predict_num = 1,
         predict_cat = 'younger')
for(i in 2:3){
  stim_pred_i <- ctd_mtx[,,i] %>%
    as.data.frame()
  colnames(stim_pred_i) <- rownames(ctd_nn)
  stim_pred_i <- pivot_longer(stim_pred_i, cols = everything(),
                            names_to = 'rownum', values_to = 'probability')
  stim_pred_i <- stim_pred_i %>%
    left_join(ctd_nn, by = 'rownum') %>%
    mutate(predict_num = i,
           predict_cat = ifelse(i == 2, 'matched', 'older'))
  stim_pred_all <- rbind(stim_pred_all, stim_pred_i)
}

## calculate contrasts
ctd_vs_lion_age1 <- lion_mtx[,,1] - ctd_mtx[,,1]
ctd_vs_lion_age2 <- lion_mtx[,,2] - ctd_mtx[,,2]
ctd_vs_lion_age3 <- lion_mtx[,,3] - ctd_mtx[,,3]
ctd_vs_human_age1 <- human_mtx[,,1] - ctd_mtx[,,1]
ctd_vs_human_age2 <- human_mtx[,,2] - ctd_mtx[,,2]
ctd_vs_human_age3 <- human_mtx[,,3] - ctd_mtx[,,3]
lion_vs_human_age1 <- human_mtx[,,1] - lion_mtx[,,1]
lion_vs_human_age2 <- human_mtx[,,2] - lion_mtx[,,2]
lion_vs_human_age3 <- human_mtx[,,3] - lion_mtx[,,3]

## summarise contrasts
contrasts <- nn_no_na %>%
  select(-stim_type) %>%
  mutate(ctd_vs_lion_age1_mu = apply(ctd_vs_lion_age1, 2, mean),
         ctd_vs_lion_age1_sd = apply(ctd_vs_lion_age1, 2, sd),
         ctd_vs_lion_age2_mu = apply(ctd_vs_lion_age2, 2, mean),
         ctd_vs_lion_age2_sd = apply(ctd_vs_lion_age2, 2, sd),
         ctd_vs_lion_age3_mu = apply(ctd_vs_lion_age3, 2, mean),
         ctd_vs_lion_age3_sd = apply(ctd_vs_lion_age3, 2, sd),
         ctd_vs_human_age1_mu = apply(ctd_vs_human_age1, 2, mean),
         ctd_vs_human_age1_sd = apply(ctd_vs_human_age1, 2, sd),
         ctd_vs_human_age2_mu = apply(ctd_vs_human_age2, 2, mean),
         ctd_vs_human_age2_sd = apply(ctd_vs_human_age2, 2, sd),
         ctd_vs_human_age3_mu = apply(ctd_vs_human_age3, 2, mean),
         ctd_vs_human_age3_sd = apply(ctd_vs_human_age3, 2, sd),
         lion_vs_human_age1_mu = apply(lion_vs_human_age1, 2, mean),
         lion_vs_human_age1_sd = apply(lion_vs_human_age1, 2, sd),
         lion_vs_human_age2_mu = apply(lion_vs_human_age2, 2, mean),
         lion_vs_human_age2_sd = apply(lion_vs_human_age2, 2, sd),
         lion_vs_human_age3_mu = apply(lion_vs_human_age3, 2, mean),
         lion_vs_human_age3_sd = apply(lion_vs_human_age3, 2, sd))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(ctd_vs_lion_age1_mu,ctd_vs_lion_age2_mu,ctd_vs_lion_age3_mu,
                        ctd_vs_human_age1_mu,ctd_vs_human_age2_mu,ctd_vs_human_age3_mu,
                        lion_vs_human_age1_mu,lion_vs_human_age2_mu,lion_vs_human_age3_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('contrast','nn_pred'),
           sep = '_age', remove = T) %>%
  separate(nn_pred, into = c('nn_pred','mu'),
           sep = '_', remove = T) %>%
  mutate(nn_pred = as.numeric(nn_pred)) %>%
  mutate(pred_type = ifelse(nn_pred == 1, 'younger',
                            ifelse(nn_pred == 2, 'matched', 'older'))) %>%
  select(-mu, -ctd_vs_lion_age1_sd, -ctd_vs_lion_age2_sd, -ctd_vs_lion_age3_sd,
         -ctd_vs_human_age1_sd, -ctd_vs_human_age2_sd, -ctd_vs_human_age3_sd,
         -lion_vs_human_age1_sd, -lion_vs_human_age2_sd, -lion_vs_human_age3_sd)

## plot contrasts
# stim_pred %>%
#   dplyr::select(ctd_lion, ctd_human, lion_human, pred_type) %>%
#   pivot_longer(cols = c('ctd_lion', 'ctd_human', 'lion_human'),
#                names_to = 'contrast') %>%
#   ggplot()+
#   geom_density(aes(x = value, colour = contrast))+
#   facet_wrap(. ~ pred_type)

stim_pred %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = as.factor(f_age_num)))+
  facet_wrap(pred_type ~ stim_type)

stim_pred_all %>%
  ggplot()+
  geom_density(aes(x = probability, colour = predict_cat))+
  facet_wrap(stim_type ~ f_age_num, scales = 'free_y')

contrasts_long <- contrasts_long %>%
  mutate(pred_type = ifelse(nn_pred == 1, 'younger',
                            ifelse(nn_pred == 2, 'matched', 'older'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('younger','matched','older'))) %>%
  separate(contrast, into = c('stim_a','stim_b'), sep = '_vs_', remove = F) %>%
  select(pred_type, f_age_num, difference, stim_a, stim_b, contrast, after_stim, nn_tminus1_num) %>%
  mutate(nn_tminus1 = ifelse(nn_tminus1_num == 1,
                             'neighbour younger at t-1',
                             ifelse(nn_tminus1_num == 2,
                                    'neighbour matched at t-1',
                                    'neighbour older at t-1')))

contrasts_long %>%
  ggplot()+
  geom_density(aes(x = difference))+
  facet_grid(pred_type ~ contrast)

for(i in unique(contrasts_long$contrast)){
  plot <- contrasts_long %>%
    filter(contrast == i) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = pred_type))+
    facet_grid(nn_tminus1 ~ f_age_num,
               scales = 'free')+
    labs(title = i)
  print(plot)
}

save.image('ele_playbacks/nearest_neighbour/neighbour_model_stimuluscontrasts_epred.RData')

## focal age ####
# load('nearest_neighbour/neighbour_model_stimuluscontrasts_epred.RData')
rm(ctd_nn, ctd_mtx, human_nn, human_mtx, lion_nn, lion_mtx,
   contrasts, contrasts_long, stim_pred,
   ctd_vs_human_age1, ctd_vs_human_age2, ctd_vs_human_age3,
   ctd_vs_lion_age1, ctd_vs_lion_age2, ctd_vs_lion_age3,
   lion_vs_human_age1, lion_vs_human_age2, lion_vs_human_age3) ; gc()

## predict with original ages
age_nn_org <- nn_no_na %>%
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
age_mtx_org <- posterior_epred(object = nn_fit, newdata = age_nn_org)
colnames(age_mtx_org) <- age_nn_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with altered ages
age_nn_alt <- nn_no_na %>%
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(f_age_num_original = f_age_num) %>%
  mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1),
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
  relocate(f_age_num_original)
age_mtx_alt <- posterior_epred(object = nn_fit, newdata = age_nn_alt)
colnames(age_mtx_alt) <- age_nn_alt$unique_data_combo
age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
save.image('ele_playbacks/nearest_neighbour/neighbour_model_agecontrasts_epred.RData')

## summarise and convert to long format
age_pred <- age_nn_org %>%
  #dplyr::select(-f_age_num) %>%
  mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
         age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
         age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
         age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
         age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
         age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
         age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
         age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
         age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
         age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
         age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
         age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd)) %>%
  pivot_longer(cols = c(age_org_prop1_mu,age_org_prop2_mu,age_org_prop3_mu,
                        age_alt_prop1_mu,age_alt_prop2_mu,age_alt_prop3_mu),
               names_to = 'focal_agenn_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(age_org_prop1_sd,age_org_prop2_sd,age_org_prop3_sd,
                        age_alt_prop1_sd,age_alt_prop2_sd,age_alt_prop3_sd),
               names_to = 'focal_agenn_sd', values_to = 'stdv_propn') %>%
  separate(col = focal_agenn_mu, into = c('focal_agenn_mu','mu'),
           sep = '_m', remove = T) %>%
  separate(col = focal_agenn_sd, into = c('focal_agenn_sd','sd'),
           sep = '_s', remove = T) %>%
  select(-mu, -sd) %>%
  filter(focal_agenn_mu == focal_agenn_sd) %>%
  separate(col = focal_agenn_mu, into = c('original_altered', 'nn_pred'),
           sep = '_prop', remove = T) %>%
  select(-focal_agenn_sd) %>%
  mutate(nn_pred = as.numeric(nn_pred),
         f_age_num = ifelse(original_altered == 'age_org',
                            f_age_num,
                            ifelse(original_altered == 'age_alt' & f_age_num == 4,
                                   1, f_age_num + 1))) %>%
  mutate(pred_type = ifelse(nn_pred == 1, 'younger',
                            ifelse(nn_pred == 2, 'matched', 'older')))

## convert full predictive distribution to long format
age_pred_all <- age_mtx_org[,,1] %>%
  as.data.frame()
colnames(age_pred_all) <- rownames(age_nn_org)
age_pred_all <- pivot_longer(age_pred_all, cols = everything(),
                             names_to = 'rownum', values_to = 'probability')
age_nn_org$rownum <- rownames(age_nn_org)
age_pred_all <- age_pred_all %>%
  left_join(age_nn_org, by = 'rownum') %>%
  mutate(predict_num = 1,
         predict_cat = 'younger')
for(i in 2:3){
  age_pred_i <- age_mtx_org[,,i] %>%
    as.data.frame()
  colnames(age_pred_i) <- rownames(age_nn_org)
  age_pred_i <- pivot_longer(age_pred_i, cols = everything(),
                             names_to = 'rownum', values_to = 'probability')
  age_pred_i <- age_pred_i %>%
    left_join(age_nn_org, by = 'rownum') %>%
    mutate(predict_num = i,
           predict_cat = ifelse(i == 2, 'matched', 'older'))
  age_pred_all <- rbind(age_pred_all, age_pred_i)
}

## calculate contrasts
alt_vs_org_young <- age_mtx_alt[,,1] - age_mtx_org[,,1]
alt_vs_org_match <- age_mtx_alt[,,2] - age_mtx_org[,,2]
alt_vs_org_older <- age_mtx_alt[,,3] - age_mtx_org[,,3]

## summarise contrasts
contrasts <- nn_no_na %>%
  mutate(alt_vs_org_young_mu = apply(alt_vs_org_young, 2, mean),
         alt_vs_org_young_sd = apply(alt_vs_org_young, 2, sd),
         alt_vs_org_match_mu = apply(alt_vs_org_match, 2, mean),
         alt_vs_org_match_sd = apply(alt_vs_org_match, 2, sd),
         alt_vs_org_older_mu = apply(alt_vs_org_older, 2, mean),
         alt_vs_org_older_sd = apply(alt_vs_org_older, 2, sd))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(alt_vs_org_young_mu,alt_vs_org_match_mu,alt_vs_org_older_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','vs','org','nn_pred','mu'),
           sep = '_', remove = T) %>%
  select(-alt_vs_org_young_sd, -alt_vs_org_match_sd, -alt_vs_org_older_sd, -alt, -vs, -org, -mu)

## plot contrasts
# age_pred %>%
#   dplyr::select(ctd_lion, ctd_human, lion_human, pred_type) %>%
#   pivot_longer(cols = c('ctd_lion', 'ctd_human', 'lion_human'),
#                names_to = 'contrast') %>%
#   ggplot()+
#   geom_density(aes(x = value, colour = contrast))+
#   facet_wrap(. ~ pred_type)

age_pred %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = as.factor(f_age_num)))+
  facet_wrap(stim_type ~ pred_type, scales = 'free_y')

age_pred_all %>%
  ggplot()+
  geom_density(aes(x = probability, colour = predict_cat))+
  facet_wrap(stim_type ~ f_age_num, scales = 'free_y')

contrasts_long <- contrasts_long %>%
  mutate(pred_type = ifelse(nn_pred == 'young', 'younger',
                            ifelse(nn_pred == 'match', 'matched', 'older'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('younger','matched','older'))) %>%
  mutate(f_age_new = ifelse(f_age_num == 4, 1, f_age_num+1)) %>%
  select(pred_type, f_age_num, f_age_new, difference, stim_type, after_stim, nn_tminus1_num) %>%
  mutate(contrast = paste0('org: ',f_age_num,', new: ', f_age_new)) %>%
  mutate(nn_tminus1 = ifelse(nn_tminus1_num == 1,
                             'neighbour younger at t-1',
                             ifelse(nn_tminus1_num == 2,
                                    'neighbour matched at t-1',
                                    'neighbour older at t-1')))

for(i in unique(contrasts_long$contrast)){
  plot <- contrasts_long %>%
    filter(contrast == i) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = pred_type))+
    facet_grid(nn_tminus1 ~ stim_type,
               scales = 'free')+
    labs(title = i)
  print(plot)
}

save.image('ele_playbacks/nearest_neighbour/neighbour_model_agecontrasts_epred.RData')

## neighbour in previous second ####
#load('nearest_neighbour/neighbour_model_agecontrasts_epred.RData')
rm(age_nn_org, age_mtx_org, age_nn_alt, age_mtx_alt, age_pred, alt_vs_org_young, alt_vs_org_match, alt_vs_org_older, contrasts, contrasts_long) ; gc()

## redo predictions with different previous neighbours: all younger -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 1, NN AT T-1 YOUNGER
young_nn <- nn_no_na %>%
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(nn_tminus1_num = 1,
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
young_mtx <- posterior_epred(object = nn_fit, newdata = young_nn)
colnames(young_mtx) <- young_nn$unique_data_combo
young_mtx <- young_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different previous neighbours: all matching
match_nn <- nn_no_na %>%
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(nn_tminus1_num = 2,
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
match_mtx <- posterior_epred(object = nn_fit, newdata = match_nn)
colnames(match_mtx) <- match_nn$unique_data_combo
match_mtx <- match_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different previous neighbours: all older -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 4, NN AT T-1 OLDER
older_nn <- nn_no_na %>%
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>%
  mutate(nn_tminus1_num = 3,
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
older_mtx <- posterior_epred(object = nn_fit, newdata = older_nn)
colnames(older_mtx) <- older_nn$unique_data_combo
older_mtx <- older_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('ele_playbacks/nearest_neighbour/neighbour_model_prevseccontrasts_epred.RData')

## summarise and convert to long format
load('ele_playbacks/nearest_neighbour/neighbour_model_prevseccontrasts_epred.RData')
prevsec_pred <- young_nn %>%
  dplyr::select(-nn_tminus1_num) %>%
  mutate(young_prop1_mu = apply(young_mtx[,,1], 2, mean),
         young_prop2_mu = apply(young_mtx[,,2], 2, mean),
         young_prop3_mu = apply(young_mtx[,,3], 2, mean),
         young_prop1_sd = apply(young_mtx[,,1], 2, sd),
         young_prop2_sd = apply(young_mtx[,,2], 2, sd),
         young_prop3_sd = apply(young_mtx[,,3], 2, sd),
         match_prop1_mu = apply(match_mtx[,,1], 2, mean),
         match_prop2_mu = apply(match_mtx[,,2], 2, mean),
         match_prop3_mu = apply(match_mtx[,,3], 2, mean),
         match_prop1_sd = apply(match_mtx[,,1], 2, sd),
         match_prop2_sd = apply(match_mtx[,,2], 2, sd),
         match_prop3_sd = apply(match_mtx[,,3], 2, sd),
         older_prop1_mu = apply(older_mtx[,,1], 2, mean),
         older_prop2_mu = apply(older_mtx[,,2], 2, mean),
         older_prop3_mu = apply(older_mtx[,,3], 2, mean),
         older_prop1_sd = apply(older_mtx[,,1], 2, sd),
         older_prop2_sd = apply(older_mtx[,,2], 2, sd),
         older_prop3_sd = apply(older_mtx[,,3], 2, sd)) %>%
  pivot_longer(cols = c(young_prop1_mu,young_prop2_mu,young_prop3_mu,
                        match_prop1_mu,match_prop2_mu,match_prop3_mu,
                        older_prop1_mu,older_prop2_mu,older_prop3_mu),
               names_to = 'prevsec_propage_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(young_prop1_sd,young_prop2_sd,young_prop3_sd,
                        match_prop1_sd,match_prop2_sd,match_prop3_sd,
                        older_prop1_sd,older_prop2_sd,older_prop3_sd),
               names_to = 'prevsec_propage_sd', values_to = 'stdv_propn') %>%
  separate(col = prevsec_propage_mu, into = c('prevsec_propage_mu','mu'),
           sep = '_m', remove = T) %>%
  separate(col = prevsec_propage_sd, into = c('prevsec_propage_sd','sd'),
           sep = '_s', remove = T) %>%
  select(-mu, -sd) %>%
  filter(prevsec_propage_mu == prevsec_propage_sd) %>%
  separate(col = prevsec_propage_mu, into = c('prevsec_type', 'nn_pred'),
           sep = '_prop', remove = T) %>%
  select(-prevsec_propage_sd) %>%
  mutate(nn_pred = as.numeric(nn_pred)) %>%
  mutate(pred_type = ifelse(nn_pred == 1, 'younger',
                            ifelse(nn_pred == 2, 'matched', 'older')))

## convert full predictive distribution to long format
make_long <- function(matrix, data){
  colnames(matrix) <- rownames(data)
  long <- matrix %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(),
                 names_to = 'rownum', values_to = 'probability') %>%
    left_join(data, by = 'rownum')
  return(long)
}

young_nn$rownum <- rownames(young_nn)
match_nn$rownum <- rownames(match_nn)
older_nn$rownum <- rownames(older_nn)
for(i in 1:3){
  for(j in 1:3){
    if(i == 1){
      matrix <- young_mtx[,,j]
      data <- young_nn
    }
    if(i == 2){
      matrix <- match_mtx[,,j]
      data <- match_nn
    }
    if(i == 3){
      matrix <- older_mtx[,,j]
      data <- older_nn
    }
    if( i == 1 & j == 1 ){
      prevsec_pred_all <- make_long(matrix, data) %>%
        mutate(predict_num = 1,
               predict_cat = 'younger')
    } else {
      prevsec_pred_new <- make_long(matrix, data) %>%
        mutate(predict_num = i,
               predict_cat = ifelse(i == 1, 'younger',
                                    ifelse(i == 2, 'matched', 'older')))
      prevsec_pred_all <- rbind(prevsec_pred_all, prevsec_pred_new)
    }
  }
}

## calculate contrasts
young_vs_match_age1 <- match_mtx[,,1] - young_mtx[,,1]
young_vs_match_age2 <- match_mtx[,,2] - young_mtx[,,2]
young_vs_match_age3 <- match_mtx[,,3] - young_mtx[,,3]
young_vs_older_age1 <- older_mtx[,,1] - young_mtx[,,1]
young_vs_older_age2 <- older_mtx[,,2] - young_mtx[,,2]
young_vs_older_age3 <- older_mtx[,,3] - young_mtx[,,3]
match_vs_older_age1 <- older_mtx[,,1] - match_mtx[,,1]
match_vs_older_age2 <- older_mtx[,,2] - match_mtx[,,2]
match_vs_older_age3 <- older_mtx[,,3] - match_mtx[,,3]

## summarise contrasts
contrasts <- nn_no_na %>%
  select(-nn_tminus1_num) %>%
  mutate(young_vs_match_age1_mu = apply(young_vs_match_age1, 2, mean),
         young_vs_match_age1_sd = apply(young_vs_match_age1, 2, sd),
         young_vs_match_age2_mu = apply(young_vs_match_age2, 2, mean),
         young_vs_match_age2_sd = apply(young_vs_match_age2, 2, sd),
         young_vs_match_age3_mu = apply(young_vs_match_age3, 2, mean),
         young_vs_match_age3_sd = apply(young_vs_match_age3, 2, sd),
         young_vs_older_age1_mu = apply(young_vs_older_age1, 2, mean),
         young_vs_older_age1_sd = apply(young_vs_older_age1, 2, sd),
         young_vs_older_age2_mu = apply(young_vs_older_age2, 2, mean),
         young_vs_older_age2_sd = apply(young_vs_older_age2, 2, sd),
         young_vs_older_age3_mu = apply(young_vs_older_age3, 2, mean),
         young_vs_older_age3_sd = apply(young_vs_older_age3, 2, sd),
         match_vs_older_age1_mu = apply(match_vs_older_age1, 2, mean),
         match_vs_older_age1_sd = apply(match_vs_older_age1, 2, sd),
         match_vs_older_age2_mu = apply(match_vs_older_age2, 2, mean),
         match_vs_older_age2_sd = apply(match_vs_older_age2, 2, sd),
         match_vs_older_age3_mu = apply(match_vs_older_age3, 2, mean),
         match_vs_older_age3_sd = apply(match_vs_older_age3, 2, sd))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(young_vs_match_age1_mu,young_vs_match_age2_mu,young_vs_match_age3_mu,
                        young_vs_older_age1_mu,young_vs_older_age2_mu,young_vs_older_age3_mu,
                        match_vs_older_age1_mu,match_vs_older_age2_mu,match_vs_older_age3_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('contrast','nn_pred'),
           sep = '_age', remove = T) %>%
  separate(nn_pred, into = c('nn_pred','mu'),
           sep = '_', remove = T) %>%
  mutate(nn_pred = as.numeric(nn_pred)) %>%
  mutate(pred_type = ifelse(nn_pred == 1, 'younger',
                            ifelse(nn_pred == 2, 'matched', 'older'))) %>%
  select(-mu, -young_vs_match_age1_sd, -young_vs_match_age2_sd, -young_vs_match_age3_sd,
         -young_vs_older_age1_sd, -young_vs_older_age2_sd, -young_vs_older_age3_sd,
         -match_vs_older_age1_sd, -match_vs_older_age2_sd, -match_vs_older_age3_sd)

## plot contrasts
prevsec_pred %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = as.factor(prevsec_type)))+
  facet_wrap(stim_type ~ pred_type, scales = 'free_y')

prevsec_pred_all %>%
  ggplot()+
  geom_density(aes(x = probability, colour = predict_cat))+
  facet_wrap(stim_type ~ f_age_num, scales = 'free_y')

contrasts_long <- contrasts_long %>%
  mutate(pred_type = ifelse(nn_pred == 1, 'younger',
                            ifelse(nn_pred == 2, 'matched', 'older'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('younger','matched','older'))) %>%
  separate(contrast, into = c('prevsec_a','prevsec_b'), sep = '_vs_', remove = F) %>%
  select(pred_type, f_age_num, prevsec_a, prevsec_b, contrast, difference, stim_type, after_stim)

for(i in unique(contrasts_long$contrast)){
  ( plot <- contrasts_long %>%
    filter(contrast == i) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = pred_type))+
    facet_grid(as.factor(f_age_num) ~ stim_type,
               scales = 'free')+
    labs(title = i) )
  print(plot)
}

save.image('ele_playbacks/nearest_neighbour/neighbour_model_prevseccontrasts_epred.RData')

# predictions %>%
#   mutate(previous = ifelse(nn_tminus1_num == 1, 'younger',
#                            ifelse(nn_tminus1_num == 2, 'same age', 'older')),
#          prediction = ifelse(prediction == 1, 'younger',
#                              ifelse(prediction == 2, 'same age', 'older'))) %>%
#   mutate(previous = factor(previous, levels = c('younger','same age','older')),
#          prediction = factor(prediction, levels = c('younger','same age','older'))) %>%
#   ggplot()+
#   geom_bar(aes(x = prediction, fill = as.factor(previous)),
#            position = 'dodge')+
#   scale_y_continuous(expand = c(0,0))+
#   labs(colour = 'previous second')+
#   scale_fill_viridis_d()
#
# prevsec2 <- pred_prop %>% filter(nn_tminus1_num == 2)
# prevsec3 <- pred_prop %>% filter(nn_tminus1_num == 3)
# pred_prev <- pred_prop %>%
#   filter(nn_tminus1_num == 1) %>%
#   rename(count_1 = count_predictions,
#          prop_1 = proportion) %>%
#   select(f_age_num, after_stim, stim_type, prediction, count_1, prop_1) %>%
#   left_join(prevsec2[,c('f_age_num','after_stim','stim_type','prediction','count_predictions','proportion')],
#             by = c('f_age_num','after_stim','stim_type','prediction')) %>%
#   rename(count_2 = count_predictions,
#          prop_2 = proportion) %>%
#   left_join(prevsec3[,c('f_age_num','after_stim','stim_type','prediction','count_predictions','proportion')],
#             by = c('f_age_num','after_stim','stim_type','prediction')) %>%
#   rename(count_3 = count_predictions,
#          prop_3 = proportion) %>%
#   mutate(nn1_2 = prop_1 - prop_2,
#          nn1_3 = prop_1 - prop_3,
#          nn2_3 = prop_2 - prop_3)
# pred_prev %>%
#   select(stim_type, after_stim, f_age_num, prediction,
#          nn1_2,nn1_3,nn2_3) %>%
#   pivot_longer(cols = c('nn1_2', 'nn1_3', 'nn2_3'),
#                names_to = 'contrast', values_to = 'value') %>%
#   mutate(contrast = ifelse(contrast == 'nn1_2', 'younger vs same',
#                            ifelse(contrast == 'nn1_3', 'younger vs older',
#                                   'same vs older'))) %>%
#   ggplot()+
#   geom_density(aes(x = value, colour = contrast), linewidth = 1)+
#   scale_colour_viridis_d()+
#   labs(colour = 't-1 pair',
#        x = 'difference between neighbours at previous second')+
#   geom_vline(xintercept = 0, linetype = 2)+
#   scale_x_continuous(limits = c(-2,2))

## time since stimulus ####
# load('nearest_neighbour/neighbour_model_prevseccontrasts_epred.RData')
rm(young_nn, young_mtx, match_nn, match_mtx, older_nn, older_mtx,
   contrasts, contrasts_long, prevsec_pred,
   young_vs_match_age1, young_vs_match_age2, young_vs_match_age3,
   young_vs_older_age1, young_vs_older_age2, young_vs_older_age3,
   match_vs_older_age1, match_vs_older_age2, match_vs_older_age3) ; gc()

## predict with original times
time_nn_org <- nn_no_na %>% 
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>% 
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
time_mtx_org <- posterior_epred(object = nn_fit, newdata = time_nn_org)
colnames(time_mtx_org) <- time_nn_org$unique_data_combo
time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +15 seconds
time_nn_alt_0.25 <- nn_no_na %>% 
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>% 
  mutate(after_stim_org = after_stim) %>% 
  mutate(after_stim = after_stim + 1/4,
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
  relocate(after_stim_org)
time_mtx_alt_0.25 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.25)
colnames(time_mtx_alt_0.25) <- time_nn_alt_0.25$unique_data_combo
time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +30 seconds
time_nn_alt_0.50 <- nn_no_na %>% 
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>% 
  mutate(after_stim_org = after_stim) %>% 
  mutate(after_stim = after_stim + 1/2,
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
  relocate(after_stim_org)
time_mtx_alt_0.50 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.50)
colnames(time_mtx_alt_0.50) <- time_nn_alt_0.50$unique_data_combo
time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +45 seconds
time_nn_alt_0.75 <- nn_no_na %>% 
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>% 
  mutate(after_stim_org = after_stim) %>% 
  mutate(after_stim = after_stim + 3/4,
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
  relocate(after_stim_org)
time_mtx_alt_0.75 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.75)
colnames(time_mtx_alt_0.75) <- time_nn_alt_0.75$unique_data_combo
time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with shifted times: +60 seconds
time_nn_alt_1.00 <- nn_no_na %>% 
  dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
                focal_id, stim_id, playback_id) %>% 
  mutate(after_stim_org = after_stim) %>% 
  mutate(after_stim = after_stim + 1,
         unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
  relocate(after_stim_org)
time_mtx_alt_1.00 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_1.00)
colnames(time_mtx_alt_1.00) <- time_nn_alt_1.00$unique_data_combo
time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('ele_playbacks/nearest_neighbour/neighbour_model_timecontrasts_epred.RData')

## summarise and convert to long format
time_pred <- time_nn_org %>% 
  mutate(time_org_0.00_prop1_mu = apply(time_mtx_org[,,1], 2, mean),
         time_org_0.00_prop2_mu = apply(time_mtx_org[,,2], 2, mean),
         time_org_0.00_prop3_mu = apply(time_mtx_org[,,3], 2, mean),
         time_org_0.00_prop1_sd = apply(time_mtx_org[,,1], 2, sd),
         time_org_0.00_prop2_sd = apply(time_mtx_org[,,2], 2, sd),
         time_org_0.00_prop3_sd = apply(time_mtx_org[,,3], 2, sd),
         time_alt_0.25_prop1_mu = apply(time_mtx_alt_0.25[,,1], 2, mean),
         time_alt_0.25_prop2_mu = apply(time_mtx_alt_0.25[,,2], 2, mean),
         time_alt_0.25_prop3_mu = apply(time_mtx_alt_0.25[,,3], 2, mean),
         time_alt_0.25_prop1_sd = apply(time_mtx_alt_0.25[,,1], 2, sd),
         time_alt_0.25_prop2_sd = apply(time_mtx_alt_0.25[,,2], 2, sd),
         time_alt_0.25_prop3_sd = apply(time_mtx_alt_0.25[,,3], 2, sd),
         time_alt_0.50_prop1_mu = apply(time_mtx_alt_0.50[,,1], 2, mean),
         time_alt_0.50_prop2_mu = apply(time_mtx_alt_0.50[,,2], 2, mean),
         time_alt_0.50_prop3_mu = apply(time_mtx_alt_0.50[,,3], 2, mean),
         time_alt_0.50_prop1_sd = apply(time_mtx_alt_0.50[,,1], 2, sd),
         time_alt_0.50_prop2_sd = apply(time_mtx_alt_0.50[,,2], 2, sd),
         time_alt_0.50_prop3_sd = apply(time_mtx_alt_0.50[,,3], 2, sd),
         time_alt_0.75_prop1_mu = apply(time_mtx_alt_0.75[,,1], 2, mean),
         time_alt_0.75_prop2_mu = apply(time_mtx_alt_0.75[,,2], 2, mean),
         time_alt_0.75_prop3_mu = apply(time_mtx_alt_0.75[,,3], 2, mean),
         time_alt_0.75_prop1_sd = apply(time_mtx_alt_0.75[,,1], 2, sd),
         time_alt_0.75_prop2_sd = apply(time_mtx_alt_0.75[,,2], 2, sd),
         time_alt_0.75_prop3_sd = apply(time_mtx_alt_0.75[,,3], 2, sd),
         time_alt_1.00_prop1_mu = apply(time_mtx_alt_1.00[,,1], 2, mean),
         time_alt_1.00_prop2_mu = apply(time_mtx_alt_1.00[,,2], 2, mean),
         time_alt_1.00_prop3_mu = apply(time_mtx_alt_1.00[,,3], 2, mean),
         time_alt_1.00_prop1_sd = apply(time_mtx_alt_1.00[,,1], 2, sd),
         time_alt_1.00_prop2_sd = apply(time_mtx_alt_1.00[,,2], 2, sd),
         time_alt_1.00_prop3_sd = apply(time_mtx_alt_1.00[,,3], 2, sd)) %>% 
  pivot_longer(cols = c(time_org_0.00_prop1_mu,time_org_0.00_prop2_mu,time_org_0.00_prop3_mu,
                        time_alt_0.25_prop1_mu,time_alt_0.25_prop2_mu,time_alt_0.25_prop3_mu,
                        time_alt_0.50_prop1_mu,time_alt_0.50_prop2_mu,time_alt_0.50_prop3_mu,
                        time_alt_0.75_prop1_mu,time_alt_0.75_prop2_mu,time_alt_0.75_prop3_mu,
                        time_alt_1.00_prop1_mu,time_alt_1.00_prop2_mu,time_alt_1.00_prop3_mu),
               names_to = 'time_org_alt_prop_agenn_mu', values_to = 'mean_propn') %>% 
  pivot_longer(cols = c(time_org_0.00_prop1_sd,time_org_0.00_prop2_sd,time_org_0.00_prop3_sd,
                        time_alt_0.25_prop1_sd,time_alt_0.25_prop2_sd,time_alt_0.25_prop3_sd,
                        time_alt_0.50_prop1_sd,time_alt_0.50_prop2_sd,time_alt_0.50_prop3_sd,
                        time_alt_0.75_prop1_sd,time_alt_0.75_prop2_sd,time_alt_0.75_prop3_sd,
                        time_alt_1.00_prop1_sd,time_alt_1.00_prop2_sd,time_alt_1.00_prop3_sd),
               names_to = 'time_org_alt_prop_agenn_sd', values_to = 'stdv_propn') %>% 
  separate(col = time_org_alt_prop_agenn_mu,
           into = c('time_mu','org_mu','alt_mu','prop_agenn_mu','mu'),
           sep = '_', remove = T) %>% 
  separate(col = time_org_alt_prop_agenn_sd,
           into = c('time_sd','org_sd','alt_sd','prop_agenn_sd','sd'),
           sep = '_', remove = T) %>% 
  select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>% 
  filter(alt_mu == alt_sd & prop_agenn_sd == prop_agenn_mu) %>% 
  mutate(nn_pred = ifelse(prop_agenn_mu == 'prop1', 1,
                          ifelse(prop_agenn_mu == 'prop2', 2,
                                 ifelse(prop_agenn_mu == 'prop3', 3, 4)))) %>% 
  select(-alt_sd, -prop_agenn_mu, -prop_agenn_sd) %>% 
  rename(mins_added = alt_mu) %>% 
  mutate(pred_type = ifelse(nn_pred == 1, 'younger',
                            ifelse(nn_pred == 2, 'matched', 'older')))

## convert full predictive distribution to long format
time_pred_all <- time_mtx_org[,,1] %>% 
  as.data.frame()
colnames(time_pred_all) <- rownames(time_nn_org)
time_pred_all <- pivot_longer(time_pred_all, cols = everything(),
                             names_to = 'rownum', values_to = 'probability')
time_nn_org$rownum <- rownames(time_nn_org)
time_pred_all <- time_pred_all %>% 
  left_join(time_nn_org, by = 'rownum') %>% 
  mutate(predict_num = 1,
         predict_cat = 'younger')
for(i in 2:3){
  time_pred_new <- time_mtx_org[,,i] %>% 
    as.data.frame()
  colnames(time_pred_new) <- rownames(time_nn_org)
  time_pred_new <- pivot_longer(time_pred_new, cols = everything(),
                           names_to = 'rownum', values_to = 'probability')
  time_pred_new <- time_pred_new %>% 
    left_join(time_nn_org, by = 'rownum') %>% 
    mutate(predict_num = i,
           predict_cat = ifelse(i == 2, 'matched', 'older'))
  time_pred_all <- rbind(time_pred_all, time_pred_new)
}

## calculate contrasts
alt0.25_vs_0.00_young <- time_mtx_alt_0.25[,,1] - time_mtx_org[,,1]
alt0.25_vs_0.00_match <- time_mtx_alt_0.25[,,2] - time_mtx_org[,,2]
alt0.25_vs_0.00_older <- time_mtx_alt_0.25[,,3] - time_mtx_org[,,3]

alt0.50_vs_0.25_young <- time_mtx_alt_0.50[,,1] - time_mtx_alt_0.25[,,1]
alt0.50_vs_0.25_match <- time_mtx_alt_0.50[,,2] - time_mtx_alt_0.25[,,2]
alt0.50_vs_0.25_older <- time_mtx_alt_0.50[,,3] - time_mtx_alt_0.25[,,3]

alt0.75_vs_0.50_young <- time_mtx_alt_0.75[,,1] - time_mtx_alt_0.50[,,1]
alt0.75_vs_0.50_match <- time_mtx_alt_0.75[,,2] - time_mtx_alt_0.50[,,2]
alt0.75_vs_0.50_older <- time_mtx_alt_0.75[,,3] - time_mtx_alt_0.50[,,3]

alt1.00_vs_0.75_young <- time_mtx_alt_1.00[,,1] - time_mtx_alt_0.75[,,1]
alt1.00_vs_0.75_match <- time_mtx_alt_1.00[,,2] - time_mtx_alt_0.75[,,2]
alt1.00_vs_0.75_older <- time_mtx_alt_1.00[,,3] - time_mtx_alt_0.75[,,3]

## summarise contrasts
contrasts <- nn_no_na %>% 
  mutate(alt0.25_vs_0.00_young_mu = apply(alt0.25_vs_0.00_young, 2, mean),
         alt0.25_vs_0.00_young_sd = apply(alt0.25_vs_0.00_young, 2, sd),
         alt0.25_vs_0.00_match_mu = apply(alt0.25_vs_0.00_match, 2, mean),
         alt0.25_vs_0.00_match_sd = apply(alt0.25_vs_0.00_match, 2, sd),
         alt0.25_vs_0.00_older_mu = apply(alt0.25_vs_0.00_older, 2, mean),
         alt0.25_vs_0.00_older_sd = apply(alt0.25_vs_0.00_older, 2, sd),
         alt0.50_vs_0.25_young_mu = apply(alt0.50_vs_0.25_young, 2, mean),
         alt0.50_vs_0.25_young_sd = apply(alt0.50_vs_0.25_young, 2, sd),
         alt0.50_vs_0.25_match_mu = apply(alt0.50_vs_0.25_match, 2, mean),
         alt0.50_vs_0.25_match_sd = apply(alt0.50_vs_0.25_match, 2, sd),
         alt0.50_vs_0.25_older_mu = apply(alt0.50_vs_0.25_older, 2, mean),
         alt0.50_vs_0.25_older_sd = apply(alt0.50_vs_0.25_older, 2, sd),
         alt0.75_vs_0.50_young_mu = apply(alt0.75_vs_0.50_young, 2, mean),
         alt0.75_vs_0.50_young_sd = apply(alt0.75_vs_0.50_young, 2, sd),
         alt0.75_vs_0.50_match_mu = apply(alt0.75_vs_0.50_match, 2, mean),
         alt0.75_vs_0.50_match_sd = apply(alt0.75_vs_0.50_match, 2, sd),
         alt0.75_vs_0.50_older_mu = apply(alt0.75_vs_0.50_older, 2, mean),
         alt0.75_vs_0.50_older_sd = apply(alt0.75_vs_0.50_older, 2, sd),
         alt1.00_vs_0.75_young_mu = apply(alt1.00_vs_0.75_young, 2, mean),
         alt1.00_vs_0.75_young_sd = apply(alt1.00_vs_0.75_young, 2, sd),
         alt1.00_vs_0.75_match_mu = apply(alt1.00_vs_0.75_match, 2, mean),
         alt1.00_vs_0.75_match_sd = apply(alt1.00_vs_0.75_match, 2, sd),
         alt1.00_vs_0.75_older_mu = apply(alt1.00_vs_0.75_older, 2, mean),
         alt1.00_vs_0.75_older_sd = apply(alt1.00_vs_0.75_older, 2, sd))
contrasts_long <- contrasts %>% 
  select(-alt0.25_vs_0.00_young_sd,-alt0.25_vs_0.00_match_sd,-alt0.25_vs_0.00_older_sd,
         -alt0.50_vs_0.25_young_sd,-alt0.50_vs_0.25_match_sd,-alt0.50_vs_0.25_older_sd,
         -alt0.75_vs_0.50_young_sd,-alt0.75_vs_0.50_match_sd,-alt0.75_vs_0.50_older_sd,
         -alt1.00_vs_0.75_young_sd,-alt1.00_vs_0.75_match_sd,-alt1.00_vs_0.75_older_sd) %>% 
  pivot_longer(cols = c(alt0.25_vs_0.00_young_mu,alt0.25_vs_0.00_match_mu,alt0.25_vs_0.00_older_mu,
                        alt0.50_vs_0.25_young_mu,alt0.50_vs_0.25_match_mu,alt0.50_vs_0.25_older_mu,
                        alt0.75_vs_0.50_young_mu,alt0.75_vs_0.50_match_mu,alt0.75_vs_0.50_older_mu,
                        alt1.00_vs_0.75_young_mu,alt1.00_vs_0.75_match_mu,alt1.00_vs_0.75_older_mu),
               names_to = 'contrast', values_to = 'difference') %>% 
  separate(contrast, into = c('alt','contrast'), sep = 3) %>% 
  separate(contrast, into = c('later','vs','earlier','pred_type','mu'),
           sep = '_', remove = T) %>% 
  select(-alt, -vs, -mu) %>% 
  mutate(later = as.numeric(later),
         earlier = as.numeric(earlier),
         contrast = paste0(later,'_',earlier))

## plot contrasts
times <- unique(time_pred$after_stim)
time_pred %>% 
  filter(after_stim %in% times[seq(1, length(times), length.out = 8)]) %>% 
  mutate(after_stim = round(after_stim, 2)) %>% 
  ggplot()+
  geom_density(aes(x = mean_propn, colour = pred_type))+
  facet_wrap(as.factor(after_stim) ~ stim_type,
             scales = 'free')

time_pred_all %>% 
  ggplot()+
  geom_density(aes(x = probability, colour = predict_cat))+
  facet_wrap(stim_type ~ f_age_num, scales = 'free_y')

contrasts_long <- contrasts_long %>% 
  mutate(pred_type = ifelse(pred_type == 'young', 'younger',
                            ifelse(pred_type == 'match', 'matched', 'older'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('younger','matched','older'))) %>%
  select(pred_type, f_age_num, contrast, earlier, later, difference, stim_type, after_stim, nn_tminus1_num) %>%
  mutate(nn_tminus1 = ifelse(nn_tminus1_num == 1,
                             'neighbour younger at t-1',
                             ifelse(nn_tminus1_num == 2,
                                    'neighbour matched at t-1',
                                    'neighbour older at t-1')))

for(i in unique(contrasts_long$contrast)){
  plot <- contrasts_long %>% 
    filter(contrast == i) %>% 
    filter(after_stim %in% times[seq(1, length(times), length.out = 8)]) %>% 
    mutate(after_stim = round(after_stim, 2)) %>% 
    ggplot()+
    geom_density(aes(x = difference, colour = pred_type))+
    facet_grid(nn_tminus1 ~ after_stim,
               scales = 'free')+
    labs(title = i)
  print(plot)
}

save.image('ele_playbacks/nearest_neighbour/neighbour_model_timecontrasts_epred.RData')
dev.off()
