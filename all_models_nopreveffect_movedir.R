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

############ movement direction                                      ####
rm(list = ls()[! ls() %in% c('behav','move','num_iter','num_chains')]) ; gc()
set.seed(12345)
pdf('../outputs/movement_ordinal_model_2bda/movement_noprev_2bda_modelprep.pdf')

behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')
move <- behav %>%
  filter(activity == 'move')

num_chains <- 4
num_iter <- 2000

#### create data                                                     ####
## remove observations where not moving or out of sight
move <- move %>%
  filter(action_name != 'not_moving') %>%
  filter(action_name != 'out_of_sight')

## rename columns
move <- move %>%
  rename(move_index = action_index,
         moving_direction = action_name)

## remove NA
move <- move %>%
  filter(!is.na(p_age_num)) %>%
  filter(!is.na(f_age_num))

## ensure correct data format
move <- move %>%
  mutate(f_age_num = as.integer(f_age_num))
str(move)
# $ pb_num          : num [1:31907] 1 1 1 1 1 1 1 1 1 1 ...
# $ focal           : chr [1:31907] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# $ partner         : chr [1:31907] "b2_e1" "b3_e1" "b4_e1" "b2_e1" ...
# $ activity        : chr [1:31907] "move" "move" "move" "move" ...
# $ moving_direction: chr [1:31907] "move away directly" "move away directly" "move away directly" "move away directly" ...
# $ move_index      : num [1:31907] 1 1 1 1 1 1 1 1 1 1 ...
# $ stim_num        : chr [1:31907] "14" "14" "14" "14" ...
# $ stim_type       : chr [1:31907] "ctd" "ctd" "ctd" "ctd" ...
# $ stim_start      : num [1:31907] 82 82 82 82 82 82 82 82 82 82 ...
# $ stim_stop       : num [1:31907] 105 105 105 105 105 105 105 105 105 105 ...
# $ second          : num [1:31907] 71 71 71 72 72 72 73 73 73 74 ...
# $ bda             : chr [1:31907] "before" "before" "before" "before" ...
# $ f_age_cat       : chr [1:31907] "26-35" "26-35" "26-35" "26-35" ...
# $ p_age_cat       : chr [1:31907] "26-35" "26-35" "26-35" "26-35" ...
# $ f_age_num       : int [1:31907] 4 4 4 4 4 4 4 4 4 4 ...
# $ p_age_num       : Factor w/ 4 levels "1","2","3","4": 4 4 4 4 4 4 4 4 4 4 ...
# $ age_combo       : chr [1:31907] "4_4" "4_4" "4_4" "4_4" ...

#### set prior                                                       ####
get_prior(formula = move_index ~ mo(f_age_num) + age_combo + stim_type * bda +
            (1|focal) + (1|stim_num) + (1|pb_num),
          data = move, family = cumulative("logit"))
#                prior     class                 coef    group resp dpar nlpar lb ub       source
#               (flat)         b                                                          default
#               (flat)         b         age_combo1_2                                (vectorized)
#               (flat)         b         age_combo1_3                                (vectorized)
#               (flat)         b         age_combo1_4                                (vectorized)
#               (flat)         b         age_combo2_1                                (vectorized)
#               (flat)         b         age_combo2_2                                (vectorized)
#               (flat)         b         age_combo2_3                                (vectorized)
#               (flat)         b         age_combo2_4                                (vectorized)
#               (flat)         b         age_combo3_1                                (vectorized)
#               (flat)         b         age_combo3_2                                (vectorized)
#               (flat)         b         age_combo3_3                                (vectorized)
#               (flat)         b         age_combo3_4                                (vectorized)
#               (flat)         b         age_combo4_1                                (vectorized)
#               (flat)         b         age_combo4_2                                (vectorized)
#               (flat)         b         age_combo4_3                                (vectorized)
#               (flat)         b         age_combo4_4                                (vectorized)
#               (flat)         b            bdabefore                                (vectorized)
#               (flat)         b            bdaduring                                (vectorized)
#               (flat)         b          mof_age_num                                (vectorized)
#               (flat)         b           stim_typeh                                (vectorized)
#               (flat)         b stim_typeh:bdabefore                                (vectorized)
#               (flat)         b stim_typeh:bdaduring                                (vectorized)
#               (flat)         b           stim_typel                                (vectorized)
#               (flat)         b stim_typel:bdabefore                                (vectorized)
#               (flat)         b stim_typel:bdaduring                                (vectorized)
# student_t(3, 0, 2.5) Intercept                                                          default
# student_t(3, 0, 2.5) Intercept                    1                                (vectorized)
# student_t(3, 0, 2.5) Intercept                    2                                (vectorized)
# student_t(3, 0, 2.5) Intercept                    3                                (vectorized)
# student_t(3, 0, 2.5) Intercept                    4                                (vectorized)
# student_t(3, 0, 2.5)        sd                                                0         default
# student_t(3, 0, 2.5)        sd                         focal                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept    focal                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                        pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept   pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                      stim_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept stim_num                  0    (vectorized)
#         dirichlet(1)      simo         mof_age_num1                                     default

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
  # interaction
  prior(normal(0,1),      class = b,    coef = stim_typeh:bdabefore),
  prior(normal(0,1),      class = b,    coef = stim_typeh:bdaduring),
  prior(normal(0,1),      class = b,    coef = stim_typel:bdabefore),
  prior(normal(0,1),      class = b,    coef = stim_typel:bdaduring))

## prior predictive check
mom_prior <- brm(
  formula = move_index ~ mo(f_age_num) + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = move, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(mom_prior) # huge variation in prior, but fairly on both sides so good

## reset plotting
dev.off()
pdf('../outputs/movement_ordinal_model_2bda/movement_noprev_2bda_modelchecks.pdf')

#### fit model                                                       ####
mom_fit <- brm(
  formula = move_index ~ mo(f_age_num) + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = move, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)
# Warning messages:
# 1: There were 1 divergent transitions after warmup. See https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup to find out why this is a problem and how to eliminate them.
# 2: There were 1 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 3: Examine the pairs() plot to diagnose sampling problems
save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')

#### extract draws                                                   ####
# load('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')

## check model diagnostics
(summary <- summary(mom_fit))
# Family: cumulative
# Links: mu = logit; disc = identity
# Formula: move_index ~ mo(f_age_num) + age_combo + stim_type * bda + (1 | focal) + (1 | stim_num) + (1 | pb_num)
# Data: move (Number of observations: 31907)
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; total post-warmup draws = 4000
# 
# Group-Level Effects:
# ~focal (Number of levels: 155)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.47      0.10     1.30     1.67 1.00      963     1979
# 
# ~pb_num (Number of levels: 45)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.16      0.12     0.01     0.46 1.01      416      732
# 
# ~stim_num (Number of levels: 29)
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.16      0.13     0.01     0.49 1.00      614     1117
# 
# Population-Level Effects:
#                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]            -0.74      0.42    -1.58     0.06 1.00      843     1616
# Intercept[2]             0.27      0.42    -0.57     1.07 1.00      844     1609
# Intercept[3]             0.57      0.42    -0.27     1.37 1.00      844     1653
# Intercept[4]             1.54      0.42     0.70     2.34 1.00      842     1664
# age_combo1_2             0.02      0.11    -0.19     0.24 1.00     5274     3532
# age_combo1_3             0.21      0.12    -0.03     0.45 1.00     4822     3387
# age_combo1_4             0.20      0.15    -0.08     0.49 1.00     5943     3182
# age_combo2_1             0.18      0.33    -0.47     0.80 1.00      827     1408
# age_combo2_2            -0.10      0.32    -0.73     0.51 1.00      835     1348
# age_combo2_3             0.48      0.32    -0.16     1.08 1.00      822     1201
# age_combo2_4            -0.65      0.32    -1.30    -0.04 1.00      871     1220
# age_combo3_1             0.15      0.32    -0.50     0.79 1.00     1202     1957
# age_combo3_2            -0.01      0.32    -0.64     0.62 1.00     1148     1752
# age_combo3_3             0.19      0.32    -0.45     0.81 1.00     1148     1675
# age_combo3_4            -1.21      0.32    -1.85    -0.59 1.00     1184     1710
# age_combo4_1             0.29      0.42    -0.54     1.11 1.00     1540     2041
# age_combo4_2             0.35      0.41    -0.47     1.15 1.00     1441     1901
# age_combo4_3             0.49      0.41    -0.32     1.28 1.00     1439     1987
# age_combo4_4            -0.45      0.42    -1.25     0.35 1.00     1474     2129
# stim_typeh               0.06      0.28    -0.49     0.63 1.00      824     1711
# stim_typel               0.62      0.34    -0.05     1.29 1.01      904     1439
# bdabefore                0.06      0.04    -0.01     0.14 1.00     4739     3184
# bdaduring               -0.26      0.07    -0.39    -0.13 1.00     4579     2564
# stim_typeh:bdabefore     0.16      0.06     0.04     0.28 1.00     4901     3364
# stim_typel:bdabefore    -0.66      0.07    -0.81    -0.52 1.00     5610     3397
# stim_typeh:bdaduring     0.33      0.09     0.16     0.50 1.00     5170     2742
# stim_typel:bdaduring     0.23      0.11     0.02     0.45 1.00     5104     3403
# mof_age_num             -0.05      0.22    -0.47     0.38 1.00      948     1628
# 
# Simplex Parameters:
#                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mof_age_num1[1]     0.33      0.18     0.05     0.71 1.00     6102     2872
# mof_age_num1[2]     0.33      0.17     0.05     0.71 1.00     6539     2876
# mof_age_num1[3]     0.33      0.18     0.05     0.71 1.00     5438     2968
# 
# Family Specific Parameters:
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS and Tail_ESS are effective sample size measures, and Rhat is the potential scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message: There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

par(mfrow = c(3,1))
hist(summary$fixed$Rhat, breaks = 50)
hist(summary$fixed$Bulk_ESS, breaks = 50)
hist(summary$fixed$Tail_ESS, breaks = 50)
par(mfrow = c(1,1))

## extract posterior distribution
draws <- as_draws_df(mom_fit) %>%
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
marg <- conditional_effects(mom_fit,
                            effects = c('f_age_num','age_combo','stim_type','bda'),
                            categorical = TRUE,
                            method = 'posterior_epred')
names(marg) # "f_age_num:cats__" "age_combo:cats__" "stim_type:cats__" "bda:cats__"
agefocal_effect <- marg[[grep('f_age_num', names(marg))]]
agecombo_effect <- marg[[grep('age_combo', names(marg))]]
stim_effect <- marg[[which(names(marg) %in% c('stim_type', 'stim_type:cats__'))]] 
bda_effect <- marg[[which(names(marg) %in% c('bda', 'bda:cats__'))]]
if(length(marg) == 5){
  interaction <- marg[[grep('stim_type:bda', names(marg))]]
}

#### plot marginal effects                                           ####
(f_age_num_plot <- ggplot(agefocal_effect)+
   geom_errorbar(aes(x = f_age_num,
                     colour = cats__,
                     ymax = upper__, ymin = lower__),
                 linewidth = 1, width = 0.2)+
   geom_point(aes(x = f_age_num,
                  colour = cats__,
                  y = estimate__),
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
ggsave(plot = f_age_num_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_noprev_2bda_marginaleffects_agefocal.png', device = 'png',
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
      y = estimate__),
      size = 3)+
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
    scale_x_discrete(name = 'partner age category')+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = agecombo_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_noprev_2bda_marginaleffects_agepartner.png', device = 'png',
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
ggsave(plot = stim_plot, filename = '../outputs/movement_ordinal_model_2bda/moving_noprev_2bda_marginaleffects_stimtype.png',
       device = 'png', width = 8.3, height = 5.8)
print(paste0('marginal effects plotted at ',Sys.time()))

## move at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
b_int3 <- draws %>% filter(parameter == 'b_Intercept[3]')
b_int4 <- draws %>% filter(parameter == 'b_Intercept[4]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int3$draw) ; hist(b_int4$draw)
par(mfrow = c(1,1))

#### posterior predictive check                                      ####
pp_check(mom_fit, ndraws = 100) # really good fit

#### plot traces and density curves                                  ####
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
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

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

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))

#### plot raw                                                        ####
## define labels for plotting
f_age_labels <- c('F: 10-15 years','F: 16-20 years','F: 21-25 years','F: 26-35 years')
names(f_age_labels) <- c(1,2,3,4)
p_age_labels <- c('T: 10-15 years','T: 16-20 years','T: 21-25 years','T: 26-35 years')
names(p_age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot control data
move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = bda, y = move_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.05))+
  facet_grid(p_age_num ~ f_age_num,
             labeller = labeller(f_age_num = f_age_labels,
                                 p_age_num = p_age_labels))+
  scale_y_continuous(name = 'focal moving direction relative to target',
                     breaks = c(1:5),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  scale_x_discrete(name = 'time relative to stimulus')+
  labs(title = 'cape turtle dove (control)')

move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = move_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
  facet_grid(p_age_num ~ f_age_num,
             labeller = labeller(f_age_num = f_age_labels,
                                 p_age_num = p_age_labels),
             drop = F)+
  scale_x_continuous(name = 'focal moving direction relative to target',
                     breaks = c(1:5),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(title = 'cape turtle dove (control)')

## plot lion data
move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = bda, y = move_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.05))+
  facet_grid(p_age_num ~ f_age_num,
             labeller = labeller(f_age_num = f_age_labels,
                                 p_age_num = p_age_labels))+
  scale_y_continuous(name = 'focal moving direction relative to target',
                     breaks = c(1:5),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  scale_x_discrete(name = 'time relative to stimulus')+
  labs(title = 'lion')

move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = move_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
  facet_grid(p_age_num ~ f_age_num,
             labeller = labeller(f_age_num = f_age_labels,
                                 p_age_num = p_age_labels),
             drop = F)+
  scale_x_continuous(name = 'focal moving direction relative to target',
                     breaks = c(1:5),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(title = 'lion')

## plot human data
move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = bda, y = move_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.05))+
  facet_grid(p_age_num ~ f_age_num,
             labeller = labeller(f_age_num = f_age_labels,
                                 p_age_num = p_age_labels))+
  scale_y_continuous(name = 'focal moving direction relative to target',
                     breaks = c(1:5),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  scale_x_discrete(name = 'time relative to stimulus')+
  labs(title = 'human')

move %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = move_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
  facet_grid(p_age_num ~ f_age_num,
             labeller = labeller(f_age_num = f_age_labels,
                                 p_age_num = p_age_labels),
             drop = F)+
  scale_x_continuous(name = 'focal moving direction relative to target',
                     breaks = c(1:5),
                     labels = c('move away directly',
                                'move away at an angle',
                                'move neither towards or away',
                                'approach at an angle',
                                'approach directly'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
  labs(title = 'human')

print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')
dev.off()

#### predict from model                                              ####
# load('movement_direction/ordinal_noprev/moving_noprev_2bda_run.RData')
rm(list = ls()[! ls() %in% c('mom_fit','move','behav','num_chains','num_iter')]) ; gc()
pdf('../outputs/movement_ordinal_model_2bda/moving_noprev_2bda_modelpredictions.pdf')

pred <- posterior_epred(object = mom_fit,
                        newdata = move)
save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')

## convert to data frame
move$data_row <- 1:nrow(move)
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
pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = move)
pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = move)
pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = move)
pred4 <- extract_predictions(prediction_array = pred, layer = 4, df = move)
pred5 <- extract_predictions(prediction_array = pred, layer = 5, df = move)

saveRDS(pred1, '../data_processed/move_dir_noprev_predictions_awaydirect.RDS')
saveRDS(pred2, '../data_processed/move_dir_noprev_predictions_awayangle.RDS')
saveRDS(pred3, '../data_processed/move_dir_noprev_predictions_neitherdir.RDS')
saveRDS(pred4, '../data_processed/move_dir_noprev_predictions_towardangle.RDS')
saveRDS(pred5, '../data_processed/move_dir_noprev_predictions_towarddirect.RDS')

pred <- rbind(pred1, pred2, pred3, pred4, pred5)
saveRDS(pred, '../data_processed/move_dir_noprev_predictions.RDS')

rm(pred1, pred2, pred3, pred4, pred5) ; gc()
save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions                                                ####
# load('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')
pred %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion','human'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_violin(aes(x = f_age_cat, y = epred,
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
  facet_grid(stim_type ~ bda)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(colour = 'predicted direction of movement relative to focal:',
       fill = 'predicted direction of movement relative to focal:',
       x = 'age category of focal elephant',
       y = 'proportion of predictions')

## reset plotting
dev.off()

#### calculate posterior contrasts from predictions                  ####
# load('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')
pdf('../outputs/movement_ordinal_model_2bda/movement_noprev_model2_modelcontrasts.pdf')

## stim type                                                         ####
stim_new <- move %>%
  dplyr::select(f_age_num, age_combo, stim_type, bda,
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, bda,
                                                         focal, stim_num, pb_num))))

## redo predictions with different stimulus types: all doves
ctd_before <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'before')
ctd_before_mtx <- posterior_epred(object = mom_fit, newdata = ctd_before)
colnames(ctd_before_mtx) <- ctd_before$unique_data_combo
ctd_before_mtx <- ctd_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

ctd_during <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'during')
ctd_during_mtx <- posterior_epred(object = mom_fit, newdata = ctd_during)
colnames(ctd_during_mtx) <- ctd_during$unique_data_combo
ctd_during_mtx <- ctd_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

ctd_after <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'after')
ctd_after_mtx <- posterior_epred(object = mom_fit, newdata = ctd_after)
colnames(ctd_after_mtx) <- ctd_after$unique_data_combo
ctd_after_mtx <- ctd_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all lions
lion_before <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'before')
lion_before_mtx <- posterior_epred(object = mom_fit, newdata = lion_before)
colnames(lion_before_mtx) <- lion_before$unique_data_combo
lion_before_mtx <- lion_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

lion_during <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'during')
lion_during_mtx <- posterior_epred(object = mom_fit, newdata = lion_during)
colnames(lion_during_mtx) <- lion_during$unique_data_combo
lion_during_mtx <- lion_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

lion_after <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'after')
lion_after_mtx <- posterior_epred(object = mom_fit, newdata = lion_after)
colnames(lion_after_mtx) <- lion_after$unique_data_combo
lion_after_mtx <- lion_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all humans
human_before <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'before')
human_before_mtx <- posterior_epred(object = mom_fit, newdata = human_before)
colnames(human_before_mtx) <- human_before$unique_data_combo
human_before_mtx <- human_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

human_during <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'during')
human_during_mtx <- posterior_epred(object = mom_fit, newdata = human_during)
colnames(human_during_mtx) <- human_during$unique_data_combo
human_during_mtx <- human_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

human_after <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'after')
human_after_mtx <- posterior_epred(object = mom_fit, newdata = human_after)
colnames(human_after_mtx) <- human_after$unique_data_combo
human_after_mtx <- human_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_stimuluscontrasts.RData')

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
contrasts <- move %>%
  select(-stim_type) %>%
  mutate(ctd_vs_lion_before = apply(ctd_vs_lion_before, 2, mean),
         ctd_vs_human_before = apply(ctd_vs_human_before, 2, mean),
         lion_vs_human_before = apply(lion_vs_human_before, 2, mean),
         
         ctd_vs_lion_during = apply(ctd_vs_lion_during, 2, mean),
         ctd_vs_human_during = apply(ctd_vs_human_during, 2, mean),
         lion_vs_human_during = apply(lion_vs_human_during, 2, mean),
         
         ctd_vs_lion_after = apply(ctd_vs_lion_after, 2, mean),
         ctd_vs_human_after = apply(ctd_vs_human_after, 2, mean),
         lion_vs_human_after = apply(lion_vs_human_after, 2, mean),
         
         before_vs_during_ctd = apply(ctd_before_vs_during, 2, mean),
         before_vs_after_ctd = apply(ctd_before_vs_after, 2, mean),
         during_vs_after_ctd = apply(ctd_during_vs_after, 2, mean),
         
         before_vs_during_lion = apply(lion_before_vs_during, 2, mean),
         before_vs_after_lion = apply(lion_before_vs_after, 2, mean),
         during_vs_after_lion = apply(lion_during_vs_after, 2, mean),
         
         before_vs_during_human = apply(human_before_vs_during, 2, mean),
         before_vs_after_human = apply(human_before_vs_after, 2, mean),
         during_vs_after_human = apply(human_during_vs_after, 2, mean))

contrasts_long <- contrasts %>%
  pivot_longer(cols = c(ctd_vs_lion_before, ctd_vs_human_before, lion_vs_human_before,
                        ctd_vs_lion_during, ctd_vs_human_during, lion_vs_human_during,
                        ctd_vs_lion_after, ctd_vs_human_after, lion_vs_human_after,
                        before_vs_during_ctd, before_vs_after_ctd, during_vs_after_ctd,
                        before_vs_during_lion, before_vs_after_lion, during_vs_after_lion,
                        before_vs_during_human, before_vs_after_human, during_vs_after_human),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('original','vs','altered','context'),
           sep = '_', remove = F) %>%
  select(-vs)

## produce values for reporting
print('dove vs lion -- before')
median(ctd_vs_lion_before)  ; mean(ctd_vs_lion_before)  ; sd(ctd_vs_lion_before)
quantile(ctd_vs_lion_before, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_vs_lion_before < 0)) / length(ctd_vs_lion_before) ) * 100
# "dove vs lion -- before"
# -0.0003931697
# 2.498126e-17
# 0.04489826
#          2.5%           50%         97.5%
# -0.0989171204 -0.0003931697  0.1066938246
# 51.90628
print('dove vs human -- before')
median(ctd_vs_human_before) ; mean(ctd_vs_human_before) ; sd(ctd_vs_human_before)
quantile(ctd_vs_human_before, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_vs_human_before < 0)) / length(ctd_vs_human_before) ) * 100
# "dove vs human -- before"
# 0.0001962928
# -1.076202e-18
# 0.04061287
#          2.5%           50%         97.5%
# -0.0963036610  0.0001962928  0.0910058276
# 48.62877
print('lion vs human -- before')
median(lion_vs_human_before); mean(lion_vs_human_before); sd(lion_vs_human_before)
quantile(lion_vs_human_before, prob = c(0.025, 0.5, 0.975))
( length(which(lion_vs_human_before < 0)) / length(lion_vs_human_before) ) * 100
# "lion vs human -- before"
# 0.0006152352
# 6.477275e-18
# 0.05227421
#          2.5%           50%         97.5%
# -0.1246563796  0.0006152352  0.1125558453
# 47.09632

print('dove vs lion -- during')
median(ctd_vs_lion_during)  ; mean(ctd_vs_lion_during)  ; sd(ctd_vs_lion_during)
quantile(ctd_vs_lion_during, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_vs_lion_during < 0)) / length(ctd_vs_lion_during) ) * 100
# "dove vs lion -- during"
# 0.002529335
# 1.097817e-16
# 0.1012384
#          2.5%          50%        97.5%
# -0.227770445  0.002529335  0.225385816
# 47.23296
print('dove vs human -- during')
median(ctd_vs_human_during) ; mean(ctd_vs_human_during) ; sd(ctd_vs_human_during)
quantile(ctd_vs_human_during, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_vs_human_during < 0)) / length(ctd_vs_human_during) ) * 100
# "dove vs human -- during"
# 0.002334388
# -3.010792e-17
# 0.05352549
#          2.5%          50%        97.5%
# -0.132889366  0.002334388  0.111847999
# 44.18667
print('lion vs human -- during')
median(lion_vs_human_during); mean(lion_vs_human_during); sd(lion_vs_human_during)
quantile(lion_vs_human_during, prob = c(0.025, 0.5, 0.975))
( length(which(lion_vs_human_during < 0)) / length(lion_vs_human_during) ) * 100
# "lion vs human -- during"
# 0.0003890309
# -4.829903e-18
# 0.06419151
#          2.5%           50%         97.5%
# -0.1520296458  0.0003890309  0.1393879715
# 49.03226

print('dove vs lion -- after')
median(ctd_vs_lion_after)  ; mean(ctd_vs_lion_after)  ; sd(ctd_vs_lion_after)
quantile(ctd_vs_lion_after, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_vs_lion_after < 0)) / length(ctd_vs_lion_after) ) * 100
# "dove vs lion -- after"
# -6.935843e-05
# 9.928891e-18
# 0.07741673
#          2.5%           50%         97.5%
# -1.742479e-01 -6.935843e-05  1.832100e-01
# 50.22513
print('dove vs human -- after')
median(ctd_vs_human_after) ; mean(ctd_vs_human_after) ; sd(ctd_vs_human_after)
quantile(ctd_vs_human_after, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_vs_human_after < 0)) / length(ctd_vs_human_after) ) * 100
# "dove vs human -- after"
# 8.104571e-05
# -3.693633e-17
# 0.0341957
#          2.5%           50%         97.5%
# -7.809751e-02  8.104571e-05  7.482195e-02
# 49.17874
print('lion vs human -- after')
median(lion_vs_human_after); mean(lion_vs_human_after); sd(lion_vs_human_after)
quantile(lion_vs_human_after, prob = c(0.025, 0.5, 0.975))
( length(which(lion_vs_human_after < 0)) / length(lion_vs_human_after) ) * 100
# "lion vs human -- after"
# 0.0001570439
# -1.311377e-18
# 0.07324986
#          2.5%           50%         97.5%
# -0.1730261311  0.0001570439  0.1601111696
# 49.55446

print('dove -- before vs during')
median(ctd_before_vs_during)  ; mean(ctd_before_vs_during)  ; sd(ctd_before_vs_during)
quantile(ctd_before_vs_during, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_before_vs_during < 0)) / length(ctd_before_vs_during) ) * 100
# "dove -- before vs during"
# -0.004547793
# 1.184894e-17
# 0.03967489
#          2.5%          50%        97.5%
# -0.079244927 -0.004547793  0.086157490
# 57.31174
print('lion -- before vs during')
median(lion_before_vs_during)  ; mean(lion_before_vs_during)  ; sd(lion_before_vs_during)
quantile(lion_before_vs_during, prob = c(0.025, 0.5, 0.975))
( length(which(lion_before_vs_during < 0)) / length(lion_before_vs_during) ) * 100
# "lion -- before vs during"
# 0.0005097983
# -1.033583e-17
# 0.06826246
#          2.5%           50%         97.5%
# -0.1389840639  0.0005097983  0.1406767648
# 49.10707
print('human -- before vs during')
median(human_before_vs_during)  ; mean(human_before_vs_during)  ; sd(human_before_vs_during)
quantile(human_before_vs_during, prob = c(0.025, 0.5, 0.975))
( length(which(human_before_vs_during < 0)) / length(human_before_vs_during) ) * 100
# "human -- before vs during"
# -0.0004058518
# -3.679702e-17
# 0.01990462
#          2.5%           50%         97.5%
# -0.0429135164 -0.0004058518  0.0435304356
# 51.9699

print('dove -- before vs after')
median(ctd_before_vs_after)  ; mean(ctd_before_vs_after)  ; sd(ctd_before_vs_after)
quantile(ctd_before_vs_after, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_before_vs_after < 0)) / length(ctd_before_vs_after) ) * 100
# "dove -- before vs after"
# -0.0002240497
# 3.578486e-17
# 0.008915405
#          2.5%           50%         97.5%
# -0.0198031219 -0.0002240497  0.0213846625
# 54.43829
print('lion -- before vs after')
median(lion_before_vs_after)  ; mean(lion_before_vs_after)  ; sd(lion_before_vs_after)
quantile(lion_before_vs_after, prob = c(0.025, 0.5, 0.975))
( length(which(lion_before_vs_after < 0)) / length(lion_before_vs_after) ) * 100
# "lion -- before vs after"
# 0.0003686601
# 1.064202e-17
# 0.0712211
#          2.5%           50%         97.5%
# -0.1434077651  0.0003686601  0.1452347579
# 49.40424
print('human -- before vs after')
median(human_before_vs_after)  ; mean(human_before_vs_after)  ; sd(human_before_vs_after)
quantile(human_before_vs_after, prob = c(0.025, 0.5, 0.975))
( length(which(human_before_vs_after < 0)) / length(human_before_vs_after) ) * 100
# "human -- before vs after"
# -0.001024985
# 2.900313e-17
# 0.02732494
#          2.5%          50%        97.5%
# -0.056536928 -0.001024985  0.057456127
# 52.63844

print('dove -- during vs after')
median(ctd_during_vs_after)  ; mean(ctd_during_vs_after)  ; sd(ctd_during_vs_after)
quantile(ctd_during_vs_after, prob = c(0.025, 0.5, 0.975))
( length(which(ctd_during_vs_after < 0)) / length(ctd_during_vs_after) ) * 100
# "dove -- during vs after"
# 0.003762167
# 1.105849e-17
# 0.03226886
#          2.5%          50%        97.5%
# -0.072103241  0.003762167  0.065066278
# 42.10783
print('lion -- during vs after')
median(lion_during_vs_after)  ; mean(lion_during_vs_after)  ; sd(lion_during_vs_after)
quantile(lion_during_vs_after, prob = c(0.025, 0.5, 0.975))
( length(which(lion_during_vs_after < 0)) / length(lion_during_vs_after) ) * 100
# "lion -- during vs after"
# -4.013247e-05
# 3.021342e-17
# 0.0102537
#          2.5%           50%         97.5%
# -2.295673e-02 -4.013247e-05  2.423272e-02
# 51.03931
print('human -- during vs after')
median(human_during_vs_after)  ; mean(human_during_vs_after)  ; sd(human_during_vs_after)
quantile(human_during_vs_after, prob = c(0.025, 0.5, 0.975))
( length(which(human_during_vs_after < 0)) / length(human_during_vs_after) ) * 100
# "human -- during vs after"
# -0.0001636495
# 2.566709e-17
# 0.009883111
#          2.5%           50%         97.5%
# -0.0219530454 -0.0001636495  0.0236350634
# 53.60621

## plot contrasts
contrasts_long %>%
  filter(context %in% c('before','during','after')) %>% 
  mutate(context = factor(context, levels = c('before','during','after'))) %>% 
  ggplot()+
  geom_density(aes(x = difference, colour = contrast))+
  facet_grid(f_age_cat ~ context)+
  scale_colour_viridis_d()+
  labs(colour = 'effect of changing stimulus')
ggsave(plot = last_plot(),
       filename = 'mom_noprev_stimulus_contrasts_splittime_new1.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       device = 'png', height = 2400, width = 1800, unit = 'px')

contrasts_long %>%
  filter(! context %in% c('before','during','after')) %>% 
  mutate(context = ifelse(context == 'ctd', 'dove (control)', context)) %>% 
  mutate(context = factor(context, levels = c('ctd','lion','human'))) %>% 
  ggplot()+
  geom_density(aes(x = difference, colour = contrast))+
  facet_grid(f_age_cat ~ context)+
  scale_colour_viridis_d()+
  labs(colour = 'effect of changing stimulus')
ggsave(plot = last_plot(),
       filename = 'mom_noprev_stimulus_contrasts_splittime_new2.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       device = 'png', height = 2400, width = 1800, unit = 'px')

save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_stimuluscontrasts.RData')

## focal age                                                         ####
# load('movement_direction/ordinal_noprev/moving_noprev_2bda_stimuluscontrasts.RData')
rm(list = ls()[!ls() %in% c('mom_fit','move','behav','num_iter','num_chains')]) ; gc()

## create new dataframe to predict from
age_new <- move %>%
  dplyr::select(f_age_num, age_combo, stim_type, bda,
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, bda,
                                                         focal, stim_num, pb_num))))

## predict with original ages
age_move_org <- age_new
age_mtx_org <- posterior_epred(object = mom_fit, newdata = age_move_org)
colnames(age_mtx_org) <- age_move_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with altered ages
age_move_alt <- age_new %>%
  mutate(f_age_num_original = f_age_num,
         age_combo_original = age_combo) %>%
  mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
  separate(age_combo, into = c('f_age_old','p_age'), sep = '_') %>%
  mutate(age_combo = paste0(f_age_num, '_', p_age)) %>%
  dplyr::select(f_age_num_original, f_age_num,
                age_combo_original, age_combo,
                stim_type, bda,
                focal, stim_num, pb_num,
                unique_data_combo)
age_mtx_alt <- posterior_epred(object = mom_fit, newdata = age_move_alt)
colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_agecontrasts.RData')

## summarise and convert to long format
# rm(list = ls()) ; gc() ; load('movement_direction/ordinal_noprev/moving_noprev_2bda_agecontrasts.RData')
age_pred <- age_move_org %>%
  #dplyr::select(-f_age_num) %>%
  mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
         age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
         age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
         age_org_prop4_mu = apply(age_mtx_org[,,4], 2, mean),
         age_org_prop5_mu = apply(age_mtx_org[,,5], 2, mean),
         age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
         age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
         age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
         age_org_prop4_sd = apply(age_mtx_org[,,4], 2, sd),
         age_org_prop5_sd = apply(age_mtx_org[,,5], 2, sd),
         age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
         age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
         age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
         age_alt_prop4_mu = apply(age_mtx_alt[,,4], 2, mean),
         age_alt_prop5_mu = apply(age_mtx_alt[,,5], 2, mean),
         age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
         age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
         age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd),
         age_alt_prop4_sd = apply(age_mtx_alt[,,4], 2, sd),
         age_alt_prop5_sd = apply(age_mtx_alt[,,5], 2, sd)) %>%
  pivot_longer(cols = c(age_org_prop1_mu, age_org_prop2_mu, age_org_prop3_mu,
                        age_org_prop4_mu, age_org_prop5_mu,
                        age_alt_prop1_mu, age_alt_prop2_mu, age_alt_prop3_mu,
                        age_alt_prop4_mu, age_alt_prop5_mu),
               names_to = 'focal_agemove_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(age_org_prop1_sd, age_org_prop2_sd, age_org_prop3_sd,
                        age_org_prop4_sd, age_org_prop5_sd,
                        age_alt_prop1_sd, age_alt_prop2_sd, age_alt_prop3_sd,
                        age_alt_prop4_sd, age_alt_prop5_sd),
               names_to = 'focal_agemove_sd', values_to = 'stdv_propn') %>%
  separate(col = focal_agemove_mu, into = c('focal_agemove_mu','mu'),
           sep = '_m', remove = T) %>%
  separate(col = focal_agemove_sd, into = c('focal_agemove_sd','sd'),
           sep = '_s', remove = T) %>%
  select(-mu, -sd) %>%
  filter(focal_agemove_mu == focal_agemove_sd) %>%
  separate(col = focal_agemove_mu, into = c('original_altered', 'move_pred'),
           sep = '_prop', remove = T) %>%
  select(-focal_agemove_sd) %>%
  mutate(move_pred = as.numeric(move_pred),
         f_age_num = ifelse(original_altered == 'age_org',
                            f_age_num,
                            ifelse(original_altered == 'age_alt' & f_age_num == 4,
                                   1, f_age_num + 1))) %>%
  mutate(pred_type = ifelse(move_pred == 1, 'move away directly',
                            ifelse(move_pred == 2, 'move away at an angle',
                                   ifelse(move_pred == 3, 'neither approach or retreat',
                                          ifelse(move_pred == 4, 'approach at an angle',
                                                 'approach directly')))))

## calculate contrasts
alt_vs_org_awaydirect <- age_mtx_alt[,,1] - age_mtx_org[,,1]
alt_vs_org_awayangle  <- age_mtx_alt[,,2] - age_mtx_org[,,2]
alt_vs_org_neither    <- age_mtx_alt[,,3] - age_mtx_org[,,3]
alt_vs_org_twdsangle  <- age_mtx_alt[,,4] - age_mtx_org[,,4]
alt_vs_org_twdsdirect <- age_mtx_alt[,,5] - age_mtx_org[,,5]

## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
print('directly away -- including all age categories')
mean(alt_vs_org_awaydirect); sd(alt_vs_org_awaydirect)
quantile(alt_vs_org_awaydirect, prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_awaydirect < 0)) / length(alt_vs_org_awaydirect) ) * 100
# "directly away -- including all age categories"
# -0.003978281
# 0.09351768
#         2.5%          50%        97.5%
# -0.194803692 -0.003379251  0.195175480
# 52.79048
print('angle away -- including all age categories')
mean(alt_vs_org_awayangle) ; sd(alt_vs_org_awayangle)
quantile(alt_vs_org_awayangle, prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_awayangle < 0)) / length(alt_vs_org_awayangle) ) * 100
# "angle away -- including all age categories"
# -3.620351e-05
# 0.03369564
#          2.5%           50%         97.5%
# -0.0701914755 -0.0002234775  0.0686775105
# 50.79608
print('neither -- including all age categories')
mean(alt_vs_org_neither)   ; sd(alt_vs_org_neither)
quantile(alt_vs_org_neither, prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_neither < 0)) / length(alt_vs_org_neither) ) * 100
# "neither -- including all age categories"
# -8.050307e-05
# 0.01001074
#          2.5%           50%         97.5%
# -2.200386e-02  9.083435e-05  2.075297e-02
# 48.65953
print('angle towards -- including all age categories')
mean(alt_vs_org_twdsangle) ; sd(alt_vs_org_twdsangle)
quantile(alt_vs_org_twdsangle, prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_twdsangle < 0)) / length(alt_vs_org_twdsangle) ) * 100
# "angle towards -- including all age categories"
# 0.0002126816
# 0.03253159
#          2.5%           50%         97.5%
# -0.0705081474  0.0004951245  0.0649321377
# 48.12385
print('directly towards -- including all age categories')
mean(alt_vs_org_twdsdirect); sd(alt_vs_org_twdsdirect)
quantile(alt_vs_org_twdsdirect, prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_twdsdirect < 0)) / length(alt_vs_org_twdsdirect) ) * 100
# "directly towards -- including all age categories"
# 0.003882306
# 0.08481055
#         2.5%          50%        97.5%
# -0.164456532  0.002385269  0.187526530
# 47.20952

## repeat excluding age category 4 because different contrast
print('directly away -- excluding category 4')
mean(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])
sd(  alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])
quantile(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)],
         prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_awaydirect < 0)) / length(alt_vs_org_awaydirect) ) * 100
# "directly away -- excluding category 4"
# -0.003019757
# 0.09173746
#         2.5%          50%        97.5%
# -0.184779026 -0.003969614  0.195291790
# 52.79048
print('angle away -- excluding category 4')
mean(alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])
sd(  alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])
quantile(alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)],
         prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_awayangle < 0)) / length(alt_vs_org_awayangle) ) * 100
# "angle away -- excluding category 4"
# 0.00110345
# 0.03261059
#          2.5%           50%         97.5%
# -6.489109e-02 -5.925196e-06  6.882935e-02
# 50.79608
print('neither -- excluding category 4')
mean(alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])
sd(  alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])
quantile(alt_vs_org_neither[,which(age_move_org$f_age_num != 4)],
         prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_neither < 0)) / length(alt_vs_org_neither) ) * 100
# "neither -- excluding category 4"
# 4.451481e-05
# 0.009681902
#          2.5%           50%         97.5%
# -0.0213402755  0.0001484417  0.0201865304
# 48.65953
print('angle towards -- excluding category 4')
mean(alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])
sd(  alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])
quantile(alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)],
         prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_twdsangle < 0)) / length(alt_vs_org_twdsangle) ) * 100
# "angle towards -- excluding category 4"
# -0.00018539
# 0.03122453
#          2.5%           50%         97.5%
# -0.0697115151  0.0003960018  0.0613152388
# 48.12385
print('directly towards -- excluding category 4')
mean(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])
sd(  alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])
quantile(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)],
         prob = c(0.025, 0.5, 0.975))
( length(which(alt_vs_org_twdsdirect < 0)) / length(alt_vs_org_twdsdirect) ) * 100
# "directly towards -- excluding category 4"
# 0.002057182
# 0.07990895
#         2.5%          50%        97.5%
# -0.161506947  0.002544171  0.171141103
# 47.20952

## split contrasts by original age category
age1v2_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 1)]
age2v3_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 2)]
age3v4_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 3)]
age1v4_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 4)] * (-1)
age1v2_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 1)]
age2v3_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 2)]
age3v4_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 3)]
age1v4_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 4)] * (-1)
age1v2_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 1)]
age2v3_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 2)]
age3v4_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 3)]
age1v4_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 4)] * (-1)
age1v2_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 1)]
age2v3_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 2)]
age3v4_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 3)]
age1v4_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 4)] * (-1)
age1v2_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 1)]
age2v3_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 2)]
age3v4_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 3)]
age1v4_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 4)] * (-1)

## calculate contrast values -- individual age categories
print('away direct, 1v2')
mean(age1v2_ad) ; sd(age1v2_ad)
quantile(age1v2_ad, prob = c(0.025,0.5,0.975))
( length(which(age1v2_ad < 0)) / length(age1v2_ad) ) * 100
# "away direct, 1v2"
# 0.0200536
# 0.09701563
#         2.5%          50%        97.5%
# -0.143293178  0.006937297  0.260480857
# 45.9929
print('away direct, 2v3')
mean(age2v3_ad) ; sd(age2v3_ad)
quantile(age2v3_ad, prob = c(0.025,0.5,0.975))
( length(which(age2v3_ad < 0)) / length(age2v3_ad) ) * 100
# "away direct, 2v3"
# 0.05561375
# 0.07128366
#        2.5%         50%       97.5%
# -0.06682953  0.04449709  0.22313594
# 19.83761
print('away direct, 3v4')
mean(age3v4_ad) ; sd(age3v4_ad)
quantile(age3v4_ad, prob = c(0.025,0.5,0.975))
( length(which(age3v4_ad < 0)) / length(age3v4_ad) ) * 100
# "away direct, 3v4"
# -0.06147584
# 0.06779488
#        2.5%         50%       97.5%
# -0.21513453 -0.05208690  0.05036839
# 85.00407
print('away direct, 1v4')
mean(age1v4_ad) ; sd(age1v4_ad)
quantile(age1v4_ad, prob = c(0.025,0.5,0.975))
( length(which(age1v4_ad < 0)) / length(age1v4_ad) ) * 100
# "away direct, 1v4"
# 0.00934261
# 0.1027488
#          2.5%           50%         97.5%
# -0.1944818325  0.0007697809  0.2430724460
# 48.79864

print('away angle, 1v2')
mean(age1v2_aa) ; sd(age1v2_aa)
quantile(age1v2_aa, prob = c(0.025,0.5,0.975))
( length(which(age1v2_aa < 0)) / length(age1v2_aa) ) * 100
# "away angle, 1v2"
# -0.003870711
# 0.0329611
#         2.5%          50%        97.5%
# -0.071297783 -0.002745793  0.068797082
# 58.40875
print('away angle, 2v3')
mean(age2v3_aa) ; sd(age2v3_aa)
quantile(age2v3_aa, prob = c(0.025,0.5,0.975))
( length(which(age2v3_aa < 0)) / length(age2v3_aa) ) * 100
# "away angle, 2v3"
# 0.007955283
# 0.02918412
#         2.5%          50%        97.5%
# -0.051621483  0.005566431  0.066901050
# 39.48237
print('away angle, 3v4')
mean(age3v4_aa) ; sd(age3v4_aa)
quantile(age3v4_aa, prob = c(0.025,0.5,0.975))
( length(which(age3v4_aa < 0)) / length(age3v4_aa) ) * 100
# "away angle, 3v4"
# -0.004184546
# 0.03430936
#         2.5%          50%        97.5%
# -0.070730364 -0.003771484  0.070299332
# 58.06495
print('away angle, 1v4')
mean(age1v4_aa) ; sd(age1v4_aa)
quantile(age1v4_aa, prob = c(0.025,0.5,0.975))
( length(which(age1v4_aa < 0)) / length(age1v4_aa) ) * 100
# "away angle, 1v4"
# 0.006414209
# 0.03860165
#         2.5%          50%        97.5%
# -0.067264446  0.002565735  0.098684989
# 45.02181

print('neither, 1v2')
mean(age1v2_n)  ; sd(age1v2_n) 
quantile(age1v2_n, prob = c(0.025,0.5,0.975))
( length(which(age1v2_n < 0)) / length(age1v2_n) ) * 100
# "neither, 1v2"
# -0.002458401
# 0.009804352
#         2.5%          50%        97.5%
# -0.025261598 -0.001049808  0.018271778
# 59.08191
print('neither, 2v3')
mean(age2v3_n)  ; sd(age2v3_n) 
quantile(age2v3_n, prob = c(0.025,0.5,0.975))
( length(which(age2v3_n < 0)) / length(age2v3_n) ) * 100
# "neither, 2v3"
# -0.001239891
# 0.009336652
#          2.5%           50%         97.5%
# -0.0231915327 -0.0001726456  0.0166637457
# 51.89077
print('neither, 3v4')
mean(age3v4_n)  ; sd(age3v4_n) 
quantile(age3v4_n, prob = c(0.025,0.5,0.975))
( length(which(age3v4_n < 0)) / length(age3v4_n) ) * 100
# "neither, 3v4"
# 0.001726875
# 0.009678894
#          2.5%           50%         97.5%
# -0.0182439925  0.0007395731  0.0233444652
# 41.83452
print('neither, 1v4')
mean(age1v4_n)  ; sd(age1v4_n) 
quantile(age1v4_n, prob = c(0.025,0.5,0.975))
( length(which(age1v4_n < 0)) / length(age1v4_n) ) * 100
# "neither, 1v4"
# 0.0007801586
# 0.01165672
#          2.5%           50%         97.5%
# -0.0244121104  0.0004258372  0.0259872434
# 46.68514

print('angle towards, 1v2')
mean(age1v2_ta) ; sd(age1v2_ta)
quantile(age1v2_ta, prob = c(0.025,0.5,0.975))
( length(which(age1v2_ta < 0)) / length(age1v2_ta) ) * 100
# "angle towards, 1v2"
# -0.008240718
# 0.03313606
#         2.5%          50%        97.5%
# -0.091915356 -0.002849396  0.048631915
# 55.39689
print('angle towards, 2v3')
mean(age2v3_ta) ; sd(age2v3_ta)
quantile(age2v3_ta, prob = c(0.025,0.5,0.975))
( length(which(age2v3_ta < 0)) / length(age2v3_ta) ) * 100
# "angle towards, 2v3"
# -0.01371451
# 0.02816527
#         2.5%          50%        97.5%
# -0.077597158 -0.009135987  0.039974309
# 68.31267
print('angle towards, 3v4')
mean(age3v4_ta) ; sd(age3v4_ta)
quantile(age3v4_ta, prob = c(0.025,0.5,0.975))
( length(which(age3v4_ta < 0)) / length(age3v4_ta) ) * 100
# "angle towards, 3v4"
# 0.01385228
# 0.02718223
#        2.5%         50%       97.5%
# -0.04218863  0.01147189  0.07027453
# 28.4193
print('angle towards, 1v4')
mean(age1v4_ta) ; sd(age1v4_ta)
quantile(age1v4_ta, prob = c(0.025,0.5,0.975))
( length(which(age1v4_ta < 0)) / length(age1v4_ta) ) * 100
# "angle towards, 1v4"
# -0.002440467
# 0.03897209
#         2.5%          50%        97.5%
# -0.076726201 -0.001857617  0.073879886
# 52.63691

print('direct towards, 1v2')
mean(age1v2_td) ; sd(age1v2_td)
quantile(age1v2_td, prob = c(0.025,0.5,0.975))
( length(which(age1v2_td < 0)) / length(age1v2_td) ) * 100
# "direct towards, 1v2"
# -0.005483772
# 0.08720911
#         2.5%          50%        97.5%
# -0.199889105 -0.005076439  0.169408882
# 54.0071
print('direct towards, 2v3')
mean(age2v3_td) ; sd(age2v3_td)
quantile(age2v3_td, prob = c(0.025,0.5,0.975))
( length(which(age2v3_td < 0)) / length(age2v3_td) ) * 100
# "direct towards, 2v3"
# -0.04861463
# 0.06302807
#        2.5%         50%       97.5%
# -0.18336426 -0.04415142  0.06866654
# 80.16239
print('direct towards, 3v4')
mean(age3v4_td) ; sd(age3v4_td)
quantile(age3v4_td, prob = c(0.025,0.5,0.975))
( length(which(age3v4_td < 0)) / length(age3v4_td) ) * 100
# "direct towards, 3v4"
# 0.05008123
# 0.06066506
#        2.5%         50%       97.5%
# -0.04161732  0.03543248  0.19793326
# 14.99593
print('direct towards, 1v4')
mean(age1v4_td) ; sd(age1v4_td)
quantile(age1v4_td, prob = c(0.025,0.5,0.975))
( length(which(age1v4_td < 0)) / length(age1v4_td) ) * 100
# "direct towards, 1v4"
# -0.01409651
# 0.1076493
#        2.5%         50%       97.5%
# -0.27881036 -0.00122051  0.18012463
# 51.20136

## summarise contrasts
contrasts <- move %>%
  mutate(alt_vs_org_awaydirect_mu = apply(alt_vs_org_awaydirect, 2, mean),
         alt_vs_org_awaydirect_sd = apply(alt_vs_org_awaydirect, 2, sd),
         alt_vs_org_awayangle_mu  = apply(alt_vs_org_awayangle,  2, mean),
         alt_vs_org_awayangle_sd  = apply(alt_vs_org_awayangle,  2, sd),
         alt_vs_org_neither_mu    = apply(alt_vs_org_neither,    2, mean),
         alt_vs_org_neither_sd    = apply(alt_vs_org_neither,    2, sd),
         alt_vs_org_twdsangle_mu  = apply(alt_vs_org_twdsangle,  2, mean),
         alt_vs_org_twdsangle_sd  = apply(alt_vs_org_twdsangle,  2, sd),
         alt_vs_org_twdsdirect_mu = apply(alt_vs_org_twdsdirect, 2, mean),
         alt_vs_org_twdsdirect_sd = apply(alt_vs_org_twdsdirect, 2, sd)
  ) %>%
  mutate(categories_different = ifelse(f_age_num == 4,
                                       '3 categories different',
                                       '1 category different'))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(alt_vs_org_awaydirect_mu, alt_vs_org_awayangle_mu,
                        alt_vs_org_neither_mu, alt_vs_org_twdsangle_mu,
                        alt_vs_org_twdsdirect_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','vs','org','move_pred','mu'),
           sep = '_', remove = T) %>%
  select(-alt_vs_org_awaydirect_sd, -alt_vs_org_awayangle_sd, -alt_vs_org_neither_sd,
         -alt_vs_org_twdsangle_sd, -alt_vs_org_twdsdirect_sd, -alt, -vs, -org, -mu)

## plot contrasts
str(age_pred)
# $ f_age_num        : num [1:319070] 4 4 4 4 4 1 1 1 1 1 ...
# $ age_combo        : chr [1:319070] "4_4" "4_4" "4_4" "4_4" ...
# $ stim_type        : chr [1:319070] "ctd" "ctd" "ctd" "ctd" ...
# $ bda              : chr [1:319070] "before" "before" "before" "before" ...
# $ focal            : chr [1:319070] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# $ stim_num         : chr [1:319070] "14" "14" "14" "14" ...
# $ pb_num           : num [1:319070] 1 1 1 1 1 1 1 1 1 1 ...
# $ unique_data_combo: int [1:319070] 725 725 725 725 725 725 725 725 725 725 ...
# $ original_altered : chr [1:319070] "age_org" "age_org" "age_org" "age_org" ...
# $ move_pred        : num [1:319070] 1 2 3 4 5 1 2 3 4 5 ...
# $ mean_propn       : Named num [1:319070] 0.7727 0.1297 0.0235 0.0446 0.0296 ...
# ..- attr(*, "names")= chr [1:319070] "725" "725" "725" "725" ...
# $ stdv_propn       : Named num [1:319070] 0.04271 0.02105 0.00479 0.00987 0.00717 ...
# ..- attr(*, "names")= chr [1:319070] "725" "725" "725" "725" ...
# $ pred_type        : chr [1:319070] "move away directly" "move away at an angle" "neither approach or retreat" "approach at an angle" ...
age_pred %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('move away directly',
                                       'move away at an angle',
                                       'neither approach or retreat',
                                       'approach at an angle',
                                       'approach directly')),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l','lion','human'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human'))) %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = stim_type, fill = stim_type),
               alpha = 0.5)+
  facet_grid(pred_type ~ .)
save.image('movement_direction/ordinal_noprev/moving_noprev_2bda_agecontrasts.RData')

## clean up a bit
rm(list = ls()[! ls() %in% c('alt_vs_org_awaydirect','alt_vs_org_awayangle',
                             'alt_vs_org_neither','alt_vs_org_twdsangle',
                             'alt_vs_org_twdsdirect','move','mom_fit','behav','num_iter','num_chains')]) ; gc()

## plot full density instead of means
colnames(alt_vs_org_awaydirect) <- move$data_row
colnames(alt_vs_org_awayangle)  <- move$data_row
colnames(alt_vs_org_neither)    <- move$data_row
colnames(alt_vs_org_twdsangle)  <- move$data_row
colnames(alt_vs_org_twdsdirect) <- move$data_row

mtx_to_df <- function(mtx, pred_type){
  df <- mtx %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(),
                 names_to = 'data_row',
                 values_to = 'contrast') %>%
    mutate(data_row = as.integer(data_row)) %>%
    left_join(move, by = 'data_row') %>%
    mutate(categories = factor(ifelse(f_age_num == 1,
                                      "10-15 to 16-20",
                                      ifelse(f_age_num == 2,
                                             "16-20 to 21-25",
                                             ifelse(f_age_num == 3,
                                                    "21-25 to 26-35",
                                                    "10-15 to 26-35"))),
                               levels = c("10-15 to 16-20", "16-20 to 21-25",
                                          "21-25 to 26-35","10-15 to 26-35"))) %>%
    mutate(contrast = ifelse(f_age_num == 4,
                             contrast * (-1), # age_contrast shows 4 -> 1 not 1-> 4
                             contrast),
           diff_cats = ifelse(f_age_num == 4,
                              'youngest to oldest', 'increase by one'),
           prediction_type = pred_type)
  return(df)
}
ad <- mtx_to_df(alt_vs_org_awaydirect, pred_type = 'move directly away')
aa <- mtx_to_df(alt_vs_org_awayangle, pred_type = 'move away at an angle')
n <- mtx_to_df(alt_vs_org_neither, pred_type = 'neither approach or retreat')
ta <- mtx_to_df(alt_vs_org_twdsangle, pred_type = 'approach at an angle')
td <- mtx_to_df(alt_vs_org_twdsdirect, pred_type = 'approach directly')

plot_contrasts <- rbind(aa, ad, n, ta, td) %>%
  mutate(prediction_type = factor(prediction_type,
                                  levels = c('move directly away',
                                             'move away at an angle',
                                             'neither approach or retreat',
                                             'approach at an angle',
                                             'approach directly')))

ggplot(plot_contrasts)+
  geom_density(aes(x = contrast,
                   fill = diff_cats, # fill = f_age_cat,
                   colour = diff_cats # colour = f_age_cat
  ),
  #fill = '#21918c', colour = '#21918c',
  alpha = 0.4)+
  scale_colour_viridis_d(begin = 0, end = 0.5)+
  scale_fill_viridis_d(begin = 0, end = 0.5)+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_grid(prediction_type ~ categories, scales = 'free_y')+#, nrow = 3, ncol = 4)+
  labs(x = 'contrast between age categories',
       fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
       colour = 'change in age\ncategory'  # colour = 'original\nage category'
  )+
  theme(legend.position = 'none')+ #c(0.8, 0.9))+
  theme_bw()
ggsave(plot = last_plot(), device = 'png',
       filename = 'movement_noprev2bda_agecontrasts.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       width = 2400, height = 3200, unit = 'px')
ggsave(plot = last_plot(), device = 'svg',
       filename = 'movement_noprev2bda_agecontrasts.svg',
       path = '../outputs/movement_ordinal_model_2bda/',
       width = 2400, height = 3200, unit = 'px')
