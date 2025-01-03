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

############ movement proportion                                     ############
pdf('../outputs/movement_binomial_model/movement_noprev_dataprep.pdf')
set.seed(12345)
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')
move <- behav %>%
  filter(activity == 'move')

num_chains <- 4
num_iter <- 2000

#### filter data                                                     ####
move_no_na <- move %>%
  # remove out of sight observations
  filter(action_index != 9) %>%
  # convert to binary move or no move
  mutate(move_index = ifelse(action_index == 0, 0, 1),
         moving_direction = ifelse(action_name == 'not_moving',
                                   'not_moving', 'moving')) %>%
  # clean up
  mutate(f_age_num = as.integer(f_age_num)) %>%
  mutate(focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num) %>%
  select(focal, moving_direction, move_index,
         f_age_cat, f_age_num,
         stim_type, bda,
         focal_id, stim_id, playback_id) %>%
  filter(!is.na(f_age_num))
str(move_no_na)

pb_types <- move_no_na %>% 
  dplyr::select(stim_type, playback_id) %>% 
  distinct()
pb_test <- c(sample(pb_types$playback_id[pb_types$stim_type == 'ctd'],
                    size = 5, replace = F),
             sample(pb_types$playback_id[pb_types$stim_type == 'l'],
                    size = 5, replace = F),
             sample(pb_types$playback_id[pb_types$stim_type == 'h'],
                    size = 5, replace = F))
move_no_na <- move_no_na %>% 
  filter(playback_id %in% pb_test)

#### plot raw                                                        ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
move_no_na %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_bar(aes(x = moving_direction,
               fill = f_age_cat),
           position = 'dodge')+
  facet_grid(bda ~ stim_type,
             labeller = labeller(stim_type = stim_labels),
             scales = 'free_y')+
  labs(fill = 'age category (years)',
       y = 'seconds moving',
       x = '')+
  scale_fill_viridis_d()
print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('movement_direction/binomial_noprev/movement_noprev_run.RData')

#### set priors                                                      ####
# set priors
get_prior(formula = move_index ~ 1 + mo(f_age_num) + stim_type * bda +
            (1|focal_id) + (1|stim_id) + (1|playback_id),
          data = move_no_na,
          family = bernoulli("logit"))
#                prior class                 coef       group resp dpar nlpar lb ub       source
#               (flat)     b                                                             default
#               (flat)     b            bdabefore                                   (vectorized)
#               (flat)     b            bdaduring                                   (vectorized)
#               (flat)     b          mof_age_num                                   (vectorized)
#               (flat)     b           stim_typeh                                   (vectorized)
#               (flat)     b stim_typeh:bdabefore                                   (vectorized)
#               (flat)     b stim_typeh:bdaduring                                   (vectorized)
#               (flat)     b           stim_typel                                   (vectorized)
#               (flat)     b stim_typel:bdabefore                                   (vectorized)
#               (flat)     b stim_typel:bdaduring                                   (vectorized)
# student_t(3, 0, 2.5)    sd                                                   0         default
# student_t(3, 0, 2.5)    sd                         focal_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept    focal_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd                      playback_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept playback_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd                          stim_id                  0    (vectorized)
# student_t(3, 0, 2.5)    sd            Intercept     stim_id                  0    (vectorized)
#         dirichlet(1)  simo         mof_age_num1                                        default

# set priors
priors <- c(
  # focal age
  prior(normal(-1,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(-1,1),      class = b,    coef = stim_typel),
  prior(normal(-1,1),      class = b,    coef = stim_typeh),
  # time
  prior(normal(-1,1),      class = b,    coef = bdabefore),
  prior(normal(-1,1),      class = b,    coef = bdaduring),
  # interaction
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdaduring),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdaduring)
)

#### prior predictive check                                          ####
prop_prior <- brm(
  formula = move_index ~ 1 + mo(f_age_num) + stim_type * bda +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(prop_prior)

print(paste0('priors set and checked at ', Sys.time()))

#### fit model                                                       ####
prop_fit <- brm(
  formula = move_index ~ 1 + mo(f_age_num) + stim_type * bda +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  control = list(adapt_delta = 0.9,
                 max_treedepth = 10))

# save workspace
save.image('movement_direction/binomial_noprev/movement_noprev_run.RData')

# inspect model
summary(prop_fit)

print(paste0('model fitted at ', Sys.time()))
dev.off()

#### check outputs                                                   ####
pdf('../outputs/movement_binomial_model/movement_noprev_modelchecks.pdf')
load('movement_direction/binomial_noprev/movement_noprev_run.RData') # load('ele_playbacks/movement_direction/binomial_noprev/movement_noprev_run.RData')

## check Stan code
prop_fit$model
prop_fit$formula

## extract posterior distribution
draws <- as_draws_df(prop_fit) %>%
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

#### plot marginal effects                                           ####
## extract marginal effects
marg <- conditional_effects(prop_fit,
                            method = 'posterior_epred')
names(marg)
# "stim_type" "bda" "f_age_num" "stim_type:bda"
stim_effect <- marg[['stim_type']]
time_effect <- marg[['bda']]
agef_effect <- marg[['f_age_num']]
intn_effect <- marg[['stim_type:bda']]

## plot marginal effects
(focal_age_plot <- ggplot(agef_effect)+
    geom_errorbar(aes(x = f_age_num,
                      ymax = upper__, ymin = lower__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = f_age_num,
                   #colour = cats__,
                   y = estimate__),
               size = 3)+ # cex = 3?
    xlab(label = 'focal age')+
    ylab('probability of movement direction')+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)))
ggsave(plot = focal_age_plot, filename = '../outputs/movement_binomial_model/movement_noprev_marginaleffects_focalage.png', device = 'png',
       width = 8.3, height = 5.8)

focal_age_labels <- c('focal age category 1',
                      'focal age category 2',
                      'focal age category 3',
                      'focal age category 4')
names(focal_age_labels) <- 1:4

(stim_plot <- ggplot(stim_effect)+
    geom_errorbar(aes(x = stim_type,
                      ymin = lower__, ymax = upper__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type,
                   #colour = cats__,
                   y = estimate__),
               cex = 3)+ # size = 3?
    xlab(label = 'stimulus type') + ylab('probability of movement')+
    scale_x_discrete(breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h'))+
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = stim_plot, filename = '../outputs/movement_binomial_model/movement_noprev_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check                                      ####
pp_check(prop_fit, ndraws = 100)

#### plot traces and density curves                                  ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_stim_typectd","b_stim_typeh","b_stim_typel",
                          "b_bdabefore","b_bdaduring",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "sigma"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

## stim type
lion <- draws_cut %>%
  filter(parameter == 'b_stim_typel') %>%
  rename(lion = draw) %>%
  select(-parameter)
human <- draws_cut %>%
  filter(parameter == 'b_stim_typeh') %>%
  rename(human = draw) %>%
  select(-parameter)

par(mfrow = c(2,1))
plot(density(lion$lion), main = 'lion vs dove') ; abline(v = 0, lty = 2)
plot(density(human$human), main = 'dove vs human') ; abline(v = 0, lty = 2)
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

## difference section
section <- draws_cut %>% filter(parameter %in% c('b_bdabefore','b_bdaduring'))
par(mfrow = c(2,1))
plot(density(section$draw[section$parameter == 'b_bdabefore']),
     main = 'before vs after slope', xlim = c(-1,0)) ; abline(v = 0, lty = 2)
plot(density(section$draw[section$parameter == 'b_bdaduring']),
     main = 'during vs after slope', xlim = c(-1,0)) ; abline(v = 0, lty = 2)

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
save.image('movement_direction/binomial_noprev/movement_noprev_run.RData')
dev.off()

#### predict from model                                              ####
load('movement_direction/binomial_noprev/movement_noprev_run.RData')
rm(list = ls()[! ls() %in% c('prop_fit','move_no_na','behav','num_iter','num_chains')]) ; gc()

str(move_no_na)

pred <- posterior_epred(object = prop_fit,
                        newdata = move_no_na) # including newdata = move_no_na was causing error of "Error in if (sum(invalid)) { : argument is not interpretable as logical". Without specifying, it's dropping ~4000 data rows -- RAW DATA CONTAINED A SET OF NA VALUES IN F_AGE_NUM FROM UNKNOWN AGE ELEPHANTS
save.image('movement_direction/binomial_noprev/movement_noprev_predictions.RData')

## convert to data frame
move_no_na$data_row <- 1:nrow(move_no_na)
pred <- as.data.frame(pred)
colnames(pred) <- 1:nrow(move_no_na)
pred <- pred %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row', values_to = 'epred') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(move_no_na, by = 'data_row')

save.image('movement_direction/binomial_noprev/movement_noprev_predictions.RData')

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions                                                ####
pdf('../outputs/movement_binomial_model/movement_noprev_modelpredictions.pdf')
load('movement_direction/binomial_noprev/movement_noprev_predictions.RData')

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

pred[1:(nrow(move_no_na)*1000),] %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
  ggplot()+
  geom_boxplot(aes(x = as.factor(f_age_num), y = epred,
                   fill = bda)) +
  facet_grid(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(fill = 'time relative to stimulus:',
       x = 'age category of focal elephant',
       y = 'predicted probability of moving')+
  theme(legend.position = 'bottom')

pred[1:(nrow(move_no_na)*1000),] %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          #colour = bda,
                          #alpha = bda,
                          #linetype = bda,
                          fill = f_age_cat)
  )+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density',
       linetype = 'time relative\nto stimulus',
       colour = 'time relative\nto stimulus',
       alpha = 'time relative\nto stimulus')+
  facet_grid(stim_type ~ bda,
             scales = 'free_x',
             labeller = labeller(stim_type = stim_labels))+
  scale_fill_viridis_d()+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_alpha_manual(values = c(0,0.6,0.6),
                     breaks = c('before','during','after'))+
  scale_linetype_manual(values = c(1,1,2),
                        breaks = c('before','during','after'))+
  theme(legend.position = 'bottom')
ggsave(filename = 'prop_binomial_noprev_predicted_ridges.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 1800, width = 1500, units = 'px')

## plot against raw data
pred[1:(nrow(move_no_na)*2),] %>%
  # group_by(data_row) %>%
  # mutate(epred = mean(epred)) %>%
  # ungroup() %>%
  group_by(focal, stim_type, bda) %>%
  summarise(epred = mean(epred),
            total_move = length(which(move_index == 1)),
            total_still = length(which(move_index == 0)),
            total_view = length(move_index),
            prop_move = total_move / total_view) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  left_join(distinct(move_no_na[,c('focal','f_age_cat')]), by = 'focal') %>%
  ggplot()+
  geom_point(aes(x = prop_move,
                 y = epred,
                 colour = f_age_cat,
                 size = total_view),
             alpha = 0.5)+
  facet_grid(stim_type ~ bda,
             labeller = labeller(stim_type = stim_labels))+
  scale_colour_viridis_d()+
  geom_abline(slope = 1, intercept = 0)+
  labs(colour = 'focal age category',
       size = 'number of seconds visible',
       x = 'proportion of time moving\n(NOT THE X VARIABLE FOR THE MODEL,\nBUT THE BEST OPTION FOR PLOTTING\nAGAINST PROBABILITY!)',
       y = 'predicted probability of moving')

pred2 <- pred[1:(nrow(move_no_na)*1000),] %>%
  #dplyr::select(-moving_direction,-time_since_stim,-after_stim) %>%
  pivot_longer(cols = c(epred, move_index),
               names_to = 'raw_pred', values_to = 'move') %>%
  mutate(raw_pred = ifelse(raw_pred == 'epred', 'predicted', 'observed')) %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h')))
pred2 %>%
  ggplot()+
  geom_density_ridges(aes(x = move,
                          y = f_age_cat,
                          fill = f_age_cat,
                          colour = raw_pred,
                          alpha = raw_pred)
  )+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density',
       colour = 'time relative\nto stimulus',
       alpha = 'time relative\nto stimulus')+
  facet_grid(bda ~ stim_type,
             scales = 'free_x',
             labeller = labeller(stim_type = stim_labels))+
  scale_fill_viridis_d()+
  scale_colour_manual(values = c('black','transparent'),
                      breaks = c('predicted','observed'))+
  scale_alpha_manual(values = c(0,0.6),
                     breaks = c('predicted','observed'))+
  theme(legend.position = 'bottom')
ggsave(filename = 'predicted_vs_raw_ridges_noprev.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 1800, width = 1500, units = 'px')

counts <- move_no_na %>%
  group_by(f_age_cat, stim_type, bda, move_index) %>%
  summarise(n = length(focal))
counts$total <- NA
for(i in 1:nrow(counts)){
  counts$total[i] <- sum(counts$n[which(counts$f_age_cat == counts$f_age_cat[i] &
                                          counts$stim_type == counts$stim_type[i] &
                                          counts$bda == counts$bda[i])])
}
counts$prop <- counts$n / counts$total

props <- pred[1:(nrow(move_no_na)*1000),] %>%
  group_by(f_age_cat, stim_type, bda, move_index) %>%
  summarise(pred_mean = mean(epred))

counts <- counts %>%
  left_join(props, by = c('f_age_cat', 'stim_type', 'bda', 'move_index'))

counts %>%
  filter(move_index == 1) %>%
  ggplot() +
  geom_point(aes(x = prop, y = pred_mean, colour = bda, shape = f_age_cat))+
  scale_colour_viridis_d()+
  facet_grid(. ~ stim_type)+
  geom_abline(slope = 1, intercept = 0)

ggplot()+
  geom_col(data = counts,
           aes(x = f_age_cat,
               y = prop,
               fill = as.factor(move_index)))+
  geom_violin(data = pred[1:(nrow(move_no_na)*1000),],
              aes(x = f_age_cat,
                  y = epred),
              fill = 'transparent')+
  scale_fill_viridis_d(begin = 1, end = 0.5)+
  facet_grid(bda ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  labs(x = 'focal age category',
       y = 'proportion of time spent moving',
       fill = 'moving')

ggplot() +
  geom_boxplot(data = counts[counts$bda == 'after',],
               aes(x = prop, y = 5,
                   fill = f_age_cat),
               width = 5, notch = F) +
  geom_density(data = pred[1:nrow(move_no_na),],
               aes(x = epred), inherit.aes = FALSE) +
  facet_grid(stim_type ~ f_age_cat,
             labeller = labeller(stim_type = stim_labels)) +
  scale_fill_viridis_d()

ggplot() +
  geom_col(data = counts,
           aes(x = move_index,
               y = prop,
               fill = f_age_cat),
           width = 0.2, alpha = 0.6) +
  geom_density(data = pred[1:nrow(move_no_na),],
               aes(x = epred), inherit.aes = FALSE) +
  facet_grid(stim_type ~ f_age_cat,
             labeller = labeller(stim_type = stim_labels)) +
  scale_fill_viridis_d()+
  scale_x_continuous(name = 'probability of moving',
                     limits = c(-0.1,1.1),
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  scale_y_continuous()

## reset plotting
dev.off()

#### calculate posterior contrasts from predictions                  ####
pdf('../outputs/movement_binomial_model/movement_noprev_modelcontrasts.pdf')
# load('movement_direction/binomial_noprev/movement_noprev_predictions.RData')

## stim type * before/after                                          ####
move_new <- move_no_na %>%
  dplyr::select(f_age_num, stim_type, bda,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, bda,focal_id, stim_id, playback_id))))

## redo predictions with different stimulus types: all doves
ctd_before <- move_new %>%
  mutate(stim_type = 'ctd',
         bda = 'before')
ctd_before_mtx <- posterior_epred(object = prop_fit, newdata = ctd_before)
colnames(ctd_before_mtx) <- ctd_before$unique_data_combo
rows_to_include <- c(1:100,1001:1100,2001:2100,3001:3100)
ctd_before_mtx <- ctd_before_mtx[rows_to_include,]

ctd_during <- move_new %>%
  mutate(stim_type = 'ctd',
         bda = 'during')
ctd_during_mtx <- posterior_epred(object = prop_fit, newdata = ctd_during)
colnames(ctd_during_mtx) <- ctd_during$unique_data_combo
ctd_during_mtx <- ctd_during_mtx[rows_to_include,]

ctd_after <- move_new %>%
  mutate(stim_type = 'ctd',
         after_after = 'after')
ctd_after_mtx <- posterior_epred(object = prop_fit, newdata = ctd_after)
colnames(ctd_after_mtx) <- ctd_after$unique_data_combo
ctd_after_mtx <- ctd_after_mtx[rows_to_include,]

## redo predictions with different stimulus types: all lions
lion_before <- move_new %>%
  mutate(stim_type = 'l',
         bda = 'before')
lion_before_mtx <- posterior_epred(object = prop_fit, newdata = lion_before)
colnames(lion_before_mtx) <- lion_before$unique_data_combo
lion_before_mtx <- lion_before_mtx[rows_to_include,]

lion_during <- move_new %>%
  mutate(stim_type = 'l',
         bda = 'during')
lion_during_mtx <- posterior_epred(object = prop_fit, newdata = lion_during)
colnames(lion_during_mtx) <- lion_during$unique_data_combo
lion_during_mtx <- lion_during_mtx[rows_to_include,]

lion_after <- move_new %>%
  mutate(stim_type = 'l',
         after_after = 'after')
lion_after_mtx <- posterior_epred(object = prop_fit, newdata = lion_after)
colnames(lion_after_mtx) <- lion_after$unique_data_combo
lion_after_mtx <- lion_after_mtx[rows_to_include,]

## redo predictions with different stimulus types: all humans
human_before <- move_new %>%
  mutate(stim_type = 'h',
         bda = 'before')
human_before_mtx <- posterior_epred(object = prop_fit, newdata = human_before)
colnames(human_before_mtx) <- human_before$unique_data_combo
human_before_mtx <- human_before_mtx[rows_to_include,]

human_during <- move_new %>%
  mutate(stim_type = 'h',
         bda = 'during')
human_during_mtx <- posterior_epred(object = prop_fit, newdata = human_during)
colnames(human_during_mtx) <- human_during$unique_data_combo
human_during_mtx <- human_during_mtx[rows_to_include,]

human_after <- move_new %>%
  mutate(stim_type = 'h',
         after_after = 'after')
human_after_mtx <- posterior_epred(object = prop_fit, newdata = human_after)
colnames(human_after_mtx) <- human_after$unique_data_combo
human_after_mtx <- human_after_mtx[rows_to_include,]

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
lion_before_vs_during <- lion_during_mtx - lion_before_mtx
human_before_vs_during <- human_during_mtx - human_before_mtx

ctd_before_vs_after <- ctd_after_mtx - ctd_before_mtx
lion_before_vs_after <- lion_after_mtx - lion_before_mtx
human_before_vs_after <- human_after_mtx - human_before_mtx

ctd_during_vs_after <- ctd_after_mtx - ctd_during_mtx
lion_during_vs_after <- lion_after_mtx - lion_during_mtx
human_during_vs_after <- human_after_mtx - human_during_mtx

## summarise contrasts
contrasts <- move_no_na %>%
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
         lion_before_vs_during_mu = apply(lion_before_vs_during, 2, mean),
         lion_before_vs_during_sd = apply(lion_before_vs_during, 2, sd),
         human_before_vs_during_mu = apply(human_before_vs_during, 2, mean),
         human_before_vs_during_sd = apply(human_before_vs_during, 2, sd),

         ctd_before_vs_after_mu = apply(ctd_before_vs_after, 2, mean),
         ctd_before_vs_after_sd = apply(ctd_before_vs_after, 2, sd),
         lion_before_vs_after_mu = apply(lion_before_vs_after, 2, mean),
         lion_before_vs_after_sd = apply(lion_before_vs_after, 2, sd),
         human_before_vs_after_mu = apply(human_before_vs_after, 2, mean),
         human_before_vs_after_sd = apply(human_before_vs_after, 2, sd),

         ctd_during_vs_after_mu = apply(ctd_during_vs_after, 2, mean),
         ctd_during_vs_after_sd = apply(ctd_during_vs_after, 2, sd),
         lion_during_vs_after_mu = apply(lion_during_vs_after, 2, mean),
         lion_during_vs_after_sd = apply(lion_during_vs_after, 2, sd),
         human_during_vs_after_mu = apply(human_during_vs_after, 2, mean),
         human_during_vs_after_sd = apply(human_during_vs_after, 2, sd),
  )
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(ctd_vs_lion_before_mu, ctd_vs_human_before_mu, lion_vs_human_before_mu,
                        ctd_vs_lion_during_mu, ctd_vs_human_during_mu, lion_vs_human_during_mu,
                        ctd_vs_lion_after_mu, ctd_vs_human_after_mu, lion_vs_human_after_mu,
                        ctd_before_vs_during_mu, lion_before_vs_during_mu, human_before_vs_during_mu,
                        ctd_before_vs_after_mu, lion_before_vs_after_mu, human_before_vs_after_mu,
                        ctd_during_vs_after_mu, lion_during_vs_after_mu, human_during_vs_after_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('contrast','mu'),
           sep = -3, remove = T) %>%
  select(-mu, -ctd_vs_lion_before_sd, -ctd_vs_human_before_sd, -lion_vs_human_before_sd,
         ctd_vs_lion_during_sd, -ctd_vs_human_during_sd, -lion_vs_human_during_sd,
         ctd_vs_lion_after_sd, -ctd_vs_human_after_sd, -lion_vs_human_after_sd,
         ctd_before_vs_during_sd, -lion_before_vs_during_sd, -human_before_vs_during_sd,
         ctd_before_vs_after_sd, -lion_before_vs_after_sd, -human_before_vs_after_sd,
         ctd_during_vs_after_sd, -lion_during_vs_after_sd, -human_during_vs_after_sd)

## save contrasts
save.image('movement_direction/binomial_noprev/movement_noprev_stimuluscontrasts.RData')
load('movement_direction/binomial_noprev/movement_noprev_stimuluscontrasts.RData')

## produce values for reporting
print('ctd_vs_lion_before')
mean(ctd_vs_lion_before)   ; sd(ctd_vs_lion_before)
quantile(ctd_vs_lion_before, prob = c(0.025, 0.5, 0.975))
# "ctd_vs_lion_before"
# 0.09997586
# 0.112192
#         2.5%          50%        97.5%
# -0.002885296  0.062505956  0.402750958
print('ctd_vs_human_before')
mean(ctd_vs_human_before)  ; sd(ctd_vs_human_before)
quantile(ctd_vs_human_before, prob = c(0.025, 0.5, 0.975))
# "ctd_vs_human_before"
# 0.1070293
# 0.1112871
#          2.5%           50%         97.5%
# -0.0001412521  0.0754436061  0.3985617737
print('lion_vs_human_before')
mean(lion_vs_human_before) ; sd(lion_vs_human_before)
quantile(lion_vs_human_before, prob = c(0.025, 0.5, 0.975))
# "lion_vs_human_before"
# 0.007053401
# 0.0998597
#        2.5%          50%        97.5%
# -0.231873753  0.001865493  0.218730992

print('ctd_vs_lion_during')
mean(ctd_vs_lion_during)   ; sd(ctd_vs_lion_during)
quantile(ctd_vs_lion_during, prob = c(0.025, 0.5, 0.975))
# "ctd_vs_lion_during"
# 0.1610598
# 0.1430077
#         2.5%          50%        97.5%
# -0.006989969  0.133194579  0.482573457
print('ctd_vs_human_during')
mean(ctd_vs_human_during)  ; sd(ctd_vs_human_during)
quantile(ctd_vs_human_during, prob = c(0.025, 0.5, 0.975))
# "ctd_vs_human_during"
# 0.1744427
# 0.1418212
#          2.5%           50%         97.5%
# -0.0003757976  0.1558683227  0.4762806373
print('lion_vs_human_during')
mean(lion_vs_human_during) ; sd(lion_vs_human_during)
quantile(lion_vs_human_during, prob = c(0.025, 0.5, 0.975))
# "lion_vs_human_during"
# 0.01338294
# 0.1294452
#         2.5%          50%        97.5%
# -0.277740066  0.004123949  0.273176854

print('ctd_vs_lion_after')
mean(ctd_vs_lion_after)    ; sd(ctd_vs_lion_after)
quantile(ctd_vs_lion_after, prob = c(0.025, 0.5, 0.975))
# "ctd_vs_lion_after"
# 0.1183215
# 0.1234973
#         2.5%          50%        97.5%
# -0.003577652  0.080971172  0.433507130
print('ctd_vs_human_after')
mean(ctd_vs_human_after)   ; sd(ctd_vs_human_after)
quantile(ctd_vs_human_after, prob = c(0.025, 0.5, 0.975))
# "ctd_vs_human_after"
# 0.1270625
# 0.122473
#          2.5%           50%         97.5%
# -0.0002041351  0.0957623656  0.4285620372
print('lion_vs_human_after')
mean(lion_vs_human_after)  ; sd(lion_vs_human_after)
quantile(lion_vs_human_after, prob = c(0.025, 0.5, 0.975))
# "lion_vs_human_after"
# 0.008740988
# 0.1100775
#         2.5%          50%        97.5%
# -0.252259975  0.002421806  0.238769251

print('ctd_before_vs_during')
mean(ctd_before_vs_during)  ; sd(ctd_before_vs_during)
quantile(ctd_before_vs_during, prob = c(0.025, 0.5, 0.975))
# "ctd_before_vs_during"
# 0.1043842
# 0.08828833
#         2.5%          50%        97.5%
# 0.0002734141 0.0818090154 0.2974717269
print('lion_before_vs_during')
mean(lion_before_vs_during) ; sd(lion_before_vs_during)
quantile(lion_before_vs_during, prob = c(0.025, 0.5, 0.975))
# "lion_before_vs_during"
# 0.1654681
# 0.1028053
#         2.5%          50%        97.5%
# 0.0007336299 0.1777316319 0.3044997908
print('human_before_vs_during')
mean(human_before_vs_during); sd(human_before_vs_during)
quantile(human_before_vs_during, prob = c(0.025, 0.5, 0.975))
# "human_before_vs_during"
# 0.1717977
# 0.1024429
#         2.5%          50%        97.5%
# 0.0008107847 0.1901273551 0.3049890262

print('ctd_before_vs_after')
mean(ctd_before_vs_after)  ; sd(ctd_before_vs_after)
quantile(ctd_before_vs_after, prob = c(0.025, 0.5, 0.975))
# "ctd_before_vs_after"
# 0.02914146
# 0.05295342
#      2.5%       50%     97.5%
# 0.0000000 0.0000000 0.1749114
print('lion_before_vs_after')
mean(lion_before_vs_after) ; sd(lion_before_vs_after)
quantile(lion_before_vs_after, prob = c(0.025, 0.5, 0.975))
# "lion_before_vs_after"
# 0.0474871
# 0.07088116
#      2.5%       50%     97.5%
# 0.0000000 0.0000000 0.2370297
print('human_before_vs_after')
mean(human_before_vs_after); sd(human_before_vs_after)
quantile(human_before_vs_after, prob = c(0.025, 0.5, 0.975))
# "human_before_vs_after"
# 0.04917469
# 0.07205638
#      2.5%       50%     97.5%
# 0.0000000 0.0000000 0.2443526

print('ctd_during_vs_after')
mean(ctd_during_vs_after)  ; sd(ctd_during_vs_after)
quantile(ctd_during_vs_after, prob = c(0.025, 0.5, 0.975))
# "ctd_during_vs_after"
# -0.07524277
# 0.07547698
#        2.5%         50%       97.5%
# -0.27613516 -0.05505516  0.00000000
print('lion_during_vs_after')
mean(lion_during_vs_after) ; sd(lion_during_vs_after)
quantile(lion_during_vs_after, prob = c(0.025, 0.5, 0.975))
# "lion_during_vs_after"
# -0.117981
# 0.09645446
#       2.5%        50%      97.5%
# -0.3010719 -0.1064389  0.0000000
print('human_during_vs_after')
mean(human_during_vs_after); sd(human_during_vs_after)
quantile(human_during_vs_after, prob = c(0.025, 0.5, 0.975))
# "human_during_vs_after"
# -0.122623
# 0.09785694
#       2.5%        50%      97.5%
# -0.3017001 -0.1124629  0.0000000

## plot contrasts
contrasts_long <- contrasts_long %>%
  mutate(contrast_stim = case_when(contrast == 'ctd_vs_lion_before' ~ 'dove -> lion',
                                   contrast == 'ctd_vs_human_before' ~ 'dove -> human',
                                   contrast == 'lion_vs_human_before' ~ 'lion -> human',

                                   contrast == 'ctd_vs_lion_during' ~ 'dove -> lion',
                                   contrast == 'ctd_vs_human_during' ~ 'dove -> human',
                                   contrast == 'lion_vs_human_during' ~ 'lion -> human',

                                   contrast == 'ctd_vs_lion_after' ~ 'dove -> lion',
                                   contrast == 'ctd_vs_human_after' ~ 'dove -> human',
                                   contrast == 'lion_vs_human_after' ~ 'lion -> human',

                                   contrast == 'ctd_before_vs_during' ~ 'dove -> dove',
                                   contrast == 'lion_before_vs_during' ~ 'lion -> lion',
                                   contrast == 'human_before_vs_during' ~ 'human -> human',

                                   contrast == 'ctd_before_vs_after' ~ 'dove -> dove',
                                   contrast == 'lion_before_vs_after' ~ 'lion -> lion',
                                   contrast == 'human_before_vs_after' ~ 'human -> human',

                                   contrast == 'ctd_during_vs_after' ~ 'dove -> dove',
                                   contrast == 'lion_during_vs_after' ~ 'lion -> lion',
                                   contrast == 'human_during_vs_after' ~ 'human -> human'),

         contrast_sect = case_when(contrast == 'ctd_vs_lion_before' ~ 'before -> before',
                                   contrast == 'ctd_vs_human_before' ~ 'before -> before',
                                   contrast == 'lion_vs_human_before' ~ 'before -> before',

                                   contrast == 'ctd_vs_lion_during' ~ 'during -> during',
                                   contrast == 'ctd_vs_human_during' ~ 'during -> during',
                                   contrast == 'lion_vs_human_during' ~ 'during -> during',

                                   contrast == 'ctd_vs_lion_after' ~ 'after -> after',
                                   contrast == 'ctd_vs_human_after' ~ 'after -> after',
                                   contrast == 'lion_vs_human_after' ~ 'after -> after',

                                   contrast == 'ctd_before_vs_during' ~ 'before -> during',
                                   contrast == 'lion_before_vs_during' ~ 'before -> during',
                                   contrast == 'human_before_vs_during' ~ 'before -> during',

                                   contrast == 'ctd_before_vs_after' ~ 'before -> after',
                                   contrast == 'lion_before_vs_after' ~ 'before -> after',
                                   contrast == 'human_before_vs_after' ~ 'before -> after',

                                   contrast == 'ctd_during_vs_after' ~ 'during -> after',
                                   contrast == 'lion_during_vs_after' ~ 'during -> after',
                                   contrast == 'human_during_vs_after' ~ 'during -> after')
  )

contrasts_long %>%
  filter(contrast_stim %in% c('dove -> lion','dove -> human','lion -> human')) %>%
  mutate(contrast_sect = ifelse(contrast_sect == 'before -> before', 'before',
                                ifelse(contrast_sect == 'during -> during', 'during', 'after'))) %>%
  mutate(contrast_sect = factor(contrast_sect, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = contrast_stim, fill = contrast_stim),
               alpha = 0.4)+
  facet_wrap(f_age_cat ~ contrast_sect, scales = 'free_y')+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  labs(colour = 'effect of changing stimulus',
       fill = 'effect of changing stimulus')+
  theme(legend.position = 'bottom')
contrasts_long %>%
  filter(contrast_stim %in% c('dove -> lion','dove -> human','lion -> human')) %>%
  mutate(contrast_sect = ifelse(contrast_sect == 'before -> before', 'before',
                                ifelse(contrast_sect == 'during -> during', 'during', 'after'))) %>%
  mutate(contrast_sect = factor(contrast_sect, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = f_age_cat, fill = f_age_cat),
               alpha = 0.4)+
  facet_wrap(contrast_stim ~ contrast_sect, scales = 'free_y')+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  labs(colour = 'effect of changing stimulus',
       fill = 'effect of changing stimulus')+
  theme(legend.position = 'bottom')

contrasts_long %>%
  filter(contrast_sect %in% c('before -> during','before -> after','during -> after')) %>%
  mutate(contrast_stim = ifelse(contrast_stim == 'dove -> dove', 'dove (control)',
                                ifelse(contrast_stim == 'lion -> lion', 'lion', 'human'))) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = contrast_stim, fill = contrast_stim),
               alpha = 0.4)+
  facet_wrap(f_age_cat ~ contrast_sect, scales = 'free_y')+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  labs(colour = 'effect of changing stimulus',
       fill = 'effect of changing stimulus')+
  theme(legend.position = 'bottom')
contrasts_long %>%
  filter(contrast_sect %in% c('before -> during','before -> after','during -> after')) %>%
  mutate(contrast_stim = ifelse(contrast_stim == 'dove -> dove', 'dove (control)',
                                ifelse(contrast_stim == 'lion -> lion', 'lion', 'human'))) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = f_age_cat, fill = f_age_cat),
               alpha = 0.4)+
  facet_wrap(contrast_stim ~ contrast_sect, scales = 'free_y')+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  labs(colour = 'effect of changing stimulus',
       fill = 'effect of changing stimulus')+
  theme(legend.position = 'bottom')

save.image('movement_direction/binomial_noprev/movement_noprev_stimuluscontrasts.RData')
rm(ctd_vs_lion_before,ctd_vs_human_before,lion_vs_human_before,
ctd_vs_lion_after,ctd_vs_human_after,lion_vs_human_after,
ctd_before_vs_after,lion_before_vs_after,human_before_vs_after,
ctd_before,ctd_before_mtx,ctd_after,ctd_after_mtx,
lion_before,lion_before_mtx,lion_after,lion_after_mtx,
human_before,human_before_mtx,human_after,human_after_mtx) ; gc()

## focal age                                                         ####
load('movement_direction/binomial_noprev/movement_noprev_stimuluscontrasts.RData')
move_new <- move_new %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(stim_type, bda,
                                                         focal_id, stim_id, playback_id))))

## predict with original ages
age_move_org <- move_new
age_mtx_org <- posterior_epred(object = prop_fit, newdata = age_move_org)
colnames(age_mtx_org) <- age_move_org$unique_data_combo

## redo predictions with altered ages
age_move_alt <- move_new %>%
  mutate(f_age_num_original = f_age_num) %>%
  mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
  relocate(f_age_num_original)
age_mtx_alt <- posterior_epred(object = prop_fit, newdata = age_move_alt)
colnames(age_mtx_alt) <- age_move_alt$unique_data_combo

save.image('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')
# load('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')

## calculate contrasts
age_contrast <- age_mtx_alt - age_mtx_org

## plot overall effect
plot(density(age_contrast))

## plot effect per category
age1_vs_age2 <- age_contrast[,which(age_move_org$f_age_num == 1)]
age2_vs_age3 <- age_contrast[,which(age_move_org$f_age_num == 2)]
age3_vs_age4 <- age_contrast[,which(age_move_org$f_age_num == 3)]
age1_vs_age4 <- age_contrast[,which(age_move_org$f_age_num == 4)]*-1

## plot category effects
plot(density(age1_vs_age2), col = 'blue',
     xlim = c(-0.2,0), ylim = c(0,30), las = 1,
     main = 'contrasts between age categories:\nblue = 1->2, red = 2->3,\ngreen = 3->4, purple = 1->4')
lines(density(age2_vs_age3), col = 'red')
lines(density(age3_vs_age4), col = 'green')
lines(density(age1_vs_age4), col = 'purple')

## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
print('age1 vs age2')
mean(age1_vs_age2) ; sd(age1_vs_age2) # -0.05072095 ;  0.06024068
quantile(age1_vs_age2, prob = c(0.025, 0.5, 0.975))
#          2.5%           50%         97.5%
# -0.2150803273 -0.0295625030 -0.0000226048
print('age2 vs age3')
mean(age2_vs_age3) ; sd(age2_vs_age3) # -0.03553748 ; 0.03827389
quantile(age2_vs_age3, prob = c(0.025, 0.5, 0.975))
#          2.5%           50%         97.5%
# -0.1371146152 -0.0224629018 -0.0000398404
print('age3 vs age4')
mean(age3_vs_age4) ; sd(age3_vs_age4) # -0.0437991 ; 0.04795357
quantile(age3_vs_age4, prob = c(0.025, 0.5, 0.975))
#          2.5%           50%         97.5%
# -0.1723784623 -0.0282496427 -0.0000334704
print('age1 vs age4')
mean(age1_vs_age4) ; sd(age1_vs_age4) # -0.1519467 ; 0.1330916
quantile(age1_vs_age4, prob = c(0.025, 0.5, 0.975))
#          2.5%           50%         97.5%
# -0.4551524263 -0.1241281505 -0.0001654837

## save output
save.image('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')   #load('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')

## plot predictions
pred <- pred %>%
  mutate(draw_id = rep(1:4000, each = nrow(age_move_org)),
         stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
                                 ifelse(stim_type == 'l','lion','human'))) %>%
  mutate(stim_type_long = factor(stim_type_long,
                                 levels = c('dove (control)','lion','human')))

pred %>%
  ggplot()+
  geom_boxplot(aes(x = f_age_cat,
                   fill = bda,
                   y = epred))+
  labs(x = 'focal age category',
       y = 'predicted probability of moving',
       fill = 'moving in previous second')+
  facet_wrap(. ~ stim_type_long)+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
print('plot1 done')

pred %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          fill = f_age_cat),
               alpha = 0.4)+
  labs(fill = 'focal age\ncategory',
       x = 'predicted probability of moving',
       y = 'probability density')+
  facet_grid(bda ~ stim_type_long,
             scales = 'free')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
print('plot2 done')

## plot contrasts
colnames(age_contrast) <- move_no_na$data_row
plot_contrasts <- age_contrast %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row',
               values_to = 'contrast') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(move_no_na, by = 'data_row') %>%
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
                            'youngest to oldest', 'increase by one'))
ggplot(plot_contrasts)+
  geom_density(aes(x = contrast,
                   fill = diff_cats, # fill = f_age_cat,
                   colour = diff_cats # colour = f_age_cat
  ),
  #fill = '#21918c', colour = '#21918c',
  alpha = 0.4)+
  scale_colour_viridis_d(begin = 0, end = 0.5)+
  scale_fill_viridis_d(begin = 0, end = 0.5)+
  facet_wrap(. ~ categories, scales = 'free_y')+
  labs(x = 'contrast between age categories',
       fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
       colour = 'change in age\ncategory'  # colour = 'original\nage category'
  )+
  theme(legend.position = 'none')+ #c(0.8, 0.9))+
  theme_bw()
ggsave(plot = last_plot(), device = 'png',
       filename = 'prop_density_agecontrasts_noprev.png',
       path = '../outputs/movement_binomial_model/',
       width = 2400, height = 1800, unit = 'px')
ggsave(plot = last_plot(), device = 'svg',
       filename = 'prop_density_agecontrasts_noprev.svg',
       path = '../outputs/movement_binomial_model/',
       width = 2400, height = 1800, unit = 'px')
print('plot3 done')

# ## replot contrasts and check that it matches -- 2 different scripts, wrote plots independently -- same data seems to produce different graphs
# data <- age_contrast %>%
#   as.data.frame() %>%
#   pivot_longer(cols = everything(),
#                names_to = 'unique_data_combo',
#                values_to = 'contrast') %>%
#   mutate(unique_data_combo = as.integer(unique_data_combo)) %>%
#   left_join(distinct(age_move_org), by = 'unique_data_combo') %>%
#   rename(f_age_num_org = f_age_num) %>%
#   mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1, f_age_num_org + 1)) %>%
#   mutate(f_age_cat_org = ifelse(f_age_num_org == 1, '10-15 yrs',
#                                 ifelse(f_age_num_org == 2, '16-20 yrs',
#                                        ifelse(f_age_num_org == 3, '21-25 yrs',
#                                               ifelse(f_age_num_org == 4, '26-35 yrs',
#                                                      '>36 yrs')))),
#          f_age_cat_alt = ifelse(f_age_num_alt == 1, '10-15 yrs',
#                                 ifelse(f_age_num_alt == 2, '16-20 yrs',
#                                        ifelse(f_age_num_alt == 3, '21-25 yrs',
#                                               ifelse(f_age_num_alt == 4, '26-35 yrs',
#                                                      '>36 yrs')))),
#          contrast = ifelse(f_age_num_org == 4,
#                            contrast*(-1),
#                            contrast)) %>%
#   relocate(f_age_num_alt, .after = (f_age_num_org)) %>%
#   relocate(f_age_cat_org, .after = (f_age_num_alt)) %>%
#   relocate(f_age_cat_alt, .after = (f_age_cat_org)) %>%
#   mutate(comparison = ifelse(f_age_num_org == 4,
#                              paste0(f_age_cat_alt, ' to ', f_age_cat_org),
#                              paste0(f_age_cat_org, ' to ', f_age_cat_alt))) %>%
#   mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                             ifelse(stim_type == 'l', 'lion',
#                                    'human'))) %>%
#   mutate(stim_type = factor(stim_type,
#                             levels = c('dove (control)', 'lion', 'human'))) %>%
#   mutate(comparison = factor(comparison,
#                              levels = c('10-15 yrs to 16-20 yrs',
#                                         '16-20 yrs to 21-25 yrs',
#                                         '21-25 yrs to 26-35 yrs',
#                                         '10-15 yrs to 26-35 yrs')))
# saveRDS(data, '../data_processed/plot_contrasts_noprev_moveprob_checkdata.RDS') # data <- readRDS('../data_processed/plot_contrasts_noprev_moveprob_checkdata.RDS')

plot_contrasts %>%
  mutate(stim_type = ifelse(stim_type == 'ctd','dove (control)',
                            ifelse(stim_type == 'l','lion','human'))) %>% 
  mutate(bda = factor(bda, levels = c('before','during','after')),
         stim_type = factor(stim_type, levels = c('dove (control)','lion','human'))) %>% 
  ggplot()+
  geom_violin(aes(x = categories,
                  y = contrast,
                  fill = categories),
              position = position_dodge(0.5))+
  geom_hline(yintercept = 0, lty = 2)+
  scale_fill_viridis_d()+
  facet_grid(stim_type ~ bda)+
  labs(fill = 'age comparison')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'prop_violin_agecontrasts_noprev.png',
       path = '../outputs/movement_binomial_model/',
       height = 1600, width = 1600, unit = 'px')
print('plot4 done')

### save and clean up
save.image('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')
dev.off()
