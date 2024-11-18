#### information ####
# script to compare overall proportions of different behaviours
# previous models gave very good predictions of if the individual would move or not once you know it was moving in the previous second, but not good indication of probability of initiating movement

#### set up ####
library(tidyverse)
library(brms)
library(LaplacesDemon)
library(patchwork)
library(ggridges)

theme_set(theme_bw())

############ movement proportion ############
pdf('../outputs/movement_proportion/movement_proportion_dataprep.pdf')
set.seed(12345)

#### import data ####
load('movement_direction/movement_binomial_run.RData')
rm(list = ls()[ ! ls() %in% c('move', 'focals')]) ; gc()

#### filter data ####
move_no_na <- move %>%
  # remove out of sight observations
  filter(move_index != 9) %>%
  # convert to binary move or no move
  mutate(move_index = ifelse(move_index == 0, 0, 1),
         moving_direction = ifelse(moving_direction == 'not_moving',
                                   'not_moving', 'moving')) %>%
  # add variable for before or after stimulus starts
  mutate(before_after = ifelse(time_since_stim < 0, 'before', 'after')) %>%
  # clean up
  mutate(f_age_num = as.integer(f_age_num)) %>%
  mutate(focal_id = as.integer(as.factor(focal)),
         stim_num = as.integer(as.factor(stim_num))) %>%
  rename(stim_id = stim_num,
         playback_id = pb_num) %>%
  select(focal, moving_direction, move_index,
         f_age_cat, f_age_num,
         time_since_stim, after_stim, stim_type, before_after,
         focal_id, stim_id, playback_id) %>%
  distinct()
str(move_no_na)

move60 <- move_no_na %>%
  filter(time_since_stim >= -60) %>%
  filter(time_since_stim <= 60) %>%
  distinct()
table(move60$focal)

move60_e34rm <- move60 %>%
  filter(playback_id != 34) # something is wrong with elephant movements in pb34 -- WILL NEED TO BE SORTED IN OTHER MODELS ASAP BUT FOR NOW JUST IGNORE IT

rm(list = ls()[!ls() %in% c('move60_e34rm')]) ; gc()

#### calculate proportions ####
prop <- move60_e34rm %>%
  dplyr::select(-moving_direction, -move_index, -time_since_stim, -after_stim) %>%
  distinct() %>%
  mutate(visible_total = NA,
         visible_section = NA,
         seconds_moving_total = NA,
         seconds_moving_section = NA,
         prop_move_total = NA,
         prop_move_section = NA)
for(i in 1:nrow(prop)){
  id <- move60_e34rm %>%
    filter(focal == prop$focal[i])
  prop$visible_total[i] <- nrow(id)
  prop$seconds_moving_total[i] <- length(which(id$move_index == 1))
  id <- id %>%
    filter(before_after == prop$before_after[i])
  prop$visible_section[i] <- nrow(id)
  prop$seconds_moving_section[i] <- length(which(id$move_index == 1))
}
prop$prop_move_total <- prop$seconds_moving_total / prop$visible_total
prop$prop_move_section <- prop$seconds_moving_section / prop$visible_section
rm(id) ; gc()

prop$prop_move_section_edit <- ifelse(prop$prop_move_section == 0, 0.0000000001, prop$prop_move_section)
prop$prop_move_section_edit <- ifelse(prop$prop_move_section_edit == 1, 0.9999999999,
                                      prop$prop_move_section_edit)

prop$logit_prop_section <- LaplacesDemon::logit(prop$prop_move_section_edit)

prop_visible <- prop %>%
  filter(visible_section > 9)

prop_visible %>% 
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>% 
  mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human'))) %>% 
  ggplot()+
  geom_boxplot(aes(x = stim_type,
                   y = prop_move_section, fill = before_after),
               notch = T)+
  scale_fill_viridis_d()+
  scale_x_discrete(name = 'stimulus type',
                   breaks = c('dove (control)', 'lion','human'))

#### plot raw ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot overall
prop %>%
  ggplot()+
  geom_vline(aes(xintercept = 10))+
  geom_point(aes(x = visible_section,
                 y = prop_move_section,
                 colour = before_after),
             alpha = 0.5)+
  facet_grid(f_age_num ~ stim_type,
             labeller = labeller(f_age_num = age_labels,
                                 stim_type = stim_labels))+
  scale_x_continuous(name = 'number of seconds visible')+
  scale_y_continuous(name = 'proportion of time spent moving')+
  scale_colour_viridis_d()
prop %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h'))) %>%
  ggplot()+
  geom_boxplot(aes(x = f_age_cat,
                   y = prop_move_section,
                   fill = before_after))+
  facet_grid(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  labs(x = 'age category (years)',
       y = 'proportion of time spent moving',
       fill = 'time relative\nto start of\nstimulus')+
  scale_fill_viridis_d()
prop %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h'))) %>%
  ggplot()+
  geom_density_ridges(aes(y = f_age_cat,
                          x = prop_move_section_edit,
                          fill = before_after),
                      alpha = 0.6)+
  facet_wrap(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  labs(x = 'proportion of time spent moving',
       y = 'stimulus type',
       fill = 'time relative\nto start of\nstimulus')+
  scale_fill_viridis_d()+
  geom_vline(xintercept = c(0,1), linetype = 2)
print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
save.image('movement_direction/movement_proportion_run.RData')
dev.off()
pdf('../outputs/movement_proportion/movement_proportion_modelchecks.RData')

#### set priors ####
# set priors
get_prior(formula = prop_move_section_edit ~ 0 + mo(f_age_num) + stim_type * before_after +
            (1|focal_id) + (1|stim_id) + (1|playback_id),
          data = prop,
          family = Beta()) #zero_inflated_beta())

# set priors
priors <- c(
  # focal age
  prior(normal(0,1),    class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(0,1),    class = b,    coef = stim_typectd),
  prior(normal(0,1),    class = b,    coef = stim_typel),
  prior(normal(0,1),    class = b,    coef = stim_typeh),
  # time
  prior(normal(0,1),    class = b,    coef = before_afterbefore),
  # interactions
  prior(normal(0,1),    class = b,    coef = stim_typeh:before_afterbefore),
  prior(normal(0,1),    class = b,    coef = stim_typel:before_afterbefore)
)

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
prop_prior <- brm(
  formula = prop_move_section_edit ~ 0 + mo(f_age_num) + stim_type * before_after +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = prop,
  family = Beta(),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(prop_prior)

print(paste0('priors set and checked at ', Sys.time()))

#### fit model ####
prop_fit <- brm(
  formula = prop_move_section_edit ~ 0 + mo(f_age_num) + stim_type * before_after +
    #s(visible_section) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = prop,
  family = Beta(),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  control = list(adapt_delta = 0.9))

# save workspace
save.image('movement_direction/movement_proportion_run.RData')

# inspect model
summary(prop_fit)

print(paste0('model fitted at ', Sys.time()))

#### check outputs ####
# load('movement_direction/movement_proportion_run.RData') # load('ele_playbacks/movement_direction/movement_proportion_run.RData')

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

#### plot marginal effects ####
## extract marginal effects
marg <- conditional_effects(prop_fit,
                            method = 'posterior_epred')
names(marg)
# "stim_type" "before_after" "stim_type:before_after" "f_age_num" #"f_age_num:stim_type" "f_age_num:before_after"
stim_effect <- marg[[1]]
time_effect <- marg[[2]]
stba_effect <- marg[[3]]
agef_effect <- marg[[4]]
# fast_effect <- marg[[5]]
# faba_effect <- marg[[6]]

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
ggsave(plot = focal_age_plot, filename = '../outputs/movement_proportion/movement_proportion_marginaleffects_focalage.png', device = 'png',
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
ggsave(plot = stim_plot, filename = '../outputs/movement_proportion/movement_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

# (focal_age_plot + stim_plot) +
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(),
#        filename = '../outputs/movement_proportion/movement_marginaleffects.png',
#        device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(prop_fit, ndraws = 100) # not perfect, but roughly the right sort of shape

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_stim_typectd","b_stim_typeh","b_stim_typel",
                          "b_before_afterbefore",
                          "b_stim_typeh:before_afterbefore","b_stim_typel:before_afterbefore",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "sigma"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

# interactions <- draws %>%
#   filter(parameter %in% c("b_stim_typeh:before_afterbefore","b_stim_typel:before_afterbefore",
#                           "bsp_mof_age_num:stim_typeh","bsp_mof_age_num:stim_typel",
#                           "bsp_mof_age_num:before_afterbefore",
#                           "bsp_mof_age_num:stim_typeh:before_afterbefore",
#                           "bsp_mof_age_num:stim_typel:before_afterbefore",
#                           "simo_mof_age_num:stim_typeh1[1]","simo_mof_age_num:stim_typeh1[2]","simo_mof_age_num:stim_typeh1[3]",
#                           "simo_mof_age_num:stim_typel1[1]","simo_mof_age_num:stim_typel1[2]","simo_mof_age_num:stim_typel1[3]",
#                           "simo_mof_age_num:before_afterbefore1[1]","simo_mof_age_num:before_afterbefore1[2]","simo_mof_age_num:before_afterbefore1[3]",
#                           "simo_mof_age_num:stim_typeh:before_afterbefore1[1]",
#                           "simo_mof_age_num:stim_typeh:before_afterbefore1[2]",
#                           "simo_mof_age_num:stim_typeh:before_afterbefore1[3]",
#                           "simo_mof_age_num:stim_typel:before_afterbefore1[1]",
#                           "simo_mof_age_num:stim_typel:before_afterbefore1[2]",
#                           "simo_mof_age_num:stim_typel:before_afterbefore1[3]"))
# ggplot(data = interactions,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none')

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
plot(density(dove_human$difference), main = 'dove vs human') ; abline(v = 0, lty = 2)
plot(density(lion_human$difference), main = 'lion vs human') ; abline(v = 0, lty = 2)
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

## difference before vs after
befaft <- draws_cut %>% filter(parameter == 'b_before_afterbefore')
par(mfrow = c(1,1)) ; plot(density(befaft$draw), main = 'befpre vs after slope') ; abline(v = 0, lty = 2)

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
save.image('movement_direction/movement_proportion_run.RData')
dev.off()

#### predict from model ####
pdf('../outputs/movement_proportion/movement_proportion_modelpredictions.pdf')
# load('movement_direction/movement_proportion_run.RData')
rm(list = ls()[! ls() %in% c('prop_fit','prop')]) ; gc()

pred <- posterior_epred(object = prop_fit,
                        newdata = prop)
save.image('movement_direction/movement_proportion_predictions.RData')

## convert to data frame
prop$data_row <- 1:nrow(prop)
pred <- as.data.frame(pred)
colnames(pred) <- 1:nrow(prop)
pred <- pred %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row', values_to = 'epred') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(prop, by = 'data_row')

save.image('movement_direction/movement_proportion_predictions.RData')

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions ####
# load('movement_direction/movement_proportion_predictions.RData')

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

pred %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
  mutate(epred = invlogit(epred)) %>%
  ggplot()+
  geom_boxplot(aes(x = as.factor(f_age_num), y = epred,
                   fill = before_after)) +
  facet_grid(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(fill = 'time relative to stimulus:',
       x = 'age category of focal elephant',
       y = 'predicted probability of moving')+
  theme(legend.position = 'bottom')

pred %>%
  mutate(before_after = factor(before_after, levels = c('before','after'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
  # mutate(epred = invlogit(epred)) %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          fill = f_age_cat,
                          colour = before_after,
                          alpha = before_after)
  )+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density',
       colour = 'time relative\nto stimulus',
       alpha = 'time relative\nto stimulus')+
  facet_grid(. ~ stim_type,
             scales = 'free_x',
             labeller = labeller(stim_type = stim_labels))+
  scale_fill_viridis_d()+
  scale_colour_manual(values = c('black','transparent'),
                      breaks = c('before','after'))+
  scale_alpha_manual(values = c(0,0.6),
                     breaks = c('before','after'))+
  theme(legend.position = 'bottom')
ggsave(filename = 'prop_predicted_ridges.png',
       path = '../outputs/movement_proportion/',
       device = 'png', height = 1800, width = 1500, units = 'px')

## plot against raw data
pred[1:13400,] %>%
  group_by(data_row) %>%
  mutate(epred = mean(epred)) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
  ggplot()+
  geom_point(aes(x = prop_move_section_edit,
                 y = epred,
                 colour = f_age_cat))+
  facet_grid(stim_type ~ before_after,
             labeller = labeller(stim_type = stim_labels))+
  scale_colour_viridis_d()+
  geom_abline(slope = 1, intercept = 0)+
  labs(colour = 'focal age category')

pred2 <- pred %>%
  pivot_longer(cols = c(epred, prop_move_section_edit),
               names_to = 'raw_pred', values_to = 'prop_move') %>% 
  dplyr::select(-seconds_moving_section, -seconds_moving_total, -prop_move_total, -logit_prop_section) %>% 
  mutate(raw_pred = ifelse(raw_pred == 'epred', 'predicted', 'observed')) %>% 
  mutate(before_after = factor(before_after, levels = c('before','after'))) %>%
  # mutate(epred = invlogit(epred)) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h')))
pred2 %>% 
  ggplot()+
  geom_density_ridges(aes(x = prop_move,
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
  facet_grid(before_after ~ stim_type,
             scales = 'free_x',
             labeller = labeller(stim_type = stim_labels))+
  scale_fill_viridis_d()+
  scale_colour_manual(values = c('black','transparent'),
                      breaks = c('predicted','observed'))+
  scale_alpha_manual(values = c(0,0.6),
                     breaks = c('predicted','observed'))+
  theme(legend.position = 'bottom')
ggsave(filename = 'predicted_vs_raw_ridges.png',
       path = '../outputs/movement_proportion/',
       device = 'png', height = 1800, width = 1500, units = 'px')

## reset plotting
dev.off()
pdf('../outputs/movement_proportion/movement_proportion_modelcontrasts.pdf')

#### calculate posterior contrasts from predictions ####
# load('movement_direction/movement_proportion_predictions.RData')

## stim type * before/after ####
move_new <- prop %>%
  dplyr::select(f_age_num, stim_type, before_after,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, before_after,focal_id, stim_id, playback_id))))

## redo predictions with different stimulus types: all doves
ctd_before <- move_new %>%
  mutate(stim_type = 'ctd',
         before_after = 'before')
ctd_before_mtx <- posterior_epred(object = prop_fit, newdata = ctd_before)
colnames(ctd_before_mtx) <- ctd_before$unique_data_combo
ctd_before_mtx <- ctd_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

ctd_after <- move_new %>%
  mutate(stim_type = 'ctd',
         after_after = 'after')
ctd_after_mtx <- posterior_epred(object = prop_fit, newdata = ctd_after)
colnames(ctd_after_mtx) <- ctd_after$unique_data_combo
ctd_after_mtx <- ctd_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all lions
lion_before <- move_new %>%
  mutate(stim_type = 'l',
         before_after = 'before')
lion_before_mtx <- posterior_epred(object = prop_fit, newdata = lion_before)
colnames(lion_before_mtx) <- lion_before$unique_data_combo
lion_before_mtx <- lion_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

lion_after <- move_new %>%
  mutate(stim_type = 'l',
         after_after = 'after')
lion_after_mtx <- posterior_epred(object = prop_fit, newdata = lion_after)
colnames(lion_after_mtx) <- lion_after$unique_data_combo
lion_after_mtx <- lion_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all humans
human_before <- move_new %>%
  mutate(stim_type = 'h',
         before_after = 'before')
human_before_mtx <- posterior_epred(object = prop_fit, newdata = human_before)
colnames(human_before_mtx) <- human_before$unique_data_combo
human_before_mtx <- human_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

human_after <- move_new %>%
  mutate(stim_type = 'h',
         after_after = 'after')
human_after_mtx <- posterior_epred(object = prop_fit, newdata = human_after)
colnames(human_after_mtx) <- human_after$unique_data_combo
human_after_mtx <- human_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## calculate contrasts
ctd_vs_lion_before <- lion_before_mtx - ctd_before_mtx
ctd_vs_human_before <- human_before_mtx - ctd_before_mtx
lion_vs_human_before <- human_before_mtx - lion_before_mtx

ctd_vs_lion_after <- lion_after_mtx - ctd_after_mtx
ctd_vs_human_after <- human_after_mtx - ctd_after_mtx
lion_vs_human_after <- human_after_mtx - lion_after_mtx

ctd_before_vs_after <- ctd_after_mtx - ctd_before_mtx
lion_before_vs_after <- lion_after_mtx - lion_before_mtx
human_before_vs_after <- human_after_mtx - human_before_mtx

## summarise contrasts
contrasts <- prop %>%
  select(-stim_type) %>%
  mutate(ctd_vs_lion_before_mu = apply(ctd_vs_lion_before, 2, mean),
         ctd_vs_lion_before_sd = apply(ctd_vs_lion_before, 2, sd),
         ctd_vs_human_before_mu = apply(ctd_vs_human_before, 2, mean),
         ctd_vs_human_before_sd = apply(ctd_vs_human_before, 2, sd),
         lion_vs_human_before_mu = apply(lion_vs_human_before, 2, mean),
         lion_vs_human_before_sd = apply(lion_vs_human_before, 2, sd),

         ctd_vs_lion_after_mu = apply(ctd_vs_lion_after, 2, mean),
         ctd_vs_lion_after_sd = apply(ctd_vs_lion_after, 2, sd),
         ctd_vs_human_after_mu = apply(ctd_vs_human_after, 2, mean),
         ctd_vs_human_after_sd = apply(ctd_vs_human_after, 2, sd),
         lion_vs_human_after_mu = apply(lion_vs_human_after, 2, mean),
         lion_vs_human_after_sd = apply(lion_vs_human_after, 2, sd),

         ctd_before_vs_after_mu = apply(ctd_before_vs_after, 2, mean),
         ctd_before_vs_after_sd = apply(ctd_before_vs_after, 2, sd),
         lion_before_vs_after_mu = apply(lion_before_vs_after, 2, mean),
         lion_before_vs_after_sd = apply(lion_before_vs_after, 2, sd),
         human_before_vs_after_mu = apply(human_before_vs_after, 2, mean),
         human_before_vs_after_sd = apply(human_before_vs_after, 2, sd),
         )
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(ctd_vs_lion_before_mu, ctd_vs_human_before_mu, lion_vs_human_before_mu,
                        ctd_vs_lion_after_mu, ctd_vs_human_after_mu, lion_vs_human_after_mu,
                        ctd_before_vs_after_mu, lion_before_vs_after_mu, human_before_vs_after_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('contrast','mu'),
           sep = -3, remove = T) %>%
  select(-mu, -ctd_vs_lion_before_sd, -ctd_vs_human_before_sd, -lion_vs_human_before_sd,
         -ctd_vs_lion_after_sd, -ctd_vs_human_after_sd, -lion_vs_human_after_sd,
         -ctd_before_vs_after_sd, -lion_before_vs_after_sd, -human_before_vs_after_sd)

## save contrasts
save.image('movement_direction/movement_proportion_stimuluscontrasts.RData')
# load('movement_direction/movement_proportion_stimuluscontrasts.RData')

## produce values for reporting
print('ctd_vs_lion_before')   ; median(ctd_vs_lion_before)   ; mean(ctd_vs_lion_before)   ; sd(ctd_vs_lion_before)
print('ctd_vs_human_before')  ; median(ctd_vs_human_before)  ; mean(ctd_vs_human_before)  ; sd(ctd_vs_human_before)
print('lion_vs_human_before') ; median(lion_vs_human_before) ; mean(lion_vs_human_before) ; sd(lion_vs_human_before)

print('ctd_vs_lion_after')    ; median(ctd_vs_lion_after)    ; mean(ctd_vs_lion_after)    ; sd(ctd_vs_lion_after)
print('ctd_vs_human_after')   ; median(ctd_vs_human_after)   ; mean(ctd_vs_human_after)   ; sd(ctd_vs_human_after)
print('lion_vs_human_after')  ; median(lion_vs_human_after)  ; mean(lion_vs_human_after)  ; sd(lion_vs_human_after)

print('ctd_before_vs_after')  ; median(ctd_before_vs_after)  ; mean(ctd_before_vs_after)  ; sd(ctd_before_vs_after)
print('lion_before_vs_after') ; median(lion_before_vs_after) ; mean(lion_before_vs_after) ; sd(lion_before_vs_after)
print('human_before_vs_after'); median(human_before_vs_after); mean(human_before_vs_after); sd(human_before_vs_after)

## plot contrasts
contrasts_long <- contrasts_long %>%
  mutate(contrast_stim = case_when(contrast == 'ctd_vs_lion_before' ~ 'dove -> lion',
                                   contrast == 'ctd_vs_human_before' ~ 'dove -> human',
                                   contrast == 'lion_vs_human_before' ~ 'lion -> human',

                                   contrast == 'ctd_vs_lion_after' ~ 'dove -> lion',
                                   contrast == 'ctd_vs_human_after' ~ 'dove -> human',
                                   contrast == 'lion_vs_human_after' ~ 'lion -> human',

                                   contrast == 'ctd_before_vs_after' ~ 'dove -> dove',
                                   contrast == 'lion_before_vs_after' ~ 'lion -> lion',
                                   contrast == 'human_before_vs_after' ~ 'human -> human'),

         contrast_sect = case_when(contrast == 'ctd_vs_lion_before' ~ 'before -> before',
                                   contrast == 'ctd_vs_human_before' ~ 'before -> before',
                                   contrast == 'lion_vs_human_before' ~ 'before -> before',

                                   contrast == 'ctd_vs_lion_after' ~ 'after -> after',
                                   contrast == 'ctd_vs_human_after' ~ 'after -> after',
                                   contrast == 'lion_vs_human_after' ~ 'after -> after',

                                   contrast == 'ctd_before_vs_after' ~ 'before -> after',
                                   contrast == 'lion_before_vs_after' ~ 'before -> after',
                                   contrast == 'human_before_vs_after' ~ 'before -> after'),
         )

contrasts_long %>%
  filter(contrast_stim %in% c('dove -> lion','dove -> human','lion -> human')) %>%
  mutate(contrast_sect = ifelse(contrast_sect == 'before -> before', 'before', 'after')) %>%
  mutate(contrast_sect = factor(contrast_sect, levels = c('before','after'))) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = contrast_stim, fill = contrast_stim),
               alpha = 0.4)+
  facet_wrap(. ~ contrast_sect, scales = 'free_y')+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  labs(colour = 'effect of changing stimulus',
       fill = 'effect of changing stimulus')+
  theme(legend.position = 'bottom')

contrasts_long %>%
  filter(contrast_sect == 'before -> after') %>%
  mutate(contrast_stim = ifelse(contrast_stim == 'dove -> dove', 'dove (control)',
                                ifelse(contrast_stim == 'lion -> lion', 'lion', 'human'))) %>%
  ggplot()+
  geom_density(aes(x = difference, colour = contrast_stim, fill = contrast_stim),
               alpha = 0.4)+
  facet_wrap(. ~ contrast_stim, scales = 'free_y')+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  labs(colour = 'effect of changing stimulus',
       fill = 'effect of changing stimulus')+
  theme(legend.position = 'bottom')

save.image('movement_direction/movement_proportion_stimuluscontrasts.RData')
rm(ctd_vs_lion_before,ctd_vs_human_before,lion_vs_human_before,
   ctd_vs_lion_after,ctd_vs_human_after,lion_vs_human_after,
   ctd_before_vs_after,lion_before_vs_after,human_before_vs_after,
   ctd_before,ctd_before_mtx,ctd_after,ctd_after_mtx,
   lion_before,lion_before_mtx,lion_after,lion_after_mtx,
   human_before,human_before_mtx,human_after,human_after_mtx) ; gc()

## focal age ####
# load('movement_direction/movement_proportion_stimuluscontrasts.RData')
move_new <- move_new %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(stim_type, before_after,
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

save.image('movement_direction/movement_proportion_agecontrasts.RData')

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
median(age1_vs_age2) ; mean(age1_vs_age2) ; sd(age1_vs_age2)
median(age2_vs_age3) ; mean(age2_vs_age3) ; sd(age2_vs_age3)
median(age3_vs_age4) ; mean(age3_vs_age4) ; sd(age3_vs_age4)
median(age1_vs_age4) ; mean(age1_vs_age4) ; sd(age1_vs_age4)

## save output
save.image('movement_direction/movement_proportion_agecontrasts.RData')

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
                   fill = before_after,
                   y = epred))+
  labs(x = 'focal age category',
       y = 'predicted probability of moving',
       fill = 'moving in previous second')+
  facet_wrap(. ~ stim_type_long)+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')

pred %>%
  mutate(before_after = factor(before_after, levels = c('before','after'))) %>%
  ggplot()+
  geom_density(aes(x = epred,
                   fill = f_age_cat),
               alpha = 0.4)+
  labs(fill = 'focal age\ncategory',
       x = 'predicted probability of moving',
       y = 'probability density')+
  facet_grid(before_after ~ stim_type_long,
             scales = 'free')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')

## plot contrasts
colnames(age_contrast) <- prop$data_row
plot_contrasts <- age_contrast %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row',
               values_to = 'contrast') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(prop, by = 'data_row') %>%
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
       filename = 'prop_density_agecontrasts.png',
       path = '../outputs/movement_proportion/',
       width = 2400, height = 1800, unit = 'px')
ggsave(plot = last_plot(), device = 'svg',
       filename = 'prop_density_agecontrasts.svg',
       path = '../outputs/movement_proportion/',
       width = 2400, height = 1800, unit = 'px')

## replot contrasts and check that it matches -- 2 different scripts, wrote plots independently -- same data seems to produce different graphs
age_contrast %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = 'unique_data_combo',
               values_to = 'contrast') %>%
  mutate(unique_data_combo = as.integer(unique_data_combo)) %>%
  left_join(distinct(age_move_org), by = 'unique_data_combo') %>%
  rename(f_age_num_org = f_age_num) %>%
  mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1, f_age_num_org + 1)) %>%
  mutate(f_age_cat_org = ifelse(f_age_num_org == 1, '10-15 yrs',
                                ifelse(f_age_num_org == 2, '16-20 yrs',
                                       ifelse(f_age_num_org == 3, '21-25 yrs',
                                              ifelse(f_age_num_org == 4, '26-35 yrs',
                                                     '>36 yrs')))),
         f_age_cat_alt = ifelse(f_age_num_alt == 1, '10-15 yrs',
                                ifelse(f_age_num_alt == 2, '16-20 yrs',
                                       ifelse(f_age_num_alt == 3, '21-25 yrs',
                                              ifelse(f_age_num_alt == 4, '26-35 yrs',
                                                     '>36 yrs')))),
         contrast = ifelse(f_age_num_org == 4, 
                           contrast*(-1), 
                           contrast)) %>%
  relocate(f_age_num_alt, .after = (f_age_num_org)) %>%
  relocate(f_age_cat_org, .after = (f_age_num_alt)) %>%
  relocate(f_age_cat_alt, .after = (f_age_cat_org)) %>%
  mutate(comparison = ifelse(f_age_num_org == 4,
                             paste0(f_age_cat_alt, ' to ', f_age_cat_org),
                             paste0(f_age_cat_org, ' to ', f_age_cat_alt))) %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion',
                                   'human'))) %>%
  mutate(stim_type = factor(stim_type,
                            levels = c('dove (control)', 'lion', 'human'))) %>%
  mutate(comparison = factor(comparison,
                             levels = c('10-15 yrs to 16-20 yrs',
                                        '16-20 yrs to 21-25 yrs',
                                        '21-25 yrs to 26-35 yrs',
                                        '10-15 yrs to 26-35 yrs'))) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = contrast,
                  fill = comparison),
              position = position_dodge(0.5))+
  geom_hline(yintercept = 0, lty = 2)+
  scale_fill_viridis_d()+
  facet_grid(stim_type ~ before_after)+
  labs(fill = 'age comparison')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'prop_violin_agecontrasts.png',
       path = '../outputs/movement_proportion/',
       height = 1600, width = 1600, unit = 'px')

### save and clean up
save.image('movement_direction/movement_proportion_agecontrasts.RData')
dev.off()

# ############ movement direction                                      ####
# rm(list = ls()) ; gc()
# set.seed(12345)
# pdf('../outputs/movement_proportion/movement_proportion_direction_modelprep.pdf')
# 
# #### create data                                                     ####
# load('movement_direction/movement_binomial_run.RData')
# rm(list = ls()[ ! ls() %in% c('move', 'focals')]) ; gc()
# 
# #### filter data                                                     ####
# move_no_na <- move %>%
#   # remove out of sight observations
#   filter(move_index != 9) %>%
#   # remove not moving
#   filter(move_index > 0) %>%
#   # add variable for before or after stimulus starts
#   mutate(before_after = ifelse(time_since_stim < 0, 'before', 'after')) %>%
#   # clean up
#   mutate(f_age_num = as.integer(f_age_num)) %>%
#   mutate(focal_id = as.integer(as.factor(focal)),
#          stim_num = as.integer(as.factor(stim_num))) %>%
#   rename(stim_id = stim_num,
#          playback_id = pb_num) %>%
#   select(focal, partner, moving_direction, move_index,
#          f_age_cat, f_age_num, p_age_cat, p_age_num,
#          time_since_stim, before_after, stim_type, before_after,
#          focal_id, stim_id, playback_id) %>%
#   distinct()
# str(move_no_na)
# 
# move60 <- move_no_na %>%
#   filter(time_since_stim >= -60) %>%
#   filter(time_since_stim <= 60) %>%
#   distinct()
# rm(list = ls()[!ls() %in% c('move60')]) ; gc()
# 
# move60 <- move60 %>% 
#   mutate(age_combo = paste0(f_age_num,'_',p_age_num)) %>%
#   filter(move_index != 3) %>% 
#   mutate(approach = ifelse(move_index < 3, 0, 1)) %>% 
#   mutate(prop = NA,
#          total_move = NA)
# for(i in 1:nrow(move60)){
#   move60$total_move[i] <- length(which(move60$focal == move60$focal[i] & move60$partner == move60$partner[i]))
#   move60$prop[i] <- length(which(move60$approach == 1 & move60$focal == move60$focal[i] & move60$partner == move60$partner[i])) / move60$total_move[i]
# }
# hist(move60$prop)
# ggplot(move60)+
#   geom_point(aes(x = total_move,
#                  y = prop),
#              alpha = 0.01)
# 
# move60 <- move60 %>% 
#   mutate(prop_edit = ifelse(prop == 0, 0.000000001,
#                             ifelse(prop == 1, 0.9999999999, prop)))
# 
# move60 <- move60 %>% 
#   filter(p_age_cat != 'unkage')
# 
# #### set prior                                                       ####
# get_prior(formula = prop_edit ~ mo(f_age_num) + age_combo + stim_type + before_after + 
#             (1|focal_id) + (1|stim_id) + (1|playback_id),
#           data = move60,
#           family = Beta())
# 
# priors <- c(
#   # focal age
#   prior(normal(0,1),      class = b,    coef = mof_age_num),
#   prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
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
#   prior(normal(0,1),      class = b,    coef = before_afterbefore))
# 
# ## prior predictive check
# num_chains <- 4
# num_iter <- 2000
# mdprop_prior <- brm(
#   formula = prop_edit ~ mo(f_age_num) + age_combo + stim_type + before_after + 
#     (1|focal_id) + (1|stim_id) + (1|playback_id),
#   data = move60,
#   family = Beta(),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(mdprop_prior) # huge variation in prior, but fairly on both sides so good
# 
# ## reset plotting
# dev.off()
# pdf('../outputs/movement_proportion/movement_proportion_direction_modelchecks.pdf')
# 
# #### fit model                                                       ####
# mdprop_fit <- brm(
#   formula = prop_edit ~ mo(f_age_num) + age_combo + stim_type + before_after +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),
#   data = move60,
#   family = Beta(),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# save.image('movement_direction/moving_proportion_direction_run.RData')
# 
# #### extract draws                                                   ####
# # load('movement_direction/moving_proportion_direction_run.RData')
#
# ## check model diagnostics -- moves very good
# (summary <- summary(mdprop_fit))
# par(mfrow = c(3,1))
# hist(summary$fixed$Rhat, breaks = 50)
# hist(summary$fixed$Bulk_ESS, breaks = 50)
# hist(summary$fixed$Tail_ESS, breaks = 50)
# par(mfrow = c(1,1))
#
# ## extract posterior distribution
# draws <- as_draws_df(mdprop_fit) %>%
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
# marg <- conditional_effects(mdprop_fit,
#                             effects = c('f_age_num','age_combo','stim_type',
#                                         'bda','prev_num'),
#                             categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg) # "f_age_num:cats__" "age_combo:cats__" "stim_type:cats__" "bda:cats__" "prev_num:cats__"
# agefocal_effect <- marg[[1]]
# agecombo_effect <- marg[[2]]
# stim_effect <- marg[[3]]
# bda_effect <- marg[[4]]
# prev_effect <- marg[[5]]
#
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,2))
# hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
# par(mfrow = c(1,1))
#
# #### plot marginal effects                                           ####
# (f_age_num_plot <- ggplot(agefocal_effect)+
#    # geom_ribbon(aes(x = f_age_num,
#    #                 ymax = upper__, ymin = lower__,
#    #                 fill = cats__),
#    #             alpha = 0.4)+
#    # geom_line(aes(x = f_age_num,
#    #               y = estimate__,
#    #               colour = cats__),
#    #           linewidth = 1)+
#    geom_errorbar(aes(x = f_age_num,
#                      ymax = upper__, ymin = lower__,
#                      colour = cats__),
#                  linewidth = 1, width = 0.2)+
#    geom_point(aes(x = f_age_num,
#                   y = estimate__,
#                   colour = cats__),
#               size = 3)+ # cex = 3?
#    xlab(label = 'focal age')+
#    ylab('probability of moving direction')+
#    scale_colour_viridis_d(name = 'moving direction:',
#                           breaks = c('1','2','3','4','5'),
#                           labels = c('move away directly',
#                                      'move away at an angle',
#                                      'move neither towards or away',
#                                      'approach at an angle',
#                                      'approach directly'))+
#    scale_fill_viridis_d(name = 'moving direction:',
#                         breaks = c('1','2','3','4','5'),
#                         labels = c('move away directly',
#                                    'move away at an angle',
#                                    'move neither towards or away',
#                                    'approach at an angle',
#                                    'approach directly'))+
#    theme(legend.position = 'bottom',
#          axis.title = element_text(size = 16),
#          axis.text = element_text(size = 12),
#          legend.title = element_text(size = 12),
#          legend.text = element_text(size = 10)))
# ggsave(plot = f_age_num_plot, filename = '../outputs/movement_proportion/moving_proportion_direction_marginaleffects_agefocal.png', device = 'png',
#        width = 8.3, height = 5.8)
#
# f_age_num_labels <- c('focal age category 1',
#                       'focal age category 2',
#                       'focal age category 3',
#                       'focal age category 4')
# names(f_age_num_labels) <- 1:4
# (agecombo_plot <- agecombo_effect %>%
#     separate(col = age_combo, sep = '_', remove = F,
#              into = c('f_age_num','partner_age')) %>%
#     mutate(agecombo = paste0(f_age_num,'-',partner_age)) %>%
#     ggplot()+
#     geom_errorbar(aes(#x = agecombo,
#       x = partner_age,
#       colour = as.factor(cats__), # moving direction?
#       ymax = upper__, ymin = lower__),
#       linewidth = 1,
#       width = 0.4)+
#     geom_point(aes(#x = agecombo,
#       x = partner_age,
#       colour = as.factor(cats__),    # moving direction?
#       #shape = f_age_num,
#       y = estimate__),
#       size = 3)+
#     # geom_ribbon(aes(#x = agecombo,
#     #                 x = as.numeric(partner_age),
#     #                 fill = as.factor(cats__),     # moving direction?
#     #                 ymax = upper__, ymin = lower__),
#     #             alpha = 0.4)+
#     # geom_line(aes(#x = agecombo,
#     #               x = as.numeric(partner_age),
#     #               colour = as.factor(cats__),     # moving direction?
#     #               y = estimate__),
#     #           linewidth = 1)+
#     facet_wrap(. ~ f_age_num,
#                labeller = labeller(f_age_num = f_age_num_labels))+
#     ylab('probability of moving direction')+
#     scale_colour_viridis_d(name = 'moving direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'move neither towards or away',
#                                       'approach at an angle',
#                                       'approach directly'))+
#     # scale_fill_viridis_d(name = 'moving direction:',
#     #                      breaks = c('1','2','3','4','5'),
#     #                      labels = c('move away directly',
#     #                                 'move away at an angle',
#     #                                 'move neither towards or away',
#     #                                 'approach at an angle',
#     #                                 'approach directly'))+
#     scale_x_discrete(name = 'partner age category')+
#     #scale_x_continuous(name = 'partner age category')+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = agecombo_plot, filename = '../outputs/movement_proportion/moving_proportion_direction_marginaleffects_agepartner.png', device = 'png',
#        width = 8.3, height = 5.8)
#
# (stim_plot <- ggplot(stim_effect)+
#     geom_errorbar(aes(x = stim_type,
#                       ymin = lower__, ymax = upper__,
#                       colour = cats__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type,
#                    y = estimate__,
#                    colour = cats__),
#                cex = 3)+ # size = 3?
#     xlab(label = 'stimulus type') + ylab('probability of moving direction')+
#     scale_colour_viridis_d(name = 'moving direction:',
#                            breaks = c('1','2','3','4','5'),
#                            labels = c('move away directly',
#                                       'move away at an angle',
#                                       'move neither towards or away',
#                                       'approach at an angle',
#                                       'approach directly'))+
#     scale_x_discrete(breaks = c('ctd','l','h'),
#                      labels = c('dove (control)', 'lion', 'human'),
#                      limits = c('ctd','l','h'))+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = stim_plot, filename = '../outputs/movement_proportion/moving_proportion_direction_marginaleffects_stimtype.png',
#        device = 'png', width = 8.3, height = 5.8)
#
# # (f_age_num_plot + agecombo_plot + stim_plot) +
# #   plot_annotation(tag_levels = 'a')
# # ggsave(plot = last_plot(),
# #        filename = '../outputs/movement_proportion/moving_proportion_direction_marginaleffects.png',
# #        device = 'png', width = (5.8*3), height = 8.3)
# print(paste0('marginal effects plotted at ',Sys.time()))
#
# #### posterior predictive check                                      ####
# pp_check(mdprop_fit, ndraws = 100) # really good fit
#
# #### plot traces and density curves                                  ####
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
#                           "b_stim_typeh","b_stim_typel",
#                           "b_bdabefore","b_bdaduring",
#                           "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
#                           "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
#                           "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
#                           "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "bsp_mof_age_num",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           "bsp_moprev_num","simo_moprev_num1[1]","simo_moprev_num1[2]"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mixing generally moves very good, though a couple of age combo ones are a touch wandery
#
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,1))
# hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
# hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')
#
# ## stim type
# lion <- draws_cut %>% filter(parameter == 'b_stim_typel')
# human <- draws_cut %>% filter(parameter == 'b_stim_typeh')
# plot(density(lion$draw), main = 'lion vs dove') ; abline(v = 0, lty = 2)
# plot(density(human$draw), main = 'human vs dove') ; abline(v = 0, lty = 2)
#
# ## focal age
# age1 <- draws_cut %>% filter(parameter == 'bsp_mof_age_num')
# age2 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[1]')
# age3 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[2]')
# age4 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[3]')
# par(mfrow = c(2,2))
# plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
# plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)
#
# ## moving direction in previous second
# prevsec_slope <- draws_cut %>% filter(parameter == 'bsp_moprev_num')
# prevsec2 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[1]')
# prevsec3 <- draws_cut %>% filter(parameter == 'simo_moprev_num1[2]')
# par(mfrow = c(3,1))
# plot(density(prevsec_slope$draw), main = 'slope of prevsec') ; abline(v = 0, lty = 2)
# plot(density(prevsec2$draw), main = 't-1 matched vs younger') ; abline(v = 0, lty = 2)
# plot(density(prevsec3$draw), main = 't-1 older vs younger') ; abline(v = 0, lty = 2)
#
# ## time since stimulus -- come back to this!
# before <- draws_cut %>% filter(parameter == 'b_bdabefore')
# during <- draws_cut %>% filter(parameter == 'b_bdaduring')
# par(mfrow = c(2,1))
# plot(density(before$draw), main = 'after --> before') ; abline(v = 0, lty = 2)
# plot(density(during$draw), main = 'after --> during') ; abline(v = 0, lty = 2)
#
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
#
# #### plot raw                                                        ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
#
# # ## plot overall
# # ggplot(move,aes(x = f_age_num, y = move_index,
# #                 colour = age_combo))+
# #   geom_jitter(alpha = 0.1)+
# #   facet_wrap(. ~ stim_type,
# #              labeller = labeller(stim_type = stim_labels))+
# #   scale_y_continuous(name = 'focal moving direction relative to target',
# #                      breaks = c(1,2,3),
# #                      labels = c('move directly at','side-on','move directly away'))+
# #   labs(colour = 'age difference')
#
# ## plot control data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')
#
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#
# ## plot lion data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')
#
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#
# ## plot human data
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = bda, y = move_index,
#              group = focal))+
#   geom_jitter(colour = rgb(0,0,1,0.05))+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels))+
#   scale_y_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   scale_x_discrete(name = 'time relative to stimulus')
#
# move %>%
#   mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = move_index, fill = bda))+
#   geom_bar(colour = rgb(0,0,1,0.05), position = 'dodge')+
#   facet_grid(f_age_num ~ p_age_num,
#              labeller = labeller(f_age_num = age_labels,
#                                  p_age_num = age_labels))+
#   scale_x_continuous(name = 'focal moving direction relative to target',
#                      breaks = c(1:5),
#                      labels = c('move away directly',
#                                 'move away at an angle',
#                                 'move neither towards or away',
#                                 'approach at an angle',
#                                 'approach directly'))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#
# print(paste0('raw data plotted at ',Sys.time()))
#
# ## reset plotting
# save.image('movement_direction/moving_proportion_direction_run.RData')
# dev.off()
# pdf('../outputs/movement_proportion/moving_proportion_direction_modelpredictions.pdf')
#
# #### predict from model                                              ####
# # load('movement_direction/moving_proportion_direction_run.RData')
# rm(list = ls()[! ls() %in% c('mdprop_fit','move','behav')]) ;
#
# pred <- posterior_epred(object = mdprop_fit,
#                         newdata = move)
# save.image('movement_direction/moving_proportion_direction_modelpredictions.RData')
#
# ## convert to data frame
# move$data_row <- 1:nrow(move)
# extract_predictions <- function(prediction_array, layer, df){
#   predictions <- as.data.frame(prediction_array[,,layer])
#   colnames(predictions) <- 1:nrow(df)
#   predictions <- predictions %>%
#     pivot_longer(cols = everything(),
#                  names_to = 'data_row', values_to = 'epred') %>%
#     mutate(data_row = as.integer(data_row)) %>%
#     left_join(df, by = 'data_row') %>%
#     mutate(pred_type = ifelse(layer == 1, 'move directly away',
#                               ifelse(layer == 2, 'move away at an angle',
#                                      ifelse(layer == 3, 'move neither towards or away',
#                                             ifelse(layer == 4, 'approach at an angle',
#                                                    ifelse(layer == 5, 'approach directly',
#                                                           'CHECK -- PROBLEM IN DATA'))))),
#            pred_type_num = layer)
#   return(predictions)
# }
# pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = move)
# pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = move)
# pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = move)
# pred4 <- extract_predictions(prediction_array = pred, layer = 4, df = move)
# pred5 <- extract_predictions(prediction_array = pred, layer = 5, df = move)
#
# pred <- rbind(pred1, pred2, pred3, pred4, pred5)
# save.image('movement_direction/moving_proportion_direction_modelpredictions.RData')
# rm(pred1, pred2, pred3, pred4, pred5) ; gc()
#
# print(paste0('predictions calculated at ',Sys.time()))
#
# #### plot predictions                                                ####
# # load('movement_direction/moving_proportion_direction_modelpredictions.RData')
#
# ## make labels for movement in previous second
# prevsec_labels <- c('directly away at t-1',
#                     'angle away at t-1',
#                     'neither at t-1',
#                     'angle approach at t-1',
#                     'directly approach at t-1')
# names(prevsec_labels) <- 1:5
#
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# (ctd_plot <- pred %>%
#     filter(stim_type == 'ctd') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('move directly away',
#                                                         'move away at an angle',
#                                                         'move neither towards or away',
#                                                         'approach at an angle',
#                                                         'approach directly')),
#                     colour = factor(pred_type, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'cape turtle dove (control)')+
#     theme(legend.position = 'bottom'))
# (lion_plot <- pred %>%
#     filter(stim_type == 'l') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('move directly away',
#                                                         'move away at an angle',
#                                                         'move neither towards or away',
#                                                         'approach at an angle',
#                                                         'approach directly')),
#                     colour = factor(pred_type, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'lion')+
#     theme(legend.position = 'bottom'))
# (human_plot <- pred %>%
#     filter(stim_type == 'h') %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                     fill = factor(pred_type, levels = c('move directly away',
#                                                         'move away at an angle',
#                                                         'move neither towards or away',
#                                                         'approach at an angle',
#                                                         'approach directly')),
#                     colour = factor(pred_type, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))
#     )) +
#     facet_grid(prev_num ~ bda,
#                labeller = labeller(prev_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted direction of movement relative to focal:',
#          fill = 'predicted direction of movement relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# # (ctd_plot + lion_plot + human_plot)+
# #   plot_amoveotation(tag_levels = 'a')
# # ggsave(plot = last_plot(), file = '../outputs/movement_ordinal_model_2/movement_ordinal_model2_predictions_violin.png',
# #        device = 'png', height = 8, width = 24)
#
# ## reset plotting
# dev.off()
#
# #### calculate posterior contrasts from predictions                  ####
# # load('movement_direction/moving_proportion_direction_modelpredictions.RData')
# pdf('../outputs/movement_proportion/movement_ordinal_model2_modelcontrasts.pdf')
#
# ## stim type                                                         ####
# stim_new <- move %>%
#   dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
#                                                          focal, stim_num, pb_num))))
#
# ## redo predictions with different stimulus types: all doves
# ctd_move <- stim_new %>%
#   mutate(stim_type = 'ctd')
# ctd_mtx <- posterior_epred(object = mdprop_fit, newdata = ctd_move)
# colnames(ctd_mtx) <- ctd_move$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with different stimulus types: all lions
# lion_move <- stim_new %>%
#   mutate(stim_type = 'l')
# lion_mtx <- posterior_epred(object = mdprop_fit, newdata = lion_move)
# colnames(lion_mtx) <- lion_move$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with different stimulus types: all humans
# human_move <- stim_new %>%
#   mutate(stim_type = 'h')
# human_mtx <- posterior_epred(object = mdprop_fit, newdata = human_move)
# colnames(human_mtx) <- human_move$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# save.image('movement_direction/moving_proportion_direction_stimuluscontrasts.RData')
#
# ## calculate contrasts
# ctd_vs_lion <- lion_mtx - ctd_mtx
# ctd_vs_human <- human_mtx - ctd_mtx
# lion_vs_human <- human_mtx - lion_mtx
#
# ## summarise contrasts
# contrasts <- move %>%
#   select(-stim_type) %>%
#   mutate(ctd_vs_lion_mu = apply(ctd_vs_lion, 2, mean),
#          ctd_vs_lion_sd = apply(ctd_vs_lion, 2, sd),
#          ctd_vs_human_mu = apply(ctd_vs_human, 2, mean),
#          ctd_vs_human_sd = apply(ctd_vs_human, 2, sd),
#          lion_vs_human_mu = apply(lion_vs_human, 2, mean),
#          lion_vs_human_sd = apply(lion_vs_human, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(ctd_vs_lion_mu, ctd_vs_human_mu, lion_vs_human_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','mu'),
#            sep = -3, remove = T) %>%
#   select(-mu, -ctd_vs_lion_sd, -ctd_vs_human_sd, -lion_vs_human_sd)
#
# ## produce values for reporting
# median(ctd_vs_lion)  ; mean(ctd_vs_lion)  ; sd(ctd_vs_lion)
# #       -6.260316e-11;       -3.355116e-17;     0.003822004
# median(ctd_vs_human) ; mean(ctd_vs_human) ; sd(ctd_vs_human)
# #       -4.767453e-11;       -3.203541e-17;      0.003070772
# median(lion_vs_human); mean(lion_vs_human); sd(lion_vs_human)
# #        2.588019e-11;       -2.453751e-17;       0.003578163
#
# ## plot contrasts
# contrasts_long %>%
#   mutate(contrast = ifelse(contrast == 'ctd_vs_human',
#                            'dove -> human',
#                            ifelse(contrast == 'ctd_vs_lion',
#                                   'dove -> lion', 'lion -> human'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast))+
#   scale_colour_viridis_d()+
#   labs(colour = 'effect of changing stimulus')
#
# save.image('movement_direction/moving_proportion_direction_stimuluscontrasts.RData')
#
# ## focal age                                                         ####
# # load('movement_direction/moving_proportion_direction_stimuluscontrasts.RData')
# rm(list = ls()[!ls() %in% c('mdprop_fit','move','behav')]) ; gc()
#
# ## create new dataframe to predict from
# age_new <- move %>%
#   dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
#                                                          focal, stim_num, pb_num))))
#
# ## predict with original ages
# age_move_org <- age_new
# age_mtx_org <- posterior_epred(object = mdprop_fit, newdata = age_move_org)
# colnames(age_mtx_org) <- age_move_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# ## redo predictions with altered ages
# age_move_alt <- age_new %>%
#   mutate(f_age_num_original = f_age_num,
#          age_combo_original = age_combo) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
#   separate(age_combo, into = c('f_age_old','p_age'), sep = '_') %>%
#   mutate(age_combo = paste0(f_age_num, '_', p_age)) %>%
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%  # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   dplyr::select(f_age_num_original, f_age_num,
#                 age_combo_original, age_combo,
#                 stim_type, prev_num, bda,
#                 focal, stim_num, pb_num,
#                 unique_data_combo)
# age_mtx_alt <- posterior_epred(object = mdprop_fit, newdata = age_move_alt)
# colnames(age_mtx_alt) <- age_move_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
#
# save.image('movement_direction/moving_proportion_direction_agecontrasts.RData')
#
# ## summarise and convert to long format
# # rm(list = ls()) ; gc() ; load('movement_direction/moving_proportion_direction_agecontrasts.RData')
# age_pred <- age_move_org %>%
#   #dplyr::select(-f_age_num) %>%
#   mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
#          age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
#          age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
#          age_org_prop4_mu = apply(age_mtx_org[,,4], 2, mean),
#          age_org_prop5_mu = apply(age_mtx_org[,,5], 2, mean),
#          age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
#          age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
#          age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
#          age_org_prop4_sd = apply(age_mtx_org[,,4], 2, sd),
#          age_org_prop5_sd = apply(age_mtx_org[,,5], 2, sd),
#          age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
#          age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
#          age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
#          age_alt_prop4_mu = apply(age_mtx_alt[,,4], 2, mean),
#          age_alt_prop5_mu = apply(age_mtx_alt[,,5], 2, mean),
#          age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
#          age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
#          age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd),
#          age_alt_prop4_sd = apply(age_mtx_alt[,,4], 2, sd),
#          age_alt_prop5_sd = apply(age_mtx_alt[,,5], 2, sd)) %>%
#   pivot_longer(cols = c(age_org_prop1_mu, age_org_prop2_mu, age_org_prop3_mu,
#                         age_org_prop4_mu, age_org_prop5_mu,
#                         age_alt_prop1_mu, age_alt_prop2_mu, age_alt_prop3_mu,
#                         age_alt_prop4_mu, age_alt_prop5_mu),
#                names_to = 'focal_agemove_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(age_org_prop1_sd, age_org_prop2_sd, age_org_prop3_sd,
#                         age_org_prop4_sd, age_org_prop5_sd,
#                         age_alt_prop1_sd, age_alt_prop2_sd, age_alt_prop3_sd,
#                         age_alt_prop4_sd, age_alt_prop5_sd),
#                names_to = 'focal_agemove_sd', values_to = 'stdv_propn') %>%
#   separate(col = focal_agemove_mu, into = c('focal_agemove_mu','mu'),
#            sep = '_m', remove = T) %>%
#   separate(col = focal_agemove_sd, into = c('focal_agemove_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-mu, -sd) %>%
#   filter(focal_agemove_mu == focal_agemove_sd) %>%
#   separate(col = focal_agemove_mu, into = c('original_altered', 'move_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-focal_agemove_sd) %>%
#   mutate(move_pred = as.numeric(move_pred),
#          f_age_num = ifelse(original_altered == 'age_org',
#                             f_age_num,
#                             ifelse(original_altered == 'age_alt' & f_age_num == 4,
#                                    1, f_age_num + 1))) %>%
#   mutate(pred_type = ifelse(move_pred == 1, 'move away directly',
#                             ifelse(move_pred == 2, 'move away at an angle',
#                                    ifelse(move_pred == 3, 'neither approach or retreat',
#                                           ifelse(move_pred == 4, 'approach at an angle',
#                                                  'approach directly')))))
#
# ## calculate contrasts
# alt_vs_org_awaydirect <- age_mtx_alt[,,1] - age_mtx_org[,,1]
# alt_vs_org_awayangle  <- age_mtx_alt[,,2] - age_mtx_org[,,2]
# alt_vs_org_neither    <- age_mtx_alt[,,3] - age_mtx_org[,,3]
# alt_vs_org_twdsangle  <- age_mtx_alt[,,4] - age_mtx_org[,,4]
# alt_vs_org_twdsdirect <- age_mtx_alt[,,5] - age_mtx_org[,,5]
#
# ## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
# mean(alt_vs_org_awaydirect); sd(alt_vs_org_awaydirect)
# #            -0.0002091124 ± 0.005366929
# mean(alt_vs_org_awayangle) ; sd(alt_vs_org_awayangle)
# #             0.0003604267 ± 0.008324114
# mean(alt_vs_org_neither)   ; sd(alt_vs_org_neither)
# #             5.762972e-05 ± 0.006689415
# mean(alt_vs_org_twdsangle) ; sd(alt_vs_org_twdsangle)
# #            -9.615321e-05 ± 0.008087485
# mean(alt_vs_org_twdsdirect); sd(alt_vs_org_twdsdirect)
# #            -0.0001127909 ± 0.006072838
#
# ## repeat excluding age category 4 because different contrast
# mean(alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     # 0.0001874596
# sd(  alt_vs_org_awaydirect[,which(age_move_org$f_age_num != 4)])     # 0.00500905
# mean(alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])      # 0.0003329584
# sd(  alt_vs_org_awayangle[,which(age_move_org$f_age_num != 4)])      # 0.008628699
# mean(alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])        # -2.34897e-05
# sd(  alt_vs_org_neither[,which(age_move_org$f_age_num != 4)])        # 0.006254979
# mean(alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])      # 0.0001419014
# sd(  alt_vs_org_twdsangle[,which(age_move_org$f_age_num != 4)])      # 0.008315686
# mean(alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # -0.0006388297
# sd(  alt_vs_org_twdsdirect[,which(age_move_org$f_age_num != 4)])     # 0.005419001
#
# ## split contrasts by original age category
# age1v2_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 1)]
# age2v3_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 2)]
# age3v4_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 3)]
# age1v4_ad <- alt_vs_org_awaydirect[,which(age_move_org$f_age_num == 4)] * (-1)
# age1v2_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 1)]
# age2v3_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 2)]
# age3v4_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 3)]
# age1v4_aa <- alt_vs_org_awayangle[,which(age_move_org$f_age_num == 4)] * (-1)
# age1v2_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 1)]
# age2v3_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 2)]
# age3v4_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 3)]
# age1v4_n  <- alt_vs_org_neither[,which(age_move_org$f_age_num == 4)] * (-1)
# age1v2_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 1)]
# age2v3_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 2)]
# age3v4_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 3)]
# age1v4_ta <- alt_vs_org_twdsangle[,which(age_move_org$f_age_num == 4)] * (-1)
# age1v2_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 1)]
# age2v3_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 2)]
# age3v4_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 3)]
# age1v4_td <- alt_vs_org_twdsdirect[,which(age_move_org$f_age_num == 4)] * (-1)
#
# ## calculate contrast values
# mean(age1v2_ad) ; sd(age1v2_ad) #   0.001598347 ± 0.004651106
# mean(age2v3_ad) ; sd(age2v3_ad) #   0.002304834 ± 0.004418379
# mean(age3v4_ad) ; sd(age3v4_ad) #  -0.002004788 ± 0.004642566
# mean(age1v4_ad) ; sd(age1v4_ad) #   0.002430653 ± 0.006617351
# mean(age1v2_aa) ; sd(age1v2_aa) #  0.0006133569 ± 0.008945386
# mean(age2v3_aa) ; sd(age2v3_aa) #  0.0004832358 ± 0.008219821
# mean(age3v4_aa) ; sd(age3v4_aa) #  0.0001421945 ± 0.008918144
# mean(age1v4_aa) ; sd(age1v4_aa) # -0.0005143003 ± 0.006351178
# mean(age1v2_n)  ; sd(age1v2_n)  #  -0.000426104 ± 0.007017158
# mean(age2v3_n)  ; sd(age2v3_n)  #   0.000202071 ± 0.006216517
# mean(age3v4_n)  ; sd(age3v4_n)  # -0.0001491432 ± 0.006123056
# mean(age1v4_n)  ; sd(age1v4_n)  # -0.0005120494 ± 0.008718205
# mean(age1v2_ta) ; sd(age1v2_ta) #  2.209731e-05 ± 0.009590372
# mean(age2v3_ta) ; sd(age2v3_ta) # -9.723911e-05 ± 0.007675877
# mean(age3v4_ta) ; sd(age3v4_ta) #  0.0003817826 ± 0.008593495
# mean(age1v4_ta) ; sd(age1v4_ta) #   0.001429702 ± 0.006507124
# mean(age1v2_td) ; sd(age1v2_td) #  -0.001807697 ± 0.006288685
# mean(age2v3_td) ; sd(age2v3_td) #  -0.002892902 ± 0.004759463
# mean(age3v4_td) ; sd(age3v4_td) #   0.001629954 ± 0.004841764
# mean(age1v4_td) ; sd(age1v4_td) #  -0.002834005 ± 0.008290771
#
# ## summarise contrasts
# contrasts <- move %>%
#   mutate(alt_vs_org_awaydirect_mu = apply(alt_vs_org_awaydirect, 2, mean),
#          alt_vs_org_awaydirect_sd = apply(alt_vs_org_awaydirect, 2, sd),
#          alt_vs_org_awayangle_mu  = apply(alt_vs_org_awayangle,  2, mean),
#          alt_vs_org_awayangle_sd  = apply(alt_vs_org_awayangle,  2, sd),
#          alt_vs_org_neither_mu    = apply(alt_vs_org_neither,    2, mean),
#          alt_vs_org_neither_sd    = apply(alt_vs_org_neither,    2, sd),
#          alt_vs_org_twdsangle_mu  = apply(alt_vs_org_twdsangle,  2, mean),
#          alt_vs_org_twdsangle_sd  = apply(alt_vs_org_twdsangle,  2, sd),
#          alt_vs_org_twdsdirect_mu = apply(alt_vs_org_twdsdirect, 2, mean),
#          alt_vs_org_twdsdirect_sd = apply(alt_vs_org_twdsdirect, 2, sd)
#   ) %>%
#   mutate(categories_different = ifelse(f_age_num == 4,
#                                        '3 categories different',
#                                        '1 category different'))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(alt_vs_org_awaydirect_mu, alt_vs_org_awayangle_mu,
#                         alt_vs_org_neither_mu, alt_vs_org_twdsangle_mu,
#                         alt_vs_org_twdsdirect_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('alt','vs','org','move_pred','mu'),
#            sep = '_', remove = T) %>%
#   select(-alt_vs_org_awaydirect_sd, -alt_vs_org_awayangle_sd, -alt_vs_org_neither_sd,
#          -alt_vs_org_twdsangle_sd, -alt_vs_org_twdsdirect_sd, -alt, -vs, -org, -mu)
#
# ## plot contrasts
# age_pred %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('move away directly',
#                                        'move away at an angle',
#                                        'neither approach or retreat',
#                                        'approach at an angle',
#                                        'approach directly'))) %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = pred_type, fill = pred_type),
#                alpha = 0.5)+
#   facet_grid(pred_type ~ stim_type)
# contrasts_long %>%
#   mutate(pred_type = ifelse(move_pred == 'awaydirect',
#                             'move away directly',
#                             ifelse(move_pred == 'awayangle',
#                                    'move away at an angle',
#                                    ifelse(move_pred == 'neither',
#                                           'neither approach or retreat',
#                                           ifelse(move_pred == 'twdsangle',
#                                                  'approach at an angle',
#                                                  'approach directly'))))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('move away directly',
#                                        'move away at an angle',
#                                        'neither approach or retreat',
#                                        'approach at an angle',
#                                        'approach directly'))) %>%
#   mutate(f_age_new = ifelse(f_age_num == 4,
#                             'youngest to oldest',
#                             paste0('category ',f_age_num,' to ',f_age_num+1))) %>%
#   mutate(difference = ifelse(f_age_num == 4,
#                              difference * (-1),
#                              difference)) %>%
#   ggplot()+
#   geom_density(aes(x = difference,
#                    colour = categories_different,
#                    fill = categories_different),
#                alpha = 0.5)+
#   geom_vline(xintercept = 0, linetype = 2)+
#   facet_grid(pred_type ~ f_age_new, scales = 'free_y')+
#   scale_colour_viridis_d(begin = 0.5, end = 0)+
#   scale_fill_viridis_d(begin = 0.5, end = 0)+
#   labs(colour = 'categories\ndifferent',
#        fill = 'categories\ndifferent')
# save.image('movement_direction/moving_proportion_direction_agecontrasts.RData')
#
# ## clean up a bit
# rm(list = ls()[! ls() %in% c('alt_vs_org_awaydirect','alt_vs_org_awayangle',
#                              'alt_vs_org_neither','alt_vs_org_twdsangle',
#                              'alt_vs_org_twdsdirect','move','mdprop_fit','behav')]) ; gc()
#
# ## plot full density instead of means
# colnames(alt_vs_org_awaydirect) <- move$data_row
# colnames(alt_vs_org_awayangle)  <- move$data_row
# colnames(alt_vs_org_neither)    <- move$data_row
# colnames(alt_vs_org_twdsangle)  <- move$data_row
# colnames(alt_vs_org_twdsdirect) <- move$data_row
#
# mtx_to_df <- function(mtx, pred_type){
#   df <- mtx %>%
#     as.data.frame() %>%
#     pivot_longer(cols = everything(),
#                  names_to = 'data_row',
#                  values_to = 'contrast') %>%
#     mutate(data_row = as.integer(data_row)) %>%
#     left_join(move, by = 'data_row') %>%
#     mutate(categories = factor(ifelse(f_age_num == 1,
#                                       "10-15 to 16-20",
#                                       ifelse(f_age_num == 2,
#                                              "16-20 to 21-25",
#                                              ifelse(f_age_num == 3,
#                                                     "21-25 to 26-35",
#                                                     "10-15 to 26-35"))),
#                                levels = c("10-15 to 16-20", "16-20 to 21-25",
#                                           "21-25 to 26-35","10-15 to 26-35"))) %>%
#     mutate(contrast = ifelse(f_age_num == 4,
#                              contrast * (-1), # age_contrast shows 4 -> 1 not 1-> 4
#                              contrast),
#            diff_cats = ifelse(f_age_num == 4,
#                               'youngest to oldest', 'increase by one'),
#            prediction_type = pred_type)
#   return(df)
# }
# ad <- mtx_to_df(alt_vs_org_awaydirect, pred_type = 'move directly away')
# aa <- mtx_to_df(alt_vs_org_awayangle, pred_type = 'move away at an angle')
# n <- mtx_to_df(alt_vs_org_neither, pred_type = 'neither approach or retreat')
# ta <- mtx_to_df(alt_vs_org_twdsangle, pred_type = 'approach at an angle')
# td <- mtx_to_df(alt_vs_org_twdsdirect, pred_type = 'approach directly')
#
# plot_contrasts <- rbind(aa, ad, n, ta, td) %>%
#   mutate(prediction_type = factor(prediction_type,
#                                   levels = c('move directly away',
#                                              'move away at an angle',
#                                              'neither approach or retreat',
#                                              'approach at an angle',
#                                              'approach directly')))
#
# ggplot(plot_contrasts)+
#   geom_density(aes(x = contrast,
#                    fill = diff_cats, # fill = f_age_cat,
#                    colour = diff_cats # colour = f_age_cat
#   ),
#   #fill = '#21918c', colour = '#21918c',
#   alpha = 0.4)+
#   scale_colour_viridis_d(begin = 0, end = 0.5)+
#   scale_fill_viridis_d(begin = 0, end = 0.5)+
#   geom_vline(xintercept = 0, linetype = 2)+
#   facet_grid(prediction_type ~ categories, scales = 'free_y')+#, nrow = 3, ncol = 4)+
#   labs(x = 'contrast between age categories',
#        fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
#        colour = 'change in age\ncategory'  # colour = 'original\nage category'
#   )+
#   theme(legend.position = 'none')+ #c(0.8, 0.9))+
#   theme_bw()
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'movement_ordinal2bda_agecontrasts.png',
#        path = '../outputs/movement_proportion/',
#        width = 2400, height = 3200, unit = 'px')
# ggsave(plot = last_plot(), device = 'svg',
#        filename = 'movement_ordinal2bda_agecontrasts.svg',
#        path = '../outputs/movement_proportion/',
#        width = 2400, height = 3200, unit = 'px')
#
# ## time since stimulus                                               ####
# # load('movement_direction/moving_proportion_direction_agecontrasts.RData')
# rm(list = ls()[!ls() %in% c('mdprop_fit','move','behav')]) ; gc()
# 
# ## create new dataframe to predict from
# time_new <- move %>%
#   dplyr::select(f_age_num, age_combo, stim_type, prev_num, bda,
#                 focal, stim_num, pb_num) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, prev_num, bda,
#                                                          focal, stim_num, pb_num))))
# 
# # ## predict with original times
# # time_move_org <- time_new
# # time_mtx_org <- posterior_epred(object = mdprop_fit, newdata = time_move_org)
# # colnames(time_mtx_org) <- time_move_org$unique_data_combo
# # time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions: all before
# move_before <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'before') %>%
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, bda, focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# move_before_mtx <- posterior_epred(object = mdprop_fit, newdata = move_before)
# colnames(move_before_mtx) <- move_before$unique_data_combo
# move_before_mtx <- move_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions: all during
# move_during <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'during') %>%
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, bda,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# move_during_mtx <- posterior_epred(object = mdprop_fit, newdata = move_during)
# colnames(move_during_mtx) <- move_during$unique_data_combo
# move_during_mtx <- move_during_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions: all after
# move_after <- time_new %>%
#   mutate(bda_org = bda) %>%
#   mutate(bda = 'after') %>%
#   #mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, move_tminus1_num, bda,focal_id, stim_id, playback_id)))) %>% # commented out because I THINK this should actually be the same as before and not overwritten with the new f_age_num, but I'm not certain
#   relocate(bda_org)
# move_after_mtx <- posterior_epred(object = mdprop_fit, newdata = move_after)
# colnames(move_after_mtx) <- move_after$unique_data_combo
# move_after_mtx <- move_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('movement_direction/moving_proportion_direction_timecontrasts.RData')
# 
# ## calculate contrasts
# before_vs_during <- move_during_mtx - move_before_mtx
# before_vs_after  <- move_after_mtx  - move_before_mtx
# during_vs_after  <- move_after_mtx  - move_during_mtx
# 
# ## summarise contrasts
# contrasts <- move %>%
#   select(-stim_type) %>%
#   mutate(before_vs_during_mu = apply(before_vs_during, 2, mean),
#          before_vs_during_sd = apply(before_vs_during, 2, sd),
#          before_vs_after_mu  = apply(before_vs_after,  2, mean),
#          before_vs_after_sd  = apply(before_vs_after,  2, sd),
#          during_vs_after_mu  = apply(during_vs_after,  2, mean),
#          during_vs_after_sd  = apply(during_vs_after,  2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(before_vs_during_mu, before_vs_after_mu, during_vs_after_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','mu'),
#            sep = -3, remove = T) %>%
#   select(-mu, -before_vs_during_sd, -before_vs_after_sd, -during_vs_after_sd)
# 
# ## produce values for reporting
# median(before_vs_during); mean(before_vs_during); sd(before_vs_during)
# #           1.378635e-10;             3.5256e-17;          0.004788988
# median(before_vs_after) ; mean(before_vs_after) ; sd(before_vs_after)
# #          1.046273e-11 ;         -1.473294e-17 ;         0.001715361
# median(during_vs_after) ; mean(during_vs_after) ; sd(during_vs_after)
# #         -1.167906e-10 ;         -3.457326e-17 ;         0.004501539
# 
# ## plot contrasts
# contrasts_long %>%
#   mutate(contrast = ifelse(contrast == 'before_vs_during',
#                            'before -> during',
#                            ifelse(contrast == 'before_vs_after',
#                                   'before -> after', 'during -> after'))) %>%
#   ggplot()+
#   geom_density(aes(x = difference, colour = contrast))+
#   scale_colour_viridis_d()+
#   labs(colour = 'effect of changing stimulus')
# 
# save.image('movement_direction/moving_proportion_direction_timecontrasts.RData')
# dev.off()
# 
# rm(list = ls()) ; gc()
# print('movement direction completed')
