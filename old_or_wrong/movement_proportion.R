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

ggplot(prop_visible)+
  geom_boxplot(aes(x = factor(stim_type, levels = c('ctd','l','h')),
                   y = prop_move_section, fill = before_after),
               notch = T)+
  scale_fill_viridis_d()

#### set priors ####
# set priors
get_prior(formula = prop_move_section ~ 0 + mo(f_age_num) * stim_type * before_after + 
            #s(visible_section) +
            (1|focal_id) + (1|stim_id) + (1|playback_id),
          data = prop,
          family = gaussian(link = 'identity'))

# set priors
priors <- c(
  # focal age
  prior(normal(0,0.25),    class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num1),
  # stimulus type
  prior(normal(0,0.25),    class = b,    coef = stim_typectd),
  prior(normal(0,0.25),    class = b,    coef = stim_typel),
  prior(normal(0,0.25),    class = b,    coef = stim_typeh),
  # # spline: seconds visible
  # prior(normal(0,0.25),    class = b,    coef = svisible_section_1),
  # time
  prior(normal(0,0.25),    class = b,    coef = before_afterbefore),
  # interactions
  prior(normal(0,0.25),    class = b,    coef = mof_age_num:before_afterbefore),
  prior(normal(0,0.25),    class = b,    coef = mof_age_num:stim_typeh),
  prior(normal(0,0.25),    class = b,    coef = mof_age_num:stim_typeh:before_afterbefore),
  prior(normal(0,0.25),    class = b,    coef = mof_age_num:stim_typel),
  prior(normal(0,0.25),    class = b,    coef = mof_age_num:stim_typel:before_afterbefore),
  prior(normal(0,0.25),    class = b,    coef = stim_typeh:before_afterbefore),
  prior(normal(0,0.25),    class = b,    coef = stim_typel:before_afterbefore),
  
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num:before_afterbefore1),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num:stim_typeh:before_afterbefore1),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num:stim_typeh1),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num:stim_typel:before_afterbefore1),
  prior(dirichlet(2,2,2),  class = simo, coef = mof_age_num:stim_typel1)
  )

#### prior predictive check ####
num_chains <- 4
num_iter <- 2000
prop_prior <- brm(
  formula = logit_prop_section ~ 0 + mo(f_age_num) * stim_type * before_after + 
    #s(visible_section) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = prop,
  family = gaussian(link = 'identity'),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(prop_prior)

print(paste0('priors set and checked at ', Sys.time()))

## reset plotting
dev.off()
pdf('../outputs/movement_proportion/')

#### fit model ####
prop_fit <- brm(
  formula = logit_prop_section ~ 0 + mo(f_age_num) * stim_type * before_after + 
    #s(visible_section) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = prop,
  family = gaussian(link = 'identity'),
  prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  control = list(adapt_delta = 0.9))

# save workspace
save.image('movement_direction/movement_proportion_run.RData') # save.image('ele_playbacks/movement_direction/movement_binomial_run.RData')

# inspect model
summary(prop_fit)

print(paste0('model fitted at ', Sys.time()))

#### check outputs ####
# load('movement_direction/movement_binomial_run.RData') # load('ele_playbacks/movement_direction/movement_binomial_run.RData')

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
# names(marg)
# # "stim_type" "before_after" "f_age_num" "visible_section"
# stim_effect <- marg[[1]]
# time_effect <- marg[[2]]
# agef_effect <- marg[[3]]
# # vsbl_effect <- marg[[4]]
names(marg)
# "stim_type" "before_after" "stim_type:before_after" "f_age_num" "f_age_num:stim_type" "f_age_num:before_after"
stim_effect <- marg[[1]]
time_effect <- marg[[2]]
stba_effect <- marg[[3]]
agef_effect <- marg[[4]]
fast_effect <- marg[[5]]
faba_effect <- marg[[6]]

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
ggsave(plot = stim_plot, filename = '../outputs/movement_binomial_model/movement_marginaleffects_stimtype_agecombo.png', device = 'png',
       width = 8.3, height = 5.8)

(focal_age_plot + stim_plot) +
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(),
       filename = '../outputs/movement_binomial_model/movement_marginaleffects.png',
       device = 'png', width = (5.8*3), height = 8.3)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check ####
pp_check(prop_fit, ndraws = 100) # good position of predictions but doesn't fit multimodal shape

#### plot traces and density curves ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_stim_typectd","b_stim_typeh","b_stim_typel",
                          "b_before_afterbefore",
                          #"bs_svisible_section_1","sds_svisible_section_1",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "sigma"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

interactions <- draws %>%
  filter(parameter %in% c("b_stim_typeh:before_afterbefore","b_stim_typel:before_afterbefore",
                          "bsp_mof_age_num:stim_typeh","bsp_mof_age_num:stim_typel",
                          "bsp_mof_age_num:before_afterbefore",
                          "bsp_mof_age_num:stim_typeh:before_afterbefore",
                          "bsp_mof_age_num:stim_typel:before_afterbefore",
                          "simo_mof_age_num:stim_typeh1[1]","simo_mof_age_num:stim_typeh1[2]","simo_mof_age_num:stim_typeh1[3]",
                          "simo_mof_age_num:stim_typel1[1]","simo_mof_age_num:stim_typel1[2]","simo_mof_age_num:stim_typel1[3]",
                          "simo_mof_age_num:before_afterbefore1[1]","simo_mof_age_num:before_afterbefore1[2]","simo_mof_age_num:before_afterbefore1[3]",
                          "simo_mof_age_num:stim_typeh:before_afterbefore1[1]",
                          "simo_mof_age_num:stim_typeh:before_afterbefore1[2]",
                          "simo_mof_age_num:stim_typeh:before_afterbefore1[3]",
                          "simo_mof_age_num:stim_typel:before_afterbefore1[1]",
                          "simo_mof_age_num:stim_typel:before_afterbefore1[2]",
                          "simo_mof_age_num:stim_typel:before_afterbefore1[3]"))
ggplot(data = interactions,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

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
                 colour = before_after))+
  facet_grid(f_age_num ~ stim_type,
             labeller = labeller(f_age_num = age_labels,
                                 stim_type = stim_labels))+
  scale_x_continuous(name = 'number of seconds visible')+
  scale_y_continuous(name = 'proportion of time spent moving')+
  scale_colour_viridis_d()
prop %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h'))) %>% 
  ggplot()+
  geom_boxplot(aes(x = factor(f_age_num),
                   y = prop_move_section,
                   fill = before_after))+
  facet_grid(. ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  labs(x = 'number of seconds visible',
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
  facet_wrap(. ~ stim_type)+
  labs(x = 'proportion of time spent moving',
       y = 'stimulus type',
       fill = 'time relative\nto start of\nstimulus')+
  scale_fill_viridis_d()
print(paste0('raw data plotted at ',Sys.time()))

## reset plotting
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
pred %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>% 
  mutate(epred = invlogit(epred)) %>% 
  ggplot()+
  geom_boxplot(aes(x = as.factor(f_age_num), y = epred,
                   fill = before_after)) +
  facet_grid(. ~ stim_type)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(fill = 'time relative to stimulus:',
       x = 'age category of focal elephant',
       y = 'predicted probability of moving')+
  theme(legend.position = 'bottom')

pred %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>% 
  mutate(epred = invlogit(epred)) %>% 
  ggplot()+
  geom_density_ridges(aes(y = f_age_cat,
                          x = epred,
                          fill = before_after),
                      alpha = 0.1) +
  facet_grid(. ~ stim_type)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(fill = 'time relative to stimulus:',
       y = 'age category of focal elephant',
       x = 'predicted probability of moving')+
  theme(legend.position = 'bottom')

## plot against raw data
pred[1:13400,] %>% 
  group_by(data_row) %>% 
  mutate(epred = mean(epred)) %>% 
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>% 
  ggplot()+
  geom_point(aes(x = logit_prop_section,
                 y = epred,
                 colour = f_age_cat))+
  facet_grid(stim_type ~ before_after)+
  scale_colour_viridis_d()+
  geom_abline(slope = 1, intercept = 0)

## reset plotting
dev.off()
pdf('../outputs/movement_binomial_model/movement_binomial_modelcontrasts.pdf')

rm(list = ls()[! ls() %in% c('move','focals')]) ; gc()

