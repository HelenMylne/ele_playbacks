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

############ neighbour proportion ############
# pdf('../outputs/neighbour_proportion/neighbour_proportion_binomial_dataprep.pdf')
# set.seed(12345)
# 
# #### import data ####
# load('nearest_neighbour/neighbour_binomial_run.RData')
# rm(list = ls()[ ! ls() %in% c('nn')]) ; gc()
# 
# #### filter data ####
# nn_no_na <- nn %>%
#   # convert to binary move or no move
#   mutate(action_name = ifelse(action_name == '0',
#                                    'not_neighbours', 'neighbours')) %>%
#   # add variable for before or after stimulus starts
#   mutate(before_after = ifelse(bda == 'before', 'before', 'after')) %>%
#   # clean up
#   mutate(f_age_num = as.integer(f_age_num)) %>%
#   mutate(focal_id = as.integer(as.factor(focal)),
#          stim_num = as.integer(as.factor(stim_num))) %>%
#   rename(stim_id = stim_num,
#          playback_id = pb_num) %>%
#   select(focal, action_name, action,
#          f_age_cat, f_age_num,
#          p_age_cat, p_age_num,
#          stim_type, before_after, second,
#          focal_id, stim_id, playback_id) %>%
#   distinct()
# str(nn_no_na)
# 
# nn_no_na <- nn_no_na %>% 
#   mutate(age_combo = paste0('F',f_age_num,'P',p_age_num)) %>% 
#   mutate(age_combo_long = paste0(f_age_cat,' to ',p_age_cat, ' yrs'))
# 
# #### plot raw ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# ## plot overall
# (b <- nn_no_na %>%
#   filter(before_after == 'before') %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h')),
#          action_name = ifelse(action_name == 'neighbours', 'yes', 'no')) %>%
#   ggplot()+
#   geom_bar(aes(x = action_name,
#                fill = f_age_cat),
#            position = 'dodge')+
#   facet_grid(p_age_num ~ stim_type,
#              labeller = labeller(stim_type = stim_labels,
#                                  p_age_num = age_labels))+
#   labs(fill = 'focal age (years)',
#        y = 'seconds as neighbours',
#        x = 'neighbours',
#        title = 'before')+
#   scale_fill_viridis_d())
# (a <- nn_no_na %>%
#   filter(before_after == 'after') %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h')),
#          action_name = ifelse(action_name == 'neighbours', 'yes', 'no')) %>%
#   ggplot()+
#   geom_bar(aes(x = action_name,
#                fill = f_age_cat),
#            position = 'dodge')+
#   facet_grid(p_age_num ~ stim_type,
#              labeller = labeller(stim_type = stim_labels,
#                                  p_age_num = age_labels))+
#   labs(fill = 'focal age (years)',
#        y = 'seconds as neighbours',
#        x = 'neighbours',
#        title = 'after')+
#   scale_fill_viridis_d())
# 
# library(patchwork)
# (b + a)+
#   plot_annotation(tag_levels = 'a')+
#   plot_layout(guides = "collect") & theme(legend.position = 'bottom')
# 
# nn_no_na %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd', 'l', 'h')),
#          # action_name = ifelse(action_name == 'neighbours', 'yes', 'no'),
#          before_after = factor(before_after, levels = c('before', 'after'))) %>%
#   ggplot()+
#   geom_bar(aes(x = age_combo,
#                fill = action_name),
#            position = 'stack')+
#   facet_grid(stim_type ~ before_after,
#              labeller = labeller(stim_type = stim_labels))+
#   labs(fill = 'neighbours',
#        y = 'seconds observed',
#        x = 'age combination')+
#   scale_fill_viridis_d()+
#   theme(axis.text.x = element_text(angle = 90))
# 
# ## reset plotting
# save.image('nearest_neighbour/neighbour_proportion_binomial_run.RData')
# print(paste0('raw data plotted at ',Sys.time()))
# 
# #### set priors ####
# # set priors
# get_prior(formula = action ~ 0 + age_combo + stim_type * before_after +
#             (1|focal_id) + (1|stim_id) + (1|playback_id),          # random effects
#           data = nn_no_na,
#           family = bernoulli("logit"))
# 
# # set priors
# priors <- c(
#   # age combination
#   prior(normal(-1,1), class = b, coef = age_comboF1P1),
#   prior(normal(-1,1), class = b, coef = age_comboF1P2),
#   prior(normal(-1,1), class = b, coef = age_comboF1P3),
#   prior(normal(-1,1), class = b, coef = age_comboF1P4),
#   prior(normal(-1,1), class = b, coef = age_comboF2P1),
#   prior(normal(-1,1), class = b, coef = age_comboF2P2),
#   prior(normal(-1,1), class = b, coef = age_comboF2P3),
#   prior(normal(-1,1), class = b, coef = age_comboF2P4),
#   prior(normal(-1,1), class = b, coef = age_comboF3P1),
#   prior(normal(-1,1), class = b, coef = age_comboF3P2),
#   prior(normal(-1,1), class = b, coef = age_comboF3P3),
#   prior(normal(-1,1), class = b, coef = age_comboF3P4),
#   prior(normal(-1,1), class = b, coef = age_comboF4P1),
#   prior(normal(-1,1), class = b, coef = age_comboF4P2),
#   prior(normal(-1,1), class = b, coef = age_comboF4P3),
#   prior(normal(-1,1), class = b, coef = age_comboF4P4),
#   # stimulus type
#   # prior(normal(-1,1), class = b, coef = stim_typectd),
#   prior(normal(-1,1), class = b, coef = stim_typel),
#   prior(normal(-1,1), class = b, coef = stim_typeh),
#   # time
#   prior(normal(-1,1), class = b, coef = before_afterbefore),
#   # interactions
#   prior(normal(-1,1), class = b, coef = stim_typeh:before_afterbefore),
#   prior(normal(-1,1), class = b, coef = stim_typel:before_afterbefore)
# )
# 
# num_chains <- 4
# num_iter <- 2000
# 
# #### prior predictive check ####
# prop_prior <- brm(
#   formula = action ~ 0 + age_combo + stim_type * before_after +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),          # random effects
#   data = nn_no_na,
#   family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(prop_prior)
# 
# print(paste0('priors set and checked at ', Sys.time()))
# 
# #### fit model ####
# prop_fit <- brm(
#   formula = action ~ 0 + age_combo + stim_type * before_after +
#     (1|focal_id) + (1|stim_id) + (1|playback_id),          # random effects
#   data = nn_no_na,
#   family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   control = list(adapt_delta = 0.9))
# 
# # save workspace
# save.image('nearest_neighbour/neighbour_proportion_binomial_run.RData')
# 
# # inspect model
# summary(prop_fit)
# 
# print(paste0('model fitted at ', Sys.time()))
# dev.off()
# 
# #### check outputs ####
# pdf('../outputs/neighbour_proportion/neighbour_proportion_binomial_modelchecks.RData')
# load('nearest_neighbour/neighbour_proportion_binomial_run.RData') # load('ele_playbacks/nearest_neighbour/neighbour_proportion_binomial_run.RData')
# 
# ## check Stan code
# prop_fit$model
# prop_fit$formula
# 
# ## extract posterior distribution
# draws <- as_draws_df(prop_fit) %>%
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
# print(paste0('posterior extracted at ',Sys.time()))
# 
# #### plot marginal effects ####
# ## extract marginal effects
# marg <- conditional_effects(prop_fit,
#                             method = 'posterior_epred')
# names(marg)
# # "age_combo" "stim_type" "before_after" "stim_type:before_after"
# agec_effect <- marg[[1]]
# stim_effect <- marg[[2]]
# time_effect <- marg[[3]]
# stba_effect <- marg[[4]]
# 
# ## plot marginal effects
# (age_plot <- agec_effect %>% 
#     separate(age_combo, into = c('f_age','p_age'),
#              remove = F, sep = 2) %>% 
#     ggplot()+
#     geom_errorbar(aes(x = age_combo,
#                       ymax = upper__, ymin = lower__,
#                       colour = f_age,
#                       linetype = p_age),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = age_combo,
#                    #colour = cats__,
#                    y = estimate__,
#                    colour = f_age,
#                    shape = p_age),
#                size = 3)+ # cex = 3?
#     labs(x = 'focal age',
#          y = 'probability of nearest neighbour',
#          colour = 'focal age',
#          shape = 'partner age',
#          linetype = 'partner age')+
#     scale_colour_viridis_d()+
#     theme(axis.title = element_text(size = 16),
#           axis.text.x = element_text(size = 12,
#                                      angle = 90),
#           axis.text.y = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)))
# ggsave(plot = age_plot, filename = '../outputs/neighbour_proportion/neighbour_proportion_binomial_marginaleffects_focalage.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# (stim_plot <- ggplot(stim_effect)+
#     geom_errorbar(aes(x = stim_type,
#                       ymin = lower__, ymax = upper__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type,
#                    #colour = cats__,
#                    y = estimate__),
#                cex = 3)+ # size = 3?
#     xlab(label = 'stimulus type') + ylab('probability of neighbours')+
#     scale_x_discrete(breaks = c('ctd','l','h'),
#                      labels = c('dove (control)', 'lion', 'human'),
#                      limits = c('ctd','l','h'))+
#     theme(axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = stim_plot, filename = '../outputs/neighbour_proportion/neighbour_marginaleffects_stimtype_agecombo.png', device = 'png',
#        width = 8.3, height = 5.8)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check ####
# pp_check(prop_fit, ndraws = 100)
# 
# #### plot traces and density curves ####
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_stim_typeh","b_stim_typel",
#                           "b_before_afterbefore",
#                           "b_stim_typeh:before_afterbefore","b_stim_typel:before_afterbefore",
#                           "b_age_comboF1P1","b_age_comboF1P2","b_age_comboF1P3","b_age_comboF1P4",
#                           "b_age_comboF2P1","b_age_comboF2P2","b_age_comboF2P3","b_age_comboF2P4",
#                           "b_age_comboF3P1","b_age_comboF3P2","b_age_comboF3P3","b_age_comboF3P4",
#                           "b_age_comboF4P1","b_age_comboF4P2","b_age_comboF4P3","b_age_comboF4P4",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "sigma"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none')
# 
# ## stim type
# dove_lion <- draws_cut %>%
#   filter(parameter == 'b_stim_typel') %>%
#   rename(lion = draw) %>%
#   select(-parameter)
# dove_human <- draws_cut %>%
#   filter(parameter == 'b_stim_typeh') %>%
#   rename(human = draw) %>%
#   select(-parameter)
# 
# par(mfrow = c(2,1))
# plot(density(dove_lion$lion), main = 'dove vs lion') ; abline(v = 0, lty = 2)
# plot(density(dove_human$human), main = 'dove vs human') ; abline(v = 0, lty = 2)
# par(mfrow = c(1,1))
# 
# ## age categories
# draws %>%
#   filter(parameter %in% c("b_age_comboF1P1","b_age_comboF1P2",
#                           "b_age_comboF1P3","b_age_comboF1P4",
#                           "b_age_comboF2P1","b_age_comboF2P2",
#                           "b_age_comboF2P3","b_age_comboF2P4",
#                           "b_age_comboF3P1","b_age_comboF3P2",
#                           "b_age_comboF3P3","b_age_comboF3P4",
#                           "b_age_comboF4P1","b_age_comboF4P2",
#                           "b_age_comboF4P3","b_age_comboF4P4")) %>% 
#   ggplot(aes(x = draw, colour = as.factor(chain)))+
#   geom_density()+
#   facet_wrap(. ~ parameter)+
#   theme(legend.position = 'none')
# 
# ## difference before vs after
# draws %>%
#   filter(parameter %in% c("b_before_afterbefore",
#                           "b_stim_typeh:before_afterbefore",
#                           "b_stim_typel:before_afterbefore")) %>% 
#   ggplot(aes(x = draw, colour = as.factor(chain)))+
#   geom_density()+
#   facet_wrap(. ~ parameter)+
#   theme(legend.position = 'none')
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# save.image('nearest_neighbour/neighbour_proportion_binomial_run.RData')
# dev.off()
# 
# #### predict from model ####
# pdf('../outputs/neighbour_proportion/neighbour_proportion_binomial_modelpredictions.pdf')
# # load('nearest_neighbour/neighbour_proportion_binomial_run.RData')
# rm(list = ls()[! ls() %in% c('prop_fit','nn_no_na')]) ; gc()
#
# pred <- posterior_epred(object = prop_fit,
#                         newdata = nn_no_na)
# save.image('nearest_neighbour/neighbour_proportion_binomial_predictions.RData')
#
# ## convert to data frame
# nn_no_na$data_row <- 1:nrow(nn_no_na)
# pred <- as.data.frame(pred)
# colnames(pred) <- 1:nrow(nn_no_na)
# pred <- pred %>%
#   pivot_longer(cols = everything(),
#                names_to = 'data_row', values_to = 'epred') %>%
#   mutate(data_row = as.integer(data_row)) %>%
#   left_join(nn_no_na, by = 'data_row')
#
# save.image('nearest_neighbour/neighbour_proportion_binomial_predictions.RData')
#
# print(paste0('predictions calculated at ',Sys.time()))
# 
# #### plot predictions ####
load('nearest_neighbour/neighbour_proportion_binomial_predictions.RData')

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
## define labels for plotting
focal_age_labels <- c('focal: 10-15 yrs',
                      'focal: 16-20 yrs',
                      'focal: 21-25 yrs',
                      'focal: 26-35yrs')
names(focal_age_labels) <- c('F1','F2','F3','F4')
partner_age_labels <- c('target: 10-15 yrs',
                        'target: 16-20 yrs',
                        'target: 21-25 yrs',
                        'target: 26-35yrs')
names(partner_age_labels) <- c('P1','P2','P3','P4')
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

pred <- pred %>% 
  separate(age_combo, into = c('focal_age','partner_age'),
           remove = F, sep = 2)

pred[1:(nrow(nn_no_na)*1000),] %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
  ggplot()+
  geom_boxplot(aes(x = focal_age, y = epred,
                   fill = before_after)) +
  facet_grid(partner_age ~ stim_type,
             labeller = labeller(partner_age = partner_age_labels,
                                 stim_type = stim_labels))+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(fill = 'time relative to stimulus:',
       x = 'age category of focal elephant',
       y = 'predicted probability of being neighbours')+
  theme(legend.position = 'bottom')

pred[1:(nrow(nn_no_na)*1000),] %>%
  mutate(before_after = factor(before_after, levels = c('before','after'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = focal_age,
                          fill = partner_age,
                          colour = before_after,
                          alpha = before_after)
  )+
  labs(fill = 'focal age category',
       x = 'predicted probability of being neighbours',
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
ggsave(filename = 'prop_binomial_predicted_ridges.png',
       path = '../outputs/neighbour_proportion/',
       device = 'png', height = 1800, width = 1500, units = 'px')

## plot against raw data
pred[1:(nrow(nn_no_na)*2),] %>%
  group_by(focal, age_combo, stim_type, before_after) %>%
  summarise(epred = mean(epred),
            total_nn = length(which(action == 1)),
            total_not = length(which(action == 0)),
            total_view = length(action),
            prop_nn = total_nn / total_view) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h')),
         before_after = factor(before_after, levels = c('before','after'))) %>% 
  ggplot()+
  geom_point(aes(x = prop_nn,
                 y = epred,
                 colour = age_combo,
                 size = total_view),
             alpha = 0.5)+
  facet_grid(stim_type ~ before_after,
             labeller = labeller(stim_type = stim_labels))+
  scale_colour_viridis_d()+
  geom_abline(slope = 1, intercept = 0)+
  labs(colour = 'age category combination',
       size = 'number of seconds visible',
       x = 'proportion of time moving\n(NOT THE X VARIABLE FOR THE MODEL,\nBUT THE BEST OPTION FOR PLOTTING\nAGAINST PROBABILITY!)',
       y = 'predicted probability of being neighbours')

pred2 <- pred[1:(nrow(nn_no_na)*1000),] %>%
  # dplyr::select(-action_name,-time_since_stim,-after_stim) %>%
  pivot_longer(cols = c(epred, action),
               names_to = 'raw_pred', values_to = 'nn') %>%
  mutate(raw_pred = ifelse(raw_pred == 'epred', 'predicted', 'observed')) %>%
  mutate(before_after = factor(before_after, levels = c('before','after'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>% 
  separate(age_combo, into = c('focal_age','partner_age'),
           remove = F, sep = 2)
pred2 %>%
  ggplot()+
  geom_density_ridges(aes(x = nn,
                          y = focal_age,
                          fill = partner_age,
                          colour = raw_pred,
                          alpha = raw_pred)
  )+
  labs(fill = 'target age category',
       x = 'predicted probability of being neighbours',
       y = 'focal age category',
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
       path = '../outputs/neighbour_proportion/',
       device = 'png', height = 1800, width = 1500, units = 'px')

counts <- nn_no_na %>%
  group_by(focal, age_combo, stim_type, before_after, action) %>%
  summarise(n = length(focal))
counts$total <- NA
for(i in 1:nrow(counts)){
  counts$total[i] <- sum(counts$n[which(counts$age_combo == counts$age_combo[i] &
                                          counts$stim_type == counts$stim_type[i] &
                                          counts$before_after == counts$before_after[i])])
}
counts$prop <- counts$n / counts$total

props <- pred[1:(nrow(nn_no_na)*1000),] %>%
  group_by(focal, age_combo, stim_type, before_after, action) %>%
  summarise(pred_mean = mean(epred))

counts <- counts %>%
  left_join(props, by = c('focal','age_combo', 'stim_type', 'before_after', 'action')) %>% 
  separate(age_combo, into = c('focal_age','partner_age'),
           remove = F, sep = 2)

counts %>%
  filter(action == 1) %>%
  ggplot() +
  geom_point(aes(x = prop, y = pred_mean,
                 colour = before_after))+
  scale_colour_viridis_d()+
  facet_grid(age_combo ~ stim_type)+
  geom_abline(slope = 1, intercept = 0)

ggplot()+
  geom_col(data = counts,
           aes(x = age_combo,
               y = prop,
               fill = as.factor(action)))+
  geom_violin(data = pred[1:(nrow(nn_no_na)*1000),],
              aes(x = age_combo,
                  y = epred),
              fill = 'transparent')+
  scale_fill_viridis_d(begin = 1, end = 0.5)+
  facet_grid(before_after ~ stim_type,
             labeller = labeller(stim_type = stim_labels))+
  labs(x = 'age category combination',
       y = 'proportion of time spent as neighbours',
       fill = 'neighbours')

# library(see)
# counts <- counts %>%
#   mutate(f_age_num = ifelse(f_age_cat == '10-15', 1,
#                             ifelse(f_age_cat == '16-20', 2,
#                                    ifelse(f_age_cat == '21-25', 3, 4))))
# ggplot()+
#   geom_col(data = counts,
#            aes(x = as.factor(f_age_num),
#                y = prop,
#                fill = as.factor(action)))+
#   geom_violinhalf(data = pred[1:(nrow(nn_no_na)*40),],
#               aes(x = as.factor(f_age_num-0.1),
#                   y = epred),
#               fill = 'transparent',
#               position = 'dodge')+
#   scale_fill_viridis_d(begin = 1, end = 0.5)+
#   facet_grid(before_after ~ stim_type,
#              labeller = labeller(stim_type = stim_labels))+
#   labs(x = 'focal age category',
#        y = 'proportion of time spent moving',
#        fill = 'moving')

ggplot() +
  geom_boxplot(data = counts[counts$before_after == 'after',],
               aes(x = prop, y = 5,
                   fill = partner_age),
               width = 5, notch = F) +
  geom_density(data = pred[1:nrow(nn_no_na),],
               aes(x = epred), inherit.aes = FALSE) +
  facet_grid(stim_type ~ focal_age,
             labeller = labeller(stim_type = stim_labels)) +
  scale_fill_viridis_d()

ggplot() +
  geom_col(data = counts,
           aes(x = action,
               y = prop,
               fill = partner_age),
           width = 0.2, alpha = 0.6) +
  geom_density(data = pred[1:nrow(nn_no_na),],
               aes(x = epred), inherit.aes = FALSE) +
  facet_grid(stim_type ~ focal_age,
             labeller = labeller(stim_type = stim_labels)) +
  scale_fill_viridis_d()+
  scale_x_continuous(name = 'probability of being neighbours',
                     limits = c(-0.1,1.1),
                     breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  scale_y_continuous()

## reset plotting
dev.off()

#### calculate posterior contrasts from predictions ####
pdf('../outputs/neighbour_proportion/neighbour_proportion_binomial_modelcontrasts.pdf')
load('nearest_neighbour/neighbour_proportion_binomial_predictions.RData')

## stim type * before/after ####
nn_new <- nn_no_na %>%
  dplyr::select(age_combo, f_age_num, p_age_num, stim_type, before_after,
                focal_id, stim_id, playback_id) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(age_combo, before_after,focal_id, stim_id, playback_id))))

## redo predictions with different stimulus types: all doves
ctd_before <- nn_new %>%
  mutate(stim_type = 'ctd',
         before_after = 'before')
ctd_before_mtx <- posterior_epred(object = prop_fit, newdata = ctd_before)
colnames(ctd_before_mtx) <- ctd_before$unique_data_combo
ctd_before_mtx <- ctd_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

ctd_after <- nn_new %>%
  mutate(stim_type = 'ctd',
         after_after = 'after')
ctd_after_mtx <- posterior_epred(object = prop_fit, newdata = ctd_after)
colnames(ctd_after_mtx) <- ctd_after$unique_data_combo
ctd_after_mtx <- ctd_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all lions
lion_before <- nn_new %>%
  mutate(stim_type = 'l',
         before_after = 'before')
lion_before_mtx <- posterior_epred(object = prop_fit, newdata = lion_before)
colnames(lion_before_mtx) <- lion_before$unique_data_combo
lion_before_mtx <- lion_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

lion_after <- nn_new %>%
  mutate(stim_type = 'l',
         after_after = 'after')
lion_after_mtx <- posterior_epred(object = prop_fit, newdata = lion_after)
colnames(lion_after_mtx) <- lion_after$unique_data_combo
lion_after_mtx <- lion_after_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with different stimulus types: all humans
human_before <- nn_new %>%
  mutate(stim_type = 'h',
         before_after = 'before')
human_before_mtx <- posterior_epred(object = prop_fit, newdata = human_before)
colnames(human_before_mtx) <- human_before$unique_data_combo
human_before_mtx <- human_before_mtx[c(1:100,1001:1100,2001:2100,3001:3100),]

human_after <- nn_new %>%
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
contrasts <- nn_no_na %>%
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
save.image('nearest_neighbour/neighbour_proportion_binomial_stimuluscontrasts.RData')
# load('nearest_neighbour/neighbour_proportion_binomial_stimuluscontrasts.RData')

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

save.image('nearest_neighbour/neighbour_proportion_binomial_stimuluscontrasts.RData')
rm(ctd_vs_lion_before,ctd_vs_human_before,lion_vs_human_before,
   ctd_vs_lion_after,ctd_vs_human_after,lion_vs_human_after,
   ctd_before_vs_after,lion_before_vs_after,human_before_vs_after,
   ctd_before,ctd_before_mtx,ctd_after,ctd_after_mtx,
   lion_before,lion_before_mtx,lion_after,lion_after_mtx,
   human_before,human_before_mtx,human_after,human_after_mtx) ; gc()

## focal age ####
# load('nearest_neighbour/neighbour_proportion_binomial_stimuluscontrasts.RData')
nn_new <- nn_new %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(stim_type, before_after,
                                                         focal_id, stim_id, playback_id))))

## predict with original ages
age_org <- nn_new
age_mtx_org <- posterior_epred(object = prop_fit, newdata = age_org)
colnames(age_mtx_org) <- age_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),]

## redo predictions with altered age combinations
age_predict <- function(df, age_combination, model){
  df_new <- df %>%
    mutate(age_combo = age_combination)
  mtx <- posterior_epred(object = model, newdata = df_new)
  colnames(mtx) <- df_new$unique_data_combo
  mtx <- mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
  return(mtx)
}
mtx_11 <- age_predict(df = nn_new, age_combination = '1_1', model = prop_fit)
mtx_12 <- age_predict(df = nn_new, age_combination = '1_2', model = prop_fit)
mtx_13 <- age_predict(df = nn_new, age_combination = '1_3', model = prop_fit)
mtx_14 <- age_predict(df = nn_new, age_combination = '1_4', model = prop_fit)
mtx_21 <- age_predict(df = nn_new, age_combination = '2_1', model = prop_fit)
mtx_22 <- age_predict(df = nn_new, age_combination = '2_2', model = prop_fit)
mtx_23 <- age_predict(df = nn_new, age_combination = '2_3', model = prop_fit)
mtx_24 <- age_predict(df = nn_new, age_combination = '2_4', model = prop_fit)
mtx_31 <- age_predict(df = nn_new, age_combination = '3_1', model = prop_fit)
mtx_32 <- age_predict(df = nn_new, age_combination = '3_2', model = prop_fit)
mtx_33 <- age_predict(df = nn_new, age_combination = '3_3', model = prop_fit)
mtx_34 <- age_predict(df = nn_new, age_combination = '3_4', model = prop_fit)
mtx_41 <- age_predict(df = nn_new, age_combination = '4_1', model = prop_fit)
mtx_42 <- age_predict(df = nn_new, age_combination = '4_2', model = prop_fit)
mtx_43 <- age_predict(df = nn_new, age_combination = '4_3', model = prop_fit)
mtx_44 <- age_predict(df = nn_new, age_combination = '4_4', model = prop_fit)

save.image('nearest_neighbour/neighbour_proportion_agecontrasts.RData')

## calculate contrasts
contrast_11v12 <- mtx_11 - mtx_12
contrast_11v13 <- mtx_11 - mtx_13
contrast_11v14 <- mtx_11 - mtx_14
contrast_11v21 <- mtx_11 - mtx_21
contrast_11v31 <- mtx_11 - mtx_31
contrast_11v41 <- mtx_11 - mtx_41
contrast_12v13 <- mtx_12 - mtx_13
contrast_12v14 <- mtx_12 - mtx_14
contrast_12v22 <- mtx_12 - mtx_22
contrast_12v32 <- mtx_12 - mtx_32
contrast_12v42 <- mtx_12 - mtx_42
contrast_13v14 <- mtx_13 - mtx_14
contrast_13v23 <- mtx_13 - mtx_23
contrast_13v33 <- mtx_13 - mtx_33
contrast_13v43 <- mtx_13 - mtx_43
contrast_14v24 <- mtx_14 - mtx_24
contrast_14v34 <- mtx_14 - mtx_34
contrast_14v44 <- mtx_14 - mtx_44

contrast_21v22 <- mtx_21 - mtx_22
contrast_21v23 <- mtx_21 - mtx_23
contrast_21v24 <- mtx_21 - mtx_24
contrast_21v31 <- mtx_21 - mtx_31
contrast_21v41 <- mtx_21 - mtx_41
contrast_22v23 <- mtx_22 - mtx_23
contrast_22v24 <- mtx_22 - mtx_24
contrast_22v32 <- mtx_22 - mtx_32
contrast_22v42 <- mtx_22 - mtx_42
contrast_23v24 <- mtx_23 - mtx_24
contrast_23v33 <- mtx_23 - mtx_33
contrast_23v43 <- mtx_23 - mtx_43
contrast_24v34 <- mtx_24 - mtx_34
contrast_24v44 <- mtx_24 - mtx_44

contrast_31v32 <- mtx_31 - mtx_32
contrast_31v33 <- mtx_31 - mtx_33
contrast_31v34 <- mtx_31 - mtx_34
contrast_31v41 <- mtx_31 - mtx_41
contrast_32v33 <- mtx_32 - mtx_33
contrast_32v34 <- mtx_32 - mtx_34
contrast_32v42 <- mtx_32 - mtx_42
contrast_33v34 <- mtx_33 - mtx_34
contrast_33v43 <- mtx_33 - mtx_43
contrast_34v44 <- mtx_34 - mtx_44

contrast_41v42 <- mtx_41 - mtx_42
contrast_41v43 <- mtx_41 - mtx_43
contrast_41v44 <- mtx_41 - mtx_44
contrast_42v43 <- mtx_42 - mtx_43
contrast_42v44 <- mtx_42 - mtx_44
contrast_43v44 <- mtx_43 - mtx_44

## summarise contrasts
contrasts <- nn %>%
  select(-stim_type) %>%
  mutate(mu_11v12 = apply(contrast_11v12, 2, mean),
         sd_11v12 = apply(contrast_11v12, 2, sd),
         mu_11v13 = apply(contrast_11v13, 2, mean),
         sd_11v13 = apply(contrast_11v13, 2, sd),
         mu_11v14 = apply(contrast_11v14, 2, mean),
         sd_11v14 = apply(contrast_11v14, 2, sd),
         mu_11v21 = apply(contrast_11v21, 2, mean),
         sd_11v21 = apply(contrast_11v21, 2, sd),
         mu_11v31 = apply(contrast_11v31, 2, mean),
         sd_11v31 = apply(contrast_11v31, 2, sd),
         mu_11v41 = apply(contrast_11v41, 2, mean),
         sd_11v41 = apply(contrast_11v41, 2, sd),
         
         mu_12v13 = apply(contrast_12v13, 2, mean),
         sd_12v13 = apply(contrast_12v13, 2, sd),
         mu_12v14 = apply(contrast_12v14, 2, mean),
         sd_12v14 = apply(contrast_12v14, 2, sd),
         mu_12v22 = apply(contrast_12v22, 2, mean),
         sd_12v22 = apply(contrast_12v22, 2, sd),
         mu_12v32 = apply(contrast_12v32, 2, mean),
         sd_12v32 = apply(contrast_12v32, 2, sd),
         mu_12v42 = apply(contrast_12v42, 2, mean),
         sd_12v42 = apply(contrast_12v42, 2, sd),
         
         mu_13v14 = apply(contrast_13v14, 2, mean),
         sd_13v14 = apply(contrast_13v14, 2, sd),
         mu_13v23 = apply(contrast_13v23, 2, mean),
         sd_13v23 = apply(contrast_13v23, 2, sd),
         mu_13v33 = apply(contrast_13v33, 2, mean),
         sd_13v33 = apply(contrast_13v33, 2, sd),
         mu_13v43 = apply(contrast_13v43, 2, mean),
         sd_13v43 = apply(contrast_13v43, 2, sd),
         
         mu_14v24 = apply(contrast_14v24, 2, mean),
         sd_14v24 = apply(contrast_14v24, 2, sd),
         mu_14v34 = apply(contrast_14v34, 2, mean),
         sd_14v34 = apply(contrast_14v34, 2, sd),
         mu_14v44 = apply(contrast_14v44, 2, mean),
         sd_14v44 = apply(contrast_14v44, 2, sd),
         
         mu_21v22 = apply(contrast_21v22, 2, mean),
         sd_21v22 = apply(contrast_21v22, 2, sd),
         mu_21v23 = apply(contrast_21v23, 2, mean),
         sd_21v23 = apply(contrast_21v23, 2, sd),
         mu_21v24 = apply(contrast_21v24, 2, mean),
         sd_21v24 = apply(contrast_21v24, 2, sd),
         mu_21v31 = apply(contrast_21v31, 2, mean),
         sd_21v31 = apply(contrast_21v31, 2, sd),
         mu_21v41 = apply(contrast_21v41, 2, mean),
         sd_21v41 = apply(contrast_21v41, 2, sd),
         
         mu_22v23 = apply(contrast_22v23, 2, mean),
         sd_22v23 = apply(contrast_22v23, 2, sd),
         mu_22v24 = apply(contrast_22v24, 2, mean),
         sd_22v24 = apply(contrast_22v24, 2, sd),
         mu_22v32 = apply(contrast_22v32, 2, mean),
         sd_22v32 = apply(contrast_22v32, 2, sd),
         mu_22v42 = apply(contrast_22v42, 2, mean),
         sd_22v42 = apply(contrast_22v42, 2, sd),
         
         mu_23v24 = apply(contrast_23v24, 2, mean),
         sd_23v24 = apply(contrast_23v24, 2, sd),
         mu_23v33 = apply(contrast_23v33, 2, mean),
         sd_23v33 = apply(contrast_23v33, 2, sd),
         mu_23v43 = apply(contrast_23v43, 2, mean),
         sd_23v43 = apply(contrast_23v43, 2, sd),
         
         mu_24v34 = apply(contrast_24v34, 2, mean),
         sd_24v34 = apply(contrast_24v34, 2, sd),
         mu_24v44 = apply(contrast_24v44, 2, mean),
         sd_24v44 = apply(contrast_24v44, 2, sd),
         
         mu_31v32 = apply(contrast_31v32, 2, mean),
         sd_31v32 = apply(contrast_31v32, 2, sd),
         mu_31v33 = apply(contrast_31v33, 2, mean),
         sd_31v33 = apply(contrast_31v33, 2, sd),
         mu_31v34 = apply(contrast_31v34, 2, mean),
         sd_31v34 = apply(contrast_31v34, 2, sd),
         mu_31v41 = apply(contrast_31v41, 2, mean),
         sd_31v41 = apply(contrast_31v41, 2, sd),
         
         mu_32v33 = apply(contrast_32v33, 2, mean),
         sd_32v33 = apply(contrast_32v33, 2, sd),
         mu_32v34 = apply(contrast_32v34, 2, mean),
         sd_32v34 = apply(contrast_32v34, 2, sd),
         mu_32v42 = apply(contrast_32v42, 2, mean),
         sd_32v42 = apply(contrast_32v42, 2, sd),
         
         mu_33v34 = apply(contrast_33v34, 2, mean),
         sd_33v34 = apply(contrast_33v34, 2, sd),
         mu_33v43 = apply(contrast_33v43, 2, mean),
         sd_33v43 = apply(contrast_33v43, 2, sd),
         
         mu_34v44 = apply(contrast_34v44, 2, mean),
         sd_34v44 = apply(contrast_34v44, 2, sd),
         
         mu_41v42 = apply(contrast_41v42, 2, mean),
         sd_41v42 = apply(contrast_41v42, 2, sd),
         mu_41v43 = apply(contrast_41v43, 2, mean),
         sd_41v43 = apply(contrast_41v43, 2, sd),
         mu_41v44 = apply(contrast_41v44, 2, mean),
         sd_41v44 = apply(contrast_41v44, 2, sd),
         
         mu_42v43 = apply(contrast_42v43, 2, mean),
         sd_42v43 = apply(contrast_42v43, 2, sd),
         mu_42v44 = apply(contrast_42v44, 2, mean),
         sd_42v44 = apply(contrast_42v44, 2, sd),
         
         mu_43v44 = apply(contrast_43v44, 2, mean),
         sd_43v44 = apply(contrast_43v44, 2, sd)
  )

contrasts_long <- contrasts %>%
  pivot_longer(cols = c(mu_11v12,mu_11v13,mu_11v14,mu_11v21,mu_11v31,mu_11v41,
                        mu_12v13,mu_12v14,mu_12v22,mu_12v32,mu_12v42,
                        mu_13v14,mu_13v23,mu_13v33,mu_13v43,
                        mu_14v24,mu_14v34,mu_14v44,
                        mu_21v22,mu_21v23,mu_21v24,mu_21v31,mu_21v41,
                        mu_22v23,mu_22v24,mu_22v32,mu_22v42,
                        mu_23v24,mu_23v33,mu_23v43,
                        mu_24v34,mu_24v44,
                        mu_31v32,mu_31v33,mu_31v34,mu_31v41,
                        mu_32v33,mu_32v34,mu_32v42,
                        mu_33v34,mu_33v43,
                        mu_34v44,
                        mu_41v42,mu_41v43,mu_41v44,
                        mu_42v43,mu_42v44,
                        mu_43v44),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('mu','combo1','v','combo2'),
           sep = c(3,5,6), remove = T) %>%
  select(-sd_11v12,-sd_11v13,-sd_11v14,-sd_11v21,-sd_11v31,-sd_11v41,
         -sd_12v13,-sd_12v14,-sd_12v22,-sd_12v32,-sd_12v42,
         -sd_13v14,-sd_13v23,-sd_13v33,-sd_13v43,
         -sd_14v24,-sd_14v34,-sd_14v44,
         -sd_21v22,-sd_21v23,-sd_21v24,-sd_21v31,-sd_21v41,
         -sd_22v23,-sd_22v24,-sd_22v32,-sd_22v42,
         -sd_23v24,-sd_23v33,-sd_23v43,
         -sd_24v34,-sd_24v44,
         -sd_31v32,-sd_31v33,-sd_31v34,-sd_31v41,
         -sd_32v33,-sd_32v34,-sd_32v42,
         -sd_33v34,-sd_33v43,
         -sd_34v44,
         -sd_41v42,-sd_41v43,-sd_41v44,
         -sd_42v43,-sd_42v44,
         -sd_43v44,
         -mu)

## save contrasts
save.image('nearest_neighbour/neighbour_proportion_agecontrasts.RData')

## produce values for reporting
print('11 v 12') ; median(contrast_11v12); mean(contrast_11v12); sd(contrast_11v12)
median(contrast_11v13); mean(contrast_11v13); sd(contrast_11v13)
median(contrast_11v14); mean(contrast_11v14); sd(contrast_11v14)
print('11 v 21') ; median(contrast_11v21); mean(contrast_11v21); sd(contrast_11v21)
median(contrast_11v31); mean(contrast_11v31); sd(contrast_11v31)
median(contrast_11v41); mean(contrast_11v41); sd(contrast_11v41)
print('12 v 13') ; median(contrast_12v13); mean(contrast_12v13); sd(contrast_12v13)
median(contrast_12v14); mean(contrast_12v14); sd(contrast_12v14)
print('12 v 22') ; median(contrast_12v22); mean(contrast_12v22); sd(contrast_12v22)
median(contrast_12v32); mean(contrast_12v32); sd(contrast_12v32)
median(contrast_12v42); mean(contrast_12v42); sd(contrast_12v42)
print('13 v 14') ; median(contrast_13v14); mean(contrast_13v14); sd(contrast_13v14)
print('13 v 23') ; median(contrast_13v23); mean(contrast_13v23); sd(contrast_13v23)
median(contrast_13v33); mean(contrast_13v33); sd(contrast_13v33)
median(contrast_13v43); mean(contrast_13v43); sd(contrast_13v43)
print('14 v 24') ; median(contrast_14v24); mean(contrast_14v24); sd(contrast_14v24)
median(contrast_14v34); mean(contrast_14v34); sd(contrast_14v34)
median(contrast_14v44); mean(contrast_14v44); sd(contrast_14v44)

print('21 v 22') ; median(contrast_21v22); mean(contrast_21v22); sd(contrast_21v22)
median(contrast_21v23); mean(contrast_21v23); sd(contrast_21v23)
median(contrast_21v24); mean(contrast_21v24); sd(contrast_21v24)
print('21 v 31') ; median(contrast_21v31); mean(contrast_21v31); sd(contrast_21v31)
median(contrast_21v41); mean(contrast_21v41); sd(contrast_21v41)
print('22 v 23') ; median(contrast_22v23); mean(contrast_22v23); sd(contrast_22v23)
median(contrast_22v24); mean(contrast_22v24); sd(contrast_22v24)
print('22 v 32') ; median(contrast_22v32); mean(contrast_22v32); sd(contrast_22v32)
median(contrast_22v42); mean(contrast_22v42); sd(contrast_22v42)
print('23 v 24') ; median(contrast_23v24); mean(contrast_23v24); sd(contrast_23v24)
print('23 v 33') ; median(contrast_23v33); mean(contrast_23v33); sd(contrast_23v33)
median(contrast_23v43); mean(contrast_23v43); sd(contrast_23v43)
print('24 v 34') ; median(contrast_24v34); mean(contrast_24v34); sd(contrast_24v34)
median(contrast_24v44); mean(contrast_24v44); sd(contrast_24v44)

print('31 v 32') ; median(contrast_31v32); mean(contrast_31v32); sd(contrast_31v32)
median(contrast_31v33); mean(contrast_31v33); sd(contrast_31v33)
median(contrast_31v34); mean(contrast_31v34); sd(contrast_31v34)
print('31 v 41') ; median(contrast_31v41); mean(contrast_31v41); sd(contrast_31v41)
print('32 v 33') ; median(contrast_32v33); mean(contrast_32v33); sd(contrast_32v33)
median(contrast_32v34); mean(contrast_32v34); sd(contrast_32v34)
print('32 v 42') ; median(contrast_32v42); mean(contrast_32v42); sd(contrast_32v42)
print('33 v 34') ; median(contrast_33v34); mean(contrast_33v34); sd(contrast_33v34)
print('33 v 43') ; median(contrast_33v43); mean(contrast_33v43); sd(contrast_33v43)
print('34 v 44') ; median(contrast_34v44); mean(contrast_34v44); sd(contrast_34v44)

print('41 v 42') ; median(contrast_41v42); mean(contrast_41v42); sd(contrast_41v42)
median(contrast_41v43); mean(contrast_41v43); sd(contrast_41v43)
median(contrast_41v44); mean(contrast_41v44); sd(contrast_41v44)
print('42 v 43') ; median(contrast_42v43); mean(contrast_42v43); sd(contrast_42v43)
median(contrast_42v44); mean(contrast_42v44); sd(contrast_42v44)
print('43 v 44') ; median(contrast_43v44); mean(contrast_43v44); sd(contrast_43v44)

## plot contrasts
(focal_plot <- contrasts_long %>%
    separate(combo1, remove = F, into = c('f1', 't1'), sep = 1) %>% 
    separate(combo2, remove = F, into = c('f2', 't2'), sep = 1) %>% 
    filter(t1 == t2) %>% 
    mutate(contrast = paste0(combo1, ' -> ', combo2),
           cats_diff = as.factor(as.numeric(f2) - as.numeric(f1))) %>%
    mutate(contrast = factor(contrast,
                             levels = c('11 -> 21','13 -> 23','21 -> 31','24 -> 34',
                                        '11 -> 31','13 -> 33','21 -> 41','24 -> 44',
                                        '11 -> 41','13 -> 43','22 -> 32','31 -> 41',
                                        '12 -> 22','14 -> 24','22 -> 42','32 -> 42',
                                        '12 -> 32','14 -> 34','23 -> 33','33 -> 43',
                                        '12 -> 42','14 -> 44','23 -> 43','34 -> 44'
                             ))) %>%
    ggplot()+
    geom_density(aes(x = difference, colour = cats_diff, fill = cats_diff),
                 alpha = 0.4)+
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    facet_wrap(. ~ contrast, scales = 'free_y', ncol = 4)+
    scale_x_continuous(limits = c(-0.02,0.02))+
    labs(colour = 'difference in\nage category',
         fill = 'difference in\nage category',
         title = 'changing focal age',
         x = 'contrast'))
ggsave(plot = focal_plot, device = 'png',
       filename = 'nbm_focalage_contrasts.png',
       path = '../outputs/neighbour_proportion/',
       width = 2200, height = 2400, units = 'px')
ggsave(plot = focal_plot, device = 'svg',
       filename = 'nbm_focalage_contrasts.svg',
       path = '../outputs/neighbour_proportion/',
       width = 2200, height = 2400, units = 'px')
(partner_plot <- contrasts_long %>%
    separate(combo1, remove = F, into = c('f1', 't1'), sep = 1) %>% 
    separate(combo2, remove = F, into = c('f2', 't2'), sep = 1) %>% 
    filter(f1 == f2) %>% 
    mutate(contrast = paste0(combo1, ' -> ', combo2),
           cats_diff = as.factor(as.numeric(t2) - as.numeric(t1))) %>%
    mutate(contrast = factor(contrast,
                             levels = c('11 -> 12','21 -> 22','31 -> 32','41 -> 42',
                                        '11 -> 13','21 -> 23','31 -> 33','41 -> 43',
                                        '11 -> 14','21 -> 24','31 -> 34','41 -> 44',
                                        '12 -> 13','22 -> 23','32 -> 33','42 -> 43',
                                        '12 -> 14','22 -> 24','32 -> 34','42 -> 44',
                                        '13 -> 14','23 -> 24','33 -> 34','43 -> 44'))) %>% 
    ggplot()+
    geom_density(aes(x = difference, colour = cats_diff, fill = cats_diff),
                 alpha = 0.4)+
    scale_colour_viridis_d()+
    scale_fill_viridis_d()+
    scale_x_continuous(limits = c(-0.0125,0.025))+
    facet_wrap(contrast ~ ., scales = 'free_y', ncol = 4)+
    labs(colour = 'difference in\nage category',
         fill = 'difference in\nage category',
         title = 'changing partner age',
         x = 'contrast'))
ggsave(plot = partner_plot, device = 'png',
       filename = 'nbm_partnerage_contrasts.png',
       path = '../outputs/neighbour_proportion/',
       width = 2200, height = 2400, units = 'px')
ggsave(plot = partner_plot, device = 'svg',
       filename = 'nbm_partnerage_contrasts.svg',
       path = '../outputs/neighbour_proportion/',
       width = 2200, height = 2400, units = 'px')
(focal_plot + partner_plot)+
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'a')
ggsave(plot = last_plot(), device = 'png',
       filename = 'nbm_age_contrasts.png',
       path = '../outputs/neighbour_proportion/',
       width = 4400, height = 2400, units = 'px')
ggsave(plot = last_plot(), device = 'svg',
       filename = 'nbm_age_contrasts.svg',
       path = '../outputs/neighbour_proportion/',
       width = 4400, height = 2400, units = 'px')

### save and clean up
save.image('nearest_neighbour/neighbour_proportion_binomial_agecontrasts.RData')
dev.off()
