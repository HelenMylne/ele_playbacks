#### information        ####
# looking for a better way to display outputs than purely contrast density plots

#### set up             ####
#library(tidyverse); library(LaplacesDemon) ; library(patchwork); library(ggridges)
library(tidyverse, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')
library(ggridges, lib.loc = '../../packages/')

theme_set(theme_bw())

# #### movement binomial -- probability of changing behaviour ####
# pdf('../outputs/movement_binomial_model/niceplots_movementbinomial.pdf')
# load('movement_direction/movement_binomial_agecontrasts.RData')
# rm(ctd_vs_human, ctd_vs_lion, lion_vs_human, contrasts, contrasts_long) ; gc()
# rm(age_move_alt, age1_vs_age2, age2_vs_age3, age3_vs_age4, age1_vs_age4) ; gc()
# 
# ## plot predictions
# pred <- pred %>%
#   mutate(draw_id = rep(1:4000, each = nrow(age_move_org)),
#          stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
#                                  ifelse(stim_type == 'l','lion','human'))) %>%
#   mutate(stim_type_long = factor(stim_type_long,
#                                  levels = c('dove (control)','lion','human')))
# 
# pred %>%
#   mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving', 'no', 'yes')) %>%
#   mutate(move_tminus1 = factor(move_tminus1, levels = c('yes','no'))) %>%
#   ggplot()+
#   geom_boxplot(aes(x = f_age_cat,
#                    # fill = as.factor(move_index), # successfully predicts actual data
#                    fill = move_tminus1,
#                    y = epred))+
#   labs(x = 'focal age category',
#        y = 'predicted probability of moving',
#        fill = 'moving in previous second')+
#   facet_wrap(. ~ stim_type_long)+
#   scale_fill_viridis_d()+
#   theme(legend.position = 'bottom')
# 
# pred %>%
#   mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving',
#                                'not moving at t-1','moving at t-1')) %>%
#   mutate(move_tminus1 = factor(move_tminus1, levels = c('moving at t-1',
#                                                         'not moving at t-1'))) %>%
#   ggplot()+
#   geom_density(aes(x = epred,
#                    fill = f_age_cat),
#                alpha = 0.4)+
#   labs(fill = 'focal age category',
#        x = 'predicted probability of moving',
#        y = 'probability density')+
#   facet_grid(move_tminus1 ~ stim_type_long,
#              scales = 'free')+
#   scale_fill_viridis_d()+
#   theme(legend.position = 'bottom')
# 
# pred %>%
#   mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving',
#                                'not moving at t-1','moving at t-1')) %>%
#   mutate(move_tminus1 = factor(move_tminus1, levels = c('moving at t-1',
#                                                         'not moving at t-1'))) %>%
#   ggplot()+
#   geom_density_ridges(aes(x = epred,
#                           y = f_age_cat,
#                           fill = f_age_cat),
#                       alpha = 0.6)+
#   labs(fill = 'focal age category',
#        x = 'predicted probability of moving',
#        y = 'probability density')+
#   facet_grid(stim_type_long ~ move_tminus1,
#              scales = 'free_x')+
#   scale_fill_viridis_d()+
#   theme(legend.position = 'bottom')
# ggsave(filename = 'mbm_predicted_ridges.png',
#        path = '../outputs/movement_binomial_model/',
#        device = 'png', height = 1800, width = 1500, units = 'px')
# 
# rm(pred) ; gc()
# 
# ## clean up data
# age_contrast_long <- age_contrast %>%
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
#          contrast = ifelse(f_age_num_org == 4, contrast*(-1), contrast)) %>%
#   relocate(f_age_num_alt, .after = (f_age_num_org)) %>%
#   relocate(f_age_cat_org, .after = (f_age_num_alt)) %>%
#   relocate(f_age_cat_alt, .after = (f_age_cat_org)) %>%
#   mutate(comparison = ifelse(f_age_num_org == 4,
#                              paste0(f_age_cat_alt, ' to ', f_age_cat_org),
#                              paste0(f_age_cat_org, ' to ', f_age_cat_alt)))
# save.image('movement_direction/movement_binomial_ageplotting.RData')
# 
# ## plot
# age_contrast_long %>%
#   mutate(prev_move = ifelse(move_tminus1_num == 0, 'not moving at t-1', 'moving at t-1'),
#          stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                             ifelse(stim_type == 'l', 'lion',
#                                    'human'))) %>%
#   ggplot()+
#   geom_violin(aes(x = comparison,
#                   y = contrast,
#                   fill = stim_type),
#               position = position_dodge(0.5))+
#   geom_hline(yintercept = 0, lty = 2)+
#   scale_fill_viridis_d()+
#   facet_wrap(. ~ prev_move, scales = 'free_y')+
#   labs(fill = 'moving in previous second')+
#   theme(legend.position = 'bottom',
#         axis.text.x = element_text(angle = 90, vjust = 0.5))
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'mbm_violin_contrasts.png',
#        path = '../outputs/movement_binomial_model/',
#        height = 1600, width = 1600, unit = 'px')
# 
# print('movement binomial complete')
# dev.off()
# 
#### movement binomial -- probability of performing behaviour ####
pdf('../outputs/movement_binomial_model/niceplots_movementbinomial_noprev.pdf')
load('movement_direction/binomial_noprev/movement_noprev_agecontrasts.RData')
rm(ctd_vs_human, ctd_vs_lion, lion_vs_human, contrasts, contrasts_long) ; gc()
rm(age_move_alt, age1_vs_age2, age2_vs_age3, age3_vs_age4, age1_vs_age4) ; gc()

## plot predictions
pred <- pred %>%
  mutate(draw_id = rep(1:4000, each = nrow(age_move_org)),
         stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
                                 ifelse(stim_type == 'l','lion','human'))) %>%
  mutate(stim_type_long = factor(stim_type_long,
                                 levels = c('dove (control)','lion','human')),
         bda = factor(bda, levels = c('before','during','after')))

pred %>%
  ggplot()+
  geom_boxplot(aes(x = f_age_cat,
                   # fill = as.factor(move_index), # successfully predicts actual data
                   fill = stim_type_long,
                   y = epred))+
  labs(x = 'focal age category',
       y = 'predicted probability of moving',
       fill = 'moving in previous second')+
  facet_wrap(. ~ bda)+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')

pred %>%
  ggplot()+
  geom_density(aes(x = epred,
                   fill = f_age_cat),
               alpha = 0.4)+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density')+
  facet_grid(bda ~ stim_type_long,
             scales = 'free')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')

pred %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          fill = f_age_cat),
                      alpha = 0.6)+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density')+
  facet_grid(bda ~ stim_type_long,
             scales = 'free_x')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
ggsave(filename = 'mbm_noprev_predicted_ridges.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 1800, width = 1500, units = 'px')

rm(pred) ; gc()

## clean up data
age_contrast_long <- age_contrast %>%
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
         contrast = ifelse(f_age_num_org == 4, contrast*(-1), contrast)) %>%
  relocate(f_age_num_alt, .after = (f_age_num_org)) %>%
  relocate(f_age_cat_org, .after = (f_age_num_alt)) %>%
  relocate(f_age_cat_alt, .after = (f_age_cat_org)) %>%
  mutate(comparison = ifelse(f_age_num_org == 4,
                             paste0(f_age_cat_alt, ' to ', f_age_cat_org),
                             paste0(f_age_cat_org, ' to ', f_age_cat_alt)))
save.image('movement_direction/movement_binomial_ageplotting.RData')

## plot
age_contrast_long %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion',
                                   'human'))) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = contrast,
                  fill = comparison),
              position = position_dodge(0.5))+
  geom_hline(yintercept = 0, lty = 2)+
  scale_fill_viridis_d()+
  facet_wrap(stim_type ~ bda, scales = 'free_y')+
  labs(fill = 'moving in previous second')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'mbm_violin_contrasts.png',
       path = '../outputs/movement_binomial_model/',
       height = 1600, width = 1600, unit = 'px')

print('movement binomial complete')
dev.off()

# #### movement ordinal 1 ####
# pdf('../outputs/movement_ordinal_model_1/niceplots_movementordinal1.pdf')
# rm(list = ls()) ; gc() ; load('movement_direction/movement_ordinal_model1_agecontrasts.RData')
# 
# ## plot predictions
# age_move_org$data_id <- 1:nrow(age_move_org)
# extract_predictions <- function(array, slice, df){
#   matrix <- array[,,slice]
#   colnames(matrix) <- 1:nrow(df)
#   pred <- matrix %>%
#     as.data.frame() %>%
#     pivot_longer(cols = everything(), names_to = 'data_id', values_to = 'epred') %>%
#     mutate(data_id = as.integer(data_id)) %>%
#     left_join(df, by = 'data_id')%>%
#     mutate(stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
#                                    ifelse(stim_type == 'l','lion','human'))) %>%
#     mutate(stim_type_long = factor(stim_type_long,
#                                    levels = c('dove (control)','lion','human'))) %>%
#     mutate(predicted_direction = slice)
#   return(pred)
# }
# pred_ad <- extract_predictions(array = age_mtx_org, slice = 1, df = age_move_org)
# pred_aa <- extract_predictions(array = age_mtx_org, slice = 2, df = age_move_org)
# pred_nt <- extract_predictions(array = age_mtx_org, slice = 3, df = age_move_org)
# pred_ta <- extract_predictions(array = age_mtx_org, slice = 4, df = age_move_org)
# pred_td <- extract_predictions(array = age_mtx_org, slice = 5, df = age_move_org)
# 
# boxplot_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                        ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                               ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                      'approach directly'))))
# 
#   pred %>%
#     mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
#                                  ifelse(move_tminus1_num == 2, 'move away at an angle',
#                                         ifelse(move_tminus1_num == 3, 'move neither towards or away',
#                                                ifelse(move_tminus1_num == 4,
#                                                       'approach at an angle',
#                                                       'approach directly')))),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))) %>%
#     ggplot()+
#     geom_boxplot(aes(x = f_age_cat,
#                      # fill = as.factor(move_index), # successfully predicts actual data
#                      fill = move_tminus1,
#                      y = epred))+
#     labs(x = 'focal age category',
#          y = 'predicted probability of moving direction',
#          fill = 'movement in previous second',
#          title = predicted_direction)+
#     facet_grid(move_tminus1 ~ stim_type_long)+ # looks a bit rubbish, but freeing y then gives crazy scales. I ideally want one where I can limit the y to 0-0.2 for all but move_tminus1 = prediction_direction which can then go 0.8-1
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# 
# }
# (box_ad <- boxplot_pred(pred_ad))
# (box_aa <- boxplot_pred(pred_aa))
# (box_nt <- boxplot_pred(pred_nt))
# (box_ta <- boxplot_pred(pred_ta))
# (box_td <- boxplot_pred(pred_td))
# 
# density_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                        ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                               ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                      'approach directly'))))
# 
#   pred %>%
#     mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
#                                  ifelse(move_tminus1_num == 2, 'move away at an angle',
#                                         ifelse(move_tminus1_num == 3, 'move neither towards or away',
#                                                ifelse(move_tminus1_num == 4,
#                                                       'approach at an angle',
#                                                       'approach directly')))),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))) %>%
#     ggplot()+
#     geom_density(aes(x = epred,
#                      fill = f_age_cat),
#                  alpha = 0.4)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability of moving',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(move_tminus1 ~ stim_type_long,
#                scales = 'free')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom')
# 
# }
# (dens_ad <- density_pred(pred_ad))
# (dens_aa <- density_pred(pred_aa))
# (dens_nt <- density_pred(pred_nt))
# (dens_ta <- density_pred(pred_ta))
# (dens_td <- density_pred(pred_td))
# 
# ridge_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                        ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                               ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                      'approach directly'))))
# 
#   pred %>%
#     mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
#                                  ifelse(move_tminus1_num == 2, 'move away at an angle',
#                                         ifelse(move_tminus1_num == 3, 'neither towards or away',
#                                                ifelse(move_tminus1_num == 4,
#                                                       'approach at an angle',
#                                                       'approach directly')))),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))) %>%
#     ggplot()+
#     geom_density_ridges(aes(x = epred,
#                             y = f_age_cat,
#                             fill = f_age_cat),
#                         alpha = 0.6)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability of moving direction',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(stim_type_long ~ move_tminus1,
#                scales = 'free_x')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# }
# (ridge_ad <- ridge_pred(pred_ad))
# (ridge_aa <- ridge_pred(pred_aa))
# (ridge_nt <- ridge_pred(pred_nt))
# (ridge_ta <- ridge_pred(pred_ta))
# (ridge_td <- ridge_pred(pred_td))
# 
# ggsave(filename = 'mom1_predicted_ridges_directlyaway.png',
#        path = '../outputs/movement_ordinal_model_1/',
#        plot = ridge_ad,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# ggsave(filename = 'mom1_predicted_ridges_angleaway.png',
#        path = '../outputs/movement_ordinal_model_1/',
#        plot = ridge_aa,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# ggsave(filename = 'mom1_predicted_ridges_neither.png',
#        path = '../outputs/movement_ordinal_model_1/',
#        plot = ridge_nt,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# ggsave(filename = 'mom1_predicted_ridges_angleapproach.png',
#        path = '../outputs/movement_ordinal_model_1/',
#        plot = ridge_ta,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# ggsave(filename = 'mom1_predicted_ridges_directlyapproach.png',
#        path = '../outputs/movement_ordinal_model_1/',
#        plot = ridge_td,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# 
# ## plot
# contrast_plot <- function(contrasts, direction, free_y = T){
#   contrasts %>%
#     filter(move_pred == direction) %>%
#     rename(f_age_cat_org = f_age_cat,
#            f_age_num_org = f_age_num) %>%
#     mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
#                                   f_age_num_org + 1)) %>%
#     mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
#                                   ifelse(f_age_num_alt == 2, '16-20 yrs',
#                                          ifelse(f_age_num_alt == 3, '21-25 yrs',
#                                                 '26-35 yrs')))) %>%
#     mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
#            stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                               ifelse(stim_type == 'l', 'lion', 'human')),
#            move_tminus1 = ifelse(move_tminus1 == 'move away directly',
#                                  'move away directly',
#                                  ifelse(move_tminus1 == 'move away at an angle',
#                                         'move away at an angle',
#                                         ifelse(move_tminus1 == 'move directly with',
#                                                'neither towards or away',
#                                                ifelse(move_tminus1 == 'approach at an angle',
#                                                       'approach at an angle',
#                                                       'approach directly'))))) %>%
#     mutate(move_tminus1 = factor(move_tminus1,
#                                  levels = c('move away directly',
#                                             'move away at an angle',
#                                             'neither towards or away',
#                                             'approach at an angle',
#                                             'approach directly'))) %>%
#     ggplot()+
#     geom_hline(yintercept = 0, lty = 2)+
#     geom_violin(aes(x = comparison,
#                     y = difference,
#                     fill = comparison),
#                 position = position_dodge(0.5))+
#     scale_fill_viridis_d()+
#     facet_grid(move_tminus1 ~ stim_type,
#                scales = ifelse(free_y == T, 'free_y', 'fixed')
#                )+
#     labs(fill = 'comparison',
#          title = ifelse(direction == 'awayangle',
#                             'away at an angle',
#                             ifelse(direction == 'awaydirect',
#                                    'away directly',
#                                    ifelse(direction == 'neither',
#                                           'neither towards or away',
#                                           ifelse(direction == 'twdsangle',
#                                                  'towards at an angle',
#                                                  'towards directly')))))+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90, vjust = 0.5))
# }
# for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
#   contrast_plot(contrasts_long, plot, free_y = F)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom1_contrasts_fixedscales_',plot,'.png'),
#          path = '../outputs/movement_ordinal_model_1/',
#          height = 3000, width = 2100, unit = 'px')
# }
# for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
#   contrast_plot(contrasts_long, plot, free_y = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom1_contrasts_freescales_',plot,'.png'),
#          path = '../outputs/movement_ordinal_model_1/',
#          height = 3000, width = 2100, unit = 'px')
# }
# 
# print('movement ordinal 1 complete')
# dev.off()
# 
# #### movement ordinal 2 -- probability of changing behaviour ####
# pdf('../outputs/looking_ordinal_model_2bda/niceplots_movementordinal2.pdf')
# rm(list = ls()) ; gc() ; load('movement_direction/moving_ordinal_2bda_agecontrasts.RData')
# rm(age1v2_aa,age1v2_ad,age1v2_n,age1v2_ta,age1v2_td,
#    age1v4_aa,age1v4_ad,age1v4_n,age1v4_ta,age1v4_td,
#    age2v3_aa,age2v3_ad,age2v3_n,age2v3_ta,age2v3_td,
#    age3v4_aa,age3v4_ad,age3v4_n,age3v4_ta,age3v4_td) ; gc()
# rm(age_move_alt,age_new,age_pred,alt_vs_org_awayangle,alt_vs_org_awaydirect,alt_vs_org_neither,alt_vs_org_twdsangle,alt_vs_org_twdsdirect,behav,contrasts) ; gc()
# save.image('movement_direction/moving_ordinal_2bda_ageplotting.RData')
# #load('movement_direction/moving_ordinal_2bda_ageplotting.RData')
# 
# ## plot predictions
# age_move_org$data_id <- 1:nrow(age_move_org)
# extract_predictions <- function(array, slice, df){
#   matrix <- array[,,slice]
#   colnames(matrix) <- 1:nrow(df)
#   pred <- matrix %>%
#     as.data.frame() %>%
#     pivot_longer(cols = everything(), names_to = 'data_id', values_to = 'epred') %>%
#     mutate(data_id = as.integer(data_id)) %>%
#     left_join(df, by = 'data_id')%>%
#     mutate(stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
#                                    ifelse(stim_type == 'l','lion','human'))) %>%
#     mutate(stim_type_long = factor(stim_type_long,
#                                    levels = c('dove (control)','lion','human'))) %>%
#     mutate(predicted_direction = slice)
#   return(pred)
# }
# pred_ad <- extract_predictions(array = age_mtx_org, slice = 1, df = age_move_org)
# pred_aa <- extract_predictions(array = age_mtx_org, slice = 2, df = age_move_org)
# pred_nt <- extract_predictions(array = age_mtx_org, slice = 3, df = age_move_org)
# pred_ta <- extract_predictions(array = age_mtx_org, slice = 4, df = age_move_org)
# pred_td <- extract_predictions(array = age_mtx_org, slice = 5, df = age_move_org)
# 
# boxplot_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                        ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                               ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                      'approach directly'))))
# 
#   pred %>%
#     mutate(move_tminus1 = ifelse(prev_num == 1, 'move directly away',
#                                  ifelse(prev_num == 2, 'move away at an angle',
#                                         ifelse(prev_num == 3, 'move neither towards or away',
#                                                ifelse(prev_num == 4,
#                                                       'approach at an angle',
#                                                       'approach directly')))),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))) %>%
#     ggplot()+
#     geom_boxplot(aes(x = f_age_cat,
#                      # fill = as.factor(move_index), # successfully predicts actual data
#                      fill = move_tminus1,
#                      y = epred))+
#     labs(x = 'focal age category',
#          y = 'predicted probability of moving direction',
#          fill = 'movement in previous second',
#          title = predicted_direction)+
#     facet_grid(move_tminus1 ~ stim_type_long)+ # looks a bit rubbish, but freeing y then gives crazy scales. I ideally want one where I can limit the y to 0-0.2 for all but move_tminus1 = prediction_direction which can then go 0.8-1
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# }
# (box_ad <- boxplot_pred(pred_ad))
# (box_aa <- boxplot_pred(pred_aa))
# (box_nt <- boxplot_pred(pred_nt))
# (box_ta <- boxplot_pred(pred_ta))
# (box_td <- boxplot_pred(pred_td))
# 
# density_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                        ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                               ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                      'approach directly'))))
# 
#   pred %>%
#     mutate(move_tminus1 = ifelse(prev_num == 1, 'move directly away',
#                                  ifelse(prev_num == 2, 'move away at an angle',
#                                         ifelse(prev_num == 3, 'move neither towards or away',
#                                                ifelse(prev_num == 4,
#                                                       'approach at an angle',
#                                                       'approach directly')))),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'move neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))) %>%
#     ggplot()+
#     geom_density(aes(x = epred,
#                      fill = f_age_cat),
#                  alpha = 0.4)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability of moving',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(move_tminus1 ~ stim_type_long,
#                scales = 'free')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom')
# 
# }
# (dens_ad <- density_pred(pred_ad))
# (dens_aa <- density_pred(pred_aa))
# (dens_nt <- density_pred(pred_nt))
# (dens_ta <- density_pred(pred_ta))
# (dens_td <- density_pred(pred_td))
# 
# ridge_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                        ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                               ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                      'approach directly'))))
# 
#   pred %>%
#     mutate(move_tminus1 = ifelse(prev_num == 1, 'move directly away',
#                                  ifelse(prev_num == 2, 'move away at an angle',
#                                         ifelse(prev_num == 3, 'neither towards or away',
#                                                ifelse(prev_num == 4,
#                                                       'approach at an angle',
#                                                       'approach directly')))),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
#                                                           'move away at an angle',
#                                                           'neither towards or away',
#                                                           'approach at an angle',
#                                                           'approach directly'))) %>%
#     ggplot()+
#     geom_density_ridges(aes(x = epred,
#                             y = f_age_cat,
#                             fill = f_age_cat),
#                         alpha = 0.6)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability of moving direction',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(stim_type_long ~ move_tminus1,
#                scales = 'free_x')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# }
# (ridge_ad <- ridge_pred(pred_ad))
# (ridge_aa <- ridge_pred(pred_aa))
# (ridge_nt <- ridge_pred(pred_nt))
# (ridge_ta <- ridge_pred(pred_ta))
# (ridge_td <- ridge_pred(pred_td))
# 
# ggsave(filename = 'mom2_predicted_ridges_directlyaway.png',
#        path = '../outputs/movement_ordinal_model_2bda/',
#        plot = ridge_ad,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# ggsave(filename = 'mom2_predicted_ridges_angleaway.png',
#        path = '../outputs/movement_ordinal_model_2bda/',
#        plot = ridge_aa,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# ggsave(filename = 'mom2_predicted_ridges_neither.png',
#        path = '../outputs/movement_ordinal_model_2bda/',
#        plot = ridge_nt,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# ggsave(filename = 'mom2_predicted_ridges_angleapproach.png',
#        path = '../outputs/movement_ordinal_model_2bda/',
#        plot = ridge_ta,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# ggsave(filename = 'mom2_predicted_ridges_directlyapproach.png',
#        path = '../outputs/movement_ordinal_model_2bda/',
#        plot = ridge_td,
#        device = 'png', height = 1800, width = 2800, units = 'px')
# 
# ## plot
# contrast_plot <- function(contrasts, direction, free_y = T){
#   contrasts %>%
#     filter(move_pred == direction) %>%
#     rename(f_age_cat_org = f_age_cat,
#            f_age_num_org = f_age_num) %>%
#     mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
#                                   f_age_num_org + 1)) %>%
#     mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
#                                   ifelse(f_age_num_alt == 2, '16-20 yrs',
#                                          ifelse(f_age_num_alt == 3, '21-25 yrs',
#                                                 '26-35 yrs')))) %>%
#     mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
#            stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                               ifelse(stim_type == 'l', 'lion', 'human')),
#            prev_action = ifelse(prev_action == 'move away directly',
#                                  'move away directly',
#                                  ifelse(prev_action == 'move away at an angle',
#                                         'move away at an angle',
#                                         ifelse(prev_action == 'move directly with',
#                                                'neither towards or away',
#                                                ifelse(prev_action == 'approach at an angle',
#                                                       'approach at an angle',
#                                                       'approach directly'))))) %>%
#     mutate(prev_action = factor(prev_action,
#                                  levels = c('move away directly',
#                                             'move away at an angle',
#                                             'neither towards or away',
#                                             'approach at an angle',
#                                             'approach directly'))) %>%
#     ggplot()+
#     geom_violin(aes(x = comparison,
#                     y = difference,
#                     fill = comparison),
#                 position = position_dodge(0.5))+
#     scale_fill_viridis_d()+
#     facet_grid(prev_action ~ stim_type,
#                scales = ifelse(free_y == T, 'free_y', 'fixed')
#     )+
#     labs(fill = 'comparison',
#          title = ifelse(direction == 'awayangle',
#                         'away at an angle',
#                         ifelse(direction == 'awaydirect',
#                                'away directly',
#                                ifelse(direction == 'neither',
#                                       'neither towards or away',
#                                       ifelse(direction == 'twdsangle',
#                                              'towards at an angle',
#                                              'towards directly')))))+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90, vjust = 0.5))
# }
# (contrasts_ad_fixed <- contrast_plot(contrasts_long, 'awaydirect', free_y = F))
# (contrasts_aa_fixed <- contrast_plot(contrasts_long, 'awayangle', free_y = F))
# (contrasts_nt_fixed <- contrast_plot(contrasts_long, 'neither', free_y = F))
# (contrasts_ta_fixed <- contrast_plot(contrasts_long, 'twdsangle', free_y = F))
# (contrasts_td_fixed <- contrast_plot(contrasts_long, 'twdsdirect', free_y = F))
# 
# (contrasts_ad_free <- contrast_plot(contrasts_long, 'awaydirect', free_y = T))
# (contrasts_aa_free <- contrast_plot(contrasts_long, 'awayangle', free_y = T))
# (contrasts_nt_free <- contrast_plot(contrasts_long, 'neither', free_y = T))
# (contrasts_ta_free <- contrast_plot(contrasts_long, 'twdsangle', free_y = T))
# (contrasts_td_free <- contrast_plot(contrasts_long, 'twdsdirect', free_y = T))
# 
# for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
#   contrast_plot(contrasts_long, plot, free_y = F)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom2_contrasts_fixedscales_',plot,'.png'),
#          path = '../outputs/movement_ordinal_model_2bda/',
#          height = 3000, width = 2100, unit = 'px')
# }
# for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
#   contrast_plot(contrasts_long, plot, free_y = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom2_contrasts_freescales_',plot,'.png'),
#          path = '../outputs/movement_ordinal_model_2bda/',
#          height = 3000, width = 2100, unit = 'px')
# }
# 
# print('movement ordinal 2 complete')
# dev.off()
# 
#### movement ordinal 2 -- probability of performing behaviour ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_movementordinal2_noprev.pdf')
rm(list = ls()) ; gc() ; load('movement_direction/moving_noprev_2bda_modelpredictions.RData')
ls()

# pred_ad <- readRDS('../data_processed/move_dir_noprev_predictions_awaydirect.RDS')
# pred_aa <- readRDS('../data_processed/move_dir_noprev_predictions_awayangle.RDS')
# pred_nt <- readRDS('../data_processed/move_dir_noprev_predictions_neitherdir.RDS')
# pred_ta <- readRDS('../data_processed/move_dir_noprev_predictions_towardangle.RDS')
# pred_td <- readRDS('../data_processed/move_dir_noprev_predictions_towarddirect.RDS')

pred_ad <- pred %>% filter(pred_type == 'move directly away')
pred_aa <- pred %>% filter(pred_type == 'move away at an angle')
pred_nt <- pred %>% filter(pred_type == 'move neither towards or away')
pred_ta <- pred %>% filter(pred_type == 'approach at an angle')
pred_td <- pred %>% filter(pred_type == 'approach directly')
print('plot data created')

boxplot_pred <- function(pred){
  predicted_direction <- ifelse(pred$pred_type_num[1] == 1, 'move directly away',
                                ifelse(pred$pred_type_num[1] == 2, 'move away at an angle',
                                       ifelse(pred$pred_type_num[1] == 3, 'move neither towards or away',
                                              ifelse(pred$pred_type_num[1] == 4, 'approach at an angle',
                                                     'approach directly'))))

  pred %>%
    mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                              ifelse(stim_type == 'l','lion','human'))) %>%
    mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
           bda = factor(bda, levels = c('before','during','after'))) %>%
    ggplot()+
    geom_boxplot(aes(x = f_age_cat,
                     # fill = as.factor(move_index),
                     fill = p_age_cat,
                     y = epred))+
    labs(x = 'focal age category',
         y = 'predicted probability of moving direction',
         fill = 'target age category',
         title = predicted_direction)+
    facet_grid(bda ~ stim_type)+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
}
(box_ad <- boxplot_pred(pred_ad))
(box_aa <- boxplot_pred(pred_aa))
(box_nt <- boxplot_pred(pred_nt))
(box_ta <- boxplot_pred(pred_ta))
(box_td <- boxplot_pred(pred_td))
print('boxplots drawn')

ridge_pred <- function(pred){
  predicted_direction <- ifelse(pred$pred_type_num[1] == 1, 'move directly away',
                                ifelse(pred$pred_type_num[1] == 2, 'move away at an angle',
                                       ifelse(pred$pred_type_num[1] == 3, 'move neither towards or away',
                                              ifelse(pred$pred_type_num[1] == 4, 'approach at an angle',
                                                     'approach directly'))))

  pred %>%
    mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                              ifelse(stim_type == 'l','lion','human'))) %>%
    mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
           bda = factor(bda, levels = c('before','during','after'))) %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = f_age_cat),
                        alpha = 0.6)+
    labs(fill = 'focal age category',
         x = 'predicted probability of moving direction',
         y = 'focal age category',
         title = predicted_direction)+
    facet_grid(bda ~ stim_type,
               scales = 'free_x')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
}
(ridge_ad <- ridge_pred(pred_ad))
(ridge_aa <- ridge_pred(pred_aa))
(ridge_nt <- ridge_pred(pred_nt))
(ridge_ta <- ridge_pred(pred_ta))
(ridge_td <- ridge_pred(pred_td))
print('first ggridges plots created')

ggsave(filename = 'mom_noprev_predicted_simpleridges_directlyaway.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_ad,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom_noprev_predicted_simpleridges_angleaway.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_aa,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom_noprev_predicted_simpleridges_neither.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_nt,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom_noprev_predicted_simpleridges_angleapproach.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_ta,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom_noprev_predicted_simpleridges_directlyapproach.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_td,
       device = 'png', height = 1800, width = 2800, units = 'px')
print('first ggridges plots saved')

ridge_pred <- function(pred){
  predicted_direction <- ifelse(pred$pred_type_num[1] == 1, 'move directly away',
                                ifelse(pred$pred_type_num[1] == 2, 'move away at an angle',
                                       ifelse(pred$pred_type_num[1] == 3, 'move neither towards or away',
                                              ifelse(pred$pred_type_num[1] == 4, 'approach at an angle',
                                                     'approach directly'))))

  pred %>%
    mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                              ifelse(stim_type == 'l','lion','human'))) %>%
    mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
           bda = factor(bda, levels = c('before','during','after'))) %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6)+
    labs(fill = 'target age category',
         x = 'predicted probability of moving direction',
         y = 'focal age category',
         title = predicted_direction)+
    facet_grid(bda ~ stim_type,
               scales = 'free_x')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
}
(ridge_ad <- ridge_pred(pred_ad))
(ridge_aa <- ridge_pred(pred_aa))
(ridge_nt <- ridge_pred(pred_nt))
(ridge_ta <- ridge_pred(pred_ta))
(ridge_td <- ridge_pred(pred_td))
print('second ggridges plots created')

ggsave(filename = 'mom_noprev_predicted_partnerridges_directlyaway.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_ad,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom_noprev_predicted_partnerridges_angleaway.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_aa,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom_noprev_predicted_partnerridges_neither.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_nt,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom_noprev_predicted_partnerridges_angleapproach.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_ta,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom_noprev_predicted_partnerridges_directlyapproach.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_td,
       device = 'png', height = 1800, width = 2800, units = 'px')
print('second ggridges plots saved')

## plot
load('movement_direction/moving_noprev_2bda_agecontrasts.RData')

# contrasts_short <- contrasts_long[1:(nrow(contrasts_long)/100),]
# saveRDS(contrasts_short, '../data_processed/move_noprev_agecontrasts_test.RDS')
# 
# contrasts_short <- readRDS('../data_processed/move_noprev_agecontrasts_test.RDS')

contrast_plot <- function(contrasts, free_y = T){
  contrasts %>%
    rename(f_age_cat_org = f_age_cat,
           f_age_num_org = f_age_num) %>%
    mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                  f_age_num_org + 1)) %>%
    mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                  ifelse(f_age_num_alt == 2, '16-20 yrs',
                                         ifelse(f_age_num_alt == 3, '21-25 yrs',
                                                '26-35 yrs'))),
           move_pred = ifelse(move_pred == 'awayangle',
                              'away at an angle',
                              ifelse(move_pred == 'awaydirect',
                                     'away directly',
                                     ifelse(move_pred == 'neither',
                                            'neither towards or away',
                                            ifelse(move_pred == 'twdsangle',
                                                   'towards at an angle',
                                                   'towards directly'))))) %>%
    mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
           stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                              ifelse(stim_type == 'l', 'lion', 'human'))) %>%
    ggplot()+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = comparison),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(move_pred ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed')
    )+
    labs(fill = 'comparison')+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
}
contrast_plot(contrasts_long, free_y = F)
ggsave(plot = last_plot(), device = 'png',
       filename = 'mom_noprev_contrasts_fixedscales.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       height = 3000, width = 2100, unit = 'px')
contrast_plot(contrasts_long, free_y = T)
ggsave(plot = last_plot(), device = 'png',
       filename = 'mom_noprev_contrasts_freescales.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       height = 3000, width = 2100, unit = 'px')
print('simple plots created')

contrast_plot <- function(contrasts, stim, free_y = T){
  title <- ifelse(stim == 'ctd', 'dove (control)',
                  ifelse(stim == 'l', 'lion', 'human'))
  contrasts %>%
    filter(stim_type == stim) %>% 
    rename(f_age_cat_org = f_age_cat,
           f_age_num_org = f_age_num) %>%
    mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                  f_age_num_org + 1)) %>%
    mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                  ifelse(f_age_num_alt == 2, '16-20 yrs',
                                         ifelse(f_age_num_alt == 3, '21-25 yrs',
                                                '26-35 yrs'))),
           p_age_cat = paste0('T: ', p_age_cat, ' yrs'),
           move_pred = ifelse(move_pred == 'awayangle',
                              'away at an angle',
                              ifelse(move_pred == 'awaydirect',
                                     'away directly',
                                     ifelse(move_pred == 'neither',
                                            'neither towards or away',
                                            ifelse(move_pred == 'twdsangle',
                                                   'towards at an angle',
                                                   'towards directly'))))) %>%
    mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt)) %>%
    ggplot()+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = comparison),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(move_pred ~ p_age_cat,
               scales = ifelse(free_y == T, 'free_y', 'fixed')
    )+
    labs(fill = 'focal comparison',
         title = title)+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
}
for(stimulus in c('ctd','l','h')){
  contrast_plot(contrasts_long, stim = stimulus, free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_fixedscales_splitpartner_',stimulus,'.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}
for(stimulus in c('ctd','l','h')){
  contrast_plot(contrasts_long, stim = stimulus, free_y = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_freescales_splitpartner_',stimulus,'.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}
print('complex plots created')

print('movement ignoring t-1 behaviour complete')
dev.off()

# #### looking ordinal 1  ####
# pdf('../outputs/looking_ordinal_model_1/niceplots_lookingordinal1.pdf')
# rm(list = ls()) ; gc() ; load('looking_direction/looking_ordinal_model1_agecontrasts.RData')
# rm(age_new, age_pred, age_pred_all, age_pred_i, alt_vs_org_away, alt_vs_org_side, alt_vs_org_twds, away_12, away_14, away_23, away_34, plot, side_12, side_14, side_23, side_34, twds_12, twds_23, twds_14, twds_34) ; gc()
# save.image('looking_direction/looking_ordinal_model1_ageplotting.RData')
# 
# ## plot predictions
# age_look_org$data_id <- 1:nrow(age_look_org)
# extract_predictions <- function(array, slice, df){
#   matrix <- array[,,slice]
#   colnames(matrix) <- 1:nrow(df)
#   pred <- matrix %>%
#     as.data.frame() %>%
#     pivot_longer(cols = everything(), names_to = 'data_id', values_to = 'epred') %>%
#     mutate(data_id = as.integer(data_id)) %>%
#     left_join(df, by = 'data_id')%>%
#     mutate(stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
#                                    ifelse(stim_type == 'l','lion','human'))) %>%
#     mutate(stim_type_long = factor(stim_type_long,
#                                    levels = c('dove (control)','lion','human'))) %>%
#     mutate(predicted_direction = slice)
#   return(pred)
# }
# pred_a <- extract_predictions(array = age_mtx_org, slice = 1, df = age_look_org)
# pred_s <- extract_predictions(array = age_mtx_org, slice = 2, df = age_look_org)
# pred_t <- extract_predictions(array = age_mtx_org, slice = 3, df = age_look_org)
# 
# boxplot_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'side-on',
#                                        'look at'))
# 
#   pred %>%
#     mutate(look_tminus1 = ifelse(look_tminus1_num == 1, 'look away',
#                                  ifelse(look_tminus1_num == 2, 'side-on',
#                                         'look at')),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
#                                                           'side-on',
#                                                           'look at'))) %>%
#     ggplot()+
#     geom_boxplot(aes(x = f_age_cat,
#                      # fill = as.factor(move_index), # successfully predicts actual data
#                      fill = look_tminus1,
#                      y = epred))+
#     labs(x = 'focal age category',
#          y = 'predicted probability of looking direction',
#          fill = 'looking in previous second',
#          title = predicted_direction)+
#     facet_grid(look_tminus1 ~ stim_type_long)+ # looks a bit rubbish, but freeing y then gives crazy scales. I ideally want one where I can limit the y to 0-0.2 for all but move_tminus1 = prediction_direction which can then go 0.8-1
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# 
# }
# (box_a <- boxplot_pred(pred_a))
# (box_s <- boxplot_pred(pred_s))
# (box_t <- boxplot_pred(pred_t))
# 
# density_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'side-on',
#                                        'look at'))
# 
#   pred %>%
#     mutate(look_tminus1 = ifelse(look_tminus1_num == 1, 'look away',
#                                  ifelse(look_tminus1_num == 2, 'side-on',
#                                         'look at')),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
#                                                           'side-on',
#                                                           'look at'))) %>%
#     ggplot()+
#     geom_density(aes(x = epred,
#                      fill = f_age_cat),
#                  alpha = 0.4)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability of looking',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(look_tminus1 ~ stim_type_long,
#                scales = 'free')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom')
# 
# }
# (dens_a <- density_pred(pred_a))
# (dens_s <- density_pred(pred_s))
# (dens_t <- density_pred(pred_t))
# 
# ridge_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'side-on',
#                                        'look at'))
# 
#   pred %>%
#     mutate(look_tminus1 = ifelse(look_tminus1_num == 1, 'look away',
#                                  ifelse(look_tminus1_num == 2, 'side-on',
#                                         'look at')),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
#                                                           'side-on',
#                                                           'look at'))) %>%
#     ggplot()+
#     geom_density_ridges(aes(x = epred,
#                             y = f_age_cat,
#                             fill = f_age_cat),
#                         alpha = 0.6)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability of looking direction',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(stim_type_long ~ look_tminus1,
#                scales = 'free_x')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# }
# (ridge_a <- ridge_pred(pred_a))
# (ridge_s <- ridge_pred(pred_s))
# (ridge_t <- ridge_pred(pred_t))
# 
# ggsave(filename = 'lom1_predicted_ridges_away.png',
#        path = '../outputs/looking_ordinal_model_1/',
#        plot = ridge_a,
#        device = 'png', height = 1800, width = 2700, units = 'px')
# ggsave(filename = 'lom1_predicted_ridges_sideon.png',
#        path = '../outputs/looking_ordinal_model_1/',
#        plot = ridge_s,
#        device = 'png', height = 1800, width = 2700, units = 'px')
# ggsave(filename = 'lom1_predicted_ridges_towards.png',
#        path = '../outputs/looking_ordinal_model_1/',
#        plot = ridge_t,
#        device = 'png', height = 1800, width = 2700, units = 'px')
# 
# ## plot
# contrast_plot <- function(contrasts, direction, free_y = T){
#   contrasts %>%
#     filter(pred_type == direction) %>%
#     rename(f_age_num_alt = f_age_new,
#            f_age_num_org = f_age_num) %>%
#     mutate(f_age_cat_org = ifelse(f_age_num_org == 1,'10-15 yrs',
#                                   ifelse(f_age_num_org == 2, '16-20 yrs',
#                                          ifelse(f_age_num_org == 3, '21-25 yrs',
#                                                 '26-35 yrs'))),
#            f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
#                                   ifelse(f_age_num_alt == 2, '16-20 yrs',
#                                          ifelse(f_age_num_alt == 3, '21-25 yrs',
#                                                 '26-35 yrs')))) %>%
#     mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
#            stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                               ifelse(stim_type == 'l', 'lion', 'human'))) %>%
#     mutate(look_tminus1 = factor(look_tminus1,
#                                  levels = c('look away at t-1',
#                                             'side on at t-1',
#                                             'look at at t-1'))) %>%
#     ggplot()+
#     geom_hline(yintercept = 0, lty = 2)+
#     geom_violin(aes(x = comparison,
#                     y = difference,
#                     fill = comparison),
#                 position = position_dodge(0.5))+
#     scale_fill_viridis_d()+
#     facet_grid(look_tminus1 ~ stim_type,
#                scales = ifelse(free_y == T, 'free_y', 'fixed')
#     )+
#     labs(fill = 'comparison',
#          title = direction)+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90, vjust = 0.5))
# }
# for(plot in c('look away','side on','look at')){
#   contrast_plot(contrasts_long, plot, free_y = F)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('lom1_contrasts_fixedscales_',plot,'.png'),
#          path = '../outputs/looking_ordinal_model_1/',
#          height = 3000, width = 2100, unit = 'px')
# }
# for(plot in c('look away','side on','look at')){
#   contrast_plot(contrasts_long, plot, free_y = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('lom1_contrasts_freescales_',plot,'.png'),
#          path = '../outputs/looking_ordinal_model_1/',
#          height = 3000, width = 2100, unit = 'px')
# }
# 
# print('looking ordinal 1 complete')
# dev.off()
# 
# #### looking ordinal 2 -- probability of changing behaviour ####
# pdf('../outputs/looking_ordinal_model_2bda/niceplots_lookingordinal2.pdf')
# rm(list = ls()) ; gc() ; load('looking_direction/looking_ordinal_model2bda_agecontrasts.RData')
# rm(list = ls()[! ls() %in% c('age_look_org','contrasts','contrasts_long','look','age_mtx_org')]) ; gc()
# 
# ## plot predictions
# age_look_org$data_id <- 1:nrow(age_look_org)
# extract_predictions <- function(array, slice, df){
#   matrix <- array[,,slice]
#   colnames(matrix) <- 1:nrow(df)
#   pred <- matrix %>%
#     as.data.frame() %>%
#     pivot_longer(cols = everything(), names_to = 'data_id', values_to = 'epred') %>%
#     mutate(data_id = as.integer(data_id)) %>%
#     left_join(df, by = 'data_id')%>%
#     mutate(stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
#                                    ifelse(stim_type == 'l','lion','human'))) %>%
#     mutate(stim_type_long = factor(stim_type_long,
#                                    levels = c('dove (control)','lion','human'))) %>%
#     mutate(predicted_direction = slice)
#   return(pred)
# }
# pred_a <- extract_predictions(array = age_mtx_org, slice = 1, df = age_look_org)
# pred_s <- extract_predictions(array = age_mtx_org, slice = 2, df = age_look_org)
# pred_t <- extract_predictions(array = age_mtx_org, slice = 3, df = age_look_org)
# 
# boxplot_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'side-on',
#                                        'look at'))
# 
#   pred %>%
#     mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
#                                  ifelse(prev_num == 2, 'side-on',
#                                         'look at')),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
#                                                           'side-on',
#                                                           'look at'))) %>%
#     ggplot()+
#     geom_boxplot(aes(x = f_age_cat,
#                      # fill = as.factor(look_index), # successfully predicts actual data
#                      fill = look_tminus1,
#                      y = epred))+
#     labs(x = 'focal age category',
#          y = 'predicted probability of looking direction',
#          fill = 'looking in previous second',
#          title = predicted_direction)+
#     facet_grid(look_tminus1 ~ stim_type_long)+ # looks a bit rubbish, but freeing y then gives crazy scales. I ideally want one where I can limit the y to 0-0.2 for all but look_tminus1 = prediction_direction which can then go 0.8-1
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# 
# }
# (box_a <- boxplot_pred(pred_a))
# (box_s <- boxplot_pred(pred_s))
# (box_t <- boxplot_pred(pred_t))
# 
# density_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'side-on',
#                                        ifelse(pred$predicted_direction[1] == 3, 'look at')))
# 
#   graph <- pred %>%
#     mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
#                                  ifelse(prev_num == 2, 'side-on',
#                                         'look at')),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
#                                                           'side-on',
#                                                           'look at'))) %>%
#     ggplot()+
#     geom_density(aes(x = epred,
#                      fill = f_age_cat),
#                  alpha = 0.4)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability of looking',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(look_tminus1 ~ stim_type_long,
#                scales = 'free')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom')
#   return(graph)
# }
# (dens_a <- density_pred(pred_a))
# (dens_s <- density_pred(pred_s))
# (dens_t <- density_pred(pred_t))
# 
# ridge_pred <- function(pred){
#   predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
#                                 ifelse(pred$predicted_direction[1] == 2, 'side-on',
#                                        'look at'))
# 
#   pred %>%
#     mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
#                                  ifelse(prev_num == 2, 'side-on',
#                                         'look at')),
#            f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                               ifelse(f_age_num == 2, '16-20 years',
#                                      ifelse(f_age_num == 3, '21-25 years',
#                                             '26-35 years')))) %>%
#     mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
#                                                           'side-on',
#                                                           'look at'))) %>%
#     ggplot()+
#     geom_density_ridges(aes(x = epred,
#                             y = f_age_cat,
#                             fill = f_age_cat),
#                         alpha = 0.6)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability of looking direction',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(stim_type_long ~ look_tminus1,
#                scales = 'free_x')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# }
# (ridge_a <- ridge_pred(pred_a))
# (ridge_s <- ridge_pred(pred_s))
# (ridge_t <- ridge_pred(pred_t))
# 
# ggsave(filename = 'lom2_predicted_ridges_away.png',
#        path = '../outputs/looking_ordinal_model_2bda/',
#        plot = ridge_a,
#        device = 'png', height = 1800, width = 2700, units = 'px')
# ggsave(filename = 'lom2_predicted_ridges_sideon.png',
#        path = '../outputs/looking_ordinal_model_2bda/',
#        plot = ridge_s,
#        device = 'png', height = 1800, width = 2700, units = 'px')
# ggsave(filename = 'lom2_predicted_ridges_towards.png',
#        path = '../outputs/looking_ordinal_model_2bda/',
#        plot = ridge_t,
#        device = 'png', height = 1800, width = 2700, units = 'px')
# 
# ## plot
# contrast_plot <- function(contrasts, direction, free_y = T){
#   contrasts %>%
#     filter(look_pred == direction) %>%
#     rename(f_age_cat_org = f_age_cat,
#            f_age_num_org = f_age_num) %>%
#     mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
#                                   f_age_num_org + 1)) %>%
#     mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
#                                   ifelse(f_age_num_alt == 2, '16-20 yrs',
#                                          ifelse(f_age_num_alt == 3, '21-25 yrs',
#                                                 '26-35 yrs')))) %>%
#     mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
#            stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                               ifelse(stim_type == 'l', 'lion','human'))) %>%
#     mutate(prev_action = factor(prev_action,
#                                  levels = c('look directly away',
#                                             'side-on',
#                                             'look at directly')),
#            stim_type = factor(stim_type,
#                                 levels = c('dove (control)',
#                                            'lion',
#                                            'human'))) %>%
#     ggplot()+
#     geom_violin(aes(x = comparison,
#                     y = difference,
#                     fill = comparison),
#                 position = position_dodge(0.5))+
#     scale_fill_viridis_d()+
#     facet_grid(prev_action ~ stim_type,
#                scales = ifelse(free_y == T, 'free_y', 'fixed')
#     )+
#     labs(fill = 'comparison',
#          title = ifelse(direction == 'away', 'look away',
#                         ifelse(direction == 'side', 'side-on',
#                                'look towards')))+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90, vjust = 0.5))
# }
# (contrasts_a_fixed <- contrast_plot(contrasts_long, 'away', free_y = F))
# (contrasts_s_fixed <- contrast_plot(contrasts_long, 'side', free_y = F))
# (contrasts_t_fixed <- contrast_plot(contrasts_long, 'twds', free_y = F))
# 
# (contrasts_a_free <- contrast_plot(contrasts_long, 'away', free_y = T))
# (contrasts_s_free <- contrast_plot(contrasts_long, 'side', free_y = T))
# (contrasts_t_free <- contrast_plot(contrasts_long, 'twds', free_y = T))
# 
# for(plot in c('away','side','twds')){
#   contrast_plot(contrasts_long, plot, free_y = F)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('lom2_contrasts_fixedscales_',plot,'.png'),
#          path = '../outputs/looking_ordinal_model_2bda/',
#          height = 3000, width = 2100, unit = 'px')
# }
# for(plot in c('away','side','twds')){
#   contrast_plot(contrasts_long, plot, free_y = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('lom2_contrasts_freescales_',plot,'.png'),
#          path = '../outputs/looking_ordinal_model_2bda/',
#          height = 3000, width = 2100, unit = 'px')
# }
# 
# print('looking ordinal 2 complete')
# dev.off()
# 
#### looking ordinal 2 -- probability of performing behaviour ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_lookingordinal2_noprev.pdf')
rm(list = ls()) ; gc() ; load('looking_direction/looking_noprev_agecontrasts.RData')
rm(list = ls()[! ls() %in% c('pred','age_look_org','contrasts','contrasts_long','look','age_mtx_org')]) ; gc()

save.image('looking_direction/looking_noprev_plotting.RData')

## plot predictions
age_look_org$data_id <- 1:nrow(age_look_org)
extract_predictions <- function(array, slice, df){
  matrix <- array[,,slice]
  colnames(matrix) <- 1:nrow(df)
  pred <- matrix %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = 'data_id', values_to = 'epred') %>%
    mutate(data_id = as.integer(data_id)) %>%
    left_join(df, by = 'data_id')%>%
    mutate(stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
                                   ifelse(stim_type == 'l','lion','human'))) %>%
    mutate(stim_type_long = factor(stim_type_long,
                                   levels = c('dove (control)','lion','human'))) %>%
    mutate(predicted_direction = slice)
  return(pred)
}
pred_a <- extract_predictions(array = age_mtx_org, slice = 1, df = age_look_org)
pred_s <- extract_predictions(array = age_mtx_org, slice = 2, df = age_look_org)
pred_t <- extract_predictions(array = age_mtx_org, slice = 3, df = age_look_org)

boxplot_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                       'look at'))

  pred %>%
    mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                              ifelse(f_age_num == 2, '16-20 yrs',
                                     ifelse(f_age_num == 3, '21-25 yrs',
                                            '26-35 yrs'))),
           p_age_cat = ifelse(p_age_num == 1, '10-15 yrs',
                              ifelse(p_age_num == 2, '16-20 yrs',
                                     ifelse(p_age_num == 3, '21-25 yrs',
                                            '26-35 yrs')))) %>%
    ggplot()+
    geom_boxplot(aes(x = f_age_cat,
                     # fill = as.factor(look_index), # successfully predicts actual data
                     fill = p_age_cat,
                     y = epred))+
    labs(x = 'focal age category',
         y = 'predicted probability of looking direction',
         fill = 'target age',
         title = predicted_direction)+
    facet_grid(bda ~ stim_type_long)+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))

}
(box_a <- boxplot_pred(pred_a))
(box_s <- boxplot_pred(pred_s))
(box_t <- boxplot_pred(pred_t))

density_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                       ifelse(pred$predicted_direction[1] == 3, 'look at')))

  graph <- pred %>%
    mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                              ifelse(f_age_num == 2, '16-20 yrs',
                                     ifelse(f_age_num == 3, '21-25 yrs',
                                            '26-35 yrs'))),
           p_age_cat = ifelse(p_age_num == 1, '10-15 yrs',
                              ifelse(p_age_num == 2, '16-20 yrs',
                                     ifelse(p_age_num == 3, '21-25 yrs',
                                            '26-35 yrs')))) %>%
    geom_density(aes(x = epred,
                     fill = f_age_cat),
                 alpha = 0.4)+
    labs(fill = 'focal age category',
         x = 'predicted probability of looking',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(bda ~ stim_type_long,
               scales = 'free')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom')
  return(graph)
}
(dens_a <- density_pred(pred_a))
(dens_s <- density_pred(pred_s))
(dens_t <- density_pred(pred_t))

ridge_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                       'look at'))

  pred %>%
    mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                              ifelse(f_age_num == 2, '16-20 yrs',
                                     ifelse(f_age_num == 3, '21-25 yrs',
                                            '26-35 yrs'))),
           p_age_cat = ifelse(p_age_num == 1, '10-15 yrs',
                              ifelse(p_age_num == 2, '16-20 yrs',
                                     ifelse(p_age_num == 3, '21-25 yrs',
                                            '26-35 yrs')))) %>%
    mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6)+
    labs(fill = 'focal age category',
         x = 'predicted probability of looking direction',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(bda ~ stim_type_long,
               scales = 'free_x')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
}
(ridge_a <- ridge_pred(pred_a))
(ridge_s <- ridge_pred(pred_s))
(ridge_t <- ridge_pred(pred_t))

ggsave(filename = 'lom2_noprev_predicted_ridges_away.png',
       path = '../outputs/looking_ordinal_model_2bda/',
       plot = ridge_a,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'lom2_noprev_predicted_ridges_sideon.png',
       path = '../outputs/looking_ordinal_model_2bda/',
       plot = ridge_s,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'lom2_noprev_predicted_ridges_towards.png',
       path = '../outputs/looking_ordinal_model_2bda/',
       plot = ridge_t,
       device = 'png', height = 1800, width = 2700, units = 'px')

## plot
contrast_plot <- function(contrasts, direction, free_y = T){
  contrasts %>%
    filter(look_pred == direction) %>%
    rename(f_age_cat_org = f_age_cat,
           f_age_num_org = f_age_num) %>%
    mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                  f_age_num_org + 1)) %>%
    mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                  ifelse(f_age_num_alt == 2, '16-20 yrs',
                                         ifelse(f_age_num_alt == 3, '21-25 yrs',
                                                '26-35 yrs')))) %>%
    mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
           stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                              ifelse(stim_type == 'l', 'lion','human'))) %>%
    mutate(stim_type = factor(stim_type,
                                levels = c('dove (control)',
                                           'lion',
                                           'human'))) %>%
    ggplot()+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = p_age_cat),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(bda ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed')
    )+
    labs(fill = 'comparison',
         title = ifelse(direction == 'away', 'look away',
                        ifelse(direction == 'side', 'side-on',
                               'look towards')))+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
}
(contrasts_a_fixed <- contrast_plot(contrasts_long, 'away', free_y = F))
(contrasts_s_fixed <- contrast_plot(contrasts_long, 'side', free_y = F))
(contrasts_t_fixed <- contrast_plot(contrasts_long, 'twds', free_y = F))

(contrasts_a_free <- contrast_plot(contrasts_long, 'away', free_y = T))
(contrasts_s_free <- contrast_plot(contrasts_long, 'side', free_y = T))
(contrasts_t_free <- contrast_plot(contrasts_long, 'twds', free_y = T))

for(plot in c('away','side','twds')){
  contrast_plot(contrasts_long, plot, free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom2_noprev_contrasts_fixedscales_',plot,'.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}
for(plot in c('away','side','twds')){
  contrast_plot(contrasts_long, plot, free_y = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom2_noprev_contrasts_freescales_',plot,'.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}

print('looking ordinal 2 complete')
dev.off()

# #### nearest neighbour -- probability of changing behaviour ####
# pdf('../outputs/neighbour_binomial_model_bda/niceplots_nearestneighbour.pdf')
# rm(list = ls()) ; gc() ; load('nearest_neighbour/neighbour_binomial_agecontrasts.RData')
# rm(list = ls()[! ls() %in% c('contrasts','contrasts_long','age_nn_org','age_mtx_org','nn')]) ; gc()
# print('data loaded')
# 
# ## plot predictions
# colnames(age_mtx_org) <- 1:nrow(age_nn_org)
# age_nn_org$data_id <- 1:nrow(age_nn_org)
# pred <- age_mtx_org %>%
#   as.data.frame() %>%
#   pivot_longer(cols = everything(), names_to = 'data_id', values_to = 'epred') %>%
#   mutate(data_id = as.integer(data_id)) %>%
#   left_join(age_nn_org, by = 'data_id') %>%
#   mutate(stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
#                                  ifelse(stim_type == 'l','lion','human'))) %>%
#   mutate(stim_type_long = factor(stim_type_long,
#                                  levels = c('dove (control)','lion','human')))
# print('data created')
# 
# pred %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1','neighbours at t-1')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
#   ggplot()+
#   geom_boxplot(aes(x = age_combo,
#                    # fill = as.factor(move_index), # successfully predicts actual data
#                    fill = prev,
#                    y = epred))+
#   labs(x = 'age category combination',
#        y = 'predicted probability\nof being neighbours',
#        fill = 'neighbours in previous second')+
#   facet_wrap(. ~ stim_type_long)+
#   scale_fill_viridis_d()+
#   theme(legend.position = 'bottom')
# print('boxplots run')
# 
# pred %>%
#   ggplot()+
#   geom_density(aes(x = epred,
#                    fill = age_combo),
#                alpha = 0.4)+
#   labs(fill = 'age category combination',
#        x = 'predicted probability\nof being neighbours',
#        y = 'probability density')+
#   facet_grid(prev ~ stim_type_long,
#              scales = 'free')+
#   scale_fill_viridis_d()+
#   theme(legend.position = 'bottom')
# print('data loaded')
# 
# pred %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1','neighbours at t-1')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
#   separate(age_combo, into = c('f_age_num','p_age_num'), sep = '_', remove = T) %>%
#   mutate(f_age_cat = ifelse(f_age_num == 1, '10-15',
#                             ifelse(f_age_num == 2, '16-20',
#                                    ifelse(f_age_num == 3, '21-25',
#                                           '26-35'))),
#          p_age_cat = ifelse(p_age_num == 1, '10-15',
#                             ifelse(p_age_num == 2, '16-20',
#                                    ifelse(p_age_num == 3, '21-25',
#                                           '26-35')))) %>%
#   mutate(age_combo = paste0('F',f_age_num, '-P',p_age_num),
#          age_combo_long = paste0(f_age_cat,' and ',p_age_cat)) %>%
#   ggplot()+
#   geom_density_ridges(aes(x = epred,
#                           y = age_combo,
#                           fill = age_combo),
#                       alpha = 0.6)+
#   labs(fill = 'age category\ncombination',
#        x = 'predicted probability\nof being neighbours',
#        y = 'probability density')+
#   facet_grid(stim_type_long ~ prev,
#              scales = 'free_x')+
#   scale_fill_viridis_d()+
#   theme(legend.position = 'bottom')
# print('ridges produced')
# ggsave(filename = 'nbm_predicted_ridges.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        device = 'png', height = 3600, width = 2000, units = 'px')
# print('ridges saved')
# 
# rm(pred) ; gc()
# 
# ## contrasts
# contrasts_long <- contrasts_long %>%
#   mutate(comparison = paste0(combo1, '->', combo2),
#          stim_type = ifelse(stim_num == 'stim2a', 'dove (control)',
#                             ifelse(stim_num == 'stim2b', 'dove (control)',
#                                    ifelse(stim_num == 'stim2c', 'dove (control)',
#                                           ifelse(stim_num == 'stim15a.old', 'lion',
#                                                  ifelse(stim_num == 'stim15b', 'lion',
#                                                         ifelse(stim_num == 'stim16b', 'lion',
#                                                                ifelse(stim_num < 11, 'dove (control)',
#                                                                       ifelse(stim_num < 11, 'dove (control)',
#                                                                              ifelse(stim_num < 21, 'lion', 'human')))))))))) %>%
#   mutate(stim_type = factor(stim_type, levels = c('dove (control)', 'lion', 'human')))
# 
# contrasts_long %>%
#   filter(combo1 %in% c(11,12,13,14)) %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
#   ggplot()+
#   geom_violin(aes(x = comparison,
#                   y = difference,
#                   fill = combo2),
#               alpha = 0.6)+
#   scale_fill_viridis_d()+
#   facet_grid(stim_type ~ prev)+
#   theme(axis.text.x = element_text(angle = 90),
#         legend.position = 'none')
# contrasts_long %>%
#   filter(combo1 %in% c(21,22,23,24)) %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
#   ggplot()+
#   geom_violin(aes(x = comparison,
#                   y = difference,
#                   fill = combo2),
#               alpha = 0.6)+
#   scale_fill_viridis_d()+
#   facet_grid(stim_type ~ prev)+
#   theme(axis.text.x = element_text(angle = 90),
#         legend.position = 'none')
# contrasts_long %>%
#   filter(combo1 %in% c(31,32,33,34)) %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
#   ggplot()+
#   geom_violin(aes(x = comparison,
#                   y = difference,
#                   fill = combo2),
#               alpha = 0.6)+
#   scale_fill_viridis_d()+
#   facet_grid(stim_type ~ prev)+
#   theme(axis.text.x = element_text(angle = 90),
#         legend.position = 'none')
# contrasts_long %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
#   filter(combo1 %in% c(41,42,43,44)) %>%
#   ggplot()+
#   geom_violin(aes(x = comparison,
#                   y = difference,
#                   fill = combo2),
#               alpha = 0.6)+
#   scale_fill_viridis_d()+
#   facet_grid(stim_type ~ prev)+
#   theme(axis.text.x = element_text(angle = 90),
#         legend.position = 'none')
# 
# contrasts_long <- contrasts_long %>%
#   mutate(combo1 = as.integer(combo1),
#          combo2 = as.integer(combo2)) %>%
#   mutate(change = combo2 - combo1)
# 
# contrasts_long %>%
#   filter(change %in% c(1,10)) %>%
#   filter(combo1 %in% c(11,12,13,14)) %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1'),
#          change = ifelse(change == 1, 'partner age increased', 'focal age increased')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1',
#                                         'not neighbours at t-1')),
#          change = factor(change, levels = c('partner age increased',
#                                             'focal age increased')),
#          comparison = factor(comparison,
#                              levels = c('11->12','12->13','13->14',
#                                         '11->21','12->22','13->23','14->24'))) %>%
#   ggplot()+
#   geom_violin(aes(x = comparison,
#                   y = difference,
#                   fill = change, colour = change),
#               alpha = 0.6)+
#   scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#   facet_grid(prev ~ stim_type, scales = 'free_y')+
#   theme(axis.text.x = element_text(angle = 90),
#         legend.position = 'bottom')
# 
# contrasts_long %>%
#   filter(change %in% c(1,10)) %>%
#   filter(combo1 %in% c(11,12,13,14)) %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1'),
#          change = ifelse(change == 1, 'partner age increased', 'focal age increased')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1',
#                                         'not neighbours at t-1')),
#          change = factor(change, levels = c('partner age increased',
#                                             'focal age increased')),
#          comparison = factor(comparison,
#                              levels = c('14->24','13->23','12->22','11->21',
#                                         '13->14','12->13','11->12'))) %>%
#   ggplot()+
#   geom_density_ridges(aes(x = difference,
#                           y = comparison,
#                           fill = change, colour = change),
#                       #rel_min_height = 0.01,
#                       alpha = 0.6)+
#   scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#   scale_x_continuous(name = 'contrast (mean per datum predicted)',
#                      limits = c(-0.015, 0.025))+
#   facet_grid(prev ~ stim_type)+
#   theme(axis.text.x = element_text(angle = 90),
#         legend.position = 'bottom')
# 
# plot <- contrasts_long %>%
#   filter(change %in% c(1,10)) %>%
#   mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1'),
#          change = ifelse(change == 1, 'partner age increased', 'focal age increased')) %>%
#   mutate(prev = factor(prev, levels = c('neighbours at t-1',
#                                         'not neighbours at t-1')),
#          change = factor(change, levels = c('partner age increased',
#                                             'focal age increased')),
#          comparison = factor(comparison,
#                              levels = c('43->44','42->43','41->42',
#                                         '33->34','32->33','31->32',
#                                         '34->44','33->43','32->42','31->41',
#                                         '23->24','22->23','21->22',
#                                         '24->34','23->33','22->32','21->31',
#                                         '13->14','12->13','11->12',
#                                         '14->24','13->23','12->22','11->21')))
# (dove <- plot %>%
#     filter(stim_type == 'dove (control)') %>%
#     ggplot()+
#     geom_density_ridges(aes(x = difference,
#                             y = comparison,
#                             fill = change, colour = change),
#                         #rel_min_height = 0.001,
#                         alpha = 0.6)+
#     scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#     scale_x_continuous(name = 'contrast (mean per datum predicted)')+
#     facet_grid(. ~ prev, scales = 'free_x')+
#     labs(title = 'dove (control)')+
#     theme(axis.text.x = element_text(angle = 90),
#           legend.position = 'bottom'))
# ggsave(plot = dove, device = 'png',
#        filename = 'neighbour_contrasts_freescale_ctd.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# dove + scale_x_continuous(limits = c(-0.012, 0.012),
#                           name = 'contrast (mean per datum predicted)')
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'neighbour_contrasts_setscale_ctd.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# 
# (lion <- plot %>%
#     filter(stim_type == 'lion') %>%
#     ggplot()+
#     geom_density_ridges(aes(x = difference,
#                             y = comparison,
#                             fill = change, colour = change),
#                         #rel_min_height = 0.001,
#                         alpha = 0.6)+
#     scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#     scale_x_continuous(name = 'contrast (mean per datum predicted)')+
#     facet_grid(. ~ prev, scales = 'free_x')+
#     labs(title = 'lion')+
#     theme(axis.text.x = element_text(angle = 90),
#           legend.position = 'bottom'))
# ggsave(plot = lion, device = 'png',
#        filename = 'neighbour_contrasts_freescale_l.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# lion + scale_x_continuous(limits = c(-0.02, 0.02),
#                           name = 'contrast (mean per datum predicted)')
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'neighbour_contrasts_setscale_l.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# 
# (human <- plot %>%
#     filter(stim_type == 'human') %>%
#     ggplot()+
#     geom_density_ridges(aes(x = difference,
#                             y = comparison,
#                             fill = change, colour = change),
#                         #rel_min_height = 0.001,
#                         alpha = 0.6)+
#     scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#     scale_x_continuous(name = 'contrast (mean per datum predicted)')+
#     facet_grid(. ~ prev, scales = 'free_x')+
#     labs(title = 'human')+
#     theme(axis.text.x = element_text(angle = 90),
#           legend.position = 'bottom'))
# ggsave(plot = human, device = 'png',
#        filename = 'neighbour_contrasts_freescale_h.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# human + scale_x_continuous(limits = c(-0.02, 0.02),
#                           name = 'contrast (mean per datum predicted)')
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'neighbour_contrasts_setscale_h.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# 
# (prev0 <- plot %>%
#     filter(prev == 'not neighbours at t-1') %>%
#     ggplot()+
#     geom_density_ridges(aes(x = difference,
#                             y = comparison,
#                             fill = change, colour = change),
#                         #rel_min_height = 0.001,
#                         alpha = 0.6)+
#     geom_vline(xintercept = 0, lty = 2)+
#     scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#     scale_x_continuous(name = 'contrast (mean per datum predicted)')+
#     facet_grid(. ~ stim_type, scales = 'free_x')+
#     labs(title = 'not neighbours at t-1')+
#     theme(axis.text.x = element_text(angle = 90),
#           legend.position = 'bottom'))
# ggsave(plot = prev0, device = 'png',
#        filename = 'neighbour_contrasts_fullscale_prev0.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# prev0 +
#   scale_x_continuous(limits = c(-0.005, 0.005),
#                           name = 'contrast (mean per datum predicted)')
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'neighbour_contrasts_setscale_prev0.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# 
# (prev1 <- plot %>%
#     filter(prev == 'neighbours at t-1') %>%
#     ggplot()+
#     geom_vline(xintercept = 0, lty = 2)+
#     geom_density_ridges(aes(x = difference,
#                             y = comparison,
#                             fill = change, colour = change),
#                         #rel_min_height = 0.001,
#                         alpha = 0.6)+
#     scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#     scale_x_continuous(name = 'contrast (mean per datum predicted)')+
#     facet_grid(. ~ stim_type, scales = 'free_x')+
#     labs(title = 'neighbours at t-1')+
#     theme(axis.text.x = element_text(angle = 90),
#           legend.position = 'bottom'))
# ggsave(plot = prev1, device = 'png',
#        filename = 'neighbour_contrasts_fullscale_prev1.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# 
# dev.off()

#### nearest neighbour -- probability of performing behaviour ####
pdf('../outputs/neighbour_binomial_model_bda/niceplots_nearestneighbour_noprev.pdf')
rm(list = ls()) ; gc() ; load('nearest_neighbour/neighbour_noprev_agecontrasts.RData')
rm(list = ls()[! ls() %in% c('contrasts','contrasts_long','age_nn_org','age_mtx_org','nn')]) ; gc()
print('data loaded')

save.image('nearest_neighbour/neighbour_noprev_agecontrasts_plotting.RData')

## plot predictions
colnames(age_mtx_org) <- 1:nrow(age_nn_org)
age_nn_org$data_id <- 1:nrow(age_nn_org)
pred <- age_mtx_org %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = 'data_id', values_to = 'epred') %>%
  mutate(data_id = as.integer(data_id)) %>%
  left_join(age_nn_org, by = 'data_id') %>%
  mutate(stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
                                 ifelse(stim_type == 'l','lion','human'))) %>%
  mutate(stim_type_long = factor(stim_type_long,
                                 levels = c('dove (control)','lion','human')))
print('data created')

pred %>%
  ggplot()+
  geom_boxplot(aes(x = age_combo,
                   # fill = as.factor(move_index), # successfully predicts actual data
                   fill = f_age_cat,
                   y = epred))+
  labs(x = 'age category combination',
       y = 'predicted probability\nof being neighbours',
       fill = 'neighbours in previous second')+
  facet_wrap(bda ~ stim_type_long)+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
print('boxplots run')

pred %>%
  ggplot()+
  geom_density(aes(x = epred,
                   fill = age_combo),
               alpha = 0.4)+
  labs(fill = 'age category combination',
       x = 'predicted probability\nof being neighbours',
       y = 'probability density')+
  facet_grid(bda ~ stim_type_long,
             scales = 'free')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
print('data loaded')

pred %>%
  separate(age_combo, into = c('f_age_num','p_age_num'), sep = '_', remove = T) %>%
  mutate(f_age_cat = ifelse(f_age_num == 1, '10-15',
                            ifelse(f_age_num == 2, '16-20',
                                   ifelse(f_age_num == 3, '21-25',
                                          '26-35'))),
         p_age_cat = ifelse(p_age_num == 1, '10-15',
                            ifelse(p_age_num == 2, '16-20',
                                   ifelse(p_age_num == 3, '21-25',
                                          '26-35')))) %>%
  mutate(age_combo = paste0('F',f_age_num, '-P',p_age_num),
         age_combo_long = paste0(f_age_cat,' and ',p_age_cat)) %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = age_combo,
                          fill = f_age_cat),
                      alpha = 0.6)+
  labs(fill = 'age category\ncombination',
       x = 'predicted probability\nof being neighbours',
       y = 'probability density')+
  facet_grid(stim_type_long ~ bda,
             scales = 'free_x')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
print('ridges produced')
ggsave(filename = 'nbm_noprev_predicted_ridges.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       device = 'png', height = 3600, width = 2000, units = 'px')
print('ridges saved')

rm(pred) ; gc()

## contrasts
contrasts_long <- contrasts_long %>%
  mutate(comparison = paste0(combo1, '->', combo2),
         stim_type = ifelse(stim_num == 'stim2a', 'dove (control)',
                            ifelse(stim_num == 'stim2b', 'dove (control)',
                                   ifelse(stim_num == 'stim2c', 'dove (control)',
                                          ifelse(stim_num == 'stim15a.old', 'lion',
                                                 ifelse(stim_num == 'stim15b', 'lion',
                                                        ifelse(stim_num == 'stim16b', 'lion',
                                                               ifelse(stim_num < 11, 'dove (control)',
                                                                      ifelse(stim_num < 11, 'dove (control)',
                                                                             ifelse(stim_num < 21, 'lion', 'human')))))))))) %>%
  mutate(stim_type = factor(stim_type, levels = c('dove (control)', 'lion', 'human')))

contrasts_long %>%
  filter(combo1 %in% c(11,12,13,14)) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = combo2),
              alpha = 0.6)+
  scale_fill_viridis_d()+
  facet_grid(bda ~ stim_type)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')
contrasts_long %>%
  filter(combo1 %in% c(21,22,23,24)) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = combo2),
              alpha = 0.6)+
  scale_fill_viridis_d()+
  facet_grid(bda~ stim_type)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')
contrasts_long %>%
  filter(combo1 %in% c(31,32,33,34)) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = combo2),
              alpha = 0.6)+
  scale_fill_viridis_d()+
  facet_grid(bda ~ stim_type)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')
contrasts_long %>%
  filter(combo1 %in% c(41,42,43,44)) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = combo2),
              alpha = 0.6)+
  scale_fill_viridis_d()+
  facet_grid(bda ~ stim_type)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')

contrasts_long <- contrasts_long %>%
  mutate(combo1 = as.integer(combo1),
         combo2 = as.integer(combo2)) %>%
  mutate(change = combo2 - combo1)

contrasts_long %>%
  filter(change %in% c(1,10)) %>%
  filter(combo1 %in% c(11,12,13,14)) %>%
  mutate(change = factor(change, levels = c('partner age increased',
                                            'focal age increased')),
         comparison = factor(comparison,
                             levels = c('11->12','12->13','13->14',
                                        '11->21','12->22','13->23','14->24'))) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = change, colour = change),
              alpha = 0.6)+
  scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
  facet_grid(bda ~ stim_type, scales = 'free_y')+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'bottom')

contrasts_long %>%
  filter(change %in% c(1,10)) %>%
  filter(combo1 %in% c(11,12,13,14)) %>%
  mutate(change = factor(change, levels = c('partner age increased',
                                            'focal age increased')),
         comparison = factor(comparison,
                             levels = c('14->24','13->23','12->22','11->21',
                                        '13->14','12->13','11->12'))) %>%
  ggplot()+
  geom_density_ridges(aes(x = difference,
                          y = comparison,
                          fill = change, colour = change),
                      #rel_min_height = 0.01,
                      alpha = 0.6)+
  scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
  scale_x_continuous(name = 'contrast (mean per datum predicted)',
                     limits = c(-0.015, 0.025))+
  facet_grid(bda ~ stim_type)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'bottom')

plot <- contrasts_long %>%
  filter(change %in% c(1,10)) %>%
  mutate(change = factor(change, levels = c('partner age increased',
                                            'focal age increased')),
         comparison = factor(comparison,
                             levels = c('43->44','42->43','41->42',
                                        '33->34','32->33','31->32',
                                        '34->44','33->43','32->42','31->41',
                                        '23->24','22->23','21->22',
                                        '24->34','23->33','22->32','21->31',
                                        '13->14','12->13','11->12',
                                        '14->24','13->23','12->22','11->21')))
(dove <- plot %>%
    filter(stim_type == 'dove (control)') %>%
    ggplot()+
    geom_density_ridges(aes(x = difference,
                            y = comparison,
                            fill = change, colour = change),
                        #rel_min_height = 0.001,
                        alpha = 0.6)+
    scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
    scale_x_continuous(name = 'contrast (mean per datum predicted)')+
    facet_grid(stim_type ~ bda, scales = 'free_x')+
    labs(title = 'dove (control)')+
    theme(axis.text.x = element_text(angle = 90),
          legend.position = 'bottom'))
ggsave(plot = dove, device = 'png',
       filename = 'neighbour_noprev_contrasts_freescale_ctd.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')
# dove + scale_x_continuous(limits = c(-0.012, 0.012),
#                           name = 'contrast (mean per datum predicted)')
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'neighbour_noprev_contrasts_setscale_ctd.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')

# (lion <- plot %>%
#     filter(stim_type == 'lion') %>%
#     ggplot()+
#     geom_density_ridges(aes(x = difference,
#                             y = comparison,
#                             fill = change, colour = change),
#                         #rel_min_height = 0.001,
#                         alpha = 0.6)+
#     scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#     scale_x_continuous(name = 'contrast (mean per datum predicted)')+
#     facet_grid(. ~ prev, scales = 'free_x')+
#     labs(title = 'lion')+
#     theme(axis.text.x = element_text(angle = 90),
#           legend.position = 'bottom'))
# ggsave(plot = lion, device = 'png',
#        filename = 'neighbour_noprev_contrasts_freescale_l.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# lion + scale_x_continuous(limits = c(-0.02, 0.02),
#                           name = 'contrast (mean per datum predicted)')
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'neighbour_noprev_contrasts_setscale_l.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# 
# (human <- plot %>%
#     filter(stim_type == 'human') %>%
#     ggplot()+
#     geom_density_ridges(aes(x = difference,
#                             y = comparison,
#                             fill = change, colour = change),
#                         #rel_min_height = 0.001,
#                         alpha = 0.6)+
#     scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
#     scale_x_continuous(name = 'contrast (mean per datum predicted)')+
#     facet_grid(. ~ prev, scales = 'free_x')+
#     labs(title = 'human')+
#     theme(axis.text.x = element_text(angle = 90),
#           legend.position = 'bottom'))
# ggsave(plot = human, device = 'png',
#        filename = 'neighbour_noprev_contrasts_freescale_h.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')
# human + scale_x_continuous(limits = c(-0.02, 0.02),
#                           name = 'contrast (mean per datum predicted)')
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'neighbour_noprev_contrasts_setscale_h.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 2100, width = 1800, unit = 'px')

dev.off()
