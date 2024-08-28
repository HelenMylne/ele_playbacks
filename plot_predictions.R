#### information        ####
# looking for a better way to display outputs than purely contrast density plots

#### set up             ####
#library(tidyverse); library(LaplacesDemon) ; library(patchwork); library(ggridges)
library(tidyverse, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')
library(ggridges, lib.loc = '../../packages/')

theme_set(theme_bw())

#### movement binomial  ####
load('movement_direction/movement_binomial_agecontrasts.RData')
rm(ctd_vs_human, ctd_vs_lion, lion_vs_human, contrasts, contrasts_long) ; gc()
rm(age_move_alt, age1_vs_age2, age2_vs_age3, age3_vs_age4, age1_vs_age4) ; gc()

## plot predictions
pred <- pred %>% 
  mutate(draw_id = rep(1:4000, each = nrow(age_move_org)),
         stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
                                 ifelse(stim_type == 'l','lion','human'))) %>% 
  mutate(stim_type_long = factor(stim_type_long,
                                 levels = c('dove (control)','lion','human')))

pred %>% 
  mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving', 'no', 'yes')) %>% 
  mutate(move_tminus1 = factor(move_tminus1, levels = c('yes','no'))) %>% 
  ggplot()+
  geom_boxplot(aes(x = f_age_cat,
                   # fill = as.factor(move_index), # successfully predicts actual data
                   fill = move_tminus1, 
                   y = epred))+
  labs(x = 'focal age category',
       y = 'predicted probability of moving',
       fill = 'moving in previous second')+
  facet_wrap(. ~ stim_type_long)+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')

pred %>% 
  mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving',
                               'not moving at t-1','moving at t-1')) %>% 
  mutate(move_tminus1 = factor(move_tminus1, levels = c('moving at t-1',
                                                        'not moving at t-1'))) %>% 
  ggplot()+
  geom_density(aes(x = epred,
                   fill = f_age_cat),
               alpha = 0.4)+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density')+
  facet_grid(move_tminus1 ~ stim_type_long,
             scales = 'free')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')

pred %>% 
  mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving',
                               'not moving at t-1','moving at t-1')) %>% 
  mutate(move_tminus1 = factor(move_tminus1, levels = c('moving at t-1',
                                                        'not moving at t-1'))) %>% 
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          fill = f_age_cat),
                      alpha = 0.6)+
  labs(fill = 'focal age category',
       x = 'predicted probability of moving',
       y = 'probability density')+
  facet_grid(stim_type_long ~ move_tminus1,
             scales = 'free_x')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
ggsave(filename = 'mbm_predicted_ridges.png',
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
  mutate(prev_move = ifelse(move_tminus1_num == 0, 'not moving at t-1', 'moving at t-1'),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion',
                                   'human'))) %>% 
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = contrast,
                  fill = stim_type),
              position = position_dodge(0.5))+
  geom_hline(yintercept = 0, lty = 2)+
  scale_fill_viridis_d()+
  facet_wrap(. ~ prev_move, scales = 'free_y')+
  labs(fill = 'moving in previous second')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'mbm_boxplot_contrasts.png',
       path = '../outputs/movement_binomial_model/',
       height = 1600, width = 1600, unit = 'px')

#### movement ordinal 1 ####
rm(list = ls()) ; gc() ; load('movement_direction/movement_ordinal_model1_agecontrasts.RData')

## plot predictions
age_move_org$data_id <- 1:nrow(age_move_org)
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
pred_ad <- extract_predictions(array = age_mtx_org, slice = 1, df = age_move_org)
pred_aa <- extract_predictions(array = age_mtx_org, slice = 2, df = age_move_org)
pred_nt <- extract_predictions(array = age_mtx_org, slice = 3, df = age_move_org)
pred_ta <- extract_predictions(array = age_mtx_org, slice = 4, df = age_move_org)
pred_td <- extract_predictions(array = age_mtx_org, slice = 5, df = age_move_org)

boxplot_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
                                ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
                                       ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
                                              ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
                                                     'approach directly'))))
  
  pred %>% 
    mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
                                 ifelse(move_tminus1_num == 2, 'move away at an angle',
                                        ifelse(move_tminus1_num == 3, 'move neither towards or away',
                                               ifelse(move_tminus1_num == 4,
                                                      'approach at an angle',
                                                      'approach directly')))),
           f_age_cat = ifelse(f_age_num == 1, '10-15 years',
                              ifelse(f_age_num == 2, '16-20 years',
                                     ifelse(f_age_num == 3, '21-25 years',
                                            '26-35 years')))) %>% 
    mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
                                                          'move away at an angle',
                                                          'move neither towards or away',
                                                          'approach at an angle',
                                                          'approach directly'))) %>% 
    ggplot()+
    geom_boxplot(aes(x = f_age_cat,
                     # fill = as.factor(move_index), # successfully predicts actual data
                     fill = move_tminus1, 
                     y = epred))+
    labs(x = 'focal age category',
         y = 'predicted probability of moving direction',
         fill = 'movement in previous second',
         title = predicted_direction)+
    facet_grid(move_tminus1 ~ stim_type_long)+ # looks a bit rubbish, but freeing y then gives crazy scales. I ideally want one where I can limit the y to 0-0.2 for all but move_tminus1 = prediction_direction which can then go 0.8-1
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
  
}
(box_ad <- boxplot_pred(pred_ad))
(box_aa <- boxplot_pred(pred_aa))
(box_nt <- boxplot_pred(pred_nt))
(box_ta <- boxplot_pred(pred_ta))
(box_td <- boxplot_pred(pred_td))

density_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
                                ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
                                       ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
                                              ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
                                                     'approach directly'))))
  
  pred %>% 
    mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
                                 ifelse(move_tminus1_num == 2, 'move away at an angle',
                                        ifelse(move_tminus1_num == 3, 'move neither towards or away',
                                               ifelse(move_tminus1_num == 4,
                                                      'approach at an angle',
                                                      'approach directly')))),
           f_age_cat = ifelse(f_age_num == 1, '10-15 years',
                              ifelse(f_age_num == 2, '16-20 years',
                                     ifelse(f_age_num == 3, '21-25 years',
                                            '26-35 years')))) %>% 
    mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
                                                          'move away at an angle',
                                                          'move neither towards or away',
                                                          'approach at an angle',
                                                          'approach directly'))) %>% 
    ggplot()+
    geom_density(aes(x = epred,
                     fill = f_age_cat),
                 alpha = 0.4)+
    labs(fill = 'focal age category',
         x = 'predicted probability of moving',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(move_tminus1 ~ stim_type_long,
               scales = 'free')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom')
  
}
(dens_ad <- density_pred(pred_ad))
(dens_aa <- density_pred(pred_aa))
(dens_nt <- density_pred(pred_nt))
(dens_ta <- density_pred(pred_ta))
(dens_td <- density_pred(pred_td))

ridge_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
                                ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
                                       ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
                                              ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
                                                     'approach directly'))))
  
  pred %>% 
    mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
                                 ifelse(move_tminus1_num == 2, 'move away at an angle',
                                        ifelse(move_tminus1_num == 3, 'neither towards or away',
                                               ifelse(move_tminus1_num == 4,
                                                      'approach at an angle',
                                                      'approach directly')))),
           f_age_cat = ifelse(f_age_num == 1, '10-15 years',
                              ifelse(f_age_num == 2, '16-20 years',
                                     ifelse(f_age_num == 3, '21-25 years',
                                            '26-35 years')))) %>% 
    mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
                                                          'move away at an angle',
                                                          'neither towards or away',
                                                          'approach at an angle',
                                                          'approach directly'))) %>% 
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = stim_type_long),
                        alpha = 0.6,
                        rel_min_height = )+
    labs(fill = 'focal age category',
         x = 'predicted probability of moving direction',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(stim_type_long ~ move_tminus1,
               scales = 'free_x')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 45))
}
(ridge_ad <- ridge_pred(pred_ad))
(ridge_aa <- ridge_pred(pred_aa))
(ridge_nt <- ridge_pred(pred_nt))
(ridge_ta <- ridge_pred(pred_ta))
(ridge_td <- ridge_pred(pred_td))

ggsave(filename = 'mom1_predicted_ridges_directlyaway.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_ad,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'mom1_predicted_ridges_angleaway.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_aa,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'mom1_predicted_ridges_neither.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_nt,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'mom1_predicted_ridges_angleapproach.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_ta,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'mom1_predicted_ridges_directlyapproach.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_td,
       device = 'png', height = 1800, width = 2700, units = 'px')

## plot
contrast_plot <- function(contrasts, direction, free_y = T){
  contrasts %>% 
    filter(move_pred == direction) %>% 
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
                              ifelse(stim_type == 'l', 'lion', 'human')),
           move_tminus1 = ifelse(move_tminus1 == 'move away directly',
                                 'move away directly',
                                 ifelse(move_tminus1 == 'move away at an angle',
                                        'move away at an angle',
                                        ifelse(move_tminus1 == 'move directly with',
                                               'neither towards or away',
                                               ifelse(move_tminus1 == 'approach at an angle',
                                                      'approach at an angle',
                                                      'approach directly'))))) %>% 
    mutate(move_tminus1 = factor(move_tminus1,
                                 levels = c('move away directly',
                                            'move away at an angle',
                                            'neither towards or away',
                                            'approach at an angle',
                                            'approach directly'))) %>% 
    ggplot()+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = comparison),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(move_tminus1 ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed')
               )+
    labs(fill = 'comparison',
         title = ifelse(direction == 'awayangle',
                            'away at an angle',
                            ifelse(direction == 'awaydirect',
                                   'away directly',
                                   ifelse(direction == 'neither',
                                          'neither towards or away',
                                          ifelse(direction == 'twdsangle',
                                                 'towards at an angle',
                                                 'towards directly')))))+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
}
(contrasts_ad_fixed <- contrast_plot(contrasts_long, 'awaydirect', free_y = F))
(contrasts_aa_fixed <- contrast_plot(contrasts_long, 'awayangle', free_y = F))
(contrasts_nt_fixed <- contrast_plot(contrasts_long, 'neither', free_y = F))
(contrasts_ta_fixed <- contrast_plot(contrasts_long, 'twdsangle', free_y = F))
(contrasts_td_fixed <- contrast_plot(contrasts_long, 'twdsdirect', free_y = F))

(contrasts_ad_free <- contrast_plot(contrasts_long, 'awaydirect', free_y = T))
(contrasts_aa_free <- contrast_plot(contrasts_long, 'awayangle', free_y = T))
(contrasts_nt_free <- contrast_plot(contrasts_long, 'neither', free_y = T))
(contrasts_ta_free <- contrast_plot(contrasts_long, 'twdsangle', free_y = T))
(contrasts_td_free <- contrast_plot(contrasts_long, 'twdsdirect', free_y = T))

for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
  contrast_plot(contrasts_long, plot, free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom1_contrasts_fixedscales_',plot,'.png'),
         path = '../outputs/movement_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
}
for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
  contrast_plot(contrasts_long, plot, free_y = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom1_contrasts_freescales_',plot,'.png'),
         path = '../outputs/movement_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
}

#### movement ordinal 2 ####
rm(list = ls()) ; gc() ; load('movement_direction/moving_ordinal_2bda_agecontrasts.RData')

# ## plot predictions
# ggplot(data = pred[pred$move_index == 1,])+
#   geom_boxplot(aes(x = f_age_cat,
#                    y = epred))
rm(pred) ; gc()

## plot
contrasts_long %>% 
  rename(f_age_cat_org = f_age_cat,
         f_age_num_org = f_age_num) %>% 
  mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                f_age_num_org + 1)) %>% 
  mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                ifelse(f_age_num_alt == 2, '16-20 yrs',
                                       ifelse(f_age_num_alt == 3, '21-25 yrs',
                                              '10-15 yrs')))) %>% 
  mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human')),
         move_pred = ifelse(move_pred == 'awayangle',
                            'away at an angle',
                            ifelse(move_pred == 'awaydirect',
                                   'away directly',
                                   ifelse(move_pred == 'neither',
                                          'neither towards nor away',
                                          ifelse(move_pred == 'twdsangle',
                                                 'towards at an angle',
                                                 'towards directly'))))) %>% 
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = move_tminus1),
              position = position_dodge(0.5))+
  scale_fill_viridis_d()+
  facet_grid(move_pred ~ stim_type)+
  labs(fill = 'behaviour in previous second')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'mom2_boxplot_contrasts.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       height = 800, width = 800, unit = 'px')

#### looking ordinal 1  ####
rm(list = ls()) ; gc() ; load('looking_direction/looking_ordinal_model1_agecontrasts.RData')

#### looking ordinal 2  ####
rm(list = ls()) ; gc() ; load('looking_direction/looking_ordinal_model2bda_agecontrasts.RData')

#### nearest neighbour  ####
rm(list = ls()) ; gc() ; load('movement_direction/moving_ordinal_2bda_agecontrasts.RData')
