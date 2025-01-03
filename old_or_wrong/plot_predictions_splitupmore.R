#### information        ####
# looking for a better way to display outputs than purely contrast density plots

#### set up             ####
#library(tidyverse); library(LaplacesDemon) ; library(patchwork); library(ggridges)
library(tidyverse, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')
library(ggridges, lib.loc = '../../packages/')

theme_set(theme_bw())

#### movement ordinal 1 ####
pdf('../outputs/movement_ordinal_model_1/niceplots_movementordinal1_splitpartner.pdf')
rm(list = ls()) ; gc() ; load('movement_direction/ordinal_withprev/movement_ordinal_model1_agecontrasts.RData')

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

  pred2 <- pred %>%
    mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
                                 ifelse(move_tminus1_num == 2, 'move away at an angle',
                                        ifelse(move_tminus1_num == 3, 'neither towards or away',
                                               ifelse(move_tminus1_num == 4,
                                                      'approach at an angle',
                                                      'approach directly')))),
           f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                              ifelse(f_age_num == 2, '16-20 yrs',
                                     ifelse(f_age_num == 3, '21-25 yrs',
                                            '26-35 yrs'))),
           p_age_cat = ifelse(p_age_num == 1, '10-15 yrs',
                              ifelse(p_age_num == 2, '16-20 yrs',
                                     ifelse(p_age_num == 3, '21-25 yrs',
                                            '26-35 yrs')))) %>%
    mutate(move_tminus1 = factor(move_tminus1, levels = c('move directly away',
                                                          'move away at an angle',
                                                          'neither towards or away',
                                                          'approach at an angle',
                                                          'approach directly')))
  pred2 %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6)+
    labs(fill = 'target age category',
         x = 'predicted probability of moving direction',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(stim_type_long ~ move_tminus1,
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

ggsave(filename = 'mom1_predicted_ridges_directlyaway_splitpartner.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_ad,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom1_predicted_ridges_angleaway_splitpartner.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_aa,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom1_predicted_ridges_neither_splitpartner.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_nt,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom1_predicted_ridges_angleapproach_splitpartner.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_ta,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom1_predicted_ridges_directlyapproach_splitpartner.png',
       path = '../outputs/movement_ordinal_model_1/',
       plot = ridge_td,
       device = 'png', height = 1800, width = 2800, units = 'px')

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
    geom_hline(yintercept = 0, lty = 2)+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = p_age_cat),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(move_tminus1 ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed')
               )+
    labs(fill = 'target age',
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
for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
  contrast_plot(contrasts_long, plot, free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom1_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
}
for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
  contrast_plot(contrasts_long, plot, free_y = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom1_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
}

print('movement ordinal 1 complete')
dev.off()

#### movement ordinal 2 -- probability of changing behaviour ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_movementordinal2_splitpartner.pdf')
rm(list = ls()) ; gc() ; load('movement_direction/ordinal_withprev/moving_ordinal_2bda_agecontrasts.RData')
rm(age1v2_aa,age1v2_ad,age1v2_n,age1v2_ta,age1v2_td,
   age1v4_aa,age1v4_ad,age1v4_n,age1v4_ta,age1v4_td,
   age2v3_aa,age2v3_ad,age2v3_n,age2v3_ta,age2v3_td,
   age3v4_aa,age3v4_ad,age3v4_n,age3v4_ta,age3v4_td) ; gc()
rm(age_move_alt,age_new,age_pred,alt_vs_org_awayangle,alt_vs_org_awaydirect,alt_vs_org_neither,alt_vs_org_twdsangle,alt_vs_org_twdsdirect,behav,contrasts) ; gc()
save.image('movement_direction/ordinal_withprev/moving_ordinal_2bda_ageplotting.RData')
#load('movement_direction/ordinal_withprev/moving_ordinal_2bda_ageplotting.RData')

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
    mutate(move_tminus1 = ifelse(prev_num == 1, 'move directly away',
                                 ifelse(prev_num == 2, 'move away at an angle',
                                        ifelse(prev_num == 3, 'move neither towards or away',
                                               ifelse(prev_num == 4,
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
    mutate(move_tminus1 = ifelse(prev_num == 1, 'move directly away',
                                 ifelse(prev_num == 2, 'move away at an angle',
                                        ifelse(prev_num == 3, 'move neither towards or away',
                                               ifelse(prev_num == 4,
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
    mutate(move_tminus1 = ifelse(prev_num == 1, 'move directly away',
                                 ifelse(prev_num == 2, 'move away at an angle',
                                        ifelse(prev_num == 3, 'neither towards or away',
                                               ifelse(prev_num == 4,
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
    separate(age_combo, into = c('f','p_age_cat'), remove = F) %>%
    mutate(p_age_cat = ifelse(p_age_cat == '1', '10-15 yrs',
                              ifelse(p_age_cat == '2', '16-20 yrs',
                                     ifelse(p_age_cat == '3', '21-25 yrs', '26-35 yrs')))) %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6)+
    labs(fill = 'target age category',
         x = 'predicted probability of moving direction',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(stim_type_long ~ move_tminus1,
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

ggsave(filename = 'mom2_predicted_ridges_directlyaway_splitpartner.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_ad,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom2_predicted_ridges_angleaway_splitpartner.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_aa,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom2_predicted_ridges_neither_splitpartner.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_nt,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom2_predicted_ridges_angleapproach_splitpartner.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_ta,
       device = 'png', height = 1800, width = 2800, units = 'px')
ggsave(filename = 'mom2_predicted_ridges_directlyapproach_splitpartner.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       plot = ridge_td,
       device = 'png', height = 1800, width = 2800, units = 'px')

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
           prev_action = ifelse(prev_action == 'move away directly',
                                 'move away directly',
                                 ifelse(prev_action == 'move away at an angle',
                                        'move away at an angle',
                                        ifelse(prev_action == 'move directly with',
                                               'neither towards or away',
                                               ifelse(prev_action == 'approach at an angle',
                                                      'approach at an angle',
                                                      'approach directly'))))) %>%
    mutate(prev_action = factor(prev_action,
                                 levels = c('move away directly',
                                            'move away at an angle',
                                            'neither towards or away',
                                            'approach at an angle',
                                            'approach directly'))) %>%
    ggplot()+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = p_age_cat),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(prev_action ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed')
    )+
    labs(fill = 'target age',
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
         filename = paste0('mom2_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}
for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
  contrast_plot(contrasts_long, plot, free_y = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom2_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}

print('movement ordinal 2 complete')
dev.off()

#### looking ordinal 1  ####
pdf('../outputs/looking_ordinal_model_1/niceplots_lookingordinal1_splitpartner.pdf')
rm(list = ls()) ; gc() ; load('looking_direction/looking_ordinal_model1_agecontrasts.RData')
rm(age_new, age_pred, age_pred_all, age_pred_i, alt_vs_org_away, alt_vs_org_side, alt_vs_org_twds, away_12, away_14, away_23, away_34, plot, side_12, side_14, side_23, side_34, twds_12, twds_23, twds_14, twds_34) ; gc()
save.image('looking_direction/looking_ordinal_model1_ageplotting_splitpartner.RData') # load('looking_direction/looking_ordinal_model1_ageplotting_splitpartner.RData')

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

str(pred_a)

boxplot_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                       'look at'))

  pred %>%
    mutate(look_tminus1 = ifelse(look_tminus1_num == 1, 'look away',
                                 ifelse(look_tminus1_num == 2, 'side-on',
                                        'look at')),
           f_age_cat = ifelse(f_age_num == 1, '10-15 years',
                              ifelse(f_age_num == 2, '16-20 years',
                                     ifelse(f_age_num == 3, '21-25 years',
                                            '26-35 years')))) %>%
    mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
                                                          'side-on',
                                                          'look at'))) %>%
    ggplot()+
    geom_boxplot(aes(x = f_age_cat,
                     # fill = as.factor(move_index), # successfully predicts actual data
                     fill = look_tminus1,
                     y = epred))+
    labs(x = 'focal age category',
         y = 'predicted probability of looking direction',
         fill = 'looking in previous second',
         title = predicted_direction)+
    facet_grid(look_tminus1 ~ stim_type_long)+ # looks a bit rubbish, but freeing y then gives crazy scales. I ideally want one where I can limit the y to 0-0.2 for all but move_tminus1 = prediction_direction which can then go 0.8-1
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
                                       'look at'))

  pred %>%
    mutate(look_tminus1 = ifelse(look_tminus1_num == 1, 'look away',
                                 ifelse(look_tminus1_num == 2, 'side-on',
                                        'look at')),
           f_age_cat = ifelse(f_age_num == 1, '10-15 years',
                              ifelse(f_age_num == 2, '16-20 years',
                                     ifelse(f_age_num == 3, '21-25 years',
                                            '26-35 years')))) %>%
    mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
                                                          'side-on',
                                                          'look at'))) %>%
    ggplot()+
    geom_density(aes(x = epred,
                     fill = f_age_cat),
                 alpha = 0.4)+
    labs(fill = 'focal age category',
         x = 'predicted probability of looking',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(look_tminus1 ~ stim_type_long,
               scales = 'free')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom')

}
(dens_a <- density_pred(pred_a))
(dens_s <- density_pred(pred_s))
(dens_t <- density_pred(pred_t))

ridge_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                       'look at'))

  pred %>%
    separate(age_combo, into = c('f','p_age_num'), remove = F) %>%
    mutate(p_age_num = as.numeric(p_age_num)) %>%
    mutate(look_tminus1 = ifelse(look_tminus1_num == 1, 'look away',
                                 ifelse(look_tminus1_num == 2, 'side-on',
                                        'look at')),
           f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                              ifelse(f_age_num == 2, '16-20 yrs',
                                     ifelse(f_age_num == 3, '21-25 yrs',
                                            '26-35 yrs'))),
           p_age_cat = ifelse(p_age_num == 1, '10-15 yrs',
                              ifelse(p_age_num == 2, '16-20 yrs',
                                     ifelse(p_age_num == 3, '21-25 yrs',
                                            '26-35 yrs')))) %>%
    mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
                                                          'side-on',
                                                          'look at'))) %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6)+
    labs(fill = 'target age category',
         x = 'predicted probability of looking direction',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(stim_type_long ~ look_tminus1,
               scales = 'free_x')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
}
(ridge_a <- ridge_pred(pred_a))
(ridge_s <- ridge_pred(pred_s))
(ridge_t <- ridge_pred(pred_t))

ggsave(filename = 'lom1_predicted_ridges_away_splitpartner.png',
       path = '../outputs/looking_ordinal_model_1/',
       plot = ridge_a,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'lom1_predicted_ridges_sideon_splitpartner.png',
       path = '../outputs/looking_ordinal_model_1/',
       plot = ridge_s,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'lom1_predicted_ridges_towards_splitpartner.png',
       path = '../outputs/looking_ordinal_model_1/',
       plot = ridge_t,
       device = 'png', height = 1800, width = 2700, units = 'px')

## plot
contrast_plot <- function(contrasts, direction, free_y = T){
  contrasts %>%
    filter(pred_type == direction) %>%
    rename(f_age_num_alt = f_age_new,
           f_age_num_org = f_age_num) %>%
    separate(age_combo, into = c('f','p_age_num'), remove = F) %>%
    mutate(p_age_num = as.numeric(p_age_num)) %>%
    mutate(f_age_cat_org = ifelse(f_age_num_org == 1,'10-15 yrs',
                                  ifelse(f_age_num_org == 2, '16-20 yrs',
                                         ifelse(f_age_num_org == 3, '21-25 yrs',
                                                '26-35 yrs'))),
           f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                  ifelse(f_age_num_alt == 2, '16-20 yrs',
                                         ifelse(f_age_num_alt == 3, '21-25 yrs',
                                                '26-35 yrs'))),
           p_age_cat = ifelse(p_age_num == 1,'10-15 yrs',
                              ifelse(p_age_num == 2, '16-20 yrs',
                                     ifelse(p_age_num == 3, '21-25 yrs',
                                            '26-35 yrs')))) %>%
    mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
           stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                              ifelse(stim_type == 'l', 'lion', 'human'))) %>%
    mutate(look_tminus1 = factor(look_tminus1,
                                 levels = c('look away at t-1',
                                            'side on at t-1',
                                            'look at at t-1'))) %>%
    ggplot()+
    geom_hline(yintercept = 0, lty = 2)+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = p_age_cat),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(look_tminus1 ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed')
    )+
    labs(fill = 'target age',
         title = direction)+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
}
for(plot in c('look away','side on','look at')){
  contrast_plot(contrasts = contrasts_long, direction = plot, free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom1_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
}
for(plot in c('look away','side on','look at')){
  contrast_plot(contrasts_long, plot, free_y = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom1_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
}

print('looking ordinal 1 complete')
dev.off()

#### looking ordinal 2 -- probability of changing behaviour  ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_lookingordinal2_splitpartner.pdf')
rm(list = ls()) ; gc() ; load('looking_direction/looking_ordinal_model2bda_agecontrasts.RData')
rm(list = ls()[! ls() %in% c('age_look_org','contrasts','contrasts_long','look','age_mtx_org')]) ; gc()
save.image('looking_direction/looking_ordinal_model2bda_agecontrasts_plotting_splitpartner.RData') # load('looking_direction/looking_ordinal_model2bda_agecontrasts_plotting_splitpartner.RData')

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

str(pred_a)

boxplot_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                       'look at'))

  pred %>%
    mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
                                 ifelse(prev_num == 2, 'side-on',
                                        'look at')),
           f_age_cat = ifelse(f_age_num == 1, '10-15 years',
                              ifelse(f_age_num == 2, '16-20 years',
                                     ifelse(f_age_num == 3, '21-25 years',
                                            '26-35 years')))) %>%
    mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
                                                          'side-on',
                                                          'look at'))) %>%
    ggplot()+
    geom_boxplot(aes(x = f_age_cat,
                     # fill = as.factor(look_index), # successfully predicts actual data
                     fill = look_tminus1,
                     y = epred))+
    labs(x = 'focal age category',
         y = 'predicted probability of looking direction',
         fill = 'looking in previous second',
         title = predicted_direction)+
    facet_grid(look_tminus1 ~ stim_type_long)+ # looks a bit rubbish, but freeing y then gives crazy scales. I ideally want one where I can limit the y to 0-0.2 for all but look_tminus1 = prediction_direction which can then go 0.8-1
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))

}
(box_a <- boxplot_pred(pred_a))
(box_s <- boxplot_pred(pred_s))
(box_t <- boxplot_pred(pred_t))
print('boxplots done')

density_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                       ifelse(pred$predicted_direction[1] == 3, 'look at')))

  graph <- pred %>%
    mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
                                 ifelse(prev_num == 2, 'side-on',
                                        'look at')),
           f_age_cat = ifelse(f_age_num == 1, '10-15 years',
                              ifelse(f_age_num == 2, '16-20 years',
                                     ifelse(f_age_num == 3, '21-25 years',
                                            '26-35 years')))) %>%
    mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
                                                          'side-on',
                                                          'look at'))) %>%
    ggplot()+
    geom_density(aes(x = epred,
                     fill = f_age_cat),
                 alpha = 0.4)+
    labs(fill = 'focal age category',
         x = 'predicted probability of looking',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(look_tminus1 ~ stim_type_long,
               scales = 'free')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom')
  return(graph)
}
(dens_a <- density_pred(pred_a))
(dens_s <- density_pred(pred_s))
(dens_t <- density_pred(pred_t))
print('density plots done')

ridge_pred <- function(pred){
  predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                       'look at'))

  pred %>%
    mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
                                 ifelse(prev_num == 2, 'side-on',
                                        'look at')),
           f_age_cat = ifelse(f_age_num == 1, '10-15 years',
                              ifelse(f_age_num == 2, '16-20 years',
                                     ifelse(f_age_num == 3, '21-25 years',
                                            '26-35 years')))) %>%
    mutate(look_tminus1 = factor(look_tminus1, levels = c('look away',
                                                          'side-on',
                                                          'look at'))) %>%
    separate(age_combo, into = c('f','p_age_cat'), remove = F) %>%
    mutate(p_age_cat = ifelse(p_age_cat == '1', '10-15 yrs',
                              ifelse(p_age_cat == '2', '16-20 yrs',
                                     ifelse(p_age_cat == '3', '21-25 yrs', '26-35 yrs')))) %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6)+
    labs(fill = 'target age category',
         x = 'predicted probability of looking direction',
         y = 'probability density',
         title = predicted_direction)+
    facet_grid(stim_type_long ~ look_tminus1,
               scales = 'free_x')+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
}
(ridge_a <- ridge_pred(pred_a))
(ridge_s <- ridge_pred(pred_s))
(ridge_t <- ridge_pred(pred_t))
print('ridge plots done')

ggsave(filename = 'lom2_predicted_ridges_away_splitpartner.png',
       path = '../outputs/looking_ordinal_model_2bda/',
       plot = ridge_a,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'lom2_predicted_ridges_sideon_splitpartner.png',
       path = '../outputs/looking_ordinal_model_2bda/',
       plot = ridge_s,
       device = 'png', height = 1800, width = 2700, units = 'px')
ggsave(filename = 'lom2_predicted_ridges_towards_splitpartner.png',
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
    mutate(prev_action = factor(prev_action,
                                 levels = c('look directly away',
                                            'side-on',
                                            'look at directly')),
           stim_type = factor(stim_type,
                                levels = c('dove (control)',
                                           'lion',
                                           'human'))) %>%
    ggplot()+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = p_age_cat),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(prev_action ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed')
    )+
    labs(fill = 'target age',
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
         filename = paste0('lom2_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}
for(plot in c('away','side','twds')){
  contrast_plot(contrasts_long, plot, free_y = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom2_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}

print('looking ordinal 2 complete')
dev.off()
