#### information        ####
# looking for a better way to display outputs than purely contrast density plots

#### set up             ####
#library(tidyverse); library(LaplacesDemon) ; library(patchwork); library(ggridges)
library(tidyverse, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')
library(patchwork, lib.loc = '../../packages/')
library(ggridges, lib.loc = '../../packages/')

theme_set(theme_bw())

#### create function for extracting predictions ####
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

# #### create ridge plotting functions            ####
# ridge_pred_altogether <- function(pred_df, type, prev, prediction){
#   if(type == 'move_prob'){
#     if(prev == TRUE){
#       pred_df <- pred_df %>%
#         mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving',
#                                      'not moving at t-1','moving at t-1')) %>%
#         mutate(prev_action = factor(move_tminus1, levels = c('moving at t-1',
#                                                              'not moving at t-1')))
#     }
#   }
#   
#   if(type == 'move_dir'){
#     if(prev == TRUE){
#       pred_df <- pred_df %>%
#         mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
#                                      ifelse(move_tminus1_num == 2, 'move away at an angle',
#                                             ifelse(move_tminus1_num == 3, 'neither towards or away',
#                                                    ifelse(move_tminus1_num == 4,
#                                                           'approach at an angle',
#                                                           'approach directly'))))) %>%
#         mutate(prev_action = factor(move_tminus1, levels = c('move directly away',
#                                                              'move away at an angle',
#                                                              'neither towards or away',
#                                                              'approach at an angle',
#                                                              'approach directly')))
#     }
#   }
#   
#   if(type == 'look'){
#     if(prev == TRUE){
#       pred_df <- pred_df %>%
#         mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
#                                      ifelse(prev_num == 2, 'side-on',
#                                             'look towards'))) %>%
#         mutate(prev_action = factor(look_tminus1, levels = c('look away',
#                                                              'side-on',
#                                                              'look towards')))
#     }
#   }
#   
#   pred_df <- pred_df %>% 
#     mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
#                               ifelse(f_age_num == 2, '16-20 yrs',
#                                      ifelse(f_age_num == 3, '21-25 yrs',
#                                             '26-35 yrs'))))
#   plot <- pred_df %>%
#     ggplot()+
#     geom_density_ridges(aes(x = epred,
#                             y = f_age_cat,
#                             fill = f_age_cat),
#                         alpha = 0.6,
#                         scale = 0.9)+
#     labs(fill = 'target age category',
#          x = 'predicted probability',
#          y = 'probability density',
#          title = prediction)+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
#   
#   if(prev == TRUE){
#     plot <- plot + facet_grid(prev_action ~ stim_type,
#                       scales = 'free_x')
#   }
#   if(prev == FALSE){
#     plot <- plot + facet_grid(. ~ stim_type,
#                       scales = 'free_x')
#   }
#   return(plot)
# }
# 
# ridge_pred_splitpartner <- function(pred, type, prev){
#   if(type == 'move_dir'){
#     predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                   ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                          ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                                 ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                        'approach directly'))))
#     if(prev == TRUE){
#       pred <- pred %>%
#         mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
#                                      ifelse(move_tminus1_num == 2, 'move away at an angle',
#                                             ifelse(move_tminus1_num == 3, 'neither towards or away',
#                                                    ifelse(move_tminus1_num == 4,
#                                                           'approach at an angle',
#                                                           'approach directly'))))) %>%
#         mutate(prev_action = factor(move_tminus1, levels = c('move directly away',
#                                                              'move away at an angle',
#                                                              'neither towards or away',
#                                                              'approach at an angle',
#                                                              'approach directly')))
#     }
#   }
#   
#   if(type == 'look'){
#     predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
#                                   ifelse(pred$predicted_direction[1] == 2, 'side-on',
#                                          'look towards'))
#     if(prev == TRUE){
#       pred <- pred %>%
#         mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
#                                      ifelse(prev_num == 2, 'side-on',
#                                             'look towards'))) %>%
#         mutate(prev_action = factor(look_tminus1, levels = c('look away',
#                                                              'side-on',
#                                                              'look towards')))
#     }
#   }
#   
#   pred <- pred %>% 
#     mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
#                               ifelse(f_age_num == 2, '16-20 yrs',
#                                      ifelse(f_age_num == 3, '21-25 yrs',
#                                             '26-35 yrs')))) %>% 
#     separate(age_combo, into = c('f','p_age_cat'), remove = F) %>%
#     mutate(p_age_cat = ifelse(p_age_cat == '1', '10-15 yrs',
#                               ifelse(p_age_cat == '2', '16-20 yrs',
#                                      ifelse(p_age_cat == '3', '21-25 yrs', '26-35 yrs')))) %>% 
#     mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                               ifelse(stim_type == 'l', 'lion', 'human'))) %>% 
#     mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human')))
#   plot <- pred %>%
#     ggplot()+
#     geom_density_ridges(aes(x = epred,
#                             y = f_age_cat,
#                             fill = p_age_cat),
#                         alpha = 0.6,
#                         scale = 0.9)+
#     labs(fill = 'target age category',
#          x = 'predicted probability',
#          y = 'probability density',
#          title = predicted_direction)+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
#   
#   if(prev == TRUE){
#     plot <- plot + facet_grid(prev_action ~ stim_type,
#                       scales = 'free_x')
#   }
#   if(prev == FALSE){
#     plot <- plot + facet_grid(. ~ stim_type,
#                       scales = 'free_x')
#   }
#   return(plot)
# }
# 
#### create contrast plotting functions         ####
contrast_plot_altogether <- function(contrasts, type, direction, #prev,
                                     free_y){
  if(type == 'move'){
    contrasts <- contrasts %>%
      filter(move_pred == direction)
    
    # if(prev == TRUE){
    #   contrasts <- contrasts %>%
    #     mutate(prev_action = ifelse(move_tminus1_num == 1, 'move away directly',
    #                                 ifelse(move_tminus1_num == 2,'move away at an angle',
    #                                        ifelse(move_tminus1_num == 3, 'neither towards or away',
    #                                               ifelse(move_tminus1_num == 4,
    #                                                      'approach at an angle',
    #                                                      'approach directly'))))) %>%
    #     mutate(prev_action = factor(prev_action,
    #                                 levels = c('move away directly',
    #                                            'move away at an angle',
    #                                            'neither towards or away',
    #                                            'approach at an angle',
    #                                            'approach directly')))
    # }
    plot_title <- ifelse(direction == 'awayangle',
                         'away at an angle',
                         ifelse(direction == 'awaydirect',
                                'away directly',
                                ifelse(direction == 'neither',
                                       'neither towards or away',
                                       ifelse(direction == 'twdsangle',
                                              'towards at an angle',
                                              'towards directly'))))
  }
  
  if(type == 'look'){
    contrasts <- contrasts %>%
      filter(look_pred == direction)
    # if(prev == TRUE){
    #   contrasts <- contrasts %>%
    #     mutate(prev_action = factor(prev_action,
    #                                 levels = c('look directly away',
    #                                            'side-on',
    #                                            'look at directly')))
    # }
    plot_title <- ifelse(direction == 'away', 'look away',
                         ifelse(direction == 'side', 'side-on',
                                'look towards'))
  }
  
  plot <- contrasts %>%
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
                              ifelse(stim_type == 'l', 'lion', 'human'))) %>% 
    mutate(stim_type = factor(stim_type,
                              levels = c('dove (control)',
                                         'lion',
                                         'human')),
           bda = factor(bda, levels = c('before','during','after'))) %>%
    ggplot()+
    geom_hline(yintercept = 0, lty = 3)+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = comparison),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    labs(fill = 'comparison',
         title = plot_title)+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))+
    facet_grid(bda ~ stim_type)
  
  # if(prev == TRUE){
  #   plot <- plot + facet_grid(prev_action ~ stim_type,
  #              scales = ifelse(free_y == T, 'free_y', 'fixed'))
  # }
  # if(prev == FALSE){
  #   plot <- plot + facet_grid(. ~ stim_type,
  #              scales = ifelse(free_y == T, 'free_y', 'fixed'))
  # }
  return(plot)
}

contrast_plot_splitpartner <- function(contrasts, type, direction, #prev, 
                                       free_y){
  if(type == 'move'){
    contrasts <- contrasts %>%
      filter(move_pred == direction)
    # if(prev == TRUE){
    #   contrasts <- contrasts %>%
    #     mutate(prev_action = ifelse(move_tminus1 == 'move away directly',
    #                                 'move away directly',
    #                                 ifelse(move_tminus1 == 'move away at an angle',
    #                                        'move away at an angle',
    #                                        ifelse(move_tminus1 == 'move directly with',
    #                                               'neither towards or away',
    #                                               ifelse(move_tminus1 == 'approach at an angle',
    #                                                      'approach at an angle',
    #                                                      'approach directly'))))) %>%
    #     mutate(prev_action = factor(prev_action,
    #                                 levels = c('move away directly',
    #                                            'move away at an angle',
    #                                            'neither towards or away',
    #                                            'approach at an angle',
    #                                            'approach directly')))
    # }
    plot_title <- ifelse(direction == 'awayangle',
                         'away at an angle',
                         ifelse(direction == 'awaydirect',
                                'away directly',
                                ifelse(direction == 'neither',
                                       'neither towards or away',
                                       ifelse(direction == 'twdsangle',
                                              'towards at an angle',
                                              'towards directly'))))
  }
  
  if(type == 'look'){
    contrasts <- contrasts %>%
      filter(look_pred == direction)
    # if(prev == TRUE){
    #   contrasts <- contrasts %>%
    #     mutate(prev_action = factor(prev_action,
    #                                 levels = c('look directly away',
    #                                            'side-on',
    #                                            'look at directly')))
    # }
    plot_title <- ifelse(direction == 'away', 'look away',
                         ifelse(direction == 'side', 'side-on',
                                'look towards'))
  }
  
  plot <- contrasts %>%
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
           bda = factor(bda, levels = c('before','during','after'))) %>%
    ggplot()+
    geom_hline(yintercept = 0, lty = 3)+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = comparison),
                position = position_dodge(0.5))+
    scale_fill_viridis_d()+
    facet_grid(p_age_cat ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed'))+
    labs(fill = 'comparison',
         title = plot_title)+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
  return(plot)
}

#### movement ordinal   ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_movementordinal2_noprev.pdf')
rm(list = ls()[!ls() %in% c('extract_predictions',
                            'ridge_pred_altogether','ridge_pred_splitpartner',
                            'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
load('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')

## plot predictions
dir <- c('move directly away','move away at an angle','move neither towards or away','approach at an angle','approach directly')

for(prediction_direction in 1:5){
  pred_slice <- pred %>%
    filter(pred_type == dir[prediction_direction]) %>%
    separate(age_combo, into = c('f','p_age_cat'), remove = F) %>%
    mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                              ifelse(f_age_num == 2, '16-20 yrs',
                                     ifelse(f_age_num == 3, '21-25 yrs',
                                            '26-35 yrs'))),
           p_age_cat = ifelse(p_age_cat == '1', '10-15 yrs',
                              ifelse(p_age_cat == '2', '16-20 yrs',
                                     ifelse(p_age_cat == '3', '21-25 yrs', '26-35 yrs'))),
           stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                              ifelse(stim_type == 'l', 'lion','human'))) %>%
    mutate(p_age_cat = paste0('T: ',p_age_cat),
           stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
           bda = factor(bda, levels = c('before','during','after')))

  direction <- ifelse(prediction_direction == 1,'awaydirect',
                      ifelse(prediction_direction == 2,'awayangle',
                             ifelse(prediction_direction == 3,'neither',
                                    ifelse(prediction_direction == 4,'twdsangle',
                                           'twdsdirect'))))
  # ridge_pred_altogether(pred_df = pred_slice, type = 'move_dir',
  #                       prev = FALSE, prediction = dir[prediction_direction])
  pred_slice %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = f_age_cat),
                        alpha = 0.6,
                        scale = 0.9)+
    labs(fill = 'focal age category',
         x = 'predicted probability',
         y = 'probability density',
         title = dir[prediction_direction])+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))+
    facet_grid(. ~ stim_type, scales = 'free_x')
  ggsave(filename = paste0('mom_predicted_ridges_',direction,'_altogether.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
  # ridge_pred_splitpartner(pred_slice, type = 'move_dir', prev = FALSE)
  pred_slice %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6,
                        scale = 0.9)+
    labs(fill = 'target age category',
         x = 'predicted probability',
         y = 'probability density',
         title = dir[prediction_direction])+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))+
    facet_grid(. ~ stim_type, scales = 'free_x')
  ggsave(filename = paste0('mom_predicted_ridges_',direction,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')

  pred_slice %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = bda,
                            linetype = bda,
                            colour = bda),
                        alpha = 0.6,
                        size = 0.3,
                        scale = 0.9)+
    scale_fill_viridis_d()+
    scale_linetype_manual(values = c(1,2,1),
                          breaks = c('before','during','after'))+
    scale_colour_manual(values = c('transparent','black','black'),
                        breaks = c('before','during','after'))+
    scale_y_discrete(expand = c(0,0))+
    labs(fill = 'time relative to stimulus',
         colour = 'time relative to stimulus',
         linetype = 'time relative to stimulus',
         x = 'predicted probability',
         y = 'focal age category',
         title = direction)+
    facet_grid(stim_type ~ p_age_cat)+
    theme(legend.position = 'bottom')+
    guides(fill = guide_legend(nrow = 3),
           linetype = guide_legend(nrow = 3))
  ggsave(filename = paste0('mom_predicted_ridges_',direction,'_splittime.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
}

## plot contrasts
rm(list = ls()[!ls() %in% c('extract_predictions',
                            'ridge_pred_altogether','ridge_pred_splitpartner',
                            'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
load('movement_direction/ordinal_noprev/moving_noprev_2bda_agecontrasts.RData')
for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
  contrast_plot_altogether(contrasts_long, type = 'move', direction = plot,#prev = F,
                           free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_fixedscales_',plot,'_altogether.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')

  contrast_plot_altogether(contrasts_long, type = 'move', direction = plot,#prev = F,
                           free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_freescales_',plot,'_altogether.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')

  contrast_plot_splitpartner(contrasts_long, type = 'move', direction = plot,#prev = F,
                             free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')

  contrast_plot_splitpartner(contrasts_long, type = 'move', direction = plot,#prev = F,
                             free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}

plot_contrasts <- contrasts_long %>%
  rename(f_age_cat_org = f_age_cat,
         f_age_num_org = f_age_num) %>%
  mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                f_age_num_org + 1)) %>%
  mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                ifelse(f_age_num_alt == 2, '16-20 yrs',
                                       ifelse(f_age_num_alt == 3, '21-25 yrs',
                                              '26-35 yrs'))),
         move_pred = ifelse(move_pred == 'awaydirect',
                            'directly away',
                            ifelse(move_pred == 'awayangle',
                                   'away at an angle',
                                   ifelse(move_pred == 'neither',
                                          'neither towards or away',
                                          ifelse(move_pred == 'twdsangle',
                                                 'approach at an angle',
                                                 'directly approach')))),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
         move_pred = factor(move_pred,
                            levels = c('directly away', 'away at an angle',
                                       'neither towards or away',
                                       'approach at an angle', 'directly approach')))

for(stimulus in c("dove (control)","lion","human")){
  plot <- plot_contrasts %>%
    mutate(p_age_cat = paste0('T: ',p_age_cat,' yrs')) %>%
    filter(stim_type == stimulus) %>%
    ggplot()+
    geom_hline(yintercept = 0, lty = 3)+
    geom_violin(aes(x = comparison,
                    y = difference),
                fill = '#21918c',
                colour = 'transparent')+
    facet_grid(move_pred ~ p_age_cat,
               scales = 'fixed')+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
  print(plot)
}

plot_contrasts %>%
  mutate(p_age_cat = paste0('T: ',p_age_cat,' yrs'),
         stim_type = factor(stim_type, levels = c('human','lion','dove (control)'))) %>%
  filter(comparison != '26-35 to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = difference,
                          fill = comparison,
                          linetype = comparison,
                          colour = comparison),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+

  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('10-15 to 16-20 yrs',
                                   '16-20 to 21-25 yrs',
                                   '21-25 to 26-35 yrs'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('10-15 to 16-20 yrs',
                                 '16-20 to 21-25 yrs',
                                 '21-25 to 26-35 yrs'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'focal age change',
       colour = 'focal age change',
       x = 'contrast')+
  facet_grid(move_pred ~ p_age_cat,
             scales = 'fixed')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'mom_noprev_contrastridges_alldata.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

plot_contrasts %>%
  mutate(p_age_cat = paste0('T: ',p_age_cat,' yrs'),
         stim_type = factor(stim_type, levels = c('human','lion','dove (control)')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(comparison != '26-35 to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = difference,
                          fill = comparison,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       x = 'contrast')+
  facet_grid(move_pred ~ p_age_cat,
             scales = 'fixed')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'mom_noprev_contrastridges_splitbybda.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       device = 'png', height = 2800, width = 1900, unit = 'px')


# load('movement_direction/ordinal_noprev/moving_noprev_2bda_stimuluscontrasts.RData')
plot_contrasts <- contrasts_long %>%
  rename(f_age_cat_org = f_age_cat,
         f_age_num_org = f_age_num) %>%
  mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                f_age_num_org + 1)) %>%
  mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                ifelse(f_age_num_alt == 2, '16-20 yrs',
                                       ifelse(f_age_num_alt == 3, '21-25 yrs',
                                              '26-35 yrs'))),
         move_pred = ifelse(move_pred == 'awaydirect',
                            'directly away',
                            ifelse(move_pred == 'awayangle',
                                   'away at an angle',
                                   ifelse(move_pred == 'neither',
                                          'neither towards or away',
                                          ifelse(move_pred == 'twdsangle',
                                                 'approach at an angle',
                                                 'directly approach')))),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
         move_pred = factor(move_pred,
                            levels = c('directly away', 'away at an angle',
                                       'neither towards or away',
                                       'approach at an angle', 'directly approach')))

plot_contrasts %>%
  mutate(p_age_cat = paste0('T: ',p_age_cat,' yrs'),
         stim_type = factor(stim_type, levels = c('human','lion','dove (control)'))) %>%
  filter(comparison != '26-35 to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = difference,
                          fill = comparison,
                          linetype = comparison,
                          colour = comparison),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('10-15 to 16-20 yrs',
                                   '16-20 to 21-25 yrs',
                                   '21-25 to 26-35 yrs'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('10-15 to 16-20 yrs',
                                 '16-20 to 21-25 yrs',
                                 '21-25 to 26-35 yrs'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'focal age change',
       colour = 'focal age change',
       x = 'contrast')+
  facet_grid(move_pred ~ p_age_cat,
             scales = 'fixed')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'mom_noprev_contrastridges_alldata.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

plot_contrasts %>%
  mutate(p_age_cat = paste0('T: ',p_age_cat,' yrs'),
         stim_type = factor(stim_type, levels = c('human','lion','dove (control)')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(comparison != '26-35 to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = difference,
                          fill = comparison,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       x = 'contrast')+
  facet_grid(move_pred ~ p_age_cat,
             scales = 'fixed')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'mom_noprev_contrastridges_splitbybda.png',
       path = '../outputs/movement_ordinal_model_2bda/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

print('movement ignoring t-1 behaviour complete')
dev.off()

#### looking ordinal    ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_lookingordinal2_noprev.pdf')
rm(list = ls()[!ls() %in% c('extract_predictions',
                            'ridge_pred_altogether','ridge_pred_splitpartner',
                            'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
load('looking_direction/looking_noprev_model2bda_predictions.RData')
ls()

str(look)
str(pred)
unique(pred$pred_type)
unique(pred$pred_type_num)
saveRDS(pred, '../data_processed/look_pred.RDS')
saveRDS(pred[sample(x = 1:nrow(pred), size = (nrow(pred)/1000), replace = F),],
        '../data_processed/look_pred_cut.RDS')

## plot predictions
look$data_id <- 1:nrow(look)
trim <- sample(x = 1:((num_iter/2)*num_chains), size = 400, replace = F)
for(prediction_matrix in 1:3){
  pred_slice <- pred %>%
    filter(pred_type_num == prediction_matrix)
  str(pred_slice)
  direction <- ifelse(prediction_matrix == 1,'look away',
                      ifelse(prediction_matrix == 2,'side-on','look towards'))
  print(direction)
  gc()

  num_pred <- length(unique(pred_slice$data_row))
  pred_slice <- pred_slice %>%
    arrange(data_row) %>%
    mutate(draw_id = rep(1:((num_iter/2)*num_chains),
                         num_pred)) %>%
    filter(draw_id %in% trim)
  gc()

  pred_slice <- pred_slice %>%
    tidyr::separate(age_combo, into = c('f','p_age_cat'), remove = F) %>%
    dplyr::mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                                     ifelse(f_age_num == 2, '16-20 yrs',
                                            ifelse(f_age_num == 3, '21-25 yrs',
                                                   '26-35 yrs'))),
                  p_age_cat = ifelse(p_age_cat == '1', '10-15 yrs',
                                     ifelse(p_age_cat == '2', '16-20 yrs',
                                            ifelse(p_age_cat == '3', '21-25 yrs', '26-35 yrs'))),
                  stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                                     ifelse(stim_type == 'l', 'lion','human'))) %>%
    dplyr::mutate(p_age_cat = paste0('T: ',p_age_cat),
                  stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
                  bda = factor(bda, levels = c('before','during','after')))
  saveRDS(pred_slice, paste0('../data_processed/look_pred_dir',prediction_matrix,'.RDS'))
  print('data prepped')

  # ridge_pred_altogether(pred_slice, type = 'look', prev = FALSE)
  pred_slice %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = f_age_cat),
                        alpha = 0.6,
                        scale = 0.9)+
    labs(fill = 'focal age category',
         x = 'predicted probability',
         y = 'probability density',
         title = direction)+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))+
    facet_grid(. ~ stim_type, scales = 'free_x')
  ggsave(filename = paste0('lom_noprev_predicted_ridges_',direction,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
  warnings()
  print('altogether done')
}
rm(list = ls()[! ls() %in% c('contrast_plot_altogether,contrast_plot_splitpartner')]) ; gc()

for(prediction_matrix in 1:3){
  pred_slice <- readRDS(paste0('../data_processed/look_pred_dir',prediction_matrix,'.RDS'))
  # ridge_pred_splitpartner(pred_slice, type = 'look', prev = FALSE)
  direction <- ifelse(prediction_matrix == 1,'look away',
                      ifelse(prediction_matrix == 2,'side-on','look towards'))
  pred_slice %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6,
                        scale = 0.9)+
    labs(fill = 'target age category',
         x = 'predicted probability',
         y = 'probability density',
         title = direction)+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))+
    facet_grid(. ~ stim_type, scales = 'free_x')
  ggsave(filename = paste0('lom_noprev_predicted_ridges_',direction,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
  warnings()
  print('split partner done')
  gc()

  pred_slice %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = bda,
                            linetype = bda,
                            colour = bda),
                        alpha = 0.6,
                        size = 0.3,
                        scale = 0.9)+
    scale_fill_viridis_d()+
    scale_linetype_manual(values = c(1,2,1),
                          breaks = c('before','during','after'))+
    scale_colour_manual(values = c('transparent','black','black'),
                        breaks = c('before','during','after'))+
    scale_y_discrete(expand = c(0,0))+
    labs(fill = 'time relative to stimulus',
         colour = 'time relative to stimulus',
         linetype = 'time relative to stimulus',
         x = 'predicted probability',
         y = 'focal age category',
         title = direction)+
    facet_grid(stim_type ~ p_age_cat)+
    theme(legend.position = 'bottom')+
    guides(fill = guide_legend(nrow = 3),
           linetype = guide_legend(nrow = 3))
  ggsave(filename = paste0('lom_noprev_predicted_ridges_',direction,'_splittime.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
  warnings()
  print('split time done')
  gc()
}
warnings()

## plot contrasts
load('looking_direction/looking_noprev_agecontrasts.RData')
ls()

contrasts <- look %>%
  mutate(alt_vs_org_away_mu = apply(alt_vs_org_away, 2, mean),
         alt_vs_org_side_mu = apply(alt_vs_org_side, 2, mean),
         alt_vs_org_twds_mu = apply(alt_vs_org_twds, 2, mean)) %>%
  mutate(categories_different = ifelse(f_age_num == 4,
                                       '3 categories different',
                                       '1 category different'),
         alt_vs_org_away_mu = ifelse(f_age_num == 4, alt_vs_org_away_mu*(-1), alt_vs_org_away_mu),
         alt_vs_org_side_mu = ifelse(f_age_num == 4, alt_vs_org_side_mu*(-1), alt_vs_org_side_mu),
         alt_vs_org_twds_mu = ifelse(f_age_num == 4, alt_vs_org_twds_mu*(-1), alt_vs_org_twds_mu))
contrasts_long <- contrasts %>%
  pivot_longer(cols = c(alt_vs_org_away_mu,
                        alt_vs_org_side_mu,
                        alt_vs_org_twds_mu),
               names_to = 'contrast', values_to = 'difference') %>%
  separate(contrast, into = c('alt','vs','org','look_pred','mu'),
           sep = '_', remove = T) %>%
  select(-alt, -vs, -org, -mu)

## plot contrasts
contrasts_long %>%
  mutate(pred_type = ifelse(look_pred == 'away',
                            'look away directly',
                            ifelse(look_pred == 'side',
                                   'side on',
                                   'look at directly'))) %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('look away directly',
                                       'side on',
                                       'look at directly'))) %>%
  mutate(f_age_new = ifelse(f_age_num == 4,
                            'youngest to oldest',
                            paste0('category ',f_age_num,' to ',f_age_num+1))) %>%
  ggplot()+
  geom_density(aes(x = difference,
                   colour = categories_different,
                   fill = categories_different),
               alpha = 0.5)+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_grid(pred_type ~ f_age_new, scales = 'free_y')+
  scale_colour_viridis_d(begin = 0.5, end = 0)+
  scale_fill_viridis_d(begin = 0.5, end = 0)+
  labs(colour = 'categories\ndifferent',
       fill = 'categories\ndifferent')
save.image('looking_direction/looking_noprev_agecontrasts.RData')
# load('looking_direction/looking_noprev_agecontrasts.RData')

## clean up a bit
rm(list = ls()[! ls() %in% c('alt_vs_org_away','alt_vs_org_side',
                             'alt_vs_org_twds','look','lom_noprev_fit',
                             'behav','num_iter','num_chains',
                             'contrast_plot_altogether','contrast_plot_splitpartner',
                             'contrasts_long')]) ; gc()

## plot full density instead of means
look$data_row <- 1:nrow(look)
colnames(alt_vs_org_away) <- look$data_row
colnames(alt_vs_org_side) <- look$data_row
colnames(alt_vs_org_twds) <- look$data_row

mtx_to_df <- function(mtx, pred_type){
  df <- mtx %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(),
                 names_to = 'data_row',
                 values_to = 'contrast') %>%
    mutate(data_row = as.integer(data_row)) %>%
    left_join(look, by = 'data_row') %>%
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
away <- mtx_to_df(alt_vs_org_away, pred_type = 'look away directly')
side <- mtx_to_df(alt_vs_org_side, pred_type = 'side on')
twds <- mtx_to_df(alt_vs_org_twds, pred_type = 'look at directly')

plot_contrasts <- rbind(away, side, twds) %>%
  mutate(prediction_type = factor(prediction_type,
                                  levels = c('look away directly',
                                             'side on',
                                             'look at directly')))

for(plot in c('away','side','twds')){
  contrast_plot_altogether(contrasts_long, type = 'look', direction = plot,#prev = F,
                           free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom_noprev_contrasts_fixedscales_',plot,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_altogether(contrasts_long, type = 'look', direction = plot,#prev = F,
                           free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom_noprev_contrasts_freescales_',plot,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'look', direction = plot,#prev = F,
                             free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom_noprev_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'look', direction = plot,#prev = F,
                             free_y = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom_noprev_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}

as.data.frame(head(contrasts_long))
saveRDS(contrasts_long, '../data_processed/look_noprev_agecontrasts_check.RDS')

contrasts_long <- readRDS('../data_processed/look_noprev_agecontrasts_check.RDS')

unique(contrasts_long$look_pred)

plot_contrasts <- contrasts_long %>%
  rename(f_age_cat_org = f_age_cat,
         f_age_num_org = f_age_num) %>%
  mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                f_age_num_org + 1)) %>%
  mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                ifelse(f_age_num_alt == 2, '16-20 yrs',
                                       ifelse(f_age_num_alt == 3, '21-25 yrs',
                                              '26-35 yrs'))),
         look_pred = ifelse(look_pred == 'away', 'look away',
                            ifelse(look_pred == 'side', 'side on',
                                   ifelse(look_pred == 'twds', 'look towards', 'error'))),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt),
         look_pred = factor(look_pred,
                            levels = c('look away', 'side on', 'look towards')))

for(stimulus in c("dove (control)","lion","human")){
  plot <- plot_contrasts %>%
    mutate(p_age_cat = paste0('T: ',p_age_cat,' yrs')) %>%
    filter(stim_type == stimulus) %>%
    ggplot()+
    geom_hline(yintercept = 0, lty = 3)+
    geom_violin(aes(x = comparison,
                    y = difference),
                fill = '#21918c',
                colour = 'transparent')+
    facet_grid(look_pred ~ p_age_cat,
               scales = 'fixed')+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
  print(plot)
}

plot_contrasts %>%
  mutate(p_age_cat = paste0('T: ',p_age_cat,' yrs'),
         stim_type = factor(stim_type, levels = c('human','lion','dove (control)'))) %>%
  filter(comparison != '26-35 to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = difference,
                          fill = comparison,
                          linetype = comparison,
                          colour = comparison),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('10-15 to 16-20 yrs',
                                   '16-20 to 21-25 yrs',
                                   '21-25 to 26-35 yrs'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('10-15 to 16-20 yrs',
                                 '16-20 to 21-25 yrs',
                                 '21-25 to 26-35 yrs'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'focal age change',
       colour = 'focal age change',
       x = 'contrast')+
  facet_grid(look_pred ~ p_age_cat,
             scales = 'fixed')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'lom_noprev_contrastridges_alldata.png',
       path = '../outputs/looking_ordinal_model_2bda/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

plot_contrasts %>%
  mutate(p_age_cat = paste0('T: ',p_age_cat,' yrs'),
         stim_type = factor(stim_type, levels = c('human','lion','dove (control)')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(comparison != '26-35 to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = difference,
                          fill = comparison,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       x = 'contrast')+
  facet_grid(look_pred ~ p_age_cat,
             scales = 'fixed')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'lom_noprev_contrastridges_splitbybda.png',
       path = '../outputs/looking_ordinal_model_2bda/',
       device = 'png', height = 2000, width = 2000, unit = 'px')

print('looking noprev complete')
dev.off()

#### movement binomial  ####
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
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          colour = f_age_cat,
                          fill = f_age_cat),
                      alpha = 0.6,
                      scale = 0.9)+
  labs(fill = 'focal age category',
       x = 'predicted probability',
       y = 'probability density')+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90))+
  facet_grid(bda ~ stim_type_long, scales = 'free_x')
ggsave(filename = 'mbm_noprev_predicted_ridges_altogether.png',
       path = '../outputs/movement_binomial_model/',
       plot = last_plot(),
       device = 'png', height = 1800, width = 1500, units = 'px')

pred %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          fill = bda,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(fill = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       linetype = 'time relative to stimulus',
       x = 'predicted probability',
       y = 'focal age category')+
  facet_grid(stim_type_long ~ .)+
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(filename = 'mbm_noprev_predicted_ridges_splittime.png',
       path = '../outputs/movement_binomial_model/',
       plot = last_plot(),
       device = 'png', height = 1800, width = 1500, units = 'px')

## clean up data
colnames(age_contrast) <- age_move_org$unique_data_combo
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
save.image('movement_direction/binomial_noprev/movement_binomial_ageplotting.RData')
write_csv(age_contrast_long, '../data_processed/movement_binomial_agecontrasts_check.csv')
write_csv(age_contrast_long[1:(ncol(age_contrast)*2),], '../data_processed/movement_binomial_agecontrasts_check_short.csv')

## plot
length(which(is.na(age_contrast_long$comparison) == TRUE)) # should be 691956000 -- would explain why previous run gave warning message about rows non-finite outside scale range (stat_ydensity)
age_contrast_long %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion',
                                   'human'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_hline(yintercept = 0, lty = 3)+
  geom_violin(aes(x = comparison,
                  y = contrast,
                  fill = comparison),
              position = position_dodge(0.5))+
  scale_fill_viridis_d()+
  facet_grid(bda ~ stim_type, scales = 'free_y')+
  labs(fill = 'comparison')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'mbm_violin_contrasts_noprev.png',
       path = '../outputs/movement_binomial_model/',
       height = 1600, width = 1600, unit = 'px')

# load('movement_direction/binomial_noprev/movement_binomial_ageplotting.RData')
as.data.frame(head(age_contrast_long))
plot_contrasts <- age_contrast_long %>%
  # rename(f_age_cat_org = f_age_cat,
  #        f_age_num_org = f_age_num) %>%
  mutate(f_age_num_alt = ifelse(f_age_num_org == 4, 1,
                                f_age_num_org + 1)) %>%
  mutate(f_age_cat_alt = ifelse(f_age_num_alt == 1,'10-15 yrs',
                                ifelse(f_age_num_alt == 2, '16-20 yrs',
                                       ifelse(f_age_num_alt == 3, '21-25 yrs',
                                              '26-35 yrs'))),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(comparison = paste0(f_age_cat_org,' to ',f_age_cat_alt))

for(stimulus in c("dove (control)","lion","human")){
  plot <- plot_contrasts %>%
    filter(stim_type == stimulus) %>%
    ggplot()+
    geom_hline(yintercept = 0, lty = 3)+
    geom_violin(aes(x = comparison,
                    y = contrast),
                fill = '#21918c',
                colour = 'transparent')+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))+
    labs(y = 'difference')
  print(plot)
}

unique(plot_contrasts$comparison)

plot_contrasts %>%
  mutate(stim_type = factor(stim_type, levels = c('human','lion','dove (control)'))) %>%
  filter(comparison != '26-35 yrs to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = contrast,
                          fill = comparison,
                          linetype = comparison,
                          colour = comparison),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('10-15 to 16-20 yrs',
                                   '16-20 to 21-25 yrs',
                                   '21-25 to 26-35 yrs'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('10-15 to 16-20 yrs',
                                 '16-20 to 21-25 yrs',
                                 '21-25 to 26-35 yrs'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'focal age change',
       colour = 'focal age change',
       x = 'contrast')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'mbm_noprev_contrastridges_alldata.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

plot_contrasts %>%
  mutate(stim_type = factor(stim_type, levels = c('human','lion','dove (control)')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(comparison != '26-35 yrs to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = contrast,
                          fill = comparison,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       x = 'contrast')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'mbm_noprev_contrastridges_splitbybda.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

print('movement binomial complete')
dev.off()

#### neighbour binomial ####
pdf('../outputs/neighbour_binomial_model_bda/niceplots_nearestneighbour_noprev.pdf')
# load('nearest_neighbour/neighbour_noprev_agecontrasts.RData')
# rm(list = ls()[! ls() %in% c('contrasts','contrasts_long','age_nn_org','age_mtx_org','nn',
#                              'extract_predictions',
#                              'ridge_pred_altogether','ridge_pred_splitpartner',
#                              'contrast_plot_altogether','contrast_plot_splitpartner',
#                              'contrast_YYvYO','contrast_YYvOY','contrast_YYvOO',
#                              'contrast_YOvOY','contrast_YOvOO','contrast_OYvOO')]) ; gc()
# print('data loaded')
#
# save.image('nearest_neighbour/neighbour_noprev_agecontrasts_plotting.RData')
load('nearest_neighbour/neighbour_noprev_agecontrasts_plotting.RData')

## plot predictions
colnames(age_mtx_org) <- 1:nrow(age_nn_org)
age_nn_org$data_id <- 1:nrow(age_nn_org)
pred <- age_mtx_org %>%
  as.data.frame() %>%
  pivot_longer(cols = everything(), names_to = 'data_id', values_to = 'epred') %>%
  mutate(data_id = as.integer(data_id)) %>%
  left_join(age_nn_org, by = 'data_id') %>%
  separate(age_rel, into = c('f_age','p_age'), remove = F, sep = 1) %>%
  mutate(stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
                                 ifelse(stim_type == 'l','lion','human')),
         f_age_cat = ifelse(f_age == 'Y', 'young', 'old'),
         p_age_cat = ifelse(p_age == 'Y', 'T: young', 'T: old'),
         draw_id = rep(1:400, each = nrow(age_nn_org)),
         stim_type_long = ifelse(stim_type == 'ctd','dove (control)',
                                 ifelse(stim_type == 'l','lion','human'))) %>%
  mutate(stim_type_long = factor(stim_type_long,
                                 levels = c('dove (control)','lion','human')),
         bda = factor(bda, levels = c('before','during','after')),
         age_rel_long = paste0('F: ', f_age_cat, ', ', p_age_cat))
print('data created')

pred %>%
  ggplot()+
  geom_boxplot(aes(x = f_age_cat,
                   fill = p_age_cat,
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
                   fill = age_rel),
               alpha = 0.4)+
  labs(fill = 'age category combination',
       x = 'predicted probability\nof being neighbours',
       y = 'probability density')+
  facet_grid(bda ~ stim_type_long,
             scales = 'free')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
print('density plots run')

## plot predictions
pred %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          colour = f_age_cat,
                          fill = f_age_cat),
                      alpha = 0.6,
                      scale = 0.9)+
  labs(fill = 'focal age category',
       colour = 'focal age category',
       x = 'predicted probability',
       y = 'focal age category')+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90))+
  facet_grid(bda ~ stim_type_long, scales = 'free_x')
ggsave(filename = 'nbm_noprev_predicted_ridges_altogether.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       plot = last_plot(),
       device = 'png', height = 1800, width = 1500, units = 'px')

pred %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          colour = p_age_cat,
                          fill = p_age_cat),
                      alpha = 0.6,
                      scale = 0.9)+
  labs(fill = 'target age category',
       colour = 'target age category',
       x = 'predicted probability',
       y = 'focal age category')+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90))+
  facet_grid(bda ~ stim_type_long, scales = 'free_x')
ggsave(filename = 'nbm_noprev_predicted_ridges_splitpartner.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       plot = last_plot(),
       device = 'png', height = 1800, width = 1500, units = 'px')

pred %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          fill = bda,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(fill = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       linetype = 'time relative to stimulus',
       x = 'predicted probability',
       y = 'focal age category')+
  facet_grid(stim_type_long ~ p_age_cat)+
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(filename = 'nbm_noprev_predicted_ridges_splitboth.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       plot = last_plot(),
       device = 'png', height = 1800, width = 1500, units = 'px')

pred %>%
  ggplot()+
  geom_density_ridges(aes(x = epred,
                          y = f_age_cat,
                          fill = bda,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(fill = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       linetype = 'time relative to stimulus',
       x = 'predicted probability',
       y = 'focal age category')+
  facet_grid(stim_type_long ~ .)+
  theme(legend.position = 'bottom')+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(filename = 'nbm_noprev_predicted_ridges_splittime.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       plot = last_plot(),
       device = 'png', height = 1800, width = 1500, units = 'px')
print('ridges produced')
rm(pred) ; gc()

## contrasts
make_long <- function(contrast_mtx, names, comparison){
  colnames(contrast_mtx) <- names
  contrast_df <- contrast_mtx %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(),
                 names_to = 'unique_data_combo',
                 values_to = 'contrast') %>%
    mutate(unique_data_combo = as.integer(unique_data_combo),
           comparison = comparison)
  return(contrast_df)
}

contrast_YYvYO <- make_long(contrast_mtx = contrast_YYvYO,
                            names = age_nn_org$unique_data_combo,
                            comparison = 'YYvYO')
contrast_YYvOY <- make_long(contrast_mtx = contrast_YYvOY,
                            names = age_nn_org$unique_data_combo,
                            comparison = 'YYvOY')
contrast_YYvOO <- make_long(contrast_mtx = contrast_YYvOO,
                            names = age_nn_org$unique_data_combo,
                            comparison = 'YYvOO')
contrast_YOvOY <- make_long(contrast_mtx = contrast_YOvOY,
                            names = age_nn_org$unique_data_combo,
                            comparison = 'YOvOY')
contrast_YOvOO <- make_long(contrast_mtx = contrast_YOvOO,
                            names = age_nn_org$unique_data_combo,
                            comparison = 'YOvOO')
contrast_OYvOO <- make_long(contrast_mtx = contrast_OYvOO,
                            names = age_nn_org$unique_data_combo,
                            comparison = 'OYvOO')

age_contrast_long <- rbind(contrast_YYvYO, contrast_YYvOY, contrast_YYvOO,
                           contrast_YOvOY, contrast_YOvOO, contrast_OYvOO)
saveRDS(age_contrast_long, '../data_processed/nn_contrasts_youngold.RDS')

age_contrast_long <- age_contrast_long %>%
  left_join(distinct(age_nn_org), by = 'unique_data_combo') %>%
  separate(col = comparison, into = c('original', 'altered'), sep = 'v', remove = F) %>%
  separate(col = original, into = c('f_age_org','p_age_org'), sep = 1, remove = F) %>%
  separate(col = altered,  into = c('f_age_alt','p_age_alt'), sep = 1, remove = F) %>%
  mutate(f_age_org_short = ifelse(f_age_org == 'Y', 'young', 'old'),
         f_age_alt_short = ifelse(f_age_alt == 'Y', 'young', 'old'),
         p_age_org_short = ifelse(p_age_org == 'Y', 'young', 'old'),
         p_age_alt_short = ifelse(p_age_alt == 'Y', 'young', 'old')) %>%
  mutate(comparison_long = paste0('F: ', f_age_org_short, ' + T: ', p_age_org_short, ' to ',
                                  'F: ', f_age_alt_short, ' + T: ', p_age_alt_short))
save.image('nearest_neighbour/neighbour_binomial_ageplotting.RData')
write_csv(age_contrast_long, '../data_processed/neighbour_binomial_agecontrasts_check.csv')
age_contrast_long <- read_csv('../data_processed/neighbour_binomial_agecontrasts_check.csv')

# ## plot
# length(which(is.na(age_contrast_long$comparison) == TRUE))
# age_contrast_long %>%
#   mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                             ifelse(stim_type == 'l', 'lion',
#                                    'human'))) %>%
#   mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human')),
#          bda = factor(bda, levels = c('before','during','after'))) %>%
#   ggplot()+
#   geom_hline(yintercept = 0, lty = 3)+
#   geom_violin(aes(x = comparison,
#                   y = contrast,
#                   fill = comparison),
#               position = position_dodge(0.5))+
#   scale_fill_viridis_d()+
#   facet_grid(bda ~ stim_type, scales = 'free_y')+
#   labs(fill = 'comparison')+
#   theme(legend.position = 'bottom',
#         axis.text.x = element_text(angle = 90, vjust = 0.5))
# ggsave(plot = last_plot(), device = 'png',
#        filename = 'nbm_violin_contrasts_noprev.png',
#        path = '../outputs/neighbour_binomial_model_bda/',
#        height = 1600, width = 1600, unit = 'px')
#
as.data.frame(head(age_contrast_long))
plot_contrasts <- age_contrast_long %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human')))

for(stimulus in c("dove (control)","lion","human")){
  plot <- plot_contrasts %>%
    filter(stim_type == stimulus) %>%
    ggplot()+
    geom_hline(yintercept = 0, lty = 3)+
    geom_violin(aes(x = comparison,
                    y = contrast),
                fill = '#21918c',
                colour = 'transparent')+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90, vjust = 0.5))+
    labs(y = 'difference')
  print(plot)
}

unique(plot_contrasts$comparison)

plot_contrasts %>%
  mutate(stim_type = factor(stim_type, levels = c('human','lion','dove (control)'))) %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = contrast,
                          fill = comparison,
                          linetype = comparison,
                          colour = comparison),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(3,2,1,1,2,3),
                        breaks = c('F: young + T: young to F: young + T: old',
                                   'F: young + T: young to F: old + T: young',
                                   'F: young + T: young to F: old + T: old',
                                   'F: young + T: old to F: old + T: young',
                                   'F: young + T: old to F: old + T: old',
                                   'F: old + T: young to F: old + T: old'))+
  scale_colour_manual(values = c('grey','grey','grey','black','black','black'),
                      breaks = c('F: young + T: young to F: young + T: old',
                                 'F: young + T: young to F: old + T: young',
                                 'F: young + T: young to F: old + T: old',
                                 'F: young + T: old to F: old + T: young',
                                 'F: young + T: old to F: old + T: old',
                                 'F: old + T: young to F: old + T: old'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'age change',
       linetype = 'age change',
       colour = 'age change',
       x = 'contrast')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'nbm_noprev_contrastridges_alldata.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

plot_contrasts %>%
  mutate(comparison = paste0(original, '->', altered)) %>%
  mutate(stim_type = factor(stim_type, levels = c('human','lion','dove (control)')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(comparison != '26-35 yrs to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = contrast,
                          fill = comparison,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       x = 'contrast')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'nbm_noprev_contrastridges_splitbybda.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

print('neighbour binomial complete')
dev.off()

plot_contrasts %>%
  mutate(comparison = paste0(original, '->', altered)) %>%
  mutate(stim_type = factor(stim_type, levels = c('human','lion','dove (control)')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(comparison != '26-35 yrs to 10-15 yrs') %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = contrast,
                          fill = comparison,
                          linetype = bda,
                          colour = bda),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_linetype_manual(values = c(1,2,1),
                        breaks = c('before','during','after'))+
  scale_colour_manual(values = c('transparent','black','black'),
                      breaks = c('before','during','after'))+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'focal age change',
       linetype = 'time relative to stimulus',
       colour = 'time relative to stimulus',
       x = 'contrast')+
  facet_grid(. ~ bda)+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'nbm_noprev_contrastridges_splitbybda_facet.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       device = 'png', height = 2800, width = 1900, unit = 'px')

plot_contrasts %>%
  mutate(comparison = paste0(original, ' -> ', altered)) %>%
  mutate(age_change = ifelse(comparison == 'OY -> OO', 'target',
                             ifelse(comparison == 'YO -> OO', 'focal',
                                    ifelse(comparison == 'YO -> OY', 'both',
                                           ifelse(comparison == 'YY -> OO', 'both',
                                                  ifelse(comparison == 'YY -> OY', 'focal',
                                                         ifelse(comparison == 'YY -> YO', 'target', 'error'))))))) %>%
  mutate(stim_type = factor(stim_type, levels = c('human','lion','dove (control)')),
         bda = factor(bda, levels = c('before','during','after')),
         comparison = factor(comparison, levels = c('YY -> YO', 'YY -> OY', 'YY -> OO',
                                                    'YO -> OY', 'YO -> OO', 'OY -> OO')),
         age_change = factor(age_change, levels = c('focal','target','both'))) %>%
  ggplot()+
  geom_vline(xintercept = 0, linetype = 3)+
  geom_density_ridges(aes(y = stim_type,
                          x = contrast,
                          fill = age_change),
                      alpha = 0.6,
                      size = 0.3,
                      scale = 0.9)+
  scale_fill_viridis_d()+
  scale_y_discrete(expand = c(0,0))+
  labs(y = 'stimulus type',
       fill = 'individual age changed',
       x = 'contrast')+
  facet_grid(comparison ~ bda)+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))+
  guides(fill = guide_legend(nrow = 3),
         linetype = guide_legend(nrow = 3))
ggsave(plot = last_plot(),
       filename = 'nbm_noprev_contrastridges_nicestfacet_wide.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       device = 'png', height = 2400, width = 2000, unit = 'px')
