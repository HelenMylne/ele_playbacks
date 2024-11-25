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

#### create box and density plotting functions ####
# boxplot_pred <- function(pred, type){
#   if(type == 'move_prob'){
#     predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'moving', 'not moving')
#     pred <- pred %>%
#       mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving', 'no', 'yes'),
#              f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
#                                 ifelse(f_age_num == 2, '16-20 yrs',
#                                        ifelse(f_age_num == 3, '21-25 yrs',
#                                               '26-35 yrs')))) %>%
#       mutate(prev_action = factor(move_tminus1, levels = c('yes','no')))
#   }
#   
#   if(type == 'move_dir'){
#     predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                   ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                          ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                                 ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                        'approach directly'))))
#     pred <- pred %>%
#       mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
#                                    ifelse(move_tminus1_num == 2, 'move away at an angle',
#                                           ifelse(move_tminus1_num == 3, 'move neither towards or away',
#                                                  ifelse(move_tminus1_num == 4,
#                                                         'approach at an angle',
#                                                         'approach directly')))),
#              f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
#                                 ifelse(f_age_num == 2, '16-20 yrs',
#                                        ifelse(f_age_num == 3, '21-25 yrs',
#                                               '26-35 yrs')))) %>%
#       mutate(prev_action = factor(move_tminus1, levels = c('move directly away',
#                                                            'move away at an angle',
#                                                            'move neither towards or away',
#                                                            'approach at an angle',
#                                                            'approach directly')))
#   }
#   
#   pred %>% 
#     ggplot()+
#     geom_boxplot(aes(x = f_age_cat,
#                      fill = prev_action,
#                      y = epred))+
#     labs(x = 'focal age category',
#          y = 'predicted probability',
#          fill = 'action in previous second',
#          title = predicted_direction)+
#     facet_grid(prev_action ~ stim_type)+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom',
#           axis.text.x = element_text(angle = 90))
# 
# }
# 
# density_pred <- function(pred, type){
#   if(type == 'move_dir'){
#     predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
#                                   ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
#                                          ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
#                                                 ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
#                                                        'approach directly'))))
#     
#     pred <- pred %>%
#       mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
#                                    ifelse(move_tminus1_num == 2, 'move away at an angle',
#                                           ifelse(move_tminus1_num == 3, 'move neither towards or away',
#                                                  ifelse(move_tminus1_num == 4,
#                                                         'approach at an angle',
#                                                         'approach directly')))),
#              f_age_cat = ifelse(f_age_num == 1, '10-15 years',
#                                 ifelse(f_age_num == 2, '16-20 years',
#                                        ifelse(f_age_num == 3, '21-25 years',
#                                               '26-35 years')))) %>%
#       mutate(prev_action = factor(move_tminus1, levels = c('move directly away',
#                                                            'move away at an angle',
#                                                            'move neither towards or away',
#                                                            'approach at an angle',
#                                                            'approach directly')))
#   }
#   pred %>%
#     ggplot()+
#     geom_density(aes(x = epred,
#                      fill = f_age_cat),
#                  alpha = 0.4)+
#     labs(fill = 'focal age category',
#          x = 'predicted probability',
#          y = 'probability density',
#          title = predicted_direction)+
#     facet_grid(prev_action ~ stim_type,
#                scales = 'free')+
#     scale_fill_viridis_d()+
#     theme(legend.position = 'bottom')
# 
# }

#### create ridge plotting functions ####
ridge_pred_altogether <- function(pred, type, prev){
  if(type == 'move_prob'){
    predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'moving', 'not moving')
    if(prev == TRUE){
        pred <- pred %>%
        mutate(move_tminus1 = ifelse(move_tminus1 == 'not_moving',
                                     'not moving at t-1','moving at t-1')) %>%
        mutate(prev_action = factor(move_tminus1, levels = c('moving at t-1',
                                                             'not moving at t-1')))
    }
  }
  
  if(type == 'move_dir'){
    predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
                                  ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
                                         ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
                                                ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
                                                       'approach directly'))))
    if(prev == TRUE){
      pred <- pred %>%
        mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
                                     ifelse(move_tminus1_num == 2, 'move away at an angle',
                                            ifelse(move_tminus1_num == 3, 'neither towards or away',
                                                   ifelse(move_tminus1_num == 4,
                                                          'approach at an angle',
                                                          'approach directly'))))) %>%
        mutate(prev_action = factor(move_tminus1, levels = c('move directly away',
                                                             'move away at an angle',
                                                             'neither towards or away',
                                                             'approach at an angle',
                                                             'approach directly')))
    }
  }
  
  if(type == 'look'){
    predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                  ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                         'look towards'))
    if(prev == TRUE){
      pred <- pred %>%
        mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
                                     ifelse(prev_num == 2, 'side-on',
                                            'look towards'))) %>%
        mutate(prev_action = factor(look_tminus1, levels = c('look away',
                                                             'side-on',
                                                             'look towards')))
    }
  }
  
  pred <- pred %>% 
    mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                              ifelse(f_age_num == 2, '16-20 yrs',
                                     ifelse(f_age_num == 3, '21-25 yrs',
                                            '26-35 yrs'))))
  plot <- pred %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = f_age_cat),
                        alpha = 0.6,
                        scale = 0.9)+
    labs(fill = 'target age category',
         x = 'predicted probability',
         y = 'probability density',
         title = predicted_direction)+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
  
  if(prev == TRUE){
    plot <- plot + facet_grid(prev_action ~ stim_type,
                      scales = 'free_x')
  }
  if(prev == FALSE){
    plot <- plot + facet_grid(. ~ stim_type,
                      scales = 'free_x')
  }
  return(plot)
}

ridge_pred_splitpartner <- function(pred, type, prev){
  if(type == 'move_dir'){
    predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'move directly away',
                                  ifelse(pred$predicted_direction[1] == 2, 'move away at an angle',
                                         ifelse(pred$predicted_direction[1] == 3, 'move neither towards or away',
                                                ifelse(pred$predicted_direction[1] == 4, 'approach at an angle',
                                                       'approach directly'))))
    if(prev == TRUE){
      pred <- pred %>%
        mutate(move_tminus1 = ifelse(move_tminus1_num == 1, 'move directly away',
                                     ifelse(move_tminus1_num == 2, 'move away at an angle',
                                            ifelse(move_tminus1_num == 3, 'neither towards or away',
                                                   ifelse(move_tminus1_num == 4,
                                                          'approach at an angle',
                                                          'approach directly'))))) %>%
        mutate(prev_action = factor(move_tminus1, levels = c('move directly away',
                                                             'move away at an angle',
                                                             'neither towards or away',
                                                             'approach at an angle',
                                                             'approach directly')))
    }
  }
  
  if(type == 'look'){
    predicted_direction <- ifelse(pred$predicted_direction[1] == 1, 'look away',
                                  ifelse(pred$predicted_direction[1] == 2, 'side-on',
                                         'look towards'))
    if(prev == TRUE){
      pred <- pred %>%
        mutate(look_tminus1 = ifelse(prev_num == 1, 'look away',
                                     ifelse(prev_num == 2, 'side-on',
                                            'look towards'))) %>%
        mutate(prev_action = factor(look_tminus1, levels = c('look away',
                                                             'side-on',
                                                             'look towards')))
    }
  }
  
  pred <- pred %>% 
    mutate(f_age_cat = ifelse(f_age_num == 1, '10-15 yrs',
                              ifelse(f_age_num == 2, '16-20 yrs',
                                     ifelse(f_age_num == 3, '21-25 yrs',
                                            '26-35 yrs')))) %>% 
    separate(age_combo, into = c('f','p_age_cat'), remove = F) %>%
    mutate(p_age_cat = ifelse(p_age_cat == '1', '10-15 yrs',
                              ifelse(p_age_cat == '2', '16-20 yrs',
                                     ifelse(p_age_cat == '3', '21-25 yrs', '26-35 yrs'))))
  plot <- pred %>%
    ggplot()+
    geom_density_ridges(aes(x = epred,
                            y = f_age_cat,
                            fill = p_age_cat),
                        alpha = 0.6,
                        scale = 0.9)+
    labs(fill = 'target age category',
         x = 'predicted probability',
         y = 'probability density',
         title = predicted_direction)+
    scale_fill_viridis_d()+
    theme(legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
  
  if(prev == TRUE){
    plot <- plot + facet_grid(prev_action ~ stim_type,
                      scales = 'free_x')
  }
  if(prev == FALSE){
    plot <- plot + facet_grid(. ~ stim_type,
                      scales = 'free_x')
  }
  return(plot)
}

#### create contrast plotting functions ####
contrast_plot_altogether <- function(contrasts, type, direction, free_y, prev){
  if(type == 'move'){
    contrasts <- contrasts %>%
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
                                ifelse(stim_type == 'l', 'lion', 'human')))
    if(prev == TRUE){
      contrasts <- contrasts %>% 
        mutate(prev_action = ifelse(move_tminus1_num == 1, 'move away directly',
                                    ifelse(move_tminus1_num == 2,'move away at an angle',
                                           ifelse(move_tminus1_num == 3, 'neither towards or away',
                                                  ifelse(move_tminus1_num == 4,
                                                         'approach at an angle',
                                                         'approach directly'))))) %>%
        mutate(prev_action = factor(prev_action,
                                    levels = c('move away directly',
                                               'move away at an angle',
                                               'neither towards or away',
                                               'approach at an angle',
                                               'approach directly')))
    }
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
                                ifelse(stim_type == 'l', 'lion','human')))
             stim_type = factor(stim_type,
                                levels = c('dove (control)',
                                           'lion',
                                           'human'))
    if(prev == TRUE){
      contrasts <- contrasts %>% 
        mutate(prev_action = factor(prev_action,
                                    levels = c('look directly away',
                                               'side-on',
                                               'look at directly')))
    }
    plot_title <- ifelse(direction == 'away', 'look away',
                         ifelse(direction == 'side', 'side-on',
                                'look towards'))
  }
  
  plot <- contrasts %>% 
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
          axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  if(prev == TRUE){
    plot <- plot + facet_grid(prev_action ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed'))
  }
  if(prev == FALSE){
    plot <- plot + facet_grid(. ~ stim_type,
               scales = ifelse(free_y == T, 'free_y', 'fixed'))
  }
  return(plot)
}

contrast_plot_splitpartner <- function(contrasts, type, direction, free_y, prev){
  if(type == 'move'){
    contrasts <- contrasts %>%
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
                                ifelse(stim_type == 'l', 'lion', 'human')))
    if(prev == TRUE){
      contrasts <- contrasts %>%
        mutate(prev_action = ifelse(move_tminus1 == 'move away directly',
                                    'move away directly',
                                    ifelse(move_tminus1 == 'move away at an angle',
                                           'move away at an angle',
                                           ifelse(move_tminus1 == 'move directly with',
                                                  'neither towards or away',
                                                  ifelse(move_tminus1 == 'approach at an angle',
                                                         'approach at an angle',
                                                         'approach directly'))))) %>%
        mutate(prev_action = factor(prev_action,
                                    levels = c('move away directly',
                                               'move away at an angle',
                                               'neither towards or away',
                                               'approach at an angle',
                                               'approach directly')))
    }
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
                                           'human')))
    if(prev == TRUE){
      contrasts <- contrasts %>%
        mutate(prev_action = factor(prev_action,
                                    levels = c('look directly away',
                                               'side-on',
                                               'look at directly')))
    }
    plot_title <- ifelse(direction == 'away', 'look away',
                         ifelse(direction == 'side', 'side-on',
                                'look towards'))
  }
  
  plot <- contrasts %>% 
    ggplot()+
    geom_hline(yintercept = 0, lty = 3)+
    geom_violin(aes(x = comparison,
                    y = difference,
                    fill = p_age_cat),
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

# #### movement ordinal1 c -- probability of  changing  behaviour ####
# pdf('../outputs/movement_ordinal_model_1/niceplots_movementordinal1.pdf')
# rm(list = ls()[!ls() %in% c('extract_predictions',
#                             'ridge_pred_altogether','ridge_pred_splitpartner',
#                             'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
# load('movement_direction/ordinal_withprev/movement_ordinal_model1_agecontrasts.RData')
# 
# ## plot predictions
# age_move_org$data_id <- 1:nrow(age_move_org)
# for(prediction_matrix in 1:5){
#   pred_slice <- extract_predictions(array = age_mtx_org,
#                                     slice = prediction_matrix,
#                                     df = age_move_org)
#   direction <- ifelse(prediction_matrix == 1,'awaydirect',
#                       ifelse(prediction_matrix == 2,'awayangle',
#                              ifelse(prediction_matrix == 3,'neither',
#                                     ifelse(prediction_matrix == 4,'twdsangle',
#                                            'twdsdirect'))))
#   # boxplot_pred(pred_slice)
#   # density_pred(pred_slice)
#   ridge_pred_altogether(pred_slice, type = 'move_dir', prev = TRUE)
#   ggsave(filename = paste0('mom1_predicted_ridges_',direction,'_altogether.png'),
#          path = '../outputs/movement_ordinal_model_1/',
#          plot = last_plot(),
#          device = 'png', height = 1800, width = 2800, units = 'px')
#   ridge_pred_splitpartner(pred_slice, type = 'move_dir', prev = TRUE)
#   ggsave(filename = paste0('mom1_predicted_ridges_',direction,'_splitpartner.png'),
#          path = '../outputs/movement_ordinal_model_1/',
#          plot = last_plot(),
#          device = 'png', height = 1800, width = 2800, units = 'px')
# }
# 
# ## plot contrasts
# for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
#   contrast_plot_altogether(contrasts_long, type = 'move', direction = plot, free_y = F, prev = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom1_contrasts_fixedscales_',plot,'_altogether.png'),
#          path = '../outputs/movement_ordinal_model_1/',
#          height = 3000, width = 2100, unit = 'px')
# 
#   contrast_plot_altogether(contrasts_long, type = 'move', direction = plot, free_y = T, prev = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom1_contrasts_freescales_',plot,'_altogether.png'),
#          path = '../outputs/movement_ordinal_model_1/',
#          height = 3000, width = 2100, unit = 'px')
# 
#   contrast_plot_splitpartner(contrasts_long, type = 'move', direction = plot, free_y = F, prev = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom1_contrasts_fixedscales_',plot,'_splitpartner.png'),
#          path = '../outputs/movement_ordinal_model_1/',
#          height = 3000, width = 2100, unit = 'px')
# 
#   contrast_plot_splitpartner(contrasts_long, type = 'move', direction = plot, free_y = F, prev = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom1_contrasts_freescales_',plot,'_splitpartner.png'),
#          path = '../outputs/movement_ordinal_model_1/',
#          height = 3000, width = 2100, unit = 'px')
# }
# 
# print('movement ordinal 1 complete')
# dev.off()
# 
# #### movement ordinal2 c -- probability of  changing  behaviour ####
# pdf('../outputs/looking_ordinal_model_2bda/niceplots_movementordinal2.pdf')
# load('movement_direction/ordinal_withprev/moving_ordinal_2bda_agecontrasts.RData')
# rm(age1v2_aa,age1v2_ad,age1v2_n,age1v2_ta,age1v2_td,
#    age1v4_aa,age1v4_ad,age1v4_n,age1v4_ta,age1v4_td,
#    age2v3_aa,age2v3_ad,age2v3_n,age2v3_ta,age2v3_td,
#    age3v4_aa,age3v4_ad,age3v4_n,age3v4_ta,age3v4_td) ; gc()
# rm(age_move_alt,age_new,age_pred,alt_vs_org_awayangle,alt_vs_org_awaydirect,alt_vs_org_neither,alt_vs_org_twdsangle,alt_vs_org_twdsdirect,behav,contrasts) ; gc()
# save.image('movement_direction/ordinal_withprev/moving_ordinal_2bda_ageplotting.RData') #load('movement_direction/ordinal_withprev/moving_ordinal_2bda_ageplotting.RData')
# 
# ## plot predictions
# age_move_org$data_id <- 1:nrow(age_move_org)
# age_move_org$move_tminus1_num <- age_move_org$prev_num
# for(prediction_matrix in 1:5){
#   pred_slice <- extract_predictions(array = age_mtx_org,
#                                     slice = prediction_matrix,
#                                     df = age_move_org)
#   direction <- ifelse(prediction_matrix == 1,'awaydirect',
#                       ifelse(prediction_matrix == 2,'awayangle',
#                              ifelse(prediction_matrix == 3,'neither',
#                                     ifelse(prediction_matrix == 4,'twdsangle',
#                                            'twdsdirect'))))
#   # boxplot_pred(pred_slice)
#   # density_pred(pred_slice)
#   ridge_pred_altogether(pred_slice, type = 'move_dir', prev = TRUE)
#   ggsave(filename = paste0('mom2_predicted_ridges_',direction,'_altogether.png'),
#          path = '../outputs/movement_ordinal_model_2bda/',
#          plot = last_plot(),
#          device = 'png', height = 1800, width = 2800, units = 'px')
#   ridge_pred_splitpartner(pred_slice, type = 'move_dir', prev = TRUE)
#   ggsave(filename = paste0('mom2_predicted_ridges_',direction,'_splitpartner.png'),
#          path = '../outputs/movement_ordinal_model_2bda/',
#          plot = last_plot(),
#          device = 'png', height = 1800, width = 2800, units = 'px')
# }
# 
# ## plot contrasts
# contrasts_long <- contrasts_long %>% 
#   mutate(move_tminus1_num = prev_num,
#          move_tminus1 = prev_action)
# for(plot in c('awaydirect','awayangle','neither','twdsangle','twdsdirect')){
#   contrast_plot_altogether(contrasts_long, type = 'move', direction = plot, free_y = F, prev = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom2_contrasts_fixedscales_',plot,'_altogether.png'),
#          path = '../outputs/movement_ordinal_model_2bda/',
#          height = 3000, width = 2100, unit = 'px')
#   
#   contrast_plot_altogether(contrasts_long, type = 'move', direction = plot, free_y = T, prev = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom2_contrasts_freescales_',plot,'_altogether.png'),
#          path = '../outputs/movement_ordinal_model_2bda/',
#          height = 3000, width = 2100, unit = 'px')
#   
#   contrast_plot_splitpartner(contrasts_long, type = 'move', direction = plot, free_y = F, prev = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom2_contrasts_fixedscales_',plot,'_splitpartner.png'),
#          path = '../outputs/movement_ordinal_model_2bda/',
#          height = 3000, width = 2100, unit = 'px')
#   
#   contrast_plot_splitpartner(contrasts_long, type = 'move', direction = plot, free_y = T, prev = T)
#   ggsave(plot = last_plot(), device = 'png',
#          filename = paste0('mom2_contrasts_freescales_',plot,'_splitpartner.png'),
#          path = '../outputs/movement_ordinal_model_2bda/',
#          height = 3000, width = 2100, unit = 'px')
# }
# 
# print('movement ordinal 2 complete')
# dev.off()
# 
#### movement ordinal2 p -- probability of performing behaviour ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_movementordinal2_noprev.pdf')
rm(list = ls()[!ls() %in% c('extract_predictions',
                            'ridge_pred_altogether','ridge_pred_splitpartner',
                            'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
load('movement_direction/ordinal_noprev/moving_noprev_2bda_modelpredictions.RData')

## plot predictions
dir <- c('move directly away','move away at an angle','move neither towards or away','approach at an angle','approach directly')

for(prediction_direction in 1:5){
  pred_slice <- pred %>% filter(pred_type == dir[prediction_direction])
  direction <- ifelse(prediction_direction == 1,'awaydirect',
                      ifelse(prediction_direction == 2,'awayangle',
                             ifelse(prediction_direction == 3,'neither',
                                    ifelse(prediction_direction == 4,'twdsangle',
                                           'twdsdirect'))))
  # boxplot_pred(pred_slice)
  # density_pred(pred_slice)
  ridge_pred_altogether(pred_slice, type = 'move_dir', prev = FALSE)
  ggsave(filename = paste0('mom2_predicted_ridges_',direction,'_altogether.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
  ridge_pred_splitpartner(pred_slice, type = 'move_dir', prev = FALSE)
  ggsave(filename = paste0('mom2_predicted_ridges_',direction,'_splitpartner.png'),
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
  contrast_plot_altogether(contrasts_long, type = 'move', direction = plot, free_y = F, prev = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_fixedscales_',plot,'_altogether.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_altogether(contrasts_long, type = 'move', direction = plot, free_y = T, prev = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_freescales_',plot,'_altogether.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'move', direction = plot, free_y = F, prev = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'move', direction = plot, free_y = T, prev = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('mom_noprev_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/movement_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}

print('movement ignoring t-1 behaviour complete')
dev.off()

#### looking ordinal 1 c -- probability of  changing  behaviour ####
pdf('../outputs/looking_ordinal_model_1/niceplots_lookingordinal1.pdf')
rm(list = ls()[!ls() %in% c('extract_predictions',
                            'ridge_pred_altogether','ridge_pred_splitpartner',
                            'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
load('looking_direction/looking_ordinal_model1_agecontrasts.RData')
rm(age_new, age_pred, age_pred_all, age_pred_i, alt_vs_org_away, alt_vs_org_side, alt_vs_org_twds, away_12, away_14, away_23, away_34, plot, side_12, side_14, side_23, side_34, twds_12, twds_23, twds_14, twds_34) ; gc()
save.image('looking_direction/looking_ordinal_model1_ageplotting.RData')

## plot predictions
age_look_org$data_id <- 1:nrow(age_look_org)
for(prediction_matrix in 1:3){
  pred_slice <- extract_predictions(array = age_look_org,
                                    slice = prediction_matrix,
                                    df = age_move_org)
  direction <- ifelse(prediction_matrix == 1,'away',
                      ifelse(prediction_matrix == 2,'sideon','towards'))
  # boxplot_pred(pred_slice)
  # density_pred(pred_slice)
  ridge_pred_altogether(pred_slice, type = 'look', prev = TRUE)
  ggsave(filename = paste0('lom1_predicted_ridges_',direction,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_1/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
  ridge_pred_splitpartner(pred_slice, type = 'look', prev = TRUE)
  ggsave(filename = paste0('lom1_predicted_ridges_',direction,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_1/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
}

## plot contrasts
for(plot in c('look away','side on','look at')){
  contrast_plot_altogether(contrasts_long, type = 'look', direction = plot, free_y = F, prev = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom1_contrasts_fixedscales_',plot,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_altogether(contrasts_long, type = 'look', direction = plot, free_y = T, prev = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom1_contrasts_freescales_',plot,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'look', direction = plot, free_y = F, prev = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom1_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'look', direction = plot, free_y = T, prev = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom1_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_1/',
         height = 3000, width = 2100, unit = 'px')
}

print('looking ordinal 1 complete')
dev.off()

#### looking ordinal 2 c -- probability of  changing  behaviour ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_lookingordinal2.pdf')
rm(list = ls()[!ls() %in% c('extract_predictions',
                            'ridge_pred_altogether','ridge_pred_splitpartner',
                            'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
load('looking_direction/looking_ordinal_model2bda_agecontrasts.RData')
rm(list = ls()[! ls() %in% c('age_look_org','contrasts','contrasts_long','look','age_mtx_org')]) ; gc()
save.image('looking_direction/looking_ordinal_model2bda_agecontrasts_plotting_splitpartner.RData') # load('looking_direction/looking_ordinal_model2bda_agecontrasts_plotting_splitpartner.RData')

## plot predictions
age_look_org$data_id <- 1:nrow(age_look_org)
for(prediction_matrix in 1:3){
  pred_slice <- extract_predictions(array = age_look_org,
                                    slice = prediction_matrix,
                                    df = age_move_org)
  direction <- ifelse(prediction_matrix == 1,'away',
                      ifelse(prediction_matrix == 2,'sideon','towards'))
  # boxplot_pred(pred_slice)
  # density_pred(pred_slice)
  ridge_pred_altogether(pred_slice, type = 'look', prev = TRUE)
  ggsave(filename = paste0('lom2_predicted_ridges_',direction,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
  ridge_pred_splitpartner(pred_slice, type = 'look', prev = TRUE)
  ggsave(filename = paste0('lom2_predicted_ridges_',direction,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
}

## plot contrasts
for(plot in c('away','side','twds')){
  contrast_plot_altogether(contrasts_long, type = 'look', direction = plot, free_y = F, prev = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom2_contrasts_fixedscales_',plot,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_altogether(contrasts_long, type = 'look', direction = plot, free_y = T, prev = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom2_contrasts_freescales_',plot,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'look', direction = plot, free_y = F, prev = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom2_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'look', direction = plot, free_y = T, prev = T)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom2_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}

print('looking ordinal 2 complete')
dev.off()

#### looking ordinal 2 p -- probability of performing behaviour ####
pdf('../outputs/looking_ordinal_model_2bda/niceplots_lookingordinal2_noprev.pdf')
rm(list = ls()[!ls() %in% c('extract_predictions',
                            'ridge_pred_altogether','ridge_pred_splitpartner',
                            'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
load('looking_direction/looking_noprev_agecontrasts.RData')

## plot predictions
age_look_org$data_id <- 1:nrow(age_look_org)
for(prediction_matrix in 1:3){
  pred_slice <- extract_predictions(array = age_look_org,
                                    slice = prediction_matrix,
                                    df = age_move_org)
  direction <- ifelse(prediction_matrix == 1,'away',
                      ifelse(prediction_matrix == 2,'sideon','towards'))
  # boxplot_pred(pred_slice)
  # density_pred(pred_slice)
  ridge_pred_altogether(pred_slice, type = 'look', prev = FALSE)
  ggsave(filename = paste0('lom_noprev_predicted_ridges_',direction,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
  ridge_pred_splitpartner(pred_slice, type = 'look', prev = FALSE)
  ggsave(filename = paste0('lom_noprev_predicted_ridges_',direction,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         plot = last_plot(),
         device = 'png', height = 1800, width = 2800, units = 'px')
}

## plot contrasts
for(plot in c('away','side','twds')){
  contrast_plot_altogether(contrasts_long, type = 'look', direction = plot, free_y = F, prev = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom_noprev_contrasts_fixedscales_',plot,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_altogether(contrasts_long, type = 'look', direction = plot, free_y = T, prev = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom_noprev_contrasts_freescales_',plot,'_altogether.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'look', direction = plot, free_y = F, prev = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom_noprev_contrasts_fixedscales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
  
  contrast_plot_splitpartner(contrasts_long, type = 'look', direction = plot, free_y = T, prev = F)
  ggsave(plot = last_plot(), device = 'png',
         filename = paste0('lom_noprev_contrasts_freescales_',plot,'_splitpartner.png'),
         path = '../outputs/looking_ordinal_model_2bda/',
         height = 3000, width = 2100, unit = 'px')
}

print('looking noprev complete')
dev.off()

#### movement binomial c -- probability of  changing  behaviour ####
pdf('../outputs/movement_binomial_model/niceplots_movementbinomial.pdf')
load('movement_direction/binomial_withprev/movement_binomial_agecontrasts.RData')
rm(ctd_vs_human, ctd_vs_lion, lion_vs_human, contrasts, contrasts_long) ; gc()
rm(age_move_alt, age1_vs_age2, age2_vs_age3, age3_vs_age4, age1_vs_age4) ; gc()

## plot predictions
pred <- pred %>%
  mutate(draw_id = rep(1:4000, each = nrow(age_move_org)),
         stim_type = ifelse(stim_type == 'ctd','dove (control)',
                            ifelse(stim_type == 'l','lion','human'))) %>%
  mutate(stim_type = factor(stim_type,
                            levels = c('dove (control)','lion','human')))

# boxplot_pred(pred, type = 'move_prob', prev = TRUE)
# density_pred(pred, type = 'move_prob', prev = TRUE)
ridge_pred_altogether(pred, type = 'move_prob', prev = TRUE)
ggsave(filename = 'mbm_predicted_ridges.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 1800, width = 1500, units = 'px')

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
save.image('movement_direction/binomial_withprev/movement_binomial_ageplotting.RData')

## plot
age_contrast_long %>%
  mutate(prev_move = ifelse(move_tminus1_num == 0, 'not moving at t-1', 'moving at t-1'),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion',
                                   'human'))) %>%
  ggplot()+
  geom_hline(yintercept = 0, lty = 3)+
  geom_violin(aes(x = comparison,
                  y = contrast,
                  fill = prev_move),
              position = position_dodge(0.5))+
  scale_fill_viridis_d()+
  facet_wrap(bda ~ stim_type, scales = 'free_y')+
  labs(fill = 'moving in\nprevious second')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'mbm_violin_contrasts.png',
       path = '../outputs/movement_binomial_model/',
       height = 1600, width = 1600, unit = 'px')

print('movement binomial complete')
dev.off()

#### movement binomial p -- probability of performing behaviour ####
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

# boxplot_pred(pred, type = 'move_prob', prev = FALSE)
# density_pred(pred, type = 'move_prob', prev = FALSE)
ridge_pred_altogether(pred, type = 'move_prob', prev = TRUE)
ggsave(filename = 'mbm_noprev_predicted_ridges.png',
       path = '../outputs/movement_binomial_model/',
       device = 'png', height = 1800, width = 1500, units = 'px')

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
save.image('movement_direction/binomial_noprev/movement_binomial_ageplotting.RData')

## plot
age_contrast_long %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion',
                                   'human'))) %>%
  ggplot()+
  geom_hline(yintercept = 0, lty = 3)+
  geom_violin(aes(x = comparison,
                  y = contrast),
              position = position_dodge(0.5))+
  scale_fill_viridis_d()+
  facet_wrap(. ~ stim_type, scales = 'free_y')+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave(plot = last_plot(), device = 'png',
       filename = 'mbm_violin_contrasts_noprev.png',
       path = '../outputs/movement_binomial_model/',
       height = 1600, width = 1600, unit = 'px')

print('movement binomial complete')
dev.off()

#### nearest neighbour c -- probability of  changing  behaviour ####
pdf('../outputs/neighbour_binomial_model_bda/niceplots_nearestneighbour.pdf')
load('nearest_neighbour/neighbour_binomial_agecontrasts.RData')
rm(list = ls()[! ls() %in% c('contrasts','contrasts_long','age_nn_org','age_mtx_org','nn',
                             'extract_predictions',
                             'ridge_pred_altogether','ridge_pred_splitpartner',
                             'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
print('data loaded')

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
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1','neighbours at t-1')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
  ggplot()+
  geom_boxplot(aes(x = age_combo,
                   # fill = as.factor(move_index), # successfully predicts actual data
                   fill = prev,
                   y = epred))+
  labs(x = 'age category combination',
       y = 'predicted probability\nof being neighbours',
       fill = 'neighbours in previous second')+
  facet_wrap(. ~ stim_type_long)+
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
  facet_grid(prev ~ stim_type_long,
             scales = 'free')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
print('data loaded')

pred %>%
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1','neighbours at t-1')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
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
                          fill = age_combo),
                      alpha = 0.6)+
  labs(fill = 'age category\ncombination',
       x = 'predicted probability\nof being neighbours',
       y = 'probability density')+
  facet_grid(stim_type_long ~ prev,
             scales = 'free_x')+
  scale_fill_viridis_d()+
  theme(legend.position = 'bottom')
print('ridges produced')
ggsave(filename = 'nbm_predicted_ridges.png',
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
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = combo2),
              alpha = 0.6)+
  scale_fill_viridis_d()+
  facet_grid(stim_type ~ prev)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')
contrasts_long %>%
  filter(combo1 %in% c(21,22,23,24)) %>%
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = combo2),
              alpha = 0.6)+
  scale_fill_viridis_d()+
  facet_grid(stim_type ~ prev)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')
contrasts_long %>%
  filter(combo1 %in% c(31,32,33,34)) %>%
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = combo2),
              alpha = 0.6)+
  scale_fill_viridis_d()+
  facet_grid(stim_type ~ prev)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')
contrasts_long %>%
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1', 'not neighbours at t-1'))) %>%
  filter(combo1 %in% c(41,42,43,44)) %>%
  ggplot()+
  geom_violin(aes(x = comparison,
                  y = difference,
                  fill = combo2),
              alpha = 0.6)+
  scale_fill_viridis_d()+
  facet_grid(stim_type ~ prev)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none')

contrasts_long <- contrasts_long %>%
  mutate(combo1 = as.integer(combo1),
         combo2 = as.integer(combo2)) %>%
  mutate(change = combo2 - combo1)

contrasts_long %>%
  filter(change %in% c(1,10)) %>%
  filter(combo1 %in% c(11,12,13,14)) %>%
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1'),
         change = ifelse(change == 1, 'partner age increased', 'focal age increased')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1',
                                        'not neighbours at t-1')),
         change = factor(change, levels = c('partner age increased',
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
  facet_grid(prev ~ stim_type, scales = 'free_y')+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'bottom')

contrasts_long %>%
  filter(change %in% c(1,10)) %>%
  filter(combo1 %in% c(11,12,13,14)) %>%
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1'),
         change = ifelse(change == 1, 'partner age increased', 'focal age increased')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1',
                                        'not neighbours at t-1')),
         change = factor(change, levels = c('partner age increased',
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
  facet_grid(prev ~ stim_type)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'bottom')

plot <- contrasts_long %>%
  filter(change %in% c(1,10)) %>%
  mutate(prev = ifelse(prev == 0, 'not neighbours at t-1', 'neighbours at t-1'),
         change = ifelse(change == 1, 'partner age increased', 'focal age increased')) %>%
  mutate(prev = factor(prev, levels = c('neighbours at t-1',
                                        'not neighbours at t-1')),
         change = factor(change, levels = c('partner age increased',
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
    facet_grid(. ~ prev, scales = 'free_x')+
    labs(title = 'dove (control)')+
    theme(axis.text.x = element_text(angle = 90),
          legend.position = 'bottom'))
ggsave(plot = dove, device = 'png',
       filename = 'neighbour_contrasts_freescale_ctd.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')
dove + scale_x_continuous(limits = c(-0.012, 0.012),
                          name = 'contrast (mean per datum predicted)')
ggsave(plot = last_plot(), device = 'png',
       filename = 'neighbour_contrasts_setscale_ctd.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')

(lion <- plot %>%
    filter(stim_type == 'lion') %>%
    ggplot()+
    geom_density_ridges(aes(x = difference,
                            y = comparison,
                            fill = change, colour = change),
                        #rel_min_height = 0.001,
                        alpha = 0.6)+
    scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
    scale_x_continuous(name = 'contrast (mean per datum predicted)')+
    facet_grid(. ~ prev, scales = 'free_x')+
    labs(title = 'lion')+
    theme(axis.text.x = element_text(angle = 90),
          legend.position = 'bottom'))
ggsave(plot = lion, device = 'png',
       filename = 'neighbour_contrasts_freescale_l.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')
lion + scale_x_continuous(limits = c(-0.02, 0.02),
                          name = 'contrast (mean per datum predicted)')
ggsave(plot = last_plot(), device = 'png',
       filename = 'neighbour_contrasts_setscale_l.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')

(human <- plot %>%
    filter(stim_type == 'human') %>%
    ggplot()+
    geom_density_ridges(aes(x = difference,
                            y = comparison,
                            fill = change, colour = change),
                        #rel_min_height = 0.001,
                        alpha = 0.6)+
    scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
    scale_x_continuous(name = 'contrast (mean per datum predicted)')+
    facet_grid(. ~ prev, scales = 'free_x')+
    labs(title = 'human')+
    theme(axis.text.x = element_text(angle = 90),
          legend.position = 'bottom'))
ggsave(plot = human, device = 'png',
       filename = 'neighbour_contrasts_freescale_h.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')
human + scale_x_continuous(limits = c(-0.02, 0.02),
                          name = 'contrast (mean per datum predicted)')
ggsave(plot = last_plot(), device = 'png',
       filename = 'neighbour_contrasts_setscale_h.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')

(prev0 <- plot %>%
    filter(prev == 'not neighbours at t-1') %>%
    ggplot()+
    geom_density_ridges(aes(x = difference,
                            y = comparison,
                            fill = change, colour = change),
                        #rel_min_height = 0.001,
                        alpha = 0.6)+
    geom_vline(xintercept = 0, lty = 2)+
    scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
    scale_x_continuous(name = 'contrast (mean per datum predicted)')+
    facet_grid(. ~ stim_type, scales = 'free_x')+
    labs(title = 'not neighbours at t-1')+
    theme(axis.text.x = element_text(angle = 90),
          legend.position = 'bottom'))
ggsave(plot = prev0, device = 'png',
       filename = 'neighbour_contrasts_fullscale_prev0.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')
prev0 +
  scale_x_continuous(limits = c(-0.005, 0.005),
                          name = 'contrast (mean per datum predicted)')
ggsave(plot = last_plot(), device = 'png',
       filename = 'neighbour_contrasts_setscale_prev0.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')

(prev1 <- plot %>%
    filter(prev == 'neighbours at t-1') %>%
    ggplot()+
    geom_vline(xintercept = 0, lty = 2)+
    geom_density_ridges(aes(x = difference,
                            y = comparison,
                            fill = change, colour = change),
                        #rel_min_height = 0.001,
                        alpha = 0.6)+
    scale_fill_viridis_d(end = 0.5)+ scale_colour_viridis_d(end = 0.5)+
    scale_x_continuous(name = 'contrast (mean per datum predicted)')+
    facet_grid(. ~ stim_type, scales = 'free_x')+
    labs(title = 'neighbours at t-1')+
    theme(axis.text.x = element_text(angle = 90),
          legend.position = 'bottom'))
ggsave(plot = prev1, device = 'png',
       filename = 'neighbour_contrasts_fullscale_prev1.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 2100, width = 1800, unit = 'px')

dev.off()

#### nearest neighbour p -- probability of performing behaviour ####
pdf('../outputs/neighbour_binomial_model_bda/niceplots_nearestneighbour_noprev.pdf')
load('nearest_neighbour/neighbour_noprev_agecontrasts.RData')
rm(list = ls()[! ls() %in% c('contrasts','contrasts_long','age_nn_org','age_mtx_org','nn',
                             'extract_predictions',
                             'ridge_pred_altogether','ridge_pred_splitpartner',
                             'contrast_plot_altogether','contrast_plot_splitpartner')]) ; gc()
print('data loaded')

save.image('nearest_neighbour/neighbour_noprev_agecontrasts_plotting.RData') # load('nearest_neighbour/neighbour_noprev_agecontrasts_plotting.RData')

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
                                 levels = c('dove (control)','lion','human'))) %>% 
  separate(age_combo, into = c('f_age_num','p_age_num'), remove = F) %>% 
  mutate(f_age_cat = ifelse(f_age_num == '1', '10-15 yrs',
                            ifelse(f_age_num == '2', '16-20 yrs',
                                   ifelse(f_age_num == '3', '21-25 yrs', '26-35 yrs'))),
         p_age_cat = ifelse(p_age_num == '1', '10-15 yrs',
                            ifelse(p_age_num == '2', '16-20 yrs',
                                   ifelse(p_age_num == '3', '21-25 yrs', '26-35 yrs'))),
         f_age_num = as.numeric(f_age_num),
         p_age_num = as.numeric(p_age_num))
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
  labs(fill = 'focal age category',
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
