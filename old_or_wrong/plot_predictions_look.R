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

#### predictions      ####
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

## plot contrasts       ####
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

dev.off()