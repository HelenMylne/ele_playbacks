## shrink size of predictions
library(tidyverse)
set.seed(12345)
theme_set(theme_bw())

load('nearest_neighbour/neighbour_noprev_nullcompare.RData')
rm(list = ls()[! ls() %in% c('pred_all','nn','num_chains','num_iter')]) ; gc()

num_chains <- 4
num_iter <- 1000

pred_all <- pred_all %>%
  mutate(iteration = rep(rep(1:num_iter, each = nrow(nn)), (num_chains*2) ) )

mid <- sample(x = (1:(num_chains*num_iter)), size = 2000, replace = F)
pred_all <- pred_all %>%
  filter(iteration %in% mid)

gc()
save.image('nearest_neighbour/neighbour_noprev_nullcompare_smallfiles.RData')

# tiny <- sample(x = mid, size = 40, replace = F)
# pred_all <- pred_all %>% 
#   filter(iteration %in% tiny)
# 
# gc()
# save.image('nearest_neighbour/neighbour_noprev_nullcompare_testtinyfiles.RData')
# 
##### test overlap code #####
# load('nearest_neighbour/neighbour_noprev_nullcompare_smallfiles.RData')
predictions <- pred_all %>% 
  filter(model_type == 'live')

predictions_null <- pred_all %>% 
  filter(model_type == 'null')

(null_lwr <- quantile(x = predictions_null$epred, probs = 0.025))
(null_upr <- quantile(x = predictions_null$epred, probs = 0.975))

(live_lwr <- quantile(x = predictions$epred, probs = 0.025))
(live_upr <- quantile(x = predictions$epred, probs = 0.975))

length(which(predictions$epred > null_lwr)) / nrow(predictions)
length(which(predictions$epred < null_upr)) / nrow(predictions)

length(which(predictions$epred > null_lwr & predictions$epred < null_upr)) / nrow(predictions)

all <- pred_all %>% 
  mutate(model_type = ifelse(model_type == 'live',
                             'relative age + stimulus * time',
                             'stimulus * time'),
         stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human')),
         f_age_cat = paste0('F: ',f_age_cat),
         p_age_cat = paste0('T: ',p_age_cat)) %>% 
  mutate(stim_type = factor(stim_type,
                            levels = c('dove (control)','lion','human')),
         bda = factor(bda, levels = c('before','during','after'))) %>% 
  ggplot()+
  geom_density(aes(x = epred,
                   fill = model_type,
                   colour = model_type),
               alpha = 0.5)+
  scale_colour_viridis_d(end = 0.5)+
  scale_fill_viridis_d(end = 0.5)+
  labs(x = 'predicted probability of being neighbours',
       fill = 'model type',
       colour = 'model type')
ggsave(plot = all, device = 'png',
       filename = 'compare_neighbour_null.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 1200, width = 1600, units = 'px')

facet_stim <- all + 
  facet_grid(bda ~ stim_type)+
  theme(legend.position = 'bottom')
ggsave(plot = facet_stim, device = 'png',
       filename = 'compare_neighbour_null_facetstim.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 1600, width = 2000, units = 'px')

facet_age <- all + 
  facet_grid(p_age_cat ~ f_age_cat)+
  theme(legend.position = 'bottom')
ggsave(plot = facet_age, device = 'png',
       filename = 'compare_neighbour_null_facetage.png',
       path = '../outputs/neighbour_binomial_model_bda/',
       height = 1600, width = 2400, units = 'px')
