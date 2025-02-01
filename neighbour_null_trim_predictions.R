## shrink size of predictions
library(tidyverse)
set.seed(12345)

load('nearest_neighbour/neighbour_noprev_nullcompare.RData')
rm(list = ls()[! ls() %in% c('pred_all','nn','num_chains','num_iter')]) ; gc()

num_chains <- 4
num_iter <- 1000

pred_all <- pred_all %>% 
  mutate(iteration = rep(rep(1:num_iter, each = nrow(nn)), (num_chains*2) ) )

mid <- sample(x = (1:(num_chains*num_iter)), size = 1000, replace = F)
pred_all <- pred_all %>% 
  filter(iteration %in% mid)

gc()
save.image('nearest_neighbour/neighbour_noprev_nullcompare_smallfiles.RData')

tiny <- sample(x = mid, size = 40, replace = F)
pred_all <- pred_all %>% 
  filter(iteration %in% tiny)

gc()
save.image('nearest_neighbour/neighbour_noprev_nullcompare_testtinyfiles.RData')

##### test overlap code #####
predictions <- tiny %>% 
  filter(model_type == 'live')

predictions_null <- tiny %>% 
  filter(model_type == 'null')

(null_lwr <- quantile(x = predictions_null$epred, probs = 0.025))
(null_upr <- quantile(x = predictions_null$epred, probs = 0.975))

(live_lwr <- quantile(x = predictions$epred, probs = 0.025))
(live_upr <- quantile(x = predictions$epred, probs = 0.975))

length(which(predictions$epred > null_lwr)) / nrow(predictions)
length(which(predictions$epred < null_upr)) / nrow(predictions)

length(which(predictions$epred > null_lwr & predictions$epred < null_upr)) / nrow(predictions)
