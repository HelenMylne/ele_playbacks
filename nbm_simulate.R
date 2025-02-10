#### simulate NBM to check offset works ####
library(tidyverse)
library(brms)
library(LaplacesDemon)
set.seed(12345)

# #### simulate population ####
# n_eles <- 171
# n_exp <- 48
# p_old <- 0.6
# 
# nodes <- data.frame(focal = 1:n_eles,
#                     pb_num = c(rep(1:n_exp, each = 2),
#                                sample(x = 2:n_exp,
#                                       size = (n_eles - n_exp*2),
#                                       replace = T)),
#                     old = rbinom(n = n_eles, size = 1, prob = p_old)) %>% 
#   mutate(age = ifelse(old == 0, 'Y', 'O'))
# 
# dyads <- data.frame(focal = c(nodes$focal[1],    nodes$focal[2]),
#                     partner = c(nodes$focal[2],  nodes$focal[1]),
#                     pb_num = c(nodes$pb_num[1],  nodes$pb_num[1]),
#                     age_focal = c(nodes$age[1],  nodes$age[2]),
#                     age_partner = c(nodes$age[2],nodes$age[1]))
# for(i in 3:nrow(nodes)){
#   pb <- nodes %>% 
#     filter(pb_num == pb_num[i]) %>% 
#     mutate(partner = focal) %>% 
#     mutate(age_focal = age,
#            age_partner = age)
#   node <- nodes %>% 
#     filter(focal == focal[i])
#   
#   new_data <- expand_grid(focal = pb$focal,
#                           partner = pb$focal,
#                           pb_num = pb$pb_num[1]) %>% 
#     filter(focal != partner) %>% 
#     left_join(pb[,c('focal','age_focal')], by = 'focal') %>% 
#     left_join(pb[,c('partner','age_partner')], by = 'partner')
#   
#   dyads <- rbind(dyads, new_data)
# }
# dyads <- dyads %>% 
#   distinct()
# 
# 
# max(table(nodes$pb_num))
# nodes$n1 <- NA ; nodes$n2 <- NA ; nodes$n3 <- NA ; nodes$n4 <- NA ; nodes$n5 <- NA
# nodes$n6 <- NA ; nodes$n7 <- NA ; nodes$n8 <- NA ; nodes$n9 <- NA ; nodes$n10 <- NA
# 
# for(i in 1:nrow(nodes)){
#   pb <- nodes %>% 
#     filter(pb_num == pb_num[i])
#   
#   neighbour <- sample(x = as.character(pb$focal[which(pb$focal != nodes$focal[i])]),
#                       size = 180, replace = T)
#   
#   n <- as.data.frame(table(neighbour)) %>% 
#     mutate(neighbour = as.integer(neighbour))
#   
#   for(j in 1:10){
#     if(nrow(n) == j){
#       nodes[i,5:(j+4)] <- n$Freq
#     }
#   }
# }
# 
# nodes_long <- nodes %>% 
#   pivot_longer(cols = `n1`:`n10`,
#                names_to = 'n',
#                values_to = 'count') %>% 
#   filter(! is.na(count) )
# 
# dyads$n <- NA
# for(i in 1:nrow(nodes)){
#   pb <- nodes %>% 
#     filter(pb_num == nodes$pb_num[i]) %>% 
#     filter(focal != nodes$focal[i]) %>% 
#     dplyr::select(focal, pb_num, age)
#   pb$n <- 1:nrow(pb)
#   pb <- pb %>% 
#     mutate(n = paste0('n',n)) %>% 
#     rename(partner = focal,
#            age_partner = age)
#   
#   dyads$n[which(dyads$focal == nodes$focal[i])] <- pb$n
# }
# 
# dyads <- dyads %>% 
#   left_join(nodes_long[,c('focal','pb_num','n','count')],
#             by = c('focal','pb_num','n')) %>% 
#   mutate(ele_combo = paste0(focal,'_',partner))
# 
# sim <- expand.grid(ele_combo = dyads$ele_combo,
#                    second = 1:180) %>% 
#   left_join(dyads, by = 'ele_combo')
# 
# sim$neighbour <- NA
# for(i in 1:nrow(nodes)){
#   focal <- sim %>% 
#     filter(focal == nodes$focal[i])
#   sim <- sim %>% 
#     anti_join(focal)
#   
#   n_partners <- length(unique(focal$partner))
#   focal$partner_id <- as.integer(as.factor(focal$partner))
#   
#   for(j in 1:n_partners){
#     partner <- focal %>% 
#       filter(partner_id == j)
#     focal <- focal %>% 
#       anti_join(partner)
#     prev_partners <- focal %>% 
#       filter(partner_id < j) %>% 
#       dplyr::select(ele_combo, count) %>% 
#       distinct()
#     prev_secs <- sum(prev_partners$count)
#     next_partners <- focal %>% 
#       filter(partner_id > j) %>% 
#       dplyr::select(ele_combo, count) %>% 
#       distinct()
#     next_secs <- sum(next_partners$count)
#     partner$neighbour <- c(rep(0, prev_secs),
#                            rep(1, partner$count[1]),
#                            rep(0, next_secs))
#     focal <- rbind(partner,focal)
#   }
#   focal <- focal %>% 
#     dplyr::select(-partner_id)
#   sim <- rbind(focal, sim)
# }
# 
# stim <- data.frame(pb_num = 1:n_exp) %>% 
#   mutate(stim_type = ifelse(pb_num < 17, 'ctd',
#                             ifelse(pb_num < 33, 'l', 'h'))) %>% 
#   mutate(stim_num = NA)
# for(i in c('ctd','l','h')){
#   if(i == 'ctd'){ x <- 1:10 }
#   if(i == 'l'){ x <- 11:20 }
#   if(i == 'h'){ x <- 21:30 }
#   
#   stim$stim_num[stim$stim_type == i] <- c(x, sample(x, size = 6, replace = F))
# }
# 
# sim <- sim %>% 
#   mutate(bda = ifelse(second < 60, 'before',
#                       ifelse(second < 75, 'during', 'after')),
#          age_rel = paste0(age_focal, age_partner)) %>%
#   left_join(stim, by = 'pb_num') %>%
#   dplyr::select(-ele_combo, -n, -count) %>%
#   mutate(prop_agerel_popn = ifelse(age_rel == 'OO', p_old^2,
#                                    ifelse(age_rel == 'YY', (1-p_old)^2,
#                                           p_old * (1-p_old)))) %>% 
#   mutate(logit_prop_agerel_popn = logit(prop_agerel_popn)) %>% 
#   rename(action = neighbour)
# 
# #### run model ####
# rm(list = ls()[! ls() %in% c('sim','p_old')]) ; gc()
# num_iter <- 2000
# num_chains <- 4
# 
# priors <- c(
#   # age combination
#   prior(normal(0,1),          class = b,  coef = age_relOY),
#   prior(normal(0,1),          class = b,  coef = age_relYO),
#   prior(normal(0,1),          class = b,  coef = age_relYY),
#   # stim type
#   prior(normal(0,1),          class = b,  coef = stim_typeh),
#   prior(normal(0,1),          class = b,  coef = stim_typel),
#   # before/during/after
#   prior(normal(0,1),          class = b,  coef = bdabefore),
#   prior(normal(0,1),          class = b,  coef = bdaduring),
#   # interaction
#   prior(normal(0,1),          class = b,  coef = stim_typeh:bdabefore),
#   prior(normal(0,1),          class = b,  coef = stim_typeh:bdaduring),
#   prior(normal(0,1),          class = b,  coef = stim_typel:bdabefore),
#   prior(normal(0,1),          class = b,  coef = stim_typel:bdaduring),
#   # random effects / intercepts
#   prior(student_t(3, 0, 0.5), class = sd, group = focal),
#   prior(student_t(3, 0, 0.5), class = sd, group = pb_num),
#   prior(student_t(3, 0, 0.5), class = sd, group = stim_num),
#   prior(student_t(3, 0, 1),   class = Intercept))
# 
# sim_fit <- brm(
#   formula = action ~ 1 + offset(logit_prop_agerel_popn) + age_rel + stim_type * bda +
#     (1|focal) + (1|stim_num) + (1|pb_num),
#   data = sim, family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   control = list(adapt_delta = 0.9,
#                  max_treedepth = 10))
# 
# 
# save.image('nearest_neighbour/neighbour_sim_run.RData')
# 
#### calculate posterior contrasts from predictions ####
load('nearest_neighbour/neighbour_sim_run.RData')
(summary <- summary(sim_fit))

## redo predictions with altered age combinations
age_predict <- function(df, age_combination, model){
  df_new <- df %>%
    mutate(age_rel = age_combination)
  df_new$logit_prop_agerel_popn <- unique(df$logit_prop_agerel_popn[df$age_rel == age_combination])
  mtx <- posterior_epred(object = model, newdata = df_new)
  colnames(mtx) <- df_new$unique_data_combo
  mtx <- mtx[c(1:100,1001:1100,2001:2100,3001:3100),]
  return(mtx)
}
mtx_OO <- age_predict(df = sim, age_combination = 'OO', model = sim_fit)
mtx_OY <- age_predict(df = sim, age_combination = 'OY', model = sim_fit)
mtx_YO <- age_predict(df = sim, age_combination = 'YO', model = sim_fit)
mtx_YY <- age_predict(df = sim, age_combination = 'YY', model = sim_fit)

save.image('nearest_neighbour/neighbour_sim_contrasts.RData')

## calculate contrasts
contrast_YYvYO <- mtx_YY - mtx_YO
contrast_YYvOY <- mtx_YY - mtx_OY
contrast_YYvOO <- mtx_YY - mtx_OO
contrast_YOvOY <- mtx_YO - mtx_OY
contrast_YOvOO <- mtx_YO - mtx_OO
contrast_OYvOO <- mtx_OY - mtx_OO

## produce values for reporting
print('YY vs YO')
mean(contrast_YYvYO); sd(contrast_YYvYO)
quantile(contrast_YYvYO, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YYvYO < 0)) / length(contrast_YYvYO)

print('YY vs OY')
mean(contrast_YYvOY); sd(contrast_YYvOY)
quantile(contrast_YYvOY, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YYvOY < 0)) / length(contrast_YYvOY)

print('YY vs OO')
mean(contrast_YYvOO); sd(contrast_YYvOO)
quantile(contrast_YYvOO, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YYvOO < 0)) / length(contrast_YYvOO)

print('YO vs OY')
mean(contrast_YOvOY); sd(contrast_YOvOY)
quantile(contrast_YOvOY, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YOvOY < 0)) / length(contrast_YOvOY)

print('YO vs OO')
mean(contrast_YOvOO); sd(contrast_YOvOO)
quantile(contrast_YOvOO, prob = c(0.025, 0.5, 0.975))
length(which(contrast_YOvOO < 0)) / length(contrast_YOvOO)

print('OY vs OO')
mean(contrast_OYvOO); sd(contrast_OYvOO)
quantile(contrast_OYvOO, prob = c(0.025, 0.5, 0.975))
length(which(contrast_OYvOO < 0)) / length(contrast_OYvOO)

