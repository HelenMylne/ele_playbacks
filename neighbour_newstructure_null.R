#### information                                                     ####
# same models as before, but removing effect of previous second

#### set up                                                          ####
#library(StanHeaders);library(rstan);library(tidyverse);library(brms);library(LaplacesDemon)

library(StanHeaders, lib.loc = '../../packages/')
library(rstan, lib.loc = '../../packages/')

library(tidyverse, lib.loc = '../../packages/')
library(brms, lib.loc = '../../packages/')
library(LaplacesDemon, lib.loc = '../../packages/')

theme_set(theme_bw())

set.seed(12345)

######## nearest neighbour                                           ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_newstructure_null.pdf')
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')

## remove individuals where ages are unknown
behav <- behav %>%
  filter(!is.na(f_age_num))

## set parameters
num_chains <- 4
num_iter <- 2000

#### create data                                                     ####
## select specific data
nn <- behav %>%
  filter(activity == 'nn') %>%
  select(-activity, -stim_start, -stim_stop) %>%
  mutate(prev = NA,
         action = ifelse(action_name == 'out_of_sight', 9,
                         action_index - 1),
         f_age_num = as.factor(as.numeric(f_age_num)),
         p_age_num = as.factor(as.numeric(p_age_num))) %>%
  filter(!is.na(p_age_num)) %>%
  relocate(action, .after = action_index)

# create variable for nearest neighbour at time t-1
focals <- unique(nn$focal)
for(f in 1:length(focals)){
  focal <- nn %>% filter(focal == focals[f])
  nn <- nn %>% anti_join(focal, by = 'focal')
  partners <- unique(focal$partner)
  for(p in 1:length(partners)){
    focal_partner <- focal %>% filter(partner == partners[p])
    focal <- focal %>% anti_join(focal_partner, by = 'partner')
    for(i in 2:nrow(focal_partner)){
      focal_partner$prev[i] <- focal_partner$action[i-1]
    }
    focal <- rbind(focal, focal_partner)
  }
  nn <- rbind(nn, focal)
}
rm(focal, focals, focal_partner, f, p, i, partners) ; gc()

## remove observations with missing data
nn <- nn %>%
  filter(action != 9) %>%
  filter(prev != 9) %>%
  filter(!is.na(prev))

# measure percentages of time spent in each state for report
nn <- nn %>%
  mutate(age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num), 'partner younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num), 'age matched',
                                        'partner older')))
round(prop.table(table(nn$age_difference[nn$bda == 'before']))*100,2)
# age matched   partner older partner younger
# 31.95           34.89           33.16
round(prop.table(table(nn$age_difference[nn$bda == 'during']))*100,2)
# age matched   partner older partner younger
# 31.81           34.19           34.00
round(prop.table(table(nn$age_difference[nn$bda == 'after']))*100,2)
# age matched   partner older partner younger
# 32.94           32.93           34.13

#### new model: change age combo to reduce number of categories      ####
nn <- nn %>%
  mutate(f_age_wide = ifelse(as.numeric(f_age_num) < 3, 'Y', 'O'),
         p_age_wide = ifelse(as.numeric(p_age_num) < 3, 'Y', 'O')) %>%
  mutate(age_rel = paste0(f_age_wide, p_age_wide)) %>%
  filter(f_age_cat != 'unkage')
sort(unique(nn$age_rel))

## plot raw data
ggplot(nn)+
  geom_bar(aes(x = age_rel, fill = action_name))+
  scale_x_discrete(name = 'relative ages')+
  scale_fill_viridis_d(end = 0.5)+
  labs(fill = 'neighbours')+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,65000))

nn_prop <- table(nn$age_rel, nn$action_name) %>% 
  as.data.frame() %>% 
  rename(age_rel = Var1,
         neighbours = Var2,
         count = Freq) %>% 
  mutate(neighbours = ifelse(neighbours == 0, 'no','yes'),
         prop = NA) %>% 
  mutate(neighbours = factor(neighbours, levels = c('yes','no')))
for(i in 1:nrow(nn_prop)){
  nn_prop$prop[i] <- nn_prop$count[i] / sum(nn_prop$count[nn_prop$age_rel == nn_prop$age_rel[i]])
}

ggplot(nn_prop)+
  geom_col(aes(x = age_rel, fill = neighbours, y = prop))+
  scale_x_discrete(name = 'relative ages')+
  scale_fill_viridis_d(end = 0.5)+
  labs(fill = 'neighbours')+
  scale_y_continuous(expand = c(0,0),
                     name = 'proportion')

dyads <- nn %>% 
  select(pb_num, focal, partner, f_age_wide, p_age_wide, age_rel) %>% 
  distinct()
table(dyads$age_rel)

nodes <- nn %>% 
  select(pb_num, focal, f_age_wide) %>% 
  distinct()
table(nodes$f_age_wide)

groups <- nn %>% 
  select(pb_num, f_age_wide) %>% 
  distinct()
table(groups$f_age_wide, groups$pb_num)

playbacks <- nn %>% 
  select(pb_num) %>% 
  distinct() %>% 
  mutate(prop_old = NA,
         group_size = NA)
for(i in 1:nrow(playbacks)){
  pb <- nodes %>% 
    filter(pb_num == playbacks$pb_num[i])
  playbacks$prop_old[i] <- length(which(pb$f_age_wide == 'O')) / nrow(pb)
  playbacks$group_size[i] <- nrow(pb)
}

ggplot(playbacks)+
  geom_point(aes(x = group_size, y = prop_old))

nodes <- nodes %>% 
  filter(pb_num %in% nn$pb_num) %>% 
  mutate(num_old_options = NA,
         prop_old_options = NA)
for(i in 1:nrow(nodes)){
  pb <- nodes %>% 
    filter(pb_num == nodes$pb_num[i]) %>% 
    filter(focal != nodes$focal[i])
  nodes$num_old_options[i] <- length(which(pb$f_age_wide == 'O'))
  nodes$prop_old_options[i] <- nodes$num_old_options[i] / nrow(pb)
}

nn <- nn %>% 
  left_join(nodes, by = c('pb_num','focal','f_age_wide')) %>% 
  left_join(playbacks, by = 'pb_num')

#### new format of model: neighbour age ~ focal age                  ####
neighbours <- nn %>%
  filter(action == 1) %>%
  dplyr::select(pb_num,focal,partner,stim_num,stim_type,bda,
                f_age_cat,p_age_cat,f_age_wide,p_age_wide,f_age_num,p_age_num,
                group_size,prop_old,prop_old_options) %>%
  rename(f_age = f_age_wide,
         n_age = p_age_wide,
         n_age_cat = p_age_cat,
         n_age_num = p_age_num) %>%
  mutate(n_old = ifelse(n_age == 'Y',0,1)) %>%
  mutate(prop_old_invl = LaplacesDemon::invlogit(prop_old_options))
str(neighbours)

######## RUN NULL MODEL TO COMPARE CREDIBLE INTERVALS                ####
rm(list = ls()[! ls() %in% c('nn','behav','neighbours','num_iter','num_chains')]) ; gc()

nodes <- neighbours %>% 
  dplyr::select(pb_num, focal, f_age) %>% 
  distinct()
table(nodes$pb_num)
(t1 <- as.data.frame(table(nodes$f_age)))
(t2 <- prop.table(table(nodes$f_age)))

nodes <- nodes %>% 
  mutate(num_old_popn = ifelse(f_age == 'O',
                               t1$Freq[t1$Var1 == 'O'] - 1,
                               t1$Freq[t1$Var1 == 'O']),
         num_young_popn = ifelse(f_age == 'Y',
                                 t1$Freq[t1$Var1 == 'Y'] - 1,
                                 t1$Freq[t1$Var1 == 'Y'])) %>% 
  mutate(prop_old_popn = num_old_popn / (num_old_popn + num_young_popn))

neighbours <- neighbours %>% 
  left_join(nodes, by = c('pb_num','focal','f_age')) %>% 
  mutate(logit_old_popn = logit(prop_old_popn))

nn_list <- list(num_data = nrow(neighbours),
                # prop_old_popn = neighbours$prop_old_popn,
                num_old = neighbours$num_old_popn,
                num_young = neighbours$num_young_popn,
                neighbour_age = ifelse(neighbours$n_age == 'O', 1, 0),
                y = ifelse(neighbours$n_age == 'O', 1, 0))

null_model <- stan_model('neighbour_null.stan')
null <- sampling(object = null_model, data = nn_list,
                 chains = num_chains, cores = num_chains,
                 # algorithm = "Fixed_param",
                 warmup = num_iter/2, iter = num_iter)
# stan(file = 'neighbour_null.stan', model_name = "null",
#      data = nn_list, pars = NA, seed = 12345,
#      chains = 1, cores = 1,
#      iter = num_iter, warmup = num_iter/2,
#      # control = list(adapt_delta = 0.95),
#      # init = "random", seed = sample.int(.Machine$integer.max, 1),
#      # sample_file = NULL, diagnostic_file = NULL,
#      # save_dso = TRUE, verbose = FALSE, include = TRUE,
#      algorithm = "Fixed_param")

save.image('nearest_neighbour/neighbour_newstructure_null_modelrun.RData')

summary <- summary(null)
summary$summary

## extract posterior distribution of theta
draws <- extract(null)
theta <- data.frame(draw = 1:4000,
                    chain1 = rep(1:4, 1000), chain2 = rep(1:4, each = 1000), # not sure which is right so check both!
                    iter1 = rep(1:1000, each = 4), iter2 = rep(1:1000, 4),   # not sure which is right so check both!
                    theta = draws$theta)
## traceplot
ggplot(theta)+
  geom_line(aes(x = iter1, colour = as.factor(chain1), y = theta))+
  scale_color_viridis_d()+
  theme_bw()+
  theme(legend.position = 'none')+
  facet_wrap(as.factor(chain1) ~ .)

ggplot(theta)+
  geom_line(aes(x = iter2, colour = as.factor(chain2), y = theta))+
  scale_color_viridis_d()+
  theme_bw()+
  theme(legend.position = 'none')+
  facet_wrap(as.factor(chain2) ~ .)

save.image('nearest_neighbour/neighbour_newstructure_null_modelrun.RData')
# load('nearest_neighbour/neighbour_newstructure_null_modelrun.RData')

## extract generated quantities
z <- draws$z
head(z[,1:10)]










