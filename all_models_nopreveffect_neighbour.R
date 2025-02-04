#### information                                                ####
# same models as before, but removing effect of previous second

#### set up                                                     ####
library(tidyverse)
library(brms)
library(LaplacesDemon)
library(patchwork)
library(ggridges)
library(vctrs)

theme_set(theme_bw())

set.seed(12345)

######## import data about playbacks                            ####
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')

## remove individuals where ages are unknown
behav <- behav %>%
  filter(!is.na(f_age_num))

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

#### new model: change age combo to reduce number of categories ####
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

#### set prior                                                  ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelchecks_null.pdf')

get_prior(formula = action ~ 1 + stim_type * bda +
            (1|focal) + (1|stim_num) + (1|pb_num),
          data = nn, family = bernoulli("logit"))
#                prior     class                 coef    group resp dpar nlpar lb ub       source
#               (flat)         b                                                          default
#               (flat)         b            bdabefore                                (vectorized)
#               (flat)         b            bdaduring                                (vectorized)
#               (flat)         b           stim_typeh                                (vectorized)
#               (flat)         b stim_typeh:bdabefore                                (vectorized)
#               (flat)         b stim_typeh:bdaduring                                (vectorized)
#               (flat)         b           stim_typel                                (vectorized)
#               (flat)         b stim_typel:bdabefore                                (vectorized)
#               (flat)         b stim_typel:bdaduring                                (vectorized)
# student_t(3, 0, 2.5) Intercept                                                          default
# student_t(3, 0, 2.5)        sd                                                0         default
# student_t(3, 0, 2.5)        sd                         focal                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept    focal                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                        pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept   pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                      stim_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept stim_num                  0    (vectorized)

priors <- c(
  # stim type
  prior(normal(0,1),          class = b,  coef = stim_typeh),
  prior(normal(0,1),          class = b,  coef = stim_typel),
  # before/during/after
  prior(normal(0,1),          class = b,  coef = bdabefore),
  prior(normal(0,1),          class = b,  coef = bdaduring),
  # interaction
  prior(normal(0,1),          class = b,  coef = stim_typeh:bdabefore),
  prior(normal(0,1),          class = b,  coef = stim_typeh:bdaduring),
  prior(normal(0,1),          class = b,  coef = stim_typel:bdabefore),
  prior(normal(0,1),          class = b,  coef = stim_typel:bdaduring),
  # random effects / intercepts
  prior(student_t(3, 0, 0.5), class = sd, group = focal),
  prior(student_t(3, 0, 0.5), class = sd, group = pb_num),
  prior(student_t(3, 0, 0.5), class = sd, group = stim_num),
  prior(student_t(3, 0, 1),   class = Intercept))

## prior predictive check
num_chains <- 4
num_iter <- 2000
nbm_prior <- brm(
  formula = action ~ 1 + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(nbm_prior)

#### fit model                                                  ####
null_fit <- brm(
  formula = action ~ 1 + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  control = list(adapt_delta = 0.99,
                 max_treedepth = 10))

save.image('nearest_neighbour/neighbour_noprev_run_null.RData')

## check model fit
# load('nearest_neighbour/neighbour_noprev_run_null.RData')
(summary_null <- summary(null_fit))
#
par(mfrow = c(3,1))
hist(summary_null$fixed$Rhat, breaks = 50)
hist(summary_null$fixed$Bulk_ESS, breaks = 50)
hist(summary_null$fixed$Tail_ESS, breaks = 50)
par(mfrow = c(1,1))

## extract posterior distribution
draws_null <- as_draws_df(null_fit) %>%
  select(-lprior, -`lp__`)
parameters <- colnames(draws_null)[1:(ncol(draws_null)-3)]
draws_null <- draws_null  %>%
  pivot_longer(cols = all_of(parameters),
               names_to = 'parameter',
               values_to = 'draw') %>%
  rename(chain = `.chain`,
         position = `.iteration`,
         draw_id = `.draw`) %>%
  mutate(invlogit_draw = invlogit(draw))

# extract marginal effects
marg <- conditional_effects(null_fit,
                            effects = c('stim_type','bda'),
                            categorical = FALSE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
stim_effect <- marg[[1]]
bda_effect <- marg[[2]]

#### posterior predictive check                                 ####
pp_check(null_fit, ndraws = 100)

#### plot traces                                                ####
parameters_of_interest <- parameters[1:which(parameters == 'sd_stim_num__Intercept')]
draws_null %>%
  filter(parameter %in% parameters_of_interest) %>%
  ggplot(aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none')

#### plot density curves                                        ####
draws_null %>%
  filter(parameter %in% parameters_of_interest) %>%
  ggplot(aes(x = draw, colour = as.factor(chain)))+
  geom_density()+
  facet_wrap(. ~ parameter, scales = 'free')+
  theme(legend.position = 'none')

save.image('nearest_neighbour/neighbour_noprev_run_null.RData')

## reset plotting
dev.off()

#### predict from model                                         ####
# load('nearest_neighbour/neighbour_noprev_run_null.RData')
rm(list = ls()[! ls() %in% c('null_fit','nn','behav','num_iter','num_chains')])

pred_null <- posterior_epred(object = null_fit,
                        newdata = nn)
save.image('nearest_neighbour/neighbour_noprev_predictions_null.RData')

## convert to data frame
nn$data_row <- 1:nrow(nn)
predictions_null <- as.data.frame(pred_null)
colnames(predictions_null) <- 1:nrow(nn)
predictions_null <- predictions_null %>%
  pivot_longer(cols = everything(),
               names_to = 'data_row', values_to = 'epred') %>%
  mutate(data_row = as.integer(data_row)) %>%
  left_join(nn, by = 'data_row')

save.image('nearest_neighbour/neighbour_noprev_predictions_null.RData')
rm(pred_null) ; gc()

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions                                           ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelpredictions_null.pdf')
# load('nearest_neighbour/neighbour_noprev_predictions_null.RData')

predictions_null %>%
  mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
                            ifelse(stim_type == 'l', 'lion', 'human'))) %>%
  mutate(stim_type = factor(stim_type, levels = c('dove (control)', 'lion', 'human')),
         bda = factor(bda, levels = c('before','during','after'))) %>%
  ggplot()+
  geom_violin(aes(x = f_age_cat,
                  y = epred,
                  colour = bda)) +
  facet_grid(p_age_cat ~ stim_type)+
  scale_fill_viridis_d()+
  scale_colour_viridis_d()+
  labs(colour = 'time relative to stimulus:',
       fill = 'time relative to stimulus:',
       x = 'age category of focal elephant',
       y = 'predicted probability')+
  theme(legend.position = 'bottom')
ggsave(path = '../outputs/neighbour_binomial_model_bda/',
       filename = 'neighbour_noprev_modelpredictions_null.png',
       device = 'png', height = 1200, width = 1600, units = 'px')

## reset plotting
dev.off()

#### compare overlap between actual model and null model        ####
pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_nullcompare.pdf')
# load('nearest_neighbour/neighbour_noprev_predictions_null.RData')
# rm(list = ls()[! ls() %in% c('predictions_null','nn','null_fit')]) ; gc()
# nn_null <- nn # check that both models have definitely run on identical data sets
# 
# load('nearest_neighbour/neighbour_noprev_predictions.RData')
# rm(list = ls()[! ls() %in% c('predictions_null','nn_null','null_fit',
#                              'predictions','nn','nbm_fit')]) ; gc()
# 
# save.image('nearest_neighbour/neighbour_noprev_nullcompare.RData')
# 
# nrow(nn_null) == nrow(nn)
# ncol(nn_null) == ncol(nn)
# which(as.matrix(nn_null) != as.matrix(nn))
# 
# predictions_null$model_type <- 'null'
# predictions$model_type <- 'live'
# 
# pred_all <- rbind(predictions,predictions_null)
# save.image('nearest_neighbour/neighbour_noprev_nullcompare.RData')
load('nearest_neighbour/neighbour_noprev_nullcompare.RData')

ggplot(pred_all)+
  geom_density(aes(x = epred, fill = model_type, colour = model_type))+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()

ggplot(pred_all)+
  geom_density(aes(x = epred, fill = model_type, colour = model_type))+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  facet_grid(bda ~ stim_type)

dev.off()
