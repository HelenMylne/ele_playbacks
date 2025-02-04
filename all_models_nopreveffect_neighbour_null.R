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

# ######## import data and random effects                         ####
# load('nearest_neighbour/neighbour_noprev_run.RData')
#
# #### extract random effect posterior                            ####
# random <- as_draws_df(nbm_fit) %>%
#   dplyr::select(-b_Intercept, -b_age_relOY, -b_age_relYO, -b_age_relYY,
#                 -b_stim_typeh, -b_stim_typel, -b_bdabefore, -b_bdaduring,
#                 -`b_stim_typeh:bdabefore`, -`b_stim_typel:bdabefore`,
#                 -`b_stim_typeh:bdaduring`, -`b_stim_typel:bdaduring`,
#                 -lprior, -`lp__`)
# colnames(random)
# stim_num <- random %>%
#   dplyr::select(grep(pattern = 'stim_num', x = colnames(random)),
#                 `.chain`, `.iteration`, `.draw`) %>%
#   pivot_longer(cols = 1:(length(unique(nn$stim_num)) + 1),
#                names_to = 'parameter',
#                values_to = 'mcmc')
# rand_stim <- stim_num %>%
#   group_by(parameter) %>%
#   summarise(mean_stim = mean(mcmc),
#             sd_stim = sd(mcmc)) %>%
#   separate(parameter, into = c('r_stim','stim_intercept'),
#            remove = F, sep = 11) %>%
#   separate(stim_intercept, into = c('stim_num','intercept'),
#            remove = T, sep = ',') %>%
#   dplyr::select(-intercept)
#
# pb_num <- random %>%
#   dplyr::select(grep(pattern = 'pb_num', x = colnames(random)),
#                 `.chain`, `.iteration`, `.draw`) %>%
#   pivot_longer(cols = 1:(length(unique(nn$pb_num)) + 1),
#                names_to = 'parameter',
#                values_to = 'mcmc')
# rand_pb <- pb_num %>%
#   group_by(parameter) %>%
#   summarise(mean_pb = mean(mcmc),
#             sd_pb = sd(mcmc)) %>%
#   separate(parameter, into = c('r_pb','pb_intercept'),
#            remove = F, sep = 9) %>%
#   separate(pb_intercept, into = c('pb_num','intercept'),
#            remove = T, sep = ',') %>%
#   dplyr::select(-intercept)
#
# focal <- random %>%
#   dplyr::select(grep(pattern = 'focal', x = colnames(random)),
#                 `.chain`, `.iteration`, `.draw`) %>%
#   pivot_longer(cols = 1:(length(unique(nn$focal)) + 1),
#                names_to = 'parameter',
#                values_to = 'mcmc')
# rand_focal <- focal %>%
#   group_by(parameter) %>%
#   summarise(mean_focal = mean(mcmc),
#             sd_focal = sd(mcmc)) %>%
#   separate(parameter, into = c('r_focal','focal_intercept'),
#            remove = F, sep = 8) %>%
#   separate(focal_intercept, into = c('focal','intercept'),
#            remove = T, sep = ',') %>%
#   dplyr::select(-intercept)
#
# nn <- nn %>%
#   left_join(rand_stim[,c('stim_num','mean_stim','sd_stim')],
#             by = 'stim_num') %>%
#   mutate(pb_num = as.character(pb_num)) %>%
#   left_join(rand_pb[,c('pb_num','mean_pb','sd_pb')],
#             by = 'pb_num') %>%
#   mutate(pb_num = as.numeric(pb_num)) %>%
#   left_join(rand_focal[,c('focal','mean_focal','sd_focal')],
#             by = 'focal')
#
# #### set prior                                                  ####
# pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_modelchecks_null.pdf')
#
# get_prior(formula = action ~ 1 + stim_type * bda +
#             offset(mean_focal) + offset(mean_stim) + offset(mean_pb),
#           data = nn, family = bernoulli("logit"))
# #                prior     class                 coef    group resp dpar nlpar lb ub       source
# #               (flat)         b                                                          default
# #               (flat)         b            bdabefore                                (vectorized)
# #               (flat)         b            bdaduring                                (vectorized)
# #               (flat)         b           stim_typeh                                (vectorized)
# #               (flat)         b stim_typeh:bdabefore                                (vectorized)
# #               (flat)         b stim_typeh:bdaduring                                (vectorized)
# #               (flat)         b           stim_typel                                (vectorized)
# #               (flat)         b stim_typel:bdabefore                                (vectorized)
# #               (flat)         b stim_typel:bdaduring                                (vectorized)
# # student_t(3, 0, 2.5) Intercept                                                          default
# # student_t(3, 0, 2.5)        sd                                                0         default
# # student_t(3, 0, 2.5)        sd                         focal                  0    (vectorized)
# # student_t(3, 0, 2.5)        sd            Intercept    focal                  0    (vectorized)
# # student_t(3, 0, 2.5)        sd                        pb_num                  0    (vectorized)
# # student_t(3, 0, 2.5)        sd            Intercept   pb_num                  0    (vectorized)
# # student_t(3, 0, 2.5)        sd                      stim_num                  0    (vectorized)
# # student_t(3, 0, 2.5)        sd            Intercept stim_num                  0    (vectorized)
#
# priors <- c(
#   # # random effects / intercepts
#   # prior(student_t(3, 0, 1),   class = Intercept),
#   # prior(student_t(3, 0, 0.5), class = sd, group = focal),
#   # prior(student_t(3, 0, 0.5), class = sd, group = pb_num),
#   # prior(student_t(3, 0, 0.5), class = sd, group = stim_num),
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
#   prior(normal(0,1),          class = b,  coef = stim_typel:bdaduring))
#
# ## prior predictive check
# num_chains <- 4
# num_iter <- 2000
# nbm_prior <- brm(
#   formula = action ~ 1 + stim_type * bda +
#     offset(mean_focal) + offset(mean_stim) + offset(mean_pb),
#   data = nn, family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(nbm_prior)
#
# #### fit model                                                  ####
# null_fit <- brm(
#   formula = action ~ 1 + stim_type * bda +
#     offset(mean_focal) + offset(mean_stim) + offset(mean_pb),
#   data = nn, family = bernoulli("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   control = list(adapt_delta = 0.99,
#                  max_treedepth = 10))
#
# save.image('nearest_neighbour/neighbour_noprev_run_null.RData')

## check model fit
load('nearest_neighbour/neighbour_noprev_run_null.RData')
(summary_null <- summary(null_fit))
#
par(mfrow = c(3,1))
hist(summary_null$fixed$Rhat, breaks = 50)
hist(summary_null$fixed$Bulk_ESS, breaks = 50)
hist(summary_null$fixed$Tail_ESS, breaks = 50)
par(mfrow = c(1,1))

#### extract posterior distribution                             ####
draws_null <- as_draws_df(null_fit) %>%
  select(-lprior, -`lp__`)
colnames(draws_null)
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
# parameters
# parameters_of_interest <- parameters[1:which(parameters == 'sd_stim_num__Intercept')]
draws_null %>%
  # filter(parameter %in% parameters_of_interest) %>%
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

# #### compare overlap between actual model and null model        ####
# pdf('../outputs/neighbour_binomial_model_bda/neighbour_noprev_nullcompare.pdf')
# # load('nearest_neighbour/neighbour_noprev_predictions_null.RData')
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
# # load('nearest_neighbour/neighbour_noprev_nullcompare.RData')
# 
# ggplot(pred_all)+
#   geom_density(aes(x = epred, fill = model_type, colour = model_type))+
#   scale_colour_viridis_d()+
#   scale_fill_viridis_d()
# 
# ggplot(pred_all)+
#   geom_density(aes(x = epred, fill = model_type, colour = model_type))+
#   scale_colour_viridis_d()+
#   scale_fill_viridis_d()+
#   facet_grid(bda ~ stim_type)
# 
# dev.off()








