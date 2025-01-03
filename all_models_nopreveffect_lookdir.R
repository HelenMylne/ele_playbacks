#### information                                                     ####
# same models as before, but removing effect of previous second

#### set up                                                          ####
library(tidyverse)
library(brms)
library(LaplacesDemon)
library(patchwork)
library(ggridges)

theme_set(theme_bw())

behav <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS')
num_chains <- 4
num_iter <- 2000

######## looking direction                                           ####
set.seed(12345)
pdf('../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_modelchecks.pdf')

#### create data                                                     ####
## select specific data
look <- behav %>%
  filter(activity == 'look') %>%
  select(-activity, -stim_start, -stim_stop) %>%
  rename(look_index = action_index) %>%
  mutate(action = ifelse(action_name == 'out_of_sight', 9,
                         look_index),
         f_age_num = as.numeric(f_age_num),
         p_age_num = as.numeric(p_age_num)) %>%
  filter(!is.na(f_age_num)) %>%
  filter(!is.na(p_age_num)) %>%
  filter(look_index != 9)

#### set prior                                                       ####
get_prior(formula = look_index ~ mo(f_age_num) + age_combo + stim_type * bda +
            (1|focal) + (1|stim_num) + (1|pb_num),
          data = look, family = cumulative("logit"))
#                prior     class         coef    group resp dpar nlpar lb ub        source
#               (flat)         b                                                          default
#               (flat)         b         age_combo1_2                                (vectorized)
#               (flat)         b         age_combo1_3                                (vectorized)
#               (flat)         b         age_combo1_4                                (vectorized)
#               (flat)         b         age_combo2_1                                (vectorized)
#               (flat)         b         age_combo2_2                                (vectorized)
#               (flat)         b         age_combo2_3                                (vectorized)
#               (flat)         b         age_combo2_4                                (vectorized)
#               (flat)         b         age_combo3_1                                (vectorized)
#               (flat)         b         age_combo3_2                                (vectorized)
#               (flat)         b         age_combo3_3                                (vectorized)
#               (flat)         b         age_combo3_4                                (vectorized)
#               (flat)         b         age_combo4_1                                (vectorized)
#               (flat)         b         age_combo4_2                                (vectorized)
#               (flat)         b         age_combo4_3                                (vectorized)
#               (flat)         b         age_combo4_4                                (vectorized)
#               (flat)         b            bdabefore                                (vectorized)
#               (flat)         b            bdaduring                                (vectorized)
#               (flat)         b          mof_age_num                                (vectorized)
#               (flat)         b           stim_typeh                                (vectorized)
#               (flat)         b stim_typeh:bdabefore                                (vectorized)
#               (flat)         b stim_typeh:bdaduring                                (vectorized)
#               (flat)         b           stim_typel                                (vectorized)
#               (flat)         b stim_typel:bdabefore                                (vectorized)
#               (flat)         b stim_typel:bdaduring                                (vectorized)
# student_t(3, 0, 2.5) Intercept                                                          default
# student_t(3, 0, 2.5) Intercept                    1                                (vectorized)
# student_t(3, 0, 2.5) Intercept                    2                                (vectorized)
# student_t(3, 0, 2.5)        sd                                                0         default
# student_t(3, 0, 2.5)        sd                         focal                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept    focal                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                        pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept   pb_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd                      stim_num                  0    (vectorized)
# student_t(3, 0, 2.5)        sd            Intercept stim_num                  0    (vectorized)
#         dirichlet(1)      simo         mof_age_num1                                     default

priors <- c(
  # focal age
  prior(normal(-1,1),      class = b,    coef = mof_age_num),
  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
  # interaction
  prior(normal(-1,1),      class = b,    coef = age_combo1_2),
  prior(normal(-1,1),      class = b,    coef = age_combo1_3),
  prior(normal(-1,1),      class = b,    coef = age_combo1_4),
  prior(normal(-1,1),      class = b,    coef = age_combo2_1),
  prior(normal(-1,1),      class = b,    coef = age_combo2_2),
  prior(normal(-1,1),      class = b,    coef = age_combo2_3),
  prior(normal(-1,1),      class = b,    coef = age_combo2_4),
  prior(normal(-1,1),      class = b,    coef = age_combo3_1),
  prior(normal(-1,1),      class = b,    coef = age_combo3_2),
  prior(normal(-1,1),      class = b,    coef = age_combo3_3),
  prior(normal(-1,1),      class = b,    coef = age_combo3_4),
  prior(normal(-1,1),      class = b,    coef = age_combo4_1),
  prior(normal(-1,1),      class = b,    coef = age_combo4_2),
  prior(normal(-1,1),      class = b,    coef = age_combo4_3),
  prior(normal(-1,1),      class = b,    coef = age_combo4_4),
  # stim type
  prior(normal(-1,1),      class = b,    coef = stim_typeh),
  prior(normal(-1,1),      class = b,    coef = stim_typel),
  # before/during/after
  prior(normal(-1,1),      class = b,    coef = bdabefore),
  prior(normal(-1,1),      class = b,    coef = bdaduring),
  # interaction
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typeh:bdaduring),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdabefore),
  prior(normal(-1,1),      class = b,    coef = stim_typel:bdaduring))

## prior predictive check
lom_noprev_prior <- brm(
  formula = look_index ~ mo(f_age_num) + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = look, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(lom_noprev_prior) # huge variation in prior, but fairly on both sides so good

#### fit model                                                       ####
lom_noprev_fit <- brm(
  formula = look_index ~ mo(f_age_num) + age_combo + stim_type * bda +
    (1|focal) + (1|stim_num) + (1|pb_num),
  data = look, family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)
save.image('looking_direction/looking_noprev_2bda_run.RData')

#### extract draws                                                   ####
# load('looking_direction/looking_noprev_2bda_run.RData')
## check model diagnostics -- looks very good
(summary <- summary(lom_noprev_fit))

par(mfrow = c(3,1))
hist(summary$fixed$Rhat, breaks = 50)
hist(summary$fixed$Bulk_ESS, breaks = 50)
hist(summary$fixed$Tail_ESS, breaks = 50)
par(mfrow = c(1,1))

## extract posterior distribution
draws <- as_draws_df(lom_noprev_fit) %>%
  select(-lprior, -`lp__`)
parameters <- colnames(draws)[1:(ncol(draws)-3)]
draws <- draws  %>%
  pivot_longer(cols = all_of(parameters),
               names_to = 'parameter',
               values_to = 'draw') %>%
  rename(chain = `.chain`,
         position = `.iteration`,
         draw_id = `.draw`) %>%
  mutate(invlogit_draw = invlogit(draw))

## extract marginal effects
marg <- conditional_effects(lom_noprev_fit,
                            effects = c('f_age_num','age_combo','stim_type',
                                        'bda'),
                            categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg) # "f_age_num:cats__" "age_combo:cats__" "stim_type:cats__" "bda:cats__"
agefocal_effect <- marg[[1]]
agecombo_effect <- marg[[2]]
stim_effect <- marg[[3]]
bda_effect <- marg[[4]]

## look at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

#### plot marginal effects                                           ####
(f_age_num_plot <- ggplot(agefocal_effect)+
   geom_errorbar(aes(x = f_age_num,
                     ymax = upper__, ymin = lower__,
                     colour = cats__),
                 linewidth = 1, width = 0.2)+
   geom_point(aes(x = f_age_num,
                  y = estimate__,
                  colour = cats__),
              size = 3)+ # cex = 3?
   xlab(label = 'focal age')+
   ylab('probability of looking direction')+
   scale_colour_viridis_d(name = 'looking direction:',
                          breaks = c('1','2','3'),
                          labels = c('look towards',
                                     'side on',
                                     'look away'))+
   scale_fill_viridis_d(name = 'looking direction:',
                        breaks = c('1','2','3'),
                        labels = c('look towards',
                                   'side on',
                                   'look away'))+
   theme(legend.position = 'bottom',
         axis.title = element_text(size = 16),
         axis.text = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10)))
ggsave(plot = f_age_num_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_marginaleffects_agefocal.png', device = 'png',
       width = 8.3, height = 5.8)

f_age_num_labels <- c('focal age category 1',
                      'focal age category 2',
                      'focal age category 3',
                      'focal age category 4')
names(f_age_num_labels) <- 1:4
(agecombo_plot <- agecombo_effect %>%
    separate(col = age_combo, sep = '_', remove = F,
             into = c('f_age_num','partner_age')) %>%
    mutate(agecombo = paste0(f_age_num,'-',partner_age)) %>%
    ggplot()+
    geom_errorbar(aes(#x = agecombo,
      x = partner_age,
      colour = as.factor(cats__), # looking direction?
      ymax = upper__, ymin = lower__),
      linewidth = 1,
      width = 0.4)+
    geom_point(aes(#x = agecombo,
      x = partner_age,
      colour = as.factor(cats__),    # looking direction?
      #shape = f_age_num,
      y = estimate__),
      size = 3)+
    facet_wrap(. ~ f_age_num,
               labeller = labeller(f_age_num = f_age_num_labels))+
    ylab('probability of looking direction')+
    scale_colour_viridis_d(name = 'looking direction:',
                           breaks = c('1','2','3'),
                           labels = c('look towards',
                                      'side on',
                                      'look away'))+
    scale_x_discrete(name = 'partner age category')+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = agecombo_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_marginaleffects_agepartner.png', device = 'png',
       width = 8.3, height = 5.8)

(stim_plot <- ggplot(stim_effect)+
    geom_errorbar(aes(x = stim_type,
                      ymin = lower__, ymax = upper__,
                      colour = cats__),
                  linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type,
                   y = estimate__,
                   colour = cats__),
               cex = 3)+ # size = 3?
    xlab(label = 'stimulus type') + ylab('probability of looking direction')+
    scale_colour_viridis_d(name = 'looking direction:',
                           breaks = c('1','2','3'),
                           labels = c('look towards', 'side on', 'look away'))+
    scale_x_discrete(breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h'))+
    theme(legend.position = 'bottom',
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) )
ggsave(plot = stim_plot, filename = '../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_marginaleffects_stimtype.png',
       device = 'png', width = 8.3, height = 5.8)
print(paste0('marginal effects plotted at ',Sys.time()))

#### posterior predictive check                                      ####
pp_check(lom_noprev_fit, ndraws = 100) # really good fit

#### plot traces and density curves                                  ####
draws_cut <- draws %>%
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "b_bdabefore","b_bdaduring",
                          "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
                          "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
                          "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
                          "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]"))
ggplot(data = draws_cut,
       aes(x = position, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mixing generally looks very good, though a couple of age combo ones are a touch wandery

## move at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,1))
hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')

## stim type
lion <- draws_cut %>% filter(parameter == 'b_stim_typel')
human <- draws_cut %>% filter(parameter == 'b_stim_typeh')
plot(density(lion$draw), main = 'lion vs dove') ; abline(v = 0, lty = 2)
plot(density(human$draw), main = 'human vs dove') ; abline(v = 0, lty = 2)

## focal age
age1 <- draws_cut %>% filter(parameter == 'bsp_mof_age_num')
age2 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[1]')
age3 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[2]')
age4 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[3]')
par(mfrow = c(2,2))
plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)

print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))

#### plot raw                                                        ####
## define labels for plotting
age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
stim_labels <- c('dove (control)','human','lion')
names(stim_labels) <- c('ctd','h','l')

## plot control data
look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = bda, y = look_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  scale_x_discrete(name = 'time relative to stimulus')

look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'ctd') %>%
  ggplot(aes(x = look_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## plot lion data
look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = bda, y = look_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  scale_x_discrete(name = 'time relative to stimulus')

look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'l') %>%
  ggplot(aes(x = look_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## plot human data
look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = bda, y = look_index,
             group = focal))+
  geom_jitter(colour = rgb(0,0,1,0.01))+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels))+
  scale_y_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  scale_x_discrete(name = 'time relative to stimulus')

look %>%
  mutate(bda = factor(bda, levels = c('before','during','after'))) %>%
  filter(stim_type == 'h') %>%
  ggplot(aes(x = look_index, fill = bda))+
  geom_bar(colour = rgb(0,0,1,0.01), position = 'dodge')+
  facet_grid(f_age_num ~ p_age_num,
             labeller = labeller(f_age_num = age_labels,
                                 p_age_num = age_labels))+
  scale_x_continuous(name = 'focal looking direction relative to target',
                     breaks = c(1,2,3),
                     labels = c('look directly at','side-on','look directly away'))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

print(paste0('raw data plotted at ', Sys.time()))

## reset plotting
save.image('looking_direction/looking_noprev_2bda_run.RData')
dev.off()
pdf('../outputs/looking_ordinal_model_2bda/looking_noprev_2bda_modelpredictions.pdf')

#### predict from model                                              ####
# load('looking_direction/looking_noprev_2bda_run.RData')
rm(list = ls()[! ls() %in% c('lom_noprev_fit','look','behav','num_iter','num_chains')]) ; gc()

pred <- posterior_epred(object = lom_noprev_fit,
                        newdata = look)
save.image('looking_direction/looking_noprev_model2bda_predictions.RData')
print(paste0('predictions calculated at ',Sys.time()))

## convert to data frame
# load('looking_direction/looking_noprev_model2bda_predictions.RData')
look$data_row <- 1:nrow(look)
extract_predictions <- function(prediction_array, layer, df){
  predictions <- as.data.frame(prediction_array[,,layer])
  colnames(predictions) <- 1:nrow(df)
  predictions <- predictions %>%
    pivot_longer(cols = everything(),
                 names_to = 'data_row', values_to = 'epred') %>%
    mutate(data_row = as.integer(data_row)) %>%
    left_join(df, by = 'data_row') %>%
    mutate(pred_type = ifelse(layer == 1, 'look directly away',
                              ifelse(layer == 2, 'side-on',
                                     ifelse(layer == 3, 'look at directly',
                                            'CHECK -- PROBLEM IN DATA'))),
           pred_type_num = layer)
  return(predictions)
}
pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = look)
pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = look)
pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = look)

## combine data frames
pred <- rbind(pred1, pred2, pred3)
save.image('looking_direction/looking_noprev_model2bda_predictions.RData')
rm(pred1, pred2, pred3) ; gc()

print(paste0('predictions extracted at ',Sys.time()))

# #### plot predictions -- DOESN'T WORK ON VIKING AT THE MOMENT DUE TO ERROR IN VCTRS PACKAGE. COME BACK TO THIS ONLY IF YOU ABSOLUTELY NEED IT ####
# load('looking_direction/looking_noprev_model2bda_predictions.RData'); rm(pred1, pred2, pred3) ; gc()
# pred %>%
#   mutate(stim_type = ifelse(stim_type == 'ctd', 'dove (control)',
#                             ifelse(stim_type == 'l', 'lion','human'))) %>%
#   mutate(stim_type = factor(stim_type, levels = c('dove (control)','lion','human'))) %>%
#   ggplot()+
#   geom_violin(aes(x = as.factor(f_age_num), y = epred,
#                   fill = factor(pred_type, levels = c('look directly away',
#                                                       'side-on',
#                                                       'look at directly')),
#                   colour = factor(pred_type, levels = c('look directly away',
#                                                         'side-on',
#                                                         'look at directly'))
#   )) +
#   facet_grid(stim_type ~ bda)+
#   scale_fill_viridis_d()+
#   scale_colour_viridis_d()+
#   labs(colour = 'predicted looking direction relative to focal:',
#        fill = 'predicted looking direction relative to focal:',
#        x = 'age category of focal elephant',
#        y = 'proportion of predictions',
#        title = 'cape turtle dove (control)')+
#   theme(legend.position = 'bottom')
# 
# ## reset plotting
# dev.off()
# 
# print('looking direction predictions completed')
# 
#### calculate posterior contrasts from predictions                  ####
load('looking_direction/looking_noprev_model2bda_predictions.RData')
pdf('../outputs/looking_ordinal_model_2bda/looking_noprev_modelcontrasts.pdf')

## stim type                                                         ####
stim_new <- look %>%
  dplyr::select(f_age_num, age_combo, stim_type, bda,
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, bda,
                                                         focal, stim_num, pb_num))))

## redo predictions with different stimulus types: all doves
ctd_b <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'before')
ctdb_mtx <- posterior_epred(object = lom_noprev_fit, newdata = ctd_b)
colnames(ctdb_mtx) <- ctd_b$unique_data_combo
ctdb_mtx <- ctdb_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

ctd_d <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'during')
ctdd_mtx <- posterior_epred(object = lom_noprev_fit, newdata = ctd_d)
colnames(ctdd_mtx) <- ctd_d$unique_data_combo
ctdd_mtx <- ctdd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

ctd_a <- stim_new %>%
  mutate(stim_type = 'ctd',
         bda = 'after')
ctda_mtx <- posterior_epred(object = lom_noprev_fit, newdata = ctd_a)
colnames(ctda_mtx) <- ctd_a$unique_data_combo
ctda_mtx <- ctda_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all lions
lion_b <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'before')
lionb_mtx <- posterior_epred(object = lom_noprev_fit, newdata = lion_b)
colnames(lionb_mtx) <- lion_b$unique_data_combo
lionb_mtx <- lionb_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

lion_d <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'during')
liond_mtx <- posterior_epred(object = lom_noprev_fit, newdata = lion_d)
colnames(liond_mtx) <- lion_d$unique_data_combo
liond_mtx <- liond_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

lion_a <- stim_new %>%
  mutate(stim_type = 'l',
         bda = 'after')
liona_mtx <- posterior_epred(object = lom_noprev_fit, newdata = lion_a)
colnames(liona_mtx) <- lion_a$unique_data_combo
liona_mtx <- liona_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with different stimulus types: all humans
human_b <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'before')
humanb_mtx <- posterior_epred(object = lom_noprev_fit, newdata = human_b)
colnames(humanb_mtx) <- human_b$unique_data_combo
humanb_mtx <- humanb_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

human_d <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'during')
humand_mtx <- posterior_epred(object = lom_noprev_fit, newdata = human_d)
colnames(humand_mtx) <- human_d$unique_data_combo
humand_mtx <- humand_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

human_a <- stim_new %>%
  mutate(stim_type = 'h',
         bda = 'after')
humana_mtx <- posterior_epred(object = lom_noprev_fit, newdata = human_a)
colnames(humana_mtx) <- human_a$unique_data_combo
humana_mtx <- humana_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('looking_direction/looking_noprev_stimuluscontrasts.RData')

ctd_bvd <- ctdd_mtx - ctdb_mtx
ctd_bva <- ctda_mtx - ctdb_mtx
ctd_dva <- ctda_mtx - ctdd_mtx
mean(ctd_bvd) ; sd(ctd_bvd)
quantile(ctd_bvd, prob = c(0.025, 0.5, 0.975))
length(which(ctd_bvd < 0)) / length(ctd_bvd)
# -6.466706e-17
# 0.04471176
#        2.5%         50%       97.5%
# -0.06605578 -0.01051199  0.07449033
# 0.5866834
mean(ctd_bva) ; sd(ctd_bva)
quantile(ctd_bva, prob = c(0.025, 0.5, 0.975))
length(which(ctd_bva < 0)) / length(ctd_bva)
# -1.537299e-17
# 0.0186429
#        2.5%          50%        97.5%
# -0.028018171 -0.004198769  0.031878060
# 0.5806518
mean(ctd_dva) ; sd(ctd_dva)
quantile(ctd_dva, prob = c(0.025, 0.5, 0.975))
length(which(ctd_dva < 0)) / length(ctd_dva)
# -1.685351e-17
# 0.02643229
#        2.5%          50%        97.5%
# -0.046464951  0.006099425  0.039667252
# 0.4091312

lion_bvd <- liond_mtx - lionb_mtx
lion_bva <- liona_mtx - lionb_mtx
lion_dva <- liona_mtx - liond_mtx
mean(lion_bvd) ; sd(lion_bvd)
quantile(lion_bvd, prob = c(0.025, 0.5, 0.975))
length(which(lion_bvd < 0)) / length(lion_bvd)
# 8.904389e-18
# 0.01723592
#        2.5%         50%       97.5%
# -0.03630227  0.00215334  0.03113457
# 0.4229632
mean(lion_bva) ; sd(lion_bva)
quantile(lion_bva, prob = c(0.025, 0.5, 0.975))
length(which(lion_bva < 0)) / length(lion_bva)
# -3.771875e-17
# 0.02738023
#        2.5%          50%        97.5%
# -0.047488871  0.006066757  0.041433772
# 0.4189582
mean(lion_dva) ; sd(lion_dva)
quantile(lion_dva, prob = c(0.025, 0.5, 0.975))
length(which(lion_dva < 0)) / length(lion_dva)
# -9.461698e-18
# 0.01441221
#        2.5%          50%        97.5%
# -0.031418365  0.001282196  0.026847935
# 0.4340467

human_bvd <- humand_mtx - humanb_mtx
human_bva <- humana_mtx - humanb_mtx
human_dva <- humana_mtx - humand_mtx
mean(human_bvd) ; sd(human_bvd)
quantile(human_bvd, prob = c(0.025, 0.5, 0.975))
length(which(human_bvd < 0)) / length(human_bvd)
# 1.41173e-17
# 0.0176661
#        2.5%          50%        97.5%
# -0.034284531  0.003469809  0.029068192
# 0.4130827
mean(human_bva) ; sd(human_bva)
quantile(human_bva, prob = c(0.025, 0.5, 0.975))
length(which(human_bva < 0)) / length(human_bva)
# -3.136653e-17
# 0.01277953
#        2.5%          50%        97.5%
# -0.023367185  0.002738465  0.020108285
# 0.4118782
mean(human_dva) ; sd(human_dva)
quantile(human_dva, prob = c(0.025, 0.5, 0.975))
length(which(human_dva < 0)) / length(human_dva)
# -2.077351e-17
# 0.007222595
#        2.5%           50%         97.5%
# -0.0144973317 -0.0003169341  0.0169630705
# 0.5428956

bef_dvl <- lionb_mtx - ctdb_mtx
bef_dvh <- humanb_mtx - ctdb_mtx
bef_lvh <- humanb_mtx - lionb_mtx
mean(bef_dvl) ; sd(bef_dvl)
quantile(bef_dvl, prob = c(0.025, 0.5, 0.975))
length(which(bef_dvl < 0)) / length(bef_dvl)
# -1.959014e-17
# 0.06216195
#        2.5%          50%        97.5%
# -0.125373024 -0.001850436  0.143414045
# 0.5365104
mean(bef_dvh) ; sd(bef_dvh)
quantile(bef_dvh, prob = c(0.025, 0.5, 0.975))
length(which(bef_dvh < 0)) / length(bef_dvh)
# -7.247863e-17
# 0.05980087
#        2.5%          50%        97.5%
# -0.115882319 -0.002324294  0.136147353
# 0.5417823
mean(bef_lvh) ; sd(bef_lvh)
quantile(bef_lvh, prob = c(0.025, 0.5, 0.975))
length(which(bef_lvh < 0)) / length(bef_lvh)
# -2.376377e-17
# 0.05943699
#        2.5%           50%         97.5%
# -1.243163e-01 -6.402141e-05  1.273589e-01
# 0.5022122

dur_dvl <- liond_mtx - ctdd_mtx
dur_dvh <- humand_mtx - ctdd_mtx
dur_lvh <- humand_mtx - liond_mtx
mean(dur_dvl) ; sd(dur_dvl)
quantile(dur_dvl, prob = c(0.025, 0.5, 0.975))
length(which(dur_dvl < 0)) / length(dur_dvl)
# 8.1978e-17
# 0.06448215
#        2.5%          50%        97.5%
# -0.147433017  0.002539014  0.129939091
# 0.4648732
mean(dur_dvh) ; sd(dur_dvh)
quantile(dur_dvh, prob = c(0.025, 0.5, 0.975))
length(which(dur_dvh < 0)) / length(dur_dvh)
# -6.142849e-18
# 0.0568681
#        2.5%          50%        97.5%
# -0.129274055  0.002097133  0.115410532
# 0.4636168
mean(dur_lvh) ; sd(dur_lvh)
quantile(dur_lvh, prob = c(0.025, 0.5, 0.975))
length(which(dur_lvh < 0)) / length(dur_lvh)
# -4.200171e-17
# 0.06092359
#        2.5%           50%         97.5%
# -1.265843e-01  5.958758e-05  1.278865e-01
# 0.4986143

aft_dvl <- liona_mtx - ctda_mtx
aft_dvh <- humana_mtx - ctda_mtx
aft_lvh <- humana_mtx - liona_mtx
mean(aft_dvl) ; sd(aft_dvl)
quantile(aft_dvl, prob = c(0.025, 0.5, 0.975))
length(which(aft_dvl < 0)) / length(aft_dvl)
# -2.508053e-17
# 0.0586229
#        2.5%           50%         97.5%
# -0.1298320365  0.0009773756  0.1229180845
# 0.481535
mean(aft_dvh) ; sd(aft_dvh)
quantile(aft_dvh, prob = c(0.025, 0.5, 0.975))
length(which(aft_dvh < 0)) / length(aft_dvh)
# 4.183423e-17
# 0.04972901
#        2.5%           50%         97.5%
# -0.1063776209 -0.0005296223  0.1083461819
# 0.5070968
mean(aft_lvh) ; sd(aft_lvh)
quantile(aft_lvh, prob = c(0.025, 0.5, 0.975))
length(which(aft_lvh < 0)) / length(aft_lvh)
# -4.048612e-17
# 0.06287543
#        2.5%          50%        97.5%
# -0.126742476 -0.001196513  0.139544852
# 0.5165277

## focal age                                                         ####
load('looking_direction/looking_noprev_stimuluscontrasts.RData')
rm(list = ls()[!ls() %in% c('lom_noprev_fit','look','behav','num_iter','num_chains')]) ; gc()

## create new dataframe to predict from
age_new <- look %>%
  dplyr::select(f_age_num, age_combo, stim_type, bda,
                focal, stim_num, pb_num) %>%
  mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, age_combo, bda,
                                                         focal, stim_num, pb_num))))

## predict with original ages
age_look_org <- age_new
age_mtx_org <- posterior_epred(object = lom_noprev_fit, newdata = age_look_org)
colnames(age_mtx_org) <- age_look_org$unique_data_combo
age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]

## redo predictions with altered ages
age_look_alt <- age_new %>%
  mutate(f_age_num_original = f_age_num,
         age_combo_original = age_combo) %>%
  mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1)) %>%
  separate(age_combo, into = c('f_age_old','p_age'), sep = '_') %>%
  mutate(age_combo = paste0(f_age_num, '_', p_age)) %>%
  dplyr::select(f_age_num_original, f_age_num,
                age_combo_original, age_combo,
                stim_type, bda,
                focal, stim_num, pb_num,
                unique_data_combo)
age_mtx_alt <- posterior_epred(object = lom_noprev_fit, newdata = age_look_alt)
colnames(age_mtx_alt) <- age_look_alt$unique_data_combo
age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]

save.image('looking_direction/looking_noprev_agecontrasts.RData')

## summarise and convert to long format
# rm(list = ls()) ; gc()
# load('looking_direction/looking_noprev_agecontrasts.RData')
age_pred <- age_look_org %>%
  #dplyr::select(-f_age_num) %>%
  mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
         age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
         age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
         age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
         age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
         age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
         age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
         age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
         age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
         age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
         age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
         age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd)) %>%
  pivot_longer(cols = c(age_org_prop1_mu, age_org_prop2_mu, age_org_prop3_mu,
                        age_alt_prop1_mu, age_alt_prop2_mu, age_alt_prop3_mu),
               names_to = 'focal_agelook_mu', values_to = 'mean_propn') %>%
  pivot_longer(cols = c(age_org_prop1_sd, age_org_prop2_sd, age_org_prop3_sd,
                        age_alt_prop1_sd, age_alt_prop2_sd, age_alt_prop3_sd),
               names_to = 'focal_agelook_sd', values_to = 'stdv_propn') %>%
  separate(col = focal_agelook_mu, into = c('focal_agelook_mu','mu'),
           sep = '_m', remove = T) %>%
  separate(col = focal_agelook_sd, into = c('focal_agelook_sd','sd'),
           sep = '_s', remove = T) %>%
  select(-mu, -sd) %>%
  filter(focal_agelook_mu == focal_agelook_sd) %>%
  separate(col = focal_agelook_mu, into = c('original_altered', 'look_pred'),
           sep = '_prop', remove = T) %>%
  select(-focal_agelook_sd) %>%
  mutate(look_pred = as.numeric(look_pred),
         f_age_num = ifelse(original_altered == 'age_org',
                            f_age_num,
                            ifelse(original_altered == 'age_alt' & f_age_num == 4,
                                   1, f_age_num + 1))) %>%
  mutate(pred_type = ifelse(look_pred == 1, 'look away directly',
                            ifelse(look_pred == 2, 'side on',
                                   'look at directly')))

## calculate contrasts
alt_vs_org_away <- age_mtx_alt[,,1] - age_mtx_org[,,1]
alt_vs_org_side <- age_mtx_alt[,,2] - age_mtx_org[,,2]
alt_vs_org_twds <- age_mtx_alt[,,3] - age_mtx_org[,,3]

# ## calculate contrast values -- for all, standard deviation > median or mean, so difference is centered on zero
# mean(alt_vs_org_away) ; sd(alt_vs_org_away)
# mean(alt_vs_org_side) ; sd(alt_vs_org_side)
# mean(alt_vs_org_twds) ; sd(alt_vs_org_twds)

## repeat excluding age category 4 because different contrast
mean(alt_vs_org_away[,which(age_look_org$f_age_num != 4)])
sd(alt_vs_org_away[,which(age_look_org$f_age_num != 4)])
quantile(alt_vs_org_away[,which(age_look_org$f_age_num != 4)], prob = c(0.025, 0.5, 0.975))
length(which(alt_vs_org_away[,which(age_look_org$f_age_num != 4)] < 0)) / length(alt_vs_org_away[,which(age_look_org$f_age_num != 4)])
# 0.002366395
# 0.1025436
#         2.5%          50%        97.5%
# -0.182332412 -0.008654393  0.225287838
# 0.5382315
mean(alt_vs_org_side[,which(age_look_org$f_age_num != 4)])
sd(alt_vs_org_side[,which(age_look_org$f_age_num != 4)])
quantile(alt_vs_org_side[,which(age_look_org$f_age_num != 4)], prob = c(0.025, 0.5, 0.975))
length(which(alt_vs_org_side[,which(age_look_org$f_age_num != 4)] < 0)) / length(alt_vs_org_side[,which(age_look_org$f_age_num != 4)])
# -0.007962889
# 0.07055196
#         2.5%          50%        97.5%
# -0.159636004 -0.004962222  0.128889531
# 0.5417501
mean(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)])
sd(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)])
quantile(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)], prob = c(0.025, 0.5, 0.975))
length(which(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)] < 0)) / length(alt_vs_org_twds[,which(age_look_org$f_age_num != 4)])
# 0.005596494
# 0.06941912
#         2.5%          50%        97.5%
# -0.156328464  0.003694649  0.148290367
# 0.4617685

## split contrasts by original age category
age1v2_away <- alt_vs_org_away[,which(age_look_org$f_age_num == 1)]
age2v3_away <- alt_vs_org_away[,which(age_look_org$f_age_num == 2)]
age3v4_away <- alt_vs_org_away[,which(age_look_org$f_age_num == 3)]
age1v4_away <- alt_vs_org_away[,which(age_look_org$f_age_num == 4)] * (-1)
age1v2_side <- alt_vs_org_side[,which(age_look_org$f_age_num == 1)]
age2v3_side <- alt_vs_org_side[,which(age_look_org$f_age_num == 2)]
age3v4_side <- alt_vs_org_side[,which(age_look_org$f_age_num == 3)]
age1v4_side <- alt_vs_org_side[,which(age_look_org$f_age_num == 4)] * (-1)
age1v2_twds <- alt_vs_org_twds[,which(age_look_org$f_age_num == 1)]
age2v3_twds <- alt_vs_org_twds[,which(age_look_org$f_age_num == 2)]
age3v4_twds <- alt_vs_org_twds[,which(age_look_org$f_age_num == 3)]
age1v4_twds <- alt_vs_org_twds[,which(age_look_org$f_age_num == 4)] * (-1)

## calculate contrast values
mean(age1v2_away) ; sd(age1v2_away)
quantile(age1v2_away, prob = c(0.025, 0.5, 0.975))
length(which(age1v2_away < 0)) / length(age1v2_away)
# 0.1044774
# 0.1046509
#       2.5%        50%      97.5%
# -0.1021470  0.1048245  0.3060205
# 0.1659405

mean(age2v3_away) ; sd(age2v3_away)
quantile(age2v3_away, prob = c(0.025, 0.5, 0.975))
length(which(age2v3_away < 0)) / length(age2v3_away)
# -0.02859947
# 0.09134484
#        2.5%         50%       97.5%
# -0.17018621 -0.04296685  0.22946956
# 0.7424159

mean(age3v4_away) ; sd(age3v4_away)
quantile(age3v4_away, prob = c(0.025, 0.5, 0.975))
length(which(age3v4_away < 0)) / length(age3v4_away)
# 0.0143516
# 0.09831669
#        2.5%         50%       97.5%
# -0.21743991  0.01879031  0.19122349
# 0.4076998

mean(age1v4_away) ; sd(age1v4_away)
quantile(age1v4_away, prob = c(0.025, 0.5, 0.975))
length(which(age1v4_away < 0)) / length(age1v4_away)
# 0.09940839
# 0.1366719
#        2.5%         50%       97.5%
# -0.16320410  0.09012748  0.38227375
# 0.2345102

mean(age1v2_side) ; sd(age1v2_side)
quantile(age1v2_side, prob = c(0.025, 0.5, 0.975))
length(which(age1v2_side < 0)) / length(age1v2_side)
# -0.06211487
# 0.08212917
#       2.5%        50%      97.5%
# -0.2213375 -0.0611910  0.1143200
# 0.7829061

mean(age2v3_side) ; sd(age2v3_side)
quantile(age2v3_side, prob = c(0.025, 0.5, 0.975))
length(which(age2v3_side < 0)) / length(age2v3_side)
# 0.007347159
# 0.06590958
#         2.5%          50%        97.5%
# -0.155110955  0.007911878  0.127745469
# 0.4296889

mean(age3v4_side) ; sd(age3v4_side)
quantile(age3v4_side, prob = c(0.025, 0.5, 0.975))
length(which(age3v4_side < 0)) / length(age3v4_side)
# -0.01326124
# 0.06726804
#        2.5%         50%       97.5%
# -0.14333346 -0.01339541  0.13211005
# 0.6070868

mean(age1v4_side) ; sd(age1v4_side)
quantile(age1v4_side, prob = c(0.025, 0.5, 0.975))
length(which(age1v4_side < 0)) / length(age1v4_side)
# -0.006907532
# 0.1250554
#         2.5%          50%        97.5%
# -0.264461461 -0.001316608  0.239726070
# 0.5080516

mean(age1v2_twds) ; sd(age1v2_twds)
quantile(age1v2_twds, prob = c(0.025, 0.5, 0.975))
length(which(age1v2_twds < 0)) / length(age1v2_twds)
# -0.04236251
# 0.05829196
#        2.5%         50%       97.5%
# -0.18318002 -0.03522679  0.06081386
# 0.8340595

mean(age2v3_twds) ; sd(age2v3_twds)
quantile(age2v3_twds, prob = c(0.025, 0.5, 0.975))
length(which(age2v3_twds < 0)) / length(age2v3_twds)
# 0.02125231
# 0.07112862
#        2.5%         50%       97.5%
# -0.19206992  0.02073787  0.15106392
# 0.2575841

mean(age3v4_twds) ; sd(age3v4_twds)
quantile(age3v4_twds, prob = c(0.025, 0.5, 0.975))
length(which(age3v4_twds < 0)) / length(age3v4_twds)
# -0.001090365
# 0.0645151
#         2.5%          50%        97.5%
# -0.124452857 -0.006201423  0.151697310
# 0.5923002

mean(age1v4_twds) ; sd(age1v4_twds)
quantile(age1v4_twds, prob = c(0.025, 0.5, 0.975))
length(which(age1v4_twds < 0)) / length(age1v4_twds)
# -0.09250086
# 0.1316505
#        2.5%         50%       97.5%
# -0.39850323 -0.04696287  0.10052422
# 0.7654898

## summarise contrasts
contrasts <- look %>%
  mutate(alt_vs_org_away_mu = apply(alt_vs_org_away, 2, mean),
         alt_vs_org_away_sd = apply(alt_vs_org_away, 2, sd),
         alt_vs_org_side_mu = apply(alt_vs_org_side, 2, mean),
         alt_vs_org_side_sd = apply(alt_vs_org_side, 2, sd),
         alt_vs_org_twds_mu = apply(alt_vs_org_twds, 2, mean),
         alt_vs_org_twds_sd = apply(alt_vs_org_twds, 2, sd)
  ) %>%
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
  select(-alt_vs_org_away_sd,-alt_vs_org_side_sd,-alt_vs_org_twds_sd,
         -alt, -vs, -org, -mu)

## plot contrasts
age_pred %>%
  mutate(pred_type = factor(pred_type,
                            levels = c('look away directly',
                                       'side on',
                                       'look at directly'))) %>%
  ggplot()+
  geom_density(aes(x = mean_propn, colour = pred_type, fill = pred_type),
               alpha = 0.5)+
  facet_grid(pred_type ~ stim_type)
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

  ## clean up a bit
rm(list = ls()[! ls() %in% c('alt_vs_org_away','alt_vs_org_side',
                             'alt_vs_org_twds','look','lom_noprev_fit',
                             'behav','num_iter','num_chains')]) ; gc()

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

ggplot(plot_contrasts)+
  geom_density(aes(x = contrast,
                   fill = diff_cats, # fill = f_age_cat,
                   colour = diff_cats # colour = f_age_cat
  ),
  #fill = '#21918c', colour = '#21918c',
  alpha = 0.4)+
  scale_colour_viridis_d(begin = 0, end = 0.5)+
  scale_fill_viridis_d(begin = 0, end = 0.5)+
  geom_vline(xintercept = 0, linetype = 2)+
  facet_grid(prediction_type ~ categories, scales = 'free_y')+#, nrow = 3, ncol = 4)+
  labs(x = 'contrast between age categories',
       fill  =  'change in age\ncategory', #  fill  = 'original\nage category',
       colour = 'change in age\ncategory'  # colour = 'original\nage category'
  )+
  theme(legend.position = 'none')+ #c(0.8, 0.9))+
  theme_bw()
ggsave(plot = last_plot(), device = 'png',
       filename = 'looking_noprev_ordinal2bda_agecontrasts.png',
       path = '../outputs/looking_ordinal_model_2bda/',
       width = 2400, height = 3200, unit = 'px')
ggsave(plot = last_plot(), device = 'svg',
       filename = 'looking_noprev_ordinal2bda_agecontrasts.svg',
       path = '../outputs/looking_ordinal_model_2bda/',
       width = 2400, height = 3200, unit = 'px')

dev.off()
