#### information ####
# simple models to compare before/during/after for each measure of social behaviour

#### set up ####
#library(tidyverse); library(brms) ; library(LaplacesDemon) ; library(ggpubr)
library(tidyverse, lib.loc = '../packages/')
library(brms, lib.loc = '../packages/')
library(LaplacesDemon, lib.loc = '../packages/')
library(ggpubr, lib.loc = '../packages/')

theme_set(theme_classic())

#### import data about playbacks ####
## read in age data
ages <- readRDS('../data_processed/elephant_behaviour_proportions.RDS') %>% 
  select(pb_num, subject, targeted_elephant,    # random effects
         stim_type,age_category,partner_age_category,age_difference, # exposures
         age,partner_age,focal,dyad_partner,    # reference info
         group_size                             # remove any with only 2
  ) %>% 
  distinct()

## read in stimulus start time data
stim_starts <- readRDS('../data_processed/stimuli.RDS') %>% 
  filter(status == 'START' & behavior == 'STIMULUS') %>% 
  select(pb_num,time,stim_num,comment,stim_type)

## remove false starts
table(stim_starts$pb_num)
multiple_starts <- c(10, 24, 29, 32, 46, 53)
check <- stim_starts %>% 
  filter(pb_num %in% multiple_starts) # for stim 10+46+53 take first time, for 24+29+32 use second.
for(i in multiple_starts){
  x <- check %>% filter(pb_num == i)
  check <- anti_join(check, x)
  if(i %in% c(10,46,53)){
    x <- x[1,]
  }
  if(i %in% c(24,29,32)){
    x <- x[2,]
  }
  check <- rbind(check, x)
}
stim_starts <- stim_starts %>% 
  filter(! pb_num %in% multiple_starts) %>% 
  rbind(check) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(stim_start = round(time, 0)) %>% 
  select(pb_num, stim_start,stim_num,stim_type)
rm(check, i, multiple_starts, x) ; gc()

## read in stop time data
stim_stops <- readRDS('../data_processed/stimuli.RDS') %>% 
  filter(status == 'STOP' & behavior == 'STIMULUS') %>% 
  select(pb_num,time,stim_num,comment)

## remove false starts
(multiple_stops <- table(stim_stops$pb_num) %>% 
  as.data.frame() %>% 
  filter(Freq > 1))
multiple_stops <- c(10, 24, 29, 32, 46, 53)
check <- stim_stops %>% 
  filter(pb_num %in% multiple_stops) # 10+46 = take first time, 24+29+32+53 = take last time
for(i in multiple_stops){
  x <- check %>% filter(pb_num == i)
  check <- anti_join(check, x)
  if(i %in% c(10,46)){
    x <- x[1,]
  }
  if(i %in% c(24,29,32,53)){
    x <- x[nrow(x),]
  }
  check <- rbind(check, x)
}
stim_stops <- stim_stops %>% 
  filter(! pb_num %in% multiple_stops) %>% 
  rbind(check) %>% 
  mutate(time = as.numeric(time)) %>% 
  mutate(stim_stop = round(time, 0)) %>% 
  select(pb_num, stim_stop)
rm(check, i, multiple_stops, x) ; gc()

#### combine with behaviour data to identify before/during/after ####
cols_of_interest <- c('b1_nn_name','b2_nn_name','b3_nn_name','b4_nn_name',
  'b5_nn_name','b6_nn_name','b7_nn_name','b8_nn_name',
  'b1_move_name','b2_move_name','b3_move_name','b4_move_name',
  'b5_move_name','b6_move_name','b7_move_name','b8_move_name',
  'b1_look_name','b2_look_name','b3_look_name','b4_look_name',
  'b5_look_name','b6_look_name','b7_look_name','b8_look_name')
behav <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>% 
  select(subject,pb_num,second,out_frame_name,
         all_of(cols_of_interest)) %>%
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity', values_to = 'action') %>% 
  filter(is.na(action) == FALSE) %>% 
  filter(action != 'impossible_partner') %>% 
  filter(out_frame_name != 'out_of_sight') %>% 
  separate(elephant_activity, into = c('targeted_elephant','activity','name'), sep = '_', remove = T) %>% 
  dplyr::select(-name,-out_frame_name) %>% 
  mutate(targeted_elephant = paste0(targeted_elephant, '_e', pb_num)) %>% 
  left_join(ages[,c('subject','targeted_elephant','age_category','partner_age_category','age','partner_age')],
            by = c('subject','targeted_elephant')) %>% 
  rename(focal = subject,
         partner = targeted_elephant,
         f_age_num = age_category,
         f_age_cat = age,
         p_age_num = partner_age_category,
         p_age_cat = partner_age) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(stim_starts, by = 'pb_num') %>% 
  left_join(stim_stops, by = 'pb_num') %>% 
  mutate(bda = ifelse(second < stim_start, 'before',
                      ifelse(second < stim_stop, 'during','after'))) %>% 
  dplyr::select(pb_num,focal,partner,activity,action,
                stim_num,stim_type,stim_start,stim_stop,second,bda,
                f_age_cat,p_age_cat,f_age_num,p_age_num) %>% 
  mutate(f_age_num = as.factor(f_age_num),
         p_age_num = as.factor(p_age_num),
         age_combo = paste0(f_age_num,'_',p_age_num))
rm(cols_of_interest, stim_starts, stim_stops, ages) ; gc()

## remove individuals where ages are unknown
behav <- behav %>% 
  filter(!is.na(f_age_num))

#### nearest neighbour ####
## select specific data
nn <- behav %>% 
  filter(activity == 'nn') %>% 
  select(-activity, -stim_start, -stim_stop, -second) %>% 
  mutate(prev = NA,
         action = as.numeric(action),
         f_age_num = as.factor(as.numeric(f_age_num)),
         p_age_num = as.factor(as.numeric(p_age_num))) %>% 
  filter(!is.na(p_age_num))

## fill in behaviour in previous second
subjects <- unique(nn$focal)
for(i in 1:length(subjects)){
  focal <- nn %>% filter(focal == subjects[i])
  nn <- nn %>% anti_join(focal, by = 'focal')
  for(j in 2:nrow(focal)){
    focal$prev[j] <- focal$action[j-1]
  }
  nn <- rbind(nn, focal)
}
rm(focal, i, j, subjects) ; gc()

## remove observations where nearest neighbour in previous second was unknown
nn <- nn %>% 
  filter(!is.na(prev))

## set prior ####
get_prior(formula = action ~ 1 + age_combo + stim_type + bda + mo(prev) +  
            (1|focal) + (1|stim_num) + (1|pb_num), 
          data = nn, family = bernoulli("logit"))
priors <- c(
#  # focal age
#  prior(normal(0,1),      class = b,    coef = mof_age_num),
#  prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
#  # partner age
#  prior(normal(0,1),      class = b,    coef = mop_age_num),
#  prior(dirichlet(2,2,2), class = simo, coef = mop_age_num1),
  # interaction
  prior(normal(0,1),      class = b,    coef = age_combo1_2),
  prior(normal(0,1),      class = b,    coef = age_combo1_3),
  prior(normal(0,1),      class = b,    coef = age_combo1_4),
  prior(normal(0,1),      class = b,    coef = age_combo2_1),
  prior(normal(0,1),      class = b,    coef = age_combo2_2),
  prior(normal(0,1),      class = b,    coef = age_combo2_3),
  prior(normal(0,1),      class = b,    coef = age_combo2_4),
  prior(normal(0,1),      class = b,    coef = age_combo3_1),
  prior(normal(0,1),      class = b,    coef = age_combo3_2),
  prior(normal(0,1),      class = b,    coef = age_combo3_3),
  prior(normal(0,1),      class = b,    coef = age_combo3_4),
  prior(normal(0,1),      class = b,    coef = age_combo4_1),
  prior(normal(0,1),      class = b,    coef = age_combo4_2),
  prior(normal(0,1),      class = b,    coef = age_combo4_3),
  prior(normal(0,1),      class = b,    coef = age_combo4_4),
  # stim type
  prior(normal(0,1),      class = b,    coef = stim_typeh),
  prior(normal(0,1),      class = b,    coef = stim_typel),
  # before/during/after
  prior(normal(0,1),      class = b,    coef = bdabefore),
  prior(normal(0,1),      class = b,    coef = bdaduring),
  # action in previous second
  prior(normal(0,1),      class = b,    coef = moprev),
  prior(dirichlet(2),   class = simo, coef = moprev1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
nn_prior <- brm(
  formula = action ~ 1 + age_combo + stim_type + bda + mo(prev) +  
    (1|focal) + (1|stim_num) + (1|pb_num), 
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(nn_prior) # huge variation in prior, but fairly on both sides so good

## fit model ####
nn_fit <- brm(
  formula = action ~ 1 + age_combo + stim_type + bda + mo(prev) +  
    (1|focal) + (1|stim_num) + (1|pb_num), 
  data = nn, family = bernoulli("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)
save.image('nearest_neighbour/neighbour_model_run_bda.RData')

## check model fit
(summary <- summary(nn_fit))
par(mfrow = c(3,1))
hist(summary$rhat)
hist(summary$ess_bulk)
hist(summary$ess_tail)
par(mfrow = c(1,1))

## extract posterior distribution
draws <- as_draws_df(nn_fit) %>% 
  select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>% 
  pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>% 
  mutate(iteration = rep(rep(1:(num_iter/2),
                             each = length(unique(parameter))),
                         num_chains),
         chain = rep(1:num_chains,
                     each = length(unique(parameter))*(num_iter/2)),
         invlogit_draw = invlogit(draw))

## extract marginal effects
marg <- conditional_effects(nn_fit,
                            effects = c('f_age_num','stim_type',
                                        'after_stim','nn_tminus1_num'),
                            categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
f_age_effect <- marg[[1]]
stim_effect <- marg[[2]]
time_effect <- marg[[3]]
prevsec_effect <- marg[[4]]

## plot marginal effects ####
# conditional_effects(nn_fit, effects = 'f_age_num',
#                     categorical = TRUE,
#                     spaghetti = TRUE,
#                     #conditions = c('stim_type'),
#                     method = 'posterior_epred')
(focal_age_plot <- ggplot(f_age_effect)+
   geom_errorbar(aes(x = f_age_num, ymax = upper__, ymin = lower__, colour = cats__), linewidth = 1, width = 0.2)+
   #geom_ribbon(aes(x = f_age_num, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
   geom_point(aes(x = f_age_num, y = estimate__, colour = cats__), cex = 3)+
   #geom_line(aes(x = f_age_num, y = estimate__, colour = cats__), linewidth = 1)+
   xlab(label = 'focal age') + ylab('probability')+
   scale_colour_viridis_d(name = 'age difference:',
                          breaks = c('1','2','3'),
                          labels = c('partner younger',
                                     'age matched',
                                     'partner older'))+
   scale_fill_viridis_d(name = 'age difference:',
                        breaks = c('1','2','3'),
                        labels = c('partner younger',
                                   'age matched',
                                   'partner older')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = focal_age_plot, filename = '../outputs/nn_marginaleffects_focalage.png', device = 'png',
       width = 8.3, height = 5.8)

# conditional_effects(nn_fit, 'stim_type',
#                     categorical = TRUE,
#                     method = 'posterior_epred')
(stim_plot <- ggplot(stim_effect)+
    geom_errorbar(aes(x = stim_type, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type, y = estimate__, colour = cats__),cex = 3)+
    xlab(label = 'stimulus type') + ylab('probability')+
    scale_colour_viridis_d(name = 'neighbour age:',
                           breaks = c('1','2','3'),
                           labels = c('younger', 'matched', 'older'))+
    scale_fill_viridis_d(name = 'neighbour age:',
                         breaks = c('1','2','3'),
                         labels = c('younger', 'matched', 'older'))+
    scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'),
                     limits = c('ctd','l','h')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = stim_plot, filename = '../outputs/nn_marginaleffects_stimtype.png', device = 'png',
       width = 8.3, height = 5.8)

(all_plots <- ggarrange(focal_age_plot, stim_plot, ncol=2, nrow=1, common.legend = TRUE, legend = "bottom"))
ggsave(plot = all_plots, filename = '../outputs/nn_marginaleffects.png', device = 'png',
       width = (5.8*2), height = 8.3)

rm(f_age_effect,prevsec_effect, stim_effect, time_effect) ;gc()

## posterior predictive check ####
pp_check(nn_fit, ndraws = 100) # perfect fit

## plot traces ####
draws %>% 
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
                          "b_stim_typeh","b_stim_typel",
                          "bs_safter_stim_1","sds_safter_stim_1",
                          "s_safter_stim_1[1]","s_safter_stim_1[2]",
                          "s_safter_stim_1[3]","s_safter_stim_1[4]",
                          "s_safter_stim_1[5]","s_safter_stim_1[6]",
                          "s_safter_stim_1[7]","s_safter_stim_1[8]",
                          "bsp_mof_age_num",
                          "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
                          "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
                          "bsp_monn_tminus1_num",
                          "simo_monn_tminus1_num1[1]","simo_monn_tminus1_num1[2]"
  )) %>% 
  ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # mostly fine, but playback ID intercept has a weird unmixed bit

## plot density curves ####
unique(draws$parameter)
draws_cut <- draws %>% 
  filter(parameter %in% c('b_Intercept[1]','b_Intercept[2]',
                          'b_stim_typel','b_stim_typeh',
                          'bsp_mof_age_num','simo_mof_age_num1[1]',
                          'simo_mof_age_num1[2]','simo_mof_age_num1[3]',
                          'bsp_monn_tminus1_num',
                          'simo_monn_tminus1_num1[1]','simo_monn_tminus1_num1[2]',
                          'bs_safter_stim_1','sds_safter_stim_1',
                          's_safter_stim_1[1]','s_safter_stim_1[2]',
                          's_safter_stim_1[3]','s_safter_stim_1[4]',
                          's_safter_stim_1[5]','s_safter_stim_1[6]',
                          's_safter_stim_1[7]','s_safter_stim_1[8]'))


