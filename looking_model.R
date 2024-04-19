#### information ####
# script for looking direction analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
#library(cmdstanr) ; set_cmdstan_path('../packages/.cmdstan/cmdstan-2.31.0/') ; library(brms) ; library(tidyverse) ; library(LaplacesDemon)
library(StanHeaders, lib.loc = '../packages/')
library(rstan, lib.loc = '../packages/')
library(brms, lib.loc = '../packages/')
#library(cmdstanr, lib.loc = '../packages/') # library(cmdstanr)
#set_cmdstan_path('../packages/.cmdstan/cmdstan-2.31.0/')
library(tidyverse, lib.loc = '../packages/')
library(LaplacesDemon, lib.loc = '../packages/')
library(patchwork, lib.loc = '../packages/')

theme_set(theme_classic())
set.seed(12345)

pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1_modelprep.pdf')

# #### data prep ####
# # https://dagitty.net/dags.html?id=dw8twK
# # read in data
# ages <- readRDS('data_processed/behaviour_by_second_indexvariables_bda.RDS') %>%
#   # ages <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS') %>%
#   select(focal, f_age_cat, f_age_num) %>%
#   distinct() %>%
#   filter(!is.na(f_age_cat)) %>%
#   mutate(partner = focal,
#          p_age_cat = f_age_cat,
#          p_age_num = f_age_num)
# 
# stim_starts <- readRDS('data_processed/stimuli.RDS') %>%
#   # stim_starts <- readRDS('../data_processed/stimuli.RDS') %>%
#   filter(status == 'START' & behavior == 'STIMULUS') %>%
#   select(pb_num,time,stim_num,stim_type,group_size,comment)
# table(stim_starts$pb_num)
# multiple_starts <- c(10, 24, 29, 32, 46, 53)
# check <- stim_starts %>%
#   filter(pb_num %in% multiple_starts) # for stim 10+29+46+53 take first time, for 24+32 use second.
# for(i in multiple_starts){
#   x <- check %>% filter(pb_num == i)
#   check <- anti_join(check, x)
#   if(i %in% c(10,29,46,53)){
#     x <- x[1,]
#   }
#   if(i %in% c(24,32)){
#     x <- x[2,]
#   }
#   check <- rbind(check, x)
# }
# stim_starts <- stim_starts %>%
#   filter(! pb_num %in% multiple_starts) %>%
#   rbind(check) %>%
#   mutate(time = as.numeric(time)) %>%
#   mutate(stim_start = round(time, 0)) %>%
#   select(pb_num,stim_start,stim_num,stim_type,group_size)
# 
# ## looking direction data
# cols_of_interest <- c('b1_look','b2_look','b3_look','b4_look',
#                       'b5_look','b6_look','b7_look','b8_look')
# cols_of_interest_name <- c('b1_look_name','b2_look_name','b3_look_name','b4_look_name',
#                            'b5_look_name','b6_look_name','b7_look_name','b8_look_name')
# cols_of_interest_index <- c('b1_look_index','b2_look_index','b3_look_index','b4_look_index',
#                             'b5_look_index','b6_look_index','b7_look_index','b8_look_index')
# look <- readRDS('data_processed/behaviour_by_second_indexvariables.RDS') %>%
#   # look <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
#   filter(out_frame_name == 'in_frame') %>%
#   select(subject,pb_num,second,
#          all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
#   rename(b1_look = b1_look_name, b2_look = b2_look_name,
#          b3_look = b3_look_name, b4_look = b4_look_name,
#          b5_look = b5_look_name, b6_look = b6_look_name,
#          b7_look = b7_look_name, b8_look = b8_look_name) %>%
#   pivot_longer(cols = all_of(cols_of_interest),
#                names_to = 'elephant_activity_name', values_to = 'looking_direction') %>%
#   rename(b1_look = b1_look_index, b2_look = b2_look_index,
#          b3_look = b3_look_index, b4_look = b4_look_index,
#          b5_look = b5_look_index, b6_look = b6_look_index,
#          b7_look = b7_look_index, b8_look = b8_look_index) %>%
#   pivot_longer(cols = all_of(cols_of_interest),
#                names_to = 'elephant_activity_index', values_to = 'look_index') %>%
#   filter(elephant_activity_name == elephant_activity_index) %>%
#   select(-elephant_activity_index) %>%
#   rename(elephant_activity = elephant_activity_name,
#          focal = subject) %>%
#   filter(is.na(look_index) == FALSE) %>%
#   separate(elephant_activity, into = c('partner','activity'), sep = '_', relook = T) %>%
#   mutate(partner = paste0(partner, '_e', pb_num),
#          pb_num = as.numeric(pb_num)) %>%
#   left_join(ages[,c('focal','f_age_cat','f_age_num')], by = 'focal') %>%
#   left_join(ages[,c('partner','p_age_cat','p_age_num')], by = 'partner') %>%
#   left_join(stim_starts, by = 'pb_num') %>%
#   mutate(time_since_stim = second - stim_start,
#          after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60),
#          age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num),
#                                  'partner_younger',
#                                  ifelse(as.numeric(f_age_num) == as.numeric(p_age_num),
#                                         'matched',
#                                         'partner_older'))) %>%
#   select(pb_num,focal,partner,
#          activity,looking_direction,look_index,
#          stim_num,stim_type,
#          time_since_stim, after_stim,
#          f_age_cat,p_age_cat,f_age_num,p_age_num,
#          age_difference) %>%
#   mutate(f_age_num = as.factor(f_age_num),
#          p_age_num = as.factor(p_age_num),
#          age_combo = paste0(f_age_num,'_',p_age_num),
#          look_tminus1 = NA,
#          look_tminus1_num = NA)
# rm(list = ls() [ ! ls() %in% 'look']) ; gc()
# 
# unique(look$focal[is.na(look$f_age_num)])   # b6_e7 = unknown age
# unique(look$partner[is.na(look$p_age_num)]) # b2_e13 = unknown age
# length(which(is.na(look$looking_direction) == TRUE))
# 
# # create variable for looking direction at time t-1
# focals <- unique(look$focal)
# for(f in 1:length(focals)){
#   focal <- look %>% filter(focal == focals[f])
#   look <- look %>% anti_join(focal, by = 'focal')
#   partners <- unique(focal$partner)
#   for(p in 1:length(partners)){
#     focal_partner <- focal %>% filter(partner == partners[p])
#     focal <- focal %>% anti_join(focal_partner, by = 'partner')
#     for(i in 2:nrow(focal_partner)){
#       focal_partner$look_tminus1[i] <- focal_partner$looking_direction[i-1]
#       focal_partner$look_tminus1_num[i] <- focal_partner$look_index[i-1]
#     }
#     focal <- rbind(focal, focal_partner)
#   }
#   look <- rbind(look, focal)
# }
# rm(focal, focals, focal_partner, f, p, i, partners) ; gc()
# 
# # filter to relook elephants with unknown ages
# look_no_na <- look %>%
#   mutate(age_difference = factor(age_difference,
#                                  levels = c('partner_younger',
#                                             'matched',
#                                             'partner_older')),
#          age_diff_num = as.integer(age_difference),
#          f_age_num = as.integer(f_age_num),
#          p_age_num = as.integer(p_age_num)) %>%
#   filter(is.na(age_diff_num) == FALSE) %>%
#   filter(is.na(look_tminus1) == FALSE) %>%
#   mutate(focal_id = as.integer(as.factor(focal)),
#          stim_id = as.integer(as.factor(stim_num))) %>%
#   rename(playback_id = pb_num) %>%
#   select(focal, partner, looking_direction, look_index,
#          f_age_cat, p_age_cat, f_age_num, p_age_num,
#          age_difference, age_diff_num, age_combo,
#          time_since_stim, after_stim, stim_type,
#          look_tminus1, look_tminus1_num,
#          focal_id, stim_id, playback_id)
# str(look_no_na)
# 
# #### set priors ####
# get_prior(formula = looking_direction ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
#             s(after_stim) + mo(look_tminus1_num) +                                          # controls, treat time as a spline
#             (1|focal_id) + (1|stim_id) + (1|playback_id),                                   # random effects
#           data = look_no_na,
#           family = cumulative("logit"))
# priors <- c(
#   # focal age
#   prior(normal(0,1),      class = b,    coef = mof_age_num), # nn = normal(0,0.25)
#   prior(dirichlet(2,2,2), class = simo, coef = mof_age_num1),
#   # partner age
#   #prior(normal(0,0.25),     class = b,    coef = mopartner_age),
#   #prior(dirichlet(2,2,2),   class = simo, coef = mopartner_age1),
#   # age interaction
#   #prior(normal(0,0.25),     class = b,    coef = mofocal_age:mopartner_age),
#   #prior(dirichlet(2,2,2),   class = simo, coef = mofocal_age:mopartner_age1),
#   #prior(dirichlet(2,2,2),   class = simo, coef = mofocal_age:mopartner_age2),
#   prior(normal(0,1),     class = b,    coef = age_combo1_2),
#   prior(normal(0,1),     class = b,    coef = age_combo1_3),
#   prior(normal(0,1),     class = b,    coef = age_combo1_4),
#   prior(normal(0,1),     class = b,    coef = age_combo2_1),
#   prior(normal(0,1),     class = b,    coef = age_combo2_2),
#   prior(normal(0,1),     class = b,    coef = age_combo2_3),
#   prior(normal(0,1),     class = b,    coef = age_combo2_4),
#   prior(normal(0,1),     class = b,    coef = age_combo3_1),
#   prior(normal(0,1),     class = b,    coef = age_combo3_2),
#   prior(normal(0,1),     class = b,    coef = age_combo3_3),
#   prior(normal(0,1),     class = b,    coef = age_combo3_4),
#   prior(normal(0,1),     class = b,    coef = age_combo4_1),
#   prior(normal(0,1),     class = b,    coef = age_combo4_2),
#   prior(normal(0,1),     class = b,    coef = age_combo4_3),
#   prior(normal(0,1),     class = b,    coef = age_combo4_4),
#   # stim type
#   prior(normal(0,1),     class = b,    coef = stim_typeh),
#   prior(normal(0,1),     class = b,    coef = stim_typel),
#   # time spline
#   prior(normal(0,1),     class = b,    coef = safter_stim_1),
#   #prior(student_t(3,0,2.5), class = sds, coef = s(after_stim)), # not sure why this is in get_prior() but not model but it's not needed
#   # action in previous second
#   prior(normal(1,1), # normal(0,0.333),
#         class = b,    coef = molook_tminus1_num),
#   prior(dirichlet(2,2),    class = simo, coef = molook_tminus1_num1))
# 
# #### prior predictive check ####
# num_chains <- 4
# num_iter <- 2000
# lom1_prior <- brm(
#   formula = look_index ~ 1 + mo(f_age_num) + age_combo + stim_type + # fixed effects
#     s(after_stim) + mo(look_tminus1_num) +                           # controls, treat time as a spline
#     (1|focal_id) + (1|stim_id) + (1|playback_id),                    # random effects
#   data = look_no_na,
#   family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# 
# pp_check(lom1_prior) # prior expects 1 and 3 most likely, 2 least likely. data show 1 least likely, 2 middle, 3 most.
# 
# print(paste0('priors set at ',Sys.time()))
# 
# ## reset plotting
# dev.off()
# pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1_modelchecks.pdf')
# 
# #### fit model ####
# lom1_fit <- brm(
#   formula = look_index ~ 1 + mo(f_age_num) + age_combo + stim_type +   # fixed effects
#     s(after_stim) + mo(look_tminus1_num) +                             # controls, treat time as a spline
#     (1|focal_id) + (1|stim_id) + (1|playback_id),                      # random effects
#   data = look_no_na,
#   family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# 
# # save workspace
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_run.RData') # save.image('looking_direction/looking_ordinal_model1_run.RData')
# 
# # inspect model
# summary(lom1_fit)
# print(paste0('model run at ',Sys.time()))
# 
# #### check outputs ####
# load('ele_playbacks/looking_direction/looking_ordinal_model1_run.RData') # load('looking_direction/looking_ordinal_model1_run.RData')
# summary(lom1_fit)
# 
# ## check Stan code
# lom1_fit$model
# lom1_fit$formula
# 
# ## extract posterior distribution
# draws <- as_draws_df(lom1_fit) %>%
#   select(-lprior, -`lp__`)
# parameters <- colnames(draws)[1:(ncol(draws)-3)]
# draws <- draws  %>%
#   pivot_longer(cols = all_of(parameters),
#                names_to = 'parameter',
#                values_to = 'draw') %>%
#   rename(chain = `.chain`,
#          position = `.iteration`,
#          draw_id = `.draw`) %>%
#   mutate(invlogit_draw = invlogit(draw))
# # nearest neighbour version -- run with looking direction first but if it throws an error then come back to the nearest neighbour code to fix it
# # draws <- as_draws_df(nn_fit) %>%
# #   select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>%
# #   pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>%
# #   mutate(iteration = rep(rep(1:(num_iter/2),
# #                              each = length(unique(parameter))),
# #                          num_chains),
# #          chain = rep(1:num_chains,
# #                      each = length(unique(parameter))*(num_iter/2)),
# #          invlogit_draw = invlogit(draw))
# 
# print(paste0('posterior extracted at ',Sys.time()))
# 
# ## look at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,2))
# hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
# par(mfrow = c(1,1))
# 
# #### calculate log cumulative odds ####
# prop <- table(look_no_na$looking_direction) / nrow(look_no_na)
# cum_prop <- cumsum(prop)
# log_cum_odds <- logit(cum_prop)
# 
# #### plot marginal effects ####
# ## extract marginal effects
# marg <- conditional_effects(lom1_fit,
#                             categorical = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# agecombo_effect <- marg[[1]]
# stim_effect <- marg[[2]]
# agefocal_effect <- marg[[3]]
# prevsec_effect <- marg[[4]]
# time_effect <- marg[[5]]
# 
# ## plot marginal effects
# (focal_age_plot <- ggplot(agefocal_effect)+
#     # geom_ribbon(aes(x = f_age_num,
#     #                 ymax = upper__, ymin = lower__,
#     #                 fill = cats__),
#     #             alpha = 0.4)+
#     # geom_line(aes(x = f_age_num,
#     #               y = estimate__,
#     #               colour = cats__),
#     #           linewidth = 1)+
#     geom_errorbar(aes(x = f_age_num,
#                       ymax = upper__, ymin = lower__,
#                       colour = cats__),
#                   linewidth = 1, width = 0.2)+
#     geom_point(aes(x = f_age_num,
#                    y = estimate__,
#                    colour = cats__),
#                size = 3)+ # cex = 3?
#     xlab(label = 'focal age')+
#     ylab('probability of looking direction')+
#     scale_colour_viridis_d(name = 'looking direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('look towards',
#                                       'side on',
#                                       'look away'))+
#     scale_fill_viridis_d(name = 'looking direction:',
#                          breaks = c('1','2','3'),
#                          labels = c('look towards',
#                                     'side on',
#                                     'look away'))+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)))
# ggsave(plot = focal_age_plot, filename = 'outputs/looking_ordinal_model_1/looking_ordinal1_marginaleffects_focalage_agecombo.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# focal_age_labels <- c('focal age category 1',
#                       'focal age category 2',
#                       'focal age category 3',
#                       'focal age category 4')
# names(focal_age_labels) <- 1:4
# (agecombo_plot <- agecombo_effect %>%
#     separate(col = age_combo, sep = '_', relook = F,
#              into = c('focal_age','partner_age')) %>%
#     mutate(agecombo = paste0(focal_age,'-',partner_age)) %>%
#     ggplot()+
#     geom_errorbar(aes(#x = agecombo,
#                       x = partner_age,
#                       colour = as.factor(cats__), # looking direction?
#                       ymax = upper__, ymin = lower__),
#                   linewidth = 1,
#                   width = 0.4)+
#     geom_point(aes(#x = agecombo,
#                    x = partner_age,
#                    colour = as.factor(cats__),    # looking direction?
#                    #shape = focal_age,
#                    y = estimate__),
#                size = 3)+
#     # geom_ribbon(aes(#x = agecombo,
#     #                 x = as.numeric(partner_age),
#     #                 fill = as.factor(cats__),     # looking direction?
#     #                 ymax = upper__, ymin = lower__),
#     #             alpha = 0.4)+
#     # geom_line(aes(#x = agecombo,
#     #               x = as.numeric(partner_age),
#     #               colour = as.factor(cats__),     # looking direction?
#     #               y = estimate__),
#     #           linewidth = 1)+
#     facet_wrap(. ~ focal_age,
#                labeller = labeller(focal_age = focal_age_labels))+
#     ylab('probability of looking direction')+
#     scale_colour_viridis_d(name = 'looking direction:',
#                            breaks = c('1','2','3'),
#                            labels = c('look towards',
#                                       'side on',
#                                       'look away'))+
#     # scale_fill_viridis_d(name = 'looking direction:',
#     #                      breaks = c('1','2','3'),
#     #                      labels = c('look towards',
#     #                                 'side on',
#     #                                 'look away'))+
#     scale_x_discrete(name = 'partner age category')+
#     #scale_x_continuous(name = 'partner age category')+
#     theme(legend.position = 'bottom',
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 12),
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 10)) )
# ggsave(plot = agecombo_plot, filename = 'outputs/looking_ordinal_model_1/looking_ordinal1_marginaleffects_agepartner_agecombo.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# (stim_plot <- ggplot(stim_effect)+
#    geom_errorbar(aes(x = stim_type,
#                      ymin = lower__, ymax = upper__,
#                      colour = cats__),
#                  linewidth = 1, width = 0.2)+
#    geom_point(aes(x = stim_type,
#                   y = estimate__,
#                   colour = cats__),
#               cex = 3)+ # size = 3?
#    xlab(label = 'stimulus type') + ylab('probability of looking direction')+
#    scale_colour_viridis_d(name = 'looking direction:',
#                           breaks = c('1','2','3'),
#                           labels = c('look towards', 'side on', 'look away'))+
#    scale_x_discrete(breaks = c('ctd','l','h'),
#                     labels = c('dove (control)', 'lion', 'human'),
#                     limits = c('ctd','l','h'))+
#    theme(legend.position = 'bottom',
#          axis.title = element_text(size = 16),
#          axis.text = element_text(size = 12),
#          legend.title = element_text(size = 12),
#          legend.text = element_text(size = 10)) )
# ggsave(plot = stim_plot, filename = 'outputs/looking_ordinal_model_1/looking_ordinal1_marginaleffects_stimtype_agecombo.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# (focal_age_plot + agecombo_plot + stim_plot) +
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(),
#        filename = 'outputs/looking_ordinal_model_1/looking_ordinal1_marginaleffects.png',
#        device = 'png', width = (5.8*3), height = 8.3)
# print(paste0('marginal effects plotted at ',Sys.time()))
# 
# #### posterior predictive check ####
# pp_check(lom1_fit, ndraws = 100) # really good fit
# 
# #### plot traces and density curves ####
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
#                           "b_stim_typeh","b_stim_typel",
#                           "safter_stim_1","sds_s(after_stim)",
#                           "b_age_combo1_2","b_age_combo1_3","b_age_combo1_4",
#                           "b_age_combo2_1","b_age_combo2_2","b_age_combo2_3","b_age_combo2_4",
#                           "b_age_combo3_1","b_age_combo3_2","b_age_combo3_3","b_age_combo3_4",
#                           "b_age_combo4_1","b_age_combo4_2","b_age_combo4_3","b_age_combo4_4",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "bsp_mof_age_num",#"bsp_mopartner_age",
#                           "bsp_molook_tminus1_num",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           #"simo_mopartner_age1[1]","simo_mopartner_age1[2]","simo_mopartner_age1[3]",
#                           "simo_molook_tminus1_num1[1]","simo_molook_tminus1_num1[2]"))
# ggplot(data = draws_cut,
#        aes(x = position, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mixing doesn't look brilliant, esp. for bsp_mof_age_num, but only horrendous one is playback ID
# 
# ## look at intercepts (estimates of cutpoints between categories on linear model scale)
# b_int1 <- draws_cut %>% filter(parameter == 'b_Intercept[1]')
# b_int2 <- draws_cut %>% filter(parameter == 'b_Intercept[2]')
# par(mfrow = c(2,1))
# hist(b_int1$draw, main = 'b_int1') ; hist(b_int2$draw, main = 'b_int2')
# #hist(b_int1$invlogit_draw, main = 'invlogit b_int1') ; hist(b_int2$invlogit_draw, main = 'invlogit b_int2')
# 
# ## stim type
# lion <- draws_cut %>% filter(parameter == 'b_stim_typel')
# human <- draws_cut %>% filter(parameter == 'b_stim_typeh')
# plot(density(lion$draw), main = 'lion vs dove') ; abline(v = 0, lty = 2)
# plot(density(human$draw), main = 'human vs dove') ; abline(v = 0, lty = 2)
# 
# ## focal age
# age1 <- draws_cut %>% filter(parameter == 'bsp_mof_age_num')
# age2 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[1]')
# age3 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[2]')
# age4 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[3]')
# par(mfrow = c(2,2))
# plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
# plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
# plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)
# 
# ## looking direction in previous second
# prevsec1 <- draws_cut %>% filter(parameter == 'bsp_molook_tminus1_num')
# prevsec2 <- draws_cut %>% filter(parameter == 'simo_molook_tminus1_num1[1]')
# prevsec3 <- draws_cut %>% filter(parameter == 'simo_molook_tminus1_num1[2]')
# par(mfrow = c(3,1))
# plot(density(prevsec1$draw), main = 't-1 younger') ; abline(v = 0, lty = 2)
# plot(density(prevsec2$draw), main = 't-1 matched') ; abline(v = 0, lty = 2)
# plot(density(prevsec3$draw), main = 't-1 older') ; abline(v = 0, lty = 2)
# 
# # ## time since stimulus -- come back to this!
# # timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
# # times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
# # time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
# # time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
# # time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
# # time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
# # time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
# # time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
# # time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
# # time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
# # par(mfrow = c(5,2))
# # plot(density(timeb$draw), main = 'time slope') ; abline(v = 0, lty = 2)
# # plot(density(times$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
# # plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
# # plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
# # plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
# # plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
# # plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
# # plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
# # plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
# # plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)
# 
# print(paste0('posterior predictive check and traceplots completed at ',Sys.time()))
# 
# #### plot raw ####
# ## define labels for plotting
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# ## plot overall
# ggplot(look_no_na, aes(x = f_age_num, y = look_index,
#                        colour = age_difference))+
#   geom_jitter(alpha = 0.1)+
#   facet_wrap(. ~ stim_type,
#              labeller = labeller(stim_type = stim_labels))+
#   scale_y_continuous(name = 'focal looking direction relative to target',
#                      breaks = c(1,2,3),
#                      labels = c('look directly at','side-on','older'))+
#   labs(colour = 'age difference')
# 
# ## plot control data
# look_no_na %>%
#   filter(stim_type == 'ctd') %>%
#   ggplot(aes(x = time_since_stim, y = look_index,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(f_age_num ~ factor(age_difference,
#                                 levels = c('partner_younger','matched','partner_older')),
#              labeller = labeller(f_age_num = age_labels))+
#   scale_x_continuous(name = 'time since stimulus started (s)')+
#   ggtitle('dove (raw data)')
# 
# ## plot lion data
# look_no_na %>%
#   filter(stim_type == 'l') %>%
#   ggplot(aes(x = time_since_stim, y = look_index,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(f_age_num ~ factor(age_difference,
#                                 levels = c('partner_younger','matched','partner_older')),
#              labeller = labeller(f_age_num = age_labels))+
#   scale_x_continuous(name = 'time since stimulus started (s)')+
#   ggtitle('lion (raw data)')
# 
# ## plot human data
# look_no_na %>%
#   filter(stim_type == 'h') %>%
#   ggplot(aes(x = time_since_stim, y = look_index,
#              group = focal_id))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(colour = rgb(0,0,1,0.01))+
#   #geom_line()+
#   facet_grid(f_age_num ~ factor(age_difference,
#                                 levels = c('partner_younger','matched','partner_older')),
#              labeller = labeller(f_age_num = age_labels))+
#   scale_x_continuous(name = 'time since stimulus started (s)')+
#   ggtitle('human (raw data)')
# 
# print(paste0('raw data plotted at ',Sys.time()))
# 
# ## reset plotting
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_run.RData') # save.image('looking_direction/looking_ordinal_model1_run.RData')
# dev.off()
# #pdf('outputs/looking_ordinal_model_1/looking_ordinal_model1predictions.pdf')
# 
#### predict from model ####
load('ele_playbacks/looking_direction/looking_ordinal_model1_run.RData') # load('looking_direction/looking_ordinal_model1_run.RData')
rm(list = ls()[! ls() %in% c('lom1_fit','look_no_na')]) ; gc()

pred <- posterior_epred(object = lom1_fit,
                        newdata = look_no_na)
save.image('ele_playbacks/looking_direction/looking_ordinal_model1_predictions.RData')

## convert to data frame
look_no_na$data_row <- 1:nrow(look_no_na)
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
pred1 <- extract_predictions(prediction_array = pred, layer = 1, df = look_no_na)
pred2 <- extract_predictions(prediction_array = pred, layer = 2, df = look_no_na)
pred3 <- extract_predictions(prediction_array = pred, layer = 3, df = look_no_na)

## combine data frames
pred <- rbind(pred1, pred2, pred3)
save.image('looking_direction/looking_ordinal_model1_predictions.RData')
rm(pred1, pred2, pred3) ; gc()

print(paste0('predictions calculated at ',Sys.time()))

#### plot predictions ####
#load('looking_direction/looking_ordinal_model1_predictions.RData')  #load('ele_playbacks/looking_direction/looking_ordinal_model1_predictions.RData')

## make labels for looking in previous second
prevsec_labels <- c('directly away at t-1',
                    'side-on at t-1',
                    'directly towards at t-1')
names(prevsec_labels) <- 1:5

## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
(ctd_plot <- pred %>%
    filter(stim_type == 'ctd',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ after_stim,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'cape turtle dove (control)')+
    theme(legend.position = 'bottom'))
(lion_plot <- predictions_all %>%
    filter(stim_type == 'l',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ after_stim,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'lion')+
    theme(legend.position = 'bottom'))
(human_plot <- predictions_all %>%
    filter(stim_type == 'h',
           after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
    ggplot()+
    geom_violin(aes(x = as.factor(f_age_num), y = prediction,
                    fill = factor(pred_type, levels = c('look directly away',
                                                        'side-on',
                                                        'look at directly')),
                    colour = factor(pred_type, levels = c('look directly away',
                                                          'side-on',
                                                          'look at directly'))
    )) +
    facet_grid(look_tminus1_num ~ after_stim,
               labeller = labeller(look_tminus1_num = prevsec_labels))+
    scale_fill_viridis_d()+
    scale_colour_viridis_d()+
    labs(colour = 'predicted looking direction relative to focal:',
         fill = 'predicted looking direction relative to focal:',
         x = 'age category of focal elephant',
         y = 'proportion of predictions',
         title = 'human')+
    theme(legend.position = 'bottom'))
(ctd_plot + lion_plot + human_plot)+
  plot_alookotation(tag_levels = 'a')
ggsave(plot = last_plot(), file = '../outputs/looking_ordinal_model_1/looking_ordinal_model1_predictions_violin.png',
       device = 'png', height = 8, width = 24)

## reset plotting
dev.off()
# pdf('../outputs/looking_ordinal_model_1/looking_ordinal_model1_modelcontrasts.pdf')

# #### graph contrasts from predictions and extract coefficients ####
# #CALCULATE POSTERIOR CONTRASTS FROM PREDICTIONS
# # load('ele_playbacks/looking_direction/looking_ordinal_model1_predictions.RData') ; load('looking_direction/looking_ordinal_model1_predictions.RData')
# rm(prevsec_labels, ctd_plot, human_plot, lion_plot, predictions_all) ; gc()
# pdf('outputs/looking_ordinal_model1_model/looking_ordinal_model1_modelcontrasts.pdf')
# 
# ## stim type ####
# ## redo predictions with different stimulus types: all doves
# ctd_look <- look_no_na %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'ctd',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# ctd_mtx <- posterior_epred(object = look_fit, newdata = ctd_look)
# colnames(ctd_mtx) <- ctd_look$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all lions
# lion_look <- look_no_na %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'l',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# lion_mtx <- posterior_epred(object = look_fit, newdata = lion_look)
# colnames(lion_mtx) <- lion_look$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all humans
# human_look <- look_no_na %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'h',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# human_mtx <- posterior_epred(object = look_fit, newdata = human_look)
# colnames(human_mtx) <- human_look$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_stimuluscontrasts.RData')
# 
# ## count types of each prediction
# #load('looking_direction/looking_ordinal_model1_stimuluscontrasts.RData')
# # count_values <- function(vector, levels = c(1,2,3)) {
# #   x <- tabulate(factor(vector, levels), length(levels))
# #   return(list(x))
# # }
# stim_pred <- ctd_look %>%
#   dplyr::select(-stim_type) %>%
#   # mutate(ctd_count = apply(ctd_mtx, 2, count_values),
#   #        lion_count = apply(lion_mtx, 2, count_values),
#   #        human_count = apply(human_mtx, 2, count_values)) %>%
#   # ulookest(c(ctd_count, lion_count, human_count)) %>% # I've done something weird with the count_values function so for now this needs ulookesting twice, but should probably fix it at some point! For now this works!
#   # ulookest(c(ctd_count, lion_count, human_count)) %>%
#   mutate(ctd_prop1_mu = apply(ctd_mtx[,,1], 2, mean),
#          ctd_prop2_mu = apply(ctd_mtx[,,2], 2, mean),
#          ctd_prop3_mu = apply(ctd_mtx[,,3], 2, mean),
#          ctd_prop1_sd = apply(ctd_mtx[,,1], 2, sd),
#          ctd_prop2_sd = apply(ctd_mtx[,,2], 2, sd),
#          ctd_prop3_sd = apply(ctd_mtx[,,3], 2, sd),
#          lion_prop1_mu = apply(lion_mtx[,,1], 2, mean),
#          lion_prop2_mu = apply(lion_mtx[,,2], 2, mean),
#          lion_prop3_mu = apply(lion_mtx[,,3], 2, mean),
#          lion_prop1_sd = apply(lion_mtx[,,1], 2, sd),
#          lion_prop2_sd = apply(lion_mtx[,,2], 2, sd),
#          lion_prop3_sd = apply(lion_mtx[,,3], 2, sd),
#          human_prop1_mu = apply(human_mtx[,,1], 2, mean),
#          human_prop2_mu = apply(human_mtx[,,2], 2, mean),
#          human_prop3_mu = apply(human_mtx[,,3], 2, mean),
#          human_prop1_sd = apply(human_mtx[,,1], 2, sd),
#          human_prop2_sd = apply(human_mtx[,,2], 2, sd),
#          human_prop3_sd = apply(human_mtx[,,3], 2, sd)) %>%
#   pivot_longer(cols = c(ctd_prop1_mu,ctd_prop2_mu,ctd_prop3_mu,
#                         lion_prop1_mu,lion_prop2_mu,lion_prop3_mu,
#                         human_prop1_mu,human_prop2_mu,human_prop3_mu),
#                names_to = 'stim_propage_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(ctd_prop1_sd,ctd_prop2_sd,ctd_prop3_sd,
#                         lion_prop1_sd,lion_prop2_sd,lion_prop3_sd,
#                         human_prop1_sd,human_prop2_sd,human_prop3_sd),
#                names_to = 'stim_propage_sd', values_to = 'stdv_propn') %>%
#   separate(col = stim_propage_mu, into = c('stim_propage_mu','mu'),
#            sep = '_m', relook = T) %>%
#   select(-mu) %>%
#   separate(col = stim_propage_sd, into = c('stim_propage_sd','sd'),
#            sep = '_s', relook = T) %>%
#   select(-sd) %>%
#   filter(stim_propage_mu == stim_propage_sd) %>%
#   separate(col = stim_propage_mu, into = c('stim_type', 'look_pred'),
#            sep = '_prop', relook = T) %>%
#   select(-stim_propage_sd) %>%
#   mutate(look_pred = as.numeric(look_pred)) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older')))
# 
# ## convert full predictive distribution to long format
# stim_pred_all <- ctd_mtx[,,1] %>%
#   as.data.frame()
# colnames(stim_pred_all) <- rownames(ctd_look)
# stim_pred_all <- pivot_longer(stim_pred_all, cols = everything(),
#                               names_to = 'rownum', values_to = 'probability')
# ctd_look$rownum <- rownames(ctd_look)
# stim_pred_all <- stim_pred_all %>%
#   left_join(ctd_look, by = 'rownum') %>%
#   mutate(predict_num = 1,
#          predict_cat = 'younger')
# for(i in 2:3){
#   stim_pred_i <- ctd_mtx[,,i] %>%
#     as.data.frame()
#   colnames(stim_pred_i) <- rownames(ctd_look)
#   stim_pred_i <- pivot_longer(stim_pred_i, cols = everything(),
#                               names_to = 'rownum', values_to = 'probability')
#   stim_pred_i <- stim_pred_i %>%
#     left_join(ctd_look, by = 'rownum') %>%
#     mutate(predict_num = i,
#            predict_cat = ifelse(i == 2, 'matched', 'older'))
#   stim_pred_all <- rbind(stim_pred_all, stim_pred_i)
# }
# 
# ## calculate contrasts
# ctd_vs_lion_age1 <- lion_mtx[,,1] - ctd_mtx[,,1]
# ctd_vs_lion_age2 <- lion_mtx[,,2] - ctd_mtx[,,2]
# ctd_vs_lion_age3 <- lion_mtx[,,3] - ctd_mtx[,,3]
# ctd_vs_human_age1 <- human_mtx[,,1] - ctd_mtx[,,1]
# ctd_vs_human_age2 <- human_mtx[,,2] - ctd_mtx[,,2]
# ctd_vs_human_age3 <- human_mtx[,,3] - ctd_mtx[,,3]
# lion_vs_human_age1 <- human_mtx[,,1] - lion_mtx[,,1]
# lion_vs_human_age2 <- human_mtx[,,2] - lion_mtx[,,2]
# lion_vs_human_age3 <- human_mtx[,,3] - lion_mtx[,,3]
# 
# ## plot -- no difference between stimuli
# plot(density(ctd_vs_lion_age1), col = 'red', las = 1, xlim = c(-0.1, 0.1),
#      main = 'contrasts between stim types:\nred = younger, purple = match, blue = older;\nsolid = ctd vs l, dashed = ctd vs h, dotted = l vs h')
# mean(ctd_vs_lion_age1) ; sd(ctd_vs_lion_age1)
# # -3.6556e-05 ± 0.000885717
# lines(density(ctd_vs_lion_age2), col = 'purple')
# mean(ctd_vs_lion_age2) ; sd(ctd_vs_lion_age2)
# # 8.888346e-06 ± 0.001068246
# lines(density(ctd_vs_lion_age3), col = 'blue')
# mean(ctd_vs_lion_age3) ; sd(ctd_vs_lion_age3)
# # 2.766766e-05 ± 0.001021074
# lines(density(ctd_vs_human_age1), col = 'red', lty = 2)
# mean(ctd_vs_human_age1) ; sd(ctd_vs_human_age1)
# # -0.0001763442 ± 0.0007313818
# lines(density(ctd_vs_human_age2), col = 'purple', lty = 2)
# mean(ctd_vs_human_age2) ; sd(ctd_vs_human_age2)
# # -3.051439e-05 ± 0.0009024247
# lines(density(ctd_vs_human_age3), col = 'blue', lty = 2)
# mean(ctd_vs_human_age3) ; sd(ctd_vs_human_age3)
# # 0.0002068586 ± 0.0008494267
# lines(density(lion_vs_human_age1), col = 'red', lty = 3)
# mean(lion_vs_human_age1) ; sd(lion_vs_human_age1)
# # -0.0001397882 ± 0.0010269
# lines(density(lion_vs_human_age2), col = 'purple', lty = 3)
# mean(lion_vs_human_age2) ; sd(lion_vs_human_age2)
# # -3.940274e-05 ± 0.001262442
# lines(density(lion_vs_human_age3), col = 'blue', lty = 3)
# mean(lion_vs_human_age3) ; sd(lion_vs_human_age3)
# # 0.0001791909 ± 0.001200902
# 
# ## summarise contrasts
# contrasts <- look_no_na %>%
#   select(-stim_type) %>%
#   mutate(ctd_vs_lion_age1_mu = apply(ctd_vs_lion_age1, 2, mean),
#          ctd_vs_lion_age1_sd = apply(ctd_vs_lion_age1, 2, sd),
#          ctd_vs_lion_age2_mu = apply(ctd_vs_lion_age2, 2, mean),
#          ctd_vs_lion_age2_sd = apply(ctd_vs_lion_age2, 2, sd),
#          ctd_vs_lion_age3_mu = apply(ctd_vs_lion_age3, 2, mean),
#          ctd_vs_lion_age3_sd = apply(ctd_vs_lion_age3, 2, sd),
#          ctd_vs_human_age1_mu = apply(ctd_vs_human_age1, 2, mean),
#          ctd_vs_human_age1_sd = apply(ctd_vs_human_age1, 2, sd),
#          ctd_vs_human_age2_mu = apply(ctd_vs_human_age2, 2, mean),
#          ctd_vs_human_age2_sd = apply(ctd_vs_human_age2, 2, sd),
#          ctd_vs_human_age3_mu = apply(ctd_vs_human_age3, 2, mean),
#          ctd_vs_human_age3_sd = apply(ctd_vs_human_age3, 2, sd),
#          lion_vs_human_age1_mu = apply(lion_vs_human_age1, 2, mean),
#          lion_vs_human_age1_sd = apply(lion_vs_human_age1, 2, sd),
#          lion_vs_human_age2_mu = apply(lion_vs_human_age2, 2, mean),
#          lion_vs_human_age2_sd = apply(lion_vs_human_age2, 2, sd),
#          lion_vs_human_age3_mu = apply(lion_vs_human_age3, 2, mean),
#          lion_vs_human_age3_sd = apply(lion_vs_human_age3, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(ctd_vs_lion_age1_mu,ctd_vs_lion_age2_mu,ctd_vs_lion_age3_mu,
#                         ctd_vs_human_age1_mu,ctd_vs_human_age2_mu,ctd_vs_human_age3_mu,
#                         lion_vs_human_age1_mu,lion_vs_human_age2_mu,lion_vs_human_age3_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','look_pred'),
#            sep = '_age', relook = T) %>%
#   separate(look_pred, into = c('look_pred','mu'),
#            sep = '_', relook = T) %>%
#   mutate(look_pred = as.numeric(look_pred)) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older'))) %>%
#   select(-mu, -ctd_vs_lion_age1_sd, -ctd_vs_lion_age2_sd, -ctd_vs_lion_age3_sd,
#          -ctd_vs_human_age1_sd, -ctd_vs_human_age2_sd, -ctd_vs_human_age3_sd,
#          -lion_vs_human_age1_sd, -lion_vs_human_age2_sd, -lion_vs_human_age3_sd)
# 
# ## plot contrasts
# # stim_pred %>%
# #   dplyr::select(ctd_lion, ctd_human, lion_human, pred_type) %>%
# #   pivot_longer(cols = c('ctd_lion', 'ctd_human', 'lion_human'),
# #                names_to = 'contrast') %>%
# #   ggplot()+
# #   geom_density(aes(x = value, colour = contrast))+
# #   facet_wrap(. ~ pred_type)
# 
# stim_pred %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = as.factor(f_age_num)))+
#   facet_wrap(pred_type ~ stim_type, scales = 'free')
# 
# stim_pred_all %>% # something is wrong with this - they're only ctd
#   ggplot()+
#   geom_density(aes(x = probability, colour = predict_cat))+
#   facet_wrap(stim_type ~ f_age_num, scales = 'free_y')
# 
# contrasts_long <- contrasts_long %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   separate(contrast, into = c('stim_a','stim_b'), sep = '_vs_', relook = F) %>%
#   select(pred_type, f_age_num, difference, stim_a, stim_b, contrast, after_stim, look_tminus1_num) %>%
#   mutate(look_tminus1 = ifelse(look_tminus1_num == 1,
#                              'neighbour younger at t-1',
#                              ifelse(look_tminus1_num == 2,
#                                     'neighbour matched at t-1',
#                                     'neighbour older at t-1')))
# 
# contrasts_long %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast)
# 
# #pdf('../outputs/looking_ordinal_model_1/looking_ordinal_model1_contrasts_stimuli.pdf')
# for(i in unique(contrasts_long$contrast)){
#   plot <- contrasts_long %>%
#     filter(contrast == i) %>%
#     ggplot()+
#     geom_density(aes(x = difference, colour = pred_type))+
#     facet_wrap(look_tminus1 ~ f_age_num,
#                scales = 'free')+
#     labs(title = i)
#   print(plot)
# }
# #dev.off()
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_stimuluscontrasts.RData')
# 
# ## focal age ####
# # load('looking_direction/looking_ordinal_model1_stimuluscontrasts.RData')
# rm(ctd_look, ctd_mtx, human_look, human_mtx, lion_look, lion_mtx,
#    contrasts, contrasts_long, stim_pred,
#    ctd_vs_human_age1, ctd_vs_human_age2, ctd_vs_human_age3,
#    ctd_vs_lion_age1, ctd_vs_lion_age2, ctd_vs_lion_age3,
#    lion_vs_human_age1, lion_vs_human_age2, lion_vs_human_age3) ; gc()
# 
# ## predict with original ages
# age_look_org <- look_no_na %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# age_mtx_org <- posterior_epred(object = look_fit, newdata = age_look_org)
# colnames(age_mtx_org) <- age_look_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with altered ages
# age_look_alt <- look_no_na %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(f_age_num_original = f_age_num) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1),
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(f_age_num_original)
# age_mtx_alt <- posterior_epred(object = look_fit, newdata = age_look_alt)
# colnames(age_mtx_alt) <- age_look_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_agecontrasts.RData')
# 
# ## summarise and convert to long format
# age_pred <- age_look_org %>%
#   #dplyr::select(-f_age_num) %>%
#   mutate(age_org_prop1_mu = apply(age_mtx_org[,,1], 2, mean),
#          age_org_prop2_mu = apply(age_mtx_org[,,2], 2, mean),
#          age_org_prop3_mu = apply(age_mtx_org[,,3], 2, mean),
#          age_org_prop1_sd = apply(age_mtx_org[,,1], 2, sd),
#          age_org_prop2_sd = apply(age_mtx_org[,,2], 2, sd),
#          age_org_prop3_sd = apply(age_mtx_org[,,3], 2, sd),
#          age_alt_prop1_mu = apply(age_mtx_alt[,,1], 2, mean),
#          age_alt_prop2_mu = apply(age_mtx_alt[,,2], 2, mean),
#          age_alt_prop3_mu = apply(age_mtx_alt[,,3], 2, mean),
#          age_alt_prop1_sd = apply(age_mtx_alt[,,1], 2, sd),
#          age_alt_prop2_sd = apply(age_mtx_alt[,,2], 2, sd),
#          age_alt_prop3_sd = apply(age_mtx_alt[,,3], 2, sd)) %>%
#   pivot_longer(cols = c(age_org_prop1_mu,age_org_prop2_mu,age_org_prop3_mu,
#                         age_alt_prop1_mu,age_alt_prop2_mu,age_alt_prop3_mu),
#                names_to = 'focal_agelook_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(age_org_prop1_sd,age_org_prop2_sd,age_org_prop3_sd,
#                         age_alt_prop1_sd,age_alt_prop2_sd,age_alt_prop3_sd),
#                names_to = 'focal_agelook_sd', values_to = 'stdv_propn') %>%
#   separate(col = focal_agelook_mu, into = c('focal_agelook_mu','mu'),
#            sep = '_m', relook = T) %>%
#   separate(col = focal_agelook_sd, into = c('focal_agelook_sd','sd'),
#            sep = '_s', relook = T) %>%
#   select(-mu, -sd) %>%
#   filter(focal_agelook_mu == focal_agelook_sd) %>%
#   separate(col = focal_agelook_mu, into = c('original_altered', 'look_pred'),
#            sep = '_prop', relook = T) %>%
#   select(-focal_agelook_sd) %>%
#   mutate(look_pred = as.numeric(look_pred),
#          f_age_num = ifelse(original_altered == 'age_org',
#                             f_age_num,
#                             ifelse(original_altered == 'age_alt' & f_age_num == 4,
#                                    1, f_age_num + 1))) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older')))
# 
# ## convert full predictive distribution to long format
# age_pred_all <- age_mtx_org[,,1] %>%
#   as.data.frame()
# colnames(age_pred_all) <- rownames(age_look_org)
# age_pred_all <- pivot_longer(age_pred_all, cols = everything(),
#                              names_to = 'rownum', values_to = 'probability')
# age_look_org$rownum <- rownames(age_look_org)
# age_pred_all <- age_pred_all %>%
#   left_join(age_look_org, by = 'rownum') %>%
#   mutate(predict_num = 1,
#          predict_cat = 'younger')
# for(i in 2:3){
#   age_pred_i <- age_mtx_org[,,i] %>%
#     as.data.frame()
#   colnames(age_pred_i) <- rownames(age_look_org)
#   age_pred_i <- pivot_longer(age_pred_i, cols = everything(),
#                              names_to = 'rownum', values_to = 'probability')
#   age_pred_i <- age_pred_i %>%
#     left_join(age_look_org, by = 'rownum') %>%
#     mutate(predict_num = i,
#            predict_cat = ifelse(i == 2, 'matched', 'older'))
#   age_pred_all <- rbind(age_pred_all, age_pred_i)
# }
# 
# ## calculate contrasts
# alt_vs_org_young <- age_mtx_alt[,which(look_no_na$f_age_num != 4),1] - age_mtx_org[,which(look_no_na$f_age_num != 4),1]
# alt_vs_org_match <- age_mtx_alt[,which(look_no_na$f_age_num != 4),2] - age_mtx_org[,which(look_no_na$f_age_num != 4),2]
# alt_vs_org_older <- age_mtx_alt[,which(look_no_na$f_age_num != 4),3] - age_mtx_org[,which(look_no_na$f_age_num != 4),3]
# 
# ## plot
# plot(density(alt_vs_org_young), col = 'red', las = 1, xlim = c(-0.1, 0.1),
#      main = 'contrasts between adjacent age category:\nred = younger, purple = match, blue = older')
# mean(alt_vs_org_young) ; sd(alt_vs_org_young)
# # 0.001676816 ± 0.002277281
# lines(density(alt_vs_org_match), col = 'purple')
# mean(alt_vs_org_match) ; sd(alt_vs_org_match)
# # 0.001288262 ± 0.004495726
# lines(density(alt_vs_org_older), col = 'blue')
# mean(alt_vs_org_older) ; sd(alt_vs_org_older)
# # -0.002965078 ± 0.003467282
# 
# ## summarise contrasts
# contrasts <- look_no_na %>%
#   filter(f_age_num != 4) %>% 
#   mutate(alt_vs_org_young_mu = apply(alt_vs_org_young, 2, mean),
#          alt_vs_org_young_sd = apply(alt_vs_org_young, 2, sd),
#          alt_vs_org_match_mu = apply(alt_vs_org_match, 2, mean),
#          alt_vs_org_match_sd = apply(alt_vs_org_match, 2, sd),
#          alt_vs_org_older_mu = apply(alt_vs_org_older, 2, mean),
#          alt_vs_org_older_sd = apply(alt_vs_org_older, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(alt_vs_org_young_mu,alt_vs_org_match_mu,alt_vs_org_older_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('alt','vs','org','look_pred','mu'),
#            sep = '_', relook = T) %>%
#   select(-alt_vs_org_young_sd, -alt_vs_org_match_sd, -alt_vs_org_older_sd, -alt, -vs, -org, -mu)
# 
# ## plot contrasts
# # age_pred %>%
# #   dplyr::select(ctd_lion, ctd_human, lion_human, pred_type) %>%
# #   pivot_longer(cols = c('ctd_lion', 'ctd_human', 'lion_human'),
# #                names_to = 'contrast') %>%
# #   ggplot()+
# #   geom_density(aes(x = value, colour = contrast))+
# #   facet_wrap(. ~ pred_type)
# 
# age_pred %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = as.factor(f_age_num)))+
#   facet_wrap(stim_type ~ pred_type, scales = 'free_y')
# 
# age_pred_all %>%
#   ggplot()+
#   geom_density(aes(x = probability, colour = predict_cat))+
#   facet_wrap(stim_type ~ f_age_num, scales = 'free_y')
# 
# contrasts_long <- contrasts_long %>%
#   mutate(pred_type = ifelse(look_pred == 'young', 'younger',
#                             ifelse(look_pred == 'match', 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   mutate(f_age_new = ifelse(f_age_num == 4, 1, f_age_num+1)) %>%
#   select(pred_type, f_age_num, f_age_new, difference, stim_type, after_stim, look_tminus1_num) %>%
#   mutate(contrast = paste0('org: ',f_age_num,', new: ', f_age_new)) %>%
#   mutate(look_tminus1 = ifelse(look_tminus1_num == 1,
#                              'neighbour younger at t-1',
#                              ifelse(look_tminus1_num == 2,
#                                     'neighbour matched at t-1',
#                                     'neighbour older at t-1')))
# #pdf('../outputs/looking_ordinal_model_1/looking_ordinal_model1_contrasts_ages.pdf')
# for(i in unique(contrasts_long$contrast)){
#   plot <- contrasts_long %>%
#     filter(contrast == i) %>%
#     ggplot()+
#     geom_density(aes(x = difference, colour = pred_type))+
#     facet_grid(look_tminus1 ~ stim_type,
#                scales = 'free')+
#     labs(title = i)
#   print(plot)
# }
# #dev.off()
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_agecontrasts.RData')
# 
# ## looking direction in previous second ####
# #load('looking_direction/looking_ordinal_model1_agecontrasts.RData')
# rm(age_look_org, age_mtx_org, age_look_alt, age_mtx_alt, age_pred, alt_vs_org_young, alt_vs_org_match, alt_vs_org_older, contrasts, contrasts_long) ; gc()
# 
# ## redo predictions with different previous neighbours: all younger -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 1, look AT T-1 YOUNGER
# young_look <- look_no_na %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(look_tminus1_num = 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# young_mtx <- posterior_epred(object = look_fit, newdata = young_look)
# colnames(young_mtx) <- young_look$unique_data_combo
# young_mtx <- young_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different previous neighbours: all matching
# match_look <- look_no_na %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(look_tminus1_num = 2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# match_mtx <- posterior_epred(object = look_fit, newdata = match_look)
# colnames(match_mtx) <- match_look$unique_data_combo
# match_mtx <- match_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different previous neighbours: all older -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 4, look AT T-1 OLDER
# older_look <- look_no_na %>%
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(look_tminus1_num = 3,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# older_mtx <- posterior_epred(object = look_fit, newdata = older_look)
# colnames(older_mtx) <- older_look$unique_data_combo
# older_mtx <- older_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_prevseccontrasts.RData')
# 
# ## summarise and convert to long format
# load('ele_playbacks/looking_direction/looking_ordinal_model1_prevseccontrasts.RData')
# prevsec_pred <- young_look %>%
#   dplyr::select(-look_tminus1_num) %>%
#   mutate(young_prop1_mu = apply(young_mtx[,,1], 2, mean),
#          young_prop2_mu = apply(young_mtx[,,2], 2, mean),
#          young_prop3_mu = apply(young_mtx[,,3], 2, mean),
#          young_prop1_sd = apply(young_mtx[,,1], 2, sd),
#          young_prop2_sd = apply(young_mtx[,,2], 2, sd),
#          young_prop3_sd = apply(young_mtx[,,3], 2, sd),
#          match_prop1_mu = apply(match_mtx[,,1], 2, mean),
#          match_prop2_mu = apply(match_mtx[,,2], 2, mean),
#          match_prop3_mu = apply(match_mtx[,,3], 2, mean),
#          match_prop1_sd = apply(match_mtx[,,1], 2, sd),
#          match_prop2_sd = apply(match_mtx[,,2], 2, sd),
#          match_prop3_sd = apply(match_mtx[,,3], 2, sd),
#          older_prop1_mu = apply(older_mtx[,,1], 2, mean),
#          older_prop2_mu = apply(older_mtx[,,2], 2, mean),
#          older_prop3_mu = apply(older_mtx[,,3], 2, mean),
#          older_prop1_sd = apply(older_mtx[,,1], 2, sd),
#          older_prop2_sd = apply(older_mtx[,,2], 2, sd),
#          older_prop3_sd = apply(older_mtx[,,3], 2, sd)) %>%
#   pivot_longer(cols = c(young_prop1_mu,young_prop2_mu,young_prop3_mu,
#                         match_prop1_mu,match_prop2_mu,match_prop3_mu,
#                         older_prop1_mu,older_prop2_mu,older_prop3_mu),
#                names_to = 'prevsec_propage_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(young_prop1_sd,young_prop2_sd,young_prop3_sd,
#                         match_prop1_sd,match_prop2_sd,match_prop3_sd,
#                         older_prop1_sd,older_prop2_sd,older_prop3_sd),
#                names_to = 'prevsec_propage_sd', values_to = 'stdv_propn') %>%
#   separate(col = prevsec_propage_mu, into = c('prevsec_propage_mu','mu'),
#            sep = '_m', relook = T) %>%
#   separate(col = prevsec_propage_sd, into = c('prevsec_propage_sd','sd'),
#            sep = '_s', relook = T) %>%
#   select(-mu, -sd) %>%
#   filter(prevsec_propage_mu == prevsec_propage_sd) %>%
#   separate(col = prevsec_propage_mu, into = c('prevsec_type', 'look_pred'),
#            sep = '_prop', relook = T) %>%
#   select(-prevsec_propage_sd) %>%
#   mutate(look_pred = as.numeric(look_pred)) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older')))
# 
# ## convert full predictive distribution to long format
# make_long <- function(matrix, data){
#   colnames(matrix) <- rownames(data)
#   long <- matrix %>%
#     as.data.frame() %>%
#     pivot_longer(cols = everything(),
#                  names_to = 'rownum', values_to = 'probability') %>%
#     left_join(data, by = 'rownum')
#   return(long)
# }
# 
# young_look$rownum <- rownames(young_look)
# match_look$rownum <- rownames(match_look)
# older_look$rownum <- rownames(older_look)
# for(i in 1:3){
#   for(j in 1:3){
#     if(i == 1){
#       matrix <- young_mtx[,,j]
#       data <- young_look
#     }
#     if(i == 2){
#       matrix <- match_mtx[,,j]
#       data <- match_look
#     }
#     if(i == 3){
#       matrix <- older_mtx[,,j]
#       data <- older_look
#     }
#     if( i == 1 & j == 1 ){
#       prevsec_pred_all <- make_long(matrix, data) %>%
#         mutate(predict_num = 1,
#                predict_cat = 'younger')
#     } else {
#       prevsec_pred_new <- make_long(matrix, data) %>%
#         mutate(predict_num = i,
#                predict_cat = ifelse(i == 1, 'younger',
#                                     ifelse(i == 2, 'matched', 'older')))
#       prevsec_pred_all <- rbind(prevsec_pred_all, prevsec_pred_new)
#     }
#   }
# }
# 
# ## calculate contrasts
# young_vs_match_age1 <- match_mtx[,,1] - young_mtx[,,1]
# young_vs_match_age2 <- match_mtx[,,2] - young_mtx[,,2]
# young_vs_match_age3 <- match_mtx[,,3] - young_mtx[,,3]
# young_vs_older_age1 <- older_mtx[,,1] - young_mtx[,,1]
# young_vs_older_age2 <- older_mtx[,,2] - young_mtx[,,2]
# young_vs_older_age3 <- older_mtx[,,3] - young_mtx[,,3]
# match_vs_older_age1 <- older_mtx[,,1] - match_mtx[,,1]
# match_vs_older_age2 <- older_mtx[,,2] - match_mtx[,,2]
# match_vs_older_age3 <- older_mtx[,,3] - match_mtx[,,3]
# 
# ## plot contrasts
# plot(density(young_vs_match_age1), col = 'red', las = 1, xlim = c(-1, 1), ylim = c(0,40),
#      main = 'contrasts between t-1 neighbours:\nred = Pr(younger), purple = Pr(match), blue = Pr(older);\nsolid = young vs match, dashed = young vs older, dotted = match vs older')
# mean(young_vs_match_age1) ; sd(young_vs_match_age1)
# # -0.989369 ± 0.004120054
# lines(density(young_vs_match_age2), col = 'purple')
# mean(young_vs_match_age2) ; sd(young_vs_match_age2)
# # 0.9837748 ± 0.007480113
# lines(density(young_vs_match_age3), col = 'blue')
# mean(young_vs_match_age3) ; sd(young_vs_match_age3)
# # 0.005594218 ± 0.004150668
# lines(density(young_vs_older_age1), col = 'red', lty = 2)
# mean(young_vs_older_age1) ; sd(young_vs_older_age1)
# # -0.9940382 ± 0.004977479
# lines(density(young_vs_older_age2), col = 'purple', lty = 2)
# mean(young_vs_older_age2) ; sd(young_vs_older_age2)
# # 0.002520919 ± 0.009991569
# lines(density(young_vs_older_age3), col = 'blue', lty = 2)
# mean(young_vs_older_age3) ; sd(young_vs_older_age3)
# # 0.9915172 ± 0.006479285
# lines(density(match_vs_older_age1), col = 'red', lty = 3)
# mean(match_vs_older_age1) ; sd(match_vs_older_age1)
# # -0.004669165 ± 0.003245499
# lines(density(match_vs_older_age2), col = 'purple', lty = 3)
# mean(match_vs_older_age2) ; sd(match_vs_older_age2)
# # -0.9812539 ± 0.007781853
# lines(density(match_vs_older_age3), col = 'blue', lty = 3)
# mean(match_vs_older_age3) ; sd(match_vs_older_age3)
# # 0.985923 ± 0.005284724
# 
# ## summarise contrasts
# contrasts <- look_no_na %>%
#   select(-look_tminus1_num) %>%
#   mutate(young_vs_match_age1_mu = apply(young_vs_match_age1, 2, mean),
#          young_vs_match_age1_sd = apply(young_vs_match_age1, 2, sd),
#          young_vs_match_age2_mu = apply(young_vs_match_age2, 2, mean),
#          young_vs_match_age2_sd = apply(young_vs_match_age2, 2, sd),
#          young_vs_match_age3_mu = apply(young_vs_match_age3, 2, mean),
#          young_vs_match_age3_sd = apply(young_vs_match_age3, 2, sd),
#          young_vs_older_age1_mu = apply(young_vs_older_age1, 2, mean),
#          young_vs_older_age1_sd = apply(young_vs_older_age1, 2, sd),
#          young_vs_older_age2_mu = apply(young_vs_older_age2, 2, mean),
#          young_vs_older_age2_sd = apply(young_vs_older_age2, 2, sd),
#          young_vs_older_age3_mu = apply(young_vs_older_age3, 2, mean),
#          young_vs_older_age3_sd = apply(young_vs_older_age3, 2, sd),
#          match_vs_older_age1_mu = apply(match_vs_older_age1, 2, mean),
#          match_vs_older_age1_sd = apply(match_vs_older_age1, 2, sd),
#          match_vs_older_age2_mu = apply(match_vs_older_age2, 2, mean),
#          match_vs_older_age2_sd = apply(match_vs_older_age2, 2, sd),
#          match_vs_older_age3_mu = apply(match_vs_older_age3, 2, mean),
#          match_vs_older_age3_sd = apply(match_vs_older_age3, 2, sd))
# contrasts_long <- contrasts %>%
#   pivot_longer(cols = c(young_vs_match_age1_mu,young_vs_match_age2_mu,young_vs_match_age3_mu,
#                         young_vs_older_age1_mu,young_vs_older_age2_mu,young_vs_older_age3_mu,
#                         match_vs_older_age1_mu,match_vs_older_age2_mu,match_vs_older_age3_mu),
#                names_to = 'contrast', values_to = 'difference') %>%
#   separate(contrast, into = c('contrast','look_pred'),
#            sep = '_age', relook = T) %>%
#   separate(look_pred, into = c('look_pred','mu'),
#            sep = '_', relook = T) %>%
#   mutate(look_pred = as.numeric(look_pred)) %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older'))) %>%
#   select(-mu, -young_vs_match_age1_sd, -young_vs_match_age2_sd, -young_vs_match_age3_sd,
#          -young_vs_older_age1_sd, -young_vs_older_age2_sd, -young_vs_older_age3_sd,
#          -match_vs_older_age1_sd, -match_vs_older_age2_sd, -match_vs_older_age3_sd)
# 
# ## plot contrasts
# prevsec_pred %>%
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = as.factor(prevsec_type)))+
#   facet_wrap(stim_type ~ pred_type, scales = 'free_y')
# 
# prevsec_pred_all %>%
#   ggplot()+
#   geom_density(aes(x = probability, colour = predict_cat))+
#   facet_wrap(stim_type ~ f_age_num, scales = 'free_y')
# 
# contrasts_long <- contrasts_long %>%
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   separate(contrast, into = c('prevsec_a','prevsec_b'), sep = '_vs_', relook = F) %>%
#   select(pred_type, f_age_num, prevsec_a, prevsec_b, contrast, difference, stim_type, after_stim)
# 
# #pdf('../outputs/looking_ordinal_model_1/looking_ordinal_model1_contrasts_prevsec.pdf')
# for(i in unique(contrasts_long$contrast)){
#   plot <- contrasts_long %>%
#     filter(contrast == i) %>%
#     ggplot()+
#     geom_density(aes(x = difference, colour = pred_type))+
#     facet_grid(as.factor(f_age_num) ~ stim_type,
#                scales = 'free')+
#     labs(title = i)
#   print(plot)
# }
# #dev.off()
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_prevseccontrasts.RData')
# 
# # predictions %>%
# #   mutate(previous = ifelse(look_tminus1_num == 1, 'younger',
# #                            ifelse(look_tminus1_num == 2, 'same age', 'older')),
# #          prediction = ifelse(prediction == 1, 'younger',
# #                              ifelse(prediction == 2, 'same age', 'older'))) %>%
# #   mutate(previous = factor(previous, levels = c('younger','same age','older')),
# #          prediction = factor(prediction, levels = c('younger','same age','older'))) %>%
# #   ggplot()+
# #   geom_bar(aes(x = prediction, fill = as.factor(previous)),
# #            position = 'dodge')+
# #   scale_y_continuous(expand = c(0,0))+
# #   labs(colour = 'previous second')+
# #   scale_fill_viridis_d()
# #
# # prevsec2 <- pred_prop %>% filter(look_tminus1_num == 2)
# # prevsec3 <- pred_prop %>% filter(look_tminus1_num == 3)
# # pred_prev <- pred_prop %>%
# #   filter(look_tminus1_num == 1) %>%
# #   rename(count_1 = count_predictions,
# #          prop_1 = proportion) %>%
# #   select(f_age_num, after_stim, stim_type, prediction, count_1, prop_1) %>%
# #   left_join(prevsec2[,c('f_age_num','after_stim','stim_type','prediction','count_predictions','proportion')],
# #             by = c('f_age_num','after_stim','stim_type','prediction')) %>%
# #   rename(count_2 = count_predictions,
# #          prop_2 = proportion) %>%
# #   left_join(prevsec3[,c('f_age_num','after_stim','stim_type','prediction','count_predictions','proportion')],
# #             by = c('f_age_num','after_stim','stim_type','prediction')) %>%
# #   rename(count_3 = count_predictions,
# #          prop_3 = proportion) %>%
# #   mutate(look1_2 = prop_1 - prop_2,
# #          look1_3 = prop_1 - prop_3,
# #          look2_3 = prop_2 - prop_3)
# # pred_prev %>%
# #   select(stim_type, after_stim, f_age_num, prediction,
# #          look1_2,look1_3,look2_3) %>%
# #   pivot_longer(cols = c('look1_2', 'look1_3', 'look2_3'),
# #                names_to = 'contrast', values_to = 'value') %>%
# #   mutate(contrast = ifelse(contrast == 'look1_2', 'younger vs same',
# #                            ifelse(contrast == 'look1_3', 'younger vs older',
# #                                   'same vs older'))) %>%
# #   ggplot()+
# #   geom_density(aes(x = value, colour = contrast), linewidth = 1)+
# #   scale_colour_viridis_d()+
# #   labs(colour = 't-1 pair',
# #        x = 'difference between neighbours at previous second')+
# #   geom_vline(xintercept = 0, linetype = 2)+
# #   scale_x_continuous(limits = c(-2,2))
# 
# ## time since stimulus ####
# # load('looking_direction/looking_ordinal_model1_prevseccontrasts.RData')
# rm(young_look, young_mtx, match_look, match_mtx, older_look, older_mtx,
#    contrasts, contrasts_long, prevsec_pred,
#    young_vs_match_age1, young_vs_match_age2, young_vs_match_age3,
#    young_vs_older_age1, young_vs_older_age2, young_vs_older_age3,
#    match_vs_older_age1, match_vs_older_age2, match_vs_older_age3) ; gc()
# 
# ## predict with original times
# time_look_org <- look_no_na %>% 
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# time_mtx_org <- posterior_epred(object = look_fit, newdata = time_look_org)
# colnames(time_mtx_org) <- time_look_org$unique_data_combo
# time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +15 seconds
# time_look_alt_0.25 <- look_no_na %>% 
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(after_stim_org = after_stim) %>% 
#   mutate(after_stim = after_stim + 1/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
#   relocate(after_stim_org)
# time_mtx_alt_0.25 <- posterior_epred(object = look_fit, newdata = time_look_alt_0.25)
# colnames(time_mtx_alt_0.25) <- time_look_alt_0.25$unique_data_combo
# time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +30 seconds
# time_look_alt_0.50 <- look_no_na %>% 
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(after_stim_org = after_stim) %>% 
#   mutate(after_stim = after_stim + 1/2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
#   relocate(after_stim_org)
# time_mtx_alt_0.50 <- posterior_epred(object = look_fit, newdata = time_look_alt_0.50)
# colnames(time_mtx_alt_0.50) <- time_look_alt_0.50$unique_data_combo
# time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +45 seconds
# time_look_alt_0.75 <- look_no_na %>% 
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(after_stim_org = after_stim) %>% 
#   mutate(after_stim = after_stim + 3/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
#   relocate(after_stim_org)
# time_mtx_alt_0.75 <- posterior_epred(object = look_fit, newdata = time_look_alt_0.75)
# colnames(time_mtx_alt_0.75) <- time_look_alt_0.75$unique_data_combo
# time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +60 seconds
# time_look_alt_1.00 <- look_no_na %>% 
#   dplyr::select(f_age_num, stim_type, look_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(after_stim_org = after_stim) %>% 
#   mutate(after_stim = after_stim + 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, look_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
#   relocate(after_stim_org)
# time_mtx_alt_1.00 <- posterior_epred(object = look_fit, newdata = time_look_alt_1.00)
# colnames(time_mtx_alt_1.00) <- time_look_alt_1.00$unique_data_combo
# time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_timecontrasts.RData')
# 
# ## summarise and convert to long format
# time_pred <- time_look_org %>% 
#   mutate(time_org_0.00_prop1_mu = apply(time_mtx_org[,,1], 2, mean),
#          time_org_0.00_prop2_mu = apply(time_mtx_org[,,2], 2, mean),
#          time_org_0.00_prop3_mu = apply(time_mtx_org[,,3], 2, mean),
#          time_org_0.00_prop1_sd = apply(time_mtx_org[,,1], 2, sd),
#          time_org_0.00_prop2_sd = apply(time_mtx_org[,,2], 2, sd),
#          time_org_0.00_prop3_sd = apply(time_mtx_org[,,3], 2, sd),
#          time_alt_0.25_prop1_mu = apply(time_mtx_alt_0.25[,,1], 2, mean),
#          time_alt_0.25_prop2_mu = apply(time_mtx_alt_0.25[,,2], 2, mean),
#          time_alt_0.25_prop3_mu = apply(time_mtx_alt_0.25[,,3], 2, mean),
#          time_alt_0.25_prop1_sd = apply(time_mtx_alt_0.25[,,1], 2, sd),
#          time_alt_0.25_prop2_sd = apply(time_mtx_alt_0.25[,,2], 2, sd),
#          time_alt_0.25_prop3_sd = apply(time_mtx_alt_0.25[,,3], 2, sd),
#          time_alt_0.50_prop1_mu = apply(time_mtx_alt_0.50[,,1], 2, mean),
#          time_alt_0.50_prop2_mu = apply(time_mtx_alt_0.50[,,2], 2, mean),
#          time_alt_0.50_prop3_mu = apply(time_mtx_alt_0.50[,,3], 2, mean),
#          time_alt_0.50_prop1_sd = apply(time_mtx_alt_0.50[,,1], 2, sd),
#          time_alt_0.50_prop2_sd = apply(time_mtx_alt_0.50[,,2], 2, sd),
#          time_alt_0.50_prop3_sd = apply(time_mtx_alt_0.50[,,3], 2, sd),
#          time_alt_0.75_prop1_mu = apply(time_mtx_alt_0.75[,,1], 2, mean),
#          time_alt_0.75_prop2_mu = apply(time_mtx_alt_0.75[,,2], 2, mean),
#          time_alt_0.75_prop3_mu = apply(time_mtx_alt_0.75[,,3], 2, mean),
#          time_alt_0.75_prop1_sd = apply(time_mtx_alt_0.75[,,1], 2, sd),
#          time_alt_0.75_prop2_sd = apply(time_mtx_alt_0.75[,,2], 2, sd),
#          time_alt_0.75_prop3_sd = apply(time_mtx_alt_0.75[,,3], 2, sd),
#          time_alt_1.00_prop1_mu = apply(time_mtx_alt_1.00[,,1], 2, mean),
#          time_alt_1.00_prop2_mu = apply(time_mtx_alt_1.00[,,2], 2, mean),
#          time_alt_1.00_prop3_mu = apply(time_mtx_alt_1.00[,,3], 2, mean),
#          time_alt_1.00_prop1_sd = apply(time_mtx_alt_1.00[,,1], 2, sd),
#          time_alt_1.00_prop2_sd = apply(time_mtx_alt_1.00[,,2], 2, sd),
#          time_alt_1.00_prop3_sd = apply(time_mtx_alt_1.00[,,3], 2, sd)) %>% 
#   pivot_longer(cols = c(time_org_0.00_prop1_mu,time_org_0.00_prop2_mu,time_org_0.00_prop3_mu,
#                         time_alt_0.25_prop1_mu,time_alt_0.25_prop2_mu,time_alt_0.25_prop3_mu,
#                         time_alt_0.50_prop1_mu,time_alt_0.50_prop2_mu,time_alt_0.50_prop3_mu,
#                         time_alt_0.75_prop1_mu,time_alt_0.75_prop2_mu,time_alt_0.75_prop3_mu,
#                         time_alt_1.00_prop1_mu,time_alt_1.00_prop2_mu,time_alt_1.00_prop3_mu),
#                names_to = 'time_org_alt_prop_agelook_mu', values_to = 'mean_propn') %>% 
#   pivot_longer(cols = c(time_org_0.00_prop1_sd,time_org_0.00_prop2_sd,time_org_0.00_prop3_sd,
#                         time_alt_0.25_prop1_sd,time_alt_0.25_prop2_sd,time_alt_0.25_prop3_sd,
#                         time_alt_0.50_prop1_sd,time_alt_0.50_prop2_sd,time_alt_0.50_prop3_sd,
#                         time_alt_0.75_prop1_sd,time_alt_0.75_prop2_sd,time_alt_0.75_prop3_sd,
#                         time_alt_1.00_prop1_sd,time_alt_1.00_prop2_sd,time_alt_1.00_prop3_sd),
#                names_to = 'time_org_alt_prop_agelook_sd', values_to = 'stdv_propn') %>% 
#   separate(col = time_org_alt_prop_agelook_mu,
#            into = c('time_mu','org_mu','alt_mu','prop_agelook_mu','mu'),
#            sep = '_', relook = T) %>% 
#   separate(col = time_org_alt_prop_agelook_sd,
#            into = c('time_sd','org_sd','alt_sd','prop_agelook_sd','sd'),
#            sep = '_', relook = T) %>% 
#   select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>% 
#   filter(alt_mu == alt_sd & prop_agelook_sd == prop_agelook_mu) %>% 
#   mutate(look_pred = ifelse(prop_agelook_mu == 'prop1', 1,
#                           ifelse(prop_agelook_mu == 'prop2', 2,
#                                  ifelse(prop_agelook_mu == 'prop3', 3, 4)))) %>% 
#   select(-alt_sd, -prop_agelook_mu, -prop_agelook_sd) %>% 
#   rename(mins_added = alt_mu) %>% 
#   mutate(pred_type = ifelse(look_pred == 1, 'younger',
#                             ifelse(look_pred == 2, 'matched', 'older')))
# 
# ## convert full predictive distribution to long format
# time_pred_all <- time_mtx_org[,,1] %>% 
#   as.data.frame()
# colnames(time_pred_all) <- rownames(time_look_org)
# time_pred_all <- pivot_longer(time_pred_all, cols = everything(),
#                               names_to = 'rownum', values_to = 'probability')
# time_look_org$rownum <- rownames(time_look_org)
# time_pred_all <- time_pred_all %>% 
#   left_join(time_look_org, by = 'rownum') %>% 
#   mutate(predict_num = 1,
#          predict_cat = 'younger')
# for(i in 2:3){
#   time_pred_new <- time_mtx_org[,,i] %>% 
#     as.data.frame()
#   colnames(time_pred_new) <- rownames(time_look_org)
#   time_pred_new <- pivot_longer(time_pred_new, cols = everything(),
#                                 names_to = 'rownum', values_to = 'probability')
#   time_pred_new <- time_pred_new %>% 
#     left_join(time_look_org, by = 'rownum') %>% 
#     mutate(predict_num = i,
#            predict_cat = ifelse(i == 2, 'matched', 'older'))
#   time_pred_all <- rbind(time_pred_all, time_pred_new)
# }
# 
# ## calculate contrasts
# alt0.25_vs_0.00_young <- time_mtx_alt_0.25[,,1] - time_mtx_org[,,1]
# alt0.25_vs_0.00_match <- time_mtx_alt_0.25[,,2] - time_mtx_org[,,2]
# alt0.25_vs_0.00_older <- time_mtx_alt_0.25[,,3] - time_mtx_org[,,3]
# 
# alt0.50_vs_0.25_young <- time_mtx_alt_0.50[,,1] - time_mtx_alt_0.25[,,1]
# alt0.50_vs_0.25_match <- time_mtx_alt_0.50[,,2] - time_mtx_alt_0.25[,,2]
# alt0.50_vs_0.25_older <- time_mtx_alt_0.50[,,3] - time_mtx_alt_0.25[,,3]
# 
# alt0.75_vs_0.50_young <- time_mtx_alt_0.75[,,1] - time_mtx_alt_0.50[,,1]
# alt0.75_vs_0.50_match <- time_mtx_alt_0.75[,,2] - time_mtx_alt_0.50[,,2]
# alt0.75_vs_0.50_older <- time_mtx_alt_0.75[,,3] - time_mtx_alt_0.50[,,3]
# 
# alt1.00_vs_0.75_young <- time_mtx_alt_1.00[,,1] - time_mtx_alt_0.75[,,1]
# alt1.00_vs_0.75_match <- time_mtx_alt_1.00[,,2] - time_mtx_alt_0.75[,,2]
# alt1.00_vs_0.75_older <- time_mtx_alt_1.00[,,3] - time_mtx_alt_0.75[,,3]
# 
# ## plot contrasts -- no effect of time whatsoever
# plot(density(alt0.25_vs_0.00_young), col = 'red', las = 1, xlim = c(-0.001, 0.001),
#      main = 'contrasts between stim types:\nsolid = younger, dashed = match, dotted = older;\nred = 0-15s, blue = 15-30s, green = 30-45s, black = 45-60s')
# mean(alt0.25_vs_0.00_young) ; sd(alt0.25_vs_0.00_young)
# # 1.9754e-05 ± 0.0001278051
# lines(density(alt0.50_vs_0.25_young), col = 'blue')
# mean(alt0.50_vs_0.25_young) ; sd(alt0.50_vs_0.25_young)
# # 1.816309e-05 ± 0.000121
# lines(density(alt0.75_vs_0.50_young), col = 'green')
# mean(alt0.75_vs_0.50_young) ; sd(alt0.75_vs_0.50_young)
# # 1.675586e-05 ± 0.0001197705
# lines(density(alt1.00_vs_0.75_young), col = 'black')
# mean(alt1.00_vs_0.75_young) ; sd(alt1.00_vs_0.75_young)
# # 1.569622e-05 ± 0.0001257571
# lines(density(alt0.25_vs_0.00_match), col = 'red', lty = 2)
# mean(alt0.25_vs_0.00_match) ; sd(alt0.25_vs_0.00_match)
# # 1.777214e-06 ± 0.0001531117
# lines(density(alt0.50_vs_0.25_match), col = 'blue', lty = 2)
# mean(alt0.50_vs_0.25_match) ; sd(alt0.50_vs_0.25_match)
# # 6.871919e-07 ± 0.0001458013
# lines(density(alt0.75_vs_0.50_match), col = 'green', lty = 2)
# mean(alt0.75_vs_0.50_match) ; sd(alt0.75_vs_0.50_match)
# # -1.926899e-08 ± 0.0001459526
# lines(density(alt1.00_vs_0.75_match), col = 'black')
# mean(alt1.00_vs_0.75_match) ; sd(alt1.00_vs_0.75_match)
# # -6.710725e-07 ± 0.0001554494
# lines(density(alt0.25_vs_0.00_older), col = 'red', lty = 3)
# mean(alt0.25_vs_0.00_older) ; sd(alt0.25_vs_0.00_older)
# # -2.153121e-05 ± 0.0001493238
# lines(density(alt0.50_vs_0.25_older), col = 'blue', lty = 3)
# mean(alt0.50_vs_0.25_older) ; sd(alt0.50_vs_0.25_older)
# # -1.885028e-05 ± 0.0001416635
# lines(density(alt0.75_vs_0.50_older), col = 'green', lty = 3)
# mean(alt0.75_vs_0.50_older) ; sd(alt0.75_vs_0.50_older)
# # -1.673659e-05 ± 0.0001412924
# lines(density(alt1.00_vs_0.75_older), col = 'black')
# mean(alt1.00_vs_0.75_older) ; sd(alt1.00_vs_0.75_older)
# # -1.502515e-05 ± 0.0001497973
# 
# ## summarise contrasts
# contrasts <- look_no_na %>% 
#   mutate(alt0.25_vs_0.00_young_mu = apply(alt0.25_vs_0.00_young, 2, mean),
#          alt0.25_vs_0.00_young_sd = apply(alt0.25_vs_0.00_young, 2, sd),
#          alt0.25_vs_0.00_match_mu = apply(alt0.25_vs_0.00_match, 2, mean),
#          alt0.25_vs_0.00_match_sd = apply(alt0.25_vs_0.00_match, 2, sd),
#          alt0.25_vs_0.00_older_mu = apply(alt0.25_vs_0.00_older, 2, mean),
#          alt0.25_vs_0.00_older_sd = apply(alt0.25_vs_0.00_older, 2, sd),
#          alt0.50_vs_0.25_young_mu = apply(alt0.50_vs_0.25_young, 2, mean),
#          alt0.50_vs_0.25_young_sd = apply(alt0.50_vs_0.25_young, 2, sd),
#          alt0.50_vs_0.25_match_mu = apply(alt0.50_vs_0.25_match, 2, mean),
#          alt0.50_vs_0.25_match_sd = apply(alt0.50_vs_0.25_match, 2, sd),
#          alt0.50_vs_0.25_older_mu = apply(alt0.50_vs_0.25_older, 2, mean),
#          alt0.50_vs_0.25_older_sd = apply(alt0.50_vs_0.25_older, 2, sd),
#          alt0.75_vs_0.50_young_mu = apply(alt0.75_vs_0.50_young, 2, mean),
#          alt0.75_vs_0.50_young_sd = apply(alt0.75_vs_0.50_young, 2, sd),
#          alt0.75_vs_0.50_match_mu = apply(alt0.75_vs_0.50_match, 2, mean),
#          alt0.75_vs_0.50_match_sd = apply(alt0.75_vs_0.50_match, 2, sd),
#          alt0.75_vs_0.50_older_mu = apply(alt0.75_vs_0.50_older, 2, mean),
#          alt0.75_vs_0.50_older_sd = apply(alt0.75_vs_0.50_older, 2, sd),
#          alt1.00_vs_0.75_young_mu = apply(alt1.00_vs_0.75_young, 2, mean),
#          alt1.00_vs_0.75_young_sd = apply(alt1.00_vs_0.75_young, 2, sd),
#          alt1.00_vs_0.75_match_mu = apply(alt1.00_vs_0.75_match, 2, mean),
#          alt1.00_vs_0.75_match_sd = apply(alt1.00_vs_0.75_match, 2, sd),
#          alt1.00_vs_0.75_older_mu = apply(alt1.00_vs_0.75_older, 2, mean),
#          alt1.00_vs_0.75_older_sd = apply(alt1.00_vs_0.75_older, 2, sd))
# contrasts_long <- contrasts %>% 
#   select(-alt0.25_vs_0.00_young_sd,-alt0.25_vs_0.00_match_sd,-alt0.25_vs_0.00_older_sd,
#          -alt0.50_vs_0.25_young_sd,-alt0.50_vs_0.25_match_sd,-alt0.50_vs_0.25_older_sd,
#          -alt0.75_vs_0.50_young_sd,-alt0.75_vs_0.50_match_sd,-alt0.75_vs_0.50_older_sd,
#          -alt1.00_vs_0.75_young_sd,-alt1.00_vs_0.75_match_sd,-alt1.00_vs_0.75_older_sd) %>% 
#   pivot_longer(cols = c(alt0.25_vs_0.00_young_mu,alt0.25_vs_0.00_match_mu,alt0.25_vs_0.00_older_mu,
#                         alt0.50_vs_0.25_young_mu,alt0.50_vs_0.25_match_mu,alt0.50_vs_0.25_older_mu,
#                         alt0.75_vs_0.50_young_mu,alt0.75_vs_0.50_match_mu,alt0.75_vs_0.50_older_mu,
#                         alt1.00_vs_0.75_young_mu,alt1.00_vs_0.75_match_mu,alt1.00_vs_0.75_older_mu),
#                names_to = 'contrast', values_to = 'difference') %>% 
#   separate(contrast, into = c('alt','contrast'), sep = 3) %>% 
#   separate(contrast, into = c('later','vs','earlier','pred_type','mu'),
#            sep = '_', relook = T) %>% 
#   select(-alt, -vs, -mu) %>% 
#   mutate(later = as.numeric(later),
#          earlier = as.numeric(earlier),
#          contrast = paste0(later,'_',earlier))
# 
# ## plot contrasts
# times <- unique(time_pred$after_stim)
# time_pred %>% 
#   filter(after_stim %in% times[seq(1, length(times), length.out = 8)]) %>% 
#   mutate(after_stim = round(after_stim, 2)) %>% 
#   ggplot()+
#   geom_density(aes(x = mean_propn, colour = pred_type))+
#   facet_wrap(as.factor(after_stim) ~ stim_type,
#              scales = 'free')
# 
# time_pred_all %>% 
#   ggplot()+
#   geom_density(aes(x = probability, colour = predict_cat))+
#   facet_wrap(stim_type ~ f_age_num, scales = 'free_y')
# 
# contrasts_long <- contrasts_long %>% 
#   mutate(pred_type = ifelse(pred_type == 'young', 'younger',
#                             ifelse(pred_type == 'match', 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   select(pred_type, f_age_num, contrast, earlier, later, difference, stim_type, after_stim, look_tminus1_num) %>%
#   mutate(look_tminus1 = ifelse(look_tminus1_num == 1,
#                              'neighbour younger at t-1',
#                              ifelse(look_tminus1_num == 2,
#                                     'neighbour matched at t-1',
#                                     'neighbour older at t-1')))
# 
# for(i in unique(contrasts_long$contrast)){
#   plot <- contrasts_long %>% 
#     filter(contrast == i) %>% 
#     filter(after_stim %in% times[seq(1, length(times), length.out = 8)]) %>% 
#     mutate(after_stim = round(after_stim, 2)) %>% 
#     ggplot()+
#     geom_density(aes(x = difference, colour = pred_type))+
#     facet_grid(look_tminus1 ~ after_stim,
#                scales = 'free')+
#     labs(title = i)
#   print(plot)
# }
# 
# save.image('ele_playbacks/looking_direction/looking_ordinal_model1_timecontrasts.RData')
# dev.off()
