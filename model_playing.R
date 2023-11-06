#### information ####
# script for basic analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
library(tidyverse)
#library(cmdstanr) ; set_cmdstan_path('../../packages/.cmdstan/cmdstan-2.31.0/') # library(cmdstanr, lib.loc = '../../packages/')
library(brms)

theme_set(theme_classic())

#### descriptive stats ####
# read in data
prop <- readRDS('../data_processed/proportions_of_time_per_behaviour.RDS') %>% 
  mutate(stim_type_full = ifelse(stim_type == 'l', 'lion',
                                 ifelse(stim_type == 'ctd', 'control', 'human')))

# per behaviour, mean and stdev
behav <- prop %>% 
  select(section, stim_type, behaviour, behavioural_category, type, age_difference) %>% 
  distinct() %>% 
  mutate(seconds_mean = NA, seconds_stdv = NA,
         prop_mean = NA, prop_stdv = NA) %>% 
  filter(!(type == 'elephant' & is.na(age_difference) == TRUE)) %>% 
  filter(!(type == 'neighbour' & is.na(age_difference) == TRUE))

for(i in 1:nrow(behav)){
  x <- prop %>%
    filter(behaviour == behav$behaviour[i] &
             stim_type == behav$stim_type[i] &
             section == behav$section[i])
  if(behav$type[i] == 'elephant' | behav$type[i] == 'neighbour'){
    x <- x %>% filter(age_difference == behav$age_difference[i])
  }
  behav$seconds_mean[i] <- mean(x$behav_seconds, na.rm = T)
  behav$seconds_stdv[i] <- sd(x$behav_seconds, na.rm = T)
  behav$prop_mean[i] <- mean(x$propn, na.rm = T)
  behav$prop_stdv[i] <- sd(x$propn, na.rm = T)
}
rm(i, x) ; gc()

write_csv(behav, '../data_processed/proportions_descriptivestats.csv')

rm(behav, prop) ; gc()

#### Bayesian multinomial regression to predict probability of looking towards/side-on/away based on effect of stimulus, time since stimulus and age on action -- NO: MULTINOMIAL IS ONLY FOR IF THE OUTCOME CATEGORIES ARE UNORDERED, BUT THIS IS ORDERED BY "DEGREE OF TOWARDNESS" ####
# read in data
ages <- readRDS('../data_processed/elephant_behaviour_proportions.RDS') %>% 
  select(pb_num, subject, targeted_elephant,    # random effects
         #stim_num,                             # unnecessary random effects
         stim_type,age_category,partner_age_category,age_difference, # exposures
         age,partner_age,focal, dyad_partner,   # helpful for reference, don't include
         #section, # need to know when stimulus was, but don't want as categorical
         group_size # control for this
  ) %>% 
  mutate(group_size = ifelse(pb_num == 44, 7, group_size)) %>% 
  distinct()

stim_starts <- readRDS('../data_processed/stimuli.RDS') %>% 
  filter(status == 'START' & behavior == 'STIMULUS') %>% 
  select(pb_num,time,stim_num,comment)
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
  select(pb_num, stim_start,stim_num)

look <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>% 
  select(subject,bull,pb_num,second,out_frame_name,out_frame_index,
         b1_look_name,#b1_look_index,
         b2_look_name,#b2_look_index,
         b3_look_name,#b3_look_index,
         b4_look_name,#b4_look_index,
         b5_look_name,#b5_look_index,
         b6_look_name,#b6_look_index,
         b7_look_name,#b7_look_index,
         b8_look_name#,b8_look_index,
         #b1_present_index,b2_present_index,b3_present_index,b4_present_index,
         #b5_present_index,b6_present_index,b7_present_index,b8_present_index
         ) %>% 
  #pivot_longer(cols = c('b1_present_index','b2_present_index','b3_present_index','b4_present_index',
  #                      'b5_present_index','b6_present_index','b7_present_index','b8_present_index'),
  #             names_to = 'name_present', values_to = 'present') %>% 
  pivot_longer(cols = c('b1_look_name','b2_look_name','b3_look_name','b4_look_name',
                        'b5_look_name','b6_look_name','b7_look_name','b8_look_name'),
               names_to = 'elephant_look', values_to = 'direction_look') %>% 
  filter(is.na(direction_look) == FALSE) %>% 
  filter(direction_look != 'impossible_partner') %>% 
  filter(direction_look != 'out_of_sight') %>% 
  mutate(look_index = as.integer(factor(direction_look,
                                        levels = c('look at directly','side-on','look directly away')))) %>% 
  separate(elephant_look, into = c('dyad_partner','_look_name'), sep = 2) %>% 
  select(-`_look_name`) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(distinct(ages[,c('subject', 'age_category')]),
            by = 'subject') %>% 
  left_join(distinct(ages[,c('pb_num','partner_age_category','dyad_partner')]),
            by = c('dyad_partner','pb_num')) %>% 
  left_join(distinct(ages[,c('pb_num','stim_type')]),
            by = 'pb_num') %>% 
  left_join(distinct(ages[,c('subject','dyad_partner','age_difference')]),
            by = c('subject','dyad_partner')) %>% 
  left_join(stim_starts, by = 'pb_num') %>% 
  mutate(time_since_stim = second - stim_start)

## bamlss method
look_list <- list(
  direction_look ~ s(time_since_stim) #s(age_category),
  #~ s(partner_age_category),
  #~ s(stim_type)
)

## model
set.seed(123)
b <- bamlss(look_list, family = "multinomial", data = look)
summary(b)

save.image('test_multinomial_model_run.RData')

#### ordinal logistic regression -- COPY SCRIPT OVER FROM CHAPTER 1 AGE ESTIMATION AND WORK FROM THERE ####
# https://dagitty.net/dags.html?id=dw8twK

## load model ####
looking_direction_ordinal_model <- cmdstan_model("models/ordinal_regression_look.stan")

## create data list ####
look_no_na <- look %>% 
  filter(is.na(age_category) == FALSE) %>% 
  filter(is.na(partner_age_category) == FALSE) %>% 
  mutate(stim_type = ifelse(stim_type == 'h', 3,
                            ifelse(stim_type == 'l', 2, 1)),
         focal_id = as.integer(as.factor(subject)),
         stim_num = as.integer(as.factor(stim_num)))
n_obs <- nrow(look_no_na)  # number of observations
n_direct <- 3              # number of looking directions + 1
look_ls <- list(           # generate data list
  n_obs = n_obs,
  n_direct = n_direct,
  looking_direction = look_no_na$look_index,
  focal_age = look_no_na$age_category,
  partner_age = look_no_na$partner_age_category,
  #age_difference = look_no_na$age_difference,
  stim_type = look_no_na$stim_type,
  time_since_stim = look_no_na$time_since_stim,
  focal_id = look_no_na$focal_id,
  stim_id = look_no_na$stim_num,
  playback_id = look_no_na$pb_num)

## fit model to MOTNP data ####
# Fit model with cmdstanr
direction_look_fit <- looking_direction_ordinal_model$sample(
  data = look_ls, 
  chains = 1,
  parallel_chains = 4,
  iter_sampling = 10
)
save.image('looking_direction_model_run.RData')
























# Examine the estimates
age_est_mat <- age_motnp_fit$summary()[(N_motnp+2):(N_motnp*2+1), ] # true ages of elephants
summary(age_est_mat)
hist(age_est_mat$mean)                                              # plot histogram of mean age values
hist(age_est_mat$rhat, breaks = 20)                                 # check rhat values

plot_data <- data.frame(age = ifelse(motnp_ls$age == 1, 3,          # set dummy "true" age in centre of each age category (ONLY for plotting purposes)
                                     ifelse(motnp_ls$age == 2, 8,
                                            ifelse(motnp_ls$age == 3, 12,
                                                   ifelse(motnp_ls$age == 4, 18,
                                                          ifelse(motnp_ls$age == 5, 22, 
                                                                 ifelse(motnp_ls$age == 6, 32, 45)))))),
                        model_age = age_est_mat$mean)               # Mean modelled age

plot_data %>%
  ggplot(aes(x = factor(age), y = model_age)) +        # true age vs modelled age
  geom_point(size = 4,col = 'blue', alpha = 0.6) +     # add points
  #geom_vline(xintercept = c(5,10,15,20,25,40,60), linetype = "dashed", alpha = 0.6) +     # add vertical lines at category boundaries
  geom_hline(yintercept = c(5,10,15,20,25,40,60), linetype = "dashed", alpha = 0.6) +      # add horizontal lines at category boundaries
  #geom_abline(slope = 1, intercept = 0)+              # x=y line to show where values should fall if perfectly estimated
  scale_y_continuous(limits = c(0,60))+
  theme_minimal() + 
  xlab("Assigned age") + ylab("Modelled age")

# posterior predictive plot using draws from distribution to show uncertainty around mean age
true_ages <- age_motnp_fit$draws("true_age", format="df")    # data frame of true ages produced by model
#mcmc_dens(true_ages)
true_ages <- true_ages[,1:N_motnp]                           # select only columns relevant to individuals

df <- as.data.frame(do.call(rbind, true_ages)) %>%           # create small data frame of just age and ID
  mutate(age_cat = motnp_ls$age) %>% relocate(age_cat) %>%
  mutate(ID = motnp_males$id) %>% relocate(ID)

df <- df %>% pivot_longer(cols = 3:ncol(df)) %>% select(-name)     # convert to long format

df$age_cat_centre <- ifelse(df$age_cat == 1, (0+5)/2,              # for plot ONLY, set "age" as central value in each category
                            ifelse(df$age_cat == 2, (5+10)/2,
                                   ifelse(df$age_cat == 3, (10+15)/2,
                                          ifelse(df$age_cat == 4, (15+20)/2,
                                                 ifelse(df$age_cat == 5, (20+25)/2,
                                                        ifelse(df$age_cat == 6, (25+40)/2, 45))))))

df %>% ggplot(aes(x=age_cat_centre, y=value, group=factor(ID))) +  # plot intervals for each category against values set above
  geom_point(size=2,col = 'blue', alpha=0.1) +
  geom_vline(xintercept = c(5,10,15,20,25,40,60), linetype = "dashed", alpha = 0.6) +     # add vertical lines at category boundaries
  geom_hline(yintercept = c(5,10,15,20,25,40,60), linetype = "dashed", alpha = 0.6) +     # add horizontal lines at category boundaries
  theme_bw() + 
  xlab("Assigned age") + ylab("Modelled age")

df %>% ggplot() +                                                  # plot intervals for each category against values set above
  geom_violin(aes(x = age_cat_centre, y = value, group = factor(age_cat)), fill = rgb(0,0,1,0.8))+
  #geom_point(aes(x = true_age, y = value, group = factor(ID)), size = 2, col = 'red', alpha = 0.1) +
  geom_vline(xintercept = 0, alpha = 0.6) +                                               # add vertical lines at 0
  geom_vline(xintercept = c(5,10,15,20,25,40,60), linetype = "dashed", alpha = 0.6) +     # add vertical lines at category boundaries
  geom_hline(yintercept = 0, alpha = 0.6) +                                               # add horizontal lines at 0
  geom_hline(yintercept = c(5,10,15,20,25,40,60), linetype = "dashed", alpha = 0.6) +     # add horizontal lines at category boundaries
  theme_bw() + 
  xlab("Assigned age") + ylab("Modelled age")+
  theme(axis.text = element_text(size = 14))

df %>% ggplot() +                                                 # plot intervals for each category against values set above
  geom_violin(aes(x = age_cat_centre, y = value, group = factor(age_cat), fill = factor(age_cat, levels = c(7:3,NA,NA))))+
  geom_vline(xintercept = 0, alpha = 0.6) +                                               # add vertical lines at 0
  geom_vline(xintercept = c(5,10,15,20,25,40,60), linetype = "dashed", alpha = 0.6) +     # add vertical lines at category boundaries
  geom_hline(yintercept = 0, alpha = 0.6) +                                               # add horizontal lines at 0
  geom_hline(yintercept = c(5,10,15,20,25,40,60), linetype = "dashed", alpha = 0.6) +     # add horizontal lines at category boundaries
  theme_bw() + 
  xlab("Assigned age") + ylab("Modelled age")+
  theme(axis.text = element_text(size = 14), legend.position = 'none', axis.title = element_text(size = 18))+
  scale_fill_viridis_d()

### save output
colnames(true_ages) <- motnp_males$id
saveRDS(true_ages, file = '../data_processed/motnp_ageestimates_mcmcoutput.rds')          # save output for next steps
save.image('motnp_ageestimation.RData')

## extract probability distributions ####
age_probs <- age_motnp_fit$draws(format = 'df')
colnames(age_probs)
age_probs <- age_probs[,c('a0','a0_std','a1','a1_std',
                          'b0','b0_std','b1','b1_std',
                          'c','c_std','sigma_age')]
age_probs

## plot parameters
par(mfrow = c(3,4))
for(i in 1:ncol(age_probs)){
  hist(as.matrix(age_probs[,i]),
       main = colnames(age_probs)[i],
       xlab = 'parameter value')
}

# draw new output curves
max_age <- 60                             # maximum realistic age of males in the population
mortality <- gompertz_bt(a0 = mean(age_probs$a0),
                         a1 = mean(age_probs$a1),
                         c  = mean(age_probs$c),
                         b0 = mean(age_probs$b0),
                         b1 = mean(age_probs$b1),
                         1:max_age)
plot(mortality)                           # plot mortality curve
probs <- 1 - (mortality/max(mortality))   # survival = 1-mortality
plot(probs)                               # plot probability of survival



gompertz_bt <- function(a0, a1, c, b0, b1, age){  # create custom function for mortality distribution
  gompertz <- exp(b0 + b1*age)
  bathtub <- exp(a0 - a1*age) + c + gompertz
  return(bathtub)
}




age_probs$a0_test <- age_probs$a0_std*0.720- 5.13
age_probs$a1_test <- age_probs$a1_std*0.100 + 3.0
age_probs$c_test  <- age_probs$c_std*0.0060 + 0.026
age_probs$b0_test <- age_probs$b0_std*0.560 - 5.08
age_probs$b1_test <- age_probs$b1_std*0.018 + 0.09

check <- age_probs[which(round(age_probs$a0_test,3) != round(age_probs$a0,3)),c('a0','a0_std','a0_test')]
check <- age_probs[which(round(age_probs$a1_test,3) != round(age_probs$a1,3)),c('a1','a1_std','a1_test')]
check <- age_probs[which(round(age_probs$c_test, 3) != round(age_probs$c, 3)),c('c','c_std','c_test')]
check <- age_probs[which(round(age_probs$b0_test,3) != round(age_probs$b0,3)),c('b0','b0_std','b0_test')]
check <- age_probs[which(round(age_probs$b1_test,3) != round(age_probs$b1,3)),c('b1','b1_std','b1_test')]


#### LOOKING DIRECTION: ordinal logistic regression -- USE BRMS METHOD DESCRIBED IN https://journals.sagepub.com/doi/full/10.1177/2515245918823199 ####
# https://dagitty.net/dags.html?id=dw8twK
#library(brms, lib.loc = '../../packages/') # library(brms)

# read in data
ages <- readRDS('../data_processed/elephant_behaviour_proportions.RDS') %>% 
  select(pb_num, subject, targeted_elephant,    # random effects
         #stim_num,                             # unnecessary random effects
         stim_type,age_category,partner_age_category,age_difference, # exposures
         age,partner_age,focal, dyad_partner,   # helpful for reference, don't include
         #section, # need to know when stimulus was, but don't want as categorical
         group_size # control for this
  ) %>% 
  mutate(group_size = ifelse(pb_num == 44, 7, group_size)) %>% 
  distinct()

stim_starts <- readRDS('../data_processed/stimuli.RDS') %>% 
  filter(status == 'START' & behavior == 'STIMULUS') %>% 
  select(pb_num,time,stim_num,comment)
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
  select(pb_num, stim_start,stim_num)

look <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>% 
  select(subject,bull,pb_num,second,out_frame_name,out_frame_index,
         b1_look_name,#b1_look_index,
         b2_look_name,#b2_look_index,
         b3_look_name,#b3_look_index,
         b4_look_name,#b4_look_index,
         b5_look_name,#b5_look_index,
         b6_look_name,#b6_look_index,
         b7_look_name,#b7_look_index,
         b8_look_name#,b8_look_index,
         #b1_present_index,b2_present_index,b3_present_index,b4_present_index,
         #b5_present_index,b6_present_index,b7_present_index,b8_present_index
  ) %>% 
  #pivot_longer(cols = c('b1_present_index','b2_present_index','b3_present_index','b4_present_index',
  #                      'b5_present_index','b6_present_index','b7_present_index','b8_present_index'),
  #             names_to = 'name_present', values_to = 'present') %>% 
  pivot_longer(cols = c('b1_look_name','b2_look_name','b3_look_name','b4_look_name',
                        'b5_look_name','b6_look_name','b7_look_name','b8_look_name'),
               names_to = 'elephant_look', values_to = 'direction_look') %>% 
  filter(is.na(direction_look) == FALSE) %>% 
  filter(direction_look != 'impossible_partner') %>% 
  filter(direction_look != 'out_of_sight') %>% 
  mutate(look_index = as.integer(factor(direction_look,
                                        levels = c('look at directly','side-on','look directly away')))) %>% 
  separate(elephant_look, into = c('dyad_partner','_look_name'), sep = 2) %>% 
  select(-`_look_name`) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(distinct(ages[,c('subject', 'age_category')]),
            by = 'subject') %>% 
  left_join(distinct(ages[,c('pb_num','partner_age_category','dyad_partner')]),
            by = c('dyad_partner','pb_num')) %>% 
  left_join(distinct(ages[,c('pb_num','stim_type')]),
            by = 'pb_num') %>% 
  left_join(distinct(ages[,c('subject','dyad_partner','age_difference')]),
            by = c('subject','dyad_partner')) %>% 
  left_join(stim_starts, by = 'pb_num') %>% 
  mutate(time_since_stim = second - stim_start) %>% 
  mutate(look_tminus1 = NA)
playbacks <- unique(look$pb_num)
for(i in 1:length(playbacks)){
  pb <- look %>% filter(pb_num == playbacks[i])
  look <- look %>% anti_join(pb)
  for(j in 2:nrow(pb)){
    pb$look_tminus1[j] <- pb$direction_look[j-1]
  }
  look <- rbind(look, pb)
}
rm(check, x, i, multiple_starts) ; gc()

# filter to remove elephants with unknown ages
look_no_na <- look %>% 
  filter(is.na(age_category) == FALSE) %>% 
  filter(is.na(partner_age_category) == FALSE) %>% 
  filter(is.na(look_tminus1) == FALSE) %>% 
  mutate(#stim_type = ifelse(stim_type == 'h', 3,
         #                   ifelse(stim_type == 'l', 2, 1)),
         focal_id = as.integer(as.factor(subject)),
         stim_num = as.integer(as.factor(stim_num)),
         age_category = as.factor(age_category),
         partner_age_category = as.factor(partner_age_category)) %>% 
  rename(looking_direction = look_index,
         focal_age = age_category,
         partner_age = partner_age_category,
         stim_id = stim_num,
         playback_id = pb_num)
str(look_no_na)
# n_obs <- nrow(look_no_na)  # number of observations
# n_direct <- 3              # number of looking directions + 1
# look_ls <- list(           # generate data list
#   n_obs = n_obs,
#   n_direct = n_direct,
#   looking_direction = look_no_na$look_index,
#   focal_age = look_no_na$age_category,
#   partner_age = look_no_na$partner_age_category,
#   #age_difference = look_no_na$age_difference,
#   stim_type = look_no_na$stim_type,
#   time_since_stim = look_no_na$time_since_stim,
#   focal_id = look_no_na$focal_id,
#   stim_id = look_no_na$stim_num,
#   playback_id = look_no_na$pb_num)

# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
look_no_na <- look_no_na %>% 
  mutate(#age_diff_num = ifelse(age_difference == 'partner younger', 1,
         #                      ifelse(age_difference == 'matched', 2, 3)),
         look_tminus1_num = ifelse(look_tminus1 == 'look at directly', 1,
                                   ifelse(look_tminus1 == 'side-on', 2, 3))) %>% 
  mutate(focal_age = as.integer(focal_age), #as.factor(focal_age),
         partner_age = as.integer(partner_age),
         #age_diff_num = as.integer(age_diff_num), #as.factor(age_diff_num),
         look_tminus1_num = as.integer(look_tminus1_num)) #as.factor(look_tminus1_num))

get_prior(formula = looking_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type +  # fixed effects
            time_since_stim + mo(look_tminus1_num) + mo(partner_age) +                      # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),                                   # random effects
          data = look_no_na,
          family = cumulative("logit"))
priors <- c(prior(normal(0,0.25),   class = b,    coef = mofocal_age),
            prior(dirichlet(2,2,2), class = simo, coef = mofocal_age1),
            prior(normal(0,0.333),  class = b,    coef = moage_diff_num),
            prior(dirichlet(2,2),   class = simo, coef = moage_diff_num1),
            prior(normal(0,0.25),   class = b,    coef = mopartner_age),
            prior(dirichlet(2,2,2), class = simo, coef = mopartner_age1),
            prior(normal(0,1),      class = b,    coef = stim_typeh),
            prior(normal(0,1),      class = b,    coef = stim_typel),
            prior(normal(0,1),      class = b,    coef = time_since_stim),
            prior(normal(0,0.333),  class = b,    coef = molook_tminus1_num),
            prior(dirichlet(2,2),   class = simo, coef = molook_tminus1_num1))

## prior predictive check
num_chains <- 4
num_iter <- 2000
direction_look_prior <- brm(
  formula = looking_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type + 
    time_since_stim + mo(look_tminus1_num) + mo(partner_age) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(direction_look_prior)

## fit model
direction_look_fit <- brm(
  formula = looking_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type + 
    time_since_stim + mo(look_tminus1_num) + mo(partner_age) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = look_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# inspect model
summary(direction_look_fit)

# save workspace
save.image('looking_direction_model_run - dirichlet.RData')
#load('looking_direction_model_run - dirichlet.RData') ; rm(biologylibs, homedrive, homelibs, homelibsprofile,rlibs,Rversion)

## check outputs ####
library(LaplacesDemon)
#load('looking_direction_model_run - dirichlet.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()
summary(direction_look_fit)

## check Stan code
direction_look_fit$model

## calculate log cumulative odds
prop <- table(look_no_na$looking_direction) / nrow(look_no_na)
cum_prop <- cumsum(prop)
log_cum_odds <- logit(cum_prop)

## extract posterior distribution
draws <- as_draws_df(direction_look_fit) %>% 
  select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>% 
  pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>% 
  mutate(iteration = rep(rep(1:(num_iter/2),
                             each = length(unique(parameter))),
                         num_chains),
         chain = rep(1:num_chains,
                     each = length(unique(parameter))*(num_iter/2)),
         invlogit_draw = invlogit(draw))

## look at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

## extract marginal effects
marg <- conditional_effects(direction_look_fit, categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
stim_effect <- marg[[1]]
time_effect <- marg[[2]]
focal_age_effect <- marg[[3]]
agediff_effect <- marg[[4]]
prevsec_effect <- marg[[5]]
partner_age_effect <- marg[[6]]
# marg <- conditional_effects(direction_look_fit, effects = 'focal_age', categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# age_effect <- marg[[1]]


## plot marginal effects
conditional_effects(direction_look_fit, effects = 'focal_age', categorical = TRUE,
                    spaghetti = TRUE,
                    #conditions = c('age_diff_num','stim_type'),
                    #int_conditions = list(focal_age = c(1:4), age_diff_num = c(1:3), stim_type = c('ctd','l','h'),
                    method = 'posterior_epred')
(focal_age_plot <- ggplot(focal_age_effect)+
  geom_ribbon(aes(x = focal_age, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
  geom_line(aes(x = focal_age, y = estimate__, colour = cats__), linewidth = 1)+
  xlab(label = 'focal age') + ylab('probability')+
  scale_colour_viridis_d(name = 'looking direction:',
                         breaks = c('1','2','3'),
                         labels = c('look towards', 'side on', 'look away'))+
  scale_fill_viridis_d(name = 'looking direction:',
                       breaks = c('1','2','3'),
                       labels = c('look towards', 'side on', 'look away')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = focal_age_plot, filename = '../outputs/looking_marginaleffects_focalage.png', device = 'png',
       width = 8.3, height = 5.8)

conditional_effects(direction_look_fit, effects = 'age_diff_num', categorical = TRUE,
                    spaghetti = TRUE,
                    method = 'posterior_epred')
(agediff_plot <- ggplot(agediff_effect)+
  #geom_ribbon(aes(x = age_diff_num, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
  geom_line(aes(x = age_diff_num, y = estimate__, colour = cats__), linewidth = 1)+
  geom_errorbar(aes(x = age_diff_num, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
  geom_point(aes(x = age_diff_num, y = estimate__, colour = cats__),cex = 3)+
  ylab('probability')+
  scale_colour_viridis_d(name = 'looking direction:',
                         breaks = c('1','2','3'),
                         labels = c('look towards', 'side on', 'look away'))+
  scale_fill_viridis_d(name = 'looking direction:',
                       breaks = c('1','2','3'),
                       labels = c('look towards', 'side on', 'look away'))+
  scale_x_continuous(name = 'difference in age category',
                     breaks = c(1,2,3),
                     labels = c('partner younger', 'same age', 'partner older')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = agediff_plot, filename = '../outputs/looking_marginaleffects_agediff.png', device = 'png',
       width = 8.3, height = 5.8)

conditional_effects(direction_look_fit, 'stim_type', categorical = TRUE,
                    method = 'posterior_epred')
(stim_plot <- ggplot(stim_effect)+
  geom_errorbar(aes(x = stim_type, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
  geom_point(aes(x = stim_type, y = estimate__, colour = cats__),cex = 3)+
  ylab('probability')+
  scale_colour_viridis_d(name = 'looking direction:',
                         breaks = c('1','2','3'),
                         labels = c('look towards', 'side on', 'look away'))+
  scale_fill_viridis_d(name = 'looking direction:',
                       breaks = c('1','2','3'),
                       labels = c('look towards', 'side on', 'look away'))+
  scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
                   labels = c('dove (control)', 'lion', 'human'), limits = c('ctd','l','h')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = stim_plot, filename = '../outputs/looking_marginaleffects_stimtype.png', device = 'png',
       width = 8.3, height = 5.8)

#conditional_effects(direction_look_fit, 'time_since_stim', categorical = TRUE)
#conditional_effects(direction_look_fit, 'look_tminus1_num', categorical = TRUE)

library(ggpubr)
(all_plots <- ggarrange(age_plot, agediff_plot, stim_plot, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom"))
ggsave(plot = all_plots, filename = '../outputs/looking_marginaleffects.png', device = 'png',
       width = (5.8*3), height = 8.3)

## posterior predictive check
pp_check(direction_look_fit, ndraws = 100)

## plot traces
draws %>% 
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]","b_stim_typeh","b_stim_typel","b_time_since_stim","bsp_mofocal_age","bsp_moage_diff_num","bsp_molook_tminus1_num","sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept","simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]","simo_moage_diff_num1[1]","simo_moage_diff_num1[2]","simo_molook_tminus1_num1[1]","simo_molook_tminus1_num1[2]")) %>% 
  ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # time since stim has ENORMOUS range (main body approx. -5000 to 10000), sd_playback_id poorly mixed

## plot raw
ggplot(look_no_na, aes(x = focal_age, y = looking_direction,
                       colour = age_difference))+
  geom_jitter()+
  facet_wrap(. ~ stim_type)

look_no_na %>% 
  filter(stim_type == 'ctd') %>% 
  ggplot(aes(x = time_since_stim, y = looking_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
look_no_na %>% 
  filter(stim_type == 'h') %>% 
  ggplot(aes(x = time_since_stim, y = looking_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
look_no_na %>% 
  filter(stim_type == 'l') %>% 
  ggplot(aes(x = time_since_stim, y = looking_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))

## predict from model ####
rm(list = ls()[! ls() %in% c('direction_look_fit','look_no_na')]) ; gc()
subjects <- sample(unique(look_no_na$focal_id), 5, replace = F)
stimuli <- sample(unique(look_no_na$stim_id), 5, replace = F)
pbs <- sample(unique(look_no_na$playback_id), 5, replace = F)
predict_data <- data.frame(focal_age = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
                           age_diff_num = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
                           stim_type = rep(c('ctd','h','l'),
                                           each = 26*3*length(subjects)*length(stimuli)*length(pbs)),
                           time_since_stim = rep(rep(seq(from = -200, to = 300, by = 20),
                                                     each = 3*length(subjects)*length(stimuli)*length(pbs)),
                                                 3),
                           look_tminus1_num = rep(rep(1:3,#c('look at directly','side-on','look directly away'),
                                                      each = length(subjects)*length(stimuli)*length(pbs)),
                                                  3*26),
                           focal_id = rep(rep(subjects,
                                              each = length(stimuli)*length(pbs)),
                                          3*26*3),
                           stim_id = rep(rep(stimuli,
                                             each = length(pbs)),
                                         3*26*3*length(subjects)),
                           playback_id = rep(pbs, 3*26*3*length(subjects)*length(stimuli)))
pred <- posterior_predict(object = direction_look_fit,
                          newdata = predict_data)
age_types <- c('age1_diff1','age1_diff2','age1_diff3',
               'age2_diff1','age2_diff2','age2_diff3',
               'age3_diff1','age3_diff2','age3_diff3',
               'age4_diff1','age4_diff2','age4_diff3')
pred_all <- array(data = NA, dim = c(nrow(pred), ncol(pred), 12),
                  dimnames = list(rownames(pred), colnames(pred),
                                  age_types))
pred_all[,,1] <- pred
save.image('looking_direction_dirichlet_predictions.RData')
for(i in 2:length(age_types)){
  predict_data$focal_age <- ifelse(i < 4, 1,
                                   ifelse(i < 7, 2,
                                          ifelse(i < 10, 3 , 4)))
  predict_data$age_diff_num <- ifelse(i %in% c(1,4,7,10), 1,
                                      ifelse(i %in% c(2,5,8,11), 2, 3))
  # predict_data$focal_age <- ifelse(i < 5, 1,
  #                                  ifelse(i < 9, 2, 3))
  # predict_data$age_diff_num <- ifelse(i %in% c(1,5,9), 1,
  #                                     ifelse(i %in% c(2,6,10), 2,
  #                                            ifelse(i %in% c(3,7,11), 3, 4)))
  pred <- posterior_predict(object = direction_look_fit,
                            newdata = predict_data)
  pred_all[,,i] <- pred
  save.image('looking_direction_dirichlet_predictions.RData')
}

load('looking_direction_dirichlet_predictions.RData')
predict_data$num <- row_number(predict_data)
predictions <- pred_all[,,age_types[1]] %>% 
  as.data.frame()
predictions <- predictions[1:100,] %>% 
  pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
  separate(Vnum, into = c('v','num'), sep = 1) %>% 
  select(-v) %>% 
  mutate(age_type = age_types[1],
         num = as.numeric(num)) %>% 
  left_join(predict_data[,3:ncol(predict_data)], by = 'num')
for(i in 2:length(age_types)){
  pred <- pred_all[,,age_types[i]] %>% 
    as.data.frame()
  pred <- pred[1:100,] %>% 
    pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
    separate(Vnum, into = c('v','num'), sep = 1) %>% 
    select(-v) %>% 
    mutate(age_type = age_types[i],
           num = as.numeric(num)) %>% 
    left_join(predict_data[,3:ncol(predict_data)], by = 'num')
  predictions <- rbind(predictions, pred)
}
save.image('looking_direction_dirichlet_predictions.RData')

age_types <- data.frame(age_type = age_types) %>% 
  separate(age_type, into = c('age','age_diff_num'), remove = F, sep = '_diff') %>% 
  mutate(focal_age = ifelse(age == 'age1', 1,
                            ifelse(age == 'age2', 2,
                                   ifelse(age == 'age3', 3, 4))),
         age_diff_num = as.numeric(age_diff_num)) %>% 
  select(age_type, focal_age, age_diff_num)

rm(pred, pbs, stimuli, subjects, i) ; gc()
predictions <- left_join(predictions, age_types, by = 'age_type')
rm(age_types) ; gc()
save.image('looking_direction_dirichlet_predictions.RData')

## plot outputs ####
load('looking_direction_dirichlet_predictions.RData')
ggplot(predictions, aes(x = time_since_stim, y = prediction))+
  geom_line()+
  geom_point()+
  facet_wrap(.~stim_type)

## plot raw data
look_no_na %>% 
  filter(stim_type == 'ctd') %>% 
  ggplot(aes(x = time_since_stim, y = looking_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))

look_no_na %>% 
  filter(stim_type == 'h') %>% 
  ggplot(aes(x = time_since_stim, y = looking_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))

look_no_na %>% 
  filter(stim_type == 'l') %>% 
  ggplot(aes(x = time_since_stim, y = looking_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))

## plot predictions
summary(direction_look_fit)
# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: looking_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type + time_since_stim + mo(look_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id) 
# Data: look_no_na (Number of observations: 171820) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~focal_id (Number of levels: 176) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.16      0.07     1.03     1.31 1.00      739     1753
# 
# ~playback_id (Number of levels: 48) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.30      0.18     0.01     0.66 1.03      103      763
# 
# ~stim_id (Number of levels: 30) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.19      0.14     0.01     0.50 1.02      362     1019
# 
# Population-Level Effects: 
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]          -1.06      0.27    -1.57    -0.50 1.00     1595     2125
# Intercept[2]           1.39      0.27     0.88     1.94 1.00     1598     2129
# stim_typeh            -0.23      0.27    -0.77     0.31 1.00     1939     2524
# stim_typel            -0.42      0.27    -0.94     0.12 1.00     1419     2101
# time_since_stim        0.00      0.00    -0.00     0.00 1.00     4052     3382
# mofocal_age            0.16      0.10    -0.03     0.36 1.00     1329     2050
# moage_diff_num         0.25      0.01     0.23     0.27 1.00     5693     3046
# molook_tminus1_num     0.60      0.01     0.59     0.62 1.00     6678     2955
# 
# Simplex Parameters: 
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mofocal_age1[1]            0.33      0.17     0.06     0.68 1.00     5208     3140
# mofocal_age1[2]            0.32      0.16     0.06     0.67 1.00     4590     3384
# mofocal_age1[3]            0.35      0.17     0.07     0.73 1.00     4632     3302
# moage_diff_num1[1]         0.72      0.03     0.66     0.77 1.00     7827     2761
# moage_diff_num1[2]         0.28      0.03     0.23     0.34 1.00     7827     2761
# molook_tminus1_num1[1]     0.46      0.01     0.44     0.48 1.00     6562     3121
# molook_tminus1_num1[2]     0.54      0.01     0.52     0.56 1.00     6562     3121
# 
# Family Specific Parameters: 
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message:
#   There were 20 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

## want to extract an estimated probability of each value at every time point, split across the different types
## take predictions from model and mean across random effects? so mean+/-stdev prediction for seconds = -120, age = 10-15, age_diff = 1, stim = ctd  -- pretty sure this would be the equivalent of treating it like a continuous variable again which is definitely not right, but right now I have no other ideas!! I think I probably need to extract it directly from the model draws for each parameter, but I don't think I know how to do that...

age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
ggplot()+
  annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
  geom_point(data = look_no_na,
             mapping = aes(x = time_since_stim, y = looking_direction),
             alpha = 0.01, shape = 19)+
  # geom_violin(data = look_no_na,
  #             mapping = aes(y = time_since_stim, x = as.factor(looking_direction)))+
  # coord_flip()+
  facet_wrap(. ~ focal_age,
             labeller = labeller(focal_age = age_labels))+
  scale_y_continuous(name = 'looking direction',
                     breaks = c(1,2,3), labels = c('look at','side on','look away'),
                     expand = c(0,0))+
  scale_x_continuous(name = 'time since stimulus (s)')

predict_means <- predictions %>% 
  select(focal_age, age_diff_num, stim_type, time_since_stim, look_tminus1_num) %>% 
  distinct() %>% 
  mutate(mean_pred = NA, stdv_pred = NA)
for(i in 1:nrow(predict_means)){
  x <- predictions$prediction[predictions$focal_age == predict_means$focal_age[i] &
                                predictions$stim_type == predict_means$stim_type[i] &
                                predictions$age_diff_num == predict_means$age_diff_num[i] &
                                predictions$time_since_stim == predict_means$time_since_stim[i] &
                                predictions$look_tminus1_num == predict_means$look_tminus1_num[i]]
  predict_means$mean_pred[i] <- mean(x)
  predict_means$stdv_pred[i] <- sd(x)
}
predict_means$lwr <- predict_means$mean_pred - predict_means$stdv_pred
predict_means$upr <- predict_means$mean_pred + predict_means$stdv_pred

save.image('looking_direction_dirichlet_predictions.RData')

age_labels <- c('partner younger','age matched','partner older')
names(age_labels) <- c(1,2,3)
stim_labels <- c('dove (control)','lion','human')
names(stim_labels) <- c('ctd','l','h')
ggplot()+
  annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
  geom_point(data = look_no_na,
             mapping = aes(x = time_since_stim, y = looking_direction),
             alpha = 0.01, shape = 19)+
  facet_grid(stim_type ~ age_diff_num,
             labeller = labeller(age_diff_num = age_labels,
                                 stim_type = stim_labels))+
  # geom_ribbon(data = predict_means,
  #             mapping = aes(x = time_since_stim, ymax = upr, ymin = lwr,
  #                           fill = as.factor(focal_age)),
  #             alpha = 0.2)+
  geom_line(data = predict_means,
            mapping = aes(x = time_since_stim, y = mean_pred,
                          colour = as.factor(focal_age),
                          linetype = as.factor(look_tminus1_num)))+
  scale_y_continuous(name = 'looking direction',
                     breaks = c(1,2,3), labels = c('look at','side on','look away'),
                     expand = c(0,0))+
  scale_x_continuous(name = 'time since stimulus (s)')










#### MOVEMENT DIRECTION: ordinal logistic regression ####
# https://dagitty.net/dags.html?id=dw8twK
#library(brms, lib.loc = '../../packages/') # library(brms)

# read in data
ages <- readRDS('../data_processed/elephant_behaviour_proportions.RDS') %>% 
  select(pb_num, subject, targeted_elephant,    # random effects
         #stim_num,                             # unnecessary random effects
         stim_type,age_category,partner_age_category,age_difference, # exposures
         age,partner_age,focal, dyad_partner,   # helpful for reference, don't include
         #section, # need to know when stimulus was, but don't want as categorical
         group_size # control for this
  ) %>% 
  mutate(group_size = ifelse(pb_num == 44, 7, group_size)) %>% 
  distinct()

stim_starts <- readRDS('../data_processed/stimuli.RDS') %>% 
  filter(status == 'START' & behavior == 'STIMULUS') %>% 
  select(pb_num,time,stim_num,comment)
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
  select(pb_num, stim_start,stim_num)

move <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>% 
  select(subject,bull,pb_num,second,out_frame_name,out_frame_index,
         b1_move_name,#b1_move_index,
         b2_move_name,#b2_move_index,
         b3_move_name,#b3_move_index,
         b4_move_name,#b4_move_index,
         b5_move_name,#b5_move_index,
         b6_move_name,#b6_move_index,
         b7_move_name,#b7_move_index,
         b8_move_name#,b8_move_index,
  ) %>% 
  pivot_longer(cols = c('b1_move_name','b2_move_name','b3_move_name','b4_move_name',
                        'b5_move_name','b6_move_name','b7_move_name','b8_move_name'),
               names_to = 'elephant_move', values_to = 'direction_move') %>% 
  filter(is.na(direction_move) == FALSE) %>% 
  filter(direction_move != 'impossible_partner') %>% 
  filter(direction_move != 'out_of_sight') %>% 
  mutate(direction_move = ifelse(direction_move == 'approach directly',
                                 'approach_directly',
                                 ifelse(direction_move == 'approach at an angle',
                                        'approach_angle',
                                        ifelse(direction_move == 'move directly with',
                                               'move_with',
                                               ifelse(direction_move == 'move away at an angle',
                                                      'away_angle',
                                                      ifelse(direction_move == 'move away directly',
                                                             'away_directly',direction_move)))))
  ) %>% 
  mutate(move_index = as.integer(factor(direction_move,
                                        levels = c('not_moving',
                                                   'approach_directly','approach_angle',
                                                   'move_with',
                                                   'away_angle','away_directly')))) %>% 
  separate(elephant_move, into = c('dyad_partner','_move_name'), sep = 2) %>% 
  select(-`_move_name`) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(distinct(ages[,c('subject', 'age_category')]),
            by = 'subject') %>% 
  left_join(distinct(ages[,c('pb_num','partner_age_category','dyad_partner')]),
            by = c('dyad_partner','pb_num')) %>% 
  left_join(distinct(ages[,c('pb_num','stim_type')]),
            by = 'pb_num') %>% 
  left_join(distinct(ages[,c('subject','dyad_partner','age_difference')]),
            by = c('subject','dyad_partner')) %>% 
  left_join(stim_starts, by = 'pb_num') %>% 
  mutate(time_since_stim = second - stim_start) %>% 
  mutate(move_tminus1 = NA)
playbacks <- unique(move$pb_num)
for(i in 1:length(playbacks)){
  pb <- move %>% filter(pb_num == playbacks[i])
  move <- move %>% anti_join(pb)
  for(j in 2:nrow(pb)){
    pb$move_tminus1[j] <- pb$direction_move[j-1]
  }
  move <- rbind(move, pb)
}
rm(check, x, i, multiple_starts) ; gc()

# filter to remove elephants with unknown ages
move_no_na <- move %>% 
  filter(is.na(age_category) == FALSE) %>% 
  filter(is.na(partner_age_category) == FALSE) %>% 
  filter(is.na(move_tminus1) == FALSE) %>% 
  mutate(#stim_type = ifelse(stim_type == 'h', 3,
    #                   ifelse(stim_type == 'l', 2, 1)),
    focal_id = as.integer(as.factor(subject)),
    stim_num = as.integer(as.factor(stim_num)),
    age_category = as.factor(age_category),
    partner_age_category = as.factor(partner_age_category)) %>% 
  rename(moving_direction = move_index,
         focal_age = age_category,
         partner_age = partner_age_category,
         stim_id = stim_num,
         playback_id = pb_num)
str(move_no_na)
# n_obs <- nrow(move_no_na)  # number of observations
# n_direct <- 3              # number of moving directions + 1
# move_ls <- list(           # generate data list
#   n_obs = n_obs,
#   n_direct = n_direct,
#   moving_direction = move_no_na$move_index,
#   focal_age = move_no_na$age_category,
#   partner_age = move_no_na$partner_age_category,
#   #age_difference = move_no_na$age_difference,
#   stim_type = move_no_na$stim_type,
#   time_since_stim = move_no_na$time_since_stim,
#   focal_id = move_no_na$focal_id,
#   stim_id = move_no_na$stim_num,
#   playback_id = move_no_na$pb_num)

# set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
move_no_na <- move_no_na %>% 
  mutate(age_diff_num = ifelse(age_difference == 'partner younger', 1,
                               ifelse(age_difference == 'matched', 2, 3)),
         move_tminus1_num = ifelse(move_tminus1 == 'move at directly', 1,
                                   ifelse(move_tminus1 == 'side-on', 2, 3))) %>% 
  mutate(focal_age = as.integer(focal_age),               #as.factor(focal_age),
         partner_age = as.integer(partner_age),
         age_diff_num = as.integer(age_diff_num),         #as.factor(age_diff_num),
         move_tminus1_num = as.integer(move_tminus1_num)) #as.factor(move_tminus1_num))

get_prior(formula = moving_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type +   # fixed effects
            time_since_stim + mo(move_tminus1_num) + mo(partner_age) +                      # controls
            (1|focal_id) + (1|stim_id) + (1|playback_id),                                   # random effects
          data = move_no_na,
          family = cumulative("logit"))
priors <- c(prior(normal(0,0.25),   class = b,    coef = mofocal_age),
            prior(dirichlet(2,2,2), class = simo, coef = mofocal_age1),
            prior(normal(0,0.333),  class = b,    coef = moage_diff_num),
            prior(dirichlet(2,2),   class = simo, coef = moage_diff_num1),
            prior(normal(0,0.25),   class = b,    coef = mopartner_age),
            prior(dirichlet(2,2,2), class = simo, coef = mopartner_age1),
            prior(normal(0,1),      class = b,    coef = stim_typeh),
            prior(normal(0,1),      class = b,    coef = stim_typel),
            prior(normal(0,1),      class = b,    coef = time_since_stim),
            prior(normal(0,0.333),  class = b,    coef = momove_tminus1_num),
            prior(dirichlet(2),     class = simo, coef = momove_tminus1_num1))

## prior predictive check -- DOESN'T WORK BECAUSE LENGTH OF SIMPLEX IS 0, BUT I DON'T UNDERSTAND THAT....
num_chains <- 4
num_iter <- 2000
direction_move_prior <- brm(
  formula = moving_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type + 
    time_since_stim + mo(move_tminus1_num) + mo(partner_age) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')
pp_check(direction_move_prior)

make_stancode(
  formula = moving_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type + 
    time_since_stim + mo(move_tminus1_num) + mo(partner_age) +
    (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345,
  sample_prior = 'only')


## fit model
direction_move_fit <- brm(
  formula = moving_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type + time_since_stim + mo(move_tminus1_num) + mo(partner_age) + (1|focal_id) + (1|stim_id) + (1|playback_id),
  data = move_no_na,
  family = cumulative("logit"),
  prior = priors, chains = num_chains, cores = num_chains,
  iter = num_iter, warmup = num_iter/2, seed = 12345)

# inspect model
summary(direction_move_fit)

# save workspace
save.image('moving_direction_model_run - dirichlet.RData')

## check outputs ####
library(LaplacesDemon)
#load('moving_direction_model_run - dirichlet.RData') # rm(biologylibs, homedrive, homelibs, homelibsprofile, rlibs, Rversion) ; gc()
summary(direction_move_fit)

## check Stan code
direction_move_fit$model

## calculate log cumulative odds
prop <- table(move_no_na$moving_direction) / nrow(move_no_na)
cum_prop <- cumsum(prop)
log_cum_odds <- logit(cum_prop)

## extract posterior distribution
draws <- as_draws_df(direction_move_fit) %>% 
  select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>% 
  pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>% 
  mutate(iteration = rep(rep(1:(num_iter/2),
                             each = length(unique(parameter))),
                         num_chains),
         chain = rep(1:num_chains,
                     each = length(unique(parameter))*(num_iter/2)),
         invlogit_draw = invlogit(draw))

## move at intercepts (estimates of cutpoints between categories on linear model scale)
b_int1 <- draws %>% filter(parameter == 'b_Intercept[1]')
b_int2 <- draws %>% filter(parameter == 'b_Intercept[2]')
par(mfrow = c(2,2))
hist(b_int1$draw) ; hist(b_int2$draw) ; hist(b_int1$invlogit_draw) ; hist(b_int2$invlogit_draw)
par(mfrow = c(1,1))

## extract marginal effects
marg <- conditional_effects(direction_move_fit, categorical = TRUE,
                            #spaghetti = TRUE,
                            method = 'posterior_epred')
names(marg)
stim_effect <- marg[[1]]
time_effect <- marg[[2]]
age_effect <- marg[[3]]
agediff_effect <- marg[[4]]
prevsec_effect <- marg[[5]]
# marg <- conditional_effects(direction_move_fit, effects = 'focal_age', categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# age_effect <- marg[[1]]


## plot marginal effects
conditional_effects(direction_move_fit, effects = 'focal_age', categorical = TRUE,
                    spaghetti = TRUE,
                    #conditions = c('age_diff_num','stim_type'),
                    #int_conditions = list(focal_age = c(1:4), age_diff_num = c(1:3), stim_type = c('ctd','l','h'),
                    method = 'posterior_epred')
(age_plot <- ggplot(age_effect)+
    geom_ribbon(aes(x = focal_age, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
    geom_line(aes(x = focal_age, y = estimate__, colour = cats__), linewidth = 1)+
    xlab(label = 'focal age') + ylab('probability')+
    scale_colour_viridis_d(name = 'moving direction:', breaks = c('1','2','3'), labels = c('move towards', 'side on', 'move away'))+
    scale_fill_viridis_d(name = 'moving direction:', breaks = c('1','2','3'), labels = c('move towards', 'side on', 'move away')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = age_plot, filename = '../outputs/moving_marginaleffects_age.png', device = 'png',
       width = 8.3, height = 5.8)

conditional_effects(direction_move_fit, effects = 'age_diff_num', categorical = TRUE,
                    spaghetti = TRUE,
                    method = 'posterior_epred')
(agediff_plot <- ggplot(agediff_effect)+
    #geom_ribbon(aes(x = age_diff_num, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
    geom_line(aes(x = age_diff_num, y = estimate__, colour = cats__), linewidth = 1)+
    geom_errorbar(aes(x = age_diff_num, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
    geom_point(aes(x = age_diff_num, y = estimate__, colour = cats__),cex = 3)+
    ylab('probability')+
    scale_colour_viridis_d(name = 'moving direction:', breaks = c('1','2','3'), labels = c('move towards', 'side on', 'move away'))+
    scale_fill_viridis_d(name = 'moving direction:', breaks = c('1','2','3'), labels = c('move towards', 'side on', 'move away'))+
    scale_x_continuous(name = 'difference in age category', breaks = c(1,2,3), labels = c('partner younger', 'same age', 'partner older')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = agediff_plot, filename = '../outputs/moving_marginaleffects_agediff.png', device = 'png',
       width = 8.3, height = 5.8)

conditional_effects(direction_move_fit, 'stim_type', categorical = TRUE,
                    method = 'posterior_epred')
(stim_plot <- ggplot(stim_effect)+
    geom_errorbar(aes(x = stim_type, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
    geom_point(aes(x = stim_type, y = estimate__, colour = cats__),cex = 3)+
    ylab('probability')+
    scale_colour_viridis_d(name = 'moving direction:', breaks = c('1','2','3'), labels = c('move towards', 'side on', 'move away'))+
    scale_fill_viridis_d(name = 'moving direction:', breaks = c('1','2','3'), labels = c('move towards', 'side on', 'move away'))+
    scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
                     labels = c('dove (control)', 'lion', 'human'), limits = c('ctd','l','h')))+
  theme(legend.position = 'bottom',
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
ggsave(plot = stim_plot, filename = '../outputs/moving_marginaleffects_stimtype.png', device = 'png',
       width = 8.3, height = 5.8)

#conditional_effects(direction_move_fit, 'time_since_stim', categorical = TRUE)
#conditional_effects(direction_move_fit, 'move_tminus1_num', categorical = TRUE)

library(ggpubr)
(all_plots <- ggarrange(age_plot, agediff_plot, stim_plot, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom"))
ggsave(plot = all_plots, filename = '../outputs/moving_marginaleffects.png', device = 'png',
       width = (5.8*3), height = 8.3)

## posterior predictive check
pp_check(direction_move_fit, ndraws = 100)

## plot traces
draws %>% 
  filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]","b_stim_typeh","b_stim_typel","b_time_since_stim","bsp_mofocal_age","bsp_moage_diff_num","bsp_momove_tminus1_num","sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept","simo_mofocal_age1[1]","simo_mofocal_age1[2]","simo_mofocal_age1[3]","simo_moage_diff_num1[1]","simo_moage_diff_num1[2]","simo_momove_tminus1_num1[1]","simo_momove_tminus1_num1[2]")) %>% 
  ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
  geom_line()+
  facet_wrap(. ~ parameter, scales = 'free_y')+
  theme(legend.position = 'none') # time since stim has ENORMOUS range (main body approx. -5000 to 10000), sd_playback_id poorly mixed

## plot raw
ggplot(move_no_na, aes(x = focal_age, y = moving_direction,
                       colour = age_difference))+
  geom_jitter()+
  facet_wrap(. ~ stim_type)

move_no_na %>% 
  filter(stim_type == 'ctd') %>% 
  ggplot(aes(x = time_since_stim, y = moving_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
move_no_na %>% 
  filter(stim_type == 'h') %>% 
  ggplot(aes(x = time_since_stim, y = moving_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))
move_no_na %>% 
  filter(stim_type == 'l') %>% 
  ggplot(aes(x = time_since_stim, y = moving_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))

## predict from model ####
rm(list = ls()[! ls() %in% c('direction_move_fit','move_no_na')]) ; gc()
subjects <- sample(unique(move_no_na$focal_id), 5, replace = F)
stimuli <- sample(unique(move_no_na$stim_id), 5, replace = F)
pbs <- sample(unique(move_no_na$playback_id), 5, replace = F)
predict_data <- data.frame(focal_age = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
                           age_diff_num = rep(1, 3*26*3*length(subjects)*length(stimuli)*length(pbs)),
                           stim_type = rep(c('ctd','h','l'),
                                           each = 26*3*length(subjects)*length(stimuli)*length(pbs)),
                           time_since_stim = rep(rep(seq(from = -200, to = 300, by = 20),
                                                     each = 3*length(subjects)*length(stimuli)*length(pbs)),
                                                 3),
                           move_tminus1_num = rep(rep(1:3,#c('move at directly','side-on','move directly away'),
                                                      each = length(subjects)*length(stimuli)*length(pbs)),
                                                  3*26),
                           focal_id = rep(rep(subjects,
                                              each = length(stimuli)*length(pbs)),
                                          3*26*3),
                           stim_id = rep(rep(stimuli,
                                             each = length(pbs)),
                                         3*26*3*length(subjects)),
                           playback_id = rep(pbs, 3*26*3*length(subjects)*length(stimuli)))
pred <- posterior_predict(object = direction_move_fit,
                          newdata = predict_data)
age_types <- c('age1_diff1','age1_diff2','age1_diff3',
               'age2_diff1','age2_diff2','age2_diff3',
               'age3_diff1','age3_diff2','age3_diff3',
               'age4_diff1','age4_diff2','age4_diff3')
pred_all <- array(data = NA, dim = c(nrow(pred), ncol(pred), 12),
                  dimnames = list(rownames(pred), colnames(pred),
                                  age_types))
pred_all[,,1] <- pred
save.image('moving_direction_dirichlet_predictions.RData')
for(i in 2:length(age_types)){
  predict_data$focal_age <- ifelse(i < 4, 1,
                                   ifelse(i < 7, 2,
                                          ifelse(i < 10, 3 , 4)))
  predict_data$age_diff_num <- ifelse(i %in% c(1,4,7,10), 1,
                                      ifelse(i %in% c(2,5,8,11), 2, 3))
  # predict_data$focal_age <- ifelse(i < 5, 1,
  #                                  ifelse(i < 9, 2, 3))
  # predict_data$age_diff_num <- ifelse(i %in% c(1,5,9), 1,
  #                                     ifelse(i %in% c(2,6,10), 2,
  #                                            ifelse(i %in% c(3,7,11), 3, 4)))
  pred <- posterior_predict(object = direction_move_fit,
                            newdata = predict_data)
  pred_all[,,i] <- pred
  save.image('moving_direction_dirichlet_predictions.RData')
}

load('moving_direction_dirichlet_predictions.RData')
predict_data$num <- row_number(predict_data)
predictions <- pred_all[,,age_types[1]] %>% 
  as.data.frame()
predictions <- predictions[1:100,] %>% 
  pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
  separate(Vnum, into = c('v','num'), sep = 1) %>% 
  select(-v) %>% 
  mutate(age_type = age_types[1],
         num = as.numeric(num)) %>% 
  left_join(predict_data[,3:ncol(predict_data)], by = 'num')
for(i in 2:length(age_types)){
  pred <- pred_all[,,age_types[i]] %>% 
    as.data.frame()
  pred <- pred[1:100,] %>% 
    pivot_longer(everything(), names_to = 'Vnum', values_to = 'prediction') %>% 
    separate(Vnum, into = c('v','num'), sep = 1) %>% 
    select(-v) %>% 
    mutate(age_type = age_types[i],
           num = as.numeric(num)) %>% 
    left_join(predict_data[,3:ncol(predict_data)], by = 'num')
  predictions <- rbind(predictions, pred)
}
save.image('moving_direction_dirichlet_predictions.RData')

age_types <- data.frame(age_type = age_types) %>% 
  separate(age_type, into = c('age','age_diff_num'), remove = F, sep = '_diff') %>% 
  mutate(focal_age = ifelse(age == 'age1', 1,
                            ifelse(age == 'age2', 2,
                                   ifelse(age == 'age3', 3, 4))),
         age_diff_num = as.numeric(age_diff_num)) %>% 
  select(age_type, focal_age, age_diff_num)

rm(pred, pbs, stimuli, subjects, i) ; gc()
predictions <- left_join(predictions, age_types, by = 'age_type')
rm(age_types) ; gc()
save.image('moving_direction_dirichlet_predictions.RData')

## plot outputs ####
load('moving_direction_dirichlet_predictions.RData')
ggplot(predictions, aes(x = time_since_stim, y = prediction))+
  geom_line()+
  geom_point()+
  facet_wrap(.~stim_type)

## plot raw data
move_no_na %>% 
  filter(stim_type == 'ctd') %>% 
  ggplot(aes(x = time_since_stim, y = moving_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))

move_no_na %>% 
  filter(stim_type == 'h') %>% 
  ggplot(aes(x = time_since_stim, y = moving_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))

move_no_na %>% 
  filter(stim_type == 'l') %>% 
  ggplot(aes(x = time_since_stim, y = moving_direction,
             group = focal_id))+
  geom_vline(aes(xintercept = 0))+
  geom_point(colour = rgb(0,0,1,0.01))+
  #geom_line()+
  facet_grid(focal_age ~ factor(age_difference, levels = c('partner younger','matched','partner older')))

## plot predictions
summary(direction_move_fit)
# Family: cumulative 
# Links: mu = logit; disc = identity 
# Formula: moving_direction ~ 1 + mo(focal_age) + mo(age_diff_num) + stim_type + time_since_stim + mo(move_tminus1_num) + (1 | focal_id) + (1 | stim_id) + (1 | playback_id) 
# Data: move_no_na (Number of observations: 171820) 
# Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
# total post-warmup draws = 4000
# 
# Group-Level Effects: 
#   ~focal_id (Number of levels: 176) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.16      0.07     1.03     1.31 1.00      739     1753
# 
# ~playback_id (Number of levels: 48) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.30      0.18     0.01     0.66 1.03      103      763
# 
# ~stim_id (Number of levels: 30) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.19      0.14     0.01     0.50 1.02      362     1019
# 
# Population-Level Effects: 
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept[1]          -1.06      0.27    -1.57    -0.50 1.00     1595     2125
# Intercept[2]           1.39      0.27     0.88     1.94 1.00     1598     2129
# stim_typeh            -0.23      0.27    -0.77     0.31 1.00     1939     2524
# stim_typel            -0.42      0.27    -0.94     0.12 1.00     1419     2101
# time_since_stim        0.00      0.00    -0.00     0.00 1.00     4052     3382
# mofocal_age            0.16      0.10    -0.03     0.36 1.00     1329     2050
# moage_diff_num         0.25      0.01     0.23     0.27 1.00     5693     3046
# momove_tminus1_num     0.60      0.01     0.59     0.62 1.00     6678     2955
# 
# Simplex Parameters: 
#                        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# mofocal_age1[1]            0.33      0.17     0.06     0.68 1.00     5208     3140
# mofocal_age1[2]            0.32      0.16     0.06     0.67 1.00     4590     3384
# mofocal_age1[3]            0.35      0.17     0.07     0.73 1.00     4632     3302
# moage_diff_num1[1]         0.72      0.03     0.66     0.77 1.00     7827     2761
# moage_diff_num1[2]         0.28      0.03     0.23     0.34 1.00     7827     2761
# momove_tminus1_num1[1]     0.46      0.01     0.44     0.48 1.00     6562     3121
# momove_tminus1_num1[2]     0.54      0.01     0.52     0.56 1.00     6562     3121
# 
# Family Specific Parameters: 
#      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# disc     1.00      0.00     1.00     1.00   NA       NA       NA
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).
# Warning message:
#   There were 20 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup

## want to extract an estimated probability of each value at every time point, split across the different types
## take predictions from model and mean across random effects? so mean+/-stdev prediction for seconds = -120, age = 10-15, age_diff = 1, stim = ctd  -- pretty sure this would be the equivalent of treating it like a continuous variable again which is definitely not right, but right now I have no other ideas!! I think I probably need to extract it directly from the model draws for each parameter, but I don't think I know how to do that...

age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
names(age_labels) <- c(1,2,3,4)
ggplot()+
  annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
  geom_point(data = move_no_na,
             mapping = aes(x = time_since_stim, y = moving_direction),
             alpha = 0.01, shape = 19)+
  # geom_violin(data = move_no_na,
  #             mapping = aes(y = time_since_stim, x = as.factor(moving_direction)))+
  # coord_flip()+
  facet_wrap(. ~ focal_age,
             labeller = labeller(focal_age = age_labels))+
  scale_y_continuous(name = 'moving direction',
                     breaks = c(1,2,3), labels = c('move at','side on','move away'),
                     expand = c(0,0))+
  scale_x_continuous(name = 'time since stimulus (s)')

predict_means <- predictions %>% 
  select(focal_age, age_diff_num, stim_type, time_since_stim, move_tminus1_num) %>% 
  distinct() %>% 
  mutate(mean_pred = NA, stdv_pred = NA)
for(i in 1:nrow(predict_means)){
  x <- predictions$prediction[predictions$focal_age == predict_means$focal_age[i] &
                                predictions$stim_type == predict_means$stim_type[i] &
                                predictions$age_diff_num == predict_means$age_diff_num[i] &
                                predictions$time_since_stim == predict_means$time_since_stim[i] &
                                predictions$move_tminus1_num == predict_means$move_tminus1_num[i]]
  predict_means$mean_pred[i] <- mean(x)
  predict_means$stdv_pred[i] <- sd(x)
}
predict_means$lwr <- predict_means$mean_pred - predict_means$stdv_pred
predict_means$upr <- predict_means$mean_pred + predict_means$stdv_pred

save.image('moving_direction_dirichlet_predictions.RData')

age_labels <- c('partner younger','age matched','partner older')
names(age_labels) <- c(1,2,3)
stim_labels <- c('dove (control)','lion','human')
names(stim_labels) <- c('ctd','l','h')
ggplot()+
  annotate('rect', xmin = 0, xmax = 30, ymin = 0.9, ymax = 3.1, fill = 'grey90')+
  geom_point(data = move_no_na,
             mapping = aes(x = time_since_stim, y = moving_direction),
             alpha = 0.01, shape = 19)+
  facet_grid(stim_type ~ age_diff_num,
             labeller = labeller(age_diff_num = age_labels,
                                 stim_type = stim_labels))+
  # geom_ribbon(data = predict_means,
  #             mapping = aes(x = time_since_stim, ymax = upr, ymin = lwr,
  #                           fill = as.factor(focal_age)),
  #             alpha = 0.2)+
  geom_line(data = predict_means,
            mapping = aes(x = time_since_stim, y = mean_pred,
                          colour = as.factor(focal_age),
                          linetype = as.factor(move_tminus1_num)))+
  scale_y_continuous(name = 'moving direction',
                     breaks = c(1,2,3), labels = c('move at','side on','move away'),
                     expand = c(0,0))+
  scale_x_continuous(name = 'time since stimulus (s)')


