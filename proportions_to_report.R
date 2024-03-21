#### information ####
# script for obtaining basic proportions of time spent per activity

#### set up ####
#library(tidyverse) ; library(LaplacesDemon)
library(tidyverse, lib.loc = '../packages/')
library(LaplacesDemon, lib.loc = '../packages/')

set.seed(12345)

#### data import ####
# read in data
prop <- readRDS('../data_processed/proportions_of_time_per_behaviour.RDS') %>%
  mutate(stim_type_full = ifelse(stim_type == 'l', 'lion',
                                 ifelse(stim_type == 'ctd', 'control', 'human')))
bda <- read_csv('../data_processed/behaviour_by_second_indexvariables_bda.RDS')

# make sure age data is present for all rows
ages <- bda %>% 
  select(focal, f_age_cat, f_age_num) %>% 
  distinct() %>% 
  filter(!is.na(f_age_cat))
bda <- bda %>% 
  select(-f_age_cat, -f_age_num) %>% 
  left_join(ages, by = 'focal')
ages <- ages %>% 
  rename(partner = focal,
         p_age_cat = f_age_cat,
         p_age_num = f_age_num)
bda <- bda %>% 
  select(-p_age_cat, -p_age_num) %>% 
  left_join(ages, by = 'partner')

#### total counts of experiments ####
length(unique(bda$pb_num))
length(unique(bda$focal))

#### all behaviours ####
behav <- prop %>%
  filter(focal != 'b2_e13') %>%  # out of sight for entire useful period of interest
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

#### create function for writing outputs to file ####
write_prop <- function(df, stimulus, category){
  short <- df %>%
    filter(stim_type == stimulus)
  if(category == 'look'){
    short <- short %>%
      select(bda,age_difference,look_towards,side_on,look_away)
  }
  if(category == 'move'){
    short <- short %>%
      select(bda,age_difference,move_towards_direct,move_towards_angle,
             move_side,move_away_angle,move_away_direct)
  }
  if(category == 'nn'){
    short <- short %>%
      select(bda,nn_young,nn_match,nn_older)
  }
  write_csv(short, paste0('../outputs/proportions_',category,'_',stimulus,'.csv'))
}

#### looking direction ####
look <- bda %>%
  filter(action != 'out_of_sight') %>% 
  filter(activity == 'look') %>%
  mutate(age_difference = ifelse(f_age_num > p_age_num, 'partner younger',
                                 ifelse(f_age_num == p_age_num, 'matched', 'partner older'))) %>% 
  filter(!is.na(age_difference)) %>% 
  select(bda, action, focal, partner, stim_type, age_difference, second)

look_summary <- expand.grid(bda = c('before','during','after'),
                            action = unique(look$action),
                            focal = unique(look$focal),
                            partner = unique(look$partner)) %>% 
  mutate(focal = as.character(focal),
         partner = as.character(partner)) %>% 
  filter(focal != partner) %>% 
  separate(focal, into = c('bull_focal','pb_focal'),
           remove = F, sep = '_') %>% 
  separate(partner, into = c('bull_partner','pb_partner'),
           remove = F, sep = '_') %>% 
  filter(pb_focal == pb_partner) %>% 
  select(-pb_focal, -pb_partner, -bull_focal, -bull_partner) %>% 
  left_join(distinct(bda[,c('focal','stim_type','f_age_num')]), by = 'focal') %>% 
  left_join(distinct(bda[,c('partner','p_age_num')]), by = 'partner') %>% 
  mutate(age_difference = ifelse(f_age_num > p_age_num, 'partner younger',
                                 ifelse(f_age_num == p_age_num, 'matched', 'partner older'))) %>% 
  mutate(count = NA,
         total = NA,
         propn = NA)

## no separation by stimulus or age -- report in text
for(i in 1:nrow(look_summary)){
  look_summary$total[i] <- length(which(look$bda == look_summary$bda[i] & 
                                          look$focal == look_summary$focal[i] & 
                                          look$partner == look_summary$partner[i]))
  look_summary$count[i] <- length(which(look$bda == look_summary$bda[i] & 
                                          look$action == look_summary$action[i] & 
                                          look$focal == look_summary$focal[i] & 
                                          look$partner == look_summary$partner[i]))
}
look_summary <- look_summary %>%
  mutate(propn = count / total)
View(look_summary %>%
       mutate(percent = propn * 100) %>% 
       group_by(bda, action) %>%
       summarise(mean = mean(percent, na.rm = T),
                 stdv = sd(percent, na.rm = T)))

## separate by stimulus and age -- report in table
for(i in 1:nrow(look_summary)){
  look_summary$total[i] <- length(which(look$bda == look_summary$bda[i] & 
                                          look$focal == look_summary$focal[i] & 
                                          look$partner == look_summary$partner[i] &
                                          look$stim_type == look_summary$stim_type[i]))
  look_summary$count[i] <- length(which(look$bda == look_summary$bda[i] & 
                                          look$action == look_summary$action[i] & 
                                          look$focal == look_summary$focal[i] & 
                                          look$partner == look_summary$partner[i] &
                                          look$stim_type == look_summary$stim_type[i]))
}
look_summary <- look_summary %>%
  mutate(propn = count / total)
View(look_summary %>%
       mutate(percent = propn * 100) %>% 
       group_by(bda, action, age_difference, stim_type) %>%
       summarise(mean = mean(percent, na.rm = T),
                 stdv = sd(percent, na.rm = T)))

## graph
look_summary %>%
  mutate(percent = propn * 100) %>% 
  group_by(bda, action, age_difference, stim_type) %>%
  summarise(mean = mean(percent, na.rm = T),
            stdv = sd(percent, na.rm = T)) %>% 
  ggplot()+
  geom_col(aes(x = bda, y = mean, fill = age_difference),
           position = 'dodge')+
  geom_errorbar(aes(x = bda, ymin = mean-stdv, ymax = mean+stdv, group = age_difference),
                position = 'dodge', width = 1)+
  facet_wrap(. ~ action)

## make wider so nicer to read as a table
look_wide <- look_summary %>%
  mutate(percent = propn * 100) %>% 
  group_by(bda, action, age_difference, stim_type) %>%
  summarise(mean = mean(percent, na.rm = T),
            stdv = sd(percent, na.rm = T)) %>% 
  pivot_wider(names_from = 'action', values_from = c('mean','stdv')) %>%
  rename(at_mean = `mean_look at directly`,
         away_mean = `mean_look directly away`,
         side_mean = `mean_side-on`,
         at_stdv = `stdv_look at directly`,
         away_stdv = `stdv_look directly away`,
         side_stdv = `stdv_side-on`) %>%
  mutate(look_towards = paste0(round(at_mean,2),  ' ± ', round(at_stdv,2)),
         side_on      = paste0(round(side_mean,2),' ± ', round(side_stdv,2)),
         look_away    = paste0(round(away_mean,2),' ± ', round(away_stdv,2)))

# save outputs as files
write_prop(df = look_wide, stimulus = 'ctd', category = 'look')
write_prop(df = look_wide, stimulus = 'l', category = 'look')
write_prop(df = look_wide, stimulus = 'h', category = 'look')

# check outputs
head(read_csv('../outputs/proportions_look_ctd.csv'))

#### movement direction ####
move <- bda %>%
  filter(action != 'out_of_sight') %>% 
  filter(activity == 'move') %>%
  mutate(binary = ifelse(action == 'not_moving', 0, 1),
         age_difference = ifelse(f_age_num > p_age_num, 'partner younger',
                                 ifelse(f_age_num == p_age_num, 'matched', 'partner older'))) %>% 
  select(bda, action, binary, focal, partner, stim_type, age_difference, second)

move_binary <- expand.grid(bda = c('before','during','after'),
                           action = unique(move$binary),
                           focal = unique(move$focal)#, partner = unique(move$partner)
                           ) %>% 
  mutate(focal = as.character(focal)#, partner = as.character(partner)
         ) %>% 
  #filter(focal != partner) %>% 
  # separate(focal, into = c('bull_focal','pb_focal'),
  #          remove = F, sep = '_') %>% 
  # separate(partner, into = c('bull_partner','pb_partner'),
  #          remove = F, sep = '_') %>% 
  # filter(pb_focal == pb_partner) %>% 
  # select(-pb_focal, -pb_partner, -bull_focal, -bull_partner) %>% 
  left_join(distinct(bda[,c('focal','stim_type','f_age_num')]), by = 'focal') %>% 
  #left_join(distinct(bda[,c('partner','p_age_num')]), by = 'partner') %>% 
  # mutate(age_difference = ifelse(f_age_num > p_age_num, 'partner younger',
  #                                ifelse(f_age_num == p_age_num, 'matched', 'partner older'))) %>% 
  mutate(count = NA,
         total = NA,
         propn = NA)

move_summary <- expand.grid(bda = c('before','during','after'),
                            action = unique(move$action),
                            focal = unique(move$focal),
                            partner = unique(move$partner)) %>% 
  mutate(focal = as.character(focal),
         partner = as.character(partner)) %>% 
  filter(focal != partner) %>% 
  filter(action != 'not_moving') %>% 
  separate(focal, into = c('bull_focal','pb_focal'),
           remove = F, sep = '_') %>% 
  separate(partner, into = c('bull_partner','pb_partner'),
           remove = F, sep = '_') %>% 
  filter(pb_focal == pb_partner) %>% 
  select(-pb_focal, -pb_partner, -bull_focal, -bull_partner) %>% 
  left_join(distinct(bda[,c('focal','stim_type','f_age_num')]), by = 'focal') %>% 
  left_join(distinct(bda[,c('partner','p_age_num')]), by = 'partner') %>% 
  mutate(age_difference = ifelse(f_age_num > p_age_num, 'partner younger',
                                 ifelse(f_age_num == p_age_num, 'matched', 'partner older'))) %>% 
  mutate(count = NA,
         total = NA,
         propn = NA)

## proportion moving vs not moving -- report in text
for(i in 1:nrow(move_binary)){
  move_binary$total[i] <- length(which(move$bda == move_binary$bda[i] &
                                         move$focal == move_binary$focal[i]))
  move_binary$count[i] <- length(which(move$bda == move_binary$bda[i] &
                                         move$focal == move_binary$focal[i] &
                                         move$binary == move_binary$action[i]))
}
move_binary <- move_binary %>%
  mutate(propn = count / total)
View(move_binary %>%
       mutate(percent = propn * 100) %>% 
       group_by(bda, action) %>%
       summarise(mean = mean(percent, na.rm = T),
                 stdv = sd(percent, na.rm = T)))

## no separation by stimulus or age -- report in text
move <- move %>% 
  filter(binary == 1) %>% 
  select(-binary)
for(i in 1:nrow(move_summary)){
  move_summary$total[i] <- length(which(move$bda == move_summary$bda[i] & 
                                          move$focal == move_summary$focal[i] & 
                                          move$partner == move_summary$partner[i]))
  move_summary$count[i] <- length(which(move$bda == move_summary$bda[i] & 
                                          move$action == move_summary$action[i] & 
                                          move$focal == move_summary$focal[i] & 
                                          move$partner == move_summary$partner[i]))
}
move_summary <- move_summary %>%
  mutate(propn = count / total)
View(move_summary %>%
       mutate(percent = propn * 100) %>% 
       group_by(bda, action) %>%
       summarise(mean = mean(percent, na.rm = T),
                 stdv = sd(percent, na.rm = T)))

## separate by stimulus and age -- report in table
move <- move %>% 
  filter(!is.na(age_difference))
move_summary <- move_summary %>% 
  filter(!is.na(age_difference))
for(i in 1:nrow(move_summary)){
  move_summary$total[i] <- length(which(move$bda == move_summary$bda[i] & 
                                          move$focal == move_summary$focal[i] & 
                                          move$partner == move_summary$partner[i] &
                                          move$stim_type == move_summary$stim_type[i]))
  move_summary$count[i] <- length(which(move$bda == move_summary$bda[i] & 
                                          move$action == move_summary$action[i] & 
                                          move$focal == move_summary$focal[i] & 
                                          move$partner == move_summary$partner[i] &
                                          move$stim_type == move_summary$stim_type[i]))
}
move_summary <- move_summary %>%
  mutate(propn = count / total)
View(move_summary %>%
       mutate(percent = propn * 100) %>% 
       group_by(bda, action, age_difference, stim_type) %>%
       summarise(mean = mean(percent, na.rm = T),
                 stdv = sd(percent, na.rm = T)))

## graph
move_summary %>%
  mutate(percent = propn * 100) %>% 
  group_by(bda, action, age_difference, stim_type) %>%
  summarise(mean = mean(percent, na.rm = T),
            stdv = sd(percent, na.rm = T)) %>% 
  ggplot()+
  geom_col(aes(x = bda, y = mean, fill = age_difference),
           position = 'dodge')+
  geom_errorbar(aes(x = bda, ymin = mean-stdv, ymax = mean+stdv, group = age_difference),
                position = 'dodge', width = 1)+
  facet_wrap(. ~ action)

## make wider so nicer to read as a table
move_wide <- move_summary %>%
  mutate(percent = propn * 100) %>% 
  group_by(bda, action, age_difference, stim_type) %>%
  summarise(mean = mean(percent, na.rm = T),
            stdv = sd(percent, na.rm = T)) %>% 
  pivot_wider(names_from = 'action', values_from = c('mean','stdv')) %>%
  rename(at_d_mean = `mean_approach directly`,
         at_a_mean = `mean_approach at an angle`,
         side_mean = `mean_move directly with`,
         away_a_mean = `mean_move away at an angle`,
         away_d_mean = `mean_move away directly`,
         at_d_stdv = `stdv_approach directly`,
         at_a_stdv = `stdv_approach at an angle`,
         side_stdv = `stdv_move directly with`,
         away_a_stdv = `stdv_move away at an angle`,
         away_d_stdv = `stdv_move away directly`) %>%
  mutate(move_towards_direct = paste0(round(at_d_mean,2),  ' ± ', round(at_d_stdv,2)),
         move_towards_angle  = paste0(round(at_a_mean,2),  ' ± ', round(at_a_stdv,2)),
         move_side           = paste0(round(side_mean,2),  ' ± ', round(side_stdv,2)),
         move_away_angle     = paste0(round(away_a_mean,2),' ± ', round(away_a_stdv,2)),
         move_away_direct    = paste0(round(away_d_mean,2),' ± ', round(away_d_stdv,2)))

# save outputs as files
write_prop(df = move_wide, stimulus = 'ctd', category = 'move')
write_prop(df = move_wide, stimulus = 'l', category = 'move')
write_prop(df = move_wide, stimulus = 'h', category = 'move')

# check outputs
head(read_csv('../outputs/proportions_move_ctd.csv'))

#### nearest neighbour ####
nn <- bda %>%
  filter(action != 'out_of_sight') %>% 
  filter(activity == 'nn') %>%
  filter(action == '1') %>% 
  mutate(age_difference = ifelse(f_age_num > p_age_num, 'partner younger',
                                 ifelse(f_age_num == p_age_num, 'matched', 'partner older'))) %>% 
  select(bda, age_difference, focal, stim_type, second) %>% 
  filter(!is.na(age_difference))

nn_summary <- expand.grid(bda = c('before','during','after'),
                          focal = unique(nn$focal),
                          age_difference = unique(nn$age_difference)) %>% 
  mutate(focal = as.character(focal)) %>% 
  left_join(distinct(bda[,c('focal','stim_type','f_age_num')]), by = 'focal') %>% 
  mutate(count = NA,
         total = NA,
         propn = NA)

## separate by stimulus -- report in table
for(i in 1:nrow(nn_summary)){
  nn_summary$total[i] <- length(which(nn$bda == nn_summary$bda[i] & 
                                        nn$focal == nn_summary$focal[i] & 
                                        nn$stim_type == nn_summary$stim_type[i]))
  nn_summary$count[i] <- length(which(nn$bda == nn_summary$bda[i] & 
                                        nn$age_difference == nn_summary$age_difference[i] & 
                                        nn$focal == nn_summary$focal[i] & 
                                        nn$stim_type == nn_summary$stim_type[i]))
}
nn_summary <- nn_summary %>%
  mutate(propn = count / total) %>% 
  mutate(percent = propn * 100)
View(nn_summary %>%
       group_by(bda, age_difference, stim_type) %>%
       summarise(mean = mean(percent, na.rm = T),
                 stdv = sd(percent, na.rm = T)))

## graph
nn_summary %>%
  group_by(bda, age_difference, stim_type) %>%
  summarise(mean = mean(percent, na.rm = T),
            stdv = sd(percent, na.rm = T)) %>% 
  ggplot()+
  geom_col(aes(x = bda, y = mean, fill = stim_type),
           position = 'dodge')+
  geom_errorbar(aes(x = bda, ymin = mean-stdv, ymax = mean+stdv, group = stim_type),
                position = 'dodge', width = 1)+
  facet_wrap(. ~ age_difference)

## make wider so nicer to read as a table
nn_wide <- nn_summary %>%
  group_by(bda, age_difference, stim_type) %>%
  summarise(mean = mean(percent, na.rm = T),
            stdv = sd(percent, na.rm = T)) %>% 
  pivot_wider(names_from = 'age_difference', values_from = c('mean','stdv')) %>%
  rename(young_mean = `mean_partner younger`,
         match_mean = `mean_matched`,
         older_mean = `mean_partner older`,
         young_stdv = `stdv_partner younger`,
         match_stdv = `stdv_matched`,
         older_stdv = `stdv_partner older`) %>%
  mutate(nn_young = paste0(round(young_mean,2),' ± ', round(young_stdv,2)),
         nn_match = paste0(round(match_mean,2),' ± ', round(match_stdv,2)),
         nn_older = paste0(round(older_mean,2),' ± ', round(older_stdv,2)))

# save outputs as files
write_prop(df = nn_wide, stimulus = 'ctd', category = 'nn')
write_prop(df = nn_wide, stimulus = 'l', category = 'nn')
write_prop(df = nn_wide, stimulus = 'h', category = 'nn')

# check outputs
head(read_csv('../outputs/proportions_nn_ctd.csv'))

print(paste0('proportional descriptions completed at ',Sys.time()))
