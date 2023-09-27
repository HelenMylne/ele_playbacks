#### information ####
# script for basic analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
library(tidyverse)

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

#### Bayesian multinomial regression to predict probability of looking towards/side-on/away based on effect of stimulus, time since stimulus and age on action ####
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
  select(pb_num,time,comment)
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
  select(pb_num, stim_start)

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
  left_join(distinct(ages[,c('pb_num','group_size','stim_type')]),
            by = 'pb_num') %>% 
  left_join(stim_starts, by = 'pb_num') %>% 
  mutate(time_since_stim = second - stim_start)

## bamlss method -- doesn't work with categorical exposure variables (or maybe it does if you use the right outcome variable? it doesn't like look_index )  ####
look_list <- list(
  direction_look ~ s(time_since_stim) #s(age_category),
  #~ s(partner_age_category),
  #~ s(stim_type)
)

## model
set.seed(123)
b <- bamlss(look_list, family = "multinomial", data = look)
summary(b)
######

# https://dagitty.net/dags.html?id=dw8twK






















