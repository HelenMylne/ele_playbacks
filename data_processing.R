## information ####
# script to process the raw data from the playback videos, converting the raw times into:
  # proportions of time spent per activity
  # latency to change activity/neighbour from start of stimulus

## set up ####
library(tidyverse)
library(cowsay)

theme_set(theme_classic())

# run for loop
animals_dont_work_on_windows <- c('anxiouscat', 'fish', 'grumpycat', 'longcat', 'longtailcat', 'mushroom', 'shortcat', 'signbunny', 'stretchycat')
animals <- names(cowsay::animals)
animals <- animals[! animals %in% animals_dont_work_on_windows ]
rm(animals_dont_work_on_windows) ; gc()

###### INITIAL DATA PROCESSING ####
## data import: stimuli ####
### playback number 1
# import data
file_list <- Sys.glob("../data_raw/stimuli/*.csv")
stim <- read_csv(file = file_list[1])
file <- colnames(stim)[2]
stim <- janitor::clean_names(stim)

# extract information about playback
row_num <- which(stim$observation_id == 'Time')
metadata <- stim[1:(row_num-2), 1:2] %>% 
  filter(is.na(observation_id) == FALSE) %>% 
  filter(observation_id != 'independent variables' & observation_id != 'variable' & observation_id != 'Player #1')
colnames(metadata) <- c('variable','value')
file_data <- file %>% as.data.frame()
file_data <- separate(data = file_data, col = ., sep = '_', into = c('pb','stim','date','time','data_type')) %>% 
  separate(col = pb, sep = 2, into = c('pb','pb_num'), remove = T) %>% 
  separate(col = stim, sep = 4, into = c('stim','stim_num'), remove = T) %>% 
  separate(col = time, sep = 2, into = c('hour','minute'), remove = T)
str(file_data)
file_data$pb_num <- as.numeric(file_data$pb_num)
file_data$stim_num <- as.numeric(file_data$stim_num)
file_data$date <- as.Date(file_data$date)

# combine information about playback
metadata$variable[which(metadata$variable == 'Media file(s)')] <- 'media'
metadata$variable[which(metadata$variable == 'Observation date')] <- 'date'
metadata$variable[which(metadata$variable == 'Description')] <- 'description'
metadata$variable[which(metadata$variable == 'Time offset (s)')] <- 'offset'
metadata$variable[which(metadata$variable == 'stimulus')] <- 'stim_type'
metadata <- rbind(metadata,
                  c('pb_num',file_data$pb_num[1]),
                  c('stim_num',file_data$stim_num[1]),
                  c('play_time',paste0(file_data$hour[1], ':', file_data$minute[1])))
#metadata$value[which(metadata$variable == 'date')] <- as.Date(file_data$date[1], format = '%yyyy-%mm-%dd')

# extract times from video
labels <- as.character(stim[row_num,])
stim <- stim[(row_num+1):nrow(stim),]
colnames(stim) <- labels
stim <- janitor::clean_names(stim)

# combine data frames
metadata <- pivot_wider(metadata, names_from = 'variable', values_from = 'value')
metadata$pb_num <- file_data$pb_num[1]
stim$pb_num <- file_data$pb_num[1]
df_stim <- left_join(x = stim, y = metadata, by = 'pb_num')

### loop to add in data for all other stimuli
for(i in 2:length(file_list)){
  # import data
  stim <- read_csv(file = file_list[i])
  file <- colnames(stim)[2]
  stim <- janitor::clean_names(stim)
  
  # extract information about playback
  row_num <- which(stim$observation_id == 'Time')
  metadata <- stim[1:(row_num-2), 1:2] %>% 
    filter(is.na(observation_id) == FALSE) %>% 
    filter(observation_id != 'independent variables' & observation_id != 'variable' & observation_id != 'Player #1')
  colnames(metadata) <- c('variable','value')
  file_data <- file %>% as.data.frame()
  file_data <- separate(data = file_data, col = ., sep = '_', into = c('pb','stimulus','date','time','data_type')) %>% 
    separate(col = pb, sep = 2, into = c('pb','pb_num'), remove = T) %>% 
    separate(col = stimulus, sep = 4, into = c('stim','stim_num'), remove = F) %>% 
    separate(col = time, sep = 2, into = c('hour','minute'), remove = T)
  str(file_data)
  file_data$pb_num <- as.numeric(file_data$pb_num)
  file_data$stim_num <- as.numeric(file_data$stim_num)
  file_data$date <- as.Date(file_data$date)
  
  # combine information about playback
  metadata$variable[which(metadata$variable == 'Media file(s)')] <- 'media'
  metadata$variable[which(metadata$variable == 'Observation date')] <- 'date'
  metadata$variable[which(metadata$variable == 'Description')] <- 'description'
  metadata$variable[which(metadata$variable == 'Time offset (s)')] <- 'offset'
  metadata$variable[which(metadata$variable == 'stimulus')] <- 'stim_type'
  metadata <- rbind(metadata,
                    c('pb_num',file_data$pb_num[1]),
                    c('stim_num', ifelse(is.na(file_data$stim_num[1]) == TRUE,
                                         file_data$stimulus[1], file_data$stim_num[1])),
                    c('play_time',paste0(file_data$hour[1], ':', file_data$minute[1])))
  #metadata$value[which(metadata$variable == 'date')] <- as.Date(file_data$date[1], format = '%yyyy-%mm-%dd')
  
  # extract times from video
  labels <- as.character(stim[row_num,])
  stim <- stim[(row_num+1):nrow(stim),]
  colnames(stim) <- labels
  stim <- janitor::clean_names(stim)
  
  # combine data frames
  metadata <- pivot_wider(metadata, names_from = 'variable', values_from = 'value')
  metadata$pb_num <- file_data$pb_num[1]
  stim$pb_num <- file_data$pb_num[1]
  df_new <- left_join(x = stim, y = metadata, by = 'pb_num')
  
  # add to previous data file
  df_stim <- rbind(df_stim, df_new)
  
}

saveRDS(df_stim, file = '../data_processed/stimuli.RDS')

rm(df_new, file_data, metadata, stim, file, file_list, i, labels, row_num) ; gc()

## data import: elephants ####
# import data
file_list <- Sys.glob("../data_raw/elephants/*.csv")
eles <- read_csv(file = file_list[1])
file <- colnames(eles)[2]
eles <- janitor::clean_names(eles)

# extract information about playback
row_num <- which(eles$observation_id == 'Time')
metadata <- eles[1:(row_num-2), 1:2] %>% 
  filter(is.na(observation_id) == FALSE) %>% 
  filter(observation_id != 'independent variables' & observation_id != 'variable' & observation_id != 'Player #1')
colnames(metadata) <- c('variable','value')
file_data <- file %>% as.data.frame()
file_data <- separate(data = file_data, col = ., sep = '_', into = c('pb','stimulus','date','time','bull','age')) %>% 
  separate(col = pb, sep = 2, into = c('pb','pb_num'), remove = T) %>% 
  separate(col = stimulus, sep = 4, into = c('stim','stim_num'), remove = F) %>% 
  separate(col = time, sep = 2, into = c('hour','minute'), remove = T) %>% 
  separate(col = bull, sep = 4, into = c('bull','bull_id'))
str(file_data)
file_data$pb_num <- as.numeric(file_data$pb_num)
file_data$stim_num <- as.numeric(file_data$stim_num)
file_data$date <- as.Date(file_data$date)
file_data$bull_id <- as.numeric(file_data$bull_id)

# combine information about playback
metadata$variable[which(metadata$variable == 'Media file(s)')] <- 'media'
metadata$variable[which(metadata$variable == 'Observation date')] <- 'date'
metadata$variable[which(metadata$variable == 'Description')] <- 'description'
metadata$variable[which(metadata$variable == 'Time offset (s)')] <- 'offset'
metadata$variable[which(metadata$variable == 'stimulus')] <- 'stim_type'
metadata <- rbind(metadata,
                  c('pb_num',file_data$pb_num[1]),
                  c('stim_num',ifelse(is.na(file_data$stim_num[1]) == TRUE,
                                      file_data$stimulus[1], file_data$stim_num[1])),
                  c('play_time',paste0(file_data$hour[1], ':', file_data$minute[1])),
                  c('bull',file_data$bull_id[1]),
                  c('age',file_data$age[1]))
#metadata$value[which(metadata$variable == 'date')] <- as.Date(file_data$date[1], format = '%yyyy-%mm-%dd')

# extract times from video
labels <- as.character(eles[row_num,])
eles <- eles[(row_num+1):nrow(eles),]
colnames(eles) <- labels
eles <- janitor::clean_names(eles)

# combine data frames
metadata <- pivot_wider(metadata, names_from = 'variable', values_from = 'value')
metadata$pb_num <- file_data$pb_num[1]
eles$pb_num <- file_data$pb_num[1]
df_eles <- left_join(x = eles, y = metadata, by = 'pb_num')

### loop to add in data for all other stimuli
for(i in 2:length(file_list)){
  # import data
  eles <- read_csv(file = file_list[i])
  file <- colnames(eles)[2]
  eles <- janitor::clean_names(eles)
  
  # extract information about playback
  row_num <- which(eles$observation_id == 'Time')
  metadata <- eles[1:(row_num-2), 1:2] %>% 
    filter(is.na(observation_id) == FALSE) %>% 
    filter(observation_id != 'independent variables' & observation_id != 'variable' & observation_id != 'Player #1')
  colnames(metadata) <- c('variable','value')
  file_data <- file %>% as.data.frame()
  file_data <- separate(data = file_data, col = ., sep = '_', into = c('pb','stimulus','date','time','bull','age')) %>% 
    separate(col = pb, sep = 2, into = c('pb','pb_num'), remove = T) %>% 
    separate(col = stimulus, sep = 4, into = c('stim','stim_num'), remove = F) %>% 
    separate(col = time, sep = 2, into = c('hour','minute'), remove = T) %>% 
    separate(col = bull, sep = 4, into = c('bull','bull_id'))
  str(file_data)
  file_data$pb_num <- as.numeric(file_data$pb_num)
  file_data$stim_num <- as.numeric(file_data$stim_num)
  file_data$date <- as.Date(file_data$date)
  file_data$bull_id <- as.numeric(file_data$bull_id)
  
  # combine information about playback
  metadata$variable[which(metadata$variable == 'Media file(s)')] <- 'media'
  metadata$variable[which(metadata$variable == 'Observation date')] <- 'date'
  metadata$variable[which(metadata$variable == 'Description')] <- 'description'
  metadata$variable[which(metadata$variable == 'Time offset (s)')] <- 'offset'
  metadata$variable[which(metadata$variable == 'stimulus')] <- 'stim_type'
  metadata <- rbind(metadata,
                    c('pb_num',file_data$pb_num[1]),
                    c('stim_num',ifelse(is.na(file_data$stim_num[1]) == TRUE,
                                        file_data$stimulus[1], file_data$stim_num[1])),
                    c('play_time',paste0(file_data$hour[1], ':', file_data$minute[1])),
                    c('bull',file_data$bull_id[1]),
                    c('age',file_data$age[1]))
  #metadata$value[which(metadata$variable == 'date')] <- as.Date(file_data$date[1], format = '%yyyy-%mm-%dd')
  
  # extract times from video
  labels <- as.character(eles[row_num,])
  eles <- eles[(row_num+1):nrow(eles),]
  colnames(eles) <- labels
  eles <- janitor::clean_names(eles)
  
  # combine data frames
  metadata <- pivot_wider(metadata, names_from = 'variable', values_from = 'value')
  metadata$pb_num <- file_data$pb_num[1]
  eles$pb_num <- file_data$pb_num[1]
  df_new <- left_join(x = eles, y = metadata, by = 'pb_num')
  
  # add to previous data file
  df_eles <- rbind(df_eles, df_new)
  
}

# simplify and sort structure
df_eles <- df_eles %>% 
  select(-media_file_path, -total_length, -fps, -media, -offset)
str(df_eles)
df_eles$time <- as.numeric(df_eles$time)
df_eles$group_size <- as.numeric(df_eles$group_size)

### categorise target of behaviour
df_eles$behavior <- ifelse(df_eles$behavior == 'nearest neighbour',
                           'nearest neighbour: nearest neighbour',
                           df_eles$behavior)
df_eles$behavior <- ifelse(df_eles$behavior == 'heasd toss' | df_eles$behavior == 'head toss',
                           'stress: head toss',
                           df_eles$behavior)
df_eles$behavior <- ifelse(df_eles$behavior == 'OUT OF SIGHT',
                           'OUT OF SIGHT: OUT OF SIGHT',
                           df_eles$behavior)
df_eles$behavior <- ifelse(df_eles$behavior == 'NEW VIDEO',
                           'NEW VIDEO: NEW VIDEO',
                           df_eles$behavior)
df_eles <- df_eles %>% separate(behavior, into = c('type','action'), remove = F, sep = ': ')
which(is.na(df_eles$behavioral_category) == FALSE)
df_eles$target <- ifelse(df_eles$type == 'tail', 'stress',
                         ifelse(df_eles$type == 'trunk','stress',
                                ifelse(df_eles$type == 'ears','stress',
                                       df_eles$type)))
unique(df_eles$target)
df_eles$target <- ifelse(df_eles$target == 'nearest neighbour', 'neighbour',
                         ifelse(df_eles$target == 'OUT OF SIGHT', NA,
                                ifelse(df_eles$target == 'NEW VIDEO', NA,
                                       df_eles$target)))

# categorise behaviour types
sort(unique(df_eles$action))
df_eles$behavioral_category <- case_when(df_eles$action == 'approach at an angle' ~ 'move',
                                         df_eles$action == 'approach directly' ~ 'move',
                                         df_eles$action == 'move away at an angle' ~ 'move',
                                         df_eles$action == 'move away directly' ~ 'move',
                                         df_eles$action == 'move directly with' ~ 'move',
                                         df_eles$action == 'move neither towards or away' ~ 'move',
                                         
                                         df_eles$action == 'side-on' ~ 'look',
                                         df_eles$action == 'look at directly' ~ 'look',
                                         df_eles$action == 'look directly away' ~ 'look',
                                         
                                         df_eles$action == 'nearest neighbour' ~ 'social',
                                         df_eles$action == 'physical contact elephant' ~ 'social',
                                         
                                         df_eles$action == 'down and ground-level sniff' ~ 'stress',
                                         df_eles$action == 'not sniffing (rest/feed/dust/stationary)' ~ 'stress',
                                         df_eles$action == 'tip not visible' ~ 'stress',
                                         df_eles$action == 'up and periscope sniffing' ~ 'stress',
                                         df_eles$action == 'flared' ~ 'stress',
                                         df_eles$action == 'head toss' ~ 'stress',
                                         df_eles$action == 'down' ~ 'stress',
                                         df_eles$action == 'up' ~ 'stress',
                                         df_eles$action == 'not visible' ~ 'stress',
                                         df_eles$action == 'relaxed (back or flapping regularly)' ~ 'stress')

# correct stim_types
table(df_eles$stim_num, df_eles$stim_type) # stim 14, 18, 20, 22, 26, and 30 have more than one type. stim13 is wrong.
df_eles$stim_type <- ifelse(df_eles$stim_num == 14 | df_eles$stim_num == 18 | df_eles$stim_num == 20, 'l',
                            ifelse(df_eles$stim_num == 22 | df_eles$stim_num == 26 | df_eles$stim_num == 30, 'h',
                                   df_eles$stim_type))
df_eles$stim_type <- ifelse(df_eles$stim_num == 13, 'l', df_eles$stim_type)

# save output
saveRDS(df_eles, file = '../data_processed/elephants.RDS')

# clean environment
rm(df_new, eles, file_data, metadata, file, file_list, i, labels, row_num) ; gc()

## prep for splitting into time before stimulus, time during stimulus and time after stimulus ####
# if an action starts before the stimulus does and finishes during it, take this as 2 actions, one before and one during, with the cut off being when the stimulus starts
#df_eles <- readRDS('../data_processed/elephants.RDS') ; df_stim <- readRDS('../data_processed/stimuli.RDS')

# obtain data frame of stimuli only -- ignore other noises for now
stimuli <- df_stim %>%
  filter(behavior == 'STIMULUS') %>% 
  #mutate() %>% 
  select(-fps, -subject, -behavioral_category, -media)
str(stimuli)
stimuli$time <- as.numeric(stimuli$time)

# start with basic starts and ends, can look for other anomalies later (e.g. remove points in the middle where no stimulus playing, remove parts where stimulus plays only through phone etc.)
x <- table(stimuli$pb_num) %>% # pb10, pb24, pb29, pb32, pb46, pb53 all have more than one start/end pair
  as.data.frame() %>% 
  rename(pb_num = Var1, freq = Freq) %>% 
  mutate(total = freq/2) %>% 
  filter(total > 1)
check <- stimuli %>% 
  filter(pb_num %in% x$pb_num)
stimuli <- anti_join(stimuli, check)
check2 <- check %>% 
  filter(pb_num != 10 | time < 440) %>% # pb10: on 14 s, 10 s off, on 1s -- remove 1 s stimulus
  filter(pb_num != 24 | comment != "STARTS PLAYING ONLY THROUGH PHONE NOT SPEAKER -- SHOULDN'T BE LOUD FOR ELEPHANTS") %>%  # pb24: discount first 1 s stimulus as not playing through speaker
  filter(pb_num != 29 & pb_num != 32 & pb_num != 53) %>%  # pb29 and 53: just take 1st start time and 2nd end time -- add back in afterwards. pb32: jsut take start and end time of whole actual stimulus, and also remove phone bit.
  filter(pb_num != 46 | comment != 'stim15a')

pb29 <- check[check$pb_num == 29,] # pb29: was making a noise for entire time, just that first couple of seconds were dodgy -- use full time and remove stop/start in middle
pb29 <- pb29[c(1,4),]

pb32 <- check[check$pb_num == 32 & is.na(check$comment),] # pb32: plays for < 2 s through phone so discount that as elephants shouldn't have heard it. breaks off for 7.5 s in middle but for now assume that plays throughout

pb53 <- check[check$pb_num == 53,] # pb53: plays louder and quieter (based on small camera) but never stops so just take start and end time
pb53 <- pb53[c(1,nrow(pb53)),]

# recombine to a single data frame
check <- rbind(check2, pb29, pb32, pb53)
stimuli <- rbind(stimuli, check)
rm(check, check2, pb29, pb32, pb53, x)

# create new data frame with pb_num, stim start time and stim stop time
stim <- data.frame(pb_num = sort(unique(stimuli$pb_num)),
                   stim_start = NA,
                   stim_stop = NA,
                   stim_duration = NA)
for(i in 1:nrow(stim)){
  x <- stimuli[stimuli$pb_num == stim$pb_num[i],]
  stim$stim_start[i] <- x$time[which(x$status == 'START')]
  stim$stim_stop[i] <- x$time[which(x$status == 'STOP')]
  stim$stim_duration[i] <- stim$stim_stop[i] - stim$stim_start[i]
}

# merge elephant actions with stimuli
df <- left_join(df_eles, stim, by = 'pb_num')
rm(df_stim, stim, stimuli, x, i) ; gc()

# add column for if line is before, during or after stimulus
df$bda <- ifelse(df$time < df$stim_start, 'before',
                 ifelse(df$time > df$stim_stop, 'after', 'during'))
table(df$bda) # this looks like a reasonable split

# breakdown to behavioural types to combine into pairs
df$activity_unique <- paste0(df$subject,'_',df$behavioral_category,'_',df$type)

# split off elephant-relative movements/looks as more likely to have matched times
df <- df[df$behavior != 'NEW VIDEO: NEW VIDEO',]
eles <- df[df$type == 'elephant' | df$type == 'nearest neighbour',] ; eles <- eles[!is.na(eles$time),]
df <- df[df$type != 'elephant',] ; df <- df[!is.na(df$time),]
unique(df$behavior)

# split off point actions as don't have a start/stop time
pt <- df[df$status == 'POINT',] ; pt <- pt[!is.na(pt$time),]
df <- df[df$status != 'POINT',] ; df <- df[!is.na(df$time),]

###### PROPORTIONS OF TIME PER ACTIVITY: VEHICLE AND SPEAKER ####
## split into time before/during/after stimulus ####
# create set of individual/behaviour breakdowns to go through
id_behav <- sort(unique(df$activity_unique))
N <- length(id_behav)

# prep data frame for rbind at end of loop, to put all of the levels back together
df <- df %>% 
  rename(behaviour = behavior) %>% 
  mutate(activity_id = rep(NA, nrow(df)),
         act_id = rep(NA, nrow(df)),
         action_unique = rep(NA, nrow(df))) %>% 
  select(subject, behaviour, time, bda, status, type, action, behavioral_category, target, comment, pb_num, stim_num, stim_type, stim_start, stim_stop, stim_duration, activity_id, act_id, action_unique, activity_unique, group_size, age, bull, date, play_time, description)

for(elephant_behaviour in 1:N){
  # select individual and behavioural set
  response <- df[df$activity_unique == id_behav[elephant_behaviour],] ; response <- response[is.na(response$time) == F,]
  df <- anti_join(df, response)
  
  # check in correct order
  response$order_correct <- NA
  response$order_correct[1] <- ifelse(response$time[1] < response$time[2], 'yes', 
                                      ifelse(response$time[1] == response$time[2], 'match', 'no'))
  if(nrow(response) > 2 ) {
    for(i in 2:(nrow(response)-1)){
      response$order_correct[i] <- ifelse(response$time[i] < response$time[i+1] &
                                            response$time[i] > response$time[i-1], 'yes', 
                                          ifelse(response$time[i] == response$time[i+1] |
                                                   response$time[i] == response$time[i-1], 'match',
                                                 'no'))
    }
  } else {
    response$order_correct <- ifelse(response$time[1] < response$time[2], 'yes', 'no')
  }
    
  if(length(which(response$order_correct == 'no')) > 0) {
    cowsay::say('STOP: ORDER INCORRECT', by = animals[runif(1,1,length(animals))] )
    response$order_correct*response$order_correct # will throw an error and halt operation if 'no' is an option
  }
  if(length(which(response$order_correct == 'match')) > 0) {
    matching_times <- response[response$order_correct == 'match',]
    matching_times <- matching_times[!is.na(matching_times$order_correct),]
    response <- anti_join(response,matching_times)
    matching_times$change <- as.integer(as.factor(matching_times$time))
    for(j in unique(matching_times$change)){
      pair <- matching_times[matching_times$change == j,]
      matching_times <- anti_join(matching_times, pair)
      pair <- pair[!is.na(pair$change),]
      pair$time[which(pair$status == 'STOP')] <- pair$time[which(pair$status == 'STOP')] - 0.001
      matching_times <- rbind(matching_times, pair)
    }
    matching_times <- matching_times %>%
      select(-change) %>% 
      arrange(time)
    response <- rbind(response, matching_times) %>% 
      arrange(time)
    rm(matching_times, pair)
  }
  
  # check pairs are together (should be if all in correct order!)
  response$pairs_together <- NA
  if(nrow(response) > 2 ) {
    for(i in 1:(nrow(response)-1)){
      response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                       ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
    }
  } else {
    response$pairs_together <- ifelse(response$status[1] == 'START' & response$status[2] == 'STOP', 'yes', 'no')
  }
  
  if(length(which(response$pairs_together == 'no')) > 0) {
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = animals[runif(1,1,length(animals))] )
    for(k in 1:(nrow(response)-1)){
      if(response$pairs_together[k] == 'no' & #response$pairs_together[k+1] == 'yes' &
         response$status[k] == 'START' & response$status[k+1] == 'START') {
        response$time[k+1] <- response$time[k+1]+0.001
      }
      if(response$pairs_together[k] == 'no' & #response$pairs_together[k+1] == 'yes' &
         response$status[k] == 'STOP' & response$status[k+1] == 'STOP') {
        response$time[k] <- response$time[k]-0.001
      }
    }
    response <- response %>% arrange(time)
    # repeat to ensure has now worked
    if(nrow(response) > 2 ) {
      for(i in 1:(nrow(response)-1)){
        response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                             ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
      }
    } else {
      response$pairs_together <- ifelse(response$status[1] == 'START' & response$status[2] == 'STOP', 'yes', 'no')
    }
    if(length(which(response$pairs_together == 'no')) > 0) {
      cowsay::say('STOP: PAIRS NOT TOGETHER', by = animals[runif(1,1,length(animals))] )
      response$pairs_together*response$pairs_together # auto fail out if still hasn't worked
    }
  }
  
  # add label per start/stop pair
  response$act_id <- rep(1:(nrow(response)/2), each = 2)
  response$action_unique <- paste0(response$act_id,'_',response$subject,'_',response$type,'_',response$action)
  
  # for pairs that span across multiple parts of experiment, create variable indicating which parts
  response$bda_split <- NA; for(i in 1:nrow(response)){
    x <- response[response$act_id == response$act_id[i],]
    response$bda_split[i] <- ifelse(x$bda[1] == x$bda[2], 'no',
                                ifelse(x$bda[1] == 'before',
                                       ifelse(x$bda[2] == 'during', 'bd','ba'),
                                       'da'))
  }
  rm(x)
  
  # single split pairs that span before-during or during-after. double split pairs that span before-after.
  if(length(which(response$bda_split != 'no') > 0)) {
    response_split <- response[response$bda_split != 'no',]
    response <- anti_join(response, response_split)
    if('bd' %in% response_split$bda_split) {
      bd <- response_split[response_split$bda_split == 'bd',]
      response_split <- anti_join(response_split, bd)
      bd <- rbind(bd, bd)
      bd$time[2] <- bd$stim_start[1]-0.001
      bd$time[3] <- bd$stim_start[1]
      response_split <- rbind(bd, response_split)
      rm(bd)
    }
    if('da' %in% response_split$bda_split) {
      da <- response_split[response_split$bda_split == 'da',]
      response_split <- anti_join(response_split, da)
      da <- rbind(da, da)
      da$time[2] <- da$stim_stop[1]
      da$time[3] <- da$stim_stop[1]+0.001
      response_split <- rbind(response_split, da)
      rm(da)
    }
    if('ba' %in% response_split$bda_split) {
      ba <- response_split[response_split$bda_split == 'ba',]
      response_split <- anti_join(response_split, ba)
      ba <- rbind(ba, ba, ba)
      ba$time[2] <- ba$stim_start[1]-0.001
      ba$time[3] <- ba$stim_start[1]
      ba$time[4] <- ba$stim_stop[1]
      ba$time[5] <- ba$stim_stop[1]+0.001
      response_split <- rbind(response_split, ba)
      rm(ba)
    } 
    
    # re-calculate which section each one goes in
    response_split$bda <- ifelse(response_split$time < response_split$stim_start, 'before',
                                 ifelse(response_split$time > response_split$stim_stop, 'after', 'during'))
  
    # recombine into individual elephant actions per category
    response <- rbind(response, response_split)
    
  }
    response <- response %>% 
      arrange(time) %>% 
      mutate(activity_id = as.numeric(as.factor(paste0(action_unique, bda)))) %>%
      select(-order_correct, -pairs_together, -bda_split)
  
  # recombine into full dataset
  df <- rbind(df, response)
  
  # clean up
  gc()
  
}

rm(response, response_split, elephant_behaviour, i, id_behav, j, k, N) ; gc()

saveRDS(df, '../data_processed/elephants_behaviour_split_relative_to_stimulus_speakervehicle.RDS')


## calculate length of action by subtracting START time from STOP time ####
# df <- readRDS('../data_processed/elephants_behaviour_split_relative_to_stimulus_speakervehicle.RDS')

# create a variable with a unique value for every action, also split across before/during/after
df$action_unique <- paste0(df$action_unique, '_', df$bda)
df$act_id <- as.integer(as.factor(df$action_unique))

# combine start and stop lines for every action so it is a single row per behaviour
start <- df %>% 
  filter(status == 'START') %>% 
  select(subject,behaviour,time,act_id) %>% 
  rename(start_time = time)
stop <- df %>% 
  filter(status == 'STOP') %>% 
  select(-subject, -behaviour, -status, -activity_unique, -activity_id) %>% 
  rename(stop_time = time)
behav <- left_join(start, stop, by = 'act_id') %>% 
  relocate(act_id)

# calculate duration of each behaviour
behav$duration <- behav$stop_time - behav$start_time

# neaten up
behav <- behav[,c(1:5,25,6:24)]

# save
saveRDS(behav, '../data_processed/elephants_behaviour_startstop_speakervehiclestress.RDS')

## calculate time per elephant that was in frame in each part of the experiment ####
# behav <- readRDS('../data_processed/elephants_behaviour_startstop_speakervehiclestress.RDS')

# separate out OUT OF SIGHT behaviours
unseen <- behav %>% filter(behaviour == 'OUT OF SIGHT: OUT OF SIGHT')
saveRDS(unseen, '../data_processed/elephants_time_out_of_sight.RDS')

# create data frame to calculate total possible time per experiment
playbacks <- data.frame(pb_num = rep(sort(unique(behav$pb_num)), each = 3),
                        section = rep(c('before','during','after'), length(unique(behav$pb_num))),
                        start_time = NA,
                        end_time = NA,
                        duration = NA)

# extract start and end times for each playback section
start <- df %>% filter(status == 'START') %>% 
  select(pb_num, time, bda) %>% distinct()
stop <- df %>% filter(status == 'STOP') %>% 
  select(pb_num, time, bda) %>% distinct()
for(i in 1:nrow(playbacks)){
  pb_start <- start %>% filter(pb_num == playbacks$pb_num[i] & bda == playbacks$section[i])
  pb_stop <- stop %>% filter(pb_num == playbacks$pb_num[i] & bda == playbacks$section[i])
  playbacks$start_time[i] <- min(pb_start$time)
  playbacks$end_time[i] <- max(pb_stop$time)
}

# calculate total duration of each experiment
playbacks$duration <- playbacks$end_time - playbacks$start_time

# create data frame containing total time each elephant is visible within each playback section
in_frame <- data.frame(subject = rep(sort(unique(behav$subject)), each = 3),
                       section = rep(c('before','during','after'), length(unique(behav$subject)))) %>% 
  separate(subject, into = c('bull','pb_num'), remove = F, sep = '_e') %>% 
  select(-bull) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>%
  left_join(playbacks, by = c('pb_num','section')) %>% 
  rename(video_duration = duration,
         video_start = start_time,
         video_end = end_time) %>% 
  mutate(out_frame_seconds = NA,
         in_frame_seconds = NA)
for( i in 1:nrow(in_frame) ){
  out_of_sight <- unseen %>%
    filter(subject == in_frame$subject[i]) %>% 
    filter(bda == in_frame$section[i])
  in_frame$out_frame_seconds[i] <- sum(out_of_sight$duration)
}
in_frame$in_frame_seconds <- in_frame$video_duration - in_frame$out_frame_seconds

# save for later
write_csv(in_frame, '../data_processed/elephants_time_in_frame.csv')

# clean up
rm(out_of_sight, pb_start, pb_stop, playbacks, start, stop, unseen, i) ; gc()

## calculate proportion of time spent per activity  ####
# create data frame showing amount of time spent on each behaviour by every elephant in each playback section
num_eles <- length(unique(behav$subject))
num_behav <- length(unique(behav$behaviour))
props <- data.frame(subject = rep(rep(sort(unique(behav$subject)), each = 3), each = num_behav),
                    section = rep(rep(c('before','during','after'), num_eles), each = num_behav),
                    behaviour = rep(sort(unique(behav$behaviour)), num_eles*3)#,
                    ) %>% 
  left_join(in_frame, by = c('subject','section')) %>% 
  rename() %>% 
  mutate(behav_seconds = NA,
         propn = NA) %>% 
  filter(behaviour != 'nearest neighbour: nearest neighbour') %>% 
  filter(behaviour != 'OUT OF SIGHT: OUT OF SIGHT')

# calculate time per elephant per playback section spent on each behaviour
for( i in 1:nrow(props) ) {
  elephant_behaviour <- behav %>%
    filter(subject == props$subject[i]) %>% 
    filter(bda == props$section[i]) %>% 
    filter(behaviour == props$behaviour[i])
  props$behav_seconds[i] <- ifelse(nrow(elephant_behaviour) == 0, 0, sum(elephant_behaviour$duration) )
}

# convert times to proportions so that duration in frame does not affect total
props$propn <- props$behav_seconds / props$in_frame_seconds

# add in additional information about each behaviour and elephant ages
props <- props %>% 
 left_join(distinct(behav[,c('behaviour', 'type', 'action', 'behavioral_category', 'target')]), by = 'behaviour') %>% 
 left_join(distinct(df_eles[,c('subject','age', 'stim_num', 'stim_type', 'group_size')]), by = 'subject')

# correct wrong age
props$age <- ifelse(props$age == '16-25', '21-25', props$age) # experiment 19, bull1 listed as 16-25, which is not a correct age

# save
saveRDS(props, '../data_processed/speaker_vehicle_stress_behaviour_proportions.RDS')

# clean up
rm(elephant_behaviour, in_frame, i, num_behav, num_eles) ; gc()

## remove "not visible" actions for tail/ears/trunk and shift so that relaxed/up options make up total time ####
#props <- readRDS('../data_processed/speaker_vehicle_stress_behaviour_proportions.RDS')

stress <- props %>% filter(target == 'stress')
nv <- stress %>% filter(action == 'not visible' | action == 'tip not visible')
stress <- anti_join(stress, nv)

ears_v <- stress %>% filter(type == 'ears') ; ears_nv <- nv %>% filter(type == 'ears')
tail_v <- stress %>% filter(type == 'tail') ; tail_nv <- nv %>% filter(type == 'tail')
trnk_v <- stress %>% filter(type == 'trunk'); trnk_nv <- nv %>% filter(type == 'trunk')

ears <- ears_nv %>%
  select(subject, section, behav_seconds) %>% 
  rename(ears_not_visible = behav_seconds) %>% 
  left_join(ears_v, by = c('subject','section'), multiple = 'all') %>% 
  mutate(ears_in_frame = in_frame_seconds - ears_not_visible) %>% 
  mutate(propn = behav_seconds / ears_in_frame) %>% 
  select(colnames(props))

tail <- tail_nv %>%
  select(subject, section, behav_seconds) %>% 
  rename(tail_not_visible = behav_seconds) %>% 
  left_join(tail_v, by = c('subject','section'), multiple = 'all') %>% 
  mutate(tail_in_frame = in_frame_seconds - tail_not_visible) %>% 
  mutate(propn = behav_seconds / tail_in_frame) %>% 
  select(colnames(props))

trnk <- trnk_nv %>%
  select(subject, section, behav_seconds) %>% 
  rename(trnk_not_visible = behav_seconds) %>% 
  left_join(trnk_v, by = c('subject','section'), multiple = 'all') %>% 
  mutate(trnk_in_frame = in_frame_seconds - trnk_not_visible) %>% 
  mutate(propn_new = behav_seconds / trnk_in_frame) %>% 
  select(colnames(props))

stress <- rbind(ears,tail,trnk)

props <- props %>% filter(target != 'stress') %>% 
  rbind(stress)

# save
saveRDS(props, '../data_processed/speaker_vehicle_stress_behaviour_proportions.RDS')

# clean up
rm(ears, ears_nv, ears_v, nv, stress, tail, tail_nv, tail_v, trnk, trnk_nv, trnk_v) ; gc()

## graph proportions ####
#props <- readRDS('../data_processed/speaker_vehicle_stress_behaviour_proportions.RDS')
props$section <- factor(props$section, levels = c('before','during','after'))
props$stimulus <- ifelse(props$stim_type == 'ctd', 'cape turtle dove (control)', 
                         ifelse(props$stim_type == 'h', 'human', 'lion')) %>% 
  factor(levels = c('cape turtle dove (control)','lion','human'))
props$age <- factor(props$age, levels = c('10-15','16-20','21-25','26-35','unkage'))

# looking direction relative to speaker and vehicle
look <- props %>% 
  filter(behavioral_category == 'look') %>% 
  mutate(looking_direction = factor(action, levels = c('look at directly', 'side-on', 'look directly away'))) %>% 
  filter(propn != 'NaN')
ggplot(data = look, aes(x = section, y = propn, group = subject))+
  geom_line(linewidth = 0.1, aes(colour = subject))+
  geom_point(size = 0.1, alpha = 0.4, aes(colour = subject))+
  facet_grid(looking_direction ~ target)+
  scale_y_continuous('proportion of time visible on camera spent looking at vehicle and speaker')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  theme(legend.position = 'none')
ggplot(data = look, aes(x = section, y = propn, group = subject))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(looking_direction ~ target)+
  scale_y_continuous('proportion of time visible on camera spent looking at vehicle and speaker')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')
look$behaviour <- factor(look$behaviour, levels = c('speaker: look at directly','speaker: side-on','speaker: look directly away',
                                                    'vehicle: look at directly','vehicle: side-on','vehicle: look directly away'))
ggplot(data = look, aes(x = section, y = propn, group = subject))+
  geom_line(linewidth = 0.2, aes(colour = age))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = age))+
  facet_grid(stimulus ~ behaviour)+
  scale_y_continuous('proportion of time visible on camera spent looking at vehicle and speaker')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')
ggplot(data = look, aes(x = section, y = propn, group = subject))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ behaviour)+
  scale_y_continuous('proportion of time visible on camera spent looking at vehicle and speaker')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')

# movements relative to speaker and vehicle
move <- props %>% 
  filter(behavioral_category == 'move') %>% 
  mutate(looking_direction = factor(action, levels = c('approach directly', 'approach at an angle', 'move neither towards or away', 'move away at an angle', 'move away directly'))) %>% 
  filter(propn != 'NaN')
ggplot(data = move, aes(x = section, y = propn, group = subject))+
  geom_line(linewidth = 0.1, aes(colour = subject))+
  geom_point(size = 0.1, alpha = 0.4, aes(colour = subject))+
  facet_grid(looking_direction ~ target)+
  scale_y_continuous('proportion of time visible on camera spent moving relative to vehicle and speaker')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  theme(legend.position = 'none')
ggplot(data = move, aes(x = section, y = propn, group = subject))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(looking_direction ~ target)+
  scale_y_continuous('proportion of time visible on camera spent moving relative to vehicle and speaker')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')

# trunk movements
trunk <- props %>% 
  filter(type == 'trunk') %>% 
  mutate(action_trunk = ifelse(action == 'not sniffing (rest/feed/dust/stationary)', 'relaxed',
                               ifelse(action == 'up and periscope sniffing', 'periscope sniff', 'ground-level sniff'))) %>% 
  mutate(action_trunk = factor(action_trunk, levels = c('relaxed', 'ground-level sniff', 'periscope sniff'))) %>% 
  filter(propn != 'NaN')
ggplot(data = trunk, aes(x = section, y = propn, group = subject))+
  geom_jitter(alpha = 0.4, aes(colour = stimulus), width = 0.3)+
  facet_grid(stimulus ~ action_trunk)+
  scale_y_continuous('proportion of time visible on camera spent using trunk')+
  scale_x_discrete(expand = c(0.2,0.2), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')

# add a grouped and stacked bar plot with 9 bars, or 3 sets of 3 stacks: before/during/after and dove/lion/human, each one split into colours by proportion relaxed vs GL-sniff vs P-sniff -- DON'T THINK THIS IS ACTUALLY RIGHT, BUT ALSO DON'T THINK IT'S FAR OFF
trunk_props <- trunk %>% select(section, action_trunk) %>% distinct()
trunk_props$propn <- NA
for(i in 1:nrow(trunk_props)){
  x <- trunk %>% filter(section == trunk_props$section[i] &
                          action_trunk == trunk_props$action[i])
  trunk_props$propn[i] <- sum(x$propn)
}
trunk_props$propn_new <- NA
for(bda in c('before','during','after')){
  x <- trunk_props %>% filter(section == bda)
  x$propn_new <- x$propn / sum(x$propn)
  trunk_props$propn_new[which(trunk_props$section == bda)] <- x$propn_new
}
ggplot(data = trunk_props, aes(x = section, y = propn_new))+
  geom_col(aes(fill = action_trunk))+
  #facet_grid(stimulus ~ .)+
  scale_y_continuous('proportion of time visible on camera spent using trunk')+
  scale_x_discrete(expand = c(0.2,0.2), 'time relative to stimulus')+
  scale_fill_viridis_d(direction = -1)+
  labs(fill = '')+
  theme(legend.position = 'bottom')

# ear movements
ears <- props %>% 
  filter(type == 'ears') %>% 
  mutate(action_ears = ifelse(action == 'relaxed (back or flapping regularly)', 'relaxed', action)) %>% 
  mutate(action_ears = factor(action_ears, levels = c('relaxed', 'flared', 'not visible'))) %>% 
  filter(propn != 'NaN')
ggplot(data = ears, aes(x = section, y = propn, group = subject))+
  geom_jitter(alpha = 0.4, aes(colour = stimulus), width = 0.3)+
  facet_grid(stimulus ~ action_ears)+
  scale_y_continuous('proportion of time visible on camera spent showing stress with ears and head position')+
  scale_x_discrete(expand = c(0.2,0.2), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')
# add a grouped and stacked bar plot with 9 bars, or 3 sets of 3 stacks: before/during/after and dove/lion/human, each one split into colours by proportion relaxed vs flared

# tail position
tail <- props %>% 
  filter(type == 'tail') %>% 
  mutate(action_tail = factor(action, levels = c('down', 'up', 'not visible'))) %>% 
  filter(propn != 'NaN') %>% 
  filter(action != 'not visible') # STILL NEED TO REMOVE THIS FROM TOTAL TIME SO THAT RELAXED VS FLARED IS THE TOTAL 100% OF THE TIME
ggplot(data = tail, aes(x = section, y = propn, group = subject))+
  geom_jitter(alpha = 0.4, aes(colour = stimulus), width = 0.3)+
  facet_grid(stimulus ~ action_tail)+
  scale_y_continuous('proportion of time visible on camera spent showing stress in tail position')+
  scale_x_discrete(expand = c(0.2,0.2), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')
# add a grouped and stacked bar plot with 9 bars, or 3 sets of 3 stacks: before/during/after and dove/lion/human, each one split into colours by proportion relaxed vs up

###### PROPORTIONS OF TIME PER ACTIVITY: ELEPHANTS ####
## split into time before/during/after stimulus ####
# split into separate elephants it's relative to
eles$comment <- ifelse(eles$comment == 'b2_right; b3_right', 'b2_right;b3_right',
                       ifelse(eles$comment == 'b3;b4;5b;b6;b7', 'b3;b4;b5;b6;b7',
                              ifelse(eles$comment == 'b1;b2b4', 'b1;b2;b4',
                                     ifelse(eles$comment == 'b1;b2;b3:b4', 'b1;b2;b3;b4',
                                            eles$comment))))

eles_long <- eles %>% 
  separate(comment, into = c('ele1','ele2','ele3','ele4','ele5','ele6','ele7','ele8'), sep = ';', remove = F) %>% 
  pivot_longer(cols = c('ele1','ele2','ele3','ele4','ele5','ele6','ele7','ele8'),
               names_to = 'ele_num', values_to = 'elephant_looking_direction') %>% 
  filter(is.na(elephant_looking_direction) == F) %>% 
  select(-ele_num)

# filter out looking directions from neighbours
neighbour <- eles_long %>% filter(target == 'neighbour')
elephants <- eles_long %>% filter(target == 'elephant')

# separate out left and right for side-on
sort(unique(elephants$elephant_looking_direction))
elephants$elephant_looking_direction <- ifelse(elephants$elephant_looking_direction == 'backs_away_from_b4','backs away from b4',
                                        ifelse(elephants$elephant_looking_direction == "displaced_by_b2","displaced by b2",
                                        ifelse(elephants$elephant_looking_direction == "displaces_b1","displaces b1",
                                        ifelse(elephants$elephant_looking_direction == "mock_charge_vehicle","mock charge vehicle",
                                        ifelse(elephants$elephant_looking_direction == "no_obvious_stimulus","no obvious stimulus",
                                        ifelse(elephants$elephant_looking_direction == "pushed_by_b2","pushed by b2",
                                        ifelse(elephants$elephant_looking_direction == "reverse_away","reverse away",
                                        ifelse(elephants$elephant_looking_direction == "reverse_towards","reverse towards"
                                        ,ifelse(elephants$elephant_looking_direction == "very_slow","very slow",
                                        ifelse(elephants$elephant_looking_direction == "very_slowly","very slow",
                                               elephants$elephant_looking_direction))))))))))
elephants <- elephants %>% 
  rename(elephant_comment = elephant_looking_direction,
         behaviour = behavior) %>% 
  separate(elephant_comment, into = c('elephant_look','elephant_side'), sep = '_', remove = F)
other_comments <- elephants %>% 
  filter(elephant_look != 'b1') %>% filter(elephant_look != 'b2') %>% 
  filter(elephant_look != 'b3') %>% filter(elephant_look != 'b4') %>% 
  filter(elephant_look != 'b5') %>% filter(elephant_look != 'b6') %>% 
  filter(elephant_look != 'b7') %>% filter(elephant_look != 'b8')
sort(unique(elephants$elephant_look))
elephants <- elephants %>% 
  filter(elephant_look == 'b1' | elephant_look == 'b2' | elephant_look == 'b3' | elephant_look == 'b4' |
           elephant_look == 'b5' | elephant_look == 'b6' | elephant_look == 'b7' | elephant_look == 'b8') %>% 
  mutate(remove_pb18_b7 = paste0(pb_num,elephant_look)) %>% 
  filter(remove_pb18_b7 != '18b7') %>% 
  select(-remove_pb18_b7)

# create set of individual/behaviour breakdowns to go through
sort(unique(elephants$elephant_look))
elephants$elephant_look_unique <- paste0(elephants$subject,'_',elephants$elephant_look,'_', elephants$behavioral_category)
id_behav <- sort(unique(elephants$elephant_look_unique))
N <- length(id_behav)

# prep data frame for rbind at end of loop, to put all of the levels back together
elephants <- elephants %>% 
  mutate(activity_id = rep(NA, nrow(elephants)),
         act_id = rep(NA, nrow(elephants)),
         action_unique = rep(NA, nrow(elephants))) %>% 
  select(subject, behaviour, time, bda, status, type, action, behavioral_category, target, elephant_look, elephant_look_unique, comment, pb_num, stim_num, stim_type, stim_start, stim_stop, stim_duration, activity_id, act_id, action_unique, activity_unique, group_size, age, bull, date, play_time, description)

# set up list of ones which fail to come back to in BORIS
correct_in_boris <- as.data.frame(id_behav) %>% 
  separate(id_behav, into = c('bull', 'pb_num','target','type'), remove = F) %>% 
  mutate(check = NA, time_fails = NA)

# run for loop
for(elephant_behaviour in 1:N){
  # select individual and behavioural set
  response <- elephants[elephants$elephant_look_unique == id_behav[elephant_behaviour],]
  response <- response[is.na(response$time) == F,]
  elephants <- anti_join(elephants, response)
  
  # check in correct order
  response$order_correct <- NA
  response$order_correct[1] <- ifelse(response$time[1] < response$time[2], 'yes', 
                                      ifelse(response$time[1] == response$time[2], 'match', 'no'))
  if(nrow(response) > 2 ) {
    for(i in 2:(nrow(response)-1)){
      response$order_correct[i] <- ifelse(response$time[i] < response$time[i+1] &
                                            response$time[i] > response$time[i-1], 'yes', 
                                          ifelse(response$time[i] == response$time[i+1] |
                                                   response$time[i] == response$time[i-1], 'match',
                                                 'no'))
    }
  } else {
    response$order_correct <- ifelse(response$time[1] < response$time[2], 'yes', 'no')
  }
    
  if(length(which(response$order_correct == 'no')) > 0) {
    cowsay::say('STOP: ORDER INCORRECT', by = animals[runif(1,1,length(animals))] )
    correct_in_boris$time_fails[which(correct_in_boris$id_behav == id_behav[elephant_behaviour])] <- list(response$time[which(response$order_correct == 'no')])
  }
  if(length(which(response$order_correct == 'match')) > 0) {
    matching_times <- response[response$order_correct == 'match',]
    matching_times <- matching_times[!is.na(matching_times$order_correct),]
    response <- anti_join(response,matching_times)
    matching_times$change <- as.integer(as.factor(matching_times$time))
    for(j in unique(matching_times$change)){
      pair <- matching_times[matching_times$change == j,]
      matching_times <- anti_join(matching_times, pair)
      pair <- pair[!is.na(pair$change),]
      pair$time[which(pair$status == 'STOP')] <- pair$time[which(pair$status == 'STOP')] - 0.001
      matching_times <- rbind(matching_times, pair)
    }
    matching_times <- matching_times %>%
      select(-change) %>% 
      arrange(time)
    response <- rbind(response, matching_times) %>% 
      arrange(time)
    rm(matching_times, pair)
  }
  
  # check pairs are together (should be if all in correct order!)
  response$pairs_together <- NA
  if(nrow(response) > 2 ) {
    for(i in 1:(nrow(response)-1)){
      response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                       ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
    }
  } else {
    response$pairs_together <- ifelse(response$status[1] == 'START' & response$status[2] == 'STOP', 'yes', 'no')
  }
  
  if(length(which(response$pairs_together == 'no')) > 0) {
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = animals[runif(1,1,length(animals))] )
    for(k in 1:(nrow(response)-1)){
      if(response$pairs_together[k] == 'no' & #response$pairs_together[k+1] == 'yes' &
         response$status[k] == 'START' & response$status[k+1] == 'START') {
        response$time[k+1] <- response$time[k+1]+0.001
      }
      if(response$pairs_together[k] == 'no' & #response$pairs_together[k+1] == 'yes' &
         response$status[k] == 'STOP' & response$status[k+1] == 'STOP') {
        response$time[k] <- response$time[k]-0.001
      }
    }
    response <- response %>% arrange(time)
    # repeat to ensure has now worked
    if(nrow(response) > 2 ) {
      for(i in 1:(nrow(response)-1)){
        response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                             ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
      }
    } else {
      response$pairs_together <- ifelse(response$status[1] == 'START' & response$status[2] == 'STOP', 'yes', 'no')
    }
    if(length(which(response$pairs_together == 'no')) > 0) {
      cowsay::say('STOP: PAIRS NOT TOGETHER', by = animals[runif(1,1,length(animals))] )
      correct_in_boris$time_fails[which(correct_in_boris$id_behav == id_behav[elephant_behaviour])] <- list(response$time[which(response$pairs_together == 'no')])
    }
  }
  
  # add label per start/stop pair
  response$act_id <- rep(1:(nrow(response)/2), each = 2)
  response$action_unique <- paste0(response$act_id,'_',response$subject,'_',response$type,'_',response$action)
  
  # for pairs that span across multiple parts of experiment, create variable indicating which parts
  response$bda_split <- NA; for(i in 1:nrow(response)){
    x <- response[response$act_id == response$act_id[i],]
    response$bda_split[i] <- ifelse(x$bda[1] == x$bda[2], 'no',
                                ifelse(x$bda[1] == 'before',
                                       ifelse(x$bda[2] == 'during', 'bd','ba'),
                                       'da'))
  }
  rm(x)
  
  # single split pairs that span before-during or during-after. double split pairs that span before-after.
  if(length(which(response$bda_split != 'no') > 0)) {
    response_split <- response[response$bda_split != 'no',]
    response <- anti_join(response, response_split)
    if('bd' %in% response_split$bda_split) {
      bd <- response_split[response_split$bda_split == 'bd',]
      response_split <- anti_join(response_split, bd)
      bd <- rbind(bd, bd)
      bd$time[2] <- bd$stim_start[1]-0.001
      bd$time[3] <- bd$stim_start[1]
      response_split <- rbind(bd, response_split)
      rm(bd)
    }
    if('da' %in% response_split$bda_split) {
      da <- response_split[response_split$bda_split == 'da',]
      response_split <- anti_join(response_split, da)
      da <- rbind(da, da)
      da$time[2] <- da$stim_stop[1]
      da$time[3] <- da$stim_stop[1]+0.001
      response_split <- rbind(response_split, da)
      rm(da)
    }
    if('ba' %in% response_split$bda_split) {
      ba <- response_split[response_split$bda_split == 'ba',]
      response_split <- anti_join(response_split, ba)
      ba <- rbind(ba, ba, ba)
      ba$time[2] <- ba$stim_start[1]-0.001
      ba$time[3] <- ba$stim_start[1]
      ba$time[4] <- ba$stim_stop[1]
      ba$time[5] <- ba$stim_stop[1]+0.001
      response_split <- rbind(response_split, ba)
      rm(ba)
    } 
    
    # re-calculate which section each one goes in
    response_split$bda <- ifelse(response_split$time < response_split$stim_start, 'before',
                                 ifelse(response_split$time > response_split$stim_stop, 'after', 'during'))
  
    # recombine into individual elephant actions per category
    response <- rbind(response, response_split)
    
  }
    response <- response %>% 
      arrange(time) %>% 
      mutate(activity_id = as.numeric(as.factor(paste0(action_unique, bda)))) %>%
      select(-order_correct, -pairs_together, -bda_split)
  
  # recombine into full dataset
  elephants <- rbind(elephants, response)
  
  # clean up
  gc()
  
}

saveRDS(elephants, '../data_processed/elephants_behaviour_split_relative_to_stimulus_elephantdirections.RDS')

# ones to go back and check on
correct_in_boris <- correct_in_boris %>% filter(time_fails != 'NA')
nrow(correct_in_boris)

# clean up
rm(correct_in_boris, response, response_split, elephant_behaviour, i, id_behav, j, k, N) ; gc()

## calculate length of action by subtracting START time from STOP time ####
# elephants <- readRDS('../data_processed/elephants_behaviour_split_relative_to_stimulus_elephantdirections.RDS')
# create a variable with a unique value for every action, also split across before/during/after
elephants$action_unique <- paste0(elephants$action_unique, '_', elephants$bda,'_',elephants$elephant_look)
elephants$act_id <- as.integer(as.factor(elephants$action_unique))

# combine start and stop lines for every action so it is a single row per behaviour
start <- elephants %>% 
  filter(status == 'START') %>% 
  select(subject,behaviour,time,act_id) %>% 
  rename(start_time = time)
stop <- elephants %>% 
  filter(status == 'STOP') %>% 
  select(-subject, -behaviour, -status, -activity_unique, -activity_id) %>% 
  rename(stop_time = time)
behav <- left_join(start, stop, by = 'act_id') %>% 
  relocate(act_id)

# calculate duration of each behaviour
behav$duration <- behav$stop_time - behav$start_time

# neaten up
behav <- behav[,c(1:5,27,6:26)]

# save
saveRDS(behav, '../data_processed/elephants_behaviour_startstop_elephants.RData')

## calculate proportion of time spent per activity ####
# read in time in frame per elephant
in_frame <- read_csv('../data_processed/elephants_time_in_frame.csv')
#df_eles <- readRDS('../data_processed/elephants.RDS')

# create data frame showing amount of time spent on each behaviour by every elephant in each playback section
num_eles <- length(unique(behav$subject))
num_behav <- length(unique(behav$behaviour))
props <- data.frame(subject = rep(rep(rep(sort(unique(behav$subject)),
                                          each = 8),
                                      each = num_behav),
                                  each = 3),
                    dyad_partner = rep(rep(rep(c('b1','b2','b3','b4','b5','b6','b7','b8'),
                                               num_eles),
                                           each = num_behav),
                                       each = 3),
                    behaviour = rep(rep(rep(sort(unique(behav$behaviour)),
                                            num_eles),
                                        8),
                                    each = 3),
                    section = rep(rep(rep(c('before','during','after'),
                                          8),
                                      num_eles),
                                  num_behav)) %>% 
  separate(subject, into = c('focal','pb_num'), sep = '_e', remove = F) %>% 
  filter(focal != dyad_partner) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  mutate(targeted_elephant = paste0(dyad_partner, '_e', pb_num)) %>% 
  filter(targeted_elephant %in% behav$subject) %>% 
  left_join(in_frame, by = c('subject','pb_num','section')) %>% 
  mutate(behav_seconds = NA,
         propn = NA)

# calculate time per elephant per playback section spent on each behaviour
for( i in 1:nrow(props) ) {
  elephant_behaviour <- behav %>%
    filter(subject == props$subject[i]) %>% 
    filter(elephant_look == props$dyad_partner[i]) %>% 
    filter(bda == props$section[i]) %>% 
    filter(behaviour == props$behaviour[i])
  props$behav_seconds[i] <- ifelse(nrow(elephant_behaviour) == 0, 0, sum(elephant_behaviour$duration) )
}

# convert times to proportions so that duration in frame does not affect total
props$propn <- props$behav_seconds / props$in_frame_seconds

# add in additional information about each behaviour and elephant ages
props <- props %>% 
  left_join(distinct(behav[,c('behaviour', 'action', 'behavioral_category')]), by = 'behaviour') %>% 
  left_join(distinct(df_eles[,c('subject','age', 'stim_num', 'stim_type', 'group_size')]), by = 'subject')
partner_info <- df_eles %>% 
  select(subject, age) %>% 
  distinct() %>% 
  rename(targeted_elephant = subject,
         partner_age = age) %>% 
  mutate(partner_age = ifelse(targeted_elephant == 'b1_e19', '21-25',
                              partner_age))
props <- props %>% 
  left_join(partner_info, by = 'targeted_elephant')

# correct wrong age
props$age <- ifelse(props$age == '16-25', '21-25', props$age) # experiment 19, bull1 listed as 16-25, which is not a correct age

# age differences
props$age_category <- ifelse(props$age == '10-15', 1,
                             ifelse(props$age == '16-20', 2,
                                    ifelse(props$age == '21-25', 3,
                                           ifelse(props$age == '26-35', 4, NA))))
props$partner_age_category <- ifelse(props$partner_age == '10-15', 1,
                                     ifelse(props$partner_age == '16-20', 2,
                                            ifelse(props$partner_age == '21-25', 3,
                                                   ifelse(props$partner_age == '26-35', 4, NA))))
props$age_difference <- ifelse(props$age_category == props$partner_age_category, 'matched',
                               ifelse(props$age_category > props$partner_age_category,
                                      'partner younger', 'partner older'))

# save
saveRDS(props, '../data_processed/elephant_behaviour_proportions.RDS')

## graph proportions ####
#props <- readRDS('../data_processed/elephant_behaviour_proportions.RDS')
props$section <- factor(props$section, levels = c('before','during','after'))
props$stimulus <- ifelse(props$stim_type == 'ctd', 'cape turtle dove (control)', 
                         ifelse(props$stim_type == 'h', 'human', 'lion')) %>% 
  factor(levels = c('cape turtle dove (control)','lion','human'))
props$age <- factor(props$age, levels = c('10-15','16-20','21-25','26-35','unkage'))
props$partner_age <- factor(props$partner_age, levels = c('10-15','16-20','21-25','26-35','unkage'))
props$age_difference <- factor(props$age_difference,
                               levels = c('partner younger','matched','partner older'))
props$dyad <- paste0('e',props$pb_num,'_', props$focal,'_', props$dyad_partner)

# looking direction relative to elephants
look <- props %>% 
  filter(behavioral_category == 'look') %>% 
  mutate(looking_direction = factor(action, levels = c('look at directly', 'side-on', 'look directly away'))) %>% 
  filter(propn != 'NaN')

ggplot(data = look, aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(looking_direction ~ age_difference)+
  scale_y_continuous('proportion of time visible on camera spent looking at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')

ggplot(data = look[look$action == 'look at directly',],
       aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
    facet_grid(age ~ partner_age)+  # columns = partner age, rows = focal age
  scale_y_continuous('proportion of time visible on camera spent looking at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'look directly at other elephants\n(rows = focal age, columns = partner age)')

ggplot(data = look[look$action == 'side-on',],
       aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+
  scale_y_continuous('proportion of time visible on camera spent looking at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'side on to other elephants\n(rows = focal age, columns = partner age)')

look %>% 
  filter(action == 'look directly away') %>% 
  #filter(age_category != 'unkage') %>% 
  #filter(partner_age_category != 'unkage') %>% 
  ggplot(aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+
  scale_y_continuous('proportion of time visible on camera spent looking at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'look directly away from other elephants\n(rows = focal age, columns = partner age)')

# movements relative to elephants
move <- props %>% 
  filter(behavioral_category == 'move') %>% 
  mutate(looking_direction = factor(action, levels = c('approach directly', 'approach at an angle', 'move neither towards or away', 'move away at an angle', 'move away directly'))) %>% 
  filter(propn != 'NaN')

ggplot(data = move, aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(looking_direction ~ age_difference)+
  scale_y_continuous('proportion of time visible on camera spent moving at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')

ggplot(data = move[move$action == 'approach directly',],
       aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+  # columns = partner age, rows = focal age
  scale_y_continuous('proportion of time visible on camera spent moving at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'move directly towards other elephants\n(rows = focal age, columns = partner age)')

ggplot(data = move[move$action == 'approach at an angle',],
       aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+
  scale_y_continuous('proportion of time visible on camera spent moving at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'move indirectly towards other elephants\n(rows = focal age, columns = partner age)')

ggplot(data = move[move$action == 'move away at an angle',],
       aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+
  scale_y_continuous('proportion of time visible on camera spent moving at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'move indirectly away from other elephants\n(rows = focal age, columns = partner age)')

move %>% 
  filter(action == 'move away directly') %>% 
  filter(age_category != 'unkage') %>% 
  filter(partner_age_category != 'unkage') %>% 
  ggplot(aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+
  scale_y_continuous('proportion of time visible on camera spent moving at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'move directly away from other elephants\n(rows = focal age, columns = partner age)')

###### PROPORTIONS OF TIME PER ACTIVITY: NEAREST NEIGHBOUR ######
## split into time before/during/after stimulus ####
neighbour <- eles_long %>% 
  filter(target == 'neighbour') %>% 
  select(-elephant_looking_direction) %>% 
  distinct()
# filter out looking directions from neighbours
rm(df, df_eles, eles, pt) ; gc()
str(neighbour)
sort(unique(neighbour$comment))

# standardise
neighbour <- neighbour %>% 
  separate(comment, into = c('neighbour','other'), remove = F, sep = ';') %>% 
  mutate(neighbour = ifelse(neighbour == 'b1' | neighbour == 'b2' | 
                              neighbour == 'b3' | neighbour == 'b4' | 
                              neighbour == 'b5' | neighbour == 'b6' | 
                              neighbour == 'b7' | neighbour == 'b8',
                            neighbour, 'unknown'))

# create set of individual/behaviour breakdowns to go through
neighbour$elephant_neighbour_unique <- paste0(neighbour$subject,'_',neighbour$neighbour)
id_behav <- sort(unique(neighbour$elephant_neighbour_unique))
N <- length(id_behav)

# prep data frame for rbind at end of loop, to put all of the levels back together
neighbour <- neighbour %>% 
  rename(behaviour = behavior) %>% 
  mutate(activity_id = NA,
         act_id = NA,
         action_unique = NA)

# set up list of ones which fail to come back to in BORIS
correct_in_boris <- as.data.frame(id_behav) %>% 
  separate(id_behav, into = c('bull', 'pb_num','target'), remove = F) %>% 
  mutate(check = NA, time_fails = NA)

# run for loop
for(elephant_behaviour in 1:N){ # REMEMBER TO UNCOMMENT ANTI-JOIN ON LINE 1347 BEFORE RUNNING FOR REAL
  # select individual and behavioural set
  response <- neighbour[neighbour$elephant_neighbour_unique == id_behav[elephant_behaviour],]
  response <- response[is.na(response$time) == F,]
  neighbour <- anti_join(neighbour, response)
  
  # check in correct order
  response$order_correct <- NA
  response$order_correct[1] <- ifelse(response$time[1] < response$time[2], 'yes', 
                                      ifelse(response$time[1] == response$time[2], 'match', 'no'))
  if(nrow(response) > 2 ) {
    for(i in 2:(nrow(response)-1)){
      response$order_correct[i] <- ifelse(response$time[i] < response$time[i+1] &
                                            response$time[i] > response$time[i-1], 'yes', 
                                          ifelse(response$time[i] == response$time[i+1] |
                                                   response$time[i] == response$time[i-1], 'match',
                                                 'no'))
    }
  } else {
    response$order_correct <- ifelse(response$time[1] < response$time[2], 'yes', 'no')
  }
  
  if(length(which(response$order_correct == 'no')) > 0) {
    cowsay::say('STOP: ORDER INCORRECT', by = animals[runif(1,1,length(animals))] )
    correct_in_boris$time_fails[which(correct_in_boris$id_behav == id_behav[elephant_behaviour])] <- list(response$time[which(response$order_correct == 'no')])
  }
  if(length(which(response$order_correct == 'match')) > 0) {
    matching_times <- response[response$order_correct == 'match',]
    matching_times <- matching_times[!is.na(matching_times$order_correct),]
    response <- anti_join(response,matching_times)
    matching_times$change <- as.integer(as.factor(matching_times$time))
    for(j in unique(matching_times$change)){
      pair <- matching_times[matching_times$change == j,]
      matching_times <- anti_join(matching_times, pair)
      pair <- pair[!is.na(pair$change),]
      pair$time[which(pair$status == 'STOP')] <- pair$time[which(pair$status == 'STOP')] - 0.001
      matching_times <- rbind(matching_times, pair)
    }
    matching_times <- matching_times %>%
      select(-change) %>% 
      arrange(time)
    response <- rbind(response, matching_times) %>% 
      arrange(time)
    rm(matching_times, pair)
  }
  
  # check pairs are together (should be if all in correct order!)
  response$pairs_together <- NA
  if(nrow(response) > 2 ) {
    for(i in 1:(nrow(response)-1)){
      response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                           ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
    }
  } else {
    response$pairs_together <- ifelse(response$status[1] == 'START' & response$status[2] == 'STOP', 'yes', 'no')
  }
  
  if(length(which(response$pairs_together == 'no')) > 0) {
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = animals[runif(1,1,length(animals))] )
    for(k in 1:(nrow(response)-1)){
      if(response$pairs_together[k] == 'no' & #response$pairs_together[k+1] == 'yes' &
         response$status[k] == 'START' & response$status[k+1] == 'START') {
        response$time[k+1] <- response$time[k+1]+0.001
      }
      if(response$pairs_together[k] == 'no' & #response$pairs_together[k+1] == 'yes' &
         response$status[k] == 'STOP' & response$status[k+1] == 'STOP') {
        response$time[k] <- response$time[k]-0.001
      }
    }
    response <- response %>% arrange(time)
    # repeat to ensure has now worked
    if(nrow(response) > 2 ) {
      for(i in 1:(nrow(response)-1)){
        response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                             ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
      }
    } else {
      response$pairs_together <- ifelse(response$status[1] == 'START' & response$status[2] == 'STOP', 'yes', 'no')
    }
    if(length(which(response$pairs_together == 'no')) > 0) {
      cowsay::say('STOP: PAIRS NOT TOGETHER', by = animals[runif(1,1,length(animals))] )
      correct_in_boris$time_fails[which(correct_in_boris$id_behav == id_behav[elephant_behaviour])] <- list(response$time[which(response$pairs_together == 'no')])
    }
  }
  
  # add label per start/stop pair
  response$act_id <- rep(1:(nrow(response)/2), each = 2)
  response$action_unique <- paste0(response$act_id,'_',response$subject,'_',response$type,'_',response$action)
  
  # for pairs that span across multiple parts of experiment, create variable indicating which parts
  response$bda_split <- NA; for(i in 1:nrow(response)){
    x <- response[response$act_id == response$act_id[i],]
    response$bda_split[i] <- ifelse(x$bda[1] == x$bda[2], 'no',
                                    ifelse(x$bda[1] == 'before',
                                           ifelse(x$bda[2] == 'during', 'bd','ba'),
                                           'da'))
  }
  rm(x)
  
  # single split pairs that span before-during or during-after. double split pairs that span before-after.
  if(length(which(response$bda_split != 'no') > 0)) {
    response_split <- response[response$bda_split != 'no',]
    response <- anti_join(response, response_split)
    if('bd' %in% response_split$bda_split) {
      bd <- response_split[response_split$bda_split == 'bd',]
      response_split <- anti_join(response_split, bd)
      bd <- rbind(bd, bd)
      bd$time[2] <- bd$stim_start[1]-0.001
      bd$time[3] <- bd$stim_start[1]
      response_split <- rbind(bd, response_split)
      rm(bd)
    }
    if('da' %in% response_split$bda_split) {
      da <- response_split[response_split$bda_split == 'da',]
      response_split <- anti_join(response_split, da)
      da <- rbind(da, da)
      da$time[2] <- da$stim_stop[1]
      da$time[3] <- da$stim_stop[1]+0.001
      response_split <- rbind(response_split, da)
      rm(da)
    }
    if('ba' %in% response_split$bda_split) {
      ba <- response_split[response_split$bda_split == 'ba',]
      response_split <- anti_join(response_split, ba)
      ba <- rbind(ba, ba, ba)
      ba$time[2] <- ba$stim_start[1]-0.001
      ba$time[3] <- ba$stim_start[1]
      ba$time[4] <- ba$stim_stop[1]
      ba$time[5] <- ba$stim_stop[1]+0.001
      response_split <- rbind(response_split, ba)
      rm(ba)
    } 
    
    # re-calculate which section each one goes in
    response_split$bda <- ifelse(response_split$time < response_split$stim_start, 'before',
                                 ifelse(response_split$time > response_split$stim_stop, 'after', 'during'))
    
    # recombine into individual elephant actions per category
    response <- rbind(response, response_split)
    
  }
  response <- response %>% 
    arrange(time) %>% 
    mutate(activity_id = as.numeric(as.factor(paste0(action_unique, bda)))) %>%
    select(-order_correct, -pairs_together, -bda_split)
  
  # recombine into full dataset
  neighbour <- rbind(neighbour, response)
  
  # clean up
  gc()
  
}

saveRDS(neighbour, '../data_processed/elephants_behaviour_split_relative_to_stimulus_nearestneighbour.RDS')

# ones to go back and check on
correct_in_boris <- correct_in_boris %>% filter(time_fails != 'NA')
nrow(correct_in_boris)

# clean up
rm(correct_in_boris, response, response_split, elephant_behaviour, i, id_behav, N) ; gc()

## calculate length of action by subtracting START time from STOP time ####
# neighbour <- readRDS('../data_processed/elephants_behaviour_split_relative_to_stimulus_nearestneighbour.RDS')
# create a variable with a unique value for every action, also split across before/during/after
neighbour$action_unique <- paste0(neighbour$elephant_neighbour_unique, '_', neighbour$bda, '_', neighbour$activity_id)
neighbour$act_id <- as.integer(as.factor(neighbour$action_unique))
length(unique(neighbour$act_id))

# combine start and stop lines for every action so it is a single row per behaviour
start <- neighbour %>% 
  filter(status == 'START') %>% 
  select(subject,behaviour,time,act_id) %>% 
  rename(start_time = time)
stop <- neighbour %>% 
  filter(status == 'STOP') %>% 
  select(-subject, -behaviour, -status, -activity_unique, -activity_id) %>% 
  rename(stop_time = time)
behav <- left_join(start, stop, by = 'act_id') %>% 
  relocate(act_id)

# calculate duration of each behaviour
behav$duration <- behav$stop_time - behav$start_time

# neaten up
behav <- behav[,c(1:5,28,6:27)]

## remove "unknown" neighbours so that known pairs make up total time ####
nn_unk <- behav %>% filter(neighbour == 'unknown')
nn_known <- behav %>% filter(neighbour != 'unknown')

unk <- nn_unk %>%
  select(subject, bda, duration) %>% 
  rename(neighbour_unk = duration) %>% 
  mutate(unique = paste0(subject,'_',bda)) %>% 
  group_by(unique) %>% 
  mutate(total_neighbour_unk = sum(neighbour_unk)) %>% 
  ungroup() %>% 
  select(-neighbour_unk, -unique) %>% 
  rename(neighbour_unk = total_neighbour_unk) %>% 
  distinct()

#in_frame <- read_csv('../data_processed/elephants_time_in_frame.csv')
behav <- behav %>% 
  left_join(unk, by = c('subject','bda')) %>%
  rename(section = bda) %>% 
  left_join(in_frame[,c('subject','section','in_frame_seconds')],
            by = c('subject', 'section')) %>% 
  mutate(neighbour_known = in_frame_seconds - neighbour_unk) %>% 
  mutate(propn_new = duration / neighbour_known)

# save
saveRDS(behav, '../data_processed/elephants_behaviour_startstop_neighbour.RData')

## calculate proportion of time spent per activity ####
# read in time in frame per elephant
#df_eles <- readRDS('../data_processed/elephants.RDS')

# create data frame showing amount of time spent on each behaviour by every elephant in each playback section
behav <- behav %>% 
  filter(neighbour != 'unknown') %>% 
  mutate(neighbour_known_seconds = ifelse(is.na(neighbour_known) == TRUE,
                                          in_frame_seconds,neighbour_known))
num_eles <- length(unique(behav$subject))
props <- data.frame(subject = rep(rep(sort(unique(behav$subject)),
                                      each = 8),
                                  each = 3),
                    dyad_partner = rep(rep(c('b1','b2','b3','b4','b5','b6','b7','b8'),
                                           num_eles),
                                       each = 3),
                    section = rep(rep(c('before','during','after'),
                                      8),
                                  num_eles)) %>% 
  separate(subject, into = c('focal','pb_num'), sep = '_e', remove = F) %>% 
  filter(focal != dyad_partner) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  mutate(targeted_elephant = paste0(dyad_partner, '_e', pb_num)) %>% 
  filter(targeted_elephant %in% behav$subject) %>% 
  left_join(distinct(behav[,c('subject','section','neighbour_known_seconds')]),
            by = c('subject','section')) %>% 
  mutate(behav_seconds = NA,
         propn = NA)

# calculate time per elephant per playback section spent on each behaviour
for( i in 1:nrow(props) ) {
  nearest <- behav %>%
    filter(subject == props$subject[i]) %>% 
    filter(neighbour == props$dyad_partner[i]) %>% 
    filter(section == props$section[i])
  props$behav_seconds[i] <- ifelse(nrow(nearest) == 0, 0, sum(nearest$duration) )
}

# convert times to proportions so that duration in frame does not affect total
props$propn <- props$behav_seconds / props$neighbour_known_seconds

# add in additional information about each behaviour and elephant ages
props <- props %>% 
  left_join(distinct(df_eles[,c('subject','age', 'stim_num', 'stim_type', 'group_size')]),
            by = 'subject')
partner_info <- df_eles %>% 
  select(subject, age) %>% 
  distinct() %>% 
  rename(targeted_elephant = subject,
         partner_age = age) %>% 
  mutate(partner_age = ifelse(targeted_elephant == 'b1_e19', '21-25',
                              partner_age))
props <- props %>% 
  left_join(partner_info, by = 'targeted_elephant')

# correct wrong age
props$age <- ifelse(props$age == '16-25', '21-25', props$age) # experiment 19, bull1 listed as 16-25, which is not a correct age

# age differences
props$age_category <- ifelse(props$age == '10-15', 1,
                             ifelse(props$age == '16-20', 2,
                                    ifelse(props$age == '21-25', 3,
                                           ifelse(props$age == '26-35', 4, NA))))
props$partner_age_category <- ifelse(props$partner_age == '10-15', 1,
                                     ifelse(props$partner_age == '16-20', 2,
                                            ifelse(props$partner_age == '21-25', 3,
                                                   ifelse(props$partner_age == '26-35', 4, NA))))
props$age_difference <- ifelse(props$age_category == props$partner_age_category, 'matched',
                               ifelse(props$age_category > props$partner_age_category,
                                      'partner younger', 'partner older'))

# save
saveRDS(props, '../data_processed/neighbour_behaviour_proportions.RDS')

## graph proportions ####
#props <- readRDS('../data_processed/elephant_behaviour_proportions.RDS')
props$section <- factor(props$section, levels = c('before','during','after'))
props$stimulus <- ifelse(props$stim_type == 'ctd', 'cape turtle dove (control)', 
                         ifelse(props$stim_type == 'h', 'human', 'lion')) %>% 
  factor(levels = c('cape turtle dove (control)','lion','human'))
props$age <- factor(props$age, levels = c('10-15','16-20','21-25','26-35','unkage'))
props$partner_age <- factor(props$partner_age, levels = c('10-15','16-20','21-25','26-35','unkage'))
props$age_difference <- factor(props$age_difference,
                               levels = c('partner younger','matched','partner older'))
props$dyad <- paste0('e',props$pb_num,'_', props$focal,'_', props$dyad_partner)

# nearest neighbour
props %>% 
  filter(propn != 'NaN') %>% 
  filter(!is.na(age_difference)) %>% 
  ggplot(aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(stimulus ~ age_difference)+
  scale_y_continuous('proportion of time visible on camera spent closest to other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')

props %>% 
  filter(propn != 'NaN') %>% 
  ggplot(aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+  # columns = partner age, rows = focal age
  scale_y_continuous('proportion of time visible on camera spent looking at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'neighbour ages\n(rows = focal age, columns = partner age)')

###### PROPORTIONS OF TIME PER ACTIVITY: SOCIAL INTERACTIONS ######
## split into time before/during/after stimulus -- DONE ####
# run "prep for splitting" section, then go from here
rm(pt) ; gc()
social <- df %>% filter(type == 'social')

# fill in missing data: 3 points, all of which match to everything around them so can assume is the same
for(i in which(is.na(social$comment) == TRUE)){
  social$comment[i] <- ifelse(social$status[i] == 'START',
                              social$comment[i+1],
                              social$comment[i-1])
}

# standardise -- only keep "who touched who" for now
unique(social$comment)
social$partner <- case_when(
  social$comment == 'b2_brushed_by_b2' ~ 'b2',
  social$comment == 'brushed_by_b2' ~ 'b2',
  social$comment == 'face_on_b2' ~ 'b2',
  social$comment == 'backs_into_b1' ~ 'b1',
  social$comment == 'b1_face_on_b2_butt' ~ 'b1',
  social$comment == 'b6_trunk_on_b3' ~ 'b6',
  social$comment == 'puts trunk on b3' ~ 'b3',
  social$comment == 'b3_wrestle' ~ 'b3',
  social$comment == 'wrestling b2' ~ 'b2',
  social$comment == 'b4_pushes_b1' ~ 'b4',
  social$comment == 'b4_touches_flank_b1' ~ 'b4',
  social$comment == 'b6_pushes_b1' ~ 'b6',
  social$comment == 'b1_pushes_b1' ~ 'b1',
  social$comment == 'b6_trunk_on_b4' ~ 'b6',
  social$comment == 'b7_pushes_b4' ~ 'b7',
  social$comment == 'b4_trunk_on_b4' ~ 'b4',
  social$comment == 'b5_pushes_b5' ~ 'b5',
  social$comment == 'b5_leans_on_b7' ~ 'b5',
  social$comment == 'b3_leans_on_b1' ~ 'b3',
  social$comment == 'b2_trunk_on_b1_back;b3_leans_on_b1' ~ 'b2;b3',
  social$comment == 'b2_trunk_on_b1_back' ~ 'b2',
  social$comment == 'b1_trunk_on_back_b1' ~ 'b1',
  social$comment == 'b1_trunk_on_back_b1;b3_trunk_on_ear_b3' ~ 'b1;b3',
  social$comment == 'b3_headbutt_b3' ~ 'b3',
  social$comment == 'b3_head_on_b3_butt' ~ 'b3',
  social$comment == 'b1_ear_on_b1_back' ~ 'b1',
  social$comment == 'b1_ear_on_b1_back;b2_trunk_on_b3_ear' ~ 'b1;b2',
  social$comment == 'b2_head_touches_b2' ~ 'b2',
  social$comment == 'b1_head_on_b1' ~ 'b1',
  social$comment == 'b4_trunk_on_b3_leg' ~ 'b4',
  social$comment == 'b4_trunk_on_b1_leg' ~ 'b4',
  social$comment == 'b4_head_on_b1_butt' ~ 'b4',
  social$comment == 'b3_trunk_on_b3_leg' ~ 'b3',
  social$comment == 'b1_head_on_b1_butt' ~ 'b1',
  social$comment == 'b4_pushes_b3_butt' ~ 'b4',
  social$comment == 'b3_pushes_b3' ~ 'b3',
  social$comment == 'pushed_by_b2' ~ 'b2',
  social$comment == 'pushed_by_b3' ~ 'b3',
  social$comment == 'pushes_past_b2' ~ 'b2',
  .default = social$comment)
social$initiated_by <- case_when(
  social$comment == 'b2_brushed_by_b2' ~ 'b2',
  social$comment == 'brushed_by_b2' ~ 'b2',
  social$comment == 'face_on_b2' ~ 'focal',
  social$comment == 'backs_into_b1' ~ 'focal',
  social$comment == 'b1_face_on_b2_butt' ~ 'b1',
  social$comment == 'b6_trunk_on_b3' ~ 'b6',
  social$comment == 'puts trunk on b3' ~ 'focal',
  social$comment == 'b3_wrestle' ~ 'unk',
  social$comment == 'wrestling b2' ~ 'unk',
  social$comment == 'b4_pushes_b1' ~ 'b4',
  social$comment == 'b4_touches_flank_b1' ~ 'b4',
  social$comment == 'b6_pushes_b1' ~ 'b6',
  social$comment == 'b1_pushes_b1' ~ 'focal',
  social$comment == 'b6_trunk_on_b4' ~ 'b6',
  social$comment == 'b7_pushes_b4' ~ 'b7',
  social$comment == 'b4_trunk_on_b4' ~ 'focal',
  social$comment == 'b5_pushes_b5' ~ 'focal',
  social$comment == 'b5_leans_on_b7' ~ 'b5',
  social$comment == 'b3_leans_on_b1' ~ 'b3',
  social$comment == 'b2_trunk_on_b1_back;b3_leans_on_b1' ~ 'b2;b3',
  social$comment == 'b2_trunk_on_b1_back' ~ 'b2',
  social$comment == 'b1_trunk_on_back_b1' ~ 'focal',
  social$comment == 'b1_trunk_on_back_b1;b3_trunk_on_ear_b3' ~ 'focal;focal',
  social$comment == 'b3_headbutt_b3' ~ 'focal',
  social$comment == 'b3_head_on_b3_butt' ~ 'focal',
  social$comment == 'b1_ear_on_b1_back' ~ 'focal',
  social$comment == 'b1_ear_on_b1_back;b2_trunk_on_b3_ear' ~ 'focal;b2',
  social$comment == 'b2_head_touches_b2' ~ 'focal',
  social$comment == 'b1_head_on_b1' ~ 'focal',
  social$comment == 'b4_trunk_on_b3_leg' ~ 'b4',
  social$comment == 'b4_trunk_on_b1_leg' ~ 'b4',
  social$comment == 'b4_head_on_b1_butt' ~ 'b4',
  social$comment == 'b3_trunk_on_b3_leg' ~ 'focal',
  social$comment == 'b1_head_on_b1_butt' ~ 'focal',
  social$comment == 'b4_pushes_b3_butt' ~ 'b4',
  social$comment == 'b3_pushes_b3' ~ 'focal',
  social$comment == 'pushed_by_b2' ~ 'b2',
  social$comment == 'pushed_by_b3' ~ 'b3',
  social$comment == 'pushes_past_b2' ~ 'focal',
  .default = 'unknown')
unique(social$partner)
unique(social$initiated_by)

for(i in 1:nrow(social)){
  if(social$partner[i] == 'b1;b2' | social$partner[i] == 'b2;b3' | social$partner[i] == 'b1;b3'){
    x <- social[i,]
    pre_x <- social[1:(i-1),]
    post_x <- social[(i+1):nrow(social),]
    x <- rbind(x, x) %>% 
      separate(partner, into = c('partner1','partner2'), sep = ';', remove = T) %>% 
      mutate(partner = c(partner1[1], partner2[1])) %>% 
      select(colnames(social)) %>% 
      separate(initiated_by, into = c('partner1','partner2'), sep = ';', remove = T) %>% 
      mutate(initiated_by = c(partner1[1], partner2[1])) %>% 
      select(colnames(social))
    social <- rbind(pre_x, x, post_x)
  }
}
rm(pre_x, post_x, x) ; gc()

social <- social %>% 
  separate(subject, into = c('focal', 'experiment'), remove = F, sep = '_') %>% 
  select(-experiment) %>% 
  mutate(targeted_elephant = ifelse(focal == partner, 'error',
                                    ifelse(initiated_by == 'unknown',
                                           'unknown',
                                           ifelse(initiated_by == 'focal',
                                                  partner,
                                                  focal))),
         initiated_by = ifelse(initiated_by == 'focal',
                               focal, initiated_by))

# create set of individual/behaviour breakdowns to go through
social$elephant_social_unique <- paste0(social$subject,'_',social$initiated_by, social$targeted_elephant)
id_behav <- sort(unique(social$elephant_social_unique))
N <- length(id_behav)

# prep data frame for rbind at end of loop, to put all of the levels back together
social <- social %>% 
  rename(behaviour = behavior) %>% 
  mutate(activity_id = NA,
         act_id = NA,
         action_unique = NA)

# set up list of ones which fail to come back to in BORIS
correct_in_boris <- as.data.frame(id_behav) %>% 
  separate(id_behav, into = c('bull', 'pb_num','target'), remove = F) %>% 
  mutate(check = NA, time_fails = NA)

# run for loop
for(elephant_behaviour in 1:N){
  # select individual and behavioural set
  response <- social[social$elephant_social_unique == id_behav[elephant_behaviour],]
  response <- response[is.na(response$time) == F,]
  social <- anti_join(social, response)
  
  # check in correct order
  response$order_correct <- NA
  response$order_correct[1] <- ifelse(response$time[1] < response$time[2], 'yes', 
                                      ifelse(response$time[1] == response$time[2], 'match', 'no'))
  if(nrow(response) > 2 ) {
    for(i in 2:(nrow(response)-1)){
      response$order_correct[i] <- ifelse(response$time[i] < response$time[i+1] &
                                            response$time[i] > response$time[i-1], 'yes', 
                                          ifelse(response$time[i] == response$time[i+1] |
                                                   response$time[i] == response$time[i-1], 'match',
                                                 'no'))
    }
  } else {
    response$order_correct <- ifelse(response$time[1] < response$time[2], 'yes', 'no')
  }
  
  if(length(which(response$order_correct == 'no')) > 0) {
    cowsay::say('STOP: ORDER INCORRECT', by = animals[runif(1,1,length(animals))] )
    correct_in_boris$time_fails[which(correct_in_boris$id_behav == id_behav[elephant_behaviour])] <- list(response$time[which(response$order_correct == 'no')])
  }
  if(length(which(response$order_correct == 'match')) > 0) {
    matching_times <- response[response$order_correct == 'match',]
    matching_times <- matching_times[!is.na(matching_times$order_correct),]
    response <- anti_join(response,matching_times)
    matching_times$change <- as.integer(as.factor(matching_times$time))
    for(j in unique(matching_times$change)){
      pair <- matching_times[matching_times$change == j,]
      matching_times <- anti_join(matching_times, pair)
      pair <- pair[!is.na(pair$change),]
      pair$time[which(pair$status == 'STOP')] <- pair$time[which(pair$status == 'STOP')] - 0.001
      matching_times <- rbind(matching_times, pair)
    }
    matching_times <- matching_times %>%
      select(-change) %>% 
      arrange(time)
    response <- rbind(response, matching_times) %>% 
      arrange(time)
    rm(matching_times, pair)
  }
  
  # check pairs are together (should be if all in correct order!)
  response$pairs_together <- NA
  if(nrow(response) > 2 ) {
    for(i in 1:(nrow(response)-1)){
      response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                           ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
    }
  } else {
    response$pairs_together <- ifelse(response$status[1] == 'START' & response$status[2] == 'STOP', 'yes', 'no')
  }
  
  if(length(which(response$pairs_together == 'no')) > 0) {
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = animals[runif(1,1,length(animals))] )
    for(k in 1:(nrow(response)-1)){
      if(response$pairs_together[k] == 'no' & #response$pairs_together[k+1] == 'yes' &
         response$status[k] == 'START' & response$status[k+1] == 'START') {
        response$time[k+1] <- response$time[k+1]+0.001
      }
      if(response$pairs_together[k] == 'no' & #response$pairs_together[k+1] == 'yes' &
         response$status[k] == 'STOP' & response$status[k+1] == 'STOP') {
        response$time[k] <- response$time[k]-0.001
      }
    }
    response <- response %>% arrange(time)
    # repeat to ensure has now worked
    if(nrow(response) > 2 ) {
      for(i in 1:(nrow(response)-1)){
        response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                             ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
      }
    } else {
      response$pairs_together <- ifelse(response$status[1] == 'START' & response$status[2] == 'STOP', 'yes', 'no')
    }
    if(length(which(response$pairs_together == 'no')) > 0) {
      cowsay::say('STOP: PAIRS NOT TOGETHER', by = animals[runif(1,1,length(animals))] )
      correct_in_boris$time_fails[which(correct_in_boris$id_behav == id_behav[elephant_behaviour])] <- list(response$time[which(response$pairs_together == 'no')])
    }
  }
  
  # add label per start/stop pair
  response$act_id <- rep(1:(nrow(response)/2), each = 2)
  response$action_unique <- paste0(response$act_id,'_',response$subject,'_',response$type,'_',response$action)
  
  # for pairs that span across multiple parts of experiment, create variable indicating which parts
  response$bda_split <- NA; for(i in 1:nrow(response)){
    x <- response[response$act_id == response$act_id[i],]
    response$bda_split[i] <- ifelse(x$bda[1] == x$bda[2], 'no',
                                    ifelse(x$bda[1] == 'before',
                                           ifelse(x$bda[2] == 'during', 'bd','ba'),
                                           'da'))
  }
  rm(x)
  
  # single split pairs that span before-during or during-after. double split pairs that span before-after.
  if(length(which(response$bda_split != 'no') > 0)) {
    response_split <- response[response$bda_split != 'no',]
    response <- anti_join(response, response_split)
    if('bd' %in% response_split$bda_split) {
      bd <- response_split[response_split$bda_split == 'bd',]
      response_split <- anti_join(response_split, bd)
      bd <- rbind(bd, bd)
      bd$time[2] <- bd$stim_start[1]-0.001
      bd$time[3] <- bd$stim_start[1]
      response_split <- rbind(bd, response_split)
      rm(bd)
    }
    if('da' %in% response_split$bda_split) {
      da <- response_split[response_split$bda_split == 'da',]
      response_split <- anti_join(response_split, da)
      da <- rbind(da, da)
      da$time[2] <- da$stim_stop[1]
      da$time[3] <- da$stim_stop[1]+0.001
      response_split <- rbind(response_split, da)
      rm(da)
    }
    if('ba' %in% response_split$bda_split) {
      ba <- response_split[response_split$bda_split == 'ba',]
      response_split <- anti_join(response_split, ba)
      ba <- rbind(ba, ba, ba)
      ba$time[2] <- ba$stim_start[1]-0.001
      ba$time[3] <- ba$stim_start[1]
      ba$time[4] <- ba$stim_stop[1]
      ba$time[5] <- ba$stim_stop[1]+0.001
      response_split <- rbind(response_split, ba)
      rm(ba)
    } 
    
    # re-calculate which section each one goes in
    response_split$bda <- ifelse(response_split$time < response_split$stim_start, 'before',
                                 ifelse(response_split$time > response_split$stim_stop, 'after', 'during'))
    
    # recombine into individual elephant actions per category
    response <- rbind(response, response_split)
    
  }
  response <- response %>% 
    arrange(time) %>% 
    mutate(activity_id = as.numeric(as.factor(paste0(action_unique, bda)))) %>%
    select(-order_correct, -pairs_together, -bda_split)
  
  # recombine into full dataset
  social <- rbind(social, response)
  
  # clean up
  gc()
  
}

saveRDS(social, '../data_processed/elephants_behaviour_split_relative_to_stimulus_socialtouch.RDS')

# ones to go back and check on
correct_in_boris <- correct_in_boris %>% filter(time_fails != 'NA')
nrow(correct_in_boris)

# clean up
rm(correct_in_boris, response, response_split, elephant_behaviour, i, id_behav, N) ; gc()

## calculate length of action by subtracting START time from STOP time ####
# social <- readRDS('../data_processed/elephants_behaviour_split_relative_to_stimulus_socialtouch.RDS')
# create a variable with a unique value for every action, also split across before/during/after
social$action_unique <- paste0(social$elephant_social_unique, '_', social$bda, '_', social$activity_id)
social$act_id <- as.integer(as.factor(social$action_unique))
length(unique(social$act_id))

# combine start and stop lines for every action so it is a single row per behaviour
start <- social %>% 
  filter(status == 'START') %>% 
  select(subject,behaviour,time,act_id) %>% 
  rename(start_time = time)
stop <- social %>% 
  filter(status == 'STOP') %>% 
  select(-subject, -behaviour, -status, -activity_unique, -activity_id) %>% 
  rename(stop_time = time)
behav <- left_join(start, stop, by = 'act_id') %>% 
  relocate(act_id)

# calculate duration of each behaviour
behav$duration <- behav$stop_time - behav$start_time

# neaten up
behav <- behav[,c(1:5,30,6:29)]

# save
saveRDS(behav, '../data_processed/elephants_behaviour_startstop_socialtouch.RData')

## calculate proportion of time spent per activity ####
# read in time in frame per elephant
#df_eles <- readRDS('../data_processed/elephants.RDS') ; in_frame <- read.csv('../data_processed/elephants_time_in_frame.csv')

# create data frame showing amount of time spent on each behaviour by every elephant in each playback section
num_eles <- length(unique(behav$subject))
props <- data.frame(subject = rep(rep(sort(unique(behav$subject)),
                                      each = 8),
                                  each = 3),
                    dyad_partner = rep(rep(c('b1','b2','b3','b4','b5','b6','b7','b8'),
                                           num_eles),
                                       each = 3),
                    section = rep(rep(c('before','during','after'),
                                      8),
                                  num_eles)) %>% 
  separate(subject, into = c('focal','pb_num'), sep = '_e', remove = F) %>% 
  filter(focal != dyad_partner) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  mutate(targeted_elephant = paste0(dyad_partner, '_e', pb_num)) %>% 
  filter(targeted_elephant %in% behav$subject) %>% 
  left_join(in_frame[,c('subject','section','in_frame_seconds')],
            by = c('subject','section')) %>% 
  mutate(giver = NA,
         receiver = NA,
         behav_seconds = NA,
         propn = NA)
props <- props %>% 
  rbind(props) %>% 
  mutate(giver = c(focal[1:nrow(props)], dyad_partner[1:nrow(props)]),
         receiver = c(dyad_partner[1:nrow(props)], focal[1:nrow(props)]))

# calculate time per elephant per playback section spent on each behaviour
for( i in 1:nrow(props) ) {
  touch <- behav %>%
    filter(subject == props$subject[i]) %>% 
    filter(partner == props$dyad_partner[i]) %>% 
    filter(bda == props$section[i])
  props$behav_seconds[i] <- ifelse(nrow(touch) == 0, 0,
                                   sum(touch$duration[which(touch$initiated_by == props$giver[i])]))
}

# convert times to proportions so that duration in frame does not affect total
props$propn <- props$behav_seconds / props$in_frame_seconds

# add in additional information about each behaviour and elephant ages
props <- props %>% 
  left_join(distinct(df_eles[,c('subject','age', 'stim_num', 'stim_type', 'group_size')]),
            by = 'subject')
partner_info <- df_eles %>% 
  select(subject, age) %>% 
  distinct() %>% 
  rename(targeted_elephant = subject,
         partner_age = age) %>% 
  mutate(partner_age = ifelse(targeted_elephant == 'b1_e19', '21-25',
                              partner_age))
props <- props %>% 
  left_join(partner_info, by = 'targeted_elephant')

# correct wrong age
props$age <- ifelse(props$age == '16-25', '21-25', props$age) # experiment 19, bull1 listed as 16-25, which is not a correct age

# age differences
props$age_category <- ifelse(props$age == '10-15', 1,
                             ifelse(props$age == '16-20', 2,
                                    ifelse(props$age == '21-25', 3,
                                           ifelse(props$age == '26-35', 4, NA))))
props$partner_age_category <- ifelse(props$partner_age == '10-15', 1,
                                     ifelse(props$partner_age == '16-20', 2,
                                            ifelse(props$partner_age == '21-25', 3,
                                                   ifelse(props$partner_age == '26-35', 4, NA))))
props$age_difference <- ifelse(props$age_category == props$partner_age_category, 'matched',
                               ifelse(props$age_category > props$partner_age_category,
                                      'partner younger', 'partner older'))

# save
saveRDS(props, '../data_processed/socialtouch_behaviour_proportions.RDS')

## graph proportions ####
#props <- readRDS('../data_processed/elephant_behaviour_proportions.RDS')
props$section <- factor(props$section, levels = c('before','during','after'))
props$stimulus <- ifelse(props$stim_type == 'ctd', 'cape turtle dove (control)', 
                         ifelse(props$stim_type == 'h', 'human', 'lion')) %>% 
  factor(levels = c('cape turtle dove (control)','lion','human'))
props$age <- factor(props$age, levels = c('10-15','16-20','21-25','26-35','unkage'))
props$partner_age <- factor(props$partner_age, levels = c('10-15','16-20','21-25','26-35','unkage'))
props$age_difference <- factor(props$age_difference,
                               levels = c('partner younger','matched','partner older'))
props$dyad <- paste0('e',props$pb_num,'_', props$focal,'_', props$dyad_partner)

# social interactions
props %>% 
  filter(propn != 'NaN') %>% 
  ggplot(aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(stimulus ~ age_difference)+
  scale_y_continuous('proportion of time in physical contact with other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')

props %>% 
  filter(propn != 'NaN') %>% 
  ggplot(aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+  # columns = partner age, rows = focal age
  scale_y_continuous('proportion of time in physical contact with other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'neighbour ages\n(rows = focal age, columns = partner age)')

###### PROPORTIONS OF TIME PER ACTIVITY: COMBINE INTO SINGLE DATA FRAME ######
# clean environment
rm(list = ls()) ; gc()

# read in proportion data
svs <- readRDS('../data_processed/speaker_vehicle_stress_behaviour_proportions.RDS')
ele <- readRDS('../data_processed/elephant_behaviour_proportions.RDS')
nn  <- readRDS('../data_processed/neighbour_behaviour_proportions.RDS')
soc <- readRDS('../data_processed/socialtouch_behaviour_proportions.RDS')

# set order of columns for binding together
column_order <- c("pb_num","section","subject","focal","age","age_category",
                  "type","target","action","behavioral_category","behaviour",
                  "behav_seconds","propn",
                  "dyad_partner","targeted_elephant",
                  "partner_age","partner_age_category","age_difference",
                  "stim_num","stim_type","group_size",
                  "video_start","video_end","video_duration",
                  "out_frame_seconds","in_frame_seconds",
                  "neighbour_known_seconds","giver","receiver")

# import information missing from nearest neighbour and social contact
in_frame <- read_csv('../data_processed/elephants_time_in_frame.csv')

# standardise columns for binding
svs <- svs %>% 
  separate(subject, into = c('focal','remove'), sep = '_', remove = F) %>% 
  select(-remove) %>% 
  mutate(dyad_partner = NA,
         targeted_elephant = NA,
         partner_age = NA,
         age_category = ifelse(age == '10-15', 1,
                               ifelse(age == '16-20', 2,
                                      ifelse(age == '21-25', 3,
                                             ifelse(age == '26-35', 4, NA)))),
         partner_age_category = NA,
         age_difference = NA,
         neighbour_known_seconds = NA,
         giver = NA,
         receiver = NA) %>% 
  filter(target != 'social') %>% 
  select(all_of(column_order))
ele <- ele %>% 
  mutate(type = 'elephant',
         target = 'elephant',
         neighbour_known_seconds = NA,
         giver = NA,
         receiver = NA) %>% 
  select(all_of(column_order))
nn <- nn %>% 
  left_join(in_frame, by = c('subject','pb_num','section')) %>% 
  mutate(behaviour = 'nearest_neighbour',
         type = 'neighbour',
         action = 'nearest_neighbour',
         behavioral_category = 'social',
         target = 'neighbour',
         giver = NA,
         receiver = NA) %>% 
  select(all_of(column_order))
soc <- soc %>% 
  left_join(in_frame[,1:7], by = c('subject','pb_num','section')) %>% 
  mutate(behaviour = 'social_contact_elephant',
         type = 'contact',
         action = 'contact',
         behavioral_category = 'social',
         target = 'elephant',
         neighbour_known_seconds = NA) %>% 
  select(all_of(column_order))

# bind all together
props <- rbind(svs, ele, nn, soc) %>% 
  rename(behavioural_category = behavioral_category)

# check columns with NA values -- some could be combined
na_cols <- data.frame(column = colnames(props),
                      length_na = NA)
for(i in 1:ncol(props)){
  na_cols$length_na[i] <- length(which(is.na(props[,i] == TRUE)))
}

column_check <- props[is.na(props$age_category),]
na_cols$length_na[na_cols$column == 'age_category'] <- 0

column_check <- props[is.na(props$propn),]
check2 <- column_check[column_check$behav_seconds != 0,]    # behaviour always 0
check2 <- column_check[column_check$in_frame_seconds != 0,] # 154 where time in frame > 0
check2 <- check2[check2$target != 'stress',] # 134 caused by ears/tail/trunk never visible, 20 caused by time with known neighbour = NA
na_cols$length_na[na_cols$column == 'propn'] <- 0

column_check <- props[is.na(props$dyad_partner),]
unique(column_check$type)
na_cols$length_na[na_cols$column == 'dyad_partner'] <- 0

column_check <- props[is.na(props$targeted_elephant),]
unique(column_check$type)
na_cols$length_na[na_cols$column == 'targeted_elephant'] <- 0

column_check <- props[is.na(props$partner_age),]
unique(column_check$type)
na_cols$length_na[na_cols$column == 'partner_age'] <- 0

column_check <- props[is.na(props$partner_age_category) & 
                        !is.na(props$partner_age),]
unique(column_check$partner_age)
na_cols$length_na[na_cols$column == 'partner_age_category'] <- 0

column_check <- props[is.na(props$age_difference) & 
                        !is.na(props$partner_age) & 
                        !is.na(props$age),]
table(column_check$age, column_check$partner_age)
na_cols$length_na[na_cols$column == 'age_difference'] <- 0

column_check <- props[!is.na(props$neighbour_known_seconds),]
unique(column_check$type)
na_cols$length_na[na_cols$column == 'neighbour_known_seconds'] <- 0

column_check <- props[!is.na(props$giver),]
unique(column_check$type)
na_cols$length_na[na_cols$column == 'giver'] <- 0

column_check <- props[!is.na(props$receiver),]
unique(column_check$type)
na_cols$length_na[na_cols$column == 'receiver'] <- 0

# clean environment
rm(column_check, check2, na_cols, in_frame, nn, soc, svs, ele, column_order, i) ; gc()

# save output
saveRDS(props, '../data_processed/proportions_of_time_per_behaviour.RDS')

# box plot proportions
props %>% 
  mutate(stim_type_full = ifelse(stim_type == 'l', 'lion',
                                 ifelse(stim_type == 'ctd', 'control', 'human'))) %>% 
  filter(type == 'elephant' & behavioural_category == 'look') %>% 
  filter(age != 'unkage' & partner_age != 'unkage') %>% 
  mutate(action = factor(action, levels = c('look at directly','side-on','look directly away')),
         section = factor(section, levels = c('before','during','after')),
         stim_type_full = factor(stim_type_full, levels = c('control','lion','human')),
         age_difference = factor(age_difference, levels = c('partner younger','matched','partner older'))) %>% 
  ggplot(mapping = aes(x = action, y = propsn,
                       fill = stim_type_full))+
  geom_boxplot(notch = F)+
  facet_grid(section ~ age_difference)+
  scale_y_continuous(name = 'propsortion of time spent', expand = c(0.01,0.01))+
  scale_x_discrete(name = 'looking direction relative to other elephants')+
  scale_fill_viridis_d()+
  theme(panel.spacing = unit(0.5, 'cm', data = NULL)#, axis.text.x = element_text(angle = 90)
  )+
  labs(fill = 'experiment type')
ggsave('../outputs/looking_boxplots_elephants.png', device = 'png',
       width = 11.70, height = 8.30)

props %>% 
  filter(type == 'elephant' & behavioural_category == 'move') %>% 
  filter(age != 'unkage' & partner_age != 'unkage') %>% 
  mutate(action = factor(action, levels = c('approach at an angle','approach directly','move away at an angle','move away directly','move directly with')),
         section = factor(section, levels = c('before','during','after')),
         stim_type_full = factor(stim_type_full, levels = c('control','lion','human')),
         age_difference = factor(age_difference, levels = c('partner younger','matched','partner older'))) %>% 
  ggplot(mapping = aes(x = action, y = propsn,
                       fill = stim_type_full))+
  geom_boxplot(notch = F)+
  facet_grid(section ~ age_difference)+
  scale_y_continuous(name = 'propsortion of time spent', expand = c(0.01,0.01))+
  scale_x_discrete(name = 'movement direction relative to other elephants')+
  scale_fill_viridis_d()+
  theme(panel.spacing = unit(0.5, 'cm', data = NULL), axis.text.x = element_text(angle = 90))+
  labs(fill = 'experiment type')
ggsave('../outputs/moving_boxplots_elephants.png', device = 'png',
       width = 11.70, height = 8.30)

###### LATENCY TO CHANGE BEHAVIOUR FROM START OF STIMULUS ######
in_frame <- read_csv('../data_processed/elephants_time_in_frame.csv') %>% 
  select('subject','section','in_frame_seconds')
column_order <- c("subject","behaviour",
                  "start_time","stop_time","duration",
                  "type","action","behavioral_category",
                  "section",
                  "target","focal","partner","elephant_partner_unique",
                  "comment",
                  "age","bull",
                  "pb_num","stim_num","stim_type","stim_start","stim_stop","stim_duration",
                  "group_size","date","play_time",
                  "action_unique","act_id",
                  "in_frame_seconds","neighbour_unk","neighbour_known",
                  "initiated_by","targeted_elephant")

soc <- readRDS('../data_processed/elephants_behaviour_startstop_socialtouch.RData') %>% 
  rename(section = bda,
         elephant_partner_unique = elephant_social_unique) %>% 
  left_join(in_frame, by = c('subject','section'))

nn <- readRDS('../data_processed/elephants_behaviour_startstop_neighbour.RData') %>% 
  rename(elephant_partner_unique = elephant_neighbour_unique,
         partner = neighbour) %>% 
  mutate(age = NA,
         bull = NA,
         play_time = NA,
         focal = subject,
         initiated_by = NA,
         targeted_elephant = NA) %>% 
  select(all_of(column_order))

soc <- soc %>% 
  left_join(distinct(nn[,c('subject','section','neighbour_unk','neighbour_known')]),
            by = c('subject','section')) %>% 
  select(all_of(column_order))

svs <- readRDS('../data_processed/elephants_behaviour_startstop_speakervehiclestress.RData') %>% 
  rename(section = bda) %>% 
  left_join(in_frame, by = c('subject','section')) %>% 
  left_join(distinct(nn[,c('subject','section','neighbour_unk','neighbour_known')]),
            by = c('subject','section')) %>% 
  mutate(partner = NA,
         elephant_partner_unique = NA,
         focal = subject,
         initiated_by = NA,
         targeted_elephant = NA) %>% 
  select(all_of(column_order))

ele <- readRDS('../data_processed/elephants_behaviour_startstop_elephants.RData') %>% 
  rename(section = bda,
         elephant_partner_unique = elephant_look_unique,
         partner = elephant_look) %>% 
  left_join(in_frame, by = c('subject','section')) %>% 
  left_join(distinct(nn[,c('subject','section','neighbour_unk','neighbour_known')]),
            by = c('subject','section')) %>% 
  mutate(focal = subject,
         initiated_by = subject,
         targeted_elephant = partner) %>% 
  select(all_of(column_order))
  
# combine together
df <- rbind(svs, ele, nn, soc)
rm(in_frame, column_order) ; gc()
rm(svs, ele, nn, soc) ; gc()

# standardise
df$act_id_all <- as.integer(as.factor(df$action_unique))
max(df$act_id_all)

# start time since stimulus start and stop
df$time_since_stim_start <- df$start_time - df$stim_start
df$time_since_stim_stop  <- df$start_time - df$stim_stop

# graph
df %>% 
  filter(action != 'not visible') %>% 
  filter(action != 'tip not visible') %>% 
  filter(behaviour != 'OUT OF SIGHT: OUT OF SIGHT') %>% 
  mutate(behaviour = factor(behaviour, 
                            levels = c("elephant: approach directly","elephant: approach at an angle","elephant: move directly with","elephant: move away at an angle","elephant: move away directly",
                                       "elephant: look at directly","elephant: side-on","elephant: look directly away",
                                       "nearest neighbour: nearest neighbour",
                                       "social: physical contact elephant",
                                       "ears: flared","ears: relaxed (back or flapping regularly)",
                                       "speaker: approach directly","speaker: approach at an angle","speaker: move away at an angle","speaker: move away directly",
                                       "speaker: look at directly","speaker: side-on","speaker: look directly away",
                                       "tail: down","tail: up",
                                       "trunk: up and periscope sniffing","trunk: down and ground-level sniff","trunk: not sniffing (rest/feed/dust/stationary)",
                                       "vehicle: approach directly","vehicle: approach at an angle","vehicle: move neither towards or away","vehicle: move away at an angle","vehicle: move away directly",
                                       "vehicle: look at directly","vehicle: side-on","vehicle: look directly away"))) %>% 
  mutate(stimulus = ifelse(stim_type == 'ctd', 'cape turtle dove (control)',
                           ifelse(stim_type == 'h', 'human', 'lion'))) %>% 
  ggplot()+
  geom_jitter(aes(x = behaviour, y = time_since_stim_start,
                  colour = behavioral_category, alpha = duration))+
  scale_colour_viridis_d()+
  scale_y_continuous(name = 'time since start of stimulus\n(negative value = time until stimulus')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  labs(colour = 'behavioural category')+
  facet_wrap(. ~ stimulus, nrow = 3)

  
###### AT EVERY SECOND, WHAT ARE THEY DOING ######
### out of sight #### 
# clean environment
rm(list = ls()) ; gc()

# read in data
svs <- readRDS('../data_processed/elephants_behaviour_split_relative_to_stimulus_speakervehicle.RDS')

# standardise timings
#svs$time_std <- svs$time + (180 - svs$stim_start)
#svs$stim_start_std <- svs$stim_start + (180 - svs$stim_start)
#svs$stim_stop_std <- svs$stim_stop + (180 - svs$stim_start)

# extract out of sight timings
out_frame <- svs %>% filter(behaviour == 'OUT OF SIGHT: OUT OF SIGHT')

# identify times out of sight
out_frame$time_round <- round(out_frame$time, 0)

# read in start/end data
in_frame <- read_csv('../data_processed/elephants_time_in_frame.csv')
videos <- data.frame(pb_num = unique(in_frame$pb_num),
                     video_start = NA, video_end = NA)
for(i in 1:nrow(videos)){
  x <- in_frame[in_frame$pb_num == videos$pb_num[i], c('pb_num','video_start','video_end')] %>% distinct()
  videos$video_start[i] <- min(x$video_start)
  videos$video_end[i] <- max(x$video_end)
}
rm(x, i)

# create data frame to fill in
pb1_start <- ceiling(videos$video_start[videos$pb_num == 1])
pb1_end <- ceiling(videos$video_end[videos$pb_num == 1])
pb1_length <- pb1_end - pb1_start
by_sec <- data.frame(subject = rep(unique(svs$subject[svs$pb_num == 1]), each = (pb1_length+1)),
                     time_round = pb1_start:pb1_end)
for(i in 2:nrow(videos)){
  pb_start <- floor(videos$video_start[i])
  pb_end <- ceiling(videos$video_end[i])
  pb_length <- pb_end - pb_start
  new <- data.frame(subject = rep(unique(svs$subject[svs$pb_num == videos$pb_num[i]]),
                                  each = (pb_length+1)),
                    time_round = pb_start:pb_end)
  by_sec <- rbind(by_sec, new)
}
rm(i, pb_end, pb_length, pb_start, pb1_end, pb1_length, pb1_start) ; gc()

# add in out of sight times
by_sec <- by_sec %>% 
  left_join(out_frame[, c('subject','time_round','status')],
            by = c('subject','time_round')) %>% 
  rename(out_frame = status,
         second = time_round) %>% 
  mutate(ears = NA, trunk = NA, tail = NA)

# fill in times of static behaviour: out of sight
filled_rows <- which(is.na(by_sec$out_frame) == FALSE)
for(i in 1:(length(filled_rows)-1) ) {
  if(by_sec[filled_rows[i],'out_frame'] == 'START') {
    by_sec[(filled_rows[i]:(filled_rows[i+1]-1) ),'out_frame'] <- 'out_of_sight'
  }
}
rm(filled_rows); gc()

# standardise
by_sec$out_frame <- ifelse(by_sec$out_frame == 'STOP', 'out_of_sight', by_sec$out_frame)
by_sec$ears <- ifelse(by_sec$out_frame == 'out_of_sight', 'out_of_sight', by_sec$ears)
by_sec$tail <- ifelse(by_sec$out_frame == 'out_of_sight', 'out_of_sight', by_sec$tail)
by_sec$trunk <- ifelse(by_sec$out_frame == 'out_of_sight', 'out_of_sight', by_sec$trunk)

# fill in blanks
by_sec$out_frame <- ifelse( is.na(by_sec$out_frame) == TRUE, 'in_frame', by_sec$out_frame)

### stress ####
# import data
svs <- readRDS('../data_processed/elephants_behaviour_startstop_speakervehiclestress.RData')

# split into behavioural categories
stress <- svs %>% filter(behavioral_category == 'stress')# %>% rbind(out_frame)
svs_look <- svs %>% filter(behavioral_category == 'look')# %>% rbind(out_frame)
svs_move <- svs %>% filter(behavioral_category == 'move')# %>% rbind(out_frame)

# identify times of changing behaviour: stress
for(i in 1:nrow(stress)){
  by_sec[which(by_sec$second == round(stress$start_time[i], 0) &
                 by_sec$subject == stress$subject[i]),
         which(colnames(by_sec) == stress$type[i])] <- stress$action[i]
}

# fill in times of static behaviour: stress
for(i in 1:nrow(by_sec)) {
  for(j in 4:ncol(by_sec)) {
    if( is.na(by_sec[i,j] == TRUE) ) {
      by_sec[i,j] <- by_sec[(i-1), j]
    }
  }
}

# save output
saveRDS(by_sec, '../data_processed/behaviour_by_second.RDS')

### speaker and vehicle looking direction ####
# by_sec <- readRDS('../data_processed/behaviour_by_second.RDS') ;  svs <- readRDS('../data_processed/elephants_behaviour_startstop_speakervehiclestress.RData') ; svs_look <- svs %>% filter(behavioral_category == 'look')
by_sec <- by_sec %>% mutate(speaker = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
                            vehicle = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA))

# identify times of changing behaviour: svs_look
for(i in 1:nrow(svs_look)){
  by_sec[which(by_sec$second == round(svs_look$start_time[i], 0) &
                 by_sec$subject == svs_look$subject[i]),
         which(colnames(by_sec) == svs_look$type[i])] <- svs_look$action[i]
}

# fill in times of static behaviour: svs_look
for(i in 1:nrow(by_sec)) {
  for(j in 7:ncol(by_sec)) {
    if( is.na(by_sec[i,j] == TRUE) ) {
      by_sec[i,j] <- by_sec[(i-1), j]
    }
  }
}

# rename speker and vehicle to indicate looking directions
by_sec <- by_sec %>% 
  rename(speaker_look = speaker,
         vehicle_look = vehicle)

# save output
saveRDS(by_sec, '../data_processed/behaviour_by_second.RDS')

### speaker and vehicle movement direction ####
# by_sec <- readRDS('../data_processed/behaviour_by_second.RDS') ; svs <- readRDS('../data_processed/elephants_behaviour_startstop_speakervehiclestress.RData') ; svs_move <- svs %>% filter(behavioral_category == 'move')
by_sec <- by_sec %>% mutate(speaker = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
                            vehicle = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA))

# identify times of changing behaviour: svs_move
for(i in 1:nrow(svs_move)){
  # add in stop times because won't always be one or another, sometimes there's nothing
  by_sec[which(by_sec$second == round(svs_move$stop_time[i], 0) &
                 by_sec$subject == svs_move$subject[i]),
         which(colnames(by_sec) == svs_move$type[i])] <- svs_move$action[i]
  # add in start times so where movement direction just changes instead of halting, shows new start
  by_sec[which(by_sec$second == round(svs_move$start_time[i], 0) &
                 by_sec$subject == svs_move$subject[i]),
         which(colnames(by_sec) == svs_move$type[i])] <- svs_move$action[i]
}

# fill in times of static behaviour: speaker/vehicle movement
for(behaviour in 1:length(unique(svs_move$behaviour))){
  for(elephant in 1:length(unique(by_sec$subject))){
    x <- svs_move[svs_move$behaviour == unique(svs_move$behaviour)[behaviour] &
                    svs_move$subject == unique(by_sec$subject)[elephant] , ]
    if(nrow(x) > 0){
      for( i in 1:nrow(x)){
        times <- seq(from = floor(x$start_time[i]), to = ceiling(x$stop_time[i]), by = 1)
        by_sec[which(by_sec$subject == unique(by_sec$subject)[elephant] &
                       by_sec$second %in% times),
               which(colnames(by_sec) == unique(svs_move$type)[behaviour])] <- unique(x$action)
      }
    }
  }
}

# fill in NA values, rename speaker and vehicle to indicate movement directions
by_sec <- by_sec %>% 
  mutate(speaker = ifelse( is.na(speaker) == TRUE, 'not_moving', speaker),
         vehicle = ifelse( is.na(vehicle) == TRUE, 'not_moving', vehicle)) %>% 
  rename(speaker_move = speaker,
         vehicle_move = vehicle)

# save output
saveRDS(by_sec, '../data_processed/behaviour_by_second.RDS')

# clean environment
rm(new, stress, svs, svs_look, svs_move, x, behaviour, elephant, i, j, times) ; gc()

### elephants looking direction ####
# by_sec <- readRDS('../data_processed/behaviour_by_second.RDS')

# import data
ele <- readRDS('../data_processed/elephants_behaviour_startstop_elephants.RData')
unique(ele$behavioral_category)
ele_look <- ele %>% 
  filter(behavioral_category == 'look')
ele_move <- ele %>% 
  filter(behavioral_category == 'move')
sort(unique(ele_look$elephant_look)) ; sort(unique(ele_move$elephant_look))
rm(ele) ; gc()

# create new columns for running
by_sec <- by_sec %>% 
  separate(subject, into = c('bull','pb_num'), sep = '_e', remove = F) %>% 
  mutate(b1 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b2 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b3 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b4 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b5 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b6 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b7 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b8 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA))

# identify times of changing behaviour: ele_look
for(i in 1:nrow(ele_look)){
  by_sec[which(by_sec$second == round(ele_look$start_time[i], 0) &
                 by_sec$subject == ele_look$subject[i]),
         which(colnames(by_sec) == ele_look$elephant_look[i])] <- ele_look$action[i]
}

# fill in times of static behaviour: ele_look
for(i in 1:nrow(by_sec)) {
  x <- ele_look[ele_look$subject == by_sec$subject[i],]
  partners <- sort(unique(x$elephant_look))
  for(partner in partners) {
    col_num <- which(colnames(by_sec) == partner)
    if( is.na(by_sec[i, col_num]) == TRUE ) {
      by_sec[i,col_num] <- by_sec[(i-1), col_num]
    }
  }
}

# fill in impossible dyads
for(row in 1:nrow(by_sec)) {
  for(column in 13:20){
    if(colnames(by_sec)[column] == by_sec$bull[row]){
      by_sec[row,column] <- 'impossible_partner'
    }
  }
}

# rename elephant columns to indicate looking directions
by_sec <- by_sec %>% 
  rename(b1_look = b1,
         b2_look = b2,
         b3_look = b3,
         b4_look = b4,
         b5_look = b5,
         b6_look = b6,
         b7_look = b7,
         b8_look = b8)

# save output
saveRDS(by_sec, '../data_processed/behaviour_by_second.RDS')

### elephants movement direction ####
# by_sec <- readRDS('../data_processed/behaviour_by_second.RDS') ; ele <- readRDS('../data_processed/elephants_behaviour_startstop_elephants.RData') ; ele_move <- ele %>% filter(behavioral_category == 'move')

# create new columns for running
by_sec <- by_sec %>% 
  mutate(b1 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b2 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b3 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b4 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b5 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b6 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b7 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b8 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA))

# identify times of changing behaviour: ele_move
for(i in 1:nrow(ele_move)){
  # add in stop times because won't always be one or another, sometimes there's nothing
  by_sec[which(by_sec$second == round(ele_move$stop_time[i], 0) &
                 by_sec$subject == ele_move$subject[i]),
         which(colnames(by_sec) == ele_move$elephant_look[i])] <- ele_move$action[i]
  # add in start times so where movement direction just changes instead of halting, shows new start
  by_sec[which(by_sec$second == round(ele_move$start_time[i], 0) &
                 by_sec$subject == ele_move$subject[i]),
         which(colnames(by_sec) == ele_move$elephant_look[i])] <- ele_move$action[i]
}

# fill in times of static behaviour: elephant movement
for(behaviour in 1:length(unique(ele_move$behaviour))){
  for(elephant in 1:length(unique(by_sec$subject))){
    x <- ele_move[ele_move$subject == unique(by_sec$subject)[elephant],]
    partners <- sort(unique(x$elephant_look))
    for(partner in partners){
      x <- ele_move[ele_move$behaviour == unique(ele_move$behaviour)[behaviour] &
                      ele_move$subject == unique(by_sec$subject)[elephant] & 
                      ele_move$elephant_look == partner, ]
      if(nrow(x) > 0){
        for( i in 1:nrow(x)){
          times <- seq(from = floor(x$start_time[i]), to = ceiling(x$stop_time[i]), by = 1)
          by_sec[which(by_sec$subject == unique(by_sec$subject)[elephant] &
                         by_sec$second %in% times),
                 which(colnames(by_sec) == partner)] <- unique(x$action)
        }
      }
    }
  }
}
rm(x, behaviour, elephant, i, partner, partners, times)

# fill in impossible dyads and zeroes
for(row in 1:nrow(by_sec)) {
  for(column in 21:28){
    if(colnames(by_sec)[column] == by_sec$bull[row]){
      by_sec[row,column] <- 'impossible_partner'
    }
  }
}

# fill in NA values, rename elephant columns to indicate looking directions
by_sec <- by_sec %>% 
  mutate(b1 = ifelse( is.na(b1) == TRUE, 'not_moving', b1),
         b2 = ifelse( is.na(b2) == TRUE, 'not_moving', b2),
         b3 = ifelse( is.na(b3) == TRUE, 'not_moving', b3),
         b4 = ifelse( is.na(b4) == TRUE, 'not_moving', b4),
         b5 = ifelse( is.na(b5) == TRUE, 'not_moving', b5),
         b6 = ifelse( is.na(b6) == TRUE, 'not_moving', b6),
         b7 = ifelse( is.na(b7) == TRUE, 'not_moving', b7),
         b8 = ifelse( is.na(b8) == TRUE, 'not_moving', b8)) %>% 
  rename(b1_move = b1,
         b2_move = b2,
         b3_move = b3,
         b4_move = b4,
         b5_move = b5,
         b6_move = b6,
         b7_move = b7,
         b8_move = b8)

# save output
saveRDS(by_sec, '../data_processed/behaviour_by_second.RDS')

# clean environment
rm(list = ls()[!ls() %in% c('by_sec', 'videos')]) ; gc()

### nearest neighbour ####
# by_sec <- readRDS('../data_processed/behaviour_by_second.RDS')
by_sec <- by_sec[,1:28]

# import data
nn <- readRDS('../data_processed/elephants_behaviour_startstop_neighbour.RData')

# create new columns for running
by_sec <- by_sec %>% 
  mutate(neighbour = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b1 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b2 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b3 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b4 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b5 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b6 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b7 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b8 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA))

# fill in times of each behaviour: neighbour
for(behaviour in 1:length(unique(nn$behaviour))){
  for(elephant in 1:length(unique(by_sec$subject))){
    x <- nn[nn$subject == unique(by_sec$subject)[elephant],]
    partners <- sort(unique(x$neighbour))
    for(partner in partners){
      x <- nn[nn$behaviour == unique(nn$behaviour)[behaviour] &
                      nn$subject == unique(by_sec$subject)[elephant] & 
                      nn$neighbour == partner, ]
      if(nrow(x) > 0){
        for( i in 1:nrow(x)){
          times <- seq(from = round(x$start_time[i]), to = round(x$stop_time[i]), by = 1)
          by_sec[which(by_sec$subject == unique(by_sec$subject)[elephant] &
                         by_sec$second %in% times),
                 'neighbour'] <- unique(x$neighbour)
          by_sec[which(by_sec$subject == unique(by_sec$subject)[elephant] &
                         by_sec$second %in% times),
                 which(colnames(by_sec) == partner)] <- 1
        }
      }
    }
  }
}

# remove times with 2 nearest neighbours at the same time
for(i in 1:nrow(by_sec)){
  if( length(which(is.na(by_sec[i,30:37]) == FALSE)) > 1 ){
    for(j in 30:37){
      old_neighbour <- colnames(by_sec)[(30:37)[!is.na(by_sec[(i-1),30:37])]]
      by_sec[i,old_neighbour] <- NA
    }
  }
}
rm(x, behaviour, elephant, i, partner, partners, times)

# fill in impossible dyads
for(row in 1:nrow(by_sec)) {
  for(column in 30:37){
    if(colnames(by_sec)[column] == by_sec$bull[row]){
      by_sec[row,column] <- 'impossible_partner'
    }
  }
}
rm(column, j, old_neighbour, row) ; gc()

# rename elephant columns to indicate looking directions
by_sec <- by_sec %>% 
  mutate(b1 = ifelse( is.na(b1) == TRUE, '0', b1),
         b2 = ifelse( is.na(b2) == TRUE, '0', b2),
         b3 = ifelse( is.na(b3) == TRUE, '0', b3),
         b4 = ifelse( is.na(b4) == TRUE, '0', b4),
         b5 = ifelse( is.na(b5) == TRUE, '0', b5),
         b6 = ifelse( is.na(b6) == TRUE, '0', b6),
         b7 = ifelse( is.na(b7) == TRUE, '0', b7),
         b8 = ifelse( is.na(b8) == TRUE, '0', b8)) %>% 
  rename(b1_nn = b1,
         b2_nn = b2,
         b3_nn = b3,
         b4_nn = b4,
         b5_nn = b5,
         b6_nn = b6,
         b7_nn = b7,
         b8_nn = b8)

# save output
saveRDS(by_sec, '../data_processed/behaviour_by_second.RDS')

### social behaviour ####
# by_sec <- readRDS('../data_processed/behaviour_by_second.RDS')

# import data
soc <- readRDS('../data_processed/elephants_behaviour_startstop_socialtouch.RData')

# create new columns for running
by_sec <- by_sec %>% 
  mutate(social = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b1 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b2 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b3 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b4 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b5 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b6 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b7 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA),
         b8 = ifelse(out_frame == 'out_of_sight', 'out_of_sight', NA))

# identify times of changing behaviour: social touch
for(i in 1:nrow(soc)){
  row_num_start <- which(by_sec$second == ceiling(soc$start_time[i]) & by_sec$subject == soc$subject[i])
  row_num_stop <- which(by_sec$second == round(soc$stop_time[i], 0) & by_sec$subject == soc$subject[i])
  by_sec[row_num_start:row_num_stop, 'social'] <- soc$partner[i]
  by_sec[row_num_start:row_num_stop, which(colnames(by_sec) == soc$partner[i])] <- soc$initiated_by[i]
}

check <- by_sec %>%
  filter(!is.na(social)) %>% 
  filter(out_frame == 'in_frame') %>% 
  select(c(1:5,38:46)) %>% 
  mutate(unique = paste0(subject, second)) %>% 
  group_by(unique) %>% 
  mutate(num_touch = length(which(is.na(c(b1,b2,b3,b4,b5,b6,b7,b8)) == FALSE))) %>% 
  filter(num_touch > 1) # all 3 individuals were in contact on this occaison: this is correct

# fill in impossible dyads and zeroes
for(row in 1:nrow(by_sec)) {
  for(column in 38:46){
    if(colnames(by_sec)[column] == by_sec$bull[row]){
      by_sec[row,column] <- 'impossible_partner'
    }
  }
}

# rename elephant columns to indicate looking directions
by_sec <- by_sec %>% 
  mutate(social = ifelse( is.na(social) == TRUE, 'no contact', social),
         b1 = ifelse( is.na(b1) == TRUE, '0', b1),
         b2 = ifelse( is.na(b2) == TRUE, '0', b2),
         b3 = ifelse( is.na(b3) == TRUE, '0', b3),
         b4 = ifelse( is.na(b4) == TRUE, '0', b4),
         b5 = ifelse( is.na(b5) == TRUE, '0', b5),
         b6 = ifelse( is.na(b6) == TRUE, '0', b6),
         b7 = ifelse( is.na(b7) == TRUE, '0', b7),
         b8 = ifelse( is.na(b8) == TRUE, '0', b8)) %>% 
  rename(b1_social = b1,
         b2_social = b2,
         b3_social = b3,
         b4_social = b4,
         b5_social = b5,
         b6_social = b6,
         b7_social = b7,
         b8_social = b8)

# save output
saveRDS(by_sec, '../data_processed/behaviour_by_second.RDS')


















# fill in times of each behaviour: social touch
for(behaviour in 1:length(unique(soc$behaviour))){
  for(elephant in 1:length(unique(by_sec$subject))){
    x <- soc[soc$subject == unique(by_sec$subject)[elephant],]
    partners <- sort(unique(x$neighbour))
    for(partner in partners){
      x <- soc[soc$behaviour == unique(soc$behaviour)[behaviour] &
                 soc$subject == unique(by_sec$subject)[elephant] & 
                 soc$neighbour == partner, ]
      if(nrow(x) > 0){
        for( i in 1:nrow(x)){
          times <- seq(from = round(x$start_time[i]), to = round(x$stop_time[i]), by = 1)
          by_sec[which(by_sec$subject == unique(by_sec$subject)[elephant] &
                         by_sec$second %in% times),
                 'neighbour'] <- unique(x$neighbour)
          by_sec[which(by_sec$subject == unique(by_sec$subject)[elephant] &
                         by_sec$second %in% times),
                 which(colnames(by_sec) == partner)] <- 1
        }
      }
    }
  }
}

# remove times with 2 nearest neighbours at the same time
for(i in 1:nrow(by_sec)){
  if( length(which(is.na(by_sec[i,30:37]) == FALSE)) > 1 ){
    for(j in 30:37){
      old_neighbour <- colnames(by_sec)[(30:37)[!is.na(by_sec[(i-1),30:37])]]
      by_sec[i,old_neighbour] <- NA
    }
  }
}
rm(x, behaviour, elephant, i, partner, partners, times)

# fill in impossible dyads
for(row in 1:nrow(by_sec)) {
  for(column in 30:37){
    if(colnames(by_sec)[column] == by_sec$bull[row]){
      by_sec[row,column] <- 'impossible_partner'
    }
  }
}
rm(column, j, old_neighbour, row) ; gc()

# rename elephant columns to indicate looking directions
by_sec <- by_sec %>% 
  mutate(b1 = ifelse( is.na(b1) == TRUE, '0', b1),
         b2 = ifelse( is.na(b2) == TRUE, '0', b2),
         b3 = ifelse( is.na(b3) == TRUE, '0', b3),
         b4 = ifelse( is.na(b4) == TRUE, '0', b4),
         b5 = ifelse( is.na(b5) == TRUE, '0', b5),
         b6 = ifelse( is.na(b6) == TRUE, '0', b6),
         b7 = ifelse( is.na(b7) == TRUE, '0', b7),
         b8 = ifelse( is.na(b8) == TRUE, '0', b8)) %>% 
  rename(b1_nn = b1,
         b2_nn = b2,
         b3_nn = b3,
         b4_nn = b4,
         b5_nn = b5,
         b6_nn = b6,
         b7_nn = b7,
         b8_nn = b8)

# save output
saveRDS(by_sec, '../data_processed/behaviour_by_second.RDS')

### convert to long format and remove impossible dyads ####
# by_sec <- readRDS('../data_processed/behaviour_by_second.RDS')
rm(list = ls() [ !ls() %in% c('by_sec','videos') ])

by_sec_long <- by_sec %>% 
  pivot_longer(cols = 6:45, names_to = 'behaviour_type', values_to = 'action') %>% 
  filter(action != 'impossible_partner') %>% 
  separate(behaviour_type, into = c('target','type'), remove = F) %>% 
  mutate(type = ifelse(target == 'ears' | target == 'trunk' | target == 'tail', 'stress',
                       ifelse(target == 'neighbour', 'intitiated_by', type))) %>% 
  mutate(partner = ifelse( target == 'stress' | target == 'neighbour' | target == 'speaker' | target == 'vehicle',
                           subject, paste0(target, '_e', pb_num))) %>% 
  filter(partner %in% unique(by_sec$subject)) %>% 
  mutate(partner = ifelse(partner == subject, NA, partner),
         target = ifelse(target == 'b1' | target == 'b2' | target == 'b3' | target == 'b4' |
                           target == 'b5' | target == 'b6' | target == 'b7' | target == 'b8',
                         'elephant', target))

saveRDS(by_sec_long, '../data_processed/behaviour_by_second_long.RDS')

# obtain number of elephants with behaviour observed for each playback
subjects <- data.frame(subject = sort(unique(by_sec$subject))) %>% 
  separate(subject, into = c('bull','pb_num'), sep = '_e', remove = F) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(videos, by = 'pb_num') %>% 
  group_by(pb_num) %>% 
  mutate(monitored_elephants = length(unique(subject))) %>% 
  ungroup()

# add new columns for identifying who is present (make non-existent partners NA)
by_sec_wide <- by_sec %>% 
  mutate(b1 = NA, b2 = NA, b3 = NA, b4 = NA,
         b5 = NA, b6 = NA, b7 = NA, b8 = NA)

# 1 = elephant present in group, 0 = elephant absent (e.g. only 4 elephants in first experiment threrefore all rows where pb_num=1 will have 1 for b1,b2,b3,b4 and 0 for b5,b6,b7,b8)
for( i in 1:nrow(by_sec_wide)) {
  for( j in 1:8){
    column_name <- paste0('b',j)
    by_sec_wide[i,column_name] <- ifelse(unique(subjects$monitored_elephants[subjects$pb_num == by_sec$pb_num[i]]) >= j, 1, 0)
  }
}

# set values for all columns for NA where the partner elephant does not exist
by_sec_wide <- by_sec_wide %>% 
  mutate(b1_look = ifelse(b1 == 0, NA, b1_look), b2_look = ifelse(b2 == 0, NA, b2_look),
         b3_look = ifelse(b3 == 0, NA, b3_look), b4_look = ifelse(b4 == 0, NA, b4_look),
         b5_look = ifelse(b5 == 0, NA, b5_look), b6_look = ifelse(b6 == 0, NA, b6_look),
         b7_look = ifelse(b7 == 0, NA, b7_look), b8_look = ifelse(b8 == 0, NA, b8_look)) %>% 
  mutate(b1_move = ifelse(b1 == 0, NA, b1_move), b2_move = ifelse(b2 == 0, NA, b2_move),
         b3_move = ifelse(b3 == 0, NA, b3_move), b4_move = ifelse(b4 == 0, NA, b4_move),
         b5_move = ifelse(b5 == 0, NA, b5_move), b6_move = ifelse(b6 == 0, NA, b6_move),
         b7_move = ifelse(b7 == 0, NA, b7_move), b8_move = ifelse(b8 == 0, NA, b8_move)) %>% 
  mutate(b1_nn  = ifelse(b1 == 0, NA, b1_nn), b2_nn = ifelse(b2 == 0, NA, b2_nn),
         b3_nn = ifelse(b3 == 0, NA, b3_nn), b4_nn = ifelse(b4 == 0, NA, b4_nn),
         b5_nn = ifelse(b5 == 0, NA, b5_nn), b6_nn = ifelse(b6 == 0, NA, b6_nn),
         b7_nn = ifelse(b7 == 0, NA, b7_nn), b8_nn = ifelse(b8 == 0, NA, b8_nn)) %>% 
  mutate(b1_social = ifelse(b1 == 0, NA, b1_social), b2_social = ifelse(b2 == 0, NA, b2_social),
         b3_social = ifelse(b3 == 0, NA, b3_social), b4_social = ifelse(b4 == 0, NA, b4_social),
         b5_social = ifelse(b5 == 0, NA, b5_social), b6_social = ifelse(b6 == 0, NA, b6_social),
         b7_social = ifelse(b7 == 0, NA, b7_social), b8_social = ifelse(b8 == 0, NA, b8_social)) %>% 
  rename(b1_present = b1, b2_present = b2, b3_present = b3, b4_present = b4,
         b5_present = b5, b6_present = b6, b7_present = b7, b8_present = b8)

# convert to index variable
by_sec_index <- by_sec_wide
for( j in 5:45 ){
  by_sec_index[,j] <- as.integer(as.factor(by_sec_index[,j]))
}

# combine to single data frame so have both index variable and name together
by_sec_index2 <- by_sec_index %>% 
  cbind(by_sec_wide[,5:46])

# reorder columns
colnames(by_sec_index2) <- c("subject","bull","pb_num","second",
                             "out_frame_index","ears_index","trunk_index","tail_index",
                             "speaker_look_index","vehicle_look_index",
                             "speaker_move_index","vehicle_move_index",
                             "b1_look_index","b2_look_index","b3_look_index","b4_look_index",
                             "b5_look_index","b6_look_index","b7_look_index","b8_look_index",
                             "b1_move_index","b2_move_index","b3_move_index","b4_move_index",
                             "b5_move_index","b6_move_index","b7_move_index","b8_move_index",
                             "neighbour_index",
                             "b1_nn_index","b2_nn_index","b3_nn_index","b4_nn_index",
                             "b5_nn_index","b6_nn_index","b7_nn_index","b8_nn_index",
                             "social_index",
                             "b1_social_index","b2_social_index","b3_social_index","b4_social_index",
                             "b5_social_index","b6_social_index","b7_social_index","b8_social_index",
                             "b1_present_index","b2_present_index","b3_present_index","b4_present_index",
                             "b5_present_index","b6_present_index","b7_present_index","b8_present_index",
                             "unique_index",
                             "out_frame_name",
                             "ears_name","trunk_name","tail_name",
                             "speaker_look_name","vehicle_look_name",
                             "speaker_move_name","vehicle_move_name",
                             "b1_look_name","b2_look_name","b3_look_name","b4_look_name",
                             "b5_look_name","b6_look_name","b7_look_name","b8_look_name",
                             "b1_move_name","b2_move_name","b3_move_name","b4_move_name",
                             "b5_move_name","b6_move_name","b7_move_name","b8_move_name",
                             "neighbour_name",
                             "b1_nn_name","b2_nn_name","b3_nn_name","b4_nn_name",
                             "b5_nn_name","b6_nn_name","b7_nn_name","b8_nn_name",
                             "social_name",
                             "b1_social_name","b2_social_name","b3_social_name","b4_social_name",
                             "b5_social_name","b6_social_name","b7_social_name","b8_social_name")
col_order <- c("subject","bull","pb_num","second",
               "out_frame_name","out_frame_index",
               "ears_name","ears_index","trunk_name","trunk_index","tail_name","tail_index",
               "speaker_look_name","speaker_look_index","vehicle_look_name","vehicle_look_index",
               "speaker_move_name","speaker_move_index","vehicle_move_name","vehicle_move_index",
               "b1_look_name","b1_look_index","b2_look_name","b2_look_index",
               "b3_look_name","b3_look_index","b4_look_name","b4_look_index",
               "b5_look_name","b5_look_index","b6_look_name","b6_look_index",
               "b7_look_name","b7_look_index","b8_look_name","b8_look_index",
               "b1_move_name","b1_move_index","b2_move_name","b2_move_index",
               "b3_move_name","b3_move_index","b4_move_name","b4_move_index",
               "b5_move_name","b5_move_index","b6_move_name","b6_move_index",
               "b7_move_name","b7_move_index","b8_move_name","b8_move_index",
               "neighbour_name","neighbour_index",
               "b1_nn_name","b1_nn_index","b2_nn_name","b2_nn_index",
               "b3_nn_name","b3_nn_index","b4_nn_name","b4_nn_index",
               "b5_nn_name","b5_nn_index","b6_nn_name","b6_nn_index",
               "b7_nn_name","b7_nn_index","b8_nn_name","b8_nn_index",
               "social_name","social_index",
               "b1_social_name","b1_social_index","b2_social_name","b2_social_index",
               "b3_social_name","b3_social_index","b4_social_name","b4_social_index",
               "b5_social_name","b5_social_index","b6_social_name","b6_social_index",
               "b7_social_name","b7_social_index","b8_social_name","b8_social_index",
               "b1_present_index","b2_present_index","b3_present_index","b4_present_index",
               "b5_present_index","b6_present_index","b7_present_index","b8_present_index")
by_sec_index2 <- by_sec_index2 %>% 
  select(all_of(col_order))

saveRDS(by_sec_index2, '../data_processed/behaviour_by_second_indexvariables.RDS')

### graphs ####
by_sec <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>% 
  mutate(b1_nn_name = ifelse(b1_nn_name == '0', 'not_nearest',
                             ifelse(b1_nn_name == '1', 'nearest_neighbour', b1_nn_name))) %>% 
  mutate(b2_nn_name = ifelse(b2_nn_name == '0', 'not_nearest',
                             ifelse(b2_nn_name == '1', 'nearest_neighbour', b2_nn_name))) %>% 
  mutate(b3_nn_name = ifelse(b3_nn_name == '0', 'not_nearest',
                             ifelse(b3_nn_name == '1', 'nearest_neighbour', b3_nn_name))) %>% 
  mutate(b4_nn_name = ifelse(b4_nn_name == '0', 'not_nearest',
                             ifelse(b4_nn_name == '1', 'nearest_neighbour', b4_nn_name))) %>% 
  mutate(b5_nn_name = ifelse(b5_nn_name == '0', 'not_nearest',
                             ifelse(b5_nn_name == '1', 'nearest_neighbour', b5_nn_name))) %>% 
  mutate(b6_nn_name = ifelse(b6_nn_name == '0', 'not_nearest',
                             ifelse(b6_nn_name == '1', 'nearest_neighbour', b6_nn_name))) %>% 
  mutate(b7_nn_name = ifelse(b7_nn_name == '0', 'not_nearest',
                             ifelse(b7_nn_name == '1', 'nearest_neighbour', b7_nn_name))) %>% 
  mutate(b8_nn_name = ifelse(b8_nn_name == '0', 'not_nearest',
                             ifelse(b8_nn_name == '1', 'nearest_neighbour', b8_nn_name)))

ages <- readRDS('../data_processed/elephants.RDS') %>% 
  select(subject, bull, age, pb_num) %>% 
  distinct()

stimuli <- readRDS('../data_processed/stimuli.RDS') %>% 
  filter(behavior == 'STIMULUS')

b1_e1 <- by_sec %>%
  filter(subject == 'b1_e1') %>% 
  select("second","ears_name","trunk_name","tail_name",
         "speaker_look_name","vehicle_look_name","speaker_move_name","vehicle_move_name",
         "b1_look_name","b2_look_name","b3_look_name","b4_look_name",
         "b1_move_name","b2_move_name","b3_move_name","b4_move_name",
         "neighbour_name","b1_nn_name","b2_nn_name","b3_nn_name","b4_nn_name",
         "social_name","b1_social_name","b2_social_name","b3_social_name","b4_social_name")
ages_e1 <- ages %>% 
  filter(pb_num == 1) %>% 
  mutate(partner_look = paste0('b',bull),
         partner_move = paste0('b',bull),
         partner_age = age)
stim_e1 <- stimuli %>% 
  filter(pb_num == 1) %>% 
  select(time, status) %>% 
  mutate(second = round(as.numeric(time), 0))

look <- b1_e1 %>% 
  select("second","b1_look_name","b2_look_name","b3_look_name","b4_look_name") %>% 
  pivot_longer(cols = c("b1_look_name","b2_look_name","b3_look_name","b4_look_name"),
               names_to = 'partner_look', values_to = 'direction_look') %>% 
  filter(direction_look != 'impossible_partner') %>% 
  separate(partner_look, into = c('partner_look','look_name'), sep = 2) %>% 
  select(-look_name) %>% 
  left_join(ages_e1[,c('partner_look','partner_age')], by = 'partner_look') %>% 
  mutate(direction = ifelse(direction_look == 'look at directly',3,
                            ifelse(direction_look == 'side-on',2,1)))
ggplot()+
  geom_rect(data = stim_e1, aes(xmin = second[which(status == 'START')], xmax = second[which(status == 'STOP')],
                                ymin = min(look$direction)-0.5, ymax = max(look$direction)+0.5),
            fill = 'grey')+
  geom_line(data = look, mapping = aes(x = second, y = direction, colour = partner_look))+
  coord_cartesian(ylim = c(min(look$direction)-0.1, max(look$direction)+0.1), expand = FALSE)

move <- b1_e1 %>% 
  select("second","b1_move_name","b2_move_name","b3_move_name","b4_move_name") %>% 
  pivot_longer(cols = c("b1_move_name","b2_move_name","b3_move_name","b4_move_name"),
               names_to = 'partner_move', values_to = 'direction_move') %>% 
  filter(direction_move != 'impossible_partner') %>% 
  separate(partner_move, into = c('partner_move','move_name'), sep = 2) %>% 
  select(-move_name) %>% 
  left_join(ages_e1[,c('partner_move','partner_age')], by = 'partner_move') %>% 
  mutate(direction = ifelse(direction_move == 'approach directly',5,
                            ifelse(direction_move == 'approach at an angle',4,
                                   ifelse(direction_move == 'move directly with',3,
                                          ifelse(direction_move == 'move away at an angle',2,
                                                 ifelse(direction_move == 'move away directly',1, NA))))))
ggplot()+
  geom_rect(data = stim_e1, aes(xmin = second[which(status == 'START')], xmax = second[which(status == 'STOP')],
                                ymin = min(move$direction, na.rm = T)-0.5, ymax = max(move$direction, na.rm = T)+0.5),
            fill = 'grey')+
  geom_line(data = move, mapping = aes(x = second, y = direction, colour = partner_move))+
  coord_cartesian(ylim = c(min(move$direction, na.rm = T)-0.1, max(move$direction, na.rm = T)+0.1), expand = FALSE)








e3 <- by_sec %>%
  filter(pb_num == 3) %>% 
  select("subject","pb_num","second","ears_name","trunk_name","tail_name",
         "speaker_look_name","vehicle_look_name","speaker_move_name","vehicle_move_name",
         "b1_look_name","b2_look_name","b3_look_name","b4_look_name",
         "b5_look_name","b6_look_name","b7_look_name","b8_look_name",
         "b1_move_name","b2_move_name","b3_move_name","b4_move_name",
         "b5_move_name","b6_move_name","b7_move_name","b8_move_name",
         "neighbour_name","b1_nn_name","b2_nn_name","b3_nn_name","b4_nn_name","b5_nn_name","b6_nn_name","b7_nn_name","b8_nn_name",
         "social_name","b1_social_name","b2_social_name","b3_social_name","b4_social_name","b5_social_name","b6_social_name","b7_social_name","b8_social_name")
ages_e3 <- ages %>% 
  filter(pb_num == 3) %>% 
  mutate(partner_look = paste0('b',bull),
         partner_move = paste0('b',bull),
         partner_age = age)
stim_e3 <- stimuli %>% 
  filter(pb_num == 3) %>% 
  select(time, status) %>% 
  mutate(second = round(as.numeric(time), 0)) %>% 
  mutate(second_std = second - min(second))

look <- e3 %>% 
  select("subject","pb_num","second","b1_look_name","b2_look_name","b3_look_name","b4_look_name","b5_look_name","b6_look_name","b7_look_name","b8_look_name") %>% 
  pivot_longer(cols = c("b1_look_name","b2_look_name","b3_look_name","b4_look_name","b5_look_name","b6_look_name","b7_look_name","b8_look_name"),
               names_to = 'partner_look', values_to = 'direction_look') %>% 
  filter(direction_look != 'impossible_partner') %>% 
  left_join(ages_e3[,c('subject','age')], by = 'subject') %>% 
  separate(partner_look, into = c('partner_look','look_name'), sep = 2) %>% 
  select(-look_name) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(ages_e3[,c('partner_look','pb_num','partner_age')], by = c('partner_look','pb_num')) %>% 
  mutate(age = ifelse(age == '16-25','21-25',age),
         partner_age = ifelse(partner_age == '16-25','21-25',partner_age)) %>% 
  mutate(age_focal = as.integer(as.factor(age)),
         age_partner = as.integer(as.factor(partner_age))) %>% 
  mutate(age_difference = ifelse(age_focal == age_partner, 'age matched',
                                 ifelse(age_focal > age_partner, 'older than partner', 'younger than partner')),
         direction = ifelse(direction_look == 'look at directly',3,
                            ifelse(direction_look == 'side-on',2,
                                   ifelse(direction_look == 'look directly away',1, NA)))) %>% 
  mutate(second_std = second - min(stim_e3$second)) %>% 
  separate(subject, into = c('bull', 'exp'), sep = '_', remove = F) %>% 
  select(-exp) %>% 
  mutate(bull = paste0(bull, ' (focal)'),
         partner_look = paste0(partner_look, ' (target)'))

ggplot()+
  #facet_wrap(. ~ subject)+
  facet_grid(partner_look ~ bull)+
  annotate('rect',xmin = stim_e3$second_std[which(stim_e3$status == 'START')],
           xmax = stim_e3$second_std[which(stim_e3$status == 'STOP')],
           ymin = min(look$direction, na.rm = T)-0.5,
           ymax = max(look$direction, na.rm = T)+0.5,
           fill = 'grey')+
  geom_line(data = look, mapping = aes(x = second_std, y = direction, 
                                       colour = age_difference, group = partner_look))+
  coord_cartesian(ylim = c(min(look$direction, na.rm = T)-0.1, max(look$direction, na.rm = T)+0.1), expand = FALSE)+
  #theme(legend.position = c(0.92, 0.05),
  #      legend.justification = c(1, 0))+
  theme(legend.position = 'bottom')+#,
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank())+
  # scale_y_continuous(breaks = 1:3, labels = c('away','side','face'))+
  labs(colour = 'age relative to partner')

e7 <- by_sec %>%
  filter(pb_num == 7) %>% 
  select("subject","pb_num","second","ears_name","trunk_name","tail_name",
         "speaker_look_name","vehicle_look_name","speaker_move_name","vehicle_move_name",
         "b1_look_name","b2_look_name","b3_look_name","b4_look_name",
         "b5_look_name","b6_look_name","b7_look_name","b8_look_name",
         "b1_move_name","b2_move_name","b3_move_name","b4_move_name",
         "b5_move_name","b6_move_name","b7_move_name","b8_move_name",
         "neighbour_name","b1_nn_name","b2_nn_name","b3_nn_name","b4_nn_name","b5_nn_name","b6_nn_name","b7_nn_name","b8_nn_name",
         "social_name","b1_social_name","b2_social_name","b3_social_name","b4_social_name","b5_social_name","b6_social_name","b7_social_name","b8_social_name")
ages_e7 <- ages %>% 
  filter(pb_num == 7) %>% 
  mutate(partner_look = paste0('b',bull),
         partner_move = paste0('b',bull),
         partner_age = age)
stim_e7 <- stimuli %>% 
  filter(pb_num == 7) %>% 
  select(time, status) %>% 
  mutate(second = round(as.numeric(time), 0)) %>% 
  mutate(second_std = second - min(second))

look <- e7 %>% 
  select("subject","pb_num","second","b1_look_name","b2_look_name","b3_look_name","b4_look_name","b5_look_name","b6_look_name","b7_look_name","b8_look_name") %>% 
  pivot_longer(cols = c("b1_look_name","b2_look_name","b3_look_name","b4_look_name","b5_look_name","b6_look_name","b7_look_name","b8_look_name"),
               names_to = 'partner_look', values_to = 'direction_look') %>% 
  filter(direction_look != 'impossible_partner') %>% 
  left_join(ages_e7[,c('subject','age')], by = 'subject') %>% 
  separate(partner_look, into = c('partner_look','look_name'), sep = 2) %>% 
  select(-look_name) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(ages_e7[,c('partner_look','pb_num','partner_age')], by = c('partner_look','pb_num')) %>% 
  mutate(age = ifelse(age == '16-25','21-25',age),
         partner_age = ifelse(partner_age == '16-25','21-25',partner_age)) %>% 
  mutate(age_focal = as.integer(as.factor(age)),
         age_partner = as.integer(as.factor(partner_age))) %>% 
  mutate(age_difference = ifelse(age_focal == age_partner, 'age matched',
                                 ifelse(age_focal > age_partner, 'older than partner', 'younger than partner')),
         direction = ifelse(direction_look == 'look at directly',3,
                            ifelse(direction_look == 'side-on',2,
                                   ifelse(direction_look == 'look directly away',1, NA)))) %>% 
  mutate(second_std = second - min(stim_e7$second)) %>% 
  separate(subject, into = c('bull', 'exp'), sep = '_', remove = F) %>% 
  select(-exp) %>% 
  mutate(bull = paste0(bull, ' (focal)'),
         partner_look = paste0(partner_look, ' (target)'))

ggplot()+
  #facet_wrap(. ~ subject)+
  facet_grid(partner_look ~ bull)+
  annotate('rect',xmin = stim_e7$second_std[which(stim_e7$status == 'START')],
           xmax = stim_e7$second_std[which(stim_e7$status == 'STOP')],
           ymin = min(look$direction, na.rm = T)-0.5,
           ymax = max(look$direction, na.rm = T)+0.5,
           fill = 'grey')+
  geom_line(data = look, mapping = aes(x = second_std, y = direction, 
                                       colour = age_difference, group = partner_look))+
  coord_cartesian(ylim = c(min(look$direction, na.rm = T)-0.1, max(look$direction, na.rm = T)+0.1), expand = FALSE)+
  #theme(legend.position = c(0.92, 0.05),
  #      legend.justification = c(1, 0))+
  theme(legend.position = 'bottom')+#,
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank())+
  # scale_y_continuous(breaks = 1:3, labels = c('away','side','face'))+
  labs(colour = 'age relative to partner')













plot <- by_sec %>%
  select("subject","bull","pb_num","second","ears_name","trunk_name","tail_name",
         "speaker_look_name","vehicle_look_name","speaker_move_name","vehicle_move_name",
         "b1_look_name","b2_look_name","b3_look_name","b4_look_name","b5_look_name","b6_look_name","b7_look_name","b8_look_name",
         "b1_move_name","b2_move_name","b3_move_name","b4_move_name","b5_move_name","b6_move_name","b7_move_name","b8_move_name",
         "neighbour_name","b1_nn_name","b2_nn_name","b3_nn_name","b4_nn_name","b5_nn_name","b6_nn_name","b7_nn_name","b8_nn_name",
         "social_name","b1_social_name","b2_social_name","b3_social_name","b4_social_name","b5_social_name","b6_social_name","b7_social_name","b8_social_name",
         "b1_present_index","b2_present_index","b3_present_index","b4_present_index","b5_present_index","b6_present_index","b7_present_index","b8_present_index")
ages <- ages %>% 
  mutate(partner_look = paste0('b',bull),
         partner_move = paste0('b',bull),
         partner_age = age)
stim <- stimuli %>% 
  select(pb_num, time, status) %>% 
  mutate(second = round(as.numeric(time), 0))

look <- plot %>% 
  select("subject","pb_num","bull","second","b1_look_name","b2_look_name","b3_look_name","b4_look_name","b5_look_name","b6_look_name","b7_look_name","b8_look_name") %>% 
  pivot_longer(cols = c("b1_look_name","b2_look_name","b3_look_name","b4_look_name","b5_look_name","b6_look_name","b7_look_name","b8_look_name"),
               names_to = 'partner_look', values_to = 'direction_look') %>% 
  filter(direction_look != 'impossible_partner') %>% 
  left_join(ages[,c('subject','age')], by = 'subject') %>% 
  separate(partner_look, into = c('partner_look','look_name'), sep = 2) %>% 
  select(-look_name) %>% 
  mutate(pb_num = as.numeric(pb_num)) %>% 
  left_join(ages[,c('partner_look','pb_num','partner_age')], by = c('partner_look','pb_num')) %>% 
  mutate(age = ifelse(age == '16-25','21-25',age),
         partner_age = ifelse(partner_age == '16-25','21-25',partner_age)) %>% 
  mutate(age_focal = as.integer(as.factor(age)),
         age_partner = as.integer(as.factor(partner_age))) %>% 
  mutate(age_difference = ifelse(age_focal == age_partner, 'age_matched',
                                 ifelse(age_focal > age_partner, 'partner_younger', 'partner_older')),
         direction = ifelse(direction_look == 'look at directly',3,
                            ifelse(direction_look == 'side-on',2,
                                   ifelse(direction_look == 'look directly away',1, NA))))
ggplot()+
  # geom_rect(data = stim, aes(xmin = second[which(status == 'START')], xmax = second[which(status == 'STOP')],
  #                            ymin = min(look$direction)-0.5, ymax = max(look$direction)+0.5),
  #           fill = 'grey')+
  geom_line(data = look, mapping = aes(x = second, y = direction, colour = age_difference, group_by = subject))+
  facet_wrap(. ~ as.numeric(pb_num), scales = 'free_x')+
  coord_cartesian(ylim = c(min(look$direction)-0.1, max(look$direction)+0.1), expand = FALSE)



move <- plot %>% 
  select("second","b1_move_name","b2_move_name","b3_move_name","b4_move_name") %>% 
  pivot_longer(cols = c("b1_move_name","b2_move_name","b3_move_name","b4_move_name"),
               names_to = 'partner_move', values_to = 'direction_move') %>% 
  filter(direction_move != 'impossible_partner') %>% 
  separate(partner_move, into = c('partner_move','move_name'), sep = 2) %>% 
  select(-move_name) %>% 
  left_join(ages_e1[,c('partner_move','partner_age')], by = 'partner_move') %>% 
  mutate(direction = ifelse(direction_move == 'approach directly',5,
                            ifelse(direction_move == 'approach at an angle',4,
                                   ifelse(direction_move == 'move directly with',3,
                                          ifelse(direction_move == 'move away at an angle',2,
                                                 ifelse(direction_move == 'move away directly',1, NA))))))
ggplot()+
  geom_rect(data = stim_e1, aes(xmin = second[which(status == 'START')], xmax = second[which(status == 'STOP')],
                                ymin = min(move$direction, na.rm = T)-0.5, ymax = max(move$direction, na.rm = T)+0.5),
            fill = 'grey')+
  geom_line(data = move, mapping = aes(x = second, y = direction, colour = partner_move, group = ))+
  coord_cartesian(ylim = c(min(move$direction, na.rm = T)-0.1, max(move$direction, na.rm = T)+0.1), expand = FALSE)












