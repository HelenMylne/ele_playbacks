## information ####
# script to process the raw data from the playback videos, converting the raw times into:
  # proportions of time spent per activity
  # latency to change activity from start of stimulus
  # proportions of time spent with each age as nearest neighbour
  # latency to change nearest neighbour from start of stimulus

## set up ####
library(tidyverse)

theme_set(theme_classic())

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

# run for loop
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
    cowsay::say('STOP: ORDER INCORRECT', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
      cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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

saveRDS(df, '../data_processed/elephants_behaviour_split_relative_to_stimulus.RDS')


## calculate length of action by subtracting START time from STOP time ####
# df <- readRDS('../data_processed/elephants_behaviour_split_relative_to_stimulus.RDS')

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

## calculate time per elephant that was in frame in each part of the experiment ####
# separate out OUT OF SIGHT behaviours
unseen <- behav %>% filter(behaviour == 'OUT OF SIGHT: OUT OF SIGHT')

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
    cowsay::say('STOP: ORDER INCORRECT', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
      cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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

ggplot(data = look[look$action == 'look directly away',],
       aes(x = section, y = propn, group = dyad))+
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

ggplot(data = move[move$action == 'move away directly',],
       aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+
  scale_y_continuous('proportion of time visible on camera spent moving at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'move directly away from other elephants\n(rows = focal age, columns = partner age)')

###### PROPORTIONS OF TIME PER ACTIVITY: NEAREST NEIGHBOUR -- NOT DONE YET ######
## split into time before/during/after stimulus -- DONE AS FAR AS BIG LOOP ####
# filter out looking directions from neighbours
str(neighbour)
sort(unique(neighbour$elephant_looking_direction))

# standardise
neighbour$neighbour <- ifelse(neighbour$elephant_looking_direction == 'b1' | 
                                neighbour$elephant_looking_direction == 'b2' | 
                                neighbour$elephant_looking_direction == 'b3' | 
                                neighbour$elephant_looking_direction == 'b4' | 
                                neighbour$elephant_looking_direction == 'b5' | 
                                neighbour$elephant_looking_direction == 'b6' | 
                                neighbour$elephant_looking_direction == 'b7' | 
                                neighbour$elephant_looking_direction == 'b8',
                              neighbour$elephant_looking_direction,
                              'unknown')

# create set of individual/behaviour breakdowns to go through
neighbour$elephant_neighbour_unique <- paste0(neighbour$subject,'_',neighbour$neighbour,'_')
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
  separate(id_behav, into = c('bull', 'pb_num','target','type'), remove = F) %>% 
  mutate(check = NA, time_fails = NA)

# run for loop
for(elephant_behaviour in 1:N){
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
    cowsay::say('STOP: ORDER INCORRECT', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
      cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
rm(correct_in_boris, response, response_split, elephant_behaviour, i, id_behav, j, k, N) ; gc()

## calculate length of action by subtracting START time from STOP time -- NOT DONE YET ####
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

## calculate proportion of time spent per activity -- NOT DONE YET ####
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

## graph proportions -- NOT DONE YET ####
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

ggplot(data = look[look$action == 'look directly away',],
       aes(x = section, y = propn, group = dyad))+
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

ggplot(data = move[move$action == 'move away directly',],
       aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+
  scale_y_continuous('proportion of time visible on camera spent moving at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'move directly away from other elephants\n(rows = focal age, columns = partner age)')

###### PROPORTIONS OF TIME PER ACTIVITY: SOCIAL BEHAVIOUR -- NOT DONE YET ######
## split into time before/during/after stimulus -- NOT DONE YET ####
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
    cowsay::say('STOP: ORDER INCORRECT', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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
      cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
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

## calculate length of action by subtracting START time from STOP time -- NOT DONE YET ####
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

## calculate proportion of time spent per activity -- NOT DONE YET ####
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

## graph proportions -- NOT DONE YET ####
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

ggplot(data = look[look$action == 'look directly away',],
       aes(x = section, y = propn, group = dyad))+
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

ggplot(data = move[move$action == 'move away directly',],
       aes(x = section, y = propn, group = dyad))+
  geom_line(linewidth = 0.2, aes(colour = stimulus))+
  geom_point(size = 0.4, alpha = 0.6, aes(colour = stimulus))+
  facet_grid(age ~ partner_age)+
  scale_y_continuous('proportion of time visible on camera spent moving at other elephants')+
  scale_x_discrete(expand = c(0.05,0.05), 'time relative to stimulus')+
  scale_colour_viridis_d(direction = -1)+
  theme(legend.position = 'bottom')+
  labs(title = 'move directly away from other elephants\n(rows = focal age, columns = partner age)')
