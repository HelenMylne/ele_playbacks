## information ####
# script to process the raw data from the playback videos, converting the raw times into:
  # proportions of time spent per activity
  # latency to change activity from start of stimulus
  # proportions of time spent with each age as nearest neighbour
  # latency to change nearest neighbour from start of stimulus

## set up ####
library(tidyverse)

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

# save output
saveRDS(df_eles, file = '../data_processed/elephants.RDS')

# clean environment
rm(df_new, eles, file_data, metadata, file, file_list, i, labels, row_num) ; gc()

## convert to proportions of time spent on each activity ####
# reimport data if necessary
#df_eles <- readRDS('../data_processed/elephants.RDS') ; df_stim <- readRDS('../data_processed/stimuli.RDS')

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

### split activities into time before stimulus, time during stimulus and time after stimulus -- if an action starts before the stimulus does and finishes during it, take this as 2 actions, one before and one during, with the cut off being when the stimulus starts.
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
rm(df_eles, df_stim, stim, stimuli, x, i) ; gc()

# add column for if line is before, during or after stimulus
df$bda <- ifelse(df$time < df$stim_start, 'before',
                 ifelse(df$time > df$stim_stop, 'after', 'during'))
table(df$bda) # this looks like a reasonable split

# breakdown to behavioural types to combine into pairs
df$activity_unique <- paste0(df$subject,'_',df$behavioral_category,'_',df$type)

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
  for(i in 2:(nrow(response)-1)){
    response$order_correct[i] <- ifelse(response$time[i] < response$time[i+1] &
                                          response$time[i] > response$time[i-1], 'yes', 
                                        ifelse(response$time[i] == response$time[i+1] |
                                                 response$time[i] == response$time[i-1], 'match',
                                               'no'))
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
  response$pairs_together <- NA ; for(i in 1:(nrow(response)-1)){
    response$pairs_together[i] <- ifelse(response$status[i] == 'START' & response$status[i+1] == 'STOP', 'yes',
                                     ifelse(response$status[i] == 'STOP' & response$status[i+1] == 'START', 'yes', 'no'))
  }
  if(length(which(response$pairs_together == 'no')) > 0) {
    cowsay::say('STOP: PAIRS NOT TOGETHER', by = names(cowsay::animals)[runif(1,1,length(names(cowsay::animals)))] )
    response$pairs_together*response$pairs_together # will throw an error and halt operation if 'no' is an option
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
    response <- response %>% 
      arrange(time) %>% 
      mutate(activity_id = as.numeric(as.factor(paste0(action_unique, bda)))) %>% 
      select(subject, behaviour, time, bda, status, type, action, behavioral_category, target, comment, pb_num, stim_num, stim_type, stim_start, stim_stop, stim_duration, activity_id, act_id, action_unique, activity_unique, group_size, age, bull, date, play_time, description)
    
  }
  
  # recombine into full dataset
  df <- rbind(df, response)
  
  # clean up
  gc()
  
}

rm(response, response_split, elephant_behaviour, i, id_behav, j, N) ; gc()

saveRDS(df, '../data_processed/elephants_behaviour_split_relative_to_stimulus.RDS')



# calculate length of action by subtracting START time from STOP time
# calculate time per elephant that was in frame in each part of the experiment
oos <- df_eles[df_eles$action == 'OUT OF SIGHT',]


















####### SCRAP ALL THIS I THINK ##########
# breakdown to behavioural types to combine into pairs -- example
test <- df[df$subject == 'b4_e44' & df$target == 'vehicle' & df$behavioral_category == 'look',] ; test <- test[is.na(test$time) == F,]

# check in correct order
test$order_correct <- NA ; for(i in 1:(nrow(test)-1)){
  test$order_correct[i] <- ifelse(test$time[i] < test$time[i+1], 'yes', 'no')
}
table(test$order_correct)
test$pairs_together <- NA ; for(i in 1:(nrow(test)-1)){
  test$pairs_together[i] <- ifelse(test$status[i] == 'START' & test$status[i+1] == 'STOP', 'yes',
                                   ifelse(test$status[i] == 'STOP' & test$status[i+1] == 'START', 'yes', 'no'))
}
table(test$pairs_together)

# add label per start/stop pair
test$act_id <- rep(1:(nrow(test)/2), each = 2)
test$action_unique <- paste0(test$subject,'_',test$type,'_',test$action,'_',test$act_id)

# for pairs that span across multiple parts of experiment, create variable indicating which parts
test$bda_split <- NA; for(i in 1:nrow(test)){
  x <- test[test$act_id == test$act_id[i],]
  test$bda_split[i] <- ifelse(x$bda[1] == x$bda[2], 'no',
                              ifelse(x$bda[1] == 'before',
                                     ifelse(x$bda[2] == 'during', 'bd','ba'),
                                     'da'))
}

# for pairs that span before-during or during-after, single split. for pairs that span before-after, double split
if(length(which(test$bda_split != 'no') > 0)) {
  test_split <- test[test$bda_split != 'no',]
  if(test_split$bda_split[1] == 'bd'){
    test_split <- rbind(test_split, test_split)
    test_split$time[2] <- test_split$stim_start[1]-0.001
    test_split$time[3] <- test_split$stim_start[1]
  }
  if(test_split$bda_split[1] == 'da'){
    test_split <- rbind(test_split, test_split)
    test_split$time[2] <- test_split$stim_stop[1]
    test_split$time[3] <- test_split$stim_stop[1]+0.001
  }
  if(test_split$bda_split[1] == 'ba'){
    test_split <- rbind(test_split, test_split, test_split)
    test_split$time[2] <- test_split$stim_start[1]-0.001
    test_split$time[3] <- test_split$stim_start[1]
    test_split$time[4] <- test_split$stim_stop[1]
    test_split$time[5] <- test_split$stim_stop[1]+0.001
  }
  test_split$bda <- ifelse(test_split$time < test_split$stim_start, 'before',
                           ifelse(test_split$time > test_split$stim_stop, 'after', 'during'))
}


#-- THIS DOESN'T WORK IF THERE IS A SPAN ACROSS BEFORE-DURING AND ANOTHER ACROSS DURING-AFTER
#  if(length(which(response$bda_split != 'no') > 0)) {
#    response_split <- response[response$bda_split != 'no',]
#    response <- anti_join(response, response_split)
#    if(response_split$bda_split[1] == 'bd'){
#      response_split <- rbind(response_split, response_split)
#      response_split$time[2] <- response_split$stim_start[1]-0.001
#      response_split$time[3] <- response_split$stim_start[1]
#    }
#    if(response_split$bda_split[1] == 'da'){
#      response_split <- rbind(response_split, response_split)
#      response_split$time[2] <- response_split$stim_stop[1]
#    }
#      response_split$time[3] <- response_split$stim_stop[1]+0.001
#    if(response_split$bda_split[1] == 'ba'){
#      response_split <- rbind(response_split, response_split, response_split)
#      response_split$time[2] <- response_split$stim_start[1]-0.001
#      response_split$time[3] <- response_split$stim_start[1]
#      response_split$time[4] <- response_split$stim_stop[1]
#      response_split$time[5] <- response_split$stim_stop[1]+0.001
#    }
#    response_split$bda <- ifelse(response_split$time < response_split$stim_start, 'before',
#                             ifelse(response_split$time > response_split$stim_stop, 'after', 'during'))
#  # recombine
#  before

#    response <- rbind(response, response_split)


