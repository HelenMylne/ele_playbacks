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

saveRDS(df_eles, file = '../data_processed/elephants.RDS')

rm(df_new, eles, file_data, metadata, file, file_list, i, labels, row_num) ; gc()

## convert to proportions of time spent on each activity ####
# reimport data if necessary
# df_eles <- readRDS(df_eles, file = '../data_processed/elephants.RDS')
