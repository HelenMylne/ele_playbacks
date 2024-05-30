#### information ####
# script for nearest neighbour analysis of playback data.
# data inputs produced in data_processing.R script

#### set up ####
#library(tidyverse); library(brms) ; library(LaplacesDemon) ; library(patchwork)
library(StanHeaders, lib.loc = '../packages/')
library(rstan, lib.loc = '../packages/')
#library(brms, lib.loc = '../packages/')
library(tidyverse, lib.loc = '../packages/')
library(LaplacesDemon, lib.loc = '../packages/')
library(patchwork, lib.loc = '../packages/')

theme_set(theme_classic())
set.seed(12345)

#### create data ####
# https://dagitty.net/dags.html?id=fQrEyF#
pdf('outputs/neighbour_markov_model/neighbour_markov_modelprep.pdf')

# read in data
ages <- readRDS('data_processed/behaviour_by_second_indexvariables_bda.RDS') %>% 
  # ages <- readRDS('../data_processed/behaviour_by_second_indexvariables_bda.RDS') %>% 
  select(focal, f_age_cat, f_age_num) %>% 
  distinct() %>% 
  filter(!is.na(f_age_cat)) %>% 
  mutate(partner = focal,
         p_age_cat = f_age_cat,
         p_age_num = f_age_num)

stim_starts <- readRDS('data_processed/stimuli.RDS') %>%
  # stim_starts <- readRDS('../data_processed/stimuli.RDS') %>%
  filter(status == 'START' & behavior == 'STIMULUS') %>%
  select(pb_num,time,stim_num,stim_type,group_size,comment)
table(stim_starts$pb_num)
# 1  3  4  6  7 10 11 13 14 15 16 17 18 19 21 22 23 24 25 28 29 30 31 32 33 34 35 36 37 38 41 42 43 44 45 46 47 48 50 51 52 53 55 56 58 59 60 61 
# 1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  2  1  1  2  1  1  3  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  5  1  1  1  1  1  1 
multiple_starts <- c(10, 24, 29, 32, 46, 53)
check <- stim_starts %>%
  filter(pb_num %in% multiple_starts) # for stim 10+29+46+53 take first time, for 24+32 use second.
for(i in multiple_starts){
  x <- check %>% filter(pb_num == i)
  check <- anti_join(check, x)
  if(i %in% c(10,29,46,53)){
    x <- x[1,]
  }
  if(i %in% c(24,32)){
    x <- x[2,]
  }
  check <- rbind(check, x)
}
stim_starts <- stim_starts %>%
  filter(! pb_num %in% multiple_starts) %>%
  rbind(check) %>%
  mutate(time = as.numeric(time)) %>%
  mutate(stim_start = round(time, 0)) %>%
  select(pb_num,stim_start,stim_num,stim_type,group_size)

## nearest neighbour data
cols_of_interest <- c('b1_nn','b2_nn','b3_nn','b4_nn',
                      'b5_nn','b6_nn','b7_nn','b8_nn')
cols_of_interest_name <- c('b1_nn_name','b2_nn_name','b3_nn_name','b4_nn_name',
                           'b5_nn_name','b6_nn_name','b7_nn_name','b8_nn_name')
cols_of_interest_index <- c('b1_nn_index','b2_nn_index','b3_nn_index','b4_nn_index',
                            'b5_nn_index','b6_nn_index','b7_nn_index','b8_nn_index')
nn_all <- readRDS('data_processed/behaviour_by_second_indexvariables.RDS') %>%
  # nn_all <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
  # select relevant variables
  select(subject,pb_num,second,out_frame_name,
         all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
  # convert to tidy format
  rename(b1_nn = b1_nn_name, b2_nn = b2_nn_name,
         b3_nn = b3_nn_name, b4_nn = b4_nn_name,
         b5_nn = b5_nn_name, b6_nn = b6_nn_name,
         b7_nn = b7_nn_name, b8_nn = b8_nn_name) %>% 
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_name', values_to = 'neighbour') %>% 
  rename(b1_nn = b1_nn_index, b2_nn = b2_nn_index,
         b3_nn = b3_nn_index, b4_nn = b4_nn_index,
         b5_nn = b5_nn_index, b6_nn = b6_nn_index,
         b7_nn = b7_nn_index, b8_nn = b8_nn_index) %>% 
  pivot_longer(cols = all_of(cols_of_interest),
               names_to = 'elephant_activity_index', values_to = 'nn_index') %>% 
  filter(elephant_activity_name == elephant_activity_index) %>% 
  select(-elephant_activity_index) %>% 
  rename(elephant_activity = elephant_activity_name,
         focal = subject) %>% 
  # fix out of sight variable
  mutate(neighbour = ifelse(out_frame_name == 'out_of_sight',
                            'out_of_sight', neighbour),
         nn_index = ifelse(out_frame_name == 'out_of_sight',
                           9, nn_index)) %>% 
  # remove elephants that can't be partners (self-neighbour or elephants 5-8 in a group of 4)
  filter(is.na(nn_index) == FALSE) %>% 
  # join with explanatory and random variables
  separate(elephant_activity, into = c('partner','activity'),
           sep = '_', remove = T) %>% 
  mutate(partner = paste0(partner, '_e', pb_num),
         pb_num = as.numeric(pb_num)) %>% 
  left_join(ages[,c('focal','f_age_cat','f_age_num')], by = 'focal') %>% 
  left_join(ages[,c('partner','p_age_cat','p_age_num')], by = 'partner') %>% 
  left_join(stim_starts, by = 'pb_num') %>%
  # create additional model variables
  mutate(time_since_stim = second - stim_start,
         after_stim = ifelse(time_since_stim < 0, 0, time_since_stim/60),
         age_difference = ifelse(as.numeric(f_age_num) > as.numeric(p_age_num),
                                 'partner_younger',
                                 ifelse(as.numeric(f_age_num) == as.numeric(p_age_num),
                                        'matched',
                                        'partner_older'))) %>%
  # clean up
  filter(!is.na(p_age_num)) %>%    # b6_e7 + b2_e13 + b2_e34 = unknown age
  select(pb_num,focal,partner,
         activity,neighbour,
         stim_num,stim_type,
         time_since_stim, after_stim,
         f_age_cat,p_age_cat,f_age_num,p_age_num,
         age_difference) %>% 
  mutate(f_age_num = as.factor(f_age_num),
         p_age_num = as.factor(p_age_num),
         age_combo = paste0(f_age_num,'_',p_age_num),
         nn_tminus1 = NA,
         nn_tminus1_num = NA)
rm(list = ls() [ ! ls() %in% 'nn_all']) ; gc()

unique(nn_all$focal[is.na(nn_all$f_age_num)])   # b6_e7 = unknown age
#unique(nn_all$partner[is.na(nn_all$p_age_num)]) # b2_e13 + b2_e34 = unknown age
length(which(is.na(nn_all$neighbour) == TRUE))

nn <- nn_all %>%
  filter(neighbour != 0) %>%
  filter(is.na(f_age_num) == FALSE) %>%
  mutate(partner = ifelse(neighbour == 'out_of_sight', NA, partner),
         p_age_num = ifelse(neighbour == 'out_of_sight', NA, p_age_num),
         p_age_cat = ifelse(neighbour == 'out_of_sight', NA, p_age_cat),
         age_combo = ifelse(neighbour == 'out_of_sight', NA, age_combo),
         age_difference = ifelse(neighbour == 'out_of_sight', NA, age_difference)) %>% 
  distinct() %>% 
  select(-neighbour) %>% 
  rename(neighbour = partner) %>% 
  mutate(age_difference = factor(age_difference,
                                 levels = c('partner_younger',
                                            'matched',
                                            'partner_older'))) %>%
  mutate(age_diff_num = as.integer(age_difference),
         f_age_num = as.integer(f_age_num),
         p_age_num = as.integer(p_age_num)) %>% 
  mutate(age_diff_num = ifelse(is.na(age_diff_num), 9, age_diff_num)) %>% 
  relocate(age_diff_num, .after = age_difference)

# remove b3_e50 as apparently never has a nearest neighbour
nn <- nn %>% 
  filter(focal != 'b3_e50')

# create variable for nearest neighbour at time t-1
focals <- unique(nn$focal)
for(f in 1:length(focals)){
  focal <- nn %>% filter(focal == focals[f])
  nn <- nn %>% anti_join(focal, by = 'focal')
  for(i in 2:nrow(focal)){
    focal$nn_tminus1[i] <- as.character(focal$age_difference[i-1])
    focal$nn_tminus1_num[i] <- focal$age_diff_num[i-1]
  }
  nn <- rbind(nn, focal)
}
rm(list = ls() [! ls() %in% c('nn','nn_all')]) ; gc()

# clean up for analysis
nn_no_na <- nn %>%
  filter(is.na(age_diff_num) == FALSE) %>%
  #filter(is.na(nn_tminus1) == FALSE) %>%
  mutate(focal_id = as.integer(as.factor(focal)),
         stim_id = as.integer(as.factor(stim_num)),
         nn_tminus1 = factor(nn_tminus1, levels = c('partner_younger',
                                                    'matched',
                                                    'partner_older'))) %>%
  rename(playback_id = pb_num)
str(nn_no_na)
# $ playback_id    : num [1:63868] 1 1 1 1 1 1 1 1 1 1 ...
# $ focal          : chr [1:63868] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# $ neighbour      : chr [1:63868] "b3_e1" "b3_e1" "b3_e1" "b3_e1" ...
# $ activity       : chr [1:63868] "nn" "nn" "nn" "nn" ...
# $ stim_num       : chr [1:63868] "14" "14" "14" "14" ...
# $ stim_type      : chr [1:63868] "ctd" "ctd" "ctd" "ctd" ...
# $ time_since_stim: num [1:63868] -82 -81 -80 -79 -78 -77 -76 -75 -74 -73 ...
# $ after_stim     : num [1:63868] 0 0 0 0 0 0 0 0 0 0 ...
# $ f_age_cat      : chr [1:63868] "26-35" "26-35" "26-35" "26-35" ...
# $ p_age_cat      : chr [1:63868] "26-35" "26-35" "26-35" "26-35" ...
# $ f_age_num      : int [1:63868] 4 4 4 4 4 4 4 4 4 4 ...
# $ p_age_num      : int [1:63868] 4 4 4 4 4 4 4 4 4 4 ...
# $ age_difference : Factor w/ 3 levels "partner_younger",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ age_diff_num   : num [1:63868] 2 2 2 2 2 2 2 2 2 2 ...
# $ age_combo      : chr [1:63868] "4_4" "4_4" "4_4" "4_4" ...
# $ nn_tminus1     : Factor w/ 3 levels "partner_younger",..: NA 2 2 2 2 2 2 2 2 2 ...
# $ nn_tminus1_num : num [1:63868] NA 2 2 2 2 2 2 2 2 2 ...
# $ focal_id       : int [1:63868] 1 1 1 1 1 1 1 1 1 1 ...
# $ stim_id        : int [1:63868] 5 5 5 5 5 5 5 5 5 5 ...

# ################################ MARKOV MODEL WITH MARKOVCHAIN PACKAGE -- DOESN'T ALLOW ADDITION OF COVARIATES #############################
# library(markovchain)
# library(igraph)
# 
# #### create markov sequence ####
# ## select individual elephant -- changes a couple of times between neighbours
# focal <- nn_no_na %>% 
#   filter(focal == 'b1_e24') %>% 
#   mutate(age_difference = ifelse(age_difference == 'partner_older', 'Older',
#                                  ifelse(age_difference == 'partner_younger', 'Younger', 'Matched')))
# 
# mkvseq <- createSequenceMatrix(focal$age_difference, sanitize = F)
# mkvseq # transition matrix (unstandardised)
# 
# ## standardise matrix
# mkvseq_std <- mkvseq
# for(i in 1:nrow(mkvseq)){
#   for(j in 1:ncol(mkvseq)){
#     mkvseq_std[i,j] <- mkvseq_std[i,j] / sum(mkvseq[i,])
#   }
# }
# 
# #### fit markov chain ####
# mkvfit <- markovchainFit(data = focal$age_difference, # won't accept mkvseq but not sure why
#                          method = 'map',
#                          possibleStates = c('Older','Matched'))#,'Younger'))
# mkvfit
# 
# ## extract estimates
# est_mat <- mkvfit$estimate
# est_mat <- est_mat[1:nrow(est_mat),]
# 
# ## not actually got a clue what this does??
# neighbours <- new('markovchain',
#                   states = c('Older','Matched'),# 'Younger'),
#                   transitionMatrix = est_mat,
#                   name = 'neighbour chain')
# steadyStates(neighbours)
# 
# #### plot nicely ####
# ## plot basic
# plot(neighbours, nod.size = 5000, edge.curved = 0.5, 
#      vertex.color = "#1F968BFF", vertex.size = 75)
# 
# ## create edgelist for igraph
# edgelist <- as.data.frame(mkvseq_std)
# edgelist$original_state <- rownames(edgelist)
# edgelist <- edgelist %>%
#   pivot_longer(cols = c('Matched','Older'),
#                names_to = 'new_state', values_to = 'transition_probability')
# 
# ## add uncertainty to edgelist
# errors <- as.data.frame(mkvfit$standardError)
# colnames(errors) <- c('Matched','Older')
# rownames(errors) <- c('Matched','Older')
# errors$original_state <- rownames(errors)
# errors <- errors %>%
#   pivot_longer(cols = c('Matched','Older'),
#                names_to = 'new_state', values_to = 'error')
# edgelist <- edgelist %>% 
#   left_join(errors, by = c('original_state','new_state')) %>% 
#   mutate(lower = transition_probability - error,
#          upper = transition_probability + error)
# 
# ## create igraph object
# g <- graph_from_adjacency_matrix(mkvseq_std, weighted = T)
# par(mai = c(0.1,0.1,0.1,0.1))
# 
# ## plot with uncertainty
# coords <- matrix(data = c(0,2,1,1), byrow = T, nrow = 2,
#                  dimnames = list(c('Matched','Older'),
#                                  c('Matched','Older')))
# edge.label.x <- c(-0.45, 0.35, -0.5, 1.55)
# edge.label.y <- c(1.2, 0.35, -0.2, -0.8)
# plot.igraph(g, layout = coords,
#             vertex.label.color = 'white', label.family = 'Helvetica',
#             vertex.color = "#1F968BFF", vertex.size = 75,
#             frame.color = NA, frame.width = 0,
#             edge.color = 'grey', edge.arrow.size = 1, 
#             edge.width = edgelist$upper*10,
#             edge.label = NA,
#             edge.curved = 0.5)
# plot.igraph(g, layout = coords, add = T,
#             vertex.label.color = 'white', label.family = 'Helvetica',
#             vertex.color = "#1F968BFF", vertex.size = 75,
#             frame.color = NA, frame.width = 0,
#             edge.color = 'black', edge.arrow.size = 1, 
#             edge.width = edgelist$transition_probability*5,
#             edge.label = round(edgelist$transition_probability, 3),
#             edge.label.x = edge.label.x, edge.label.y = edge.label.y,
#             edge.curved = 0.5)

################################ MARKOV MODEL WITH MSM PACKAGE -- ALLOWS ADDITION OF COVARIATES AND CAN SPECIFY IMPOSSIBLE TRANSITIONS PER ELEPHANT, BUT CURRENTLY MODEL IS FAILING TO CONVERGE SO NEEDS A DIFFERENT OPTIMISATION #############################
library(msm)

#### summarise state data transitions ####
statetable.msm(age_diff_num, focal, data = nn_no_na)
#     to
# from     1     2     3     9
#    1 16487    22    26    85
#    2    25 18783    30    88
#    3    27    32 14815    74
#    9    44    42    44 13070

#### prep for Markov chains ####
## define qmatrix -- matrix showing all the impossible transitions -- To tell msm what the allowed transitions of our model are, we define a matrix of the same size as as the statetable, containing zeroes in the positions where the transition is impossible. All other positions contain an initial value for the corresponding transition intensity. The diagonal entries supplied in this matrix do not matter, as the diagonal entries are defined as minus the sum of all the other entries in the row.
qmat <- rbind(c(0.8, 0.1, 0.1),
              c(0.1, 0.8, 0.1),
              c(0.1, 0.1, 0.8))

## normalise time variable to help convergence
# nn_no_na$time_norm <- NA
# for(i in unique(nn_no_na$focal)){
#   focal <- nn_no_na %>% 
#     filter(focal == i)
#   nn_no_na <- nn_no_na %>% 
#     filter(focal != i)
#   focal$time_norm <- focal$time_since_stim + abs(min(focal$time_since_stim)) +1
#   focal$time_norm <- (focal$time_norm - min(focal$time_norm)) / (max(focal$time_norm) - min(focal$time_norm))
#   nn_no_na <- rbind(nn_no_na, focal)
# }
nn_no_na$time_norm <- nn_no_na$time_since_stim / 60 # minutes not seconds

## get crude estimates
crudes <- crudeinits.msm(age_diff_num ~ time_norm,
                         subject = focal, data = nn_no_na, qmatrix = qmat,
                         censor = 9)

## set subject-specific initial probabilities
nn_mkv <- nn_no_na %>% 
  dplyr::select(focal, playback_id, f_age_num) %>% 
  distinct() %>% 
  mutate(young = NA, match = NA, older = NA)
for(i in 1:nrow(nn_mkv)){
  pb <- nn_mkv %>%
    filter(playback_id == playback_id[i]) %>% 
    filter(focal != nn_mkv$focal[i])
  nn_mkv$young[i] <- length(which(pb$f_age_num <  nn_mkv$f_age_num[i]))
  nn_mkv$match[i] <- length(which(pb$f_age_num == nn_mkv$f_age_num[i]))
  nn_mkv$older[i] <- length(which(pb$f_age_num >  nn_mkv$f_age_num[i]))
}
init.probs <- matrix(data = NA, nrow = length(unique(nn_no_na$focal)),
                     ncol = 3, dimnames = list(unique(nn_no_na$focal), 1:3))
for(i in 1:nrow(init.probs)){
  x <- nn_mkv[i,c('young','match','older')]
  init.probs[i,] <- as.matrix(x/sum(x))
}

## make all parts of the data set the same length, with additional "out of sight" rows, or it tried to fit them all to the same length of data (I believe)
nn_mkv <- nn_no_na %>% 
  select(age_diff_num, time_since_stim, time_norm,
         f_age_cat, f_age_num, stim_type,
         focal_id, stim_id, playback_id)
min_start <- min(nn_mkv$time_since_stim)
max_end <- max(nn_mkv$time_since_stim)
for(i in unique(nn_mkv$focal_id)){
  ## define the data frame per elephant
  focal <- nn_mkv %>% 
    filter(focal_id == i)
  nn_mkv <- nn_mkv %>% 
    filter(focal_id != i)
  
  ## define the range of the times for this particular individual
  min_focal_start <- min(focal$time_since_stim)
  max_focal_end <- max(focal$time_since_stim)
  
  ## create data frames to add to start
  start_diff <- min_focal_start - min_start
  if(min_focal_start != min_start){
    start <- data.frame(age_diff_num = rep(9, start_diff),
                        time_since_stim = min_start:(min_focal_start-1),
                        time_norm = rep(NA, start_diff),
                        f_age_cat = rep(focal$f_age_cat[1], start_diff),
                        f_age_num = rep(focal$f_age_num[1], start_diff),
                        stim_type = rep(focal$stim_type[1], start_diff),
                        focal_id = rep(focal$focal_id[1], start_diff),
                        stim_id = rep(focal$stim_id[1], start_diff),
                        playback_id = rep(focal$playback_id[1], start_diff)) %>% 
      mutate(time_norm = time_since_stim / 60)
  } else {
    start <- data.frame(age_diff_num = rep(NA,0),
                        time_since_stim = rep(NA,0),
                        time_norm = rep(NA,0),
                        f_age_cat = rep(NA,0),
                        f_age_num = rep(NA,0),
                        stim_type = rep(NA,0),
                        focal_id = rep(NA,0),
                        stim_id = rep(NA,0),
                        playback_id = rep(NA,0))
  }
  
  ## create data frames to add to end
  end_diff <- max_end - max_focal_end
  if(max_focal_end != max_end){
    end <- data.frame(age_diff_num = rep(9, end_diff),
                      time_since_stim = (max_focal_end+1):max_end,
                      time_norm = rep(NA, end_diff),
                      f_age_cat = rep(focal$f_age_cat[1], end_diff),
                      f_age_num = rep(focal$f_age_num[1], end_diff),
                      stim_type = rep(focal$stim_type[1], end_diff),
                      focal_id = rep(focal$focal_id[1], end_diff),
                      stim_id = rep(focal$stim_id[1], end_diff),
                      playback_id = rep(focal$playback_id[1], end_diff)) %>% 
      mutate(time_norm = time_since_stim / 60)
  } else {
    end <- data.frame(age_diff_num = rep(NA,0),
                      time_since_stim = rep(NA,0),
                      time_norm = rep(NA,0),
                      f_age_cat = rep(NA,0),
                      f_age_num = rep(NA,0),
                      stim_type = rep(NA,0),
                      focal_id = rep(NA,0),
                      stim_id = rep(NA,0),
                      playback_id = rep(NA,0))
  }
  
  ## combine together
  focal <- rbind(start, focal, end)
  nn_mkv <- rbind(nn_mkv, focal)
}
rm(pb, x, start, end, focal, max_end, max_focal_start, max_x_end, min_focal_start, min_start, min_x_start, start_diff, end_diff, i) ; gc()

#### fit Markov chains ####
## fit chain -- baseline probabilities
mkv_base <- msm(formula = age_diff_num ~ time_norm,
                subject = focal_id,
                data = nn_mkv,
                qmatrix = qmat,
                exacttimes = T,
                initprobs = init.probs,
                censor = 9)
mkv_base
# Maximum likelihood estimates -- raw minutes (time = -3 to +3)
#                   Baseline                     
# State 1 - State 1 -25.67454 (-26.11995,-25.2367)
# State 1 - State 2   0.08379 (  0.05752,  0.1221)
# State 1 - State 3  25.59075 ( 25.15347, 26.0356)
# State 2 - State 1   0.21147 (  0.15112,  0.2959)
# State 2 - State 2  -0.48117 ( -0.56428, -0.4103)
# State 2 - State 3   0.26970 (  0.20350,  0.3574)
# State 3 - State 1  27.33383 ( 26.86692, 27.8088)
# State 3 - State 2   0.13572 (  0.10185,  0.1809)
# State 3 - State 3 -27.46955 (-27.94550,-27.0017)
# -2 * log-likelihood:  -128336.6 

## fit chain with covariates -- currently has issues with optimisation
mkv_cov <- msm(formula = age_diff_num ~ time_norm,    # response and time variable
               subject = focal_id,                    # separate different identities
               data = nn_mkv,                         # data frame of interest
               qmatrix = qmat,                        # self-defined q-matrix
               covariates = ~ f_age_num + stim_type,  # covariates of interest -- NOTE: F_AGE_NUM IS BEING TREATED AS A CONTINUOUS VARIABLE HERE, NOT A CATEGORICAL. CATEGORICAL CAUSES OVERPARAMETERISATION AND THE MODEL FAILS, BUT CONTINUOUS IS WRONG.
               exacttimes = T, #obstype = 2,          # know the times that individuals switched exactly
               opt.method = 'optim',                  # optimisation -- this is the default and may need to change
               hessian = T,                           # don't understand this bit -- also the default
               gen.inits = F,                         # don't generate initial probabilities in the model
               initprobs = init.probs,                # use these initial probabilities
               censor = 9,                            # censored category (elephant out of sight) = 9
               censor.states = c(1,2,3),              # censored category could mean true value is any other
               control = list(#reltol = 1e-32,        # set controls for optimiser 
                              #ndeps = rep(1e-6,length(unique(nn_no_na$focal)) + 7),
                              fnscale=4000))
mkv_cov # note DOES NOT CONVERGE
# msm(formula = age_diff_num ~ time_norm, subject = focal_id, data = nn_mkv, qmatrix = qmat, gen.inits = F, covariates = ~f_age_num + stim_type, initprobs = init.probs, exacttimes = T, censor = 9, censor.states = c(1, 2, 3), opt.method = "optim", hessian = T, control = list(fnscale = 4000))
#                   Baseline                       f_age_num              stim_typeh           stim_typel           
# State 1 - State 1 -26.86335 (-27.33516,-26.3997)                                                                  
# State 1 - State 2   0.06991 (  0.04243,  0.1152) 1.6126 (1.0081,2.5798) 1.797 (0.8830,3.657) 0.7221 (0.2081,2.506)
# State 1 - State 3  26.79343 ( 26.33038, 27.2646) 0.5673 (0.5556,0.5792) 1.249 (1.2020,1.298) 1.3048 (1.2413,1.371)
# State 2 - State 1   0.16204 (  0.08684,  0.3024) 1.4183 (0.9572,2.1016) 2.863 (1.0294,7.962) 0.9889 (0.2901,3.371)
# State 2 - State 2  -0.40682 ( -0.50213, -0.3296)                                                                  
# State 2 - State 3   0.24479 (  0.15548,  0.3854) 0.4003 (0.2585,0.6201) 1.285 (0.6166,2.680) 1.5264 (0.6593,3.534)
# State 3 - State 1  30.87882 ( 30.34497, 31.4221) 1.5825 (1.5482,1.6176) 0.999 (0.9616,1.038) 1.0675 (1.0165,1.121)
# State 3 - State 2   0.10939 (  0.06923,  0.1729) 0.7191 (0.4452,1.1615) 1.201 (0.6125,2.357) 2.3157 (1.1082,4.839)
# State 3 - State 3 -30.98822 (-31.53249,-30.4533)                                                                  
# -2 * log-likelihood:  -132956.1

mkv_stm <- msm(formula = age_diff_num ~ time_norm,   # time_since_stim,
               subject = focal_id,
               data = nn_mkv,
               qmatrix = qmat,
               covariates = ~ stim_type,
               exacttimes = T, #obstype = 2,
               opt.method = 'optim',
               hessian = T,
               gen.inits = T,
               initprobs = init.probs,
               censor = 9,
               censor.states = c(1,2,3))
mkv_stm
# msm(formula = age_diff_num ~ time_norm, subject = focal_id, data = nn_mkv,     qmatrix = qmat, gen.inits = T, covariates = ~stim_type, initprobs = init.probs,     exacttimes = T, censor = 9, censor.states = c(1, 2, 3), opt.method = "optim",     hessian = T)
#                   Baseline                             stim_typeh            stim_typel                    
# State 1 - State 1  -0.460361 ( -7.651e-01, -2.770e-01)                                                     
# State 1 - State 2   0.248935 (  1.721e-01,  3.601e-01) 1.5501 (0.7193,3.341) 1.989e+00 (0.6796018,  5.8193)
# State 1 - State 3   0.211426 (  6.075e-02,  7.358e-01) 1.6167 (0.8727,2.995) 1.878e-01 (0.0003366,104.7731)
# State 2 - State 1   0.075373 (  4.428e-02,  1.283e-01) 3.0764 (1.0043,9.424) 1.991e+00 (0.6159979,  6.4371)
# State 2 - State 2 -24.399424 ( -2.482e+01, -2.398e+01)                                                     
# State 2 - State 3  24.324051 (  2.391e+01,  2.475e+01) 1.0442 (1.0050,1.085) 6.175e-01 (0.5878776,  0.6486)
# State 3 - State 1   0.009473 ( 6.689e-115, 1.342e+110) 0.8854 (0.4807,1.631) 6.758e-07 (0.0000000,     Inf)
# State 3 - State 2  27.466955 (  2.700e+01,  2.795e+01) 1.0030 (0.9654,1.042) 1.151e+00 (1.0960311,  1.2092)
# State 3 - State 3 -27.476428 ( -3.008e+01, -2.509e+01)                                                     
# -2 * log-likelihood:  -127532.3 

mkv_age <- msm(formula = age_diff_num ~ time_norm,
               subject = focal_id,
               data = nn_mkv,
               qmatrix = qmat,
               covariates = ~ f_age_num, # treats this as a continuous variable not an ordered categorical -- this is WRONG but I can't find how to make it take an ordered categoricala at all, and a regular categorical just leads to overparameterisation and failed convergence
               exacttimes = T, #obstype = 2,
               opt.method = 'optim',
               hessian = T,
               gen.inits = T,
               initprobs = init.probs,
               censor = 9,
               censor.states = c(1,2,3))
mkv_age
# msm(formula = age_diff_num ~ time_norm, subject = focal_id, data = nn_mkv, qmatrix = qmat, gen.inits = T, covariates = ~f_age_num, initprobs = init.probs, exacttimes = T, censor = 9, censor.states = c(1, 2, 3), opt.method = "optim", hessian = T)
#                   Baseline                       f_age_num                
# State 1 - State 1  -0.72653 ( -0.88043, -0.5995)                          
# State 1 - State 2   0.32466 (  0.22651,  0.4653) 0.05250 (0.04241,0.06501) # increase f_age_num by 1 category = reduce Pr(y -> m) by 95%
# State 1 - State 3   0.40187 (  0.30141,  0.5358) 0.68047 (0.47092,0.98325) # reduce Pr(y -> o) by 93%
# State 2 - State 1   0.11664 (  0.08498,  0.1601) 1.74485 (1.22233,2.49075) # increase Pr(m -> y) by 74%
# State 2 - State 2 -23.93943 (-24.36220,-23.5240)
# State 2 - State 3  23.82279 ( 23.40860, 24.2443) 0.80215 (0.78584,0.81879) # reduce Pr(m -> o) by 20%
# State 3 - State 1   0.01268 (  0.00781,  0.0206) 0.01736 (0.01297,0.02322) # reduce Pr(o -> y) to almost nothing
# State 3 - State 2  29.11131 ( 28.58774, 29.6445) 1.89735 (1.85195,1.94386) # increase Pr(o -> m) by 90%
# State 3 - State 3 -29.12399 (-29.65708,-28.6005)                          
# -2 * log-likelihood:  -129871

qmatrix.msm(x = mkv_cov, covariates = 'mean', sojourn = T, ci = 'bootstrap') # calculate transition intensity matrix
#           State 1                        State 2                        State 3                       
# 

sojourn.msm(mkv_cov) # time predicted to be spent in each state
#          estimates           SE          L          U
# State 1 0.03722544 0.0003306851 0.03658292 0.03787925
# State 2 2.45807413 0.2639655757 1.99153007 3.03391273
# State 3 0.03227033 0.0002866732 0.03171333 0.03283712

pmatrix.msm(mkv_cov, t=1)  # transition probability at time 1
#           State 1   State 2   State 3
# State 1 0.4983486 0.06940858 0.4322428
# State 2 0.1711802 0.67915503 0.1496647
# State 3 0.4981231 0.06982880 0.4320481

pmatrix.msm(mkv_cov, t=10) # transition probability at time 10 -- is there a way to do this for t=start of playback?
#           State 1  State 2   State 3
# State 1 0.4406180 0.1770016 0.3823804
# State 2 0.4368173 0.1840850 0.3790977
# State 3 0.4406154 0.1770065 0.3823781

pmatrix.msm(mkv_cov, ci="boot", B=100) # Repeatedly draw bootstrap resamples of the data, refit the model for each sample, and summarise the estimates over the repeated samples
# 

totlos.msm(mkv_cov, t=10)  # predicted length of time spent in each state, both now and in the future
#  State 1  State 2  State 3 
# 4.599630 1.424465 3.975905

plot.prevalence.msm(mkv_cov) # plot predictions for proportion of population in each state, based on starting proportions

################################ ORDINAL REGRESSION MODEL -- WRONG, AS NOTHING STOPPING A DIRECT CHANGE FROM YOUNGER TO OLDER, DOESN'T HAVE TO GO THROUGH SAME AGE. CONVERT TO A MULTINOMIAL IF DAN CAN'T WORK OUT HOW TO DO IT AS A MARKOV ###########################################
# #### ordinal logistic regression -- run model ####
# # https://dagitty.net/dags.html?id=fQrEyF#
# pdf('outputs/neighbour_ordinal_model/neighbour_ordinal_modelprep.pdf')
# 
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
# # 1  3  4  6  7 10 11 13 14 15 16 17 18 19 21 22 23 24 25 28 29 30 31 32 33 34 35 36 37 38 41 42 43 44 45 46 47 48 50 51 52 53 55 56 58 59 60 61 
# # 1  1  1  1  1  2  1  1  1  1  1  1  1  1  1  1  1  2  1  1  2  1  1  3  1  1  1  1  1  1  1  1  1  1  1  2  1  1  1  1  1  5  1  1  1  1  1  1 
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
# ## nearest neighbour data
# cols_of_interest <- c('b1_nn','b2_nn','b3_nn','b4_nn',
#                       'b5_nn','b6_nn','b7_nn','b8_nn')
# cols_of_interest_name <- c('b1_nn_name','b2_nn_name','b3_nn_name','b4_nn_name',
#                            'b5_nn_name','b6_nn_name','b7_nn_name','b8_nn_name')
# cols_of_interest_index <- c('b1_nn_index','b2_nn_index','b3_nn_index','b4_nn_index',
#                             'b5_nn_index','b6_nn_index','b7_nn_index','b8_nn_index')
# nn_all <- readRDS('data_processed/behaviour_by_second_indexvariables.RDS') %>%
#   # nn_all <- readRDS('../data_processed/behaviour_by_second_indexvariables.RDS') %>%
#   filter(out_frame_name == 'in_frame') %>% 
#   select(subject,pb_num,second,
#          all_of(cols_of_interest_name),all_of(cols_of_interest_index)) %>%
#   rename(b1_nn = b1_nn_name, b2_nn = b2_nn_name,
#          b3_nn = b3_nn_name, b4_nn = b4_nn_name,
#          b5_nn = b5_nn_name, b6_nn = b6_nn_name,
#          b7_nn = b7_nn_name, b8_nn = b8_nn_name) %>% 
#   pivot_longer(cols = all_of(cols_of_interest),
#                names_to = 'elephant_activity_name', values_to = 'neighbour') %>% 
#   rename(b1_nn = b1_nn_index, b2_nn = b2_nn_index,
#          b3_nn = b3_nn_index, b4_nn = b4_nn_index,
#          b5_nn = b5_nn_index, b6_nn = b6_nn_index,
#          b7_nn = b7_nn_index, b8_nn = b8_nn_index) %>% 
#   pivot_longer(cols = all_of(cols_of_interest),
#                names_to = 'elephant_activity_index', values_to = 'nn_index') %>% 
#   filter(elephant_activity_name == elephant_activity_index) %>% 
#   select(-elephant_activity_index) %>% 
#   rename(elephant_activity = elephant_activity_name,
#          focal = subject) %>% 
#   filter(is.na(nn_index) == FALSE) %>% 
#   separate(elephant_activity, into = c('partner','activity'), sep = '_', remove = T) %>% 
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
#          activity,neighbour,
#          stim_num,stim_type,
#          time_since_stim, after_stim,
#          f_age_cat,p_age_cat,f_age_num,p_age_num,
#          age_difference) %>% 
#   mutate(f_age_num = as.factor(f_age_num),
#          p_age_num = as.factor(p_age_num),
#          age_combo = paste0(f_age_num,'_',p_age_num),
#          nn_tminus1 = NA,
#          nn_tminus1_num = NA)
# rm(list = ls() [ ! ls() %in% 'nn_all']) ; gc()
# 
# unique(nn_all$focal[is.na(nn_all$f_age_num)])   # b6_e7 = unknown age
# unique(nn_all$partner[is.na(nn_all$p_age_num)]) # b2_e13 + b2_e34 = unknown age
# length(which(is.na(nn_all$neighbour) == TRUE))
# 
# nn <- nn_all %>%
#   filter(neighbour == 1) %>%
#   filter(is.na(f_age_num) == FALSE) %>%
#   filter(is.na(p_age_num) == FALSE) %>% 
#   select(-neighbour) %>% 
#   rename(neighbour = partner) %>% 
#   mutate(age_difference = factor(age_difference,
#                                  levels = c('partner_younger',
#                                             'matched',
#                                             'partner_older'))) %>%
#   mutate(age_diff_num = as.integer(age_difference),
#          f_age_num = as.integer(f_age_num),
#          p_age_num = as.integer(p_age_num)) %>% 
#   relocate(age_diff_num, .after = age_difference)
# 
# # create variable for nearest neighbour at time t-1
# focals <- unique(nn$focal)
# for(f in 1:length(focals)){
#   focal <- nn %>% filter(focal == focals[f])
#   nn <- nn %>% anti_join(focal, by = 'focal')
#   for(i in 2:nrow(focal)){
#     focal$nn_tminus1[i] <- as.character(focal$age_difference[i-1])
#     focal$nn_tminus1_num[i] <- focal$age_diff_num[i-1]
#   }
#   nn <- rbind(nn, focal)
# }
# rm(list = ls() [! ls() %in% c('nn','nn_all')]) ; gc()
# 
# # filter to remove elephants with unknown ages
# nn_no_na <- nn %>%
#   filter(is.na(age_diff_num) == FALSE) %>%
#   filter(is.na(nn_tminus1) == FALSE) %>%
#   mutate(focal_id = as.integer(as.factor(focal)),
#          stim_id = as.integer(as.factor(stim_num)),
#          nn_tminus1 = factor(nn_tminus1, levels = c('partner_younger',
#                                                     'matched',
#                                                     'partner_older'))) %>%
#   rename(playback_id = pb_num)
# str(nn_no_na)
# # $ playback_id    : num [1:50349] 1 1 1 1 1 1 1 1 1 1 ...
# # $ focal          : chr [1:50349] "b1_e1" "b1_e1" "b1_e1" "b1_e1" ...
# # $ neighbour      : chr [1:50349] "b3_e1" "b3_e1" "b3_e1" "b3_e1" ...
# # $ activity       : chr [1:50349] "nn" "nn" "nn" "nn" ...
# # $ stim_num       : chr [1:50349] "14" "14" "14" "14" ...
# # $ stim_type      : chr [1:50349] "ctd" "ctd" "ctd" "ctd" ...
# # $ time_since_stim: num [1:50349] -81 -80 -79 -78 -77 -76 -75 -74 -73 -72 ...
# # $ after_stim     : num [1:50349] 0 0 0 0 0 0 0 0 0 0 ...
# # $ f_age_cat      : chr [1:50349] "26-35" "26-35" "26-35" "26-35" ...
# # $ p_age_cat      : chr [1:50349] "26-35" "26-35" "26-35" "26-35" ...
# # $ f_age_num      : int [1:50349] 4 4 4 4 4 4 4 4 4 4 ...
# # $ p_age_num      : int [1:50349] 4 4 4 4 4 4 4 4 4 4 ...
# # $ age_difference : Factor w/ 3 levels "partner_younger",..: 2 2 2 2 2 2 2 2 2 2 ...
# # $ age_diff_num   : int [1:50349] 2 2 2 2 2 2 2 2 2 2 ...
# # $ age_combo      : chr [1:50349] "4_4" "4_4" "4_4" "4_4" ...
# # $ nn_tminus1     : Factor w/ 3 levels "partner_younger",..: 2 2 2 2 2 2 2 2 2 2 ...
# # $ nn_tminus1_num : int [1:50349] 2 2 2 2 2 2 2 2 2 2 ...
# # $ focal_id       : int [1:50349] 1 1 1 1 1 1 1 1 1 1 ...
# # $ stim_id        : int [1:50349] 5 5 5 5 5 5 5 5 5 5 ...
# 
# # set priors -- prior predictive: I think all age categories should have equal priors, as while we would probably expect there to be the biggest difference between oldest and youngest, that's not something we're certain of.
# get_prior(formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
#             s(after_stim) + mo(nn_tminus1_num) +                       # controls
#             (1|focal_id) + (1|stim_id) + (1|playback_id),              # random effects
#           data = nn_no_na,
#           family = cumulative("logit"))
# priors <- c(
#   # focal age
#   prior(normal(0,0.25),     class = b,    coef = mof_age_num),
#   prior(dirichlet(2,2,2),   class = simo, coef = mof_age_num1),
#   # stim type
#   prior(normal(0,1),        class = b,    coef = stim_typeh),
#   prior(normal(0,1),        class = b,    coef = stim_typel),
#   # time spline
#   prior(normal(0,1),        class = b,    coef = safter_stim_1),
#   #prior(student_t(3,0,2.5), class = sds,  coef = s(after_stim)),
#   # action in previous second
#   prior(normal(1,1), # normal(0,0.333),
#         class = b, coef = monn_tminus1_num),
#   prior(dirichlet(2,2),     class = simo, coef = monn_tminus1_num1))
# 
# ## prior predictive check
# num_chains <- 4
# num_iter <- 2000
# nn_prior <- brm(
#   formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
#     s(after_stim) + mo(nn_tminus1_num) +                       # controls
#     (1|focal_id) + (1|stim_id) + (1|playback_id),              # random effects
#   data = nn_no_na,
#   family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains,
#   iter = num_iter, warmup = num_iter/2, seed = 12345,
#   sample_prior = 'only')
# pp_check(nn_prior) # huge variation in prior, but fairly on both sides so good
# 
# ## fit model
# nn_fit <- brm(
#   formula = age_diff_num ~ 1 + mo(f_age_num) + stim_type +     # fixed effects
#     s(after_stim) + mo(nn_tminus1_num) +                       # controls
#     (1|stim_id) + (1|stim_id:playback_id) + (1|stim_id:playback_id:focal_id),  # nested random effects
#   data = nn_no_na,
#   family = cumulative("logit"),
#   prior = priors, chains = num_chains, cores = num_chains, threads = threading(4),
#   iter = num_iter, warmup = num_iter/2, seed = 12345)
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_run.RData')
# dev.off()
# 
# #### check outputs ####
# # load('ele_playbacks/nearest_neighbour/neighbour_ordinal_run.RData') # load('nearest_neighbour/neighbour_ordinal_run.RData')
# pdf('outputs/neighbour_ordinal_model/neighbour_ordinal_modelchecks.pdf')
# 
# ## check Stan code
# nn_fit$model
# 
# ## check model fit
# summary(nn_fit)
# # Formula: age_diff_num ~ 1 + mo(f_age_num) + stim_type + s(after_stim) + mo(nn_tminus1_num) + (1 | stim_id) + (1 | stim_id:playback_id) + (1 | stim_id:playback_id:focal_id)
# # Data: nn_no_na (Number of observations: 50349)
# # Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1; total post-warmup draws = 4000
# # Smooth Terms:
# #                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sds(safter_stim_1)     0.50      0.50     0.01     1.88 1.00     1850     2077
# # 
# # Group-Level Effects:
# #   ~stim_id (Number of levels: 30)
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     0.22      0.16     0.01     0.59 1.00      711     1489
# # 
# # ~stim_id:playback_id (Number of levels: 48)
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     0.31      0.18     0.02     0.68 1.01      470     1202
# # 
# # ~stim_id:playback_id:focal_id (Number of levels: 171)
# #               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)     0.15      0.12     0.00     0.45 1.00      919     1103
# # 
# # Population-Level Effects:
# #                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # Intercept[1]         4.26      0.43     3.35     5.00 1.01      720     1909
# # Intercept[2]        15.21      0.42    14.34    15.95 1.01      838     1992
# # stim_typeh           0.07      0.21    -0.34     0.50 1.00     3012     2455
# # stim_typel           0.02      0.24    -0.45     0.51 1.00     2885     2186
# # safter_stim_1       -0.06      0.76    -1.58     1.48 1.00     3725     2733
# # mof_age_num         -0.62      0.17    -0.96    -0.34 1.01      587     1859
# # monn_tminus1_num    10.62      0.15    10.34    10.90 1.00     1155     2082
# # 
# # Simplex Parameters:
# #                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # mof_age_num1[1]          0.20      0.10     0.03     0.40 1.00     4200     2424
# # mof_age_num1[2]          0.54      0.10     0.36     0.74 1.00     5187     2901
# # mof_age_num1[3]          0.26      0.09     0.09     0.44 1.00     3661     1638
# # monn_tminus1_num1[1]     0.51      0.01     0.50     0.53 1.00     5812     3358
# # monn_tminus1_num1[2]     0.49      0.01     0.47     0.50 1.00     5812     3358
# # 
# # Family Specific Parameters:
# #   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# # disc     1.00      0.00     1.00     1.00   NA       NA       NA
# # 
# # Draws were sampled using sampling(NUTS). For each parameter, and Tail_ESS are effective sample size measures, and Rhat is scale reduction factor on split chains (at convergence, Rhat
# # Warning message: There were 3 divergent transitions after warmup. Increasing a8 may help. See http://mc-stan.org/misc/warnings.html#diverger-warmup
# # Warning message: There were 3 divergent transitions after warmup. Increasing a8 may help. See http://mc-stan.org/misc/warnings.html#diverger-warmup
# 
# ## check chain mixing
# summary <- summary(nn_fit)
# hist(summary$fixed$Rhat, breaks = 50)
# range(summary$fixed$Rhat)
# # 1.001229 1.013411
# 
# ## check effective sample size
# hist(summary$fixed$Bulk_ESS, breaks = 50)
# range(summary$fixed$Bulk_ESS)
# # 586.7052 3724.9700
# rownames(summary$fixed)[which(summary$fixed$Bulk_ESS == min(summary$fixed$Bulk_ESS))] # f_age_num has the lowest effective sample size
# hist(summary$fixed$Tail_ESS, breaks = 50)
# range(summary$fixed$Tail_ESS)
# # 1858.512 2732.741
# 
# summary$random
# # $stim_id
# #                Estimate Est.Error    l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# # sd(Intercept) 0.2150773 0.1582964 0.009541558 0.5916455 1.003302 710.5277 1489.159
# # 
# # $`stim_id:playback_id`
# #                  Estimate Est.Error   l-95% CI  u-95% CI    Rhat Bulk_ESS Tail_ESS
# # sd(Intercept)   0.3064286 0.1847595 0.01767821 0.6831676 1.01342 469.7863 1202.196
# # 
# # $`stim_id:playback_id:focal_id`
# #                Estimate Est.Error    l-95% CI  u-95% CI     Rhat Bulk_ESS Tail_ESS
# # sd(Intercept) 0.1462336 0.1194749 0.004368648 0.4512497 1.001519 918.7878 1102.701
# 
# ## extract posterior distribution
# draws <- as_draws_df(nn_fit) %>%
#   select(-disc, -lprior, -lp__, -`.chain`, -`.iteration`, -`.draw`) %>%
#   pivot_longer(cols = everything(), names_to = 'parameter', values_to = 'draw') %>%
#   mutate(iteration = rep(rep(1:(num_iter/2),
#                              each = length(unique(parameter))),
#                          num_chains),
#          chain = rep(1:num_chains,
#                      each = length(unique(parameter))*(num_iter/2)),
#          invlogit_draw = invlogit(draw))
# 
# ## extract marginal effects
# marg <- conditional_effects(nn_fit,
#                             effects = c('f_age_num','stim_type',
#                                         'after_stim','nn_tminus1_num'),
#                             categorical = TRUE,
#                             #spaghetti = TRUE,
#                             method = 'posterior_epred')
# names(marg)
# # "f_age_num:cats__"      "stim_type:cats__"      "after_stim:cats__"     "nn_tminus1_num:cats__"
# f_age_effect <- marg[[1]]
# stim_effect <- marg[[2]]
# time_effect <- marg[[3]]
# prevsec_effect <- marg[[4]]
# 
# #### plot marginal effects
# # conditional_effects(nn_fit, effects = 'f_age_num',
# #                     categorical = TRUE,
# #                     spaghetti = TRUE,
# #                     #conditions = c('stim_type'),
# #                     method = 'posterior_epred')
# (focal_age_plot <- ggplot(f_age_effect)+
#    geom_errorbar(aes(x = f_age_num, ymax = upper__, ymin = lower__, colour = cats__), linewidth = 1, width = 0.2)+
#    #geom_ribbon(aes(x = f_age_num, ymax = upper__, ymin = lower__, fill = cats__), alpha = 0.4)+
#    geom_point(aes(x = f_age_num, y = estimate__, colour = cats__), cex = 3)+
#    #geom_line(aes(x = f_age_num, y = estimate__, colour = cats__), linewidth = 1)+
#    xlab(label = 'focal age') + ylab('probability')+
#    scale_colour_viridis_d(name = 'age difference:',
#                           breaks = c('1','2','3'),
#                           labels = c('partner younger',
#                                      'age matched',
#                                      'partner older'))+
#    scale_fill_viridis_d(name = 'age difference:',
#                         breaks = c('1','2','3'),
#                         labels = c('partner younger',
#                                    'age matched',
#                                    'partner older')))+
#   theme(legend.position = 'bottom',
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 10))
# ggsave(plot = focal_age_plot, filename = 'outputs/neighbour_ordinal_model/neighbour_ordinal_marginaleffects_focalage.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# # conditional_effects(nn_fit, 'stim_type',
# #                     categorical = TRUE,
# #                     method = 'posterior_epred')
# (stim_plot <- ggplot(stim_effect)+
#     geom_errorbar(aes(x = stim_type, ymin = lower__, ymax = upper__, colour = cats__), linewidth = 1, width = 0.2)+
#     geom_point(aes(x = stim_type, y = estimate__, colour = cats__),cex = 3)+
#     xlab(label = 'stimulus type') + ylab('probability')+
#     scale_colour_viridis_d(name = 'neighbour age:',
#                            breaks = c('1','2','3'),
#                            labels = c('younger', 'matched', 'older'))+
#     scale_fill_viridis_d(name = 'neighbour age:',
#                          breaks = c('1','2','3'),
#                          labels = c('younger', 'matched', 'older'))+
#     scale_x_discrete(name = 'stimulus type', breaks = c('ctd','l','h'),
#                      labels = c('dove (control)', 'lion', 'human'),
#                      limits = c('ctd','l','h')))+
#   theme(legend.position = 'bottom',
#         axis.title = element_text(size = 16),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 10))
# ggsave(plot = stim_plot, filename = 'outputs/neighbour_ordinal_model/neighbour_ordinal_marginaleffects_stimtype.png', device = 'png',
#        width = 8.3, height = 5.8)
# 
# #(all_plots <- ggarrange(focal_age_plot, stim_plot, ncol=2, nrow=1, common.legend = TRUE, legend = "bottom"))
# #ggsave(plot = all_plots, filename = '../outputs/neighbour_ordinal_model/neighbour_ordinal_marginaleffects.png', device = 'png',
# #       width = (5.8*2), height = 8.3)
# 
# ggsave(plot = focal_age_plot, filename = 'outputs/neighbour_ordinal_model/neighbour_ordinal_marginaleffects_focalage.png', device = 'png',
#        width = 5.8, height = 8.3)
# ggsave(plot = stim_plot, filename = 'outputs/neighbour_ordinal_model/neighbour_ordinal_marginaleffects_stimtype.png', device = 'png',
#        width = 5.8, height = 8.3)
# 
# rm(f_age_effect,prevsec_effect, stim_effect, time_effect) ;gc()
# 
# #### posterior predictive check
# pp_check(nn_fit, ndraws = 100) # perfect fit
# 
# #### plot traces
# unique(draws$parameter)
# draws_cut <- draws %>%
#   filter(parameter %in% c("b_Intercept[1]","b_Intercept[2]",
#                           "b_stim_typeh","b_stim_typel",
#                           "bs_safter_stim_1","sds_safter_stim_1",
#                           "s_safter_stim_1[1]","s_safter_stim_1[2]",
#                           "s_safter_stim_1[3]","s_safter_stim_1[4]",
#                           "s_safter_stim_1[5]","s_safter_stim_1[6]",
#                           "s_safter_stim_1[7]","s_safter_stim_1[8]",
#                           "bsp_mof_age_num",
#                           "simo_mof_age_num1[1]","simo_mof_age_num1[2]","simo_mof_age_num1[3]",
#                           "sd_focal_id__Intercept","sd_playback_id__Intercept","sd_stim_id__Intercept",
#                           "bsp_monn_tminus1_num",
#                           "simo_monn_tminus1_num1[1]","simo_monn_tminus1_num1[2]"
#   )) 
# draws_cut %>%
#   ggplot(aes(x = iteration, y = draw, colour = as.factor(chain)))+
#   geom_line()+
#   facet_wrap(. ~ parameter, scales = 'free_y')+
#   theme(legend.position = 'none') # mostly fine, but playback ID intercept has a weird unmixed bit
# 
# #### plot density curves
# ## move at intercepts (estimates of cutpoints between categories on linear model scale)
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
# mean(lion$draw) ; sd(lion$draw)
# # 0.01961241 ± 0.2350546
# plot(density(human$draw), main = 'human vs dove') ; abline(v = 0, lty = 2)
# mean(human$draw) ; sd(human$draw)
# # 0.07305522 ± 0.2121808
# 
# ## focal age
# age1 <- draws_cut %>% filter(parameter == 'bsp_mof_age_num')
# age2 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[1]')
# age3 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[2]')
# age4 <- draws_cut %>% filter(parameter == 'simo_mof_age_num1[3]')
# par(mfrow = c(2,2))
# plot(density(age1$draw), main = 'age intercept') ; abline(v = 0, lty = 2)
# mean(age1$draw) ; sd(age1$draw)
# # -0.6182791 ± 0.1664937
# plot(density(age2$draw), main = 'age2 vs age1') ; abline(v = 0, lty = 2)
# mean(age2$draw) ; sd(age2$draw)
# # 0.1953138 ± 0.09633965
# plot(density(age3$draw), main = 'age3 vs age1') ; abline(v = 0, lty = 2)
# mean(age3$draw) ; sd(age3$draw)
# # 0.5418168 ± 0.0995131
# plot(density(age4$draw), main = 'age4 vs age1') ; abline(v = 0, lty = 2)
# mean(age4$draw) ; sd(age4$draw)
# # 0.2628693 ± 0.08918488
# 
# ## neighbour in previous second
# prevsec1 <- draws_cut %>% filter(parameter == 'bsp_monn_tminus1_num')
# prevsec2 <- draws_cut %>% filter(parameter == 'simo_monn_tminus1_num1[1]')
# prevsec3 <- draws_cut %>% filter(parameter == 'simo_monn_tminus1_num1[2]')
# par(mfrow = c(3,1))
# plot(density(prevsec1$draw), main = 't-1 younger') ; abline(v = 0, lty = 2)
# mean(prevsec1$draw) ; sd(prevsec1$draw)
# # 10.61679 ± 0.1468672
# plot(density(prevsec2$draw), main = 't-1 matched') ; abline(v = 0, lty = 2)
# mean(prevsec2$draw) ; sd(prevsec2$draw)
# # 0.512553 ± 0.006696236
# plot(density(prevsec3$draw), main = 't-1 older') ; abline(v = 0, lty = 2)
# mean(prevsec3$draw) ; sd(prevsec3$draw)
# # 0.487447 ± 0.006696236
# 
# ## time since stimulus
# timeb <- draws_cut %>% filter(parameter == 'bs_safter_stim_1')
# times <- draws_cut %>% filter(parameter == 'sds_safter_stim_1')
# time1 <- draws_cut %>% filter(parameter == 's_safter_stim_1[1]')
# time2 <- draws_cut %>% filter(parameter == 's_safter_stim_1[2]')
# time3 <- draws_cut %>% filter(parameter == 's_safter_stim_1[3]')
# time4 <- draws_cut %>% filter(parameter == 's_safter_stim_1[4]')
# time5 <- draws_cut %>% filter(parameter == 's_safter_stim_1[5]')
# time6 <- draws_cut %>% filter(parameter == 's_safter_stim_1[6]')
# time7 <- draws_cut %>% filter(parameter == 's_safter_stim_1[7]')
# time8 <- draws_cut %>% filter(parameter == 's_safter_stim_1[8]')
# par(mfrow = c(5,2))
# plot(density(timeb$draw), main = 'time slope') ; abline(v = 0, lty = 2)
# mean(timeb$draw) ; sd(timeb$draw)
# # -0.05578981 ± 0.7562788
# plot(density(times$draw), main = 'time intercept') ; abline(v = 0, lty = 2)
# mean(times$draw) ; sd(times$draw)
# # 0.5003595 ± 0.5041333
# plot(density(time1$draw), main = 'time spline 1') ; abline(v = 0, lty = 2)
# mean(time1$draw) ; sd(time1$draw)
# # -0.06217671 ± 0.7570976
# plot(density(time2$draw), main = 'time spline 2') ; abline(v = 0, lty = 2)
# mean(time2$draw) ; sd(time2$draw)
# # 0.04705627 ± 0.486139
# plot(density(time3$draw), main = 'time spline 3') ; abline(v = 0, lty = 2)
# mean(time3$draw) ; sd(time3$draw)
# # 0.02241826 ± 0.3337269
# plot(density(time4$draw), main = 'time spline 4') ; abline(v = 0, lty = 2)
# mean(time4$draw) ; sd(time4$draw)
# # -0.09282363 ± 0.488172
# plot(density(time5$draw), main = 'time spline 5') ; abline(v = 0, lty = 2)
# mean(time5$draw) ; sd(time5$draw)
# # 0.02672542 ± 0.6521429
# plot(density(time6$draw), main = 'time spline 6') ; abline(v = 0, lty = 2)
# mean(time6$draw) ; sd(time6$draw)
# # 0.01953563 ± 0.6604809
# plot(density(time7$draw), main = 'time spline 7') ; abline(v = 0, lty = 2)
# mean(time7$draw) ; sd(time7$draw)
# # 0.02226062 ± 0.6840069
# plot(density(time8$draw), main = 'time spline 8') ; abline(v = 0, lty = 2)
# mean(time8$draw) ; sd(time8$draw)
# # 0.01007547 ± 0.7118232
# 
# #### plot raw
# par(mfrow = c(1,1))
# age_labels <- c('10-15 years','16-20 years','21-25 years','26-35 years')
# names(age_labels) <- c(1,2,3,4)
# stim_labels <- c('dove (control)','human','lion')
# names(stim_labels) <- c('ctd','h','l')
# 
# ggplot(nn_no_na, aes(x = f_age_num, y = age_diff_num,
#                      colour = as.factor(nn_tminus1_num)))+
#   geom_jitter(alpha = 0.1)+
#   facet_wrap(. ~ stim_type,
#              labeller = labeller(stim_type = stim_labels))+
#   scale_y_continuous(name = 'neighbour age relative to focal',
#                      breaks = c(1,2,3),
#                      labels = c('younger','matched','older'))+
#   labs(colour = 'neigbour age at t-1:')
# 
# nn_no_na %>%
#   ggplot(aes(x = time_since_stim, y = age_diff_num))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(aes(colour = as.factor(f_age_num)#rgb(0,0,1,0.01)
#   ), alpha = 0.01)+
#   facet_grid(f_age_num ~ stim_type,
#              labeller = labeller(f_age_num = age_labels,
#                                  stim_type = stim_labels))+
#   scale_y_continuous(name = 'neighbour age relative to focal',
#                      breaks = c(1,2,3),
#                      labels = c('younger',
#                                 'matched',
#                                 'older'))+
#   scale_x_continuous(name = 'time since stimulus started (s)') # no particular effect of time since stimulus -- all seem pretty similar before and after. possible differences between stimuli: youngest are more likely to age match during control but be near older during lion/human (but that is also the case beforethe stimulus); 21-25 more likely to age match during lion than other 2; oldest switch from being near younger to age matching during lion stimuli; oldest do not age match for human stimuli; 16-20 more likely to be near youngest after lion and human than before
# 
# ## plot raw data -- this looks a lot more exciting at first glance than it is: basically just shows that far and away the strongest effect is neighbour age at second t-1
# stimuli <- c('dove (control)','lion','human')
# names(stimuli) <- c('ctd','l','h')
# nn_no_na %>%
#   mutate(stim_type = factor(stim_type, levels = c('ctd','l','h'))) %>%
#   ggplot(aes(x = after_stim, y = age_diff_num,
#              colour = stim_type, shape = nn_tminus1))+
#   geom_vline(aes(xintercept = 0))+
#   geom_point(alpha = 0.1)+ # colour = rgb(0,0,1,0.01)
#   facet_grid(stim_type ~ factor(nn_tminus1,
#                                 levels = c('partner_younger',
#                                            'matched',
#                                            'partner_older')),
#              labeller = labeller(stim_type = stimuli))
# 
# ## save output
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_run.RData')
# dev.off()
# 
# #### predict from model ####
# #load('ele_playbacks/nearest_neighbour/neighbour_ordinal_run.RData') # load('nearest_neighbour/neighbour_ordinal_run.RData')
# rm(list = ls()[! ls() %in% c('nn_fit','nn_no_na')]) ; gc()
# pdf('outputs/neighbour_ordinal_model/neighbour_ordinal_modelpredictions.pdf')
# 
# ## predict from raw data
# nn_no_na$unique_data_combo <- 1:nrow(nn_no_na)
# pred_mtx <- posterior_epred(object = nn_fit, newdata = nn_no_na)
# colnames(pred_mtx) <- nn_no_na$unique_data_combo
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_predictions.RData')
# 
# ## convert predictions to long format data set, using only first 100 values per chain
# #load('ele_playbacks/nearest_neighbour/neighbour_ordinal_predictions.RData')
# predictions1 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,1] %>%
#   as.data.frame() %>%
#   pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
#   mutate(unique_data_combo = as.integer(unique_data_combo),
#          pred_type = 1) %>%
#   left_join(nn_no_na, by = 'unique_data_combo')
# predictions2 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,2] %>%
#   as.data.frame() %>%
#   pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
#   mutate(unique_data_combo = as.integer(unique_data_combo),
#          pred_type = 2) %>%
#   left_join(nn_no_na, by = 'unique_data_combo')
# predictions3 <- pred_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,3] %>%
#   as.data.frame() %>%
#   pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
#   mutate(unique_data_combo = as.integer(unique_data_combo),
#          pred_type = 3) %>%
#   left_join(nn_no_na, by = 'unique_data_combo')
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_predictions.RData')
# 
# ## create long format data set with all predictions -- use for calculating some uncertainty measure (standard deviation/error amongst sets of predictions)
# # predictions_all <- predictions %>%
# #   mutate(chain_set100 = 1)
# # for(i in 2:10){
# #   j <- seq(from = 100*(i-1)+1, to = (100*i), by = 1)
# #   pred <- pred_mtx[c(j,1000+j,2000+j,3000+j),] %>%
# #     as.data.frame() %>%
# #     pivot_longer(everything(), names_to = 'unique_data_combo', values_to = 'prediction') %>%
# #     mutate(unique_data_combo = as.integer(unique_data_combo)) %>%
# #     left_join(nn_no_na, by = 'unique_data_combo') %>%
# #     mutate(chain_set100 = i)
# #   predictions_all <- rbind(predictions_all, pred)
# # }
# predictions_all <- rbind(predictions1, predictions2, predictions3)
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_predictions.RData')
# 
# #### plot predictions
# #load('ele_playbacks/nearest_neighbour/neighbour_ordinal_predictions.RData')  #load('nearest_neighbour/neighbour_ordinal_predictions.RData')
# rm(pred_mtx, predictions1, predictions2, predictions3) ; gc()
# 
# ## make labels for prediction type look nice
# predictions_all <- predictions_all %>%
#   mutate(pred_label = ifelse(pred_type == 1, 'younger',
#                              ifelse(pred_type == 2, 'age matched', 'older')))
# 
# ## make labels for age of neighbour in previous second look nice
# prevsec_labels <- c('neighbour younger at t-1',
#                     'neighbour same age at t-1',
#                     'neighbour older at t-1')
# names(prevsec_labels) <- 1:3
# 
# ## plot in 3 sections -- split by stimulus type as the thing that I changed, each graph by age as the thing I'm interested in
# (ctd_plot <- predictions_all %>%
#     filter(stim_type == 'ctd',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = prediction,
#                     fill = as.factor(pred_label),
#                     colour = as.factor(pred_label))) +
#     # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
#     #                 colour = as.factor(pred_label)))+
#     facet_grid(nn_tminus1_num ~ after_stim,
#                labeller = labeller(nn_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted age of neighbour relative to focal:',
#          fill = 'predicted age of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'cape turtle dove (control)')+
#     theme(legend.position = 'bottom'))
# (lion_plot <- predictions_all %>%
#     filter(stim_type == 'l',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = prediction,
#                     fill = as.factor(pred_label),
#                     colour = as.factor(pred_label))) +
#     # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
#     #                 colour = as.factor(pred_label)))+
#     facet_grid(nn_tminus1_num ~ after_stim,
#                labeller = labeller(nn_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted age of neighbour relative to focal:',
#          fill = 'predicted age of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'lion')+
#     theme(legend.position = 'bottom'))
# (human_plot <- predictions_all %>%
#     filter(stim_type == 'h',
#            after_stim %in% c(0, 0.5, 1, 1.5, 2, 2.5, 3)) %>%
#     ggplot()+
#     geom_violin(aes(x = as.factor(f_age_num), y = prediction,
#                     fill = as.factor(pred_label),
#                     colour = as.factor(pred_label))) +
#     # geom_boxplot(aes(x = as.factor(f_age_num), y = prediction,
#     #                 colour = as.factor(pred_label)))+
#     facet_grid(nn_tminus1_num ~ after_stim,
#                labeller = labeller(nn_tminus1_num = prevsec_labels))+
#     scale_fill_viridis_d()+
#     scale_colour_viridis_d()+
#     labs(colour = 'predicted age of neighbour relative to focal:',
#          fill = 'predicted age of neighbour relative to focal:',
#          x = 'age category of focal elephant',
#          y = 'proportion of predictions',
#          title = 'human')+
#     theme(legend.position = 'bottom'))
# (ctd_plot + lion_plot + human_plot)+
#   plot_annotation(tag_levels = 'a')
# ggsave(plot = last_plot(), file = '../outputs/neighbour_ordinal_model/neighbour_ordinal_predictions_violin.png',
#        device = 'png', height = 8, width = 48)
# 
# ## save output
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_predictions.RData')
# dev.off()
# 
# #### graph contrasts from predictions and extract coefficients ####
# #CALCULATE POSTERIOR CONTRASTS FROM PREDICTIONS
# # load('ele_playbacks/nearest_neighbour/neighbour_ordinal_predictions.RData') ; load('nearest_neighbour/neighbour_ordinal_predictions.RData')
# rm(prevsec_labels, ctd_plot, human_plot, lion_plot, predictions_all) ; gc()
# pdf('outputs/neighbour_ordinal_model/neighbour_ordinal_modelcontrasts.pdf')
# 
# ## stim type
# ## redo predictions with different stimulus types: all doves
# ctd_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'ctd',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# ctd_mtx <- posterior_epred(object = nn_fit, newdata = ctd_nn)
# colnames(ctd_mtx) <- ctd_nn$unique_data_combo
# ctd_mtx <- ctd_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all lions
# lion_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'l',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# lion_mtx <- posterior_epred(object = nn_fit, newdata = lion_nn)
# colnames(lion_mtx) <- lion_nn$unique_data_combo
# lion_mtx <- lion_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different stimulus types: all humans
# human_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(stim_type = 'h',
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# human_mtx <- posterior_epred(object = nn_fit, newdata = human_nn)
# colnames(human_mtx) <- human_nn$unique_data_combo
# human_mtx <- human_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_stimuluscontrasts.RData')
# 
# ## count types of each prediction
# #load('nearest_neighbour/neighbour_ordinal_stimuluscontrasts.RData')
# # count_values <- function(vector, levels = c(1,2,3)) {
# #   x <- tabulate(factor(vector, levels), length(levels))
# #   return(list(x))
# # }
# stim_pred <- ctd_nn %>%
#   dplyr::select(-stim_type) %>%
#   # mutate(ctd_count = apply(ctd_mtx, 2, count_values),
#   #        lion_count = apply(lion_mtx, 2, count_values),
#   #        human_count = apply(human_mtx, 2, count_values)) %>%
#   # unnest(c(ctd_count, lion_count, human_count)) %>% # I've done something weird with the count_values function so for now this needs unnesting twice, but should probably fix it at some point! For now this works!
#   # unnest(c(ctd_count, lion_count, human_count)) %>%
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
#            sep = '_m', remove = T) %>%
#   select(-mu) %>%
#   separate(col = stim_propage_sd, into = c('stim_propage_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-sd) %>%
#   filter(stim_propage_mu == stim_propage_sd) %>%
#   separate(col = stim_propage_mu, into = c('stim_type', 'nn_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-stim_propage_sd) %>%
#   mutate(nn_pred = as.numeric(nn_pred)) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older')))
# 
# ## convert full predictive distribution to long format
# stim_pred_all <- ctd_mtx[,,1] %>%
#   as.data.frame()
# colnames(stim_pred_all) <- rownames(ctd_nn)
# stim_pred_all <- pivot_longer(stim_pred_all, cols = everything(),
#                               names_to = 'rownum', values_to = 'probability')
# ctd_nn$rownum <- rownames(ctd_nn)
# stim_pred_all <- stim_pred_all %>%
#   left_join(ctd_nn, by = 'rownum') %>%
#   mutate(predict_num = 1,
#          predict_cat = 'younger')
# for(i in 2:3){
#   stim_pred_i <- ctd_mtx[,,i] %>%
#     as.data.frame()
#   colnames(stim_pred_i) <- rownames(ctd_nn)
#   stim_pred_i <- pivot_longer(stim_pred_i, cols = everything(),
#                             names_to = 'rownum', values_to = 'probability')
#   stim_pred_i <- stim_pred_i %>%
#     left_join(ctd_nn, by = 'rownum') %>%
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
# contrasts <- nn_no_na %>%
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
#   separate(contrast, into = c('contrast','nn_pred'),
#            sep = '_age', remove = T) %>%
#   separate(nn_pred, into = c('nn_pred','mu'),
#            sep = '_', remove = T) %>%
#   mutate(nn_pred = as.numeric(nn_pred)) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older'))) %>%
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
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   separate(contrast, into = c('stim_a','stim_b'), sep = '_vs_', remove = F) %>%
#   select(pred_type, f_age_num, difference, stim_a, stim_b, contrast, after_stim, nn_tminus1_num) %>%
#   mutate(nn_tminus1 = ifelse(nn_tminus1_num == 1,
#                              'neighbour younger at t-1',
#                              ifelse(nn_tminus1_num == 2,
#                                     'neighbour matched at t-1',
#                                     'neighbour older at t-1')))
# 
# contrasts_long %>%
#   ggplot()+
#   geom_density(aes(x = difference))+
#   facet_grid(pred_type ~ contrast)
# 
# #pdf('../outputs/neighbour_ordinal_model/neighbour_ordinal_contrasts_stimuli.pdf')
# for(i in unique(contrasts_long$contrast)){
#   plot <- contrasts_long %>%
#     filter(contrast == i) %>%
#     ggplot()+
#     geom_density(aes(x = difference, colour = pred_type))+
#     facet_wrap(nn_tminus1 ~ f_age_num,
#                scales = 'free')+
#     labs(title = i)
#   print(plot)
# }
# #dev.off()
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_stimuluscontrasts.RData')
# 
# ## focal age
# # load('nearest_neighbour/neighbour_ordinal_stimuluscontrasts.RData')
# rm(ctd_nn, ctd_mtx, human_nn, human_mtx, lion_nn, lion_mtx,
#    contrasts, contrasts_long, stim_pred,
#    ctd_vs_human_age1, ctd_vs_human_age2, ctd_vs_human_age3,
#    ctd_vs_lion_age1, ctd_vs_lion_age2, ctd_vs_lion_age3,
#    lion_vs_human_age1, lion_vs_human_age2, lion_vs_human_age3) ; gc()
# 
# ## predict with original ages
# age_nn_org <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# age_mtx_org <- posterior_epred(object = nn_fit, newdata = age_nn_org)
# colnames(age_mtx_org) <- age_nn_org$unique_data_combo
# age_mtx_org <- age_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with altered ages
# age_nn_alt <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(f_age_num_original = f_age_num) %>%
#   mutate(f_age_num = ifelse(f_age_num == 4, 1, f_age_num + 1),
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>%
#   relocate(f_age_num_original)
# age_mtx_alt <- posterior_epred(object = nn_fit, newdata = age_nn_alt)
# colnames(age_mtx_alt) <- age_nn_alt$unique_data_combo
# age_mtx_alt <- age_mtx_alt[c(1:100,1001:1100,2001:2100,3001:3100),,]
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_agecontrasts.RData')
# 
# ## summarise and convert to long format
# age_pred <- age_nn_org %>%
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
#                names_to = 'focal_agenn_mu', values_to = 'mean_propn') %>%
#   pivot_longer(cols = c(age_org_prop1_sd,age_org_prop2_sd,age_org_prop3_sd,
#                         age_alt_prop1_sd,age_alt_prop2_sd,age_alt_prop3_sd),
#                names_to = 'focal_agenn_sd', values_to = 'stdv_propn') %>%
#   separate(col = focal_agenn_mu, into = c('focal_agenn_mu','mu'),
#            sep = '_m', remove = T) %>%
#   separate(col = focal_agenn_sd, into = c('focal_agenn_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-mu, -sd) %>%
#   filter(focal_agenn_mu == focal_agenn_sd) %>%
#   separate(col = focal_agenn_mu, into = c('original_altered', 'nn_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-focal_agenn_sd) %>%
#   mutate(nn_pred = as.numeric(nn_pred),
#          f_age_num = ifelse(original_altered == 'age_org',
#                             f_age_num,
#                             ifelse(original_altered == 'age_alt' & f_age_num == 4,
#                                    1, f_age_num + 1))) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older')))
# 
# ## convert full predictive distribution to long format
# age_pred_all <- age_mtx_org[,,1] %>%
#   as.data.frame()
# colnames(age_pred_all) <- rownames(age_nn_org)
# age_pred_all <- pivot_longer(age_pred_all, cols = everything(),
#                              names_to = 'rownum', values_to = 'probability')
# age_nn_org$rownum <- rownames(age_nn_org)
# age_pred_all <- age_pred_all %>%
#   left_join(age_nn_org, by = 'rownum') %>%
#   mutate(predict_num = 1,
#          predict_cat = 'younger')
# for(i in 2:3){
#   age_pred_i <- age_mtx_org[,,i] %>%
#     as.data.frame()
#   colnames(age_pred_i) <- rownames(age_nn_org)
#   age_pred_i <- pivot_longer(age_pred_i, cols = everything(),
#                              names_to = 'rownum', values_to = 'probability')
#   age_pred_i <- age_pred_i %>%
#     left_join(age_nn_org, by = 'rownum') %>%
#     mutate(predict_num = i,
#            predict_cat = ifelse(i == 2, 'matched', 'older'))
#   age_pred_all <- rbind(age_pred_all, age_pred_i)
# }
# 
# ## calculate contrasts
# alt_vs_org_young <- age_mtx_alt[,which(nn_no_na$f_age_num != 4),1] - age_mtx_org[,which(nn_no_na$f_age_num != 4),1]
# alt_vs_org_match <- age_mtx_alt[,which(nn_no_na$f_age_num != 4),2] - age_mtx_org[,which(nn_no_na$f_age_num != 4),2]
# alt_vs_org_older <- age_mtx_alt[,which(nn_no_na$f_age_num != 4),3] - age_mtx_org[,which(nn_no_na$f_age_num != 4),3]
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
# contrasts <- nn_no_na %>%
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
#   separate(contrast, into = c('alt','vs','org','nn_pred','mu'),
#            sep = '_', remove = T) %>%
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
#   mutate(pred_type = ifelse(nn_pred == 'young', 'younger',
#                             ifelse(nn_pred == 'match', 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   mutate(f_age_new = ifelse(f_age_num == 4, 1, f_age_num+1)) %>%
#   select(pred_type, f_age_num, f_age_new, difference, stim_type, after_stim, nn_tminus1_num) %>%
#   mutate(contrast = paste0('org: ',f_age_num,', new: ', f_age_new)) %>%
#   mutate(nn_tminus1 = ifelse(nn_tminus1_num == 1,
#                              'neighbour younger at t-1',
#                              ifelse(nn_tminus1_num == 2,
#                                     'neighbour matched at t-1',
#                                     'neighbour older at t-1')))
# #pdf('../outputs/neighbour_ordinal_model/neighbour_ordinal_contrasts_ages.pdf')
# for(i in unique(contrasts_long$contrast)){
#   plot <- contrasts_long %>%
#     filter(contrast == i) %>%
#     ggplot()+
#     geom_density(aes(x = difference, colour = pred_type))+
#     facet_grid(nn_tminus1 ~ stim_type,
#                scales = 'free')+
#     labs(title = i)
#   print(plot)
# }
# #dev.off()
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_agecontrasts.RData')
# 
# ## neighbour in previous second
# #load('nearest_neighbour/neighbour_ordinal_agecontrasts.RData')
# rm(age_nn_org, age_mtx_org, age_nn_alt, age_mtx_alt, age_pred, alt_vs_org_young, alt_vs_org_match, alt_vs_org_older, contrasts, contrasts_long) ; gc()
# 
# ## redo predictions with different previous neighbours: all younger -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 1, NN AT T-1 YOUNGER
# young_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(nn_tminus1_num = 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# young_mtx <- posterior_epred(object = nn_fit, newdata = young_nn)
# colnames(young_mtx) <- young_nn$unique_data_combo
# young_mtx <- young_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different previous neighbours: all matching
# match_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(nn_tminus1_num = 2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# match_mtx <- posterior_epred(object = nn_fit, newdata = match_nn)
# colnames(match_mtx) <- match_nn$unique_data_combo
# match_mtx <- match_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with different previous neighbours: all older -- NOTE: THIS INCLUDES IMPOSSIBLE COMBINATIONS OF FOCAL AGE 4, NN AT T-1 OLDER
# older_nn <- nn_no_na %>%
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>%
#   mutate(nn_tminus1_num = 3,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# older_mtx <- posterior_epred(object = nn_fit, newdata = older_nn)
# colnames(older_mtx) <- older_nn$unique_data_combo
# older_mtx <- older_mtx[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_prevseccontrasts.RData')
# 
# ## summarise and convert to long format
# load('ele_playbacks/nearest_neighbour/neighbour_ordinal_prevseccontrasts.RData')
# prevsec_pred <- young_nn %>%
#   dplyr::select(-nn_tminus1_num) %>%
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
#            sep = '_m', remove = T) %>%
#   separate(col = prevsec_propage_sd, into = c('prevsec_propage_sd','sd'),
#            sep = '_s', remove = T) %>%
#   select(-mu, -sd) %>%
#   filter(prevsec_propage_mu == prevsec_propage_sd) %>%
#   separate(col = prevsec_propage_mu, into = c('prevsec_type', 'nn_pred'),
#            sep = '_prop', remove = T) %>%
#   select(-prevsec_propage_sd) %>%
#   mutate(nn_pred = as.numeric(nn_pred)) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older')))
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
# young_nn$rownum <- rownames(young_nn)
# match_nn$rownum <- rownames(match_nn)
# older_nn$rownum <- rownames(older_nn)
# for(i in 1:3){
#   for(j in 1:3){
#     if(i == 1){
#       matrix <- young_mtx[,,j]
#       data <- young_nn
#     }
#     if(i == 2){
#       matrix <- match_mtx[,,j]
#       data <- match_nn
#     }
#     if(i == 3){
#       matrix <- older_mtx[,,j]
#       data <- older_nn
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
# contrasts <- nn_no_na %>%
#   select(-nn_tminus1_num) %>%
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
#   separate(contrast, into = c('contrast','nn_pred'),
#            sep = '_age', remove = T) %>%
#   separate(nn_pred, into = c('nn_pred','mu'),
#            sep = '_', remove = T) %>%
#   mutate(nn_pred = as.numeric(nn_pred)) %>%
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older'))) %>%
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
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older'))) %>%
#   mutate(pred_type = factor(pred_type,
#                             levels = c('younger','matched','older'))) %>%
#   separate(contrast, into = c('prevsec_a','prevsec_b'), sep = '_vs_', remove = F) %>%
#   select(pred_type, f_age_num, prevsec_a, prevsec_b, contrast, difference, stim_type, after_stim)
# 
# #pdf('../outputs/neighbour_ordinal_model/neighbour_ordinal_contrasts_prevsec.pdf')
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
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_prevseccontrasts.RData')
# 
# # predictions %>%
# #   mutate(previous = ifelse(nn_tminus1_num == 1, 'younger',
# #                            ifelse(nn_tminus1_num == 2, 'same age', 'older')),
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
# # prevsec2 <- pred_prop %>% filter(nn_tminus1_num == 2)
# # prevsec3 <- pred_prop %>% filter(nn_tminus1_num == 3)
# # pred_prev <- pred_prop %>%
# #   filter(nn_tminus1_num == 1) %>%
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
# #   mutate(nn1_2 = prop_1 - prop_2,
# #          nn1_3 = prop_1 - prop_3,
# #          nn2_3 = prop_2 - prop_3)
# # pred_prev %>%
# #   select(stim_type, after_stim, f_age_num, prediction,
# #          nn1_2,nn1_3,nn2_3) %>%
# #   pivot_longer(cols = c('nn1_2', 'nn1_3', 'nn2_3'),
# #                names_to = 'contrast', values_to = 'value') %>%
# #   mutate(contrast = ifelse(contrast == 'nn1_2', 'younger vs same',
# #                            ifelse(contrast == 'nn1_3', 'younger vs older',
# #                                   'same vs older'))) %>%
# #   ggplot()+
# #   geom_density(aes(x = value, colour = contrast), linewidth = 1)+
# #   scale_colour_viridis_d()+
# #   labs(colour = 't-1 pair',
# #        x = 'difference between neighbours at previous second')+
# #   geom_vline(xintercept = 0, linetype = 2)+
# #   scale_x_continuous(limits = c(-2,2))
# 
# ## time since stimulus
# # load('nearest_neighbour/neighbour_ordinal_prevseccontrasts.RData')
# rm(young_nn, young_mtx, match_nn, match_mtx, older_nn, older_mtx,
#    contrasts, contrasts_long, prevsec_pred,
#    young_vs_match_age1, young_vs_match_age2, young_vs_match_age3,
#    young_vs_older_age1, young_vs_older_age2, young_vs_older_age3,
#    match_vs_older_age1, match_vs_older_age2, match_vs_older_age3) ; gc()
# 
# ## predict with original times
# time_nn_org <- nn_no_na %>% 
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id))))
# time_mtx_org <- posterior_epred(object = nn_fit, newdata = time_nn_org)
# colnames(time_mtx_org) <- time_nn_org$unique_data_combo
# time_mtx_org <- time_mtx_org[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +15 seconds
# time_nn_alt_0.25 <- nn_no_na %>% 
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(after_stim_org = after_stim) %>% 
#   mutate(after_stim = after_stim + 1/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
#   relocate(after_stim_org)
# time_mtx_alt_0.25 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.25)
# colnames(time_mtx_alt_0.25) <- time_nn_alt_0.25$unique_data_combo
# time_mtx_alt_0.25 <- time_mtx_alt_0.25[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +30 seconds
# time_nn_alt_0.50 <- nn_no_na %>% 
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(after_stim_org = after_stim) %>% 
#   mutate(after_stim = after_stim + 1/2,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
#   relocate(after_stim_org)
# time_mtx_alt_0.50 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.50)
# colnames(time_mtx_alt_0.50) <- time_nn_alt_0.50$unique_data_combo
# time_mtx_alt_0.50 <- time_mtx_alt_0.50[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +45 seconds
# time_nn_alt_0.75 <- nn_no_na %>% 
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(after_stim_org = after_stim) %>% 
#   mutate(after_stim = after_stim + 3/4,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
#   relocate(after_stim_org)
# time_mtx_alt_0.75 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_0.75)
# colnames(time_mtx_alt_0.75) <- time_nn_alt_0.75$unique_data_combo
# time_mtx_alt_0.75 <- time_mtx_alt_0.75[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# ## redo predictions with shifted times: +60 seconds
# time_nn_alt_1.00 <- nn_no_na %>% 
#   dplyr::select(f_age_num, stim_type, nn_tminus1_num, after_stim,
#                 focal_id, stim_id, playback_id) %>% 
#   mutate(after_stim_org = after_stim) %>% 
#   mutate(after_stim = after_stim + 1,
#          unique_data_combo = as.integer(as.factor(paste0(f_age_num, nn_tminus1_num, after_stim,focal_id, stim_id, playback_id)))) %>% 
#   relocate(after_stim_org)
# time_mtx_alt_1.00 <- posterior_epred(object = nn_fit, newdata = time_nn_alt_1.00)
# colnames(time_mtx_alt_1.00) <- time_nn_alt_1.00$unique_data_combo
# time_mtx_alt_1.00 <- time_mtx_alt_1.00[c(1:100,1001:1100,2001:2100,3001:3100),,]
# 
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_timecontrasts.RData')
# 
# ## summarise and convert to long format
# time_pred <- time_nn_org %>% 
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
#                names_to = 'time_org_alt_prop_agenn_mu', values_to = 'mean_propn') %>% 
#   pivot_longer(cols = c(time_org_0.00_prop1_sd,time_org_0.00_prop2_sd,time_org_0.00_prop3_sd,
#                         time_alt_0.25_prop1_sd,time_alt_0.25_prop2_sd,time_alt_0.25_prop3_sd,
#                         time_alt_0.50_prop1_sd,time_alt_0.50_prop2_sd,time_alt_0.50_prop3_sd,
#                         time_alt_0.75_prop1_sd,time_alt_0.75_prop2_sd,time_alt_0.75_prop3_sd,
#                         time_alt_1.00_prop1_sd,time_alt_1.00_prop2_sd,time_alt_1.00_prop3_sd),
#                names_to = 'time_org_alt_prop_agenn_sd', values_to = 'stdv_propn') %>% 
#   separate(col = time_org_alt_prop_agenn_mu,
#            into = c('time_mu','org_mu','alt_mu','prop_agenn_mu','mu'),
#            sep = '_', remove = T) %>% 
#   separate(col = time_org_alt_prop_agenn_sd,
#            into = c('time_sd','org_sd','alt_sd','prop_agenn_sd','sd'),
#            sep = '_', remove = T) %>% 
#   select(-time_mu,-org_mu, -time_sd,-org_sd,-mu,-sd) %>% 
#   filter(alt_mu == alt_sd & prop_agenn_sd == prop_agenn_mu) %>% 
#   mutate(nn_pred = ifelse(prop_agenn_mu == 'prop1', 1,
#                           ifelse(prop_agenn_mu == 'prop2', 2,
#                                  ifelse(prop_agenn_mu == 'prop3', 3, 4)))) %>% 
#   select(-alt_sd, -prop_agenn_mu, -prop_agenn_sd) %>% 
#   rename(mins_added = alt_mu) %>% 
#   mutate(pred_type = ifelse(nn_pred == 1, 'younger',
#                             ifelse(nn_pred == 2, 'matched', 'older')))
# 
# ## convert full predictive distribution to long format
# time_pred_all <- time_mtx_org[,,1] %>% 
#   as.data.frame()
# colnames(time_pred_all) <- rownames(time_nn_org)
# time_pred_all <- pivot_longer(time_pred_all, cols = everything(),
#                              names_to = 'rownum', values_to = 'probability')
# time_nn_org$rownum <- rownames(time_nn_org)
# time_pred_all <- time_pred_all %>% 
#   left_join(time_nn_org, by = 'rownum') %>% 
#   mutate(predict_num = 1,
#          predict_cat = 'younger')
# for(i in 2:3){
#   time_pred_new <- time_mtx_org[,,i] %>% 
#     as.data.frame()
#   colnames(time_pred_new) <- rownames(time_nn_org)
#   time_pred_new <- pivot_longer(time_pred_new, cols = everything(),
#                            names_to = 'rownum', values_to = 'probability')
#   time_pred_new <- time_pred_new %>% 
#     left_join(time_nn_org, by = 'rownum') %>% 
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
# contrasts <- nn_no_na %>% 
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
#            sep = '_', remove = T) %>% 
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
#   select(pred_type, f_age_num, contrast, earlier, later, difference, stim_type, after_stim, nn_tminus1_num) %>%
#   mutate(nn_tminus1 = ifelse(nn_tminus1_num == 1,
#                              'neighbour younger at t-1',
#                              ifelse(nn_tminus1_num == 2,
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
#     facet_grid(nn_tminus1 ~ after_stim,
#                scales = 'free')+
#     labs(title = i)
#   print(plot)
# }
# 
# save.image('ele_playbacks/nearest_neighbour/neighbour_ordinal_timecontrasts.RData')
# dev.off()

