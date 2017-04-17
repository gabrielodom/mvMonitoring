# Real Data Analysis

library(magrittr)
library(scales)
library(tidyverse)
library(xts)
library(mvMonitoring)

######  False Alarm Rates  ######

# Read in the NOC data from before the pH fault
NOC_1min <- read_csv("/Users/gabriel_odom/Box Sync/Consulting/Dr. Hering/MV_Process_Control/Kazor Code/NOC_1min_clean_Interpolated_Inner.txt")
NOC_1min %>% str

# xts Matrix
NOC_xts <- xts(NOC_1min[,-1], order.by = NOC_1min[[1]])
NOC_xts %>% head(1) # 13 June at 7:10AM
NOC_xts %>% tail(1) # 2 September at 6:00AM

# Check for near-constant columns and NAs
sapply(1:ncol(NOC_xts), function(i){
  length(unique(NOC_xts[,i])) < 20
})

# Potential low-variance offenders
NOC_xts[,2] %>% unique() # Bio1 Phase Switching
NOC_xts[,3] %>% unique() # Bio2 Phase Switching
NOC_xts[,5] %>% unique() # MBR1 State (usually in 1)
NOC_xts[,8] %>% unique() # MBR2 State (usually in 1)
NOC_xts[,15] %>% unique() # MBR1 Flux Mode (usually in 0)
NOC_xts[,16] %>% unique() # MBR1 Mode Switching
NOC_xts[,18] %>% unique() # MBR2 Mode Switching
NOC_xts[,20] %>% unique() # Blower1 Switching
NOC_xts[,21] %>% unique() # Blower2 Switching
NOC_xts[,22] %>% unique() # Either Blower Switching

###  BioX Blowers  ###
# There are three rows of Blower columns with values equal to 1 + 1/3. Get rid
# of these. Also, recode blower_x as an actual blower state vector. Code
# bio_x_blower as 0 = neither blower on, 1 = blower 1 on, 2 = blower 2 on, and
# 3 = both blowers on.Because there are a few minutes where both blowers are on,
# roll anything in 3 into a 2 (blower 2 on) or a 1 (blower 1 on) at random. This
# only happens in 246 of the over 116k rows, so we aren't losing much.
which(NOC_xts[,22] == 1 + 1/3)
NOC_xts[,20] <- round(NOC_xts[,20])
NOC_xts[,21] <- round(NOC_xts[,21])
NOC_xts[,22] <- (NOC_xts[,20] - 1) + 2 * (NOC_xts[,21] - 1)
NOC_xts[NOC_xts[,22] > 2,22] <- round(runif(246)) + 1

### BioX Phases  ###
# The features bio_1_phase and bio_2_phase alternate between 1, 2, and 3. There
# is 1 record (row 9496) that has a 0 recorded for both. We'll turn this into a
# 3 for bio_1_phase and a 2 for bio_2_phase (the last recorded values).
NOC_xts[9496,2] <- 3
NOC_xts[9496,3] <- 2

###  Remove NAs  ###
nrow(NOC_xts) # 116570
NOC_xts <- na.omit(NOC_xts)
nrow(NOC_xts) # 111336
# We lost 5,234 observations

###  Split the Label Columns  ###
# Split off the Switching Indicators, and remove columns 5, 8, and 15
NOC_xts_labels <- NOC_xts[,c(2, 3, 16, 18, 20, 21, 22)]
NOC_xts <- NOC_xts[,-c(2, 3, 5, 8, 15, 16, 18, 20, 21, 22)]
# Did we get all the problem columns?
sapply(1:ncol(NOC_xts), function(i){
  length(unique(NOC_xts[,i])) < 20
})
# Looks like
rm(NOC_1min)

###  Save the Clean Data  ###
# setwd("/Users/gabriel_odom/Box Sync/Consulting/Dr. Hering/MV_Process_Control/Kazor Code")
# write_csv(NOC_xts, "NOC_Continuous_Clean_20170329.csv")
# write_csv(NOC_xts_labels, "NOC_Continuous_Clean_Labels_20170329.csv")


######  Implement the mspTrain Function  ######
# My memory can't hold all 111k observations, so I want to run this on the
# last 60 days. Kazor et al used the first 40 days, so do that too.

###  True Multi-state Observations  ###
trainMins <- 1:(60 * 24 * 20)
Sys.time()
NOC_MS_blower_ls <- mspTrain(data = NOC_xts[trainMins,],
                          # Split on the blowers
                          labelVector = NOC_xts_labels[trainMins,7] + 1,
                          trainObs = 10080,
                          updateFreq = 4320,
                          alpha = 0.001,
                          faultsToTriggerAlarm = 5,
                          lagsIncluded = 0:1,
                          var.amnt = 0.90)
Sys.time() # 14.5 minutes for the first 40 days, 46.5 minutes for the last 60
# days, but we increased phyisical memory use from 18% to 80%. We don't have
# the memory for the entire data set.

setwd("~/GitHub/mvMonitoring/mvMonitoring/data")
saveRDS(NOC_MS_blower_ls, file = "NOC_Results_byBlower2_20170329.rds")
NOC_MS_blower_ls <- readRDS("NOC_Results_byBlower2_20170329.rds")

# False Alarm rate MS
nrow(NOC_MS_blower_ls$FaultChecks)
# T2
length(which(NOC_MS_blower_ls$Alarms[,85] %in% c(1,3))) / 45721
# SPE
length(which(NOC_MS_blower_ls$Alarms[,85] %in% c(2,3))) / 45721

###  No Multi-state Observations  ###
Sys.time()
NOC_SS_blower_ls <- mspTrain(data = NOC_xts[trainMins,],
                          # Split on the blowers
                          labelVector = rep(1, length(trainMins)),
                          trainObs = 10080,
                          updateFreq = 4320,
                          alpha = 0.001,
                          faultsToTriggerAlarm = 5,
                          lagsIncluded = 0:1,
                          var.amnt = 0.90)
trainMins2 <- (60 * 24 * 20 + 1 - 10080):(60 * 24 * 40)
NOC_SS_blower_ls2 <- mspTrain(data = NOC_xts[trainMins2,],
                             # Split on the blowers
                             labelVector = rep(1, length(trainMins2)),
                             trainObs = 10080,
                             updateFreq = 4320,
                             alpha = 0.001,
                             faultsToTriggerAlarm = 5,
                             lagsIncluded = 0:1,
                             var.amnt = 0.90)
Sys.time() # 14.5 minutes for the first 40 days, 56 minutes for the last 60
# days, but we increased phyisical memory use from 18% to 80%. We don't have
# the memory for the entire data set.

setwd("~/GitHub/mvMonitoring/mvMonitoring/data")
saveRDS(NOC_SS_blower_ls, file = "NOC_SS_Results_byBlower2_20170330.rds")
NOC_SS_blower_ls <- readRDS("NOC_SS_Results_byBlower2_20170330.rds")

# False Alarm rate sS
nrow(NOC_SS_blower_ls$FaultChecks) + nrow(NOC_SS_blower_ls2$FaultChecks)
# T2
(length(which(NOC_SS_blower_ls$Alarms[,85] %in% c(1,3))) +
    length(which(NOC_SS_blower_ls2$Alarms[,85] %in% c(1,3)))) / 47518
# SPE
(length(which(NOC_SS_blower_ls$Alarms[,85] %in% c(2,3))) +
    length(which(NOC_SS_blower_ls2$Alarms[,85] %in% c(2,3)))) / 47518
