# This file exists to generation the simulation tabulated results for detection
# times and the histograms for false alarm rates

######  Multi-State Simulation  ###############################################

# Read in the latest simulation data
library(xts)
library(readr)
library(reshape2)
library(tidyverse)
detection_times2 <- read_csv("~/Box Sync/Consulting/Dr. Hering/MV_Process_Control/MVSPC/Simulation_Data/detection_times_1000_20170314.csv")

# Table out summary of detection times
mspSummary <- function(vec,
                       quants = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1),
                       oobCode = Inf){
  # Take in a vector, find the proportion out of bounds (NA, NaN, or Inf), and
  # calculate quantile statistics (min, 0.05, 0.25, 0.5, mean, 0.75, 0.95, max)
  # and spread statistics (IQR, standard deviation). Return this info as a named
  # vector

  vec <- vec %>% as.matrix() %>% as.vector()

  ###  Out of Bounds Observations  ###
  # subset out the out of bounds observations
  vecClean <- vec[!(vec %in% oobCode)]
  oobProp <- 1 - length(vecClean) / length(vec)

  ###  Summary Statistics  ###
  # Make sure the quantiles necessary for the IQR are included
  quantsFull <- union(quants, c(0.25, 0.75))
  # Calculate summaries
  vecQuants <- quantile(vecClean, probs = quantsFull)
  vecIQR <- vecQuants["75%"] - vecQuants["25%"]
  names(vecIQR) <- NULL
  vecMean <- mean(vecClean)
  vecSD <- sd(vecClean)

  ###  Return  ###
  returnVec <- c(vecQuants,
                 "IQR" = vecIQR,
                 "Mean" = vecMean,
                 "StdDev" = vecSD,
                 "Prop_OOB" = oobProp)
}


# Fault 1A
detection_times2 %>%
  filter(Faults == "A1") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 1B
detection_times2 %>%
  filter(Faults == "B1") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 2A
detection_times2 %>%
  filter(Faults == "A2") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 1B
detection_times2 %>%
  filter(Faults == "B2") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 3A
detection_times2 %>%
  filter(Faults == "A3") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 3B
detection_times2 %>%
  filter(Faults == "B3") %>%
  select(-Faults) %>%
  sapply(mspSummary)


###  False Alarm Rates  ###
FA_rates2 <- read_csv("~/Box Sync/Consulting/Dr. Hering/MV_Process_Control/MVSPC/Simulation_Data/false_alarm_rates_1000_20170314.csv")
FA_rates2$MSAD_SPE %>%
  hist(xlim = c(0, 0.06), main = "MSAD SPE False Alarm Rates")
FA_rates2$AD_T2 %>%
  hist(xlim = c(0, 0.06), main = "AD T2 False Alarm Rates")

######  Single-State Simulation  ##############################################
###  Detection Times  ###
# Read Detection Times Data

# Fault 1A
ss_detectionTimes %>%
  filter(Faults == "A1") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 1B
ss_detectionTimes %>%
  filter(Faults == "B1") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 2A
ss_detectionTimes %>%
  filter(Faults == "A2") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 2B
ss_detectionTimes %>%
  filter(Faults == "B2") %>%
  select(-Faults) %>%
  sapply(mspSummary)

# Fault 3A
ss_detectionTimes %>%
  filter(Faults == "A3") %>%
  select(-Faults) %>%
  sapply(mspSummary) %>% round(3)

# Fault 3B
ss_detectionTimes %>%
  filter(Faults == "B3") %>%
  select(-Faults) %>%
  sapply(mspSummary)

###  False Alarm Rates  ###
# Read in False Alarm Rates Data
ss_FA_rates[,1] %>% hist(xlim = c(0, 0.03), main = "MSAD SPE False Alarm Rates")
ss_FA_rates[,3] %>% hist(xlim = c(0, 0.03), main = "AD SPE False Alarm Rates")
ss_FA_rates[,2] %>% hist(xlim = c(0.05, 0.2), main = "MSAD T2 False Alarm Rates")
ss_FA_rates[,4] %>% hist(xlim = c(0.05, 0.2), main = "AD T2 False Alarm Rates")
