# pH Fault Analysis

######  Remove Discrete Features  ######
pH_df[,31:41] %>% sapply(unique)
# To cut: bio_1_phase, bio_2_phase,
#   mbr_1_state, mbr_2_state,
#   mbr_1_mode, mbr_2_mode, mbr_flux_mode,
#   bio_1_temp, bio_2_temp

# Label Matrix
pH_labels_df <- pH_df %>% select(dateTime,
                                 bio_1_phase, bio_2_phase, # Useful
                                 mbr_1_state, mbr_2_state, # Almost always 1
                                 mbr_1_mode, mbr_2_mode, # Usually 1
                                 mbr_flux_mode, # Useful
                                 bio_1_temp, bio_2_temp)
# Some of these state labels were simply recorded incorrectly. Recode them.
pH_labels_df[!is.na(pH_labels_df$bio_1_phase) &
               pH_labels_df$bio_1_phase %in% c(0, 1.5),
          "bio_1_phase"] <- NA
pH_labels_df[!is.na(pH_labels_df$bio_2_phase) &
               pH_labels_df$bio_2_phase %in% c(0, 0.5),
             "bio_2_phase"] <- NA
pH_labels_df %<>% fill(bio_1_phase, bio_2_phase)
# These appear to move in pairs.
sapply(pH_labels_df[,-1], unique)
pH_labels_xts <- xts(pH_labels_df[,-1], order.by = pH_labels_df[,1])

# Continuous Observations
pH_obs_df <- pH_df %>% select(-bio_1_phase, -bio_2_phase,
                              -mbr_1_state, -mbr_2_state,
                              -mbr_1_mode, -mbr_2_mode, -mbr_flux_mode,
                              -bio_1_temp, -bio_2_temp)
pH_obs_xts <- xts(pH_obs_df[,-1], order.by = pH_obs_df[,1])

######  AD-PCA  ######

### Training Set 1 ###
# Train on all obervations up to the 19th:
trainStart <- which(pH_df$dateTime == as.POSIXct("2010-04-19 00:00:00 CDT"))
pH_df_AD_Results_ls <- mspTrain(data = pH_obs_xts[1:trainStart,],
                                           labelVector = rep(1, trainStart),
                                           trainObs = 10080,
                                           updateFreq = 1440,
                                           alpha = 0.001,
                                           faultsToTriggerAlarm = 10,
                                           lagsIncluded = 0:1,
                                           var.amnt = 0.99)

######  MSAD-PCA  ######

### Training Set 1 ###
pH_df_MSAD_Results_ls <- mspTrain(data = pH_obs_xts[1:trainStart,],
                                labelVector = pH_labels_xts[1:trainStart,"bio_1_phase"],
                                trainObs = 10080,
                                updateFreq = 1440,
                                alpha = 0.001,
                                faultsToTriggerAlarm = 10,
                                lagsIncluded = 0:1,
                                var.amnt = 0.99)
