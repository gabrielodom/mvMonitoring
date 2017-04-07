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

###  Blower Operation  ###
# For whatever freaking reason, blower operation was not recorded as a logical
#   in the pH_Fault data set. We need to extract this information from the
#   bio_x_blow_flow variable:

# 1
ggplot(pH_1m_df,
       aes(x = dateTime, y = round(bio_1_blow_flow))) +
  geom_hline(yintercept = 3) +
  geom_point(alpha = 0.1)
# Notice that 0 is a modal value after rounding the variable. Also, histogram:
pH_1m_df$bio_1_blow_flow %>% hist()
# Now we can see a distinct "on/off" switching around every half hour or so
ggplot(pH_1m_df[1:120,],
       aes(x = dateTime, y = round(bio_1_blow_flow))) +
  geom_point(alpha = 1)
# For blower 1, lets cut the observations at 3. There are
(round(pH_1m_df$bio_1_blow_flow) < 3) %>% sum()
# values less than 3, which is just over 46% of the observations. This should
# balance out the blower readings from just turning the fans on and off. We
# will code the logical backwards, so that 2 = on, 1 = off. We need this because
# our package *really* doesn't like labels marked with a numeric 0. It makes
# list subsetting a nightmare.
pH_labels_df <- pH_obs_df %>%
  transmute(bio_1_blower = (bio_1_blow_flow > 2) + 1) %>%
  select(bio_1_blower) %>%
  bind_cols(pH_labels_df, .)

# 2
ggplot(pH_1m_df,
       aes(x = dateTime, y = round(bio_2_blow_flow))) +
  geom_hline(yintercept = 4) +
  geom_point(alpha = 0.1)
pH_1m_df$bio_2_blow_flow %>% hist()
ggplot(pH_1m_df[1:120,],
       aes(x = dateTime, y = round(bio_2_blow_flow))) +
  geom_point(alpha = 1)
(round(pH_1m_df$bio_2_blow_flow) < 4) %>% sum()
# Once again, code the logical backwards so that 2 = on, 1 = off
pH_labels_df <- pH_obs_df %>%
  transmute(bio_2_blower = (bio_2_blow_flow > 3) + 1) %>%
  select(bio_2_blower) %>%
  bind_cols(pH_labels_df, .)

# Blower indicator
pH_labels_df %<>% mutate(bio_blower = (bio_1_blower + 2) * bio_2_blower)

# # We need to test the proportionality assignment in the mspTrain() function.
# classes <- unique(pH_labels_df$bio_blower)
# sapply(classes, function(x){
#   sum(pH_labels_df$bio_blower == x) / nrow(pH_labels_df)
# })
# sapply(1, function(x){
#   sum(matrix(rep(1,1000), ncol = 1, nrow = 1000) == x) /
#     nrow(matrix(rep(1,1000), ncol = 1, nrow = 1000))
# })
# # It works.
# Code:
#   3 * 1 = 3: blower1 off, blower2 off; 10369 / 38147 = 27%
(pH_labels_df$bio_blower == 3) %>% sum
#   3 * 2 = 6: blower1 off, blower2 on; 7191 / 38147 = 19%
(pH_labels_df$bio_blower == 6) %>% sum
#   4 * 1 = 4: blower1 on, blower2 off; 7121 / 38147 = 19%
(pH_labels_df$bio_blower == 4) %>% sum
#   4 * 2 = 8: blower1 on, blower2 on; 13466 / 38147 = 35%
(pH_labels_df$bio_blower == 8) %>% sum


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
dfEnd <- nrow(pH_obs_xts)
pH_df_AD_Results_ls <- mspTrain(data = pH_obs_xts[1:trainStart,],
                                           labelVector = rep(1, trainStart),
                                           trainObs = 10080,
                                           updateFreq = 1440,
                                           alpha = 0.001,
                                           faultsToTriggerAlarm = 10,
                                           lagsIncluded = 0:1,
                                           var.amnt = 0.90)

######  MSAD-PCA  ######

### Training Set 1 ###
pH_df_MSAD_Results_ls <- mspTrain(data = pH_obs_xts[1:trainStart,],
                                labelVector = pH_labels_xts[1:trainStart,"bio_blower"],
                                trainObs = 10080,
                                updateFreq = 1440,
                                alpha = 0.001,
                                faultsToTriggerAlarm = 10,
                                lagsIncluded = 0:1,
                                var.amnt = 0.90)
