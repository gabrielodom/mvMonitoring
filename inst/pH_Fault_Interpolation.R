

library(fitdistrplus) # For bio_x_tss
library(magrittr)
library(scales)
library(plyr)
library(tidyverse)
library(xts)
library(mvMonitoring)
library(pbapply)

######  Load the Data  ######
load("data/pH_fault_data.ave")
D.Ave.q %>% str
D.Ave.q %>% head(1) # Starts 10 April, 2010 at 01:10
D.Ave.q %>% tail(1) # Ends 11 May, 2010 at 01:00
load("/Users/gabriel_odom/Box Sync/Consulting/Dr. Hering/MV_Process_Control/Kazor Code/Raw_Data from Baylor Box/raw_1min_pH_data")
d.1m %>% str
d.1m %>% head(1) # Starts 10 April, 2010 at 00:00
d.1m %>% tail(1) # Ends 11 May, 2010 at 00:00


######  Data Cleaning  #######
pH_10m_df <- data.frame(dateTime = index(D.Ave.q), D.Ave.q)
row.names(pH_10m_df) <- NULL
pH_1m_df <- d.1m
pH_1m_df %<>% tibble::rownames_to_column(var = "dateTime")
pH_1m_df %<>% mutate(dateTime = as.POSIXct(dateTime))

rm(d.1m, T, D.Ave.q)

# NaNs / NAs
# Check for NaNs (they have to be handled differently than NAs)
sapply(1:37, function(i){  # A bunch of NaNs
  sum(is.nan(pH_10m_df[,i]))
})
# Columns 11, 12, and 23 have been seriously degraded with NAs
pH_10m_df[,c(11, 12, 23)] %>% head
# mbr_x_air_pres are missing over half their observations, and mbr_air_scour
# pressure is missing over a third.
sapply(1:24, function(i){  # No NaNs
  sum(is.nan(unique(pH_1m_df[,i])))
})
# All columns (other than dateTime) have NaNs, so we replace them with NAs.
pH_10m_df[,2:ncol(pH_10m_df)] <- rapply(pH_10m_df[,2:37], function(x){
  ifelse(is.nan(x), NA, x)
},
how = "replace")

# Now NAs.
sapply(1:37, function(i){  # How many NAs?
  sum(is.na(pH_10m_df[,i]))
})

sapply(1:24, function(i){
  anyNA(pH_1m_df[,i])
})
# We have NAs in the last two columns of the 1-minute data. How many?
names(pH_1m_df[23:24])
sapply(23:24, function(i){
  sum(is.na(pH_1m_df[,i]))
})
# We don't even have these features in the NOC data. Cut them.
pH_1m_df[23:24] <- NULL
# In the 10-minute data, cut the offending columns
pH_10m_df[,c(11, 12, 23)] <- NULL


######  Graphs  #######

# Graphs
# We have to plot the features first, then determine which kind of variance
# structure to apply to the jittering errors.
obsSeq <- seq(from = 1, to = nrow(pH_1m_df), by = 10)
# Add vertical detection times
adDetect_time <- as.POSIXct("2010-04-21 13:40:00 CDT")
humanDetect_time <- as.POSIXct("2010-04-24 10:00:00 CDT")
pH_names <- sapply(names(pH_1m_df), as.name)

for(i in 2:length(pH_names)){
  xx <- ggplot(data = pH_1m_df[obsSeq,],
               aes(x = dateTime, y = eval(pH_names[[i]]))) +
    scale_y_continuous(name = eval(quote(pH_names[[i]]))) +
    scale_x_datetime(labels = date_format("%m-%d"),
                     minor_breaks = date_breaks("1 day")) +
    geom_point() +
    geom_vline(aes(xintercept = as.numeric(adDetect_time)),
               linetype = 4, colour = "blue", size = 1) +
    geom_vline(aes(xintercept = as.numeric(humanDetect_time)),
               linetype = 4, colour = "red", size = 1)
  print(xx)
}

pH_10m_names <- sapply(names(pH_10m_df), as.name)

for(i in 2:length(pH_10m_names)){
  xx <- ggplot(data = pH_10m_df,
               aes(x = dateTime, y = eval(pH_10m_names[[i]]))) +
    scale_y_continuous(name = eval(quote(pH_10m_names[[i]]))) +
    scale_x_datetime(labels = date_format("%m-%d"),
                     minor_breaks = date_breaks("1 day")) +
    geom_point() +
    geom_vline(aes(xintercept = as.numeric(adDetect_time)),
               linetype = 4, colour = "blue", size = 1) +
    geom_vline(aes(xintercept = as.numeric(humanDetect_time)),
               linetype = 4, colour = "red", size = 1)
  print(xx)
}


######  Initialise Data Frames  ######
MADs_df <- pH_10m_df %>% select(dateTime) # intialise MAD as a data.frame
pH_1min_Seq <- seq.POSIXt(from = pH_10m_df[1,1],
                          to = pH_10m_df[nrow(pH_10m_df),1],
                          by = "min")
pH_1min_Seq <- data.frame(dateTime = pH_1min_Seq)


######  Feature Error Variance Calculation  ######
# NOTE: features which end in "_1min" are the actual interpolated features.
#   features which end in "_jitter" are the actual errors for those features.

# ANOTHER NOTE: GOD DANG IT GABRIEL - you are can act like a real idiot some
#   times. Why didn't you check to see which columns were duplicated in the 10
#   minute data first, so that you wouldn't have to linearly interpolate them?
#   Bloody hell. - Gabriel, on 5th April.

### mbr_x_perm_flow
# mbr_x_perm_flow - strict interpolation
ggplot(data = pH_10m_df, aes(x = dateTime, y = mbr_2_perm_flow)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()

MADs_df$mbr_2_perm_flow <- MADs_df$mbr_1_perm_flow <- 0

### mbr_x_perm_pres
# mbr_x_perm_pres - jittered interpolation; the signal thickness is about 0.25
#   units, so train a constant error variance to match
ggplot(data = pH_10m_df, aes(x = dateTime, y = mbr_1_perm_pres)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
# 1
noNA <- pH_10m_df %>%
  select(dateTime, mbr_1_perm_pres) %>%
  na.omit
mod <- smooth.spline(noNA$mbr_1_perm_pres ~ noNA$dateTime)
mod %>% residuals() %>% plot
mod %>% residuals() %>% mad()
plot(1:4300, rnorm(4300, sd = 0.19 / sqrt(2 / pi)), ylim = c(-2,1.5))

MADs_df$mbr_1_perm_pres <- mod %>% residuals() %>% mad() / sqrt(2 / pi)

# 2
noNA <- pH_10m_df %>%
  select(dateTime, mbr_2_perm_pres) %>%
  na.omit
mod <- smooth.spline(noNA$mbr_2_perm_pres ~ noNA$dateTime)
mod %>% residuals() %>% plot
mod %>% residuals() %>% mad()
plot(1:4300, rnorm(4300, sd = 0.22 / sqrt(2 / pi)), ylim = c(-3,2))

MADs_df$mbr_2_perm_pres <- mod %>% residuals() %>% mad() / sqrt(2 / pi)


### mbr_x_air_flow
# mbr_x_air_flow - after looking only at the values between 5 and 10, the
#   outliers do not mask a strong underlying structure. Leave them.
ggplot(data = pH_10m_df, aes(x = dateTime, y = mbr_2_air_flow)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()

MADs_df$mbr_2_air_flow <- MADs_df$mbr_1_air_flow <- pH_10m_df$mbr_1_air_flow %>%
  mad(na.rm = TRUE) / sqrt(2 / pi)


### mbr_x_tmp
# mbr_x_tmp - jittered interpolation; the signal thickness is about 0.25
#   units, so train a constant error variance to match (sd = 0.5)
ggplot(data = pH_10m_df, aes(x = dateTime, y = mbr_1_tmp)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
# 1
noNA <- pH_10m_df %>%
  select(dateTime, mbr_1_tmp) %>%
  na.omit
mod <- smooth.spline(noNA$mbr_1_tmp ~ noNA$dateTime)
mod %>% residuals() %>% plot
mod %>% residuals() %>% mad() # This overestimates the variance a bit, try
# cutting this in half
plot(1:4300, rnorm(4300, sd = 0.18 / (2 * sqrt(2 / pi))), ylim = c(-1.5,2))

MADs_df$mbr_1_tmp <- mod %>% residuals() %>% mad() / (2 * sqrt(2 / pi))

# 2
noNA <- pH_10m_df %>%
  select(dateTime, mbr_2_tmp) %>%
  na.omit
mod <- smooth.spline(noNA$mbr_2_tmp ~ noNA$dateTime)
mod %>% residuals() %>% plot
mod %>% residuals() %>% mad()# This overestimates the variance a bit, try
# cutting this in half
plot(1:4300, rnorm(4300, sd = 0.202 / (2 * sqrt(2 / pi))), ylim = c(-2,3))

MADs_df$mbr_2_tmp <- mod %>% residuals() %>% mad() / (2 * sqrt(2 / pi))


### ras_temp
# ras_temp - LOCF
ggplot(data = pH_10m_df, aes(x = dateTime, y = ras_temp)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
oneMin_ls <- list(ras_temp = pH_10m_df %>%
                    select(dateTime, ras_temp) %>%
                    left_join(pH_1min_Seq, .) %>%
                    fill(ras_temp, .direction = "down") %>%
                    select(ras_temp))
# the fill() function is apparently smart enough to recognise the original NAs

MADs_df$ras_temp <- 0


### bio_x_do
# bio_x_do - *very* strong signal for values < 0.25 (1) or 0.5 (2). If the
#   dissolved O2 measure is less than 0.25, perform strict linear interpolation?
#   No, LOCF (see bio_x_blow_flow), otherwise add some error with constant
#   sd = 0.1
ggplot(data = pH_10m_df, aes(x = dateTime, y = bio_2_do)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_hline(yintercept = 0.5) +
  geom_point()
# 1
bio_1_do_1min <- pH_10m_df %>%
  select(dateTime, bio_1_do) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(bio_1_do, .direction = "down") %>%
  select(bio_1_do)
# Now add noise for values greater than 0.25. The observations greater than
# 0.25 could easily follow a uniform distribution
bio_1_do_sd <- pH_10m_df %>%
  filter(bio_1_do > 0.25) %>%
  select(bio_1_do) %>%
  as.matrix() %>%
  mad / sqrt(2 / pi)
MADs_df$bio_1_do <- (pH_10m_df %>%
                       select(bio_1_do) %>%
  mutate(bio_1_do = as.numeric(!is.na(bio_1_do) & bio_1_do > 0.25) *
           bio_1_do_sd / 20))[[1]]
bio_1_do_sd_df <- MADs_df %>%
  select(dateTime, bio_1_do) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(bio_1_do, .direction = "down")

bio_1_do_1min$bio_1_do <- sapply(1:nrow(bio_1_do_1min), function(i){
  bio_1_do_1min[i,1] + rnorm(1, sd = bio_1_do_sd_df[i,2])
})

oneMin_ls$bio_1_do <- bio_1_do_1min
rm(bio_1_do_1min, bio_1_do_sd_df, bio_1_do_sd)

ggplot(data = oneMin_ls$bio_1_do,
       aes(x = dateTime, y = bio_1_do)) +
  geom_point(alpha = 0.1)

# 2
bio_2_do_1min <- pH_10m_df %>%
  select(dateTime, bio_2_do) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(bio_2_do, .direction = "down") %>%
  select(bio_2_do)
# Now add noise for values greater than 0.5. The observations greater than
# 0.5 could easily follow a uniform distribution
bio_2_do_sd <- pH_10m_df %>%
  filter(bio_2_do > 0.5) %>%
  select(bio_2_do) %>%
  as.matrix() %>%
  mad / sqrt(2 / pi)
MADs_df$bio_2_do <- (pH_10m_df %>%
                       select(bio_2_do) %>%
  mutate(bio_2_do = as.numeric(!is.na(bio_2_do) & bio_2_do > 0.5) *
           (bio_2_do_sd / 20)))[[1]]
bio_2_do_sd_df <- MADs_df %>%
  select(dateTime, bio_2_do) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(bio_2_do, .direction = "down")

bio_2_do_1min$bio_2_do <- sapply(1:nrow(bio_2_do_1min), function(i){
  bio_2_do_1min[i,1] + rnorm(1, sd = bio_2_do_sd_df[i,2])
})
oneMin_ls$bio_2_do <- bio_2_do_1min
rm(bio_2_do_1min, bio_2_do_sd_df, bio_2_do_sd)

ggplot(data = oneMin_ls$bio_2_do,
       aes(x = dateTime, y = bio_2_do)) +
  geom_point(alpha = 0.1)


### mbr_x_inf_flow
# mbr_x_inf_flow - jittered interpolation; fit a cubic spline, and calculate an
#   error variance from the spline residuals
ggplot(data = pH_10m_df, aes(x = dateTime, y = mbr_2_inf_flow)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
# 1
noNA <- pH_10m_df %>%
  select(dateTime, mbr_1_inf_flow) %>%
  na.omit
mod <- smooth.spline(noNA$mbr_1_inf_flow ~ noNA$dateTime)
mod %>% residuals() %>% plot
mod %>% residuals() %>% mad()
plot(1:4300, rnorm(4300, sd = 0.25 / sqrt(2 / pi)), ylim = c(-10,5))

MADs_df$mbr_1_inf_flow <- mod %>% residuals() %>% mad() / sqrt(2 / pi)

# 2
noNA <- pH_10m_df %>%
  select(dateTime, mbr_2_inf_flow) %>%
  na.omit
mod <- smooth.spline(noNA$mbr_2_inf_flow ~ noNA$dateTime)
mod %>% residuals() %>% plot
mod %>% residuals() %>% mad()
plot(1:4300, rnorm(4300, sd = 0.38 / sqrt(2 / pi)), ylim = c(-10,7))

MADs_df$mbr_2_inf_flow <- mod %>% residuals() %>% mad() / sqrt(2 / pi)


### mbr_x_level
# mbr_x_level - remove values less than 9.7 (except for the fault between the
#   21st and the 23rd). This will show the medium-strength signal. Notice that
#   this feature also shows evidence of a multistate process, so use LOCF
ggplot(data = pH_10m_df, aes(x = dateTime, y = mbr_2_level)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  # scale_y_continuous(limits = c(9.7, 10.1)) +
  geom_point()
# 1
naIndex1 <- which(pH_10m_df$mbr_1_level < 9.7 &
                    (pH_10m_df$dateTime < as.POSIXct("2010-04-22 14:50:00 CDT") |
                       pH_10m_df$dateTime > as.POSIXct("2010-04-22 18:10:00 CDT")))
pH_10m_df[naIndex1, "mbr_1_level"] <- NA
oneMin_ls$mbr_1_level <- pH_10m_df %>%
  select(dateTime, mbr_1_level) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(mbr_1_level, .direction = "down") %>%
  select(mbr_1_level)
rm(naIndex1)

# 2
naIndex2 <- which(pH_10m_df$mbr_2_level < 9.25 &
                    (pH_10m_df$dateTime < as.POSIXct("2010-04-22 00:00:00 CDT") |
                       pH_10m_df$dateTime > as.POSIXct("2010-04-22 18:10:00 CDT")))
pH_10m_df[naIndex2, "mbr_2_level"] <- NA
oneMin_ls$mbr_2_level <- pH_10m_df %>%
  select(dateTime, mbr_2_level) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(mbr_2_level, .direction = "down") %>%
  select(mbr_2_level)
rm(naIndex2)

MADs_df$mbr_2_level <- MADs_df$mbr_1_level <- 0


### perm_turb
# perm_turb - jittered interpolation; fit a cubic spline, and calculate an
#   error variance from the spline residuals
ggplot(data = pH_10m_df, aes(x = dateTime, y = perm_turb)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
noNA <- pH_10m_df %>%
  select(dateTime, perm_turb) %>%
  na.omit
mod <- smooth.spline(noNA$perm_turb ~ noNA$dateTime)
mod %>% residuals() %>% plot
mod %>% residuals() %>% mad()
plot(1:4300, rnorm(4300, sd = 0.006 / sqrt(2 / pi)), ylim = c(-0.1,0.6))

MADs_df$perm_turb <- mod %>% residuals() %>% mad() / sqrt(2 / pi)


### sewage_flow
# sewage_flow - LOCF (due to the multi-state structure). Interpolation will
#   distort the underlying multi-state cycles
ggplot(data = pH_10m_df[1:1440,], aes(x = dateTime, y = sewage_flow)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
oneMin_ls$sewage_flow <- pH_10m_df %>%
  select(dateTime, sewage_flow) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(sewage_flow, .direction = "down") %>%
  select(sewage_flow)

MADs_df$sewage_flow <- 0


### bio_x_blow_flow
# bio_x_blow_flow - because of the strict cyclical state membership, LOCF is
#   more appropriate than interpolation. This is curious, because I don't know
#   what is driving the state membership. This may also imply that dissolved O2
#   has a similar underlying effect.
ggplot(data = pH_10m_df[1:144,], aes(x = dateTime, y = bio_2_blow_flow)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
oneMin_ls$bio_1_blow_flow <- pH_10m_df %>%
  select(dateTime, bio_1_blow_flow) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(bio_1_blow_flow, .direction = "down") %>%
  select(bio_1_blow_flow)
oneMin_ls$bio_2_blow_flow <- pH_10m_df %>%
  select(dateTime, bio_2_blow_flow) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(bio_2_blow_flow, .direction = "down") %>%
  select(bio_2_blow_flow)

MADs_df$bio_2_blow_flow <- MADs_df$bio_1_blow_flow <- 0


### sewage_level
# sewage_level - jittered interpolation; fit a cubic spline, and calculate an
#   error variance from the spline residuals
ggplot(data = pH_10m_df, aes(x = dateTime, y = sewage_level)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
noNA <- pH_10m_df %>%
  select(dateTime, sewage_level) %>%
  na.omit
mod <- smooth.spline(noNA$sewage_level ~ noNA$dateTime)
mod %>% residuals() %>% plot
mod %>% residuals() %>% mad()
plot(1:4300, rnorm(4300, sd = 0.123 / sqrt(2 / pi)), ylim = c(-2,2))

MADs_df$sewage_level <- mod %>% residuals() %>% mad() / sqrt(2 / pi)


### bio_x_level
# bio_x_level - remove values less than 4. This will show a strong but short
#   signal. Interpolate linearly and calculate an error variance from the spline
#   residuals. NOTE: splines won't work - the signal period is too short. Use
#   the "all.knots = TRUE" argument pick out the signal.
ggplot(data = pH_10m_df[1:288,], aes(x = dateTime, y = bio_2_level)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
# 1
pH_10m_df[!is.na(pH_10m_df$bio_1_level) & pH_10m_df$bio_1_level < 4, "bio_1_level"] <- NA
noNA <- pH_10m_df %>%
  select(dateTime, bio_1_level) %>%
  na.omit
mod <- smooth.spline(noNA$bio_1_level ~ noNA$dateTime, all.knots = TRUE)
mod %>% residuals() %>% plot()
mod %>% residuals() %>% mad()
plot(1:4300, rnorm(4300, sd = 0.035 / sqrt(2 / pi)), ylim = c(-0.2,0.2))

MADs_df$bio_1_level <- mod %>% residuals() %>% mad() / sqrt(2 / pi)

# 2
pH_10m_df[!is.na(pH_10m_df$bio_2_level) & pH_10m_df$bio_2_level < 4, "bio_2_level"] <- NA
noNA <- pH_10m_df %>%
  select(dateTime, bio_2_level) %>%
  na.omit
mod <- smooth.spline(noNA$bio_2_level ~ noNA$dateTime, all.knots = TRUE)
mod %>% residuals() %>% plot()
mod %>% residuals() %>% mad()
plot(1:4300, rnorm(4300, sd = 0.024 / sqrt(2 / pi)), ylim = c(-0.15,0.15))

MADs_df$bio_2_level <- mod %>% residuals() %>% mad() / sqrt(2 / pi)


### bio_x_temp
# bio_x_temp - truncate all values < 20, then LOCF; In 2010, Golden had record
#   low temperatures in the first week of May, so the 20F temperatures are real.
#   EDIT: after speaking with Dr. Hering, we can remove the values less than 58F,
#   even though they're real.
ggplot(data = pH_10m_df, aes(x = dateTime, y = bio_2_temp)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
pH_10m_df[!is.na(pH_10m_df$bio_1_temp) & pH_10m_df$bio_1_temp < 58, "bio_1_temp"] <- NA
oneMin_ls$bio_1_temp <- pH_10m_df %>%
  select(dateTime, bio_1_temp) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(bio_1_temp, .direction = "down") %>%
  select(bio_1_temp)
pH_10m_df[!is.na(pH_10m_df$bio_2_temp) & pH_10m_df$bio_2_temp < 58, "bio_2_temp"] <- NA
oneMin_ls$bio_2_temp <- pH_10m_df %>%
  select(dateTime, bio_2_temp) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(bio_2_temp, .direction = "down") %>%
  select(bio_2_temp)

MADs_df$bio_2_temp <- MADs_df$bio_1_temp <- 0


### bio_x_tss
# bio_x_tss - long signal; truncate all values < 3500, log transform, fit
#   a spline, calculate exponential (?) residuals
ggplot(data = pH_10m_df[1:930,], aes(x = dateTime, y = log(bio_2_tss))) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  # geom_hline(yintercept = 3500) +
  geom_point()
# 1
pH_10m_df[!is.na(pH_10m_df$bio_1_tss) & pH_10m_df$bio_1_tss < 3500, "bio_1_tss"] <- NA
pH_10m_df$bio_1_tss_log <- pH_10m_df$bio_1_tss %>% log
noNA <- pH_10m_df %>%
  select(dateTime, bio_1_tss_log) %>%
  na.omit
mod <- smooth.spline(noNA$bio_1_tss_log ~ noNA$dateTime)
bio_1_tssResid <- mod %>% residuals()
descdist(bio_1_tssResid)
fitdist(bio_1_tssResid + 1,
        "weibull", method = "mle", lower = c(0.001, 0.001),
        start = list(scale = 1, shape = 1))
# Neither the Weibull nor the Log-Normal match the distribution of the residuals
bio_1_tssResid %>% plot()
# After a two hours of trial and error, I think this mixture distribution fits
# the residuals very well.
plot(1:4300,
     (rnorm(4300, sd = 2.6 *  0.078 / sqrt(2 / pi))) ^ 2 + rnorm(4300, sd = 0.04),
     ylim = c(-0.2,1))
n1min <- nrow(pH_1min_Seq)

bio_1_tss_log_1min_jitter <- (rnorm(n1min,
                                    sd = 2.6 *  0.078 / sqrt(2 / pi))) ^ 2 +
  rnorm(n1min, sd = 0.04)

# 2
pH_10m_df[!is.na(pH_10m_df$bio_2_tss) & pH_10m_df$bio_2_tss < 3500, "bio_2_tss"] <- NA
pH_10m_df$bio_2_tss_log <- pH_10m_df$bio_2_tss %>% log
noNA <- pH_10m_df %>%
  select(dateTime, bio_2_tss_log) %>%
  na.omit
mod <- smooth.spline(noNA$bio_2_tss_log ~ noNA$dateTime)
bio_2_tssResid <- mod %>% residuals()
bio_2_tssResid %>% plot()
plot(1:4300,
     (rnorm(4300, sd = 2.5 *  0.078 / sqrt(2 / pi))) ^ 2 +
       rnorm(4300, sd = 0.035),
     ylim = c(-0.4,0.9))

bio_2_tss_log_1min_jitter <- (rnorm(n1min,
                                    sd = 2.5 *  0.078 / sqrt(2 / pi))) ^ 2 +
  rnorm(n1min, sd = 0.035)

plot(1:n1min, bio_2_tss_log_1min_jitter, ylim = c(-0.4,0.9))
# This mixture overestimates the variance for all dates before 16 April, 2010
# at 11:30AM (index 927).
index_tss <- bio_2_tss_log_1min_jitter > 0.2
index_tss <- index_tss * c(rep(1, 9279),
                           rep(0, length(bio_2_tss_log_1min_jitter) - 9279))

bio_2_tss_log_1min_jitter[which(index_tss == 1)] <- 0
plot(bio_2_tss_log_1min_jitter)

tss_jitter <- pH_1min_Seq
tss_jitter$log_bio1 <- bio_1_tss_log_1min_jitter
tss_jitter$log_bio2 <- bio_2_tss_log_1min_jitter
rm(bio_1_tss_log_1min_jitter, bio_2_tss_log_1min_jitter, index_tss)

MADs_df$bio_2_tss_log <- MADs_df$bio_1_tss_log <- 0


### batch_volume
# batch_volume - LOCF
ggplot(data = pH_10m_df[1:1440,], aes(x = dateTime, y = batch_volume)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
oneMin_ls$batch_volume <- pH_10m_df %>%
  select(dateTime, batch_volume) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(batch_volume, .direction = "down") %>%
  select(batch_volume)

MADs_df$batch_volume <- 0


### perm_tank_level
# perm_tank_level - truncate all values < 2, then LOCF. Observations are highly
# discrete
ggplot(data = pH_10m_df, aes(x = dateTime, y = perm_tank_level)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
pH_10m_df[!is.na(pH_10m_df$perm_tank_level) & pH_10m_df$perm_tank_level < 2,
          "perm_tank_level"] <- NA
oneMin_ls$perm_tank_level <- pH_10m_df %>%
  select(dateTime, perm_tank_level) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(perm_tank_level, .direction = "down") %>%
  select(perm_tank_level)

MADs_df$perm_tank_level <- 0


### ras_do
# ras_do - LOCF
ggplot(data = pH_10m_df, aes(x = dateTime, y = ras_do)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
oneMin_ls$ras_do <- pH_10m_df %>%
  select(dateTime, ras_do) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(ras_do, .direction = "down") %>%
  select(ras_do)

MADs_df$ras_do <- 0


### ras_ph
# ras_ph - truncate all values < 5.4, then LOCF
ggplot(data = pH_10m_df, aes(x = dateTime, y = ras_ph)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
pH_10m_df[!is.na(pH_10m_df$ras_ph) &
            (pH_10m_df$ras_ph < 5.4 | pH_10m_df$ras_ph > 9),
          "ras_ph"] <- NA
oneMin_ls$ras_ph <- pH_10m_df %>%
  select(dateTime, ras_ph) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(ras_ph, .direction = "down") %>%
  select(ras_ph)

MADs_df$ras_ph <- 0


### ras_tss
# ras_tss - long signal; truncate all values < 3500, log transform, fit a
#   spline, calculate day-blocked variances, then fit a (linear?) model to the
#   block-specific variances. NO. After spline fitting the logged values, we
#   only see evidence of two variance structures: before and after 16 April,
#   2010, at 11:30AM (index 927 (10 min) / 9279 (1 min)).
ggplot(data = pH_10m_df, aes(x = dateTime, y = ras_tss)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  # geom_hline(yintercept = 3500) +
  geom_point()
pH_10m_df[!is.na(pH_10m_df$ras_tss) & pH_10m_df$ras_tss < 3500, "ras_tss"] <- NA
pH_10m_df$ras_tss_log <- pH_10m_df$ras_tss %>% log
noNA <- pH_10m_df %>%
  select(dateTime, ras_tss_log) %>%
  na.omit
mod <- smooth.spline(noNA$ras_tss_log ~ noNA$dateTime)
ras_tssResid <- mod %>% residuals()
ras_tssResid[1:927] %>% plot(ylim = c(-1, 0.5))
ras_tssResid[1:927] %>% mad
plot(1:927, rnorm(927, sd = 0.186 / (1.5 * sqrt(2 / pi))), ylim = c(-1,0.5))
ras_tssResid[928:4456] %>% plot(ylim = c(-1, 0.5))
ras_tssResid[928:4456] %>% mad
plot(928:4456, rnorm(4456 - 928 + 1, sd = 0.053 / sqrt(2 / pi)), ylim = c(-1,0.5))
# Now put them together
ras_tss_1min_jitter <- c(rnorm(9279, sd = 0.186 / (1.5 * sqrt(2 / pi))),
                         rnorm(n1min - 9279, sd = 0.053 / sqrt(2 / pi)))
ras_tssResid %>% plot(ylim = c(-1, 0.5))
ras_tss_1min_jitter %>% plot(ylim = c(-1, 0.5))
tss_jitter$log_ras <- ras_tss_1min_jitter
rm(ras_tss_1min_jitter)

MADs_df$ras_tss_log <- 0


### perm_cond
# perm_cond - truncate all values < 625, then linearly interpolate with error
#   variances from the residuals of a cubic spline fit. Clearly defined and
#   strong signal
ggplot(data = pH_10m_df, aes(x = dateTime, y = perm_cond)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  # geom_hline(yintercept = 625) +
  geom_point()
pH_10m_df[!is.na(pH_10m_df$perm_cond) & pH_10m_df$perm_cond < 625, "perm_cond"] <- NA
noNA <- pH_10m_df %>%
  select(dateTime, perm_cond) %>%
  na.omit
mod <- smooth.spline(noNA$perm_cond ~ noNA$dateTime)
mod %>% residuals() %>% plot(ylim = c(-50, 50))
mod %>% residuals() %>% mad
plot(1:4300, rnorm(4300, sd = 1.560 / sqrt(2 / pi)), ylim = c(-50,50))

MADs_df$perm_cond <- mod %>% residuals() %>% mad() / sqrt(2 / pi)


### ambient_temp
# ambient_temp - truncate all values < 30, the strict linear interpolation. See
#   bio_x_temp for more.
ggplot(data = pH_10m_df, aes(x = dateTime, y = ambient_temp)) +
  scale_x_datetime(labels = date_format("%m-%d"),
                   minor_breaks = date_breaks("1 day")) +
  geom_point()
pH_10m_df[!is.na(pH_10m_df$ambient_temp) &
            pH_10m_df$ambient_temp < 30,
          "ambient_temp"] <- NA
oneMin_ls$ambient_temp <- pH_10m_df %>%
  select(dateTime, ambient_temp) %>%
  left_join(pH_1min_Seq, .) %>%
  fill(ambient_temp, .direction = "down") %>%
  select(ambient_temp)

MADs_df$ambient_temp <- 0

######  The jitterInterpolate() Function  ######
# # We need to modify the way the jitterInterpolate function handles features with
# # variance equal to 0. The MASS::mvrnorm function can't.
# testMASS <- mvrnorm(n = 900, mu = rep(0, 34),
#         Sigma = diag((1 / 1:34) ^ 2, ncol = 34, nrow = 34))
# testBase <- sapply(rep(c(1,0),5), function(sigma){
#   rnorm(n = 5, mean = 0, sd = sigma)
# })
# testMASS[,6] %>% plot(ylim = c(-1, 1))
# testBase[,6] %>% plot(ylim = c(-1, 1))

jitterInterpolate <- function(data, SDs_df, index, numInterpol){
  # data = a time series data frame with the first column as the time index
  # SDs_df = a data frame of standard deviations. has the same time index as
  # data
  # index = we interpolate values between index and index + 1
  # numInterpol = how many values will we interpolate
  require(dplyr)

  p <- data %>% ncol() - 1
  data_names <- colnames(data) %>% sapply(as.name)

  mads_vec <- sapply(2:(p + 1), function(i){
    SDs_df[index,] %>% select(eval(data_names[[i]])) %>% as.numeric()
  })

  # Generate the jitter values
  # The MASS::mvrnorm function requires a PD covariance matrix, which does not
  #   allow for feature variance to equal 0. For features under strict linear
  #   interpolation or LOCF, we need to generate "errors" form a degenerate
  #   distribution, so the jitter is constantly 0.
  jitter_mat <- sapply(mads_vec, function(sigma){
    rnorm(n = numInterpol, mean = 0, sd = sigma)
  })
  # jitter_mat <- mvrnorm(n = numInterpol,
  #                       mu = rep(0, p),
  #                       Sigma = diag(mads_vec ^ 2, ncol = p, nrow = p))
  # The columns of the jitter matrix are the error values to add to the
  # interpolated values between index and index + 1. Our covariance matrix is
  # diagonal because we assume local independence of the features.

  # Now for the interpolation. I couldn't find an easy way to interpolate
  # between two vectors, so we're going to bind some scalar interpolations
  # together
  interpol_mat <- matrix(0, nrow = numInterpol, ncol = p)
  for(i in 1:p){
    # If we have 1 NA, we'll hit this if statement
    if(anyNA(data[index:(index + 1), i + 1])){
      # Now that we know one of the values are NA, are they both?
      if(all(is.na(data[index:(index + 1), i + 1]))){
        # If all values are NA, then the values between should be NA too
        interpol_mat[,i] <- rep(NA, numInterpol)
      }else{
        # If they are not both NA, then impute a constant between
        interpol_mat[,i] <- approx(data[index:(index + 1), i + 1],
                                   method = "constant",
                                   n = numInterpol)$y
      }
    }else{
      interpol_mat[,i] <- approx(data[index:(index + 1), i + 1],
                                 method = "linear",
                                 n = numInterpol)$y
    }
  }


  # We now have interpolated values and a matrix of errors of the same size,
  # let's add them together
  drift_mat <- interpol_mat + jitter_mat
  # Some of the values don't make real sense - it's possible, for instance, to
  # generate negative values from the white noise distribution. Because of the
  # physical design of the system, negative values (for the most part) are
  # impossible. We will have to truncate these values after generation.
  drift_mat[drift_mat < 0] <- 0

  # We can use the seq.POSIXt function to create a sequences of times between
  # time index and time index + 1
  dateTime <- seq.POSIXt(from = data[index,1],
                         to = data[(index + 1),1],
                         length.out = numInterpol + 2)[2:(numInterpol + 1)]
  dateTime <- data.frame(dateTime = dateTime)

  # We can now bind the POSIX information to the drift matrix
  interpolated_df <- as.data.frame(drift_mat)
  interpolated_df <- bind_cols(dateTime, interpolated_df)
  names(interpolated_df) <- names(data)
  bind_rows(data[index,], interpolated_df)
}


######  Downscaling  ######
# Now that we have the MADs data frame (which is technically the standard
# deviations, not the MADs), and the feature-specific data frames for the LOCF
# interpolation, we can put these all together with the jitterInterpolate()
# function from Data_Cleaning_and_Interpolation_Summary.Rmd
names(pH_10m_df) %in% names(MADs_df)
ncol(pH_10m_df)

pH_10to1_df <- pblapply(1:(nrow(pH_10m_df) - 1),
                        function(i) {
                          jitterInterpolate(data = pH_10m_df %>%
                                              select(-bio_1_tss,
                                                     -bio_2_tss,
                                                     -ras_tss),
                                            SDs_df = MADs_df,
                                            index = i,
                                            numInterpol = 9)
                          }) %>% ldply
pH_10to1_df <- bind_rows(pH_10to1_df, pH_10m_df[4464,])

# Add in the hard-coded jitters (for the three tss variables)
pH_10to1_df$bio_1_tss_log <- pH_10to1_df$bio_1_tss_log +
  tss_jitter$log_bio1
pH_10to1_df$bio_2_tss_log <- pH_10to1_df$bio_2_tss_log +
  tss_jitter$log_bio2
# There is a hard ceiling on ras_tss, so we need to replicate that. Replace all
# values greater than 9.9 (on the log scale) with 9.9.
pH_10to1_df$ras_tss_log <- pH_10to1_df$ras_tss_log +
  tss_jitter$log_ras
pH_10to1_df$ras_tss_log[!is.na(pH_10to1_df$ras_tss_log) &
                          pH_10to1_df$ras_tss_log > 9.9] <- 9.9

# Let's visually check this interpolation
names(pH_10m_df)
ggplot(data = pH_10m_df,
       aes(x = dateTime, y = ras_tss_log)) +
  geom_point()
# If we used jittering, pull the feature from here
ggplot(data = pH_10to1_df,
       aes(x = dateTime, y = ras_tss_log)) +
  geom_point(alpha = 0.1)
# If we created the feature specifically, look at it here. Features created are:
#   ras_temp_1min, bio_1_do_1min, bio_2_do_1min, mbr_1_level, mbr_2_level,
#   sewage_flow_1min, bio_1_blow_flow_1min, bio_2_blow_flow_1min, bio_1_temp_1min,
#   bio_2_temp_1min, perm_tank_level_1min, ras_do_1min, ras_ph_1min, and
#   ambent_temp_1min
ggplot(data = ambient_temp_1min,
       aes(x = dateTime, y = ambient_temp)) +
  geom_point(alpha = 0.1)
# Overall, I think we're downscaling very well.

######  Combine the Interpolated Data Frames  ######
oneMin_df <- oneMin_ls %>% bind_cols()
rm(oneMin_ls)

pH_inter_df <- pH_10to1_df %>% left_join(pH_1min_Seq, .)
pH_inter_df$ambient_temp <- oneMin_df$ambient_temp
pH_inter_df$batch_volume <- oneMin_df$batch_volume
pH_inter_df$bio_1_blow_flow <- oneMin_df$bio_1_blow_flow
pH_inter_df$bio_1_do <- oneMin_df$bio_1_do
pH_inter_df$bio_1_temp <- oneMin_df$bio_1_temp
pH_inter_df$bio_2_blow_flow <- oneMin_df$bio_2_blow_flow
pH_inter_df$bio_2_do <- oneMin_df$bio_2_do
pH_inter_df$bio_2_temp <- oneMin_df$bio_2_temp
pH_inter_df$mbr_1_level <- oneMin_df$mbr_1_level
pH_inter_df$mbr_2_level <- oneMin_df$mbr_2_level
pH_inter_df$perm_tank_level <- oneMin_df$perm_tank_level
pH_inter_df$ras_do <- oneMin_df$ras_do
pH_inter_df$ras_ph <- oneMin_df$ras_ph
pH_inter_df$ras_temp <- oneMin_df$ras_temp
pH_inter_df$sewage_flow <- oneMin_df$sewage_flow
pH_inter_df$bio_1_tss <- NULL
pH_inter_df$bio_2_tss <- NULL
pH_inter_df$ras_tss <- NULL

#######  Save the 10-minute Data  ######
write_csv(pH_inter_df, path = "pH_Fault_interpolated_20170405.csv")

#######  Join the Data Frames  ######
pH_1m_df %>% str
pH_inter_df %>% str

# We need to round the POSIX column to the nearest minute.
pH_1m_df$dateTime %>% head
pH_1m_df$dateTime %>% round.POSIXt(units = "mins") %>% head
pH_1m_df$dateTime_round <- round.POSIXt(pH_1m_df$dateTime, units = "mins") %>%
  as.POSIXct() # round the times to the nearest minute. We now have 36 rows with
# duplicate time stamps
# Aggregate the duplicate rows by applying the "mean()" function with the
# numcolwise() call. This takes about 95 seconds
pH_1m_round_df <- ddply(pH_1m_df, "dateTime_round", numcolwise(mean))
# Rename the date-time column so we can join the two data frames
pH_1m_round_df %<>% rename(dateTime = dateTime_round)

# Remove comlumns in pH_inter_df which are duplicates of columns in pH_1m_df
names(pH_inter_df)
names(pH_1m_df)
pH_inter_clean_df <- pH_inter_df %>% select(-bio_1_do, -bio_2_do,
                                            -mbr_1_level, -mbr_2_level,
                                            -mbr_1_tmp, -mbr_2_tmp,
                                            -perm_turb, -sewage_flow,
                                            -bio_1_blow_flow, -bio_2_blow_flow,
                                            -mbr_1_perm_flow, -mbr_2_perm_flow,
                                            -mbr_1_inf_flow, -mbr_2_inf_flow)

# Full outer join the data frames
pH_Fault_df <- full_join(pH_1m_round_df, pH_inter_clean_df, by = "dateTime")

# Inner join the data frames
pH_Fault_Inner_df <- inner_join(pH_1m_round_df,
                                pH_inter_clean_df,
                                by = "dateTime")

######  Save the Joined Data  ######
write_csv(pH_Fault_Inner_df, path = "pH_Fault_InnerJoin_20170405.csv")
write_csv(pH_Fault_df, path = "pH_Fault_OuterJoin_20170405.csv")

pH_Fault_Inner_df <- read_csv("C:/Users/gabriel_odom/Box Sync/Consulting/Dr. Hering/MV_Process_Control/Kazor Code/pH_Fault_InnerJoin_20170405.csv")

# Missing Data
sum(is.na(pH_Fault_Inner_df$sewage_level))
sum(is.na(pH_Fault_Inner_df$mbr_1_perm_pres))
sum(is.na(pH_Fault_Inner_df$mbr_2_perm_pres))
sum(is.na(pH_Fault_Inner_df$mbr_1_air_flow))
sum(is.na(pH_Fault_Inner_df$mbr_2_air_flow))

pH_df <- na.omit(pH_Fault_Inner_df)

######  Graphs Part 2  ######
adDetect_time <- as.POSIXct("2010-04-21 13:40:00 CDT")
humanDetect_time <- as.POSIXct("2010-04-24 10:00:00 CDT")
pH_names_full <- sapply(names(pH_df), as.name)

for(i in 2:length(pH_names_full)){
  xx <- ggplot(data = pH_df,
               aes(x = dateTime, y = eval(pH_names_full[[i]]))) +
    scale_y_continuous(name = eval(quote(pH_names_full[[i]]))) +
    scale_x_datetime(labels = date_format("%m-%d"),
                     minor_breaks = date_breaks("1 day")) +
    geom_point(alpha = 0.1) +
    geom_vline(aes(xintercept = as.numeric(adDetect_time)),
               linetype = 4, colour = "blue", size = 1) +
    geom_vline(aes(xintercept = as.numeric(humanDetect_time)),
               linetype = 4, colour = "red", size = 1)
  print(xx)
}

# Based on these graphs, I really think we need to start training the model on
# the 16th of April, not the 10th (when the data starts).

