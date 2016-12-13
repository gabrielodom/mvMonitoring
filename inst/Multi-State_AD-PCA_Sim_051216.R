# This is our simulation file to replicate the simulation done in Kazor et al
# (2016), section 3.

######  Generate the t vector  ################################################
omega <- 60 * 24 * 7 # One week of minute-level observations
phi <- 0.75 # autocorrelation coefficient

a <- 0.01; b <- 2
# See Kazor et al pp 1533. These are the mean and variance of a Unif(0.01,2).
mean_t_err <- 0.5 * (a + b)
var_t_err <- (b - a) / 12

# We will create an autocorrelated vector of errors
t_err <- vector(length = omega)
# Initialise the errors so that the overall mean and variance match the sim of
# Kazor et al.
t_err[1] <- rnorm(n = 1,
                  mean = mean_t_err * (1 - phi),
                  sd = sqrt(var_t_err * (1 - phi ^ 2)))
for(s in 2:omega){
  t_err[s] <- phi * t_err[(s - 1)] +
    (1 - phi) * rnorm(n = 1,
                      mean = mean_t_err * (1 - phi),
                      sd = sqrt(var_t_err * (1 - phi ^ 2)))
}
# We will inspect our error vector.
plot(t_err)
mean(t_err); var(t_err)
cor(t_err[2:omega], t_err[1:(omega - 1)])
# I used a Durbin-Watson test, but the p-value was 0.4 for data with phi = 0.95
# dwtest is useless
pacf(t_err)
# Our error vector looks good.

# TEST: create a t vector without the sinusoidal component
t_star <- t_err

# # Now we create the t vector. Our three features are parametric in t, so we
# # install a nonstationary structure to the ts, to further compound the AR error
# t_star <- vector(length = omega)
# t_star[1] <- -cos(2 * pi / omega) + t_err[1]
# for(s in 2:omega){
#   t_star[s] <- -cos(2 * pi * s / omega) +
#     phi * t_err[(s - 1)] + (1 - phi) * t_err[s]
# }
# Now we scale the ts to match the ts from the Unif(0.01,2) used in Kazor et al
t_star_adj <- ((b - a) * (t_star - min(t_star))) /
  (max(t_star) - min(t_star)) + a

# Inspect our t vector
plot(t_star_adj, type = "l")
mean(t_star); var(t_star)
cor(t_star_adj[2:omega], t_star_adj[1:(omega - 1)])
lmtest::dwtest(t_star_adj[2:omega] ~ t_star_adj[1:(omega - 1)])
pacf(t_star)
# Looks great

######  Features and State Features  ##########################################
# We will use <x(t), y(t), z(t)> instead of the <x_1(t), x_2(t), x_3(t)>
# notation used in Kazor et al. This will assist us when we introduce feature
# states. Our state notation will be <x_k(t), y_k(t), z_k(t)> for State k.
library(scatterplot3d)
library(magrittr)
library(dplyr)

normal_df <- data.frame(t = t_star_adj,
                        err1 = rnorm(n = omega, sd = sqrt(0.01)),
                        err2 = rnorm(n = omega, sd = sqrt(0.01)),
                        err3 = rnorm(n = omega, sd = sqrt(0.01)))

###  State 1  ###
normal_df %<>% mutate(x = t + err1,
                     y = t ^ 2 - 3 * t + err2,
                     z = -t ^ 3 + 3 * t ^ 2 + err3)

plot(normal_df$t)
plot(normal_df$x)
plot(normal_df$y)
plot(normal_df$z)
scatterplot3d(x = normal_df$x,
              y = normal_df$y,
              z = normal_df$x)
# It checks out

orig_state_mat <- normal_df %>% select(x,y,z) %>% as.matrix


# Now we can create data for our other two states. These states will be scaled
# rotations of the current <x,y,z> set. Thus, we need "rot_and_scale3d()" from
# the "Rotation_Playground.R" file
source("inst/Rotation_Playground.R")

###  State 2  ###
state2_angles <- list(yaw = 0,
                      pitch = 90,
                      roll = 30)
state2_scales <- c(1,0.5,2)
state2_mat <- orig_state_mat %*% rot_and_scale3d(rot_angles = state2_angles,
                                               scale_factors = state2_scales)
scatterplot3d(state2_mat)

###  State 3  ###
state3_angles <- list(yaw = 90,
                      pitch = 0,
                      roll = -30)
state3_scales <- c(0.25,0.1,0.75)
state3_mat <- orig_state_mat %*% rot_and_scale3d(rot_angles = state3_angles,
                                               scale_factors = state3_scales)
scatterplot3d(state3_mat)

###  Combination and Cleaning  ###
# Now concatenate these vectors
normal_df$xState2 <- state2_mat[,1]
normal_df$yState2 <- state2_mat[,2]
normal_df$zState2 <- state2_mat[,3]
normal_df$xState3 <- state3_mat[,1]
normal_df$yState3 <- state3_mat[,2]
normal_df$zState3 <- state3_mat[,3]

# Add a State label column
normal_df %<>%
  mutate(state = rep(rep(c(1, 2, 3), each = 60), (24 / 3) * 7))

# Add a date-time column
normal_df %<>%
  mutate(dateTime = seq.POSIXt(from = as.POSIXct("2016-11-27 00:00:00 CST"),
                               by = "min", length.out = 10080))

###  Hourly Switching Process  ###
state1_df <- normal_df %>%
  filter(state == 1) %>%
  select(dateTime, state, x, y, z)
state2_df <- normal_df %>%
  filter(state == 2) %>%
  select(dateTime, state, x = xState2, y = yState2, z = zState2)
state3_df <- normal_df %>%
  filter(state == 3) %>%
  select(dateTime, state, x = xState3, y = yState3, z = zState3)
normal_switch_df <- bind_rows(state1_df, state2_df, state3_df) %>%
  arrange(dateTime)

normal_switch_df %>%
  filter(state == 3) %>%
  select(x, y, z) %>%
  scatterplot3d(xlim = c(-2, 3),
                ylim = c(-3, 1),
                zlim = c(-1, 5))

normal_switch_df %>%
  select(x, y, z) %>%
  scatterplot3d(xlim = c(-2, 3),
                ylim = c(-3, 1),
                zlim = c(-1, 5))

######  Fault Introduction  ###################################################
faultStart <- 8500

###  Fault 1A  ###
# Shift each <x, y, z> by 2
shift <- 2
fault1A_df <- normal_df %>%
  select(dateTime, state, t, x, y, z) %>%
  mutate(x = x + shift,
         y = y + shift,
         z = z + shift)
fault1A_mat <- fault1A_df %>% select(x,y,z) %>% as.matrix

# State 2
fault1A_S2_mat <- fault1A_mat %*% rot_and_scale3d(rot_angles = state2_angles,
                                                  scale_factors = state2_scales)
scatterplot3d(fault1A_S2_mat)

# State 3
fault1A_S3_mat <- fault1A_mat %*% rot_and_scale3d(rot_angles = state3_angles,
                                                  scale_factors = state3_scales)
scatterplot3d(fault1A_S3_mat)

# Now concatenate these vectors
fault1A_df$xState2 <- fault1A_S2_mat[,1]
fault1A_df$yState2 <- fault1A_S2_mat[,2]
fault1A_df$zState2 <- fault1A_S2_mat[,3]
fault1A_df$xState3 <- fault1A_S3_mat[,1]
fault1A_df$yState3 <- fault1A_S3_mat[,2]
fault1A_df$zState3 <- fault1A_S3_mat[,3]

# Hourly switching process
fault1A_state1_df <- fault1A_df %>%
  filter(state == 1) %>%
  select(dateTime, state, x, y, z)
fault1A_state2_df <- fault1A_df %>%
  filter(state == 2) %>%
  select(dateTime, state, x = xState2, y = yState2, z = zState2)
fault1A_state3_df <- fault1A_df %>%
  filter(state == 3) %>%
  select(dateTime, state, x = xState3, y = yState3, z = zState3)
fault1A_switch_df <- bind_rows(fault1A_state1_df,
                               fault1A_state2_df,
                               fault1A_state3_df) %>%
  arrange(dateTime)

fault1A_switch_df %>%
  select(x, y, z) %>%
  scatterplot3d()


###  Fault 1B  ###
# Shift each x by 2
fault1B_df <- normal_df %>%
  select(dateTime, state, t, x, y, z) %>%
  mutate(x = x + shift)
fault1B_mat <- fault1B_df %>% select(x,y,z) %>% as.matrix

# State 2
fault1B_S2_mat <- fault1B_mat %*% rot_and_scale3d(rot_angles = state2_angles,
                                                  scale_factors = state2_scales)
scatterplot3d(fault1B_S2_mat)

# State 3
fault1B_S3_mat <- fault1B_mat %*% rot_and_scale3d(rot_angles = state3_angles,
                                                  scale_factors = state3_scales)
scatterplot3d(fault1B_S3_mat)

# Now concatenate these vectors
fault1B_df$xState2 <- fault1B_S2_mat[,1]
fault1B_df$yState2 <- fault1B_S2_mat[,2]
fault1B_df$zState2 <- fault1B_S2_mat[,3]
fault1B_df$xState3 <- fault1B_S3_mat[,1]
fault1B_df$yState3 <- fault1B_S3_mat[,2]
fault1B_df$zState3 <- fault1B_S3_mat[,3]

# Hourly switching process
fault1B_state1_df <- fault1B_df %>%
  filter(state == 1) %>%
  select(dateTime, state, x, y, z)
fault1B_state2_df <- fault1B_df %>%
  filter(state == 2) %>%
  select(dateTime, state, x = xState2, y = yState2, z = zState2)
fault1B_state3_df <- fault1B_df %>%
  filter(state == 3) %>%
  select(dateTime, state, x = xState3, y = yState3, z = zState3)
fault1B_switch_df <- bind_rows(fault1B_state1_df,
                               fault1B_state2_df,
                               fault1B_state3_df) %>%
  arrange(dateTime)

fault1B_switch_df %>%
  select(x, y, z) %>%
  scatterplot3d()

###  Fault 2A  ###
# Drift each <x, y, z> by (step - faultStart) / 10 ^ 3
drift_vec <- (1:omega - faultStart) / 10 ^ 3
drift_vec[drift_vec < 0] <- 0
fault2A_df <- normal_df %>%
  select(dateTime, state, t, x, y, z) %>%
  mutate(x = x + drift_vec,
         y = y + drift_vec,
         z = z + drift_vec)
fault2A_mat <- fault2A_df %>% select(x,y,z) %>% as.matrix

# State 2
fault2A_S2_mat <- fault2A_mat %*% rot_and_scale3d(rot_angles = state2_angles,
                                                  scale_factors = state2_scales)
scatterplot3d(fault2A_S2_mat)

# State 3
fault2A_S3_mat <- fault2A_mat %*% rot_and_scale3d(rot_angles = state3_angles,
                                                  scale_factors = state3_scales)
scatterplot3d(fault2A_S3_mat)

# Now concatenate these vectors
fault2A_df$xState2 <- fault2A_S2_mat[,1]
fault2A_df$yState2 <- fault2A_S2_mat[,2]
fault2A_df$zState2 <- fault2A_S2_mat[,3]
fault2A_df$xState3 <- fault2A_S3_mat[,1]
fault2A_df$yState3 <- fault2A_S3_mat[,2]
fault2A_df$zState3 <- fault2A_S3_mat[,3]

# Hourly switching process
fault2A_state1_df <- fault2A_df %>%
  filter(state == 1) %>%
  select(dateTime, state, x, y, z)
fault2A_state2_df <- fault2A_df %>%
  filter(state == 2) %>%
  select(dateTime, state, x = xState2, y = yState2, z = zState2)
fault2A_state3_df <- fault2A_df %>%
  filter(state == 3) %>%
  select(dateTime, state, x = xState3, y = yState3, z = zState3)
fault2A_switch_df <- bind_rows(fault2A_state1_df,
                               fault2A_state2_df,
                               fault2A_state3_df) %>%
  arrange(dateTime)

fault2A_switch_df %>%
  select(x, y, z) %>%
  scatterplot3d()


###  Fault 2B  ###
# Drift each <y, z> by (step - faultStart) / 10 ^ 3
fault2B_df <- normal_df %>%
  select(dateTime, state, t, x, y, z) %>%
  mutate(y = y + drift_vec,
         z = z + drift_vec)
fault2B_mat <- fault2B_df %>% select(x,y,z) %>% as.matrix

# State 2
fault2B_S2_mat <- fault2B_mat %*% rot_and_scale3d(rot_angles = state2_angles,
                                                  scale_factors = state2_scales)
scatterplot3d(fault2B_S2_mat)

# State 3
fault2B_S3_mat <- fault2B_mat %*% rot_and_scale3d(rot_angles = state3_angles,
                                                  scale_factors = state3_scales)
scatterplot3d(fault2B_S3_mat)

# Now concatenate these vectors
fault2B_df$xState2 <- fault2B_S2_mat[,1]
fault2B_df$yState2 <- fault2B_S2_mat[,2]
fault2B_df$zState2 <- fault2B_S2_mat[,3]
fault2B_df$xState3 <- fault2B_S3_mat[,1]
fault2B_df$yState3 <- fault2B_S3_mat[,2]
fault2B_df$zState3 <- fault2B_S3_mat[,3]

# Hourly switching process
fault2B_state1_df <- fault2B_df %>%
  filter(state == 1) %>%
  select(dateTime, state, x, y, z)
fault2B_state2_df <- fault2B_df %>%
  filter(state == 2) %>%
  select(dateTime, state, x = xState2, y = yState2, z = zState2)
fault2B_state3_df <- fault2B_df %>%
  filter(state == 3) %>%
  select(dateTime, state, x = xState3, y = yState3, z = zState3)
fault2B_switch_df <- bind_rows(fault2B_state1_df,
                               fault2B_state2_df,
                               fault2B_state3_df) %>%
  arrange(dateTime)

fault2B_switch_df %>%
  select(x, y, z) %>%
  scatterplot3d()

###  Fault 3A  ###
# Amplify t for each <x, y, z> by 3 * (omega - step) * t / (2 * omega)
amplify_vec <- 2 * (1:omega - faultStart) / (omega - faultStart) + 1
amplify_vec[amplify_vec < 1] <- 1
fault3A_df <- normal_df %>%
  select(dateTime, state, t, err1, err2, err3) %>%
  mutate(t = amplify_vec * t) %>%
  mutate(x = t + err1,
         y = t ^ 2 - 3 * t + err2,
         z = -t ^ 3 + 3 * t ^ 2 + err3)
fault3A_mat <- fault3A_df %>% select(x,y,z) %>% as.matrix

# State 2
fault3A_S2_mat <- fault3A_mat %*% rot_and_scale3d(rot_angles = state2_angles,
                                                  scale_factors = state2_scales)
scatterplot3d(fault3A_S2_mat)

# State 3
fault3A_S3_mat <- fault3A_mat %*% rot_and_scale3d(rot_angles = state3_angles,
                                                  scale_factors = state3_scales)
scatterplot3d(fault3A_S3_mat)

# Now concatenate these vectors
fault3A_df$xState2 <- fault3A_S2_mat[,1]
fault3A_df$yState2 <- fault3A_S2_mat[,2]
fault3A_df$zState2 <- fault3A_S2_mat[,3]
fault3A_df$xState3 <- fault3A_S3_mat[,1]
fault3A_df$yState3 <- fault3A_S3_mat[,2]
fault3A_df$zState3 <- fault3A_S3_mat[,3]

# Hourly switching process
fault3A_state1_df <- fault3A_df %>%
  filter(state == 1) %>%
  select(dateTime, state, x, y, z)
fault3A_state2_df <- fault3A_df %>%
  filter(state == 2) %>%
  select(dateTime, state, x = xState2, y = yState2, z = zState2)
fault3A_state3_df <- fault3A_df %>%
  filter(state == 3) %>%
  select(dateTime, state, x = xState3, y = yState3, z = zState3)
fault3A_switch_df <- bind_rows(fault3A_state1_df,
                               fault3A_state2_df,
                               fault3A_state3_df) %>%
  arrange(dateTime)

fault3A_switch_df %>%
  select(x, y, z) %>%
  scatterplot3d()


###  Fault 3B  ###
# Dampen t for z by log|t|
t_log <- normal_df$t
t_log[faultStart:omega] <- log(t_log[faultStart:omega])
fault3B_df <- normal_df %>%
  select(dateTime, state, t, err1, err2, err3) %>%
  mutate(t_damp = t_log) %>%
  mutate(x = t + err1,
         y = t ^ 2 - 3 * t + err2,
         z = -t_damp ^ 3 + 3 * t_damp ^ 2 + err3)
fault3B_mat <- fault3B_df %>% select(x,y,z) %>% as.matrix

# State 2
fault3B_S2_mat <- fault3B_mat %*% rot_and_scale3d(rot_angles = state2_angles,
                                                  scale_factors = state2_scales)
scatterplot3d(fault3B_S2_mat)

# State 3
fault3B_S3_mat <- fault3B_mat %*% rot_and_scale3d(rot_angles = state3_angles,
                                                  scale_factors = state3_scales)
scatterplot3d(fault3B_S3_mat)

# Now concatenate these vectors
fault3B_df$xState2 <- fault3B_S2_mat[,1]
fault3B_df$yState2 <- fault3B_S2_mat[,2]
fault3B_df$zState2 <- fault3B_S2_mat[,3]
fault3B_df$xState3 <- fault3B_S3_mat[,1]
fault3B_df$yState3 <- fault3B_S3_mat[,2]
fault3B_df$zState3 <- fault3B_S3_mat[,3]

# Hourly switching process
fault3B_state1_df <- fault3B_df %>%
  filter(state == 1) %>%
  select(dateTime, state, x, y, z)
fault3B_state2_df <- fault3B_df %>%
  filter(state == 2) %>%
  select(dateTime, state, x = xState2, y = yState2, z = zState2)
fault3B_state3_df <- fault3B_df %>%
  filter(state == 3) %>%
  select(dateTime, state, x = xState3, y = yState3, z = zState3)
fault3B_switch_df <- bind_rows(fault3B_state1_df,
                               fault3B_state2_df,
                               fault3B_state3_df) %>%
  arrange(dateTime)

fault3B_switch_df %>%
  select(x, y, z) %>%
  scatterplot3d()

######  The Seven Data Frames  ################################################
# The normal_df data frame is without fault. We will remove rows faultStart to
# omega, and replace these rows with the same rows from each each of the fault
# data frames.
# Turn all the data frames into xts objects for Kazor's function to work.
# NOTE - this follows a depreciated version of zoo, so we need the dateTime
# column as the rownames of the data frame, not as an index.
# EDITED NOTE - B. Barnard and I rebuilt Kazor's code as a functional package.
# The functions in these packages use xts matrices as inputs.
library(xts)
normal_switch_xts <- xts(normal_switch_df[,-1],
                         order.by = normal_switch_df[,1])

# Fault 1A
normal_w_fault1A_df <- bind_rows(normal_switch_df[1:(faultStart - 1),],
                              fault1A_switch_df[faultStart:omega,])
fault1A_xts <- xts(normal_w_fault1A_df[,-1], order.by = normal_switch_df[,1])

# Fault 1B
normal_w_fault1B_df <- bind_rows(normal_switch_df[1:(faultStart - 1),],
                                 fault1B_switch_df[faultStart:omega,])
fault1B_xts <- xts(normal_w_fault1B_df[,-1], order.by = normal_switch_df[,1])

# Fault 2A
normal_w_fault2A_df <- bind_rows(normal_switch_df[1:(faultStart - 1),],
                                 fault2A_switch_df[faultStart:omega,])
fault2A_xts <- xts(normal_w_fault2A_df[,-1], order.by = normal_switch_df[,1])

# Fault 2B
normal_w_fault2B_df <- bind_rows(normal_switch_df[1:(faultStart - 1),],
                                 fault2B_switch_df[faultStart:omega,])
fault2B_xts <- xts(normal_w_fault2B_df[,-1], order.by = normal_switch_df[,1])

# Fault 3A
normal_w_fault3A_df <- bind_rows(normal_switch_df[1:(faultStart - 1),],
                                 fault3A_switch_df[faultStart:omega,])
fault3A_xts <- xts(normal_w_fault3A_df[,-1], order.by = normal_switch_df[,1])

# Fault 3B
normal_w_fault3B_df <- bind_rows(normal_switch_df[1:(faultStart - 1),],
                                 fault3B_switch_df[faultStart:omega,])
fault3B_xts <- xts(normal_w_fault3B_df[,-1], order.by = normal_switch_df[,1])

faults_ls <- list(normal = normal_switch_xts,
                  fault1A = fault1A_xts,
                  fault1B = fault1B_xts,
                  fault2A = fault2A_xts,
                  fault2B = fault2B_xts,
                  fault3A = fault3A_xts,
                  fault3B = fault3B_xts)

# Double check that faults are visible and according to plan:
# faults_ls$fault1A %>% select(x) %>% plot.ts()
# faults_ls$fault1A %>% select(y) %>% plot.ts()
# faults_ls$fault1A %>% select(z) %>% plot.ts()
# faults_ls$fault1B %>% select(x) %>% plot.ts()
# faults_ls$fault1B %>% select(y) %>% plot.ts()
# faults_ls$fault1B %>% select(z) %>% plot.ts()
# faults_ls$fault2A %>% select(x) %>% plot.ts()
# faults_ls$fault2A %>% select(y) %>% plot.ts()
# faults_ls$fault2A %>% select(z) %>% plot.ts()
# faults_ls$fault2B %>% select(x) %>% plot.ts()
# faults_ls$fault2B %>% select(y) %>% plot.ts()
# faults_ls$fault2B %>% select(z) %>% plot.ts()
# faults_ls$fault3A %>% select(x) %>% plot.ts()
# faults_ls$fault3A %>% select(y) %>% plot.ts()
# faults_ls$fault3A %>% select(z) %>% plot.ts()
# faults_ls$fault3B %>% select(x) %>% plot.ts()
# faults_ls$fault3B %>% select(y) %>% plot.ts()
# faults_ls$fault3B %>% select(z) %>% plot.ts()
faults_ls$fault1A$x %>% plot.xts()
faults_ls$fault1A$y %>% plot.xts()
faults_ls$fault1A$z %>% plot.xts()
faults_ls$fault1B$x %>% plot.xts()
faults_ls$fault1B$y %>% plot.xts()
faults_ls$fault1B$z %>% plot.xts()
faults_ls$fault2A$x %>% plot.xts()
faults_ls$fault2A$y %>% plot.xts()
faults_ls$fault2A$z %>% plot.xts()
faults_ls$fault2B$x %>% plot.xts()
faults_ls$fault2B$y %>% plot.xts()
faults_ls$fault2B$z %>% plot.xts()
faults_ls$fault3A$x %>% plot.xts()
faults_ls$fault3A$y %>% plot.xts()
faults_ls$fault3A$z %>% plot.xts()
faults_ls$fault3B$x %>% plot.xts()
faults_ls$fault3B$y %>% plot.xts()
faults_ls$fault3B$z %>% plot.xts()


######  AD-PCA Implementation  ################################################
mspMonitor(data = faults_ls$normal[,2:4],
           labelVector = faults_ls$normal[,1],
           trainObs = 720,
           updateFreq = 180)
