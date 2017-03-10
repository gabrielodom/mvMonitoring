#' Simulate NOC Observations from a Single-State or Multi-State Process
#'
#' @description This function generates data under normal operating conditions
#'   from a single-state or multi-state process model.
#'
#' @param startTime a POSIXct object specifying the day and time for the
#'   starting observation.
#' @param period The observation cycle length. Defaults to one week's worth of
#'   minute-level observations (10,080 observations).
#' @param stateDuration The number of observations generated during a stay in
#'   each state. Defaults to 60.
#' @param increment The time-sequence base increment. See "Deatails" of the
#'   seq.POSIXt() function options. Defaults to "min" for minutes.
#' @param multiState Should the observations be generated from a multi-state
#'   process? Defaults to TRUE.
#' @param angles2 Change the principal angles for State 2. Defaults to yaw = 0,
#'   pitch = 90, and roll = 30.
#' @param scales2 Change the principal scales for State 2. Defaults to 1, 0.5,
#'   and 2.
#' @param angles3 Change the principal angles for State 3. Defaults to yaw = 90,
#'   pitch = 0, and roll = -30.
#' @param scales3 Change the principal scales for State 3. Defaults to 0.25,
#'   0.1, and 0.75.
#' @param autocorellation The autocorrelation parameter. Must be less than 1 in
#'   absolute value, or the process generated will be nonstationary. Defaults to
#'   0.75 in accordance to Kazor et al (2016).
#' @param tLower Lower bound of the latent $t$ variable. Defaults to 0.01.
#' @param tUpper Upper bound of the latent $t$ variable. Defaults to 2.
#' @param errVar Error variance of the normal white noise process on the feature
#'   variables.
#'
#' @return An data frame with the following information: \itemize{
#'   \item{dateTime -- }{A POSIXct column of times starting at the user-defined
#'   `startTime` argument, length given by the `period` argument, and spacing
#'   given by the `increment` argument. For example, if the starting value is
#'   "2016-01-10", period is 10080, and the incrementation is in minutes, then
#'   this sequence will be one week's worth of observations recorded every
#'   minute from midnight on the tenth of January.} \item{state -- }{An integer
#'   column of all 1's (when the `multiState` argument is FALSE), or a column of
#'   the state values (1, 2 or 3).} \item{x -- }{A double column of generated
#'   values for the first feature.} \item{y -- }{A double column of generated
#'   values for the second feature.} \item{z -- }{A double column of generated
#'   values for the third feature.}}
#'
#' @details This function is called by the mspProcessData() function.
#'
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom stats rnorm
#' @importFrom xts xts
#'
#' @examples processNOCdata(startTime = "2016-11-27 00:00:00 CST")
processNOCdata <- function(startTime,
                           period = 7 * 24 * 60,
                           stateDuration = 60,
                           increment = "min",
                           multiState = TRUE,
                           angles2 = list(yaw = 0, pitch = 90, roll = 30),
                           scales2 = c(1, 0.5, 2),
                           angles3 = list(yaw = 90, pitch = 0, roll = -30),
                           scales3 = c(0.25, 0.1, 0.75),
                           autocorellation = 0.75,
                           tLower = 0.01,
                           tUpper = 2,
                           errVar = 0.01){
  # browser()

  ###  Create the Latent Variable Vector  ###
  omega <- period
  phi <- autocorellation
  a <- tLower; b <- tUpper
  mean_t_err <- 0.5 * (a + b) # mean of Uniform(0.01,2)
  var_t_err <- (b - a) / 12 # variance of Uniform(0.01,2)

  # Create an autocorrelated vector of errors for the random variable t
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

  # Now for the vector itself
  s <- 1:omega
  t_star <- -cos(2 * pi * s / omega) + t_err

  # Now we scale the ts to match the ts from the Unif(0.01,2)
  t_star_adj <- ((b - a) * (t_star - min(t_star))) /
    (max(t_star) - min(t_star)) + a

  ###  Generate Three Features and Timestamp  ###
  normal_df <- data.frame(t = t_star_adj,
                          err1 = rnorm(n = omega, sd = sqrt(errVar)),
                          err2 = rnorm(n = omega, sd = sqrt(errVar)),
                          err3 = rnorm(n = omega, sd = sqrt(errVar)))
  normal_df <- mutate(normal_df,
                      x = t + err1,
                      y = t ^ 2 - 3 * t + err2,
                      z = -t ^ 3 + 3 * t ^ 2 + err3)
  orig_state_mat <- normal_df %>% select(x, y, z) %>% as.matrix

  # Add a date-time column
  normal_df$dateTime <- seq.POSIXt(from = as.POSIXct(startTime),
                                   by = "min", length.out = period)

  ###  Escape for Single-State Case  ###
  if(multiState != TRUE){
    normal_df$state <- 1
    return(normal_df %>% select(dateTime, state, x, y, z))
  }

  ###  And Three States  ###
  # State 2
  state2_mat <- orig_state_mat %*%
    rotateScale3D(rot_angles = angles2, scale_factors = scales2)
  # State 3
  state3_mat <- orig_state_mat %*%
    rotateScale3D(rot_angles = angles3, scale_factors = scales3)

  # Combine these, and label the states
  normal_df$xState2 <- state2_mat[,1]
  normal_df$yState2 <- state2_mat[,2]
  normal_df$zState2 <- state2_mat[,3]
  normal_df$xState3 <- state3_mat[,1]
  normal_df$yState3 <- state3_mat[,2]
  normal_df$zState3 <- state3_mat[,3]
  normal_df$state <- rep(rep(1:3, each = stateDuration),
                         times = period / stateDuration / 3)

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

  normal_switch_df
  # xts(select(normal_switch_df, -dateTime),
  #            order.by = select(normal_switch_df, dateTime)[,1])
}
