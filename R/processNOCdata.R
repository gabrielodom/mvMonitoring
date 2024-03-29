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
#' @param increment The time-sequence base increment. See "Details" of the
#'   seq.POSIXt() function options. Defaults to "min" for minutes.
#' @param multiState Should the observations be generated from a multi-state
#'   process? Defaults to TRUE.
#' @param autocorellation The autocorrelation parameter. Must be less than 1 in
#'   absolute value, or the process generated will be nonstationary. Defaults to
#'   0.75 in accordance to Kazor et al (2016).
#' @param tLower Lower bound of the latent $t$ variable. Defaults to 0.01.
#' @param tUpper Upper bound of the latent $t$ variable. Defaults to 2.
#' @param errVar Error variance of the normal white noise process on the feature
#'   variables.
#'
#' @return An data frame with the following information: \describe{
#'   \item{dateTime -- }{A POSIXct column of times starting at the user-defined
#'     `startTime` argument, length given by the `period` argument, and spacing
#'     given by the `increment` argument. For example, if the starting value is
#'     "2016-01-10", period is 10080, and the incrementation is in minutes, then
#'     this sequence will be one week's worth of observations recorded every
#'     minute from midnight on the tenth of January.}
#'   \item{state -- }{An integer column of all 1's (when the `multiState`
#'     argument is FALSE), or a column of the state values (1, 2 or 3).}
#'   \item{x -- }{A double column of generated values for the first feature.}
#'   \item{y -- }{A double column of generated values for the second feature.}
#'   \item{z -- }{A double column of generated values for the third feature.}
#'   }
#'
#' @details This function randomly generates a non-stationary (sinusoidal) and
#'   autocorrelated latent variable t with lower and upper bounds given by the
#'   arguments "tLower" and "tUpper", respectively, with autocorrelation
#'   governed by the "autocorrelation" argument. Necessarily, this coefficient
#'   must be less than 1 in absolute value, otherwise the latent variable will
#'   be unbounded. Next, this function draws a realization of this random
#'   variable t and calculates three functions of it, then jitters these
#'   functions with a normal white noise variable (with variance set by
#'   "errVar"). These three functions are: \describe{
#'     \item{x : }{x(t) = t + error}
#'     \item{y : }{y(t) = t ^ 2 - 3t + error}
#'     \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + error}
#'   }
#'   This function is called by the mspProcessData() function. See
#'   ?mspProcessData for more details.
#'
#' @seealso Called by: \code{\link{mspProcessData}}.
#'
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom stats rnorm
#'
#' @examples processNOCdata()
processNOCdata <- function(startTime = "2015-05-16 10:00:00 CST",
                           period = 7 * 24 * 60,
                           stateDuration = 60,
                           increment = "min",
                           multiState = TRUE,
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
                      x = t + .data$err1,
                      y = t ^ 2 - 3 * t + .data$err2,
                      z = -t ^ 3 + 3 * t ^ 2 + .data$err3)
  orig_state_mat <- as.matrix(select(normal_df, .data$x, .data$y, .data$z))

  # Add a date-time column
  normal_df$dateTime <- seq.POSIXt(from = as.POSIXct(startTime),
                                   by = "min", length.out = period)

  ###  Single-State or Multi-State?  ###
  if(multiState != TRUE){
    normal_df$state <- 1
  }else{
    normal_df$state <- rep(rep(1:3, each = stateDuration),
                           times = period / stateDuration / 3)
  }

  normal_df
}
