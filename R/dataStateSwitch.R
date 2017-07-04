#' Alternate Observations in a Data Frame over States
#'
#' @description Split single-state process observations, apply multiple state
#'   projections, and combine these observations into a single data frame,
#'   arranged by process time or index.
#'
#' @param df A data frame returned by processNOCdata() or faultSwitch().
#' @param angles2 Change the principal angles for State 2.
#' @param scales2 Change the principal scales for State 2.
#' @param angles3 Change the principal angles for State 3.
#' @param scales3 Change the principal scales for State 3.
#'
#' @return A data frame containing the time index, state, and feature values
#'   after state-specific rotation and scaling; this data frame also contains
#'   the other columns of df that aren't the feature values. This data frame has
#'   \itemize{
#'     \item{dateTime - }{a POSIX column of the time stamps for each
#'       observation}
#'     \item{state - }{column of state membership (1, 2, or 3)}
#'     \item{x - }{the process values for the first feature, corresponding to
#'       t + random error}
#'     \item{y - }{the process values for the second feature, corresponding to
#'       t ^ 2 - 3 * t + random error}
#'     \item{z - }{the process values for the third feature, corresponding to
#'       -t ^ 3 + 3 * t ^ 2 + random error}
#'     \item{t - }{the non-stationary and autocorrelated latent feature}
#'     \item{err1 - }{a Gaussian white noise vector}
#'     \item{err2 - }{a Gaussian white noise vector}
#'     \item{err3 - }{a Gaussian white noise vector}
#'   }
#'
#' @details This function splits a process data frame by state, and rotates and
#'   scales the observations from states 2 and 3 by the scales and angles
#'   specified in the function arguments. After state-specific rotation and
#'   scaling, this function combines the observations back together and orders
#'   them by process time index. This function takes in data frame returned by
#'   processNOCdata() or faultSwitch(). This function calls rotateScale3D() and
#'   is called internally by mspProcessData().
#'
#' @seealso Calls: \code{\link{processNOCdata}}, \code{\link{faultSwitch}},
#'   \code{\link{rotateScale3D}}. Called by: \code{\link{mspProcessData}}
#'
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples nrml <- processNOCdata()
#' dataStateSwitch(nrml)
dataStateSwitch <- function(df,
                            angles2 = list(yaw = 0, pitch = 90, roll = 30),
                            scales2 = c(1, 0.5, 2),
                            angles3 = list(yaw = 90, pitch = 0, roll = -30),
                            scales3 = c(0.25, 0.1, 0.75)){
  # browser()

  mat <- df %>% select(x,y,z) %>% as.matrix

  # State 2
  S2_mat <- mat %*% rotateScale3D(rot_angles = angles2,
                                  scale_factors = scales2)

  # State 3
  S3_mat <- mat %*% rotateScale3D(rot_angles = angles3,
                                  scale_factors = scales3)

  # Now concatenate these vectors
  df$xState2 <- S2_mat[,1]
  df$yState2 <- S2_mat[,2]
  df$zState2 <- S2_mat[,3]
  df$xState3 <- S3_mat[,1]
  df$yState3 <- S3_mat[,2]
  df$zState3 <- S3_mat[,3]

  # Hourly switching process
  state1_df <- df %>%
    filter(state == 1) %>%
    select(dateTime, state, x, y, z)
  state2_df <- df %>%
    filter(state == 2) %>%
    select(dateTime, state, x = xState2, y = yState2, z = zState2)
  state3_df <- df %>%
    filter(state == 3) %>%
    select(dateTime, state, x = xState3, y = yState3, z = zState3)
  switch_df <- bind_rows(state1_df, state2_df, state3_df) %>%
    arrange(dateTime)
  switch_df <- bind_cols(switch_df,
                         df %>% select(t, err1, err2, err3))

  switch_df
}
