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
#'   after state-specific rotation and scaling.
#'
#' @details This function splits a process data frame by state, and rotates and
#'   scales the observations from states 2 and 3 by the scales and angles
#'   specified in the function arguments. After state-specific rotation and
#'   scaling, this function combines the observations back together and orders
#'   them by process time index.
#'
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples nrml <- processNOCdata(startTime = "2016-11-27 00:00:00 CST")
#' dataStateSwitch(nrml)
dataStateSwitch <- function(df,
                            angles2 = list(yaw = 0, pitch = 90, roll = 30),
                            scales2 = c(1, 0.5, 2),
                            angles3 = list(yaw = 90, pitch = 0, roll = -30),
                            scales3 = c(0.25, 0.1, 0.75)){
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

  switch_df
}
