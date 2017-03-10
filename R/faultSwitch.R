#' Induce the Specified Fault on NOC Observations
#'
#' @description
#'
#' @param df
#' @param fault
#' @param faultStartIndex
#' @param period
#' @param shift
#'
#' @return
#'
#' @details
#'
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples
faultSwitch <- function(df, fault,
                        faultStartIndex,
                        period, shift = 2){
  ###  Define the Fault Functions  ###
  # Fault 1A
  fault1A <- function(df, shift){
    df %>%
      select(dateTime, state, t, x, y, z) %>%
      mutate(x = x + shift,
             y = y + shift,
             z = z + shift)
  }

  # Fault1B
  fault1B <- function(df, shift){
    df %>%
      select(dateTime, state, t, x, y, z) %>%
      mutate(x = x + shift)
  }

  # Fault 2A
  fault2A <- function(df, faultStartIndex, period){
    drift_vec <- (1:period - faultStartIndex) / 10 ^ 3
    drift_vec[drift_vec < 0] <- 0
    df %>%
      select(dateTime, state, t, x, y, z) %>%
      mutate(x = x + drift_vec,
             y = y + drift_vec,
             z = z + drift_vec)
  }

  # Fault 2B
  fault2B <- function(df, faultStartIndex, period){
    drift_vec <- (1:period - faultStartIndex) / 10 ^ 3
    drift_vec[drift_vec < 0] <- 0
    df %>%
      select(dateTime, state, t, x, y, z) %>%
      mutate(y = y + drift_vec,
             z = z + drift_vec)
  }

  # Fault 3A
  fault3A <- function(df, faultStartIndex, period){
    amplify_vec <- 2 * (1:period - faultStartIndex) /
      (period - faultStartIndex) + 1
    amplify_vec[amplify_vec < 1] <- 1
    df %>%
      select(dateTime, state, t, err1, err2, err3) %>%
      mutate(t = amplify_vec * t) %>%
      mutate(x = t + err1,
             y = t ^ 2 - 3 * t + err2,
             z = -t ^ 3 + 3 * t ^ 2 + err3)
  }

  # Fault 3B
  fault3B <- function(df, faultStartIndex, period){
    t_log <- df$t
    t_log[faultStartIndex:period] <- log(t_log[faultStartIndex:period])
    df %>%
      select(dateTime, state, t, err1, err2, err3) %>%
      mutate(t_damp = t_log) %>%
      mutate(x = t + err1,
             y = t ^ 2 - 3 * t + err2,
             z = -t_damp ^ 3 + 3 * t_damp ^ 2 + err3)
  }

  ###  Return the Faulted Data  ###
  switch(fault,
         NOC = df,
         A1 = fault1A(df, shift = shift),
         B1 = fault1B(df, shift = shift),
         A2 = fault2A(df, faultStartIndex = faultStartIndex, period = period),
         B2 = fault2B(df, faultStartIndex = faultStartIndex, period = period),
         A3 = fault3A(df, faultStartIndex = faultStartIndex, period = period),
         B3 = fault3B(df, faultStartIndex = faultStartIndex, period = period))

}
