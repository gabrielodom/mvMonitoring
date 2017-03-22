#' Induce the Specified Fault on NOC Observations
#'
#' @description Infect the input data frame with a specific fault, then return
#'   the infected data frame.
#'
#' @param df A data frame returned by the processNOCdata() function.
#' @param fault A character string. Options are "NOC", "A1", "B1", "A2", "B2",
#'   "A3", "B3", or "All". See "details" of mspProcessData() for more
#'   information.
#' @param faultStartIndex An integer specifying the index at which the faults
#'   will start.
#' @param period The observation cycle length. Defaults to one week's worth of
#'   minute-level observations (10,080 observations).
#' @param shift The fault parameter for faults "A1" and "B1" corresponding to
#'   the positive shock value added to features. Defaults to 2. See "details" of
#'   mspProcessData() for more information.
#'
#' @return A data frame with the same structure as df, but with faults induced
#'   across all observations. The mspProcessData() function then subsets the
#'   observations necessary to corrupt the normal data frame, and binds them
#'   together by row. See mspProcessData() for more details.
#'
#' @details The faults return data frames as follows: \itemize{
#'   \item{A1 -- }{A data frame with 10080 rows and five columns, corresponding
#'     by default to one week worth of data recorded at a 1-minute interval (as
#'     defined by the "period" argument of this function and the "increment"
#'     argument of the processNOCdata() function). The fault is a system shift
#'     to each of the three features by 2 (the "shift" argument). The fault
#'     starts at row 8500 (specified by the argument "faultStartIndex"), and the
#'     five columns under the fault state are defined here: \itemize{
#'       \item{dateTime : }{a POSIXct column}
#'       \item{state : }{the state indicator for the multivariate system, with
#'         three levels when the argument "multiState" is TRUE and one level
#'         otherwise}
#'       \item{x : }{x(t) = t + shift + error}
#'       \item{y : }{y(t) = t ^ 2 - 3t + shift + error}
#'       \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + shift + error}
#'     }
#'     where t is a 10080-entry vector of autocorrelated and non-stationary
#'     hidden process realizations generated within the processNOCdata() function.
#'   }
#'   \item{B1 -- }{A matrix as defined in A1, but with x, y, and z feature
#'     columns defined as follows: \itemize{
#'       \item{x : }{x(t) = t + shift + error}
#'       \item{y : }{y(t) = t ^ 2 - 3t + error}
#'       \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + error}
#'     }
#'   }
#'   \item{A2 -- }{The fault is a drift on each feature by
#'     (s - faultStartIndex / 10 ^ 3, where s is the observation index. The
#'     fault starts at "faultStartIndex", and the x, y, and z feature columns
#'     are defined as follows: \itemize{
#'       \item{x : }{x(t) = t + drift + error}
#'       \item{y : }{y(t) = t ^ 2 - 3t + drift + error}
#'       \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + drift + error}
#'     }
#'   }
#'   \item{B2 -- }{The fault is a drift a drift on the "y" and "z" feature by
#'     (s - faultStartIndex / 10 ^ 3, where s is the observation index. The
#'     fault starts at "faultStartIndex", and the x, y, and z feature columns
#'     are defined as follows: \itemize{
#'       \item{x : }{x(t) = t + error}
#'       \item{y : }{y(t) = t ^ 2 - 3t + drift + error}
#'       \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + drift + error}
#'     }
#'   }
#'   \item{A3 -- }{The fault is a signal amplificaton in the determining latent
#'     t vector. The fault starts at "faultStartIndex", and the x, y, and z
#'     features under the fault state are defined here: \itemize{
#'       \item{x : }{x(t_*) = t_* + error}
#'       \item{y : }{y(t_*) = (t_*) ^ 2 - 3t_* + error}
#'       \item{z : }{z(t_*) = -(t_*) ^ 3 + 3(t_*) ^ 2 + error}
#'     }
#'     where t_* = 5 x t x (period - s) / (period - faultStartIndex) and s is
#'     the observation index.
#'   }
#'   \item{B3 -- }{The fault is a signal amplificaton in the determining latent
#'     t vector for the "z" feature only. The fault starts at "faultStartIndex",
#'     and the x, y, and z features under the fault state are defined here:
#'     \itemize{
#'       \item{x : }{x(t) = t + error}
#'       \item{y : }{y(t) = (t) ^ 2 - 3t + error}
#'       \item{z : }{z(t_*) = -(t_*) ^ 3 + 3(t_*) ^ 2 + error}
#'     }
#'     where t_* = 3 x t x (period - s) / (2 x period) and s is the observation
#'     index.
#'   }
#' }
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples nrml <- processNOCdata(startTime = "2016-11-27 00:00:00 CST")
#' faultSwitch(nrml, fault = "NOC", faultStartIndex = 8500)
faultSwitch <- function(df, fault,
                        faultStartIndex,
                        period = 10080, shift = 2){
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
    amplify_vec <- 5 * (1:period - faultStartIndex) /
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
