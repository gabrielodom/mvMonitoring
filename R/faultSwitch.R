#' Induce the Specified Fault on NOC Observations
#'
#' @description Infect the input data frame with a specific fault, then return
#'   the infected data frame.
#'
#' @param df A data frame returned by the processNOCdata() function.
#' @param fault A character string. Options are "NOC", "A1", "B1", "C1", "A2",
#'   "B2", "C2", "A3", "B3", or "C3". See "details" of mspProcessData() for more
#'   information.
#' @param period The observation cycle length. Defaults to one week's worth of
#'   minute-level observations (10,080 observations).
#' @param faultStartIndex An integer specifying the index at which the faults
#'   will start. Defaults to roughly 85 percent through the cycle.
#' @param shift The fault parameter for faults "A1" and "B1" corresponding to
#'   the positive shock value added to features. Defaults to 2. See "details" of
#'   mspProcessData() for more information.
#' @param postStateSplit Should we induce faults before or after state-splitting?
#'   Defaults to FALSE. Make this argument TRUE for faults 1C, 2C, 3C.
#'
#' @return A data frame with the same structure as df, but with faults induced
#'   across all observations. The mspProcessData() function then subsets the
#'   observations necessary to corrupt the normal data frame, and binds them
#'   together by row. This function is called by mspProcessData(). See
#'   ?mspProcessData for more details.
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
#'   \item{C1 -- }{A matrix as defined in A1, but with x, y, and z feature
#'     columns defined as follows: \itemize{
#'       \item{x : }{x(t) = t + shift / 4 + error}
#'       \item{y : }{y(t) = t ^ 2 - 3t + error}
#'       \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + shift / 4 + error}
#'     }
#'     This shift is applied only in State 3.
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
#'   \item{C2 -- }{The fault is a negative drift on the "y" feature by 1.5 *
#'     (s - faultStartIndex) / (period - faultStartIndex). Thus, \itemize{
#'       \item{x : }{x(t) = t + error}
#'       \item{y : }{y(t) = t ^ 2 - 3t - drift + error}
#'       \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + error}
#'     }
#'     This drift is applied only in State 2.
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
#'   \item{C3 -- }{This fault is a change in the error structure of feature "y".
#'     We let errorNew = 2 * error - 0.25, so that \itemize{
#'       \item{x : }{x(t) = t + error}
#'       \item{y : }{y(t) = t ^ 2 - 3t + errorNew}
#'       \item{z : }{z(t) = -t ^ 3 + 3t ^ 2 + error}
#'     }
#'     This new error structure is applied only in State 2.
#'   }
#' }
#'
#' @seealso Called by: \code{\link{mspProcessData}}.
#'
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @examples nrml <- processNOCdata()
#' faultSwitch(nrml, fault = "NOC")
faultSwitch <- function(df, fault,
                        period = 7 * 24 * 60,
                        faultStartIndex =  round(0.8433 * period),
                        shift = 2,
                        postStateSplit = FALSE){
  # browser()

  ###  Define the Fault Functions  ###
  # Fault 1A
  fault1A <- function(df, shift){
    df1 <- mutate(df,
                  x = .data$x + shift,
                  y = .data$y + shift,
                  z = .data$z + shift)
    select(df1,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  # Fault1B
  fault1B <- function(df, shift){
    df1 <- mutate(df, x = .data$x + shift)
    select(df1,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  # Fault 1C
  fault1C <- function(df, shift){
    df1 <- mutate(df, state3Ind = .data$state == 3)
    df2 <- mutate(df1,
                  x = .data$x + .data$state3Ind * shift / 4,
                  z = .data$z + .data$state3Ind * shift / 4)
    select(df2,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  # Fault 2A
  fault2A <- function(df, faultStartIndex, period){
    drift_vec <- (1:period - faultStartIndex) / 10 ^ 3
    drift_vec[drift_vec < 0] <- 0
    df1 <- mutate(df,
                  x = .data$x + drift_vec,
                  y = .data$y + drift_vec,
                  z = .data$z + drift_vec)
    select(df1,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  # Fault 2B
  fault2B <- function(df, faultStartIndex, period){
    drift_vec <- (1:period - faultStartIndex) / 10 ^ 3
    drift_vec[drift_vec < 0] <- 0
    df1 <- mutate(df,
                  y = .data$y + drift_vec,
                  z = .data$z + drift_vec)
    select(df1,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  # Fault 2C
  fault2C <- function(df, faultStartIndex, period){
    drift_vec <- 1.5 * (1:period - faultStartIndex) / (period - faultStartIndex)
    drift_vec[drift_vec < 0] <- 0
    df1 <- mutate(df, state2Ind = .data$state == 2)
    df2 <- mutate(df1, y = .data$y - .data$state2Ind * drift_vec)
    select(df2,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  # Fault 3A
  fault3A <- function(df, faultStartIndex, period){
    amplify_vec <- 5 * (1:period - faultStartIndex) /
      (period - faultStartIndex) + 1
    amplify_vec[amplify_vec < 1] <- 1
    df1 <- mutate(df, t = amplify_vec * t)
    df2 <- mutate(df1,
                  x = t + .data$err1,
                  y = t ^ 2 - 3 * t + .data$err2,
                  z = -t ^ 3 + 3 * t ^ 2 + .data$err3)
    select(df2,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  # Fault 3B
  fault3B <- function(df, faultStartIndex, period){
    t_log <- df$t
    t_log[faultStartIndex:period] <- log(t_log[faultStartIndex:period])
    df1 <- mutate(df, t_damp = t_log)
    df2 <- mutate(df1,
                  x = t + .data$err1,
                  y = t ^ 2 - 3 * t + .data$err2,
                  z = -.data$t_damp ^ 3 + 3 * .data$t_damp ^ 2 + .data$err3)
    select(df2,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  # Fault 3C
  fault3C <- function(df, faultStartIndex, period){
    # amplify_vec <- 5 * (1:period - faultStartIndex) /
    #   (period - faultStartIndex) + 1
    # amplify_vec[amplify_vec < 1] <- 1
    df1 <- mutate(df, state2Ind = .data$state == 2)
    # Remember that this is 1 + whatever the coefficent is, because
    #   y already has err2 in it.
    df2 <- mutate(df1, err2Mod = (2 * .data$err2 - 0.25) * .data$state2Ind)
    df3 <- mutate(df2, y = .data$y + .data$err2Mod)
    select(df3,
           .data$dateTime, .data$state,
           .data$t, .data$x, .data$y, .data$z,
           .data$err1, .data$err2, .data$err3)
  }

  ###  Return the Faulted Data  ###
  df <- if(postStateSplit == FALSE){
    switch(fault,
           NOC = df,
           A1  = fault1A(df, shift = shift),
           B1  = fault1B(df, shift = shift),
           C1  = df,
           A2  = fault2A(df, faultStartIndex = faultStartIndex,
                        period = period),
           B2  = fault2B(df, faultStartIndex = faultStartIndex,
                        period = period),
           C2  = df,
           A3  = fault3A(df, faultStartIndex = faultStartIndex,
                        period = period),
           B3  = fault3B(df, faultStartIndex = faultStartIndex,
                        period = period),
           C3  = df)
  }else{
    switch(fault,
           NOC = df,
           A1  = df,
           B1  = df,
           C1  = fault1C(df, shift = shift),
           A2  = df,
           B2  = df,
           C2  = fault2C(df, faultStartIndex = faultStartIndex,
                        period = period),
           A3  = df,
           B3  = df,
           C3  = fault3C(df, faultStartIndex = faultStartIndex,
                         period = period))
  }

  df
}
