#' Simulate Normal or Fault Observations from a Single-State or Multi-State
#' Process
#'
#' @description Generate single- or multi-state observations under normal
#'   operating conditions or under fault conditions.
#'
#' @param faults A character vector of faults chosen. Options are "NOC", "A1",
#'   "B1", "A2", "B2", "A3", or "B3". See details for more information.
#' @param faultStartIndex An integer specifying the index at which the faults
#'   will start.
#' @param startTime a POSIXct object specifying the day and time for the
#'   starting observation.
#' @param period The observation cycle length. Defaults to one week's worth of
#'   minute-level observations (10,080 observations).
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
#' @param ... Lazy dots for internal arguments
#'
#' @return A list of data frames named with the names of the given faults with
#'   the following information: \itemize{ \item{dateTime -- }{A POSIXct column
#'   of times starting at the user-defined `startTime` argument, length given by
#'   the `period` argument, and spacing given by the `increment` argument. For
#'   example, if the starting value is "2016-01-10", period is 10080, and the
#'   incrementation is in minutes, then this sequence will be one week's worth
#'   of observations recorded every minute from midnight on the tenth of
#'   January.} \item{state -- }{An integer column of all 1's (when the
#'   `multiState` argument is FALSE), or a column of the state values (1, 2 or
#'   3).} \item{x -- }{A double column of generated values for the first
#'   feature.} \item{y -- }{A double column of generated values for the second
#'   feature.} \item{z -- }{A double column of generated values for the third
#'   feature.}}
#'
#' @details For details on how the faults are induced, see the "details" of the
#'   faultSwitch() function.
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom magrittr %>%
#' @importFrom xts xts
#'
#' @examples mspProcessData(faults = c("NOC", "A1"),
#'                          faultStartIndex = 8500,
#'                          startTime = "2016-11-27 00:00:00 CST",
#'                          multiState = TRUE)
mspProcessData <- function(faults,
                           faultStartIndex,
                           startTime,
                           period = 7 * 24 * 60,
                           multiState = TRUE,
                           angles2 = list(yaw = 0, pitch = 90, roll = 30),
                           scales2 = c(1, 0.5, 2),
                           angles3 = list(yaw = 90, pitch = 0, roll = -30),
                           scales3 = c(0.25, 0.1, 0.75),
                           ...){
  lazy_ls <- lazy_dots(...)

  # Single-state NOC observations
  normal_df <- do.call(processNOCdata,
                       args = c(list(startTime = startTime,
                                     period = period),
                                lazy_eval(lazy_ls)))


  ###  Apply Across Chosen Faults  ###
  df_ls <- lapply(faults, function(x){
    # browser()

    fault_df <- faultSwitch(df = normal_df,
                            fault = x,
                            faultStartIndex = faultStartIndex,
                            period = period)

    ###  Modify and Combine the Observations  ###
    normal_df <- if(multiState){
      dataStateSwitch(normal_df,
                      angles2 = angles2, scales2 = scales2,
                      angles3 = angles3, scales3 = scales3)
    }else{
      normal_df$state <- 1
      normal_df %>% select(dateTime, state, x, y, z)
    }

    fault_df <- if(multiState){
      dataStateSwitch(fault_df,
                      angles2 = angles2, scales2 = scales2,
                      angles3 = angles3, scales3 = scales3)
    }else{
      fault_df$state <- 1
      fault_df %>% select(dateTime, state, x, y, z)
    }

    ###  Make xts Matrix  ###
    normal_w_fault_df <- bind_rows(normal_df[1:(faultStartIndex - 1),],
                                   fault_df[faultStartIndex:period,])

    xts(normal_w_fault_df[,-1], order.by = normal_df[,1])
  })
  names(df_ls) <- faults
  df_ls
}
