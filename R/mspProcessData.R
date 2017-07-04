#' Simulate Normal or Fault Observations from a Single-State or Multi-State
#' Process
#'
#' @description Generate single- or multi-state observations under normal
#'   operating conditions or under fault conditions.
#'
#' @param faults A character vector of faults chosen. Options are "NOC", "A1",
#'   "B1", "C1", "A2", "B2", "C2", "A3", "B3", "C3", or "All". See details for
#'   more information.
#' @param period The observation cycle length. Defaults to one week's worth of
#'   minute-level observations (10,080 observations).
#' @param faultStartIndex An integer specifying the index at which the faults
#'   will start. Defaults to roughly 85 percent through the cycle.
#' @param startTime a POSIXct object specifying the day and time for the
#'   starting observation.
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
#' @param adpcaTest If "multiState" is TRUE, incorrectly label all the states
#'   the same. This should only be used to test AD-PCA performance under a true
#'   multi-state model. Defaults to FALSE.
#' @param msadpcaTest If "multiState" is FALSE, incorrectly label all the states
#'   at random. This should only be used to test MSAD-PCA performance under a
#'   true single-state model. Defaults to FALSE.
#' @param ... Lazy dots for internal arguments
#'
#' @return A list of data frames named with the names of the given faults with
#'   the following information: \itemize{
#'     \item{dateTime -- }{A POSIXct column of times starting at the user-
#'       defined `startTime` argument, length given by the `period` argument,
#'       and spacing given by the `increment` argument. For example, if the
#'       starting value is "2016-01-10", period is 10080, and the incrementation
#'       is in minutes, then this sequence will be one week's worth of
#'       observations recorded every minute from midnight on the tenth of
#'       January.}
#'     \item{state -- }{An integer column of all 1's (when the `multiState`
#'       argument is FALSE), or a column of the state values (1, 2 or 3).}
#'     \item{altState -- }{If either adpcaTest or msadpcaTest are TRUE, this
#'       column will contain incorrect state information used for testing the
#'       different treatment arms against their respective controls.}
#'     \item{x -- }{A double column of generated values for the first feature.}
#'     \item{y -- }{A double column of generated values for the second feature.}
#'     \item{z -- }{A double column of generated values for the third feature.}
#'     }
#'   If the user only specifies one fault, then this function will return the
#'   single xts matrix, instead of a list of one matrix. For details on how
#'   these features are defined, see the "details" of the processNOCdata()
#'   function.
#'
#' @details For details on how the faults are induced, see the "details" of the
#'   faultSwitch() function. This function also includes AD-PCA versus MSAD-PCA
#'   treatment arm testing. There are four possibilities to test: \itemize{
#'     \item{1. }{The true process has one state, and we correctly assume the
#'     true process has one state. In this case, AD-PCA and MSAD-PCA are exactly
#'     the same. Draw observations from this state by setting the "multiState"
#'     argument to FALSE. The "state" label will correctly mark each observation
#'     as from the same state.}
#'     \item{2. }{The true process has one state, but we incorrectly assume the
#'     true process has multiple states. In this case, AD-PCA should outperform
#'     MSAD-PCA in false alarm rates and waiting time to the first alarm. Draw
#'     observations from this state by setting the "multiState" argument to
#'     FALSE and the "msadpcaTest" argument to TRUE. The "state" label will be
#'     contain randomly generated state values (1, 2, and 3 are all equally
#'     likely) for each observation.}
#'     \item{3. }{The true process has multiple states, but we incorrectly
#'     assume the true process has one single states. In this case, MSAD-PCA
#'     should outperform AD-PCA in false alarm rates and waiting time to the
#'     first alarm. Draw observations from this state by setting the "multiState"
#'     argument to TRUE and the "adpcaTest" argument to TRUE. The "state" label
#'     will be identical for each observation.}
#'     \item{4. }{The true process has multiple states, and we correctly assume
#'     the true process has multiple states. In this case, MSAD-PCA
#'     should outperform AD-PCA in false alarm rates and waiting time to the
#'     first alarm. Draw observations from this state by setting the "multiState"
#'     argument to TRUE. The "state" label will correctly mark each observation
#'     as from the same state.}
#'   }
#'
#' @seealso Calls: \code{\link{processNOCdata}}, \code{\link{faultSwitch}},
#'   \code{\link{dataStateSwitch}}. Simulation pipe flow: \code{mspProcessData}
#'   into \code{\link{mspTrain}} into \code{\link{mspMonitor}} into
#'   \code{\link{mspWarning}}.
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom xts xts
#'
#' @examples mspProcessData(faults = "All")
mspProcessData <- function(faults,
                           period = 7 * 24 * 60,
                           faultStartIndex = round(0.8433 * period),
                           startTime = "2015-05-16 10:00:00 CST",
                           multiState = TRUE,
                           angles2 = list(yaw = 0, pitch = 90, roll = 30),
                           scales2 = c(1, 0.5, 2),
                           angles3 = list(yaw = 90, pitch = 0, roll = -30),
                           scales3 = c(0.25, 0.1, 0.75),
                           adpcaTest = FALSE,
                           msadpcaTest = FALSE,
                           ...){
  lazy_ls <- lazy_dots(...)

  if(identical(faults, "All")){
    faults <- c("NOC",
                "A1", "B1", "C1",
                "A2", "B2", "C2",
                "A3", "B3", "C3")
  }

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
                            period = period,
                            postStateSplit = FALSE)
    # fault_xts <- xts(fault_df %>% select(x, y, z, state),
    #                  order.by = fault_df[,8])
    # mspGraphsGrid(fault_xts)

    ###  Modify and Combine the Observations  ###
    normal_df <- if(multiState){
      dataStateSwitch(normal_df,
                      angles2 = angles2, scales2 = scales2,
                      angles3 = angles3, scales3 = scales3) %>%
        select(dateTime, state, x, y, z)
    }else{
      normal_df$state <- 1
      normal_df %>% select(dateTime, state, x, y, z)
    }

    fault_df <- if(multiState){
      df <- dataStateSwitch(fault_df,
                      angles2 = angles2, scales2 = scales2,
                      angles3 = angles3, scales3 = scales3)
      # df_xts <- xts(df %>% select(x, y, z, state),
      #                  order.by = df[,1])
      # mspGraphsGrid(df_xts)
      df2 <- faultSwitch(df, fault = x,
                  faultStartIndex = faultStartIndex,
                  period = period, postStateSplit = TRUE) %>%
        select(dateTime, state, x, y, z)
      # df2_xts <- xts(df2 %>% select(x, y, z, state),
      #                  order.by = df2[,1])
      # mspGraphsGrid(df2_xts)
      df2
    }else{
      fault_df$state <- 1
      fault_df %>% select(dateTime, state, x, y, z)
    }

    ###  Bind the Normal and Fault Observations  ###
    normal_w_fault_df <- bind_rows(normal_df[1:(faultStartIndex - 1),],
                                   fault_df[faultStartIndex:period,])

    ###  Enable AD-PCA and MSAD-PCA Testing  ###
    if(multiState == TRUE & adpcaTest == TRUE){
      # If we have a true multi-state process, but want to falsely assume that
      # the observations are not from a multi-state process, then we overwrite
      # the state label. Use this to test the AD-PCA control arm under the
      # multi-state hypothesis
      normal_w_fault_df$altState <- 1
    }
    if(multiState == FALSE & msadpcaTest == TRUE){
      # If we have a true single-state process, but want to falsely assume that
      # the observations are from a multi-state process, then we overwrite the
      # state label. Use this to test the MSAD-PCA treatment arm under the
      # single-state hypothesis.
      falseStates <- runif(period, min = 1, max = 4)
      normal_w_fault_df$altState <- trunc(falseStates)
    }

    ###  Create the xts Matrix  ###
    xts(normal_w_fault_df[,-1], order.by = normal_df[,1])
  })
  names(df_ls) <- faults

  # If the user only wants one matrix, don't give them a list.
  if(length(faults) == 1){
    obj <- df_ls[[1]]
  }else{
    obj <- df_ls
  }

  obj
}

#
# ###  Fault 3C  ###
# # This fault requires us to infect the underlying t vector with a drift,
# # but only for Feature Y in state 2. We can do this by taking the fault_df
# # object, and multiplying it by the inverse of the Scale matrix of state 2
# # and then by the inverse of the rotation matrix of state 2.
# if(x == "C3"){
#   df <- dataStateSwitch(fault_df,
#                         angles2 = angles2, scales2 = scales2,
#                         angles3 = angles3, scales3 = scales3)
# }else{
#   dataStateSwitch(fault_df,
#                   angles2 = angles2, scales2 = scales2,
#                   angles3 = angles3, scales3 = scales3)
# }
