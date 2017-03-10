#' Simulate Normal or Fault Observations from a Single-State or Multi-State
#' Process
#'
#' @description
#'
#' @param fault
#' @param faultStartIndex
#' @param startTime
#' @param ...
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
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom magrittr %>%
#' @importFrom xts xts
#'
#' @examples mspProcessData(fault = "NOC",
#'                          faultStartIndex = 8500,
#'                          startTime = "2016-11-27 00:00:00 CST",
#'                          multiState = TRUE)
mspProcessData <- function(fault,
                           faultStartIndex,
                           startTime,
                           period = 7 * 24 * 60,
                           angles2 = list(yaw = 0, pitch = 90, roll = 30),
                           scales2 = c(1, 0.5, 2),
                           angles3 = list(yaw = 90, pitch = 0, roll = -30),
                           scales3 = c(0.25, 0.1, 0.75),
                           ...){
  lazy_ls <- lazy_dots(...)

  # browser()

  normal_df <- do.call(processNOCdata,
                       args = c(list(startTime = startTime,
                                     period = period,
                                     angles2 = angles2,
                                     scales2 = scales2,
                                     angles3 = angles3,
                                     scales3 = scales3),
                                lazy_eval(lazy_ls)))

  # Add if / else statement here

  ###  Call the Switch Function  ###
  fault_df <- faultSwitch(df = normal_df,
                    fault = fault,
                    faultStartIndex = faultStartIndex,
                    period = period)

  ###  Modify and Combine the Observations  ###
  fault_mat <- fault_df %>% select(x,y,z) %>% as.matrix

  # State 2
  fault_S2_mat <- fault_mat %*%
    rotateScale3D(rot_angles = angles2, scale_factors = scales2)

  # State 3
  fault_S3_mat <- fault_mat %*%
    rotateScale3D(rot_angles = angles3, scale_factors = scales3)

  # Now concatenate these vectors
  fault_df$xState2 <- fault_S2_mat[,1]
  fault_df$yState2 <- fault_S2_mat[,2]
  fault_df$zState2 <- fault_S2_mat[,3]
  fault_df$xState3 <- fault_S3_mat[,1]
  fault_df$yState3 <- fault_S3_mat[,2]
  fault_df$zState3 <- fault_S3_mat[,3]

  # Hourly switching process
  fault_state1_df <- fault_df %>%
    filter(state == 1) %>%
    select(dateTime, state, x, y, z)
  fault_state2_df <- fault_df %>%
    filter(state == 2) %>%
    select(dateTime, state, x = xState2, y = yState2, z = zState2)
  fault_state3_df <- fault_df %>%
    filter(state == 3) %>%
    select(dateTime, state, x = xState3, y = yState3, z = zState3)
  fault_switch_df <- bind_rows(fault_state1_df,
                               fault_state2_df,
                               fault_state3_df) %>%
    arrange(dateTime)

  ###  Make xts Matrix  ###
  normal_df <- normal_df %>% select(dateTime, state, x, y, z)
  normal_w_fault_df <- bind_rows(normal_df[1:(faultStartIndex - 1),],
                                 fault_switch_df[faultStartIndex:period,])

  xts(normal_w_fault_df[,-1], order.by = normal_df[,1])
}
