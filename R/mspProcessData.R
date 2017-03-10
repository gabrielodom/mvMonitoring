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
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#'
#' @examples mspProcessData(fault = 0,
#'                          faultStartIndex = 8500,
#'                          startTime = "2016-11-27 00:00:00 CST",
#'                          multiState = TRUE)
mspProcessData <- function(fault = NULL, faultStartIndex = NULL, startTime, ...){
  lazy_ls <- lazy_dots(...)

  normal_df <- do.call(processNOCdata,
                       args = c(list(startTime = startTime),
                                lazy_eval(lazy_ls)))
  normal_df
}
