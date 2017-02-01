#' Real-Time Process Monitoring Function
#'
#' @description Monitor and flag (if necessary) incoming multivariate process
#' observations.
#'
#' @param observations an n * p xts matrix. For real-time monitoring via a
#' script within a batch file, n = 1, so this must be a matrix of a single
#' row.
#' @param labelVector an n * 1 integer vector of class memberships
#' @param trainingSummary the TrainingSpecs list returned by the mspTrain()
#' function. This list contains --for each class-- the SPE and T2 thresholds,
#' as well the projection matrix.
#' @param ... Lazy dots for additional internal arguments
#'
#' @return An n * (p + 5) xts matrix, where the last five columns are the
#' monitoring statistics and corresponding fault flags, and an empty alarm
#' column
#'
#' @details This function is designed to be ran at specific time intervals
#' (every 10 seconds, 30 seconds, 1 minute, 5 minutes, 10 minutes, etc), from a
#' batch file hosted script which calls this function and mspWarning(). This
#' function takes in the specific observations to monitor and their class
#' memberships (if any) and returns an xts matrix of these observations column
#' concatenated with their monitoring statistic values, flag statuses, and an
#' empty alarm column. Users should then append these rows onto a previously
#' existing daily observations matrix. The mspWarning() function will then take
#' in the daily observation xts matrix with updated rows returned by this
#' function and check the monitoring statistic flag indicators to see if an
#' alarm status has been reached. For further details, see the mspWarning()
#' function.
#'
#' This function calls the faultDetect() function, and requires the training
#' information returned by the mspTrain function. This function will return
#' the xts matrix necessary for the mspWarning() function.
#'
#' @export
#'
#' @examples
mspMonitor <- function(observations,
                       labelVector,
                       trainingSummary,
                       ...){

  classes <- unique(labelVector)

  # If our user accidentally sends in a vector instead of an xts matrix
  if(is.null(dim(observations))){
    observations <- matrix(observations, nrow = 1)
    warning("Observation not an xts object.", immediate. = TRUE)
  }
  classData <- cbind(labelVector, observations)

  # Apply the fault detection function to each row, accounting for the chance
  # that rows come from different classes
  dataAndFaults <- lapply(1:nrow(classData), function(i){
    faultObj <- faultDetect(threshold_object = trainingSummary[[classData[i,1]]],
                            observation = classData[i, -1])
    cbind(classData[i,], faultObj)
  })
  obsAndFlags <- do.call(rbind, dataAndFaults)
  obsAndFlags <- cbind(obsAndFlags, NA)
  colnames(obsAndFlags)[ncol(obsAndFlags)] <- "Alarm"

  obsAndFlags
}
