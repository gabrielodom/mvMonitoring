#' Real-Time Process Monitoring Function
#'
#' @description
#'
#' @param observations an n * p xts matrix. For real-time monitoring via batch
#' file, n = 1, so this must be a matrix of a single row.
#' @param labelVector an n * 1 integer vector of class memberships
#' @param trainingSummary the TrainingSpecs list returned by the mspTrain()
#' function. This list contains --for each class-- the SPE and T2 thresholds,
#' as well the projection matrix.
#' @param ... Lazy dots for additional internal arguments
#'
#' @return an n * (p + 5) xts matrix, where the columns (p + 1):(p + 5) are the
#' monitoring statistics and corresponding fault flags, and an empty alarm
#' column
#'
#' @details
#'
#' @export
#'
#' @examples
mspMonitor <- function(observations,
                       labelVector,
                       trainingSummary,
                       ...){
  classes <- unique(labelVector)
  obsNames <- names(observations)

  # If our user accidentally sends in a vector instead of an xts matrix
  if(is.null(dim(observations))){
    observations <- matrix(observations, nrow = 1)
    warning("Observation not an xts object.", immediate. = TRUE)
  }
  classData <- cbind(labelVector, observations)

  # Apply the fault detection function to each row, accounting for the chance
  # that rows come from different classes
  dataAndFaults <- lapply(1:nrow(classData), function(i){
    faultObj <- faultDetect(threshold_object = trainingSummary[[classData[1,i]]],
                            observation = classData[-1, i])
    cbind(classData[,i], faultObj)
  })
  obsAndFlags <- do.call(rbind, dataAndFaults)
  obsAndFlags <- cbind(obsAndFlags, 0)

  # Reapply the names
  faultObjNames <- c("SPE", "SPE_Flag", "T2", "T2_Flag", "Alarm")
  names(obsAndFlags) <- append(obsNames, faultObjNames)

  obsAndFlags
}
