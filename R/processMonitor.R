#' Title
#'
#' @description Apply Adaptive-Dynamic PCA to state-specific data matrices.
#'
#' @param data A data frame with row names as a POSIX date time label
#' @param trainObs How many train observations will be used
#' @param ... Lazy dots for additional internal arguments
#' @param updateFreq How many non-flagged rows to collect before we update
#' @param faultsToTriggerAlarm the number of sequential faults needed to
#' trigger an alarm
#'
#' @return
#' @export
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#'
#' @examples processMonitor(MASS::mvrnorm(100, mu = c(0,0,0),
#'                                        Sigma = toeplitz(c(1, 0.5, 0.1))),
#'                          trainObs = 20, updateFreq = 5, var.amnt = 0.8)
processMonitor <- function(data,
                           trainObs,
                           updateFreq = cieling(0.2 * trainObs),
                           faultsToTriggerAlarm = 3,
                           ...){

  ls <- lazy_dots(...)

  # browser()

  faultObj_ls <- do.call(faultFilter,
                args = c(list(trainData = data[1:trainObs,],
                              testData = data[(trainObs + 1):nrow(data)],
                              updateFreq = updateFreq,
                              faultsToTriggerAlarm = faultsToTriggerAlarm),
                        lazy_eval(ls)))
  fault_xts <- faultObj_ls$faultObj
  obsToKeepNew <- faultObj_ls$nonAlarmedTestObs
  obsToKeep <- faultObj_ls$nonAlarmedTestObs

  while(nrow(obsToKeepNew) == updateFreq){
    # browser()
    # How many ok observations have we found so far with faultFilter?
    n <- nrow(obsToKeepNew)
    if(n < trainObs){
      trainData <- rbind(data[(n + 1):trainObs,], obsToKeep)
      }else{
        trainData <- head(obsToKeep[(n - trainObs + 1):n,], n = trainObs)
      }

    testTime <- index(obsToKeep[nrow(obsToKeep)])
    # Train on all observations after the last observation in obsToKeep. This
    # is what the date/ means for xts objects (date/ means that date and all
    # after it, this is why we remove the first row).
    testData <- data[paste0(testTime, "/")]
    faultObj_ls <- do.call(faultFilter,
                           args = c(list(trainData = trainData,
                                         testData = testData[-1,],
                                         updateFreq = updateFreq,
                                         faultsToTriggerAlarm = faultsToTriggerAlarm),
                                    lazy_eval(ls)))

    # Update the monitoring statistic values in the fault matrix. Because of
    # the adaptive nature of the algorithm, many observations which would have
    # been alarmed under the first run of faultFilter are now within normal
    # limits. We update the fault matrix with new statistic values and flags.
    fault_xts[index(faultObj_ls$faultObj),] <- faultObj_ls$faultObj
    obsToKeepNew <- faultObj_ls$nonAlarmedTestObs
    if(nrow(obsToKeepNew) != 0){
      obsToKeep <- rbind(obsToKeep, obsToKeepNew)
    }
  }
  faultNames <- colnames(fault_xts)
  faultNames[5] <- "Alarm"
  colnames(fault_xts) <- faultNames

  alarms_xts <- fault_xts[fault_xts[,5] != 0, ]
  alarmIndex <- index(alarms_xts)
  alarmObs <- data[alarmIndex]
  alarms_xts <- cbind(alarmObs, alarms_xts)


  list(FaultChecks = fault_xts,
       Non_Alarmed_Obs = obsToKeep,
       Alarms = alarms_xts)
}

