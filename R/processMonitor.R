#' Adaptive Process Training
#'
#' @description Apply Adaptive-Dynamic PCA to state-specific data matrices.
#'
#' @param data An xts data matrix
#' @param trainObs How many train observations will be used
#' @param ... Lazy dots for additional internal arguments
#' @param updateFreq How many non-flagged rows to collect before the function
#' updates
#' @param faultsToTriggerAlarm the number of sequential faults needed to
#' trigger an alarm
#'
#' @return A list of the following components: FaultChecks = a class specific
#' xts data matrix containing the SPE monitoring statistic and corresponding
#' logical flagging indicator, the Hotelling's T2 monitoring statitisic and
#' corresponding logical flagging indicator, and the Alarm indicator.
#' Non_Alarmed_Obs = a class specific xts data matrix of all the observations
#' with alarm states equal to 0. Alarms = a class-specific xts data matrix of
#' the features and specific alarms for Alarmed observations, where the alarm
#' code is as follows: 0 = no alarm, 1 = Hotelling's T2 alarm, 2 = Squared
#' Prediction Error alarm, and 3 = both alarms. trainSpecs = the threshold
#' object returned by the internal threshold() function. See this function's
#' help file for more details.
#'
#' @details This function is the class-specific implementation of the Adaptive-
#' Dynamic PCA described in the details of the mspTrain function. See that
#' function's help file for further details.
#'
#' This internal function is called by mspTrain(). This function calls the
#' faultFilter() function.
#'
#' @export
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom utils head
#'
#' @examples
#' data("normal_switch_xts")
#' # Select the data under state 1
#' data <- normal_switch_xts[normal_switch_xts[,1] == 1]
#' nTrainObs <- floor(0.4 * nrow(data))
#'
#' # Remove the now unnecesary state column
#' featureCols <- data[,-1]
#'
#' processMonitor(data = featureCols, trainObs = nTrainObs)
processMonitor <- function(data,
                           trainObs,
                           updateFreq = ceiling(0.2 * trainObs),
                           faultsToTriggerAlarm = 3,
                           ...){

  ls <- lazy_dots(...)

  faultObj_ls <- do.call(faultFilter,
                args = c(list(trainData = data[1:trainObs,],
                              testData = data[(trainObs + 1):nrow(data), ],
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
    if(nrow(testData) == 1) break
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
  # FaultChecks
  faultNames <- colnames(fault_xts)
  faultNames[5] <- "Alarm"
  colnames(fault_xts) <- faultNames

  # Alarms
  alarms_xts <- fault_xts[fault_xts[,5] != 0, ]
  alarmIndex <- index(alarms_xts)
  alarmObs <- data[alarmIndex]
  alarms_xts <- cbind(alarmObs, alarms_xts)

  # thresholdObj: Training Thresholds and Projection Matrix
  trainSpecs <- faultObj_ls$trainSpecs

  list(FaultChecks = fault_xts,
       Non_Alarmed_Obs = obsToKeep,
       Alarms = alarms_xts,
       TrainingSpecs = trainSpecs)
}
