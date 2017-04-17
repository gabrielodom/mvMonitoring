#' Adaptive Process Training
#'
#' @description Apply Adaptive-Dynamic PCA to state-specific data matrices.
#'
#' @param data An xts data matrix
#' @param trainObs The number of training observations to be used
#' @param ... Lazy dots for additional internal arguments
#' @param updateFreq The number of non-flagged observations to collect before
#'   the function updates. Defaults to half as many observations as the number
#'   of training observations.
#' @param faultsToTriggerAlarm The number of sequential faults needed to trigger
#'   an alarm. Defaults to 5.
#'
#' @return A list with the following components:
#'   \itemize{
#'     \item{FaultChecks -- }{a class-specific xts flagging matrix with the
#'       same number of rows as "data". This flag matrix has the following five
#'       columns:
#'         \itemize{
#'           \item{SPE -- }{the SPE statistic value for each observation in
#'             "data"}
#'           \item{SPE_Flag -- }{a vector of SPE indicators recording 0 if the
#'             test statistic is less than or equal to the critical value
#'             passed through from the threshold object}
#'           \item{T2 -- }{the T2 statistic value for each observation in
#'             "data"}
#'           \item{T2_Flag -- }{a vector of T2 fault indicators, defined like
#'             SPE_Flag}
#'           \item{Alarm -- }{a column indicating if there have been three flags
#'             in a row for either the SPE or T2 monitoring statistics or both.
#'             Alarm states are as follows: 0 = no alarm, 1 = Hotelling's T2
#'             alarm, 2 = Squared Prediction Error alarm, and 3 = both alarms.}
#'         }
#'       }
#'     \item{Non_Alarmed_Obs -- }{a class-specific xts data matrix of all the
#'       non-alarmed observations (observations with alarm state equal to 0)}
#'     \item{Alarms -- }{a class-specific xts data matrix of the features and
#'       specific alarms of Alarmed observations, where the alarm codes are
#'       listed above}
#'     \item{trainSpecs -- }{a threshold object returned by the internal
#'       threshold() function. See the threshold() function's help file for
#'       more details.}
#'   }
#'
#' @details This function is the class-specific implementation of the Adaptive-
#'   Dynamic PCA described in the details of the mspTrain() function. See
#'   the mspTrain() function's help file for further details.
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
#' nrml <- mspProcessData(faults = "NOC")
#' data <- nrml[nrml[,1] == 1]
#'
#' processMonitor(data = data[,-1], trainObs = 672)
#'
processMonitor <- function(data,
                           trainObs,
                           updateFreq = ceiling(0.5 * trainObs),
                           faultsToTriggerAlarm = 5,
                           ...){

  ls <- lazy_dots(...)

  # browser()

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
    testData <- testData[-1,]
    if(nrow(testData) == 0) break
    faultObj_ls <- do.call(faultFilter,
                           args = c(list(trainData = trainData,
                                         testData = testData,
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
