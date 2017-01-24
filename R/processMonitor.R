#' Title
#'
#' @param data A data frame with row names as a POSIX date time label
#' @param trainObs How many train observations will be used
#' @param ...
#' @param updateFreq How many non-flagged rows to collect before we update
#' @param faultsToTriggerAlarm
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
    n <- nrow(obsToKeepNew)
    if(n < trainObs){
      trainData <- rbind(data[(n + 1):trainObs,], obsToKeep)
      }else{
        trainData <- head(obsToKeep[(n - trainObs + 1):n,], n = trainObs)
      }

    testTime <- index(obsToKeep[nrow(obsToKeep)])
    faultObj_ls <- do.call(faultFilter,
                           args = c(list(trainData = trainData,
                                         testData = data[paste0(testTime, "/")],
                                         updateFreq = updateFreq,
                                         faultsToTriggerAlarm = faultsToTriggerAlarm),
                                    lazy_eval(ls)))

    fault_xts[index(faultObj_ls$faultObj),] <- faultObj_ls$faultObj
    obsToKeepNew <- faultObj_ls$nonAlarmedTestObs
    if(nrow(obsToKeepNew) != 0){
      obsToKeep <- rbind(obsToKeep, obsToKeepNew)
    }
  }
  faultNames <- colnames(fault_xts)
  faultNames[5] <- "Alarm"
  colnames(fault_xts) <- faultNames

  # browser()

  # alarms_xts1 <- faultAlarm(fault_xts,
  #                           faultsToTrigger = faultsToTriggerAlarm,
  #                           faultObs = NULL)
  alarms_xts <- faultObj_ls$alarmedTestObs

  list(FaultChecks = fault_xts,
       Non_Flagged_Obs = obsToKeep,
       Alarms = alarms_xts)
}

