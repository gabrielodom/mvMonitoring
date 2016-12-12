#' Title
#'
#' @param data A data frame with row names as a POSIX date time label
#' @param trainObs How many train observations will be used
#' @param ...
#' @param updateFreq How many non-flagged rows to collect before we update
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
                           ...){

  ls <- lazy_dots(...)
  faultObj_ls <- faultFilter(trainData = data[1:trainObs,],
                             testData = data[(trainObs + 1):nrow(data)],
                             updateFreq = updateFreq)
  fault_xts <- faultObj_ls$faultObj
  obsToKeep <- obsToKeepNew <- faultObj_ls$nonFlaggedTestObs

  while(nrow(obsToKeepNew) == updateFreq){
    n <- nrow(obsToKeepNew)
    if(n < trainObs){
      trainData <- rbind(data[(n + 1):trainObs,], obsToKeep)
      }else{
        trainData <- head(obsToKeep[(n - trainObs + 1):n,], n = trainObs)
      }
    testTime <- index(obsToKeep[nrow(obsToKeep)])
    faultObj_ls <- faultFilter(trainData = trainData,
                               testData = data[paste0(testTime, "/")],
                               updateFreq = updateFreq)
    fault_xts <- faultObj_ls$faultObj
    obsToKeepNew <- faultObj_ls$nonFlaggedTestObs
    obsToKeep <- rbind(obsToKeep, obsToKeepNew)
  }

  # alarm_function()

  list(FaultChecks = faultObj,
       Non_Flagged_Obs = unflaggedObs)
}

