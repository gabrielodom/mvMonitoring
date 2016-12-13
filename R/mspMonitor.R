


#' Title
#'
#' @param data
#' @param labelVector
#' @param trainObs
#' @param updateFreq
#' @param faultsToTriggerAlarm
#' @param ...
#'
#' @return
#' @export
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval lazy_eval
#'
#' @examples
mspMonitor <- function(data,
                       labelVector,
                       trainObs,
                       updateFreq = cieling(0.2 * trainObs),
                       faultsToTriggerAlarm = 3,
                       ...){

  # browser()

  ls <- lazy_dots(...)
  classes <- unique(labelVector)
  classData <- cbind(labelVector, data)
  data_ls <- lapply(1:length(classes), function(i){
    data_df <- classData[classData[,1] == classes[i],]
    data_df[, -1]
  })
  names(data_ls) <- classes

  monitorResults <- lapply(classes, function(i){
    do.call(processMonitor,
            args = c(list(data = data_ls[[i]],
                          trainObs = trainObs,
                          updateFreq = updateFreq,
                          faultsToTriggerAlarm = faultsToTriggerAlarm),
                     lazy_eval(ls)))
  })

  names(monitorResults) <- classes
  monitorResults
}
