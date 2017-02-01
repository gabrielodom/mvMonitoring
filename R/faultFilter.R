#' Process Fault Filtering
#'
#' @description  Flag and filter out observations beyond normal parameters,
#' then return the observations within normal operating conditions.
#'
#' @param trainData An xts data matrix of initial training observations
#' @param testData The data not included in the training data set
#' @param updateFreq How many observations from the test data matrix will be
#' returned to update the training data matrix and move it forward?
#' @param ... Lazy dots for additional internal arguments
#' @param faultsToTriggerAlarm Specifies how many sequential faults will cause
#' an alarm to trigger. Defaults to 3.
#'
#' @return A list of class "fault_ls" with the following: faultObj = an xts
#' flagging matrix with the same number of rows as "testData". This flag matrix
#' has the following five columns: the SPE test statistic for each observation
#' in "testData", an SPE indicator recording 0 if the test statistic is less
#' than or equal to the critical value passed through from the threshold object,
#' a T2 test statistic, a similar T2 indicator, and a final column indicating
#' if there have been three flags in a row for either the SPE or T2 monitoring
#' statistics or both. nonAlarmedTestObs = an xts matrix of all the rows of the
#' training data which were not alarmed. trainSpecs = the threshold object
#' returned by the internal threshold() function. See this function's help file
#' for more details.
#'
#' @details This function is essentially a wrapper function to call and organize
#' the output from these other internal functions: faultDetect(), threshold(),
#' and pca(). It is applied over a rolling window, with observation width equal
#' to updateFreq, of the larger full data matrix via the processMonitor()
#' function, wherein the testing and training data sets move forward in time
#' across the entire data matrix.
#'
#' This internal function is called by processMonitor().
#'
#' @export
#'
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval lazy_dots
#' @importFrom zoo index
#' @importFrom xts xts
#' @importFrom stats cov
#'
#' @examples
faultFilter <- function(trainData,
                        testData,
                        updateFreq,
                        faultsToTriggerAlarm,
                        ...){

  # browser()

  ls <- lazy_dots(...)
  muTrain <- colMeans(trainData)
  sigmaTrain <- cov(trainData)
  stdDevs <- sqrt(diag(sigmaTrain))
  precisRootMat <- diag(1 / stdDevs, ncol = ncol(sigmaTrain))

  scaledTrainData <- scale(trainData)

  # Call the pca.R file, passing in the scaled training data matrix and the
  # proportion of energy to preserve in the projection (defaults to 95%)
  pcaObj <- do.call(pca, args = c(list(data = scaledTrainData), lazy_eval(ls)))

  # Call the threshold.R file, passing in the object returned by the pca call
  # previous.
  thresholdObj <- do.call(threshold, args = c(list(pca_object = pcaObj),
                                              lazy_eval(ls)))

  muTrain_mat <- rep(1, nrow(testData)) %*% t(muTrain)
  scaledTest <- as.matrix(testData - muTrain_mat) %*% precisRootMat
  scaledTest <- xts(scaledTest, order.by = index(testData))
  colnames(scaledTest) <- colnames(testData)

  # We now apply the faultDetect function down each row of the scaled test data
  # set. We then return it to its form as an xts matrix.
  faultObj <- lapply(1:nrow(scaledTest), function(i){
    do.call(faultDetect,
            args = c(list(threshold_object = thresholdObj,
                          observation = scaledTest[i,]),
                     lazy_eval(ls)))
  })
  faultObj <- do.call(rbind, faultObj)
  faultObj <- xts(faultObj, order.by = index(testData))

  # This bit will find the observations which have not been flagged by either
  # statistic. However, we need to find and report the alarmed observations as
  # well. We first add a column for SPE and T2 alarm status.
  faultObj <- cbind(faultObj, rep(0, nrow(faultObj)))
  # nonFlaggedObs <- faultObj[faultObj[,2] == FALSE & faultObj[,4] == FALSE, ]

  # Now we iterate through the faultObj xts object by row, checking when we see
  # 3 (the default value of faultsToTriggerAlarm) flagged observations in a row
  alarmCheck <- rep(1, faultsToTriggerAlarm)
  # Alarm code: 1 = T2 alarm; 2 = SPE alarm; 3 = both
  for(i in faultsToTriggerAlarm:nrow(faultObj)){
    x1 <- as.vector(faultObj[(i - faultsToTriggerAlarm + 1):i, 4])
    if(identical(x1, alarmCheck)){
      faultObj[i,5] <- 1
    }
    x2 <- as.vector(faultObj[(i - faultsToTriggerAlarm + 1):i, 2])
    if(identical(x2, alarmCheck)){
      faultObj[i,5] <- faultObj[i,5] + 2
    }
  }
  # We seperate out the non-alarmed observations, and we keep as many as we
  # need to update the algorithm.
  nonAlarmedObs <- faultObj[faultObj[,5] == 0, ]
  keptObsIndex <- head(index(nonAlarmedObs), n = updateFreq)
  keptObs <- testData[keptObsIndex]

  # The faultObj is an xts with the same number of observations as testData.
  object <- list(faultObj = faultObj,
                 nonAlarmedTestObs = keptObs,
                 trainSpecs = thresholdObj)
  class(object) <- "fault_ls"
  object
}
