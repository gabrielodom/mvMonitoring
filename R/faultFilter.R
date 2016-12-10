
#' Title
#'
#' @param trainData
#' @param testData
#' @param updateFreq
#' @param ...
#'
#' @return
#' @export
#'
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval lazy_dots
#' @importFrom zoo index
#' @importFrom xts xts
#'
#' @examples
faultFilter <- function(trainData,
                        testData,
                        updateFreq,
                        ...){

  browser()

  ls <- lazy_dots(...)
  muTrain <- colMeans(trainData)
  sigmaTrain <- cov(trainData)
  sigmaInvTrain <- solve(sigmaTrain)

  scaledTrainData <- scale(trainData)


  pcaObj <- do.call(pca, args = c(list(data = scaledTrainData), lazy_eval(ls)))
  thresholdObj <- do.call(threshold, args = c(list(pca_object = pcaObj),
                                              lazy_eval(ls)))
  scaledTest <- as.matrix(testData - muTrain) %*% sigmaInvTrain
  faultObj <- lapply(1:nrow(scaledTest), function(i){
    do.call(faultDetect,
            args = c(list(threshold_object = thresholdObj,
                          observation = scaledTest[i,]),
                     lazy_eval(ls)))
  })
  faultObj <- do.call(rbind, faultObj)
  faultObj <- xts(faultObj, order.by = index(testData))

  nonSPEFlaggedObs <- faultObj[faultObj[,2] == FALSE, ]
  nonFlaggedObs <- nonSPEFlaggedObs[nonSPEFlaggedObs[,4] == FALSE, ]
  keptObsIndex <- head(index(nonFlaggedObs), n = updateFreq)

  keptObs <- testData[keptObsIndex]

  list(faultObj = faultObj,
       nonFlaggedTestObs = keptObs)
}
