
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

  ls <- lazy_dots(...)
  muTrain <- colMeans(trainData)
  sigmaTrain <- cov(trainData)
  stdDevs <- sqrt(diag(sigmaTrain))
  precisRootMat <- diag(1 / stdDevs, ncol = ncol(sigmaTrain))

  scaledTrainData <- scale(trainData)
  # muTrain_mat <- rep(1, nrow(trainData)) %*% t(muTrain)
  # scaledTrainData2 <- (trainData - muTrain_mat) %*% precisRootMat
  # scaledTrainData2 <- xts(scaledTrainData2, order.by = index(trainData))
  # names(scaledTrainData2) <- names(trainData)
  # plot.xts(scaledTrainData2$x)

  pcaObj <- do.call(pca, args = c(list(data = scaledTrainData), lazy_eval(ls)))
  thresholdObj <- do.call(threshold, args = c(list(pca_object = pcaObj),
                                              lazy_eval(ls)))

  muTrain_mat <- rep(1, nrow(testData)) %*% t(muTrain)
  scaledTest <- as.matrix(testData - muTrain_mat) %*% precisRootMat
  scaledTest <- xts(scaledTest, order.by = index(testData))
  names(scaledTest) <- names(testData)

  faultObj <- lapply(1:nrow(scaledTest), function(i){
    do.call(faultDetect,
            args = c(list(threshold_object = thresholdObj,
                          observation = scaledTest[i,]),
                     lazy_eval(ls)))
  })
  faultObj <- do.call(rbind, faultObj)
  faultObj <- xts(faultObj, order.by = index(testData))

  nonFlaggedObs <- faultObj[faultObj[,2] == FALSE & faultObj[,4] == FALSE, ]
  keptObsIndex <- head(index(nonFlaggedObs), n = updateFreq)

  keptObs <- testData[keptObsIndex]

  object <- list(faultObj = faultObj,
                 nonFlaggedTestObs = keptObs)
  class(object) <- "faultDF"
  object
}
