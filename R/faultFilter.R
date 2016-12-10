#' Title
#'
#' @param updateFreq
#' @param ...
#' @param trainData
#' @param testData
#'
#' @return
#' @export
#'
#' @importFrom lazyeval lazy_eval
#'
#' @examples
faultFilter <- function(trainData,
                        testData,
                        updateFreq,
                        ...){
  browser()
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
                          observation = scaledObs[i,]),
                     lazy_eval(ls)))
  })

  nonflaggedSPEObs <- nrow(faultObj[faultObj$SPE_flag == FALSE,])
  continueSPE <- nonflaggedSPEObs < updateFreq
  nonflaggedT2Obs <- nrow(faultObj[faultObj$T2_flag == FALSE,])
  continueT2 <- nonflaggedT2Obs < updateFreq

  if(nrow(faultObj) >= 3 && all(faultObj[(iter - 2):iter, 2]) == TRUE){
    warning("SPE detects process fault.", immediate. = TRUE)
  }
  if(nrow(faultObj) >= 3 && all(faultObj[(iter - 2):iter, 4]) == TRUE){
    warning("T2 detects process fault.", immediate. = TRUE)
  }


  if(trainObs + iter == nrow(data)){
    print("End of Data Frame Reached")
    break()
  }
}
