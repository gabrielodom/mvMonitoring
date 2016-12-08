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
  browser()
  ls <- lazy_dots(...)
  faultObj <- data.frame(SPE = integer(0),
                         SPE_flag = logical(0),
                         T2 = integer(0),
                         T2_flag = logical(0))
  unflaggedObs <- data[0,]
  n <- nrow(unflaggedObs)

  while(nrow(faultObj) < (nrow(data) - trainObs)){
    browser()
    if(n < trainObs){
      trainData <- rbind(data[(1 + n):trainObs,], unflaggedObs)
      }else{
        trainData <- unflaggedObs[(n - trainObs):n,]
        }
    muTrain <- colMeans(trainData)
    sigmaTrain <- cov(trainData)
    sigmaInvTrain <- solve(sigmaTrain)
    scaledTrainData <- scale(trainData)

    iter <- 1
    continueSPE <- TRUE
    continueT2 <- TRUE

    while(continueSPE && continueT2){
      # browser()
      pcaObj <- do.call(pca,
                        args = c(list(data = scaledTrainData), lazy_eval(ls)))
      thresholdObj <- do.call(threshold,
                              args = c(list(pca_object = pcaObj),
                                       lazy_eval(ls)))
      scaledObs <- sigmaInvTrain %*%
                      t(as.matrix(data[(trainObs + iter),]) - muTrain)
      faultObj[iter,] <- do.call(faultDetect,
                                 args = c(list(threshold_object = thresholdObj,
                                               observation = t(scaledObs)),
                                          lazy_eval(ls)))

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
      iter <- iter + 1
    }
    # browser()
    rownames(faultObj) <-
                    rownames(data[(trainObs + 1):(trainObs + nrow(faultObj)),])
    # dplyr::filter does not preserve row names, and Hadley has no intention of
    #     fixing this problem.
    nonSPEFlaggedObs <- faultObj[faultObj$SPE_flag == FALSE, ]
    nonFlaggedObs <- nonSPEFlaggedObs[nonSPEFlaggedObs$T2_flag == FALSE, ]

    unflaggedObs[(n + 1):(n + nrow(nonFlaggedObs)),] <-
                               data[rownames(data) %in% rownames(nonFlaggedObs),]
  }


}

