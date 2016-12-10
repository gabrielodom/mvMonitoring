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
  faultObj <- data.frame(SPE = integer(0),
                         SPE_flag = logical(0),
                         T2 = integer(0),
                         T2_flag = logical(0))
  unflaggedObs <- data[0,]

  while(nrow(faultObj) < (nrow(data) - trainObs)){

    n <- nrow(unflaggedObs)
    if(n < trainObs){
      trainData <- rbind(data[(1 + n):trainObs,], unflaggedObs)
      }else{
        trainData <- unflaggedObs[(n - trainObs):n,]
        }

    iter <- 1 + nrow(faultObj)
    continueSPE <- TRUE
    continueT2 <- TRUE

    while(continueSPE && continueT2){
      # FUNCTION HERE
      iter <- iter + 1
    }
    # if the number of observations kept returned by the above function is less
    # than updateFreq, then we know that there are no more good observations
    # left in that section of testData. We then reset trainData to include the
    # good observations, and start the next draw of n = trainObs rows from the
    # remaining observations

    if(nrow(faultObj) >= 3 && all(faultObj[(iter - 2):iter, 2]) == TRUE){
      warning("SPE detects process fault.", immediate. = TRUE)
    }
    if(nrow(faultObj) >= 3 && all(faultObj[(iter - 2):iter, 4]) == TRUE){
      warning("T2 detects process fault.", immediate. = TRUE)

    rownames(faultObj) <-
                    rownames(data[(trainObs + 1):(trainObs + nrow(faultObj)),])

    newObs <- !(rownames(nonFlaggedObs) %in% rownames(unflaggedObs))
    newObs <- rownames(nonFlaggedObs[newObs,])
    unflaggedObs[(n + 1):(n + length(newObs)),] <-
                               data[rownames(data) %in% newObs,]

    }
  }
  list(FaultChecks = faultObj,
       Non_Flagged_Obs = unflaggedObs)
}

