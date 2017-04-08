#' Real-Time Process Monitoring Function
#'
#' @description Monitor and flag (if necessary) incoming multivariate process
#'   observations.
#'
#' @param observations an n x p xts matrix. For real-time monitoring via a
#'   script within a batch file, n = 1, so this must be a 1 x p matrix. If lags
#'   were included at the training step, then these observations will also have
#'   lagged features.
#' @param labelVector an n x 1 integer vector of class memberships
#' @param trainingSummary the TrainingSpecs list returned by the mspTrain()
#'   function. This list contains---for each class---the SPE and T2 thresholds,
#'   as well the projection matrix.
#' @param ... Lazy dots for additional internal arguments
#'
#' @return An n x (p + 5) xts matrix, where the last five columns are:
#'   \itemize{
#'     \item{SPE -- }{the SPE statistic value for each observation
#'       in "observations"}
#'     \item{SPE_Flag -- }{a vector of SPE indicators recording 0 if the test
#'       statistic is less than or equal to the critical value passed through
#'       from the threshold object}
#'     \item{T2 -- }{the T2 statistic value for each observation in
#'       "observations"}
#'     \item{T2_Flag -- }{a vector of T2 fault indicators, defined like
#'       SPE_Flag}
#'     \item{Alarm -- }{a column indicating if there have been three flags in a
#'       row for either the SPE or T2 monitoring statistics or both. Alarm
#'       states are as follows: 0 = no alarm, 1 = Hotelling's T2 alarm, 2 =
#'       Squared Prediction Error alarm, and 3 = both alarms.}
#'   }
#'
#' @details This function is designed to be run at specific time intervals
#'   (e.g.every 10 seconds, 30 seconds, 1 minute, 5 minutes, 10 minutes) through
#'   a scheduled operating script which calls this function and mspWarning(). We
#'   expect this script to be set up in Windows "Task Scheduler" or Macintosh OX
#'   "launchd" application suites. This function takes in the specific
#'   observations to monitor and their class memberships (if any) and returns an
#'   xts matrix of these observation columns concatenated with their monitoring
#'   statistic values, flag statuses, and an empty alarm column. Users should
#'   then append these rows onto a previously existing matrix of daily
#'   observations. The mspWarning() function will then take in the daily
#'   observation xts matrix with updated rows returned by this function and
#'   check the monitoring statistic flag indicators to see if an alarm status
#'   has been reached. For further details, see the mspWarning() function.
#'
#' This function calls the faultDetect() function, and requires the training
#' information returned by the mspTrain function. This function will return
#' the xts matrix necessary for the mspWarning() function.
#'
#' @export
#'
#' @importFrom plyr ldply
#' @importFrom xts lag.xts
#' @importFrom zoo index
#'
#' @examples
#' data("normal_switch_xts")
#' # The state values are recorded in the first column.
#' n <- nrow(normal_switch_xts)
#' nTrainObs <- floor(0.4 * n)
#'
#' # Calculate the training summary, but save five observations for monitoring.
#' trainResults_ls <- mspTrain(data = normal_switch_xts[1:(n - 5), -1],
#'                             labelVector = normal_switch_xts[1:(n - 5), 1],
#'                             trainObs = nTrainObs,
#'                             lagsIncluded = c(0, -1))
#'
#' # While training, we included 1 lag (the default), so we will also lag the
#' # observations we will test.
#' testObs <- normal_switch_xts[(n - 6):n, -1]
#' testObs <- xts:::lag.xts(testObs, 0:1)
#' testObs <- testObs[-1,]
#' testObs <- cbind(normal_switch_xts[(n - 5):n, 1], testObs)
#'
#' mspMonitor(observations = testObs[, -1],
#'            labelVector = testObs[, 1],
#'            trainingSummary = trainResults_ls$TrainingSpecs)
mspMonitor <- function(observations,
                       labelVector,
                       trainingSummary,
                       ...){

  # browser()

  classes <- unique(labelVector)
  names(classes) <- LETTERS[1:length(classes)]

  # If our user accidentally sends in a vector instead of an xts matrix
  if(is.null(dim(observations))){
    observations <- matrix(observations, nrow = 1)
    warning("Observation not an xts object.", immediate. = TRUE)
  }

  if(is.vector(labelVector)){
    labelVector <- matrix(labelVector, ncol = 1)
  }
  classData <- cbind(labelVector, observations)

  # Apply the fault detection function to each row, accounting for the chance
  # that rows come from different classes
  dataAndFaults <- lapply(1:nrow(classData), function(i){
    # Subset out the appropriate training information for that observation
    train_ls <- trainingSummary[names(trainingSummary) %in% classData[i,1]][[1]]
    # Centre and scale the observation
    centredObs <- as.matrix(classData[i, -1] - train_ls$muTrain)
    scaledObs <- centredObs %*% train_ls$RootPrecisTrain
    # Now apply fault detection
    faultObj <- faultDetect(threshold_object = train_ls,
                            observation = scaledObs)
    # Bind the flagging info to the original observation. We need this as a
    # data frame because plyr::laply won't preserve the index information at
    # binding.
    xx <- cbind(classData[i,], faultObj)
    data.frame(dateTime = index(xx), xx[,-1])
  })
  # Use ldply instead of do.call(rbind, .) becuase rbind.xts() can't handle the
  # object size necessary to bind a large list.
  obsAndFlags <- ldply(dataAndFaults)
  obsAndFlags <- xts(obsAndFlags[,-1], order.by = obsAndFlags[,1])
  obsAndFlags <- cbind(obsAndFlags, NA)
  colnames(obsAndFlags)[ncol(obsAndFlags)] <- "Alarm"

  obsAndFlags
}
