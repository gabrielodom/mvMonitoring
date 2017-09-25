#' T-Squared Contribution Plots
#' @description Plots a variation of the Hotelling's T-squared statistic to
#' visualize the contribution of each variable to a fault.
#'
#' @param trainData an xts data matrix containing the training observations
#' @param trainLabel Class labels for the training data as a logical (two states
#' only) or finite numeric (two or more states) vector or matrix column (not from
#' a data frame) with length equal to the number of rows in ``data." For data with
#' only one state, this will be a vector of 1s.
#' @param newData an xts data matrix containing the new observation
#' @param newLabel the class label for the new observation
#' @param trainT2 the Hotelling's T-squared values corresponding to the
#' newLabel state calculated by mspTrain using the full training data with
#' all variables included
#' @param newT2 the Hotelling's T-squared value returned by mspMonitor using
#' the full new observation with all variables included
#' @param var.amnt the energy proportion to preserve in the projection, which
#' dictates the number of principal components to keep
#' @param trainObs the number of observations upon which to train the algorithm.
#' This will be split based on class information by a priori class membership
#' proportions.
#' @export
#' @importFrom graphics abline
#' @importFrom graphics arrows
#' @importFrom graphics hist
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' # Create some data
#' dataA1 <- mspProcessData(faults = "B1")
#' traindataA1 <- dataA1[1:8567,]
#'
#' # Train on the data that should be in control
#' trainResults <- mspTrain(traindataA1[,-1], traindataA1[,1], trainObs = 4320)
#'
#'
#' # Lag an out of control observation
#' testdataA1 <- dataA1[8567:8568,-1]
#' testdataA1 <- lag.xts(testdataA1,0:1)
#' testdataA1 <- testdataA1[-1,]
#' testdataA1 <- cbind(dataA1[8568,1],testdataA1)
#'
#' # Monitor this observation
#' monitorResults <- mspMonitor(observations = testdataA1[,-1],
#'                              labelVector = testdataA1[,1],
#'                              trainingSummary = trainResults$TrainingSpecs)
#'
#'
#' tD <- traindataA1[,-1]
#' tL <- traindataA1[,1]
#' nD <- testdataA1[,-1]
#' nL <- testdataA1[,1]
#' tO <- 4320
#' vA <- 0.95
#' nT2 <- monitorResults$T2
#' tT2 <- trainResults$TrainingSpecs[[nL]]$T2
#'
#' mspT2Plot(tD,tL,tT2,nD,nL,nT2,tO,vA)
#' }

mspT2Plot <- function(trainData,
                      trainLabel,
                      trainT2,
                      newData,
                      newLabel,
                      newT2,
                      trainObs,
                      var.amnt){

  # Create reduced training data sets
  reduced_train_data <- list()
  for(i in 1:ncol(trainData)){
    reduced_train_data[[i]] <- trainData[,-i]
  }

  reduced_new_data <- list()
  for(i in 1:(ncol(newData)/2)){
    reduced_new_data[[i]] <- newData[,-c(i, i + (ncol(newData)/2))]
  }

  # mspTrain each reduced training data set
  reduced_train <- list()
  for(i in 1:length(reduced_train_data)){
    reduced_train[[i]] <- mspTrain(data = reduced_train_data[[i]],
                                   labelVector = trainLabel,
                                   trainObs = trainObs,
                                   var.amnt = var.amnt)
  }

  #mspMonitor each reduced new data set
  reduced_monitor <- list()
  for(i in 1:length(reduced_new_data)){
    reduced_monitor[[i]] <- mspMonitor(observations = reduced_new_data[[i]],
                                       labelVector = newLabel,
                                       trainingSummary = reduced_train[[i]]$TrainingSpecs)
  }

  # Calculate the new training T2j's?
  trainT2j <- list()
  for(i in 1:length(reduced_train)){
    trainT2j[[i]] <- trainT2 - reduced_train[[i]]$TrainingSpecs[[newLabel]]$T2
  }

  # Calculate the new monitor T2j
  monitorT2j <- vector()
  for(i in 1:length(reduced_monitor)){
    monitorT2j[i] <- newT2 - reduced_monitor[[i]]$T2
  }

  # Calculate some ranges for plotting
  minT2 <- vector()
  maxT2 <- vector()
  for(j in 1:length(reduced_train)){
    minT2[j] <- min(trainT2j[[j]])
    maxT2[j] <- max(trainT2j[[j]])
  }

  for(i in 1:length(trainT2j)){
    if(monitorT2j[i] >= minT2[i] && monitorT2j[i] <= maxT2[i]){
      hist(trainT2j[[i]], breaks = 'FD', xlab = bquote(T[.({-i})]^2),
           main = bquote(Distribution~of~T[.({-i})]^2))
      abline(v = monitorT2j[i], lty = 2, col = 'red')
    }
    else if(monitorT2j[i] > maxT2[i]){
      hist(trainT2j[[i]], breaks = 'FD', xlab = bquote(T[.({-i})]^2),
           main = bquote(Distribution~of~T[.({-i})]^2))
      arrows(x0 = maxT2[i] - .2*diff(c(minT2[i], maxT2[i])),
             x1 = maxT2[i], y0 = 50, y1=50, lty =2, col = 'red')
    }
    else if(monitorT2j[i] < minT2[i]){
      hist(trainT2j[[i]], breaks = 'FD', xlab = bquote(T[.({-i})]^2),
           main = bquote(Distribution~of~T[.({-i})]^2))
      arrows(x0 = minT2[i] + .2*diff(c(minT2[i], maxT2[i])),
             x1 = minT2[i], y0 = 50, y1=50, lty =2, col = 'red')
    }
  }
list2 <- list()
# Make some output
for(i in 1:length(trainT2j)){
list1 <- list()
list1[[1]] <- trainT2j[[i]]
list1[[2]] <- monitorT2j[[i]]

list2[[i]] <- list1
}
list2
}
