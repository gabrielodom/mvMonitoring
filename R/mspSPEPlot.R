#' Squared Prediction Error Contribution Plots
#' @description Plots a variation of the squared prediction error (SPE)
#' statistic to visualize the contribution of each variable to a fault.
#' @param trainData an xts data matrix containing the training observations
#' @param trainLabel Class labels for the training data as a logical (two states
#' only) or finite numeric (two or more states) vector or matrix column (not from
#' a data frame) with length equal to the number of rows in ``data." For data with
#' only one state, this will be a vector of 1s.
#' @param newData an xts data matrix containing the new observation
#' @param newLabel the class label for the new observation
#' @param trainSPE the SPE values corresponding to the
#' newLabel state calculated by mspTrain using the full training data with
#' all variables included
#' @param newSPE the SPE value returned by mspMonitor using
#' the full new observation with all variables included
#' @param var.amnt the energy proportion to preserve in the projection, which
#' dictates the number of principal components to keep
#' @param trainObs the number of observations upon which to train the algorithm.
#' This will be split based on class information by a priori class membership
#' proportions.
#' @export
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
#' tO <- trainObs
#' vA <- 0.95
#' nSPE <- monitorResults$SPE
#' tSPE <- trainResults$TrainingSpecs[[nL]]$SPE
#'
#' mspSPEPlot(tD,tL,tSPE,nD,nL,nSPE,tO,vA)
#' }

mspSPEPlot <- function(trainData,
                       trainLabel,
                       trainSPE,
                       newData,
                       newLabel,
                       newSPE,
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

  # Calculate the new training SPEj's
  trainSPEj <- list()
  for(i in 1:length(reduced_train)){
    trainSPEj[[i]] <- trainSPE - reduced_train[[i]]$TrainingSpecs[[newLabel]]$SPE
  }

  # Calculate the new monitor SPEj
  monitorSPEj <- vector()
  for(i in 1:length(reduced_monitor)){
    monitorSPEj[i] <- newSPE - reduced_monitor[[i]]$SPE
  }

  # Calculate some ranges for plotting
  minSPE <- vector()
  maxSPE <- vector()
  for(j in 1:length(reduced_train)){
    minSPE[j] <- min(trainSPEj[[j]])
    maxSPE[j] <- max(trainSPEj[[j]])
  }

  for(i in 1:length(trainSPEj)){
    if(monitorSPEj[i] >= minSPE[i] && monitorSPEj[i] <= maxSPE[i]){
      hist(trainSPEj[[i]], breaks = 'FD', xlab = bquote(SPE[.(-i)]), main = bquote(Distribution~of~SPE[.(-i)]))
      abline(v = monitorSPEj[i], lty = 2, col = 'red')
    }
    else if(monitorSPEj[i] > maxSPE[i]){
      hist(trainSPEj[[i]], breaks = 'FD', xlab = bquote(SPE[.(-i)]), main = bquote(Distribution~of~SPE[.(-i)]))
      arrows(x0 = maxSPE[i] - 0.20*diff(c(minSPE[i],maxSPE[i])), x1 = maxSPE[i], y0 = 50, y1=50, lty =2, col = 'red')
    }
    else if(monitorSPEj[i] < minSPE[i]){
      hist(trainSPEj[[i]], breaks = 'FD', xlab = bquote(SPE[.(-i)]), main = bquote(Distribution~of~SPE[.(-i)]))
      arrows(x0 = minSPE[i] + 0.20*diff(c(minSPE[i],maxSPE[i])), x1 = minSPE[i], y0 = 50, y1=50, lty =2, col = 'red')
    }
  }

}
