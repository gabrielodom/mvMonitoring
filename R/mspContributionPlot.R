#' Contribution Plots
#'
#' @description This function plots the contribution value for each
#' variable of a newly monitored observation and compares them to the
#' contribution values of the training data.
#'
#' @param trainData an xts data matrix containing the training observations
#' @param trainLabel Class labels for the training data as a logical (two states
#' only) or finite numeric (two or more states) vector or matrix column (not from
#' a data frame) with length equal to the number of rows in ``data." For data with
#' only one state, this will be a vector of 1s.
#' @param newData an xts data matrix containing the new observation
#' @param newLabel the class label for the new observation
#' @param var.amnt the energy proportion to preserve in the projection, which
#' dictates the number of principal components to keep
#' @param trainObs the number of observations upon which to train the algorithm.
#' This will be split based on class information by a priori class membership
#' proportions.
#' @return A contribution plot and a list with the following items: \itemize{
#' \item{TrainCV -- }{A list vectors containing the contribution values corresponding
#' to each observation in the set of training observations.}
#' \item{NewCV -- }{The vector of contribution values associated with the
#' new observation}
#' }
#' @export
#' @importFrom robustbase adjbox
#' @examples
#' \dontrun{
#' # Create some data
#' dataA1 <- mspProcessData(faults = "B1")
#' traindataA1 <- dataA1[1:8567,]
#'
#' # Train on the data that should be in control
#' trainResults <- mspTrain(traindataA1[,-1], traindataA1[,1], trainObs = 4320)
#'
#' # Lag an out of control observation
#' testdataA1 <- dataA1[8567:8568,-1]
#' testdataA1 <- lag.xts(testdataA1,0:1)
#' testdataA1 <- testdataA1[-1,]
#' testdataA1 <- cbind(dataA1[8568,1],testdataA1)
#'
#' tD <- traindataA1[,-1]
#' tL <- traindataA1[,1]
#' nD <- testdataA1[,-1]
#' nL <- testdataA1[,1]
#' tO <- 4320
#' vA <- 0.95
#'
#' mspContributionPlot(tD, tL, nD, nL, vA, tO)}
#'


mspContributionPlot <- function(trainData,
                                trainLabel,
                                newData,
                                newLabel,
                                var.amnt,
                                trainObs){
  #Run mspTrain to calculate the PCA pieces needed




  trainingSummary <- mspTrain(trainData, trainLabel,
                              trainObs = trainObs, var.amnt=var.amnt)$TrainingSpecs

  trainProject <- trainingSummary[[newLabel]]$projectionMatrix
  trainLambda <- trainingSummary[[newLabel]]$LambdaInv
  # Calculate the CV for each observation in the training data set
  trainCV <- list()
  for(i in 1:nrow(trainData)){
    observation <- trainData[(i-1):i]
    observation <- lag.xts(observation, 0:1)
    observation <- observation[-1,]

    CV <- observation%*%trainProject%*%trainLambda%*%t(trainProject)

    CV2 <- vector()
    for(j in 1:(length(CV)/2)){
      CV2[j] <- CV[j] + CV[j + (length(CV)/2)]
    }
    trainCV[[i]] <- CV2
  }

  # Calculate the CV for the new observation
  newCV <- newData%*%trainProject%*%trainLambda%*%t(trainProject)
  newCV2 <- vector()
  for(i in (1:length(newCV)/2)){
    newCV2[i] <- newCV[i] + newCV[i + (length(newCV)/2)]
  }

  # Create an adjusted boxplot of the training CVs
  trainCV <- trainCV[-1]
  trainCV2 <- unlist(trainCV)
  trainGroup <- rep(names(trainData), length(trainCV))

  adjbox(trainCV2~trainGroup, xlab = "Variable", main = "Adjusted Boxplots of Contribution Values")

  # Add points corresponding to the new CVs
  points(1:length(unique(trainGroup)), newCV2, col = 'red', pch = 8, cex = 1.5)

  # Return a list of values
  list(trainingCV = trainCV, newCV = newCV2)
  }

