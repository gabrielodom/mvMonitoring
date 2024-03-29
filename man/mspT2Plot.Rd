% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mspT2Plot.R
\name{mspT2Plot}
\alias{mspT2Plot}
\title{T-Squared Contribution Plots}
\usage{
mspT2Plot(
  trainData,
  trainLabel,
  trainT2,
  newData,
  newLabel,
  newT2,
  trainObs,
  var.amnt
)
}
\arguments{
\item{trainData}{an xts data matrix containing the training observations}

\item{trainLabel}{Class labels for the training data as a logical (two states
only) or finite numeric (two or more states) vector or matrix column (not from
a data frame) with length equal to the number of rows in ``data." For data with
only one state, this will be a vector of 1s.}

\item{trainT2}{the Hotelling's T-squared values corresponding to the
newLabel state calculated by mspTrain using the full training data with
all variables included}

\item{newData}{an xts data matrix containing the new observation}

\item{newLabel}{the class label for the new observation}

\item{newT2}{the Hotelling's T-squared value returned by mspMonitor using
the full new observation with all variables included}

\item{trainObs}{the number of observations upon which to train the algorithm.
This will be split based on class information by a priori class membership
proportions.}

\item{var.amnt}{the energy proportion to preserve in the projection, which
dictates the number of principal components to keep}
}
\description{
Plots a variation of the Hotelling's T-squared statistic to
visualize the contribution of each variable to a fault.
}
\examples{
\dontrun{
# Create some data
dataA1 <- mspProcessData(faults = "B1")
traindataA1 <- dataA1[1:8567,]

# Train on the data that should be in control
trainResults <- mspTrain(traindataA1[,-1], traindataA1[,1], trainObs = 4320)


# Lag an out of control observation
testdataA1 <- dataA1[8567:8568,-1]
testdataA1 <- lag.xts(testdataA1,0:1)
testdataA1 <- testdataA1[-1,]
testdataA1 <- cbind(dataA1[8568,1],testdataA1)

# Monitor this observation
monitorResults <- mspMonitor(observations = testdataA1[,-1],
                             labelVector = testdataA1[,1],
                             trainingSummary = trainResults$TrainingSpecs)


tD <- traindataA1[,-1]
tL <- traindataA1[,1]
nD <- testdataA1[,-1]
nL <- testdataA1[,1]
tO <- 4320
vA <- 0.95
nT2 <- monitorResults$T2
tT2 <- trainResults$TrainingSpecs[[nL]]$T2

mspT2Plot(tD,tL,tT2,nD,nL,nT2,tO,vA)
}
}
