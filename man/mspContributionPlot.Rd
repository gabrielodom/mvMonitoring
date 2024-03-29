% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mspContributionPlot.R
\name{mspContributionPlot}
\alias{mspContributionPlot}
\title{Contribution Plots}
\usage{
mspContributionPlot(
  trainData,
  trainLabel,
  newData,
  newLabel,
  var.amnt,
  trainObs
)
}
\arguments{
\item{trainData}{an xts data matrix containing the training observations}

\item{trainLabel}{Class labels for the training data as a logical (two states
only) or finite numeric (two or more states) vector or matrix column (not from
a data frame) with length equal to the number of rows in ``data." For data with
only one state, this will be a vector of 1s.}

\item{newData}{an xts data matrix containing the new observation}

\item{newLabel}{the class label for the new observation}

\item{var.amnt}{the energy proportion to preserve in the projection, which
dictates the number of principal components to keep}

\item{trainObs}{the number of observations upon which to train the algorithm.
This will be split based on class information by a priori class membership
proportions.}
}
\value{
A contribution plot and a list with the following items: \describe{
  \item{TrainCV -- }{A list vectors containing the contribution values
    corresponding to each observation in the set of training observations.}
  \item{NewCV -- }{The vector of contribution values associated with the
    new observation}
}
}
\description{
This function plots the contribution value for each
variable of a newly monitored observation and compares them to the
contribution values of the training data.
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

tD <- traindataA1[,-1]
tL <- traindataA1[,1]
nD <- testdataA1[,-1]
nL <- testdataA1[,1]
tO <- 4320
vA <- 0.95

mspContributionPlot(tD, tL, nD, nL, vA, tO)}

}
