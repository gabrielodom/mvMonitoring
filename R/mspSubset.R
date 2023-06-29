#' Multi-State Subsetting
#'
#' @description This function separates the data into k subsets, one for each
#' of the k states, containing the subset of the original variables that are of interest
#' for a given state.
#' @param data An xts data matrix
#' @param labelVector Class labels as a logical (two states only) or
#' finite numeric (two or more states) vector or matrix column (not
#' from a data frame) with length equal to the number of rows in "data."
#' For data with only one state, this will be a vector of 1s.
#' @param subsetMatrix A matrix of logicals with number of rows equal to the number of
#' states and number of columns equal to the number of columns in data. The i,j entry in
#' the matrix indicates whether or not to monitor the jth variable in the
#' ith state.
#' @return A list with the following components:
#' \item{Class1Data -- }{an xts data matrix containing the subset of the state 1 data.}
#' \item{Class2Data -- }{an xts data matrix containing the subset of the state 2 data.}
#' \item{Class3Data -- }{an xts data matrix containing the subset of the state 3 data.}
#'
#' @details This function is designed to be used in conjunction with \code{\link{mspTrain}}
#' and to allow the user to monitor a different subset of the variables during each state.
#' @export
#' @examples
#' nrml <- mspProcessData(faults = "NOC")
#'
#' sub1 <- c(TRUE,TRUE,FALSE)
#' sub2 <- c(TRUE,FALSE,TRUE)
#' sub3 <- c(TRUE,FALSE,FALSE)
#' submatrix <- t(matrix(c(sub1,sub2,sub3),nrow=3,ncol=3))
#'
#' subsets <- mspSubset(data = nrml[,-1],
#' labelVector = nrml[,1],
#' subsetMatrix = submatrix)

mspSubset <- function(data,
                      labelVector = rep(1, nrow(data)),
                      subsetMatrix = matrix(TRUE,nrow = length(unique(labelVector)), ncol = ncol(data))){

  # Is data an xts object?
   if(!is.xts(data)){
    stop("Object 'data' is not an xts object. Please transform your data to an
         extendable time series.")
  }

  # What are the states?
  classes <- unique(labelVector)

  # We have to do this because of how xts objects are subsetted
  names(classes) <- LETTERS[1:length(classes)]

  if(is.vector(labelVector)){
    labelVector <- matrix(labelVector, ncol = 1)
  }
  # Attach the class vector to the data
  classData <- cbind(labelVector, data)

  # Which variables are we interested in for each state?
  SubsetData <- lapply(1:length(classes), function(i){
    dataset <- classData[classData[,1] == classes[i],]
    dataset <- dataset[,-1]
    dataset <- dataset[,subsetMatrix[i,]]

  })
  names(SubsetData) <- c("Class1Data","Class2Data","Class3Data")
  SubsetData
}
