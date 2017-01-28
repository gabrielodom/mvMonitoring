#' Fault Detection
#'
#' @description Detect if a single observation is beyond normal parameters
#'
#' @param threshold_object An object of classes "threshold" and "pca" returned
#' by the internal threshold() function.
#' @param observation A single row of an xts data matrix to test against the
#' thresholds
#' @param ... Lazy dots for additional internal arguments
#'
#' @return A named 1 row, 4 column matrix of the SPE statistic value ("SPE"),
#' SPE fault indicator ("SPE_Flag"), T2 statistic value ("T2"), and T2 fault
#' indicator for the single row observation passed to this function ("T2_Flag").
#'
#' @details This function takes in the threshold object returned by the
#' threshold.R function and a single observation which needs fault detection.
#' The function then returns a row vector of the SPE test statistics, a logical
#' indicator indicating if this statistic is beyond the threshold, the T2
#' statistic, and an indicator if this statistic is beyond the threshold. These
#' threshold values are passed in through the threshold object after calculation
#' from the training data set in the threshold() function.

#'
#' @export
#'
#' @examples
faultDetect <- function(threshold_object, observation, ...){
  UseMethod("faultDetect")
}


#' @export
#' @keywords internal
#'
#' @examples
faultDetect.threshold <- function(threshold_object, observation, ...){

  SPEthreshold <- threshold_object$SPE_threshold
  T2threshold <- threshold_object$T2_threshold
  P <- threshold_object$projectionMatrix
  LambdaInv <- threshold_object$LambdaInv

  proj_observation <- observation %*% P

  # Reduced Observation in Original Space
  obs.hat <- proj_observation %*% t(P)

  # Residual Vector
  E <- observation - obs.hat

  # Squared prediction error monitoring statistic
  SPE <- diag(E %*% t(E))
  SPE_flag <- as.numeric(SPE > SPEthreshold)

  # Hotelling's T^2 monitoring statistic
  T2 <- diag(proj_observation %*% LambdaInv %*% t(proj_observation))
  T2_flag <- as.numeric(T2 > T2threshold)

  object <- matrix(c(SPE, SPE_flag, T2, T2_flag), nrow = 1)
  colnames(object) <- c("SPE", "SPE_Flag", "T2", "T2_Flag")

  object
}
