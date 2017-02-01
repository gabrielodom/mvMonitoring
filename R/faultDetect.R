#' Process Fault Detection
#'
#' @description Detect if a single multivariate observation is beyond normal
#' parameters.
#'
#' @param threshold_object An object of classes "threshold" and "pca" returned
#' by the internal threshold() function.
#' @param observation A single row of an xts data matrix to compare against the
#' thresholds
#' @param ... Lazy dots for additional internal arguments
#'
#' @return A named 1 * 4 matrix of the SPE statistic value ("SPE"),
#' SPE fault indicator ("SPE_Flag"), T2 statistic value ("T2"), and T2 fault
#' indicator for the single row observation passed to this function ("T2_Flag").
#'
#' @details This function takes in a threshold object returned by the
#' threshold() function and a single observation as a matrix or xts row. The
#' function then returns a row vector of the SPE test statistics, a logical
#' indicator marking if this statistic is beyond the threshold, the Hotelling's
#' T2 statistic, and an indicator if this statistic is beyond the threshold.
#' Observations with monitoring statistics beyond the calculated threshold are
#' marked with a 1, while within-parameter observations are marked with a 0.
#' These threshold values are passed from the threshold() function through this
#' function via a returned threshold object. This object will be used in higher
#' function calls.
#'
#' This internal function is called by faultFilter().
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
