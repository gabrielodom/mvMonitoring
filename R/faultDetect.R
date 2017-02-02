#' Process Fault Detection
#'
#' @description Detect if a single multivariate observation is beyond normal
#'   operating conditions.
#'
#' @param threshold_object An object of classes "threshold" and "pca" returned
#'   by the internal threshold() function.
#' @param observation A single row of an xts data matrix (a 1 x p matrix) to
#'   compare against the thresholds
#' @param ... Lazy dots for additional internal arguments
#'
#' @return A named 1 x 4 matrix with the following entries for the single row
#'   observation passed to this function:
#'   \itemize{
#'     \item{SPE -- }{the SPE statistic value}
#'     \item{SPE_Flag -- }{the SPE fault indicator, where 1 represents a flag and
#'       0 marks that the observation is within the normal operating conditions}
#'     \item{T2 -- }{the T2 statistic value}
#'     \item{T2_Flag -- }{the T2 fault indicator, defined the same as SPE_Flag}
#'   }
#'
#' @details This function takes in a threshold object returned by the
#'   threshold() function and a single observation as a matrix or xts row.
#'   Internally, the function multiplies the observation by the projection
#'   matrix returned within the threshold object, calculates the SPE and T2
#'   process monitoring statistics for that observation, and compares these
#'   statistics against their corresponding threshold values to determine if the
#'   observation lies outside the expected boundaries. The function then returns
#'   a row vector of the SPE test statistic, a logical indicator marking if this
#'   statistic is beyond the threshold, the Hotelling's T2 statistic, and an
#'   indicator if this statistic is beyond the threshold. Observations with
#'   monitoring statistics beyond the calculated threshold are marked with a 1,
#'   while observations within normal operating conditions are marked with a 0.
#'   These threshold values are passed from the threshold() function through
#'   this function via a returned threshold object. This object will be used in
#'   higher function calls.
#'
#' This internal function is called by faultFilter().
#'
#' @export
#'
#' @examples
#' data("normal_switch_xts")
#' scaledData <- scale(normal_switch_xts[,-1])
#' pca_obj <- pca(scaledData, var.amnt = 0.9)
#' thresh_obj <- threshold(pca_object = pca_obj, alpha = 0.05)
#'
#' # Check a single observation. We see this observation is within the normal
#' # operating conditions at alpha = 0.05.
#' faultDetect(threshold_object = thresh_obj, observation = scaledData[1,])
#'
#' # According to the Squared Prediction Error statistic, this observation is
#' # outside the range of "normal" operation at the 0.05 level.
#' faultDetect(threshold_object = thresh_obj, observation = scaledData[20,])
#'
#' # We can also check an entire data matrix:
#' detect_ls <- lapply(1:nrow(scaledData), function(i){
#'      faultDetect(threshold_object = thresh_obj, scaledData[i,])
#' })
#' do.call(rbind, detect_ls)
faultDetect <- function(threshold_object, observation, ...){
  UseMethod("faultDetect")
}


#' @export
#' @keywords internal
#'
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
