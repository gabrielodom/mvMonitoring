#' Title
#'
#' @param threshold_object
#' @param observation
#' @param ...
#'
#' @return
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
  SPE_flag <- SPE > SPEthreshold

  # Hotelling's T^2 monitoring statistic
  T2 <- diag(proj_observation %*% LambdaInv %*% t(proj_observation))
  T2_flag <- T2 > T2threshold

  object <- data.frame(SPE = SPE,
                       SPE_flag = SPE_flag,
                       T2 = T2,
                       T2_flag = T2_flag)
  class(object) <- "faultDF"
  object
}
