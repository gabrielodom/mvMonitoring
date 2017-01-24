#' Title
#'
#' @param pca_object
#' @param alpha
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
threshold <- function(pca_object, alpha = 0.001, ...){
  UseMethod("threshold")
}

#' @export
#' @keywords internal
#'
#' @importFrom BMS quantile.density
#'
threshold.pca <- function(pca_object, alpha = 0.001, ...){
  # This function takes in a pca object returned by the pca.R function and a
  # threshold level defaulting to 0.1% of the observations (set low to reduce
  # false alarms, as described in Kazor et al (2016)). The function returns a
  # calculated SPE threshold corresponding to the 1 - alpha critical value, a
  # similar T2 threshold, and the projection and Lambda Inverse matrices passed
  # through from the pca.R function call.

  spe <- pca_object$SPE
  t2 <- pca_object$T2

  SPE.np.dens <- density(spe,
                         bw = "SJ", # Sheather Jones
                         kernel = "gaussian",
                         from = 0)

  # BMS::quantile.density
  SPE.lim.np <- quantile(SPE.np.dens, 1 - alpha)

  T2.np.dens <- density(t2,
                        bw = "SJ", # Sheather Jones
                        kernel = "gaussian",
                        from = 0)
  # BMS::quantile.density
  T2.lim.np <- quantile(T2.np.dens, 1 - alpha)

  object <- list(SPE_threshold = SPE.lim.np,
                 T2_threshold = T2.lim.np,
                 projectionMatrix = pca_object$projectionMatrix,
                 LambdaInv = pca_object$LambdaInv)

  class(object) <- c("threshold", "pca")
  object
}
