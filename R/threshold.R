#' Non-parametric Threshold Estimation
#'
#' @description Calculate the non-parametric critical value threshold estimates
#'   for the SPE and T2 monitoring test statistics.
#'
#' @param pca_object A list with class "pca" from the internal pca() function
#' @param alpha The upper 1 - alpha quantile of the SPE and T2 densities from
#'   the training data passed to this function. Defaults to 0.001.
#' @param ... Lazy dots for additional internal arguments
#'
#' @return A list with classes "threshold" and "pca" containing:
#'   \itemize{
#'     \item{SPE_threshold -- }{the 1 - alpha quantile of the estimated SPE
#'       density}
#'     \item{T2_threshold -- }{the 1 - alpha quantile of the estimated Hotelling's
#'       T2 density}
#'     \item{projectionMatrix -- }{a projection matrix from the data feature space
#'       to the feature subspace which preserves some pre-specified proportion
#'       of the energy of the data scatter matrix. This pre-specified energy
#'       proportion is user supplied as the var.amnt argument in the pca()
#'       function. See the pca() function's help file for more details.}
#'     \item{LambdaInv -- }{a diagonal matrix of the reciprocal eigenvalues of the
#'       data scatter matrix}
#'   }
#'
#' @details This function takes in a pca object returned by the pca() function
#'   and a threshold level defaulting to alpha = 0.1 percent of the
#'   observations. This critical quantile is set this low to reduce false
#'   alarms, as described in Kazor et al (2016). The function then returns a
#'   calculated SPE threshold corresponding to the 1 - alpha critical value, a
#'   similar T2 threshold, and the projection and Lambda Inverse (1 /
#'   eigenvalues) matrices passed through from the pca() function call.
#'
#' This internal function is called by faultFilter().
#'
#' @export
#'
#' @examples
#' data("normal_switch_xts")
#' scaledData <- scale(normal_switch_xts[,-1])
#' pca_obj <- pca(scaledData, var.amnt = 0.9)
#' threshold(pca_object = pca_obj, alpha = 0.05)
threshold <- function(pca_object, alpha = 0.001, ...){
  UseMethod("threshold")
}

#' @export
#' @keywords internal
#'
#' @importFrom BMS quantile.density
#' @importFrom stats density
#'
threshold.pca <- function(pca_object, alpha = 0.001, ...){

  spe <- pca_object$SPE
  t2 <- pca_object$T2

  SPE.np.dens <- density(spe,
                         bw = "SJ", # Sheather Jones
                         kernel = "gaussian",
                         from = 0)

  # BMS::quantile.density
  SPE.lim.np <- quantile.density(SPE.np.dens, 1 - alpha)

  T2.np.dens <- density(t2,
                        bw = "SJ", # Sheather Jones
                        kernel = "gaussian",
                        from = 0)
  # BMS::quantile.density
  T2.lim.np <- quantile.density(T2.np.dens, 1 - alpha)

  object <- list(SPE_threshold = SPE.lim.np,
                 T2_threshold = T2.lim.np,
                 projectionMatrix = pca_object$projectionMatrix,
                 LambdaInv = pca_object$LambdaInv)

  class(object) <- c("threshold", "pca")
  object
}
