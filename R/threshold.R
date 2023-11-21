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
#'   \describe{
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
#'     \item{T2 -- }{the vector of Hotelling's T2 test statistic values for each of the n
#'     observations in "data"}
#'     \item{SPE -- }{the vector of SPE test statistic values for each of the n
#'     observations in "data"}
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
#'   This internal function is called by faultFilter().
#'
#' @seealso Called by: \code{\link{faultFilter}}. This function uses a port of
#'   the \code{quantile.density()} function from the now-orphaned BMS package.
#'
#' @export
#'
#' @examples
#' nrml <- mspProcessData(faults = "NOC")
#' scaledData <- scale(nrml[,-1])
#' pca_obj <- pca(scaledData)
#' threshold(pca_object = pca_obj)
#'
threshold <- function(pca_object, alpha = 0.001, ...){
  UseMethod("threshold")
}

#' @export
#' @keywords internal
#'
#' @importFrom stats density quantile
#'
threshold.pca <- function(pca_object, alpha = 0.001, ...){

  spe <- pca_object$SPE
  t2 <- pca_object$T2

  # UPDATE 2023-05-12: R-devel now has a modification to the density.default()
  #   function as described in https://bugs.r-project.org/show_bug.cgi?id=18337
  # We will add the option old.coords=TRUE to preserve compatibility with our
  #   previous testing scripts. I checked the change, and the differences are
  #   in the 6th decimal place.
  SPE.np.dens <- density(
    spe,
    bw = "SJ", # Sheather Jones
    kernel = "gaussian",
    from = 0,
    old.coords = TRUE
  )

  # Ported BMS::quantile.density to quantile.density()
  SPE.lim.np <- quantile(SPE.np.dens, 1 - alpha)

  # UPDATE 2023-05-12: see comment above
  T2.np.dens <- density(
    t2,
    bw = "SJ", # Sheather Jones
    kernel = "gaussian",
    from = 0,
    old.coords = TRUE
  )
  # Ported BMS::quantile.density to quantile.density()
  T2.lim.np <- quantile(T2.np.dens, 1 - alpha)

  object <- list(SPE_threshold = SPE.lim.np,
                 T2_threshold = T2.lim.np,
                 projectionMatrix = pca_object$projectionMatrix,
                 LambdaInv = pca_object$LambdaInv,
                 T2 = pca_object[[4]],
                 SPE = pca_object[[3]])

  class(object) <- c("threshold", "pca")
  object
}
