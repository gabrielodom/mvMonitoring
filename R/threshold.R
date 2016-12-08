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
threshold <- function(pca_object, alpha = 0.05, ...){
  UseMethod("threshold")
}

#' @export
#' @keywords internal
#'
#' @importFrom BMS quantile.density
#'
threshold.pca <- function(pca_object, alpha = 0.05, ...){

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
