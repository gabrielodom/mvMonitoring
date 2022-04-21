#' Extract Quantiles from 'density' Objects
#'
#' @description Quantiles for objects of class \code{density}
#'
#' @param x a object of class \code{density} or a list of densities
#' @param probs numeric vector of probabilities with values in [0,1]. Note that
#'   elements very close to the boundaries return \code{Inf} or \code{-Inf}
#' @param names logical; if \code{TRUE}, the result has a names attribute, resp.
#'   a rownames and colnames attributes. Set to \code{FALSE} for speedup with
#'   many probabilities
#' @param normalize logical; if \code{TRUE} then the values in \code{x$y} are
#'   multiplied with a factor such that their integral is equal to one.
#' @param ... further arguments passed to or from other methods (currently
#'   unused)
#'
#' @return If x is of class \code{density} (or a list with exactly one element),
#'   a vector with quantiles. If x is a list of densities, then the output is a
#'   matrix of quantiles, with each matrix row corresponding to the respective
#'   density.
#'
#' @details This function is a near-exact copy of the \code{quantile.density}
#'   function from package BMS (\url{https://CRAN.R-project.org/package=BMS}).
#'   In spring of 2022, CRAN informed us that the BMS has been orphaned, so we
#'   copied the code (and corresponding documentation) we needed from it. See
#'   \url{https://www.doi.org/10.18637/jss.v068.i04} for their paper.
#'
#'   The function \code{quantile.density()} applies generically to the built-in
#'   class \code{density} (as least for versions where there is no such method
#'   in the pre-configured packages). Note that this function relies on
#'   trapezoidal integration in order to compute the cumulative densities
#'   necessary for the calculation of quantiles.
#'
#' @author Stefan Zeugner, \email{stefan.zeugner@@ec.europa.eu}
#' @author Martin Feldkircher, \email{martin.feldkircher@@da-vienna.ac.at}
#'
#'
#' @export
#'
#' @examples
#'   rNorm_dens <- density(rnorm(100000))
#'   quantile(rNorm_dens)
#'
quantile.density <- function (x,
                              probs = seq(0.25, 0.75, 0.25),
                              names = TRUE,
                              normalize = TRUE,
                              ...) {

    my.quantile.density = function(x, probs, names, normalize,
                                   ...) {
      ycs = (cumsum(x$y) - (x$y - x$y[[1]])/2) * diff(x$x[1:2])
      if (normalize)
        ycs = ycs/(ycs[[length(ycs)]])
      xin = x$x
      maxi = length(ycs)
      qqs = sapply(as.list(probs), function(qu) {
        iii = sum(ycs <= qu)
        if (iii == maxi)
          return(Inf)
        else if (iii == 0L)
          return(-Inf)
        else {
          return(xin[[iii + 1]] + ((ycs[[iii + 1]] - qu)/(ycs[[iii +
                                                                 1]] - ycs[[iii]])) * (xin[[iii]] - xin[[iii +
                                                                                                           1]]))
        }
      })
      if (as.logical(names))
        names(qqs) = paste(format(100 * probs, trim = TRUE,
                                  digits = max(2L, getOption("digits"))), "%",
                           sep = "")
      return(qqs)
    }
    probs = as.vector(probs)
    if (is.element("density", class(x)))
      return(my.quantile.density(x = x, probs = probs, names = names,
                                 normalize = normalize))
    if (!all(sapply(x, function(dd) is.element("density", class(dd)))))
      stop("x needs to be a density or list of densities")
    if (length(x) == 1L)
      return(my.quantile.density(x = x[[1]], probs = probs,
                                 names = names, normalize = normalize))
    qout = sapply(x, my.quantile.density, probs = probs, names = FALSE,
                  normalize = normalize)
    if (!is.matrix(qout)) {
      if (length(probs) > 1)
        return(qout)
      qout = as.matrix(qout)
    }
    else qout = t(qout)
    if (as.logical(names))
      colnames(qout) = paste(format(100 * probs, trim = TRUE,
                                    digits = max(2L, getOption("digits"))), "%", sep = "")
    return(qout)
  }
