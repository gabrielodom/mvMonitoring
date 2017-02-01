#'  Process Data under Normal Conditions
#'
#' @description Three-feature, three-state process data under normal operating
#' conditions as example data for different included functions.
#'
#' @format An xts data matrix with 10080 rows, corresponding to one week worth
#' of data recorded at a 1-minute interval, and four columns as defined here:
#' \describe{
#'   \item{state}{the state indicator for the multivariate system, with three
#'   levels}
#'   \item{x}{x(t) = t + error}
#'   \item{y}{y(t) = t ^ 2 - 3t + error}
#'   \item{z}{z(t) = - t ^ 3 + 3t ^ 2 + error}
#' }
#' where t is a 10080-entry vector of autocorrelated and non-stationary hidden
#' process realizations. See the vignette for more details.
#' @source Simluated in R.
"normal_switch_xts"
