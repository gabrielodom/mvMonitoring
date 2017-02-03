#'  Process Data under Normal Conditions
#'
#' @description Three-feature, three-state simulated process data under normal
#'   operating conditions as example data for different included functions.
#'
#' @format An xts data matrix with 10080 rows and four columns, corresponding to
#'   one week worth of data recorded at a 1-minute interval, and four columns as
#'   defined here:
#' \itemize{
#'   \item{state -- }{the state indicator for the multivariate system, with three
#'   levels}
#'   \item{x : }{x(t) = t + error}
#'   \item{y : }{y(t) = t ^ 2 - 3t + error}
#'   \item{z : }{z(t) = - t ^ 3 + 3t ^ 2 + error}
#' }
#' where t is a 10080-entry vector of autocorrelated and non-stationary hidden
#' process realizations. The states alternate each hour and are defined as
#' follows:
#' \itemize{
#'   \item{State1 -- }{As presented}
#'   \item{State2 -- }{Rotated by (yaw = 0, pitch = 90, roll = 30) and scaled by
#'   (1 * x, 0.5 * y , 2 * z).}
#'   \item{State3 -- }{Rotated by (yaw = 90, pitch = 0, roll = -30) and scaled by
#'   (0.25 * x, 0.1 * y , 0.75 * z).}
#' }
#' See the vignette for more details.
#' @source Simluated in R.
"normal_switch_xts"
