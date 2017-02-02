#'  Process Data under a Feature Signal Dampening
#'
#' @description Three-feature, three-state simulated process data including
#'   observations under normal operating conditions and observations after a
#'   dampening of one of the underlying features in the system.
#'
#' @format An xts data matrix with 10080 rows and four columns, corresponding to
#'   one week worth of data recorded at a 1-minute interval. The columns under
#'   normal conditions are defined in the help file for normal_switch_xts. The
#'   fault is a signal dampening in the underlying determining t vector for the
#'   "z" feature. The fault starts at row 8500, and the four columns under the
#'   fault state are defined here:
#' \describe{
#'   \item{state}{the state indicator for the multivariate system, with three
#'   levels}
#'   \item{x}{x(t) = t + error}
#'   \item{y}{y(t) = t ^ 2 - 3t + error}
#'   \item{z}{z(t_*) = - (t_*) ^ 3 + 3(t_*) ^ 2 + error}
#' }
#' where t_* = log|t|, and t is a 10080-entry vector of autocorrelated and non-
#' stationary hidden process realizations. The states alternate each hour and
#' are defined as follows:
#' \describe{
#'   \item{State1}{As presented}
#'   \item{State2}{Rotated by (yaw = 0, pitch = 90, roll = 30) and scaled by
#'   (1 * x, 0.5 * y , 2 * z).}
#'   \item{State3}{Rotated by (yaw = 90, pitch = 0, roll = -30) and scaled by
#'   (0.25 * x, 0.1 * y , 0.75 * z).}
#' }
#' See the vignette for more details.
#' @source Simluated in R.
"fault3B_xts"
