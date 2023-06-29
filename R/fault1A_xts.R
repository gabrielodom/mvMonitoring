#'  Process Data under a System Shift Fault
#'
#' @description Three-feature, three-state simulated process data including
#'   observations under normal operating conditions and observations after a
#'   positive shift for each feature in the system.
#'
#' @format An xts data matrix with 10080 rows and four columns, corresponding to
#'   one week worth of data recorded at a 1-minute interval. The columns under
#'   normal conditions are defined in the help file for normal_switch_xts. The
#'   fault is a system shock to each of the three features by 2. The fault
#'   starts at row 8500, and the four columns under the fault state are defined
#'   here:
#' \itemize{
#'   \item{state : }{the state indicator for the multivariate system, with three
#'   levels}
#'   \item{x : }{x(t) = t + 2 + error}
#'   \item{y : }{y(t) = t ^ 2 - 3t + 2 + error}
#'   \item{z : }{z(t) = - t ^ 3 + 3t ^ 2 + 2 + error}
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
#' @source Simulated in R.
"fault1A_xts"
