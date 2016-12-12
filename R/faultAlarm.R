# The Alarm function


#' Title
#'
#' @param faultDF
#' @param observation
#'
#' @return
#' @export
#'
#' @examples
faultAlarm <- function(faultDF, observation){
  if(nrow(faultObj) >= 3 && all(faultObj[(iter - 2):iter, 2]) == TRUE){
    warning("SPE detects process fault.", immediate. = TRUE)
  }
  if(nrow(faultObj) >= 3 && all(faultObj[(iter - 2):iter, 4]) == TRUE){
    warning("T2 detects process fault.", immediate. = TRUE)
  }
}

