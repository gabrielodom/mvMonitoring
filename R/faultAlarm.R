# The Alarm function


#' Title
#'
#' @param faultsToTrigger
#' @param faultObs
#' @param faultDF
#'
#' @return
#' @export
#'
#' @examples
faultAlarm <- function(faultDF, faultsToTrigger = 3, faultObs = NULL){
  if(is.null(faultObs)){
    SPE_alarm <- lapply(faultsToTrigger:nrow(faultDF), function(i){
      all(faultDF[(i - faultsToTrigger + 1):i, 2] == TRUE)
    })
    T2_alarm <- lapply(faultsToTrigger:nrow(faultDF), function(i){
      all(faultDF[(i - faultsToTrigger + 1):i, 4] == TRUE)
    })
    xts(matrix(c(SPE_alarm, T2_alarm), ncol = 2),
        order.by = index(faultDF[faultsToTrigger:nrow(faultDF),]))
  }else{
    if(nrow(faultDF) >= faultsToTrigger){
      check_xts <- rbind(tail(faultDF, n = 2), faultObs)
      if(all(check_xts[,2]) == TRUE){
        warning("SPE detects process fault.", immediate. = TRUE)
      }
      if(all(check_xts[,4]) == TRUE){
        warning("T2 detects process fault.", immediate. = TRUE)
      }
    }

  }

}

