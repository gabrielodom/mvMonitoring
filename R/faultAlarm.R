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
#' @importFrom xts xts
#' @importFrom zoo index
#'
#' @examples
faultAlarm <- function(faultDF, faultsToTrigger = 3, faultObs = NULL){
  if(is.null(faultObs)){

    SPE_alarm <- sapply(faultsToTrigger:nrow(faultDF), function(i){
      ifelse(all(faultDF[(i - faultsToTrigger + 1):i, 2] == TRUE), "Alarm", "Normal")
    })
    T2_alarm <- sapply(faultsToTrigger:nrow(faultDF), function(i){
      ifelse(all(faultDF[(i - faultsToTrigger + 1):i, 4] == TRUE), "Alarm", "Normal")
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

