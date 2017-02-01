#' Process Alarms
#'
#' @description Trigger an alarm, if necessary, for incoming multivariate
#' process observations.
#'
#' @param mspMonitor_object An xts matrix returned by the mspMonitor() function
#' @param faultsToTriggerAlarm Specifies how many sequential faults will cause
#' an alarm to trigger. Defaults to 3.
#'
#' @return An xts matrix of the same dimensions as mspMonitor_object, with a
#' recorded negative or positive and type-specific alarm status
#'
#' @details This function and the mspMonitor() function are designed to be ran
#' via a script within a batch. The file flow is as follows: at each time
#' interval, run the mspMonitor() function on the daily observation matrix to
#' add a flag status to the most recent incoming observation in the matrix, and
#' return this new xts matrix. Then, pass this updated daily observation matrix
#' to the mspWarning() function, which will check if the process has recorded
#' three or more sequential monitoring statistic flags in a row. Of note, since
#' these functions are expected to be repeatedly ran in real time, this
#' function will only check for an alarm within the last row of the xts matrix.
#' To check multiple rows for an alarm state, please use the mspTrain function,
#' which was designed to check multiple past observations.
#'
#' This function requires an xts matrix returned by the mspMonitor() function.
#'
#' @export
#'
#'
#' @examples
mspWarning <- function(mspMonitor_object, faultsToTriggerAlarm = 3){
  data_xts <- mspMonitor_object
  n <- nrow(data_xts)

  # If we see flags on the last row, then we continue
  if(data_xts[n, "SPE_Flag"] == 1 || data_xts[n, "T2_Flag"] == 1){

    # If we also have enough observations to even trigger an alarm, then we
    # continue
    if(n >= faultsToTriggerAlarm){

      # We create the Alarm Standard
      alarmCheck <- rep(1, faultsToTriggerAlarm)

      # Now that we are checking the alarm state for observation n, we write a
      # 0 over the previous NA value. This shows us that NAs mean the state for
      # that specific observation have not been checked.
      data_xts[n, "Alarm"] <- 0

      # We now check the last few SPE flags
      x1 <- as.vector(data_xts[(n - faultsToTriggerAlarm + 1):n, "SPE_Flag"])
      if(identical(x1, alarmCheck)){
        data_xts[n, "Alarm"] <- 1
      }

      # And we also check the last few T2 flags. We will increment the Alarm
      #state based on SPE flag status, T2 flag status, or both
      x2 <- as.vector(data_xts[(n - faultsToTriggerAlarm + 1):n, "T2_Flag"])
      if(identical(x2, alarmCheck)){
        data_xts[n, "Alarm"] <- data_xts[n, "Alarm"] + 2
      }
    }
  }else{
    if(n >= faultsToTriggerAlarm){
      # If we didn't see a flag on the last row, then no problem
      data_xts[n, "Alarm"] <- 0
    }
  }
  data_xts
}

