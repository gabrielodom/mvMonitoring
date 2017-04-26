## ---- message = FALSE, warning = FALSE-----------------------------------
library(tidyverse)
library(mvMonitoring)
fault2A_xts <- mspProcessData(faults = "A2",
                              period = 7 * 24 * 60,
                              faultStartIndex = 8500,
                              startTime = "2015-05-16 10:00:00 CST")

str(fault2A_xts)

## ------------------------------------------------------------------------
train2A_xts <- fault2A_xts[1:10020,]
# This function will run in 13 seconds on the author's machine.
train2A_ls <- mspTrain(data = train2A_xts[,-1],
                       labelVector = train2A_xts[,1],
                       trainObs = 3 * 24 * 60,
                       updateFreq = 1 * 24 * 60,
                       Dynamic = TRUE,
                       lagsIncluded = 0:1,
                       faultsToTriggerAlarm = 5)

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

