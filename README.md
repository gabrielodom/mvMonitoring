---
title: "Multi-State Adaptive-Dynamic Process Monitoring"
author: 
---


[![Travis-CI Build Status](https://travis-ci.org/gabrielodom/mvMonitoring.svg?branch=master)](https://travis-ci.org/gabrielodom/mvMonitoring)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/mvMonitoring)](https://cranlogs.r-pkg.org/badges/grand-total/mvMonitoring)
![CRAN Build Status](https://www.r-pkg.org/badges/version/mvMonitoring)

## Overview
We create this package, `mvMonitoring`, from the foundation laid by Kazor et al (2016). This package is designed to make simulation of multi-state multivariate process monitoring statistics easy and straightforward, as well as streamlining the online process monitoring component.

## Installation from CRAN
As of 18 October, 2017, we have submitted this package to CRAN. Shortly thereafter, you will be able to install the stable version of the package via
```
install.packages("mvMonitoring")
```

## Installation of Development Version
Make sure you have the latest version of the `devtools` package, and pull the package from GitHub. Remember, if you can read this file, you are one of the few people who have access to this package. In addition, you can also run the following:
```
devtools::install_github("gabrielodom/mvMonitoring", auth_token = "tokenHere")
```

where you create the value of "tokenHere" by generating a personal access token (PAT) at https://github.com/settings/tokens and copying the quoted string to this argument.
Load the library after installation by
```
library(mvMonitoring)
```

## Examples
These are the examples shown in the help files for the mspProcessData(), mspTrain(), mspMonitor(), and mspWarning() functions.
```
# Generate one week's worth of normal operating (NOC) data recorded at the one-
# minute level
nrml <- mspProcessData(faults = "NOC")
# The state values are recorded in the first column.
n <- nrow(nrml)

# Calculate the training summary, but save five observations for monitoring.
# This function will treat the first 3 days as in control (IC), and then update
# the training window each day.
trainResults_ls <- mspTrain(
  data = nrml[1:(n - 5), -1],
  labelVector = nrml[1:(n - 5), 1],
  trainObs = 4320
)

# While training, we included 1 lag (the default), so we will also lag the
# observations we will test.
testObs <- nrml[(n - 6):n, -1]
testObs <- xts:::lag.xts(testObs, 0:1)
testObs <- testObs[-1,]
testObs <- cbind(nrml[(n - 5):n, 1], testObs)

# Run the monitoring function.
dataAndFlags <- mspMonitor(
  observations = testObs[, -1],
  labelVector = testObs[, 1],
  trainingSummary = trainResults_ls$TrainingSpecs
)

# Alarm check the last row of the matrix returned by the mspMonitor function
mspWarning(dataAndFlags)
```

## Paper Graphics
The `R` code to build and save the simulation graphics from the paper are in the `inst/mspGraphsGrid.R` file.

## Acknowledgements
This work is  supported by the King Abdullah University of Science and Technology (KAUST) Office of Sponsored Research (OSR) under Award No: OSR-2015-CRG4-2582 and by the National Science Foundation PFI:BIC Award No: 1632227.
