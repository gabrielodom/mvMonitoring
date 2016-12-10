# Test File
# This data should print warnings
warningData <- rbind(MASS::mvrnorm(125, mu = c(0,0,0),
                                   Sigma = toeplitz(c(1, 0.5, 0.1))),
                     MASS::mvrnorm(100, mu = c(3,3,3),
                                   Sigma = toeplitz(c(1, 0.5, 0.1))))
warningData <- as.data.frame(warningData)
warningData$dateTime <- seq.POSIXt(from = as.POSIXct(Sys.Date()),
                                   by = "hour",
                                   length.out = nrow(warningData))
warningData <- xts::xts(warningData[, -4], order.by = warningData[,4])

# Test the faultFilter function
faultFilter(trainData = warningData[1:100,],
            testData = warningData[101:225,],
            updateFreq = 50,
            var.amnt = 0.8)



# Test the processMonitor function
monit <- processMonitor(warningData,
                        trainObs = 100,
                        updateFreq = 50,
                        var.amnt = 0.8)
