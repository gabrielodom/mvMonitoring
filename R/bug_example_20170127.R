# Example of bug:

data <- MASS::mvrnorm(1000, mu = c(0,0,0), Sigma = toeplitz(c(1, 0.5, 0.1)))
colnames(data) <- paste0("col", 1:ncol(data))
data <- xts(data,
            order.by = seq.POSIXt(from = as.POSIXlt(Sys.Date(), tz = "Chicago"),
                                  by = "min",
                                  length.out = nrow(data)))
processMonitor(data, trainObs = 200, updateFreq = 100)
