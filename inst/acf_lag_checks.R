# NOC Process correlations

# Generate a matrix of NOC observations
nrmlProc <- mspProcessData(faults = "NOC")[[1]]

###  ACF  ###
# Autocorrelation Function
nrmlProc[,2] %>% acf
nrmlProc[,3] %>% acf
nrmlProc[,4] %>% acf
# As we can see, the three features of our NOC data are heavily autocorrelated.

###  PACF  ###
# Partial Autocorrelation Function
nrmlProc[,2] %>% pacf
nrmlProc[,3] %>% pacf
nrmlProc[,4] %>% pacf
# After 2 lags, the observations are independent within feature

###  CCF  ###
# Cross-Correlation Function
ccf(x = drop(nrmlProc[,2]), y = drop(nrmlProc[,3]))
ccf(x = drop(nrmlProc[,2]), y = drop(nrmlProc[,4]))
ccf(x = drop(nrmlProc[,3]), y = drop(nrmlProc[,4]))

