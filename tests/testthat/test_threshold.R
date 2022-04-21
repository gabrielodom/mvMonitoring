

###  Setup  ###
set.seed(20160516)
nrml <- mspProcessData(faults = "NOC")
scaledData <- scale(nrml[,-1])
pca_obj <- pca(scaledData)


# Target
testOut_ls <- list(
  SPE_threshold = c(`99.9%` = 0.6230740),
  T2_threshold  = c(`99.9%` = 7.0983802)
)

# Actual
realOut_ls <- threshold(pca_object = pca_obj)


###  Tests  ###
test_that(
  "Correct SPE Threshold",
  {
    expect_equal(testOut_ls$SPE_threshold, realOut_ls$SPE_threshold)
  }
)

test_that(
  "Correct Hotelling's T2 Threshold",
  {
    expect_equal(testOut_ls$T2_threshold, realOut_ls$T2_threshold)
  }
)
