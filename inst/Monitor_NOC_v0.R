rm(list=ls())
remote <- FALSE #TRUE - If running code remotely
if(!remote)setwd("/Users/Karen/Documents/2014-Spring/ERC_NNs/RCode/MBR_Monitoring/")
if(remote)setwd("/u/eu/er/kkazor//Documents/MBR_Monitoring")	

# Running code from terminal 
#--------------
{
  # Corrected removal:
  #------
  {
    # PCA: (these all take seconds to run!)  
    #------
    # R CMD BATCH '--args method PCA Adaptive 1 Dynamic 1 thresh p rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_ADPCA_p & # Done
    # R CMD BATCH '--args method PCA Adaptive 1 Dynamic 0 thresh p rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_APCA_p & # Done
    # R CMD BATCH '--args method PCA Adaptive 1 Dynamic 1 thresh np rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_ADPCA_np & # Done
    # R CMD BATCH '--args method PCA Adaptive 1 Dynamic 0 thresh np rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_APCA_np & # Done
    # R CMD BATCH '--args method PCA Adaptive 0 Dynamic 1 thresh b rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_DPCA_b & # Done
    # R CMD BATCH '--args method PCA Adaptive 0 Dynamic 0 thresh b rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_PCA_b & # Done
    #------
    
    # KPCA: 
    #------
    # R CMD BATCH '--args method KPCA Adaptive 1 Dynamic 1 thresh p rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_ADKPCA_p & # 10min
    # R CMD BATCH '--args method KPCA Adaptive 1 Dynamic 0 thresh p rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_AKPCA_p & # 10min
    # R CMD BATCH '--args method KPCA Adaptive 1 Dynamic 1 thresh np rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_ADKPCA_np & # 12min
    # R CMD BATCH '--args method KPCA Adaptive 1 Dynamic 0 thresh np rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_AKPCA_np & # 12min
    # R CMD BATCH '--args method KPCA Adaptive 0 Dynamic 1 thresh b rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_DKPCA_b & # 1.6hr
    # R CMD BATCH '--args method KPCA Adaptive 0 Dynamic 0 thresh b rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_KPCA_b & # 1.6hr
    #------  
    
    # LLE:
    #------
    # R CMD BATCH '--args method LLE Adaptive 1 Dynamic 1 thresh p rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_ADLLE_p & # 12hr
    # R CMD BATCH '--args method LLE Adaptive 1 Dynamic 0 thresh p rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_ALLE_p & # 11hr
    # R CMD BATCH '--args method LLE Adaptive 1 Dynamic 1 thresh np rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_ADLLE_np # 12hr
    # R CMD BATCH '--args method LLE Adaptive 1 Dynamic 0 thresh np rm.flag 1'
    #     Monitor_NOC_v0.R log_file_NOC_ALLE_np & # D ch215l-03
    # R CMD BATCH '--args method LLE Adaptive 0 Dynamic 1 thresh b rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_DLLE_b & # 21hr
    # R CMD BATCH '--args method LLE Adaptive 0 Dynamic 0 thresh b rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_LLE_b & # 19hr
    #------  
  }
  #------
  
  # Without removal:
  #------
  {
    # PCA: (these all take seconds to run!)  
    #------
    # R CMD BATCH '--args method PCA Adaptive 1 Dynamic 1 thresh p rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_ADPCA_p & # Done
    # R CMD BATCH '--args method PCA Adaptive 1 Dynamic 0 thresh p rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_APCA_p & # Done
    # R CMD BATCH '--args method PCA Adaptive 1 Dynamic 1 thresh np rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_ADPCA_np & # Done
    # R CMD BATCH '--args method PCA Adaptive 1 Dynamic 0 thresh np rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_APCA_np & # Done
    #------
    
    # KPCA: 
    #------
    # R CMD BATCH '--args method KPCA Adaptive 1 Dynamic 1 thresh p rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_ADKPCA_p & # Done
    # R CMD BATCH '--args method KPCA Adaptive 1 Dynamic 0 thresh p rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_AKPCA_p & # Done
    # R CMD BATCH '--args method KPCA Adaptive 1 Dynamic 1 thresh np rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_ADKPCA_np & # Done
    # R CMD BATCH '--args method KPCA Adaptive 1 Dynamic 0 thresh np rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_AKPCA_np & # Done
    #------  
    
    #LLE:
    #------
    # R CMD BATCH '--args method LLE Adaptive 1 Dynamic 1 thresh p rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_ADLLE_p & # 23hr
    # R CMD BATCH '--args method LLE Adaptive 1 Dynamic 0 thresh p rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_ALLE_p & # 17hr
    # R CMD BATCH '--args method LLE Adaptive 1 Dynamic 1 thresh np rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_ADLLE_np & # 23hr62712
    # R CMD BATCH '--args method LLE Adaptive 1 Dynamic 0 thresh np rm.flag 0'
    #     Monitor_NOC_v0.R log_file_NOC_ALLE_np & #ch215l-17hr
    #------  
  }
  #------
}
#--------------

#Load functions:
#--------------
setwd("C:/Users/gabriel_odom/Box Sync/Consulting/Dr. Hering/MV_Process_Control/Kazor Code")
source("Originals from Baylor Box/Functions_v0.R")

#--------------

#Load packages:
#--------------
{
  packageLoad("batch")
  packageLoad("locpol") 
  packageLoad("locfit")
  packageLoad("np")	 
  packageLoad("xts")
  packageLoad("quantmod")
  packageLoad("TTR")
  packageLoad("neuralnet")
  packageLoad("xtable")
  packageLoad("vegan")
  packageLoad("scatterplot3d")
  packageLoad("rgl") 
  packageLoad("car") 
  packageLoad("lle")
  packageLoad("BMS")
  packageLoad("kernlab")
  packageLoad("foreign")	
  packageLoad("plyr")
  packageLoad("dplyr")
}
#--------------

# Data Set-up:
#--------------
{
load("NOC_data.ave")
# load(file = paste("Data/MBR_Data/NOC_data.ave", sep = ""), verbose = TRUE)
D <- D.Ave.q
rm(D.Ave.q)

# 28 Non-missing vars that both NOC and PH have in common:
monitor.vars <- c("mbr_1_perm_flow","mbr_1_perm_pres","mbr_2_perm_flow",
                  "mbr_2_perm_pres","mbr_1_air_flow","mbr_2_air_flow",
                  "mbr_1_tmp","mbr_2_tmp","bio_1_do","bio_2_do",
                  "mbr_1_inf_flow","mbr_2_inf_flow","perm_turb",
                  "bio_1_blow_flow","bio_2_blow_flow","bio_1_level",
                  "bio_1_temp","bio_2_level","bio_2_temp","batch_volume",
                  "perm_tank_level","ras_do","ras_ph","ras_tss","perm_cond",
                  "ambient_temp","mbr_1_level","mbr_2_level")
  
D <- D[,monitor.vars]
  
# Impute NAs:
#-----
{
  rows.na <- apply(D, 1, function(x) any(is.na(x)))
  sum(rows.na) / dim(D)[1] # % of data missing
  na.rows <- which(rows.na)
  cols.na <- apply(D, 2, function(x) any(is.na(x)))
  na.cols=which(cols.na)
  as.matrix(colnames(D[,na.cols]))
  dim(D)[2] - sum(cols.na) # no variables w/o any missing observations
  
  # Replace all NAs with the most recent non-NA value prior
  D.fill <- apply(D, 2, function(x) zoo::na.locf(x, na.rm = FALSE))
  # Confirm no NA values
  rows.na.fill <- apply(D.fill, 1, function(x) any(is.na(x)))
  sum(rows.na.fill) / dim(D.fill)[1]
  # Replace the original data with this imputed data
  D <- as.xts(D.fill)
  head(D);tail(D)
}
#-----

D <- as.data.frame(D) #head(D)
if(!sum(apply(D, 1, function(x) any(is.na(x)))) == 0){
  print("Warning: NAs in Data")
  } #check for NAs
}
#--------------

#Determine: 'Lags' based on Partial Autocorrelation function
#--------
plot.PACFs  <- FALSE # TRUE or FALSE. Will we plot PACFs of each variable in
                     #     dataset when determining lags?
{  
  # Consider NOC data to investigate NORMAL temporal structure of covariates
  #---
  # Used first half of NOC data for training when testing false alarm rates.
  D.lag.noc <- D[1:floor(dim(D)[1]/2),]
  if(plot.PACFs){
    dev.new(width = 6, height = 3, noRStudioGD = TRUE)
    ts.plot(D, main = "Training Window for Determining Lags")
    abline(v = dim(D.lag.noc)[1], lty = 2, lwd = 2, col = 4)
  }
  #---  
  
  #---
  {
    PACFs <- vector("list", length = dim(D.lag.noc)[2])
    lag.max <- 2 * 60 / 10 # 2 hours for 10 min data
    names(PACFs) <- names(D.lag.noc)
    
    if(plot.PACFs){
      dev.new(width = 6, height = 6, noRStudioGD = TRUE)
    }
    if(plot.PACFs){
      par(mfrow = c(2,2), oma = c(1,1,1,1))
    }
    
    for(i in 1:length(PACFs)){
      pacfs <- pacf(ts(D.lag.noc[,i], freq = 1),
                    main = "", cex.lab = 1.5, cex.axis = 2,
                    ylim = c(-.6,1), lag.max = lag.max,
                    plot = plot.PACFs)
      if(plot.PACFs){
        ttl <- names(D.lag.noc)[i]
        title(ttl, line = 1, cex.main = 2.5)
        if(i %% 4 == 0){
          title("NOC", line = -1.5, cex.main = 2.5, outer = TRUE)
        }
      }
      pacfs <- cbind(pacfs$lag, pacfs$acf)
      PACFs[[i]] <- pacfs[order(abs(pacfs[,2]), decreasing = TRUE),]	
    }
    # List the lag at which the highest partial autocorrelation is attained
    #     (Note: with 10min D.lag.noc, lag 12 corresponds to 2hrs)
    Lags <- ldply(PACFs, function(x) x[1,1:2])
    
    
    # Edit to get mbr_2_perm_flow on same page as mbr_1_perm_flow and
    #     mbr_1_level on same page as mbr_2_level
    Lags[which(Lags$.id == "mbr_2_perm_flow"), 2] <- 1
    Lags[which(Lags$.id == "mbr_1_level"), 2] <- 1
    colnames(Lags)[2:3] <- c("lag", "pacf")
  }
  #---
  
  # Tack-on Lagged Variables
  #---
  if(!identical(Lags$.id, colnames(D))){
    stop("Lags and D variable orders differ!")
  }
  lags.unq <- unique(Lags$lag)
  if(0 %in% lags.unq){
    lags.unq <- lags.unq[-which(lags.unq == 0)]
  }
  lag.set <- which(Lags$lag == lags.unq[1])
  D.lag <- apply(D[, lag.set, drop = FALSE], 2,
                 function(x) Lag(x, k = lags.unq[1]))
  colnames(D.lag) <- paste(colnames(D.lag),
                           "_lag",
                           lags.unq[1],
                           sep = "")
  if(length(lags.unq) > 1){
    for(l in c(lags.unq[-1])){
      lag.set <- which(Lags$lag == l)
      hld <- apply(D[, lag.set, drop = FALSE], 2,
                   function(x) Lag(x, k = l))
      colnames(hld) <- paste(colnames(hld),
                             "_lag",
                             l,
                             sep = "")
      D.lag <- cbind(D.lag, hld)
    }
  }
  rownames(D.lag) <- rownames(D)
  D <- cbind(D, D.lag)
  rm(D.lag)
  # head(D)
  #---
}
# In non-Dynamic cases, don't include lagged variables:
# We will need to burn the first few NA rows if Dynamic, BUT do this after
#     establishing the training windows in monitor(), so that if observations
#     are lost the training window shrinks, not the testing window.
burn.set <- c(1:max(Lags$lag))
rm.set <- which(sapply(colnames(D), function(x) grep("_lag", x)) == 1)
# head(D[-burn.set,]) #Dynamic
# head(D[,-rm.set])   #non-Dynamic
#--------

# Settings:
#--------
if(!remote){
  # GABRIEL SET NEW VALUES
  method  <- "PCA"
  Adaptive <- TRUE
  Dynamic <- TRUE
  thresh <- c("np")
  rm.flag <- TRUE # only matters if Adaptive = TRUE
}
if(remote){
  parseCommandArgs()
  if(thresh == "b")thresh <- c("p","np")
  rm.flag <- ifelse(rm.flag, TRUE, FALSE)
}
plot.rw.movie <- TRUE
#--------

# Specific Settings used in Kazor et al. (2016)
#--------
{
if(Adaptive){
  # For weekly
  train.win <- 7 * 24 * 60 / 10  # 1 week for Adaptive (same as PH)
  update.frq <- 24 * 60 / 10 # 1 day for Adaptive (same as PH) 
}
if(!Adaptive){
  train.win <- 40.5 * 24 * 60 / 10  # 40.5 days for non-Adaptive
  update.frq <- NULL
}
if(Dynamic){
  Data <- D
}
if(!Dynamic){
  Data <- D[,-rm.set] #Remove lagged variables
  burn.set <- NULL
}
# Where to save Results: 
folder <- paste(ifelse(Adaptive, "A", ""), ifelse(Dynamic, "D", ""), sep = "")
if(folder == "")folder <- "S"
# ERROR: these paths won't work on my machine. Do something else.
if(length(thresh) == 1){
  R.path <- paste("Results/MBR/NOC/",
                  method,
                  "/",
                  folder,
                  "/",
                  ifelse(!rm.flag & Adaptive, "no_rm", ""),
                  "Results.",
                  thresh,
                  sep = "")
}
if(length(thresh) == 2){
  R.path <- paste("Results/MBR/NOC/",
                  method,
                  "/",
                  folder,
                  "/",
                  ifelse(!rm.flag & Adaptive, "no_rm", ""),
                  "Results.b",
                  sep = "")
}
# FOR GABRIEL'S MACHINE
R.path <- NULL
# NOTE: if R.path = NULL, then results won't automatically be saved by the
#     monitor() function, but the function will still output 'Results' which 
#     can then be saved somewhere.) 
}
#--------

# Adaptive: Rolling window 'movie' to help determine: 'train.obs.num' and
#     'train.update.freq'
#--------
if(Adaptive & plot.rw.movie){
  #---
  dev.new(width = 9, height = 3, noRStudioGD = TRUE)
  par(mfrow = c(1,1), ask = FALSE)
  for(i in 0:floor((dim(D)[1] - train.win) / update.frq)){
    ts.plot(D[,2], ylab = colnames(D)[2])
    abline(v = c(update.frq * i, train.win + update.frq * i),
           col = 4, lwd = 3, lty = 2)
    # example of 1st shift forward
    abline(v = c(train.win + update.frq * (i + 1)),
           col = 2, lwd = 3, lty = 2) 
  }
  # blue - training window
  # red- monitoring window
  #---
}
#-------- 

# Run monitor()
#--------
# Argument values are those used in Kazor et al. (2016)
# PCA takes about 8 seconds to run
if(method == "PCA"){
  Results <- monitor(D = Data,
                     method = method,
                     train.obs.num = train.win,
                     train.update.freq = update.frq,
                     alpha = 0.001,
                     rm.outlier = TRUE,
                     rm.flagged = rm.flag,
                     thrsh.type = thresh,
                     var.amnt.pca = 0.85,
                     results.path = R.path,
                     burn = burn.set)  
}
if(method == "KPCA"){
  Results <- monitor(D = Data,
                     method = method,
                     train.obs.num = train.win,
                     train.update.freq = update.frq,
                     alpha = 0.001,
                     rm.outlier = TRUE,
                     rm.flagged = rm.flag,
                     thrsh.type = thresh,
                     kern.sigma = 1,
                     kern.g = 10,
                     var.amnt.kpca = 0.95,
                     results.path = R.path,
                     burn = burn.set)
}
if(method == "LLE"){
  Results <- monitor(D = Data,
                     method = method,
                     train.obs.num = train.win,
                     train.update.freq = update.frq,
                     alpha = 0.001,
                     rm.outlier = TRUE,
                     rm.flagged = rm.flag,
                     thrsh.type = thresh,
                     k.rng = seq(1, 50, 1),
                     d.rng = c(3, seq(4, 20, 2)),
                     results.path = R.path,
                     parallel.lle = TRUE,
                     cpus = 5,
                     burn = burn.set)
}
#-------- 
  # monitor(D = Data,
  #         train.obs.num = train.win,
  #         train.update.freq = update.frq,
  #         alpha = 0.001,
  #         rm.outlier = TRUE,
  #         rm.flagged = rm.flag,
  #         thrsh.type = thresh,
  #         var.amnt.pca = 0.85,
  #         results.path = R.path,
  #         burn = burn.set)
  # monitor(D = Data,
  #         train.obs.num = train.win,
  #         train.update.freq = update.frq,
  #         alpha = 0.001,
  #         rm.outlier = TRUE,
  #         rm.flagged = rm.flag,
  #         thrsh.type = thresh,
  #         kern.sigma = 1,
  #         kern.g = 10,
  #         var.amnt.kpca = 0.95,
  #         results.path = R.path,
  #         burn = burn.set)

q()