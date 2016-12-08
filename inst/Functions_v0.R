
#----
packageLoad <- function(packName){
# Load package if installed o.w. install it first then load. 
# Handy for running code remotely and less arduous after installing latest R
# udpate!)
	# packName - package name (as a character string), e.g. "quantmod"
  
    # Try to load the package
    if(!require(packName,character.only = TRUE)){ 
		# If the package is not available, install it
		install.packages(packName, 
		                 dependencies = TRUE,
		                 repos = "http://cran.r-project.org")

		# Try again to load the package (if there is still an error, something went
    #     wrong with the installation and the code will be stopped)
		if(!require(packName, character.only = TRUE)){
		  stop(paste("Package ",
		             packName,
		             "not found and its installation failed.",
		             sep=""))
		}
    }
}
#----

# This code requires the following packages: kernlab

#-----------
createCluster = function(noCores,
                         logfile = "/dev/null",
                         export = NULL,
                         lib = NULL){
  # When running in parallel, you might have to export some defined objects,
  # functions, or packages, so that they are defined for the worker processes.
  # See: http://www.numbertheory.nl/2011/11/14/parallelization-using-plyr-
  #     loading-objects-and-packages-into-worker-nodes/ 
  # This function is a way to do that!  
  packageLoad("doSNOW")
  cl <- makeCluster(noCores,
                    type = "SOCK",
                    outfile = logfile,
                    verbose = FALSE)
  if(!is.null(export))clusterExport(cl, export)
  if(!is.null(lib)){
    l_ply(lib, function(dum){ 
      clusterExport(cl, "dum", envir = environment())
      clusterEvalQ(cl, library(dum, character.only = TRUE))
    })
  }
  registerDoSNOW(cl)
  return(cl)
}
# Test the cluster function. This will return a list of length x, wherein each
# element of x has had the function fun applied to it. Pass additional args
# to the function after fun.
myCluster <- createCluster(noCores = 6)
clusterApply(myCluster, x = 1:10, fun = log, base = exp(1))
stopCluster(myCluster)
#-----------

#----
substrFirst <- function(x, n){
  substr(x, 1, n)
}
#----

#----
substrLast <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}
#----

#----
outlier.fnc <- function(x){
  # Function to find outliers
  rem <- which(x > median(x) + 5 * sd(x) | x < median(x) - 5 * sd(x))
  if(length(rem) == 0)rem <- NA
  rem
}
#----

#----
monitor <- function(D, method = "PCA",
                    train.obs.num, train.update.freq = NULL,
                    fix.parameters = FALSE, alpha,
                    rm.outlier = TRUE, rm.flagged = TRUE,
                    thrsh.type = "np", var.amnt.pca = 0.9,
                    kern.sigma = 1, kern.g = 10,
                    var.amnt.kpca = NULL, k = NULL, d = NULL,
                    k.rng = NULL, d.rng = NULL,
                    parallel.lle = FALSE,cpus = 2,
                    results.path = NULL, burn = NULL){
  # Function to perform (Adaptive) PCA, KPCA, or LLE monitoring using either
  # parametric or nonparametric thresholds. See Kazor et al. (2016) for more
  # details.
  
  #Arguments:
  #--------------
  {
    # D - an XTS dataset with POSIX row names and columns including the
    #     selected monitoring varaibles. NOTE: this function will error if the
    #     date and time information is in a columns rather than as the rownames
    # method - "PCA", "KPCA", or "LLE" 
    # train.obs.num - Number of observations to use in the training window
    # train.update.freq - Frequency with which rolling training window is
    #     updated. If NULL (Default) then non-Adaptive monitoring is performed.
    # fix.parameters - TRUE or FALSE. TRUE - set PCA, KPCA (Ge etal 2009), and
    #     LLE (Miao et al 2013) parameters to those used in other researcher's
    #     implementations. Default is FALSE
    # alpha - alpha for setting monitoring threshold
    # rm.outlier - TRUE or FALSE. Remove outliers from training window? Default
    #     is TRUE
    # rm.flagged - TRUE or FALSE. Remove flagged observations before tacking
    #     next day onto training set? Default is TRUE
    # thrsh.type -  Threshold type. Options: "p" - use parametric density
    #     estimation to set thresholds; "np" - use nonparametric density
    #     estimation to set thresholds; c("p","np") - use both. NOTE: When
    #     Adaptive monitoring is performed you can't run both approaches
    #     simulataneously, since it is no longer clear which versions "flagged"
    #     observations should be removed. If Adaptive and thrsh.type =
    #     c("p","np"), then the function will just loop over the threshold
    #     types first monitoring with "p" and then repeating with "np". When 
    #     non-Adaptive, this is not an issue and it is more efficient to just
    #     compute results for both at the same time. Defaults to "np".
    
    #PCA parameters:
    #-------
    # var.amnt.pca - Minimum amount of total variability for PCs to capture. 
    # Defaults to 90%.
    #-------
    
    #KPCA parameter settings:
    #-------
    # kern.sigma - This is sigma in Lee et al 2004 parameterization of 'c'
    #     (NOT sigma^2!)
    # kern.g - This is g in Lee et al 2004 parameterization of 'c'. If the
    #     Gaussian kernel were parameterized as exp{-||x-y||^2/c}, Lee et al
    #     (2004) parameterize 'c' as: c=g*m*sigma^2, where m = number of
    #     montoring variables (determined from dataset), sigma^2 = 1 (since
    #     variables are standardized). We default to g = 10 (works well in
    #     general according to Lee et al 2004).
    # var.amnt.kpca - Minimum amount of total variability for PCs to capture in
    #     the feature space; if NULL then use the >= ave.eigval method of Lee
    #     et al (2004).
    #-------
    
    #LLE parameters:
    #-------
    # k - Number of neighbors
    # d - Intrinsic dimension of data
    # NOTE: If k or d is not provided, then evaluate a range of values for k
    #     (the number of nearest neighbors) and d (the intrinsic dimension),
    #     and pick the pair that minimizes the residual variance. This is
    #     repeated for each training set.
    # k.rng - Range of values for number of neighbors to consider
    # d.rng - Range of values for intrinsic dimension to consider  
    #-------  
    
    # results.path - Path to print results to after each loop of monitoring. If
    #     NULL then results aren't printed, and the function returns "Results".
    # burn - the NA rows of D that will be deleted from the initial training
    #     window when including lagged variables
  }
  #--------------
  ave.comps = FALSE # KPCA parameter that will get turned on if var.amnt.kpca
                    # is not provided
  Adaptive =! is.null(train.update.freq)
  if(!Adaptive){
    rm.flagged = FALSE # Non-Adaptive, so only go through Monitoring Loop once,
                       # and no opportunity to remove flagged observations.
    thrsh.type = c("p","np") # quicker to evaluate together in Non-Adaptive
                             # setting
  }  
  if(method == "KPCA" & is.null(var.amnt.kpca))ave.comps <- TRUE
  if(method == "LLE" & fix.parameters){k <- 8; d <- 2} # Maio etal 2013
  if(method == "LLE" & is.null(k) | is.null(d))KDsearch = TRUE 
  
  # Store 
  #--------------
  if(method == "PCA"){
    store.params=c("comps","comps.prop.var")
  }
  if(method == "KPCA"){
    store.params <- c("comps.ave",
                      "comps.ave.prop.var",
                      "comps",
                      "comps.prop.var")
  }
  if(method == "LLE"){
    store.params <- c("k",
                      "d",
                      "resid.var",
                      "rv.elapse.sec")
  }
  Results <- vector("list", length = length(thrsh.type))
  names(Results) <- thrsh.type
  store.vars <- c(store.params, "SPE.thrsh", "T2.thrsh", "SPE", "T2")
  for(l in 1:length(Results)){
    Results[[l]] <- as.data.frame(matrix(nrow = dim(D)[1],
                                         ncol = length(store.vars)))
    rownames(Results[[l]]) <- rownames(D)[1:dim(D)[1]]
    colnames(Results[[l]]) <- store.vars
  }
  #head(Results[[1]])
  #names(Results)
  #--------------
  
  #Threshold Loop:
  #--------------
  #thrsh.i=1 
  
  for(thrsh.i in 1:ifelse(Adaptive & length(thrsh.type)>1,2,1)){
    
    thrsh.type.i <- thrsh.type[thrsh.i]
    if(!Adaptive)thrsh.type.i <- thrsh.type
    # Non-Adaptive, so only go through Monitoring Loop once, and no opportunity
    # to remove flagged observations, so we can easily compute results for "p"
    # and "np" simultaneously.
    
    #Monitoring Loop:
    #--------------
    flagged.all <- array()
    train.first <- 1
    train.last <- train.first + train.obs.num - 1
    if(!Adaptive){
      train.update.freq <- dim(D)[1] - train.obs.num
    }
    #length(train.first:train.last)==train.obs.num
    #i=1
    for(i in 1:ceiling((dim(D)[1] - train.obs.num) / train.update.freq)){	
       
      #Remove Flagged Observations:
      #----
      if(rm.flagged == TRUE && i != 1){
        rm.time.stamp <- flagged.current # dates of flagged from previous
                                         # monitoring set
        print(paste("removed flagged: ", length(rm.time.stamp)))
        D.tr <- D[train.first:train.last,] #head(D.tr);tail(D.tr)
        if(length(rm.time.stamp)!= 0){ # i.e. some observations were flagged!
          # observations just prior to training window
          tack.on.time.stamp <- which(rownames(D) == rownames(D.tr)[1]) - 1 
          # only want those that were never flagged
          tack.on.options <- rownames(D)[which(!rownames(D)[1:tack.on.time.stamp] %in%
                                                 flagged.all)]
          # the latest obs (i.e. most recent) in tack.on.options
          tack.on.us <- tack.on.options[((length(tack.on.options) + 1) -
                              length(rm.time.stamp)):length(tack.on.options)] 
          length(tack.on.us) == length(rm.time.stamp)
          # Make sure D[tack.on.us,] all proceed training window
          # tail(D[tack.on.us,1:3]);head(D.tr[,1:3])
          D.tr <- rbind(D[tack.on.us,], D.tr)	
          # Now remove the recently flagged
          rm.rows <- which(rownames(D.tr) %in% rm.time.stamp)
          # PREVIOUS ERROR: rm.rows=which(rownames(D.m)%in%rm.time.stamp),
          # i.e. I was using rows from D.m instead of D.tr!!!
          #head(D.tr[rm.rows,]);head(flagged.current )
          D.tr <- D.tr[-rm.rows,]				
        }
        dim(D.tr)[1] == train.obs.num	# so, always the same number of
                                      # observations (prior to outlier removal),
                                      # but might span a longer amount of time
        # print(paste("Training window spans: ",
        #             round(difftime(rownames(D.tr[dim(D.tr)[1],]),
        #                            rownames(D.tr[1,]), units = "days"), 2),
        #             " days", sep = ""))
        print(paste("Training Window: ",
                    rownames(D.tr)[1],
                    " thru ",
                    rownames(D.tr)[dim(D.tr)[1]],
                    "; Length = ",
                    round(difftime(rownames(D.tr)[dim(D.tr)[1]],
                                   rownames(D.tr)[1], units = "days"), 1),
                    " days", sep = ""))
      }	
      #----
      
      # Initialize Training window:
      #----
      if(rm.flagged == FALSE | i == 1){
        
        D.tr <- D[train.first:train.last,]
        if(!is.null(burn) & i == 1){
          # In dynamic cases, we will need to burn the first few NA rows
          D.tr <- D.tr[-burn,] 
          if("p" %in% thrsh.type.i){
            Results[["p"]] <- Results[["p"]][-burn,]
          }
          if("np" %in% thrsh.type.i){
            Results[["np"]] <- Results[["np"]][-burn,]
          }
        }
       
        print(paste("Training Window: ",
                    rownames(D.tr)[1],
                    " thru ",
                    rownames(D.tr)[dim(D.tr)[1]],
                    "; Length = ",
                    round(difftime(rownames(D.tr)[dim(D.tr)[1]],
                                   rownames(D.tr)[1], units = "days"), 1),
                    " days", sep = ""))
      }
      #head(D.tr);tail(D.tr)
      #----
      
      #Remove outliers 
      #----
      rm.us <- unique(as.vector(unlist(apply(as.data.frame(D.tr), 2,
                                             outlier.fnc))))
      rm.us <- rm.us[-which(is.na(rm.us))]
      if(length(rm.us) > 0)D.tr <- D.tr[-rm.us,]
      # print(paste("Number of removed outliers: ",length(rm.us)))
      # print(paste("Number of observations in training window: ",
      #              dim(D.tr)[1]))
      #----
      
      # Monitoring Window
      #----	
      monitor.first <- train.last + 1
      monitor.last <- monitor.first + train.update.freq - 1
      monitor.last <- ifelse(monitor.last <= dim(D)[1],
                             monitor.last,
                             dim(D)[1]) 
      D.m <- D[monitor.first:monitor.last,] 
      print(paste("Monitor Window: ",
                  rownames(D.m)[1],
                  " thru ",
                  rownames(D.m)[dim(D.m)[1]],
                  "; Length = ",
                  round(difftime(rownames(D.m)[dim(D.m)[1]],
                                 rownames(D.m)[1],
                                 units="days"), 1),
                  " days", sep = ""))			
      #head(D.m);tail(D.m)
      #----
      
      # Scale Training Data:
      #----
      Mean <- apply(D.tr, 2, function(x) mean(x, na.rm = TRUE))
      SD <- apply(D.tr, 2, function(x) sd(x, na.rm = TRUE))
      X <- scale(D.tr)
      p <- dim(X)[2]
      n <- dim(X)[1]		
      #----
      
      #-------
      if(method == "PCA"){
        
        # Determine Optimal number of components (comps):
        #-------
        {
          R <- cor(X, use = "pairwise.complete.obs")
          eigenR <- eigen(R)
          evalR <- eigenR$values
          evecR <- eigenR$vectors
          prop.var <- as.matrix(cumsum(evalR) / sum(evalR) * 100)	
          comps <- which(prop.var - (var.amnt.pca * 100) > 0)[1]
        }
        if("p" %in% thrsh.type.i){
          Results[["p"]][rownames(D.m),"comps"] <- rep(comps,
                                          length(monitor.first:monitor.last))	
          Results[["p"]][rownames(D.m),"comps.prop.var"] <- rep(prop.var[comps],
                                          length(monitor.first:monitor.last))  
        }  
        if("np" %in% thrsh.type.i){
          Results[["np"]][rownames(D.m),"comps"] <- rep(comps,
                                          length(monitor.first:monitor.last))
          Results[["np"]][rownames(D.m),"comps.prop.var"] <- rep(prop.var[comps],
                                          length(monitor.first:monitor.last))
        }  
        #head(Results[[1]][rownames(D.m),])
        #-------
        
        # Training:
        #-------
        {
          P <- evecR[, 1:comps, drop = FALSE] # Transformation matrix 
          if(comps == 1){
            Lambda <- evalR[1]
          }else{	
            Lambda <- diag(evalR[1:comps])
          }
          PCs <- X %*% P
          # head(PCs); cor(PCs)	
          X.hat <- PCs %*% t(P)
          # Residual Matrix
          E <- X - X.hat
          SPEs <- diag(E %*% t(E))
          T2s <- diag(PCs %*% solve(Lambda) %*% t(PCs))
        }
        if(i == 1){
        # Store this initial window of training SPE and T2 values to create a
        #     timeseries plot that starts w/ these values until the end of the
        #     initial training window, where testing SPE & T2 values can then
        #     be plotted.    
          if("p" %in% thrsh.type.i){
            Results[["p"]][rownames(D.tr), c("SPE","T2")] <- cbind(SPEs,T2s)
          }
          if("np" %in% thrsh.type.i){
            Results[["np"]][rownames(D.tr), c("SPE","T2")] <- cbind(SPEs,T2s)
          }
        }
        #head(Results[[1]][rownames(D.tr),])
        #-------
        
        # Thresholds:
        #-------
        {
          # SPE 
          #---
          # Parametric: (Jackson and Mudholkar 1979)
          if("p" %in% thrsh.type.i){
            Thetas <- array(dim = 3)
            for(ii in 1:length(Thetas)){
              Thetas[ii] <- sum((evalR[(comps + 1):length(evalR)]) ^ ii)	
            }
            h0 <- 1 - (2 / 3 * Thetas[1] * Thetas[3] / Thetas[2] ^ 2)
            c.alpha <- qnorm(1 - alpha)
            term1 <- h0 * c.alpha * sqrt(2 * Thetas[2]) / Thetas[1]
            term2 <- Thetas[2] * h0 * (h0 - 1) / Thetas[1] ^ 2
            SPE.lim.p <- Thetas[1] * (term1 + 1 + term2) ^ (1 / h0)
            Results[["p"]][rownames(D.m),"SPE.thrsh"] <- rep(SPE.lim.p,
                                          length(monitor.first:monitor.last))
          }
          # Nonparametric:
          if("np" %in% thrsh.type.i){
            SPE.np.dens <- density(SPEs,
                                   bw = "SJ", # Sheather Jones
                                   kernel = "gaussian",
                                   from = 0)
            SPE.lim.np <- quantile(SPE.np.dens, 1 - alpha)
            Results[["np"]][rownames(D.m),"SPE.thrsh"] <- rep(SPE.lim.np,
                                          length(monitor.first:monitor.last))
          }
          if(length(thrsh.type.i) == 1){
            SPE.lim <- ifelse(thrsh.type.i == "p", SPE.lim.p, SPE.lim.np)
            # Needed to flagged observations
          }
          #---
          
          # T2:
          #---
          # Parametric:
          if("p" %in% thrsh.type.i){
            # if T2 > T2.lim then abnormal
            T2.lim.p <- ((n ^ 2 - 1) * comps) / 
              (n * (n - comps)) * qf((1 - alpha), comps, n - comps) 
            Results[["p"]][rownames(D.m),"T2.thrsh"] <- rep(T2.lim.p,
                                          length(monitor.first:monitor.last))
          }
          # Nonparametric:
          if("np" %in% thrsh.type.i){
            T2s.np.dens <- density(T2s,
                                   bw = "SJ",
                                   kernel = "gaussian",
                                   from = 0)
            T2.lim.np <- quantile(T2s.np.dens, 1 - alpha)
            Results[["np"]][rownames(D.m),"T2.thrsh"] <- rep(T2.lim.np,
                                          length(monitor.first:monitor.last))
          }
          if(length(thrsh.type.i) == 1){
            T2.lim <- ifelse(thrsh.type.i == "p", T2.lim.p, T2.lim.np)
            # Needed to flagged observations
          }
          #---
        }  
        #-------
        
        # Online:
        #-------
        {
          X.new <- scale(D.m, center = Mean, scale = SD)
          spe.online <- array()
          t2.online <- array()
          #j=1
          for(j in 1:dim(X.new)[1]){
            xnew <- as.vector(X.new[j,])
            xproj <- xnew %*% P		
            xhat <- xproj %*% t(P)
            r <- t(xnew - xhat)
            SPE.new <- t(r) %*% r
            spe.online[j] <- SPE.new
            T2.new <- t(xnew) %*% P %*% solve(Lambda) %*% t(P) %*% xnew
            t2.online[j] <- T2.new
          }
        }
        if("p" %in% thrsh.type.i){
          Results[["p"]][rownames(D.m),"SPE"] <- spe.online
          Results[["p"]][rownames(D.m),"T2"] <- t2.online  
        }
        if("np" %in% thrsh.type.i){
          Results[["np"]][rownames(D.m),"SPE"] <- spe.online
          Results[["np"]][rownames(D.m),"T2"] <- t2.online  
        }
        # head(Results[[1]][rownames(D.tr),])
        # head(Results[[1]][rownames(D.m),])
        #-------
      }
      #-------

      # KPCA
      #----
      if(method == "KPCA"){
        
        #Training:
        #-------
        {	
          X.df <- as.data.frame(X)
          sig.rbf <- 1 / (kern.g * (kern.sigma ^ 2) * dim(X.df)[2]) 
          # Gaussian Radial Basis Kernel
          rbf <- kernlab::rbfdot(sigma = sig.rbf) 
          # NOTE: for rbfdot 'sigma' is sigma= (1/c) where 'c' is that of Lee 
          #     et al 2004, yielding our sig.rbf. See ?rbfdot in package
          #     "kernlab". ERROR: REQUIRE "kernlab" IN FUNCTION
          K <- kernelMatrix(rbf, as.matrix(X.df));#min(K);max(K)
          I <- diag(dim(K)[1])
          E <- rep((dim(K)[1]) ^ (-1 / 2), dim(K)[1])
          K.c <- (I - E %*% t(E)) %*% K %*% (I - E %*% t(E))
          Eig.K <- eigen(K.c)
          e.val <- Eig.K$values
          e.vec <- Eig.K$vectors 
          # Deal with numerical eigenval issues:
          #---
          if(class(e.val) == "complex"){
            if(max(abs(Im(e.val))) > 10 ^ -9){
              stop("Complex Eig.K$values")
            }else{
              e.val <- Re(e.val)
              e.vec <- Re(e.vec)
            }  
          }
          chk <- which(e.val < 0)
          if(length(chk) > 0){
            if(sum(max(abs(e.val[chk]))) > 10 ^ -9){
              stop("Negative values in Eig.K$values")
            }else{
              e.val[chk] <- abs(e.val[chk])
            }
          } 
          #---
          n.tilde <- which(cumsum(e.val) / sum(e.val) >= 0.99)[1]
          ys <- t(apply(e.vec[,1:n.tilde], 1, function(x){ 
            sqrt(e.val[1:n.tilde]) * x
            })) # dim(ys) (see Ham (6))
          #round(apply(ys,2,mean),6); # should be all 0's
          
          # Number of components:
          #----
          if(fix.parameters)comps <- 2 # Ge_etal_2009!
          if(!fix.parameters){
            ave.eig <- mean(e.val)
            ave.eig
            # round(e.val,9)
            (comps.ave <- length(which(e.val >= ave.eig)))
            prop.var <- as.matrix(cumsum(e.val) / sum(e.val) * 100)			
            (comps.var <- which(prop.var - (var.amnt.kpca * 100) > 0)[1])
            (comps <- ifelse(ave.comps == TRUE, comps.ave, comps.var))
            if(comps > n.tilde){
              print("Warning: more KPCA's than dim(F)")
              print(paste("comps = ",
                          comps,
                          "; n.tilde = ",
                          n.tilde,
                          sep = ""))
              if(comps.var < n.tilde){
                comps <- comps.var
              }else{
                comps <- n.tilde - 1	
              }
            }
            if("p" %in% thrsh.type.i){
              Results[["p"]][rownames(D.m), "comps.ave"] <- rep(comps.ave,
                                          length(monitor.first:monitor.last))
              Results[["p"]][rownames(D.m), "comps.ave.prop.var"] <- rep(prop.var[comps.ave],
                                          length(monitor.first:monitor.last))
              Results[["p"]][rownames(D.m), "comps"] <- rep(comps.var,
                                          length(monitor.first:monitor.last))	
              Results[["p"]][rownames(D.m), "comps.prop.var"] <- rep(prop.var[comps.var],
                                          length(monitor.first:monitor.last))
            }
            if("np" %in% thrsh.type.i){
              Results[["np"]][rownames(D.m), "comps.ave"] <- rep(comps.ave,
                                          length(monitor.first:monitor.last))	
              Results[["np"]][rownames(D.m), "comps.ave.prop.var"] <- rep(prop.var[comps.ave],
                                          length(monitor.first:monitor.last))
              Results[["np"]][rownames(D.m), "comps"] <- rep(comps.var,
                                          length(monitor.first:monitor.last))	
              Results[["np"]][rownames(D.m), "comps.prop.var"] <- rep(prop.var[comps.var],
                                          length(monitor.first:monitor.last))
            }
          }
          #----
          
          #----
          Lambda <- (1 / n) * diag(e.val[1:comps])
          T2s <- diag(ys[,1:comps] %*% solve(Lambda) %*% t(ys[,1:comps]))			
          head(T2s)
          SPEs <- diag(ys[,1:n.tilde] %*% t(ys[,1:n.tilde])) - 
            diag(ys[,1:comps] %*% t(ys[,1:comps]))
          #----
        }
        if(i == 1){
          # Store this initial window of training SPE and T2 values to create a
          #     timeseries plot that starts w/ these values until the end of
          #     the initial training window, where testing SPE & T2 values can 
          #     then be plotted.    
          if("p" %in% thrsh.type.i){
            Results[["p"]][rownames(D.tr), c("SPE","T2")] <- cbind(SPEs, T2s)
          }
          if("np" %in% thrsh.type.i){
            Results[["np"]][rownames(D.tr), c("SPE","T2")] <- cbind(SPEs, T2s)
          }
        }  
        #-------
        
        # Thresholds:
        #-------
        {
          # SPE 
          #------
          {
            # Parametric:
            #---
            if("p" %in% thrsh.type.i){	
              a <- mean(SPEs)
              b <- var(SPEs)
              g <- b / (2 * a)
              h <- (2 * a ^ 2) / b 		
              SPE.lim.p <- g * qchisq(p = 1 - alpha, df = h)
              # Check:
              {
                # dev.new(width = 6, height = 6, noRStudioGD = TRUE)
                # hist(SPEs,
                #      breaks = "FD",
                #      freq = FALSE,
                #      xlim = c(0, max(max(SPE.lim.p), max(SPEs))))
                # stat.val <- SPEs
                # c <- var(stat.val) / (2 * mean(stat.val))
                # v <- (2 * mean(stat.val) ^ 2 / var(stat.val)) # ceiling
                # rng <- seq.int(0, 2, length.out = 1000)
                # dens <- 1 / c * dchisq(rng * (1 / c), df = v)
                # lines(rng, dens, col = 2)
                # abline(v = SPE.lim.p, lty = 2, col = 2)
                # sapply(SPE.lim.p, function(x)length(which(SPEs >= x)) / n)
              }
              Results[["p"]][rownames(D.m), "SPE.thrsh"] <- rep(SPE.lim.p,
                                          length(monitor.first:monitor.last))
            }
            #---
            
            # Nonparametric:
            #---
            if("np" %in% thrsh.type.i){
              SPE.np.dens <- density(SPEs,
                                     bw = "SJ",
                                     kernel = "gaussian",
                                     from = 0)
              SPE.lim.np <- quantile(SPE.np.dens, 1 - alpha)
              # check:
              {
                ## NOTE: Run parametric check (above) first, then add:
                # lines(SPE.np.dens,col = 4)
                # abline(v = SPE.lim.np, col = 4, lty = 3)
                # sapply(SPE.lim.np, function(x)length(which(SPEs >= x)) / n)
              }
              #---
              Results[["np"]][rownames(D.m), "SPE.thrsh"] <- rep(SPE.lim.np,
                                          length(monitor.first:monitor.last))
            }
            #---
            
            if(length(thrsh.type.i) == 1){
              SPE.lim <- ifelse(thrsh.type.i == "p", SPE.lim.p, SPE.lim.np)
            }
          }
          #------
          
          # T2:
          #------
          {
            # Parametric:
            #---
            if("p" %in% thrsh.type.i){	
              T2.lim.p <- (n ^ 2 - 1) * comps / 
                n * (n - comps) * qf(1 - alpha, comps, n - comps) 
              hld2 <- head(T2s)
              # check:
              {
                # dev.new(width = 6, height = 6, noRStudioGD = TRUE)
                # hist(T2s, breaks = "FD", freq = FALSE)
                # hist(T2s, breaks = "FD", freq = FALSE,
                #      xlim = c(0, max(max(T2.lim.p), max(T2s))))
                # stat.val <- T2s
                # b <- max(stat.val)
                # rng <- seq.int(.0001, b, length.out = 1000)
                # d <- comps
                # dens <- df(rng * n * (n - d) / d * (n ^ 2 - 1), d, n - d)
                # lines(rng, dens, col = 2)
                # abline(v = T2.lim.p, lty = 2, col = 2)
                # sapply(T2.lim.p, function(x)length(which(T2s >= x)) / n)
              }
              #---
              Results[["p"]][rownames(D.m),"T2.thrsh"] <- rep(T2.lim.p,
                                          length(monitor.first:monitor.last))
            }
            #---
            
            # Nonparametric:
            #---
            if("np" %in% thrsh.type.i){
              T2s.np.dens <- density(T2s,
                                     bw = "SJ",
                                     kernel = "gaussian",
                                     from = 0)
              T2.lim.np <- quantile(T2s.np.dens, 1 - alpha)
              # check:
              {
                # NOTE: Run parametric check (above) first, then add:
                # lines(T2s.np.dens, col = 4)
                # abline(v = T2.lim.np, col = 4, lty = 3)
                # sapply(T2.lim.np, function(x)length(which(T2s >= x)) / n)
              }  
              #---
              Results[["np"]][rownames(D.m),"T2.thrsh"] <- rep(T2.lim.np,
                                          length(monitor.first:monitor.last))
            }
            #---
            
            if(length(thrsh.type.i) == 1){
              T2.lim <- ifelse(thrsh.type.i == "p", T2.lim.p, T2.lim.np)
            }
          }
          #-------
        }
        #------
        
        # Online:
        #-------
        {	
          kernel.vec <- function(x, y, sig = 1){
            # x <- dt[1,]; y <- dt[2,]; sig <- 0.2	
            x <- as.matrix(x); y <- as.matrix(y)
            exp(-(x-y) %*% t(x-y) * sig)
          }
          X.new <- scale(D.m, center = Mean, scale = SD)
          em.online <- matrix(nrow = dim(X.new)[1], ncol = comps) 
          spe.online <- array()
          t2.online <- array()
          for(k in 1:dim(X.new)[1]){
            # print(k)
            x.new <- t(as.vector(X.new[k,]))
            k.vec <- apply(X.df, 1, function(x){
              kernel.vec(x.new, t(x), sig.rbf)
              })	
            k.vec.c <- k.vec - rep(1 / n, n) %*% K %*% (I - E %*% t(E))
            y.new <- 1 / sqrt(e.val) * t(e.vec) %*% t(k.vec.c)
            # y.new[1:comps]
            em.online[k,] <- y.new[1:comps]
            # Check: that points in training set would map properly
            #----
            {
              # p.rng <- 1:dim(ys)[1]
              # plot(ys[p.rng,1:2], col = rainbow(length(p.rng)))
              # for(g in 1:100){}
              #   tst <- g
              #   x.new <- X.df[tst,]
              #   k.vec <- apply(X.df, 1, function(x){
              #     kernel.vec(x.new, t(x), sig.rbf)
              #     })	
              #   k.vec.c <- k.vec - rep(1 / n, n) %*% K %*% (I-E %*% t(E))
              #   y.new <- 1 / sqrt(e.val) * t(e.vec) %*% t(k.vec.c)
              #   y.new[1:2];ys[1,1:2]
              #   points(ys[tst,1], ys[tst,2], col = "grey", pch = 19)
              #   points(y.new[1],
              #          y.new[2],
              #          col = rainbow(length(p.rng))[g],
              #          pch = 19)
              # }
              # NICE!
            }	
            #----	
            T2.new <- t(y.new[1:comps]) %*% solve(Lambda) %*% y.new[1:comps]
            t2.online[k] <- T2.new
            SPE.new <- t(y.new)[1:n.tilde] %*% y.new[1:n.tilde] - 
              t(y.new)[1:comps] %*% y.new[1:comps]
            spe.online[k] <- SPE.new
          }
          # Check:
          # hist(SPEs, xlim = c(0, max(max(SPEs), SPE.new)), breaks = "FD")
          # abline(v = spe.online, col = 2)
          # hist(T2s)
          # abline(v = t2.online, col = 2)
          # # Do any of the levels considered trigger a warning?
          # length(which(spe.online > SPE.lim)) / dim(X.new)[1]
          # length(which(t2.online > T2.lim)) / dim(X.new)[1]
        }
        if("p" %in% thrsh.type.i){
          Results[["p"]][rownames(D.m), "SPE"] <- spe.online
          Results[["p"]][rownames(D.m),"T2"] <- t2.online  
        }
        if("np" %in% thrsh.type.i){
          Results[["np"]][rownames(D.m),"SPE"] <- spe.online
          Results[["np"]][rownames(D.m),"T2"] <- t2.online  
        }
        # head(Results[[1]][rownames(D.tr),])
        # head(Results[[2]][rownames(D.m),])
        #-------
      }	
      #----
      
      # LLE
      #----
      if(method == "LLE"){
        
        #Determine d and k:
        #----------
        if(KDsearch){
          kd.matrix <- matrix(nrow = length(d.rng), ncol = length(k.rng))
          rv.start.time <- proc.time()
          for(j in 1:length(d.rng)){
            # lle::calc_k - The number of neighbours belonging to the smallest
            #     value of ρ should be chosen. ERROR: REQUIRE "lle" IN FUNCTION
            #     Could be run in parallel: ,parallel = TRUE, cpus = 2
            if(parallel.lle){
              ks <- calc_k(X, m = d.rng[j], k.rng[1], k.rng[length(k.rng)],
                           plotres = FALSE, parallel = TRUE, cpus = cpus)
            }
            if(!parallel.lle){
              ks <- calc_k(X, m = d.rng[j], k.rng[1], k.rng[length(k.rng)],
                           plotres = FALSE)
            }
            if(dim(ks)[1] < dim(kd.matrix)[2]){
              ks <- c(ks[,2], rep(NA, dim(kd.matrix)[2] - dim(ks)[1]))
              kd.matrix[j,] <- ks
            }else{
              kd.matrix[j,] <- ks[,2]
            }
          }
          rv.end.time <- proc.time()
          rv <- min(kd.matrix, na.rm = TRUE)
          kd.best <- which(kd.matrix == rv, arr.ind = TRUE)
          # if there were a tie, this just takes the first (which will
          #     correspond to a lower d first, then a lower k)
          d <- d.rng[kd.best[1,1]] 
          k <- k.rng[kd.best[1,2]]
          #d <- 14; k <- 45
        }
        if(!KDsearch){
          rv.start.time <- proc.time()
          hld <- calc_k(X, m = d, kmin = k, kmax = k, plotres = FALSE) 
          rv <- hld[[2]]
          rv.end.time <- proc.time()
        }
        #----------
        if("p" %in% thrsh.type.i){
          Results[["p"]][rownames(D.m), "k"] <- rep(k,
                                          length(monitor.first:monitor.last))
          Results[["p"]][rownames(D.m), "d"] <- rep(d,
                                          length(monitor.first:monitor.last))	
          Results[["p"]][rownames(D.m), "resid.var"] <- rep(rv,
                                          length(monitor.first:monitor.last))	
          rv.calc.time <- rv.end.time - rv.start.time
          Results[["p"]][rownames(D.m), "rv.elapse.sec"] <- rep(rv.calc.time[3],
                                          length(monitor.first:monitor.last))	  
        }
        if("np" %in% thrsh.type.i){
          Results[["np"]][rownames(D.m), "k"] <- rep(k,
                                          length(monitor.first:monitor.last))	
          Results[["np"]][rownames(D.m), "d"] <- rep(d,
                                          length(monitor.first:monitor.last))	
          Results[["np"]][rownames(D.m), "resid.var"] <- rep(rv,
                                          length(monitor.first:monitor.last))	
          rv.calc.time <- rv.end.time - rv.start.time 
          Results[["np"]][rownames(D.m), "rv.elapse.sec"] <- rep(rv.calc.time[3],
                                          length(monitor.first:monitor.last))	  
        }
        # head(Results[rownames(D.m),])
        #----------
        
        # Training:
        #----------
        {
          # Ys:
          #-------
          {
            NNs <- find_nn_k(X, k, iLLE = FALSE)
            Weights <- find_weights(NNs, X, d, ss = FALSE, id = FALSE)
            W <- Weights$wgts; dim(W)
            I <- diag(dim(W)[1])		
            M <- t(I - W) %*% (I - W)
            Eig.M <- eigen(M)
            
            #-------------
            e.vecs <- Eig.M$vector[,-dim(M)[2]] #remove last
            e.vecs <- e.vecs[,dim(e.vecs)[2]:1]
            e.vals <- Eig.M$values[-dim(M)[2]]
            e.vals <- e.vals[length(e.vals):1]
            
            # Deal with numerical eigenval issues:
            #---
            if(class(e.vals) == "complex"){
              if(max(abs(Im(e.vals))) > 10 ^ -9){
                stop("Complex eigenvalues")
              }else{
                e.vals <- Re(e.vals)
                e.vecs <- Re(e.vecs)
              }  
            }
            chk <- which(e.vals < 0)
            if(length(chk) > 0){
              if(sum(max(abs(e.vals[chk]))) > 10 ^ -9){
                stop("Negative values in eigenvalues")
              }else{
                e.vals[chk] <- abs(e.vals[chk])
              }
            } 
            #---
            e.vals.matrix <- rbind(e.vals, e.vals)
            e.vals.matrix <- e.vals.matrix[rep(1,n),]
            # e.vecs the LLE embedding see (A.8) (or equivalently the u_ki's
            #     of (A.11))
            ys <- e.vecs[,1:d]
            # scale to get the KPCA equivalent
            ys.klle <- sqrt(e.vals.matrix) * (e.vecs) 
            # (see second line in last Eq of Appendix)
            #-------------
          }
          #-------		
          
          # SPEs:
          #-------
          n.tilde <- which(cumsum(e.vals) / sum(e.vals) >= 0.99)[1]
          SPEs <- diag(ys.klle[,1:n.tilde] %*% t(ys.klle[,1:n.tilde])) - 
            diag(ys.klle[,1:d] %*% t(ys.klle[,1:d]))
          # Check:
          {
            # hist(SPEs, 
            #      breaks = "FD",
            #      freq = FALSE, 
            #      xlim = c(min(SPEs), max(SPEs) + 2 * sd(SPEs)))
            # stat.val  <- SPEs
            # c <- var(stat.val) / (2 * mean(stat.val))
            # v <- 2 * mean(stat.val) ^ 2 / var(stat.val) #ceiling
            # rng <- seq.int(0, max(SPEs) + sd(SPEs), length.out = 1000)
            # dens <- 1 / c * dchisq(rng / c, df = v)
            # lines(rng, dens, col = 2)
          }
          #-------
          
          # T2s:
          #-------
          {
          if(d > 1){
            Lambda <- diag(e.vals[1:d] / (n - 1))
            T2s <- diag(ys.klle[,1:d] %*% solve(Lambda) %*% t(ys.klle[,1:d]))
          }else{
            Lambda <- e.vals[1:d] / (n - 1)
            T2s <- diag(ys.klle[,1:d] %*% solve(Lambda) %*% t(ys.klle[,1:d]))
          }
            # Check: 
            {
              # hist(T2s, breaks = "FD", freq = FALSE)
              # rng <- seq.int(0, max(T2s), length.out = 1000)
              # dens <- df(rng * n * (n-d) / (d * (n ^ 2 - 1)), d, n - d)
              # lines(rng, dens, col = 2)

              # # Lambda should be the variance of the ys:
              # if(d == 1) var(ys.klle[,1:d]); Lambda
              # if(d > 1)sum(diag(var(ys.klle[,1:d])) - diag(Lambda))
            }
          }	
          #-------
        }
        if(i == 1){
          # Store this initial window of training SPE and T2 values to create
          #     a timeseries plot that starts w/ these values until the end of
          #     the initial training window, where testing SPE & T2 values can
          #     then be plotted.    
          if("p" %in% thrsh.type.i){
            Results[["p"]][rownames(D.tr), c("SPE", "T2")] <- cbind(SPEs, T2s)
          }
          if("np" %in% thrsh.type.i){
            Results[["np"]][rownames(D.tr), c("SPE", "T2")] <- cbind(SPEs, T2s)
          }
        }  
        #----------
        
        # Thresholds:
        #-------
        {
          # SPE 
          #------
          {
            # Parametric:
            #---
            if("p" %in% thrsh.type.i){	
              # Parametric (Lee etal 2004):
              #---
              a <- mean(SPEs)
              b <- var(SPEs)
              g <- b / (2 * a)
              h <- 2 * a ^ 2 / b	
              SPE.lim.p <- g * qchisq(p = 1 - alpha, df = h)
              # Check:
              {
                # hist(SPEs, breaks = "FD", freq = FALSE,
                #      xlim = c(0, max(max(SPE.lim.p), max(SPEs))))
                # stat.val <- SPEs
                # c <- var(stat.val) / (2 * mean(stat.val))
                # v <- 2 * mean(stat.val) ^ 2 / var(stat.val) #ceiling
                # rng <- seq.int(0, max(stat.val), length.out = 1000)
                # dens <- 1 / c * dchisq(rng / c, df = v)
                # lines(rng, dens, col = 2)
                # abline(v = SPE.lim.p, lty = 2, col = 2)
                # sapply(SPE.lim.p, function(x) length(which(SPEs >= x)) / n)
              }
              #---
              Results[["p"]][rownames(D.m), "SPE.thrsh"] <- rep(SPE.lim.p,
                                          length(monitor.first:monitor.last))
            }
            #---
            
            # Nonparametric:
            #---
            if("np" %in% thrsh.type.i){
              # Nonparametric:
              #---
              SPE.np.dens <- density(SPEs, bw = "SJ",
                                     kernel = "gaussian", from = 0)
              SPE.lim.np <- quantile(SPE.np.dens, 1 - alpha)
              # Check:
              {
                # lines(SPE.np.dens, col = 4)
                # abline(v = SPE.lim.np, col = 4, lty = 3)
                # sapply(SPE.lim.np, function(x) length(which(SPEs >= x)) / n)
              }  
              #---
              Results[["np"]][rownames(D.m), "SPE.thrsh"] <- rep(SPE.lim.np,
                                          length(monitor.first:monitor.last))
            }
            #---
            
            if(length(thrsh.type.i) == 1){
              SPE.lim <- ifelse(thrsh.type.i == "p", SPE.lim.p, SPE.lim.np)
            }
          }
          #------
          
          # T2:
          #------
          {
            # Parametric:
            #---
            if("p" %in% thrsh.type.i){	
              T2.lim.p <- (n ^ 2 - 1) * d / (n * (n - d)) *
                qf(1 - alpha, d, n - d) 
              # Check:
              {
                # hist(T2s, breaks = "FD", freq = FALSE,
                #      xlim = c(0, max(max(T2.lim.p), max(T2s))))
                # stat.val <- T2s
                # b <- max(stat.val)
                # rng <- seq.int(.0001, b, length.out = 1000)
                # dens <- df(rng * n * (n - d) / (d * (n ^ 2 - 1)), d, n - d)
                # lines(rng, dens, col = 2)
                # abline(v = T2.lim.p, lty = 2, col = 2)
                # sapply(T2.lim.p, function(x) length(which(T2s >= x)) / n)
              }
              Results[["p"]][rownames(D.m), "T2.thrsh"] <- rep(T2.lim.p,
                                          length(monitor.first:monitor.last))
            }
            #---
            
            # Nonparametric:
            #---
            if("np" %in% thrsh.type.i){
              T2s.np.dens <- density(T2s, bw = "SJ",
                                     kernel = "gaussian", from = 0)
              T2.lim.np <- quantile(T2s.np.dens, 1 - alpha)
              # Check:
              {
                # lines(T2s.np.dens, col = 4)
                # abline(v = T2.lim.np, col = 4, lty = 3)
                # sapply(T2.lim.np, function(x) length(which(T2s >= x)) / n)
              }  
              Results[["np"]][rownames(D.m), "T2.thrsh"] <- rep(T2.lim.np,
                                          length(monitor.first:monitor.last))
            }
            #---
            
            if(length(thrsh.type.i) == 1){
              T2.lim <- ifelse(thrsh.type.i == "p", T2.lim.p, T2.lim.np)
            }
          }
          #-------
        }
        #-------
        
        # Online:
        #----------
        {	
          X.new <- scale(D.m, center = Mean, scale = SD)
          em.online <- matrix(nrow = dim(X.new)[1], ncol = d) 
          spe.online <- array()
          t2.online <- array()
          for(j in 1:dim(X.new)[1]){
            # y.new:
            #---------
            {
              x.new <- as.vector(X.new[j,])
              dists <- sqrt(colSums((t(X) - x.new) ^ 2))
              NNs.xnew.rows <- order(dists)[1:k]
              NNs.xnew <- X[NNs.xnew.rows,]
              # Check: How similar (on 1st two dimensions!):
              {
                # plot(as.matrix(X[,1:2]), type = "p")
                # points(as.matrix(NNs.xnew[,1:2]), col = 4, pch = 19)
                # points(x.new[1], x.new[2], col = 2, pch = 19)
              }
              ws.i <- array()
              Q <- matrix(nrow = k, ncol = k)
              xi <- as.numeric(x.new)
              nns <- NNs.xnew.rows
              for(jj in 1:dim(Q)[1]){
                for(q in 1:dim(Q)[1]){
                  Q[jj, q] <- t(xi - X[nns[jj],]) %*% (xi - X[nns[q],])
                }
              }
              for(jj in 1:dim(Q)[1]){
                Q.new <- Q + diag(k) * (0.1 ^ 2 / k) * sum(diag(Q))
                num <- sum(solve(Q.new)[jj,])
                den <- sum(solve(Q.new))
                ws.i[jj] <- num / den
              }
              sum(ws.i) == 1
              
              if(d > 1){
                y.new <- t(ys[NNs.xnew.rows,]) %*% ws.i
              }
              if(d == 1){
                y.new <- t(ys[NNs.xnew.rows]) %*% ws.i
              }
              # Check: How similar on first two dimensions of embedding?
              {
                # if(d > 1){
                #   plot(as.matrix(ys[,1:2]), type = "p")
                #   points(as.matrix(ys[NNs.xnew.rows,1:2]), col = 4, pch = 19)
                #   points(y.new[1],y.new[2], col = 2, pch = 19)
                # }
                # if(d == 1){
                #   plot(ys.klle.Kt, rep(1, length(ys)), type = "p")
                #   points(ys[NNs.xnew.rows],
                #          rep(1, length(NNs.xnew.rows)), col = 4, pch = 19)
                #   points(y.new[1], 1, col = 2, pch = 19)
                # }
              }
            }
            #---------
            
            # T2 and SPE:
            #---------
            {
              K.new <- rep(0, n) # plug in 0's for non-neighbors
              K.new[NNs.xnew.rows] <- ws.i
              y.ks <- t(e.vecs) %*% K.new	
              y.new.klle <- sqrt(e.vals) * y.ks
              # y.new[1:d]; y.new.klle[1:d]
              # plot(ys.klle[,1:2])
              # points(y.new.klle[1], y.new.klle[2], col = 2, pch = 19)
              T2.new <- t(y.new.klle[1:d]) %*%
                             solve(Lambda) %*%
                             y.new.klle[1:d]
              t2.online[j] <- T2.new
              # Check:
              {
                # hist(T2s, breaks = "FD", freq = F,
                #      xlim = c(0, max(max(T2.lim.p), max(T2s))))
                # abline(v = T2.lim[1], col = 4, lty = 2)
                # abline(v = t2.online, col = 2)
                # length(which(t2.online >= T2.lim[1])) / (dim(X.new)[1])
                # index(X.new[which(t2.online >= T2.lim[1]),])
              }  
              SPE.new <- t(y.new.klle)[1:n.tilde] %*% y.new.klle[1:n.tilde] - 
                t(y.new.klle)[1:d] %*% y.new.klle[1:d]
              spe.online[j] <- SPE.new
              # Check:
              {
                # hist(SPEs, breaks = "FD", freq = FALSE,
                #      xlim = c(0, max(c(SPEs, SPE.lim[1])) + 2 * sd(SPEs)))
                # abline(v = SPE.lim[1], col = 4, lty = 2)
                # abline(v = SPE.new.Kt, col = 2, lwd = 2)
                # abline(v = spe.online, col = 2, lwd = 2)
                # length(which(spe.online >= SPE.lim[1])) / (dim(X.new)[1])
              }
            }
            #---------
          }
        }
        if("p" %in% thrsh.type.i){
          Results[["p"]][rownames(D.m), "SPE"] <- spe.online
          Results[["p"]][rownames(D.m), "T2"] <- t2.online  
        }
        if("np" %in% thrsh.type.i){
          Results[["np"]][rownames(D.m), "SPE"] <- spe.online
          Results[["np"]][rownames(D.m), "T2"] <- t2.online  
        }
        # head(Results[[1]][rownames(D.tr),])
        # head(Results[[1]][rownames(D.m),])
        #----------
      }			
      #----
      
      if(!is.null(results.path)){
        save(Results, file=results.path)
      }
      
      # Flagged Observations:
      #----
      if(rm.flagged){
        # head(D.m[which(spe.online > SPE.lim),])
        SPE.flagged <- rownames(D.m)[which(spe.online > SPE.lim)]
        T2.flagged <- rownames(D.m)[which(t2.online > T2.lim)]		
        flagged.current <- union(SPE.flagged, T2.flagged)
        flagged.all <- c(flagged.current, flagged.all)
        if(any(is.na(flagged.all))){
          flagged.all <- flagged.all[-which(is.na(flagged.all))]
        }
        # head(flagged.all)
      }
      #----
      
      # Shift windows
      #----
      train.first <- train.first + train.update.freq	
      train.last <- train.last + train.update.freq
      #----
      
    }
    #--------------
  }
  #--------------
return(Results)
}
#----
