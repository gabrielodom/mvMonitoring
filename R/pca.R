#' Title
#'
#' @param var.amnt
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
pca <- function(data, var.amnt = 0.95, ...){
UseMethod("pca")
}

#' @keywords internal
#' @export
#'
#'
pca.matrix <- function(data, var.amnt = 0.95, ...){
  # This function takes in a training data matrix (without the label column)
  # and the energy preservation proportion (defaulting to 95% per Kazor et al
  # (2016)). This function returns a projection matrix, a diagonal matrix of
  # the reciprocal eigenvalues (LambdaInv), a vector of the SPE test statistic
  # values corresponding to the rows of the data matrix, and a T2 test
  # statistic vector similar to the SPE vector.

        R <- cor(data, use = "pairwise.complete.obs")
        eigenR <- eigen(R)
        evalR <- eigenR$values
        evecR <- eigenR$vectors
        prop.var <- as.matrix(cumsum(evalR) / sum(evalR) * 100)
        comps <- which(prop.var - (var.amnt * 100) > 0)[1]

        P <- as.matrix(evecR[, 1:comps]) # Transformation matrix

        Lambda <- diag(evalR[1:comps], ncol = length(1:comps))


        # Rotated Matrix
        PCs <- data %*% P

        # Reduced Matrix in Original Space
        X.hat <- PCs %*% t(P)

        # Residual Matrix
        E <- data - X.hat

        # Squared prediction error monitoring statistic
        SPEs <- diag(E %*% t(E))

        # Hotelling's T^2 monitoring statistic
        LambdaInv <- solve(Lambda)
        T2s <- diag(PCs %*% LambdaInv %*% t(PCs))

        object <- list(projectionMatrix = P,
                       LambdaInv = LambdaInv,
                       SPE = SPEs,
                       T2 = T2s)

        class(object) <- "pca"
        object
}


#' @keywords internal
#' @export
#'
#'
pca.xts <- pca.matrix
