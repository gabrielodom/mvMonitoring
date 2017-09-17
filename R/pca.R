#' PCA for Data Scatter Matrix
#'
#' @description Calculate the principal component analysis for a data matrix,
#'   and also find the squared prediction error (SPE) and Hotelling's T2 test
#'   statistic values for each observation in this data matrix.
#'
#' @param data A centred-and-scaled data matrix or xts matrix
#' @param var.amnt The energy proportion to preserve in the projection, which
#'   dictates the number of principal components to keep. Defaults to 0.90.
#' @param ... Lazy dots for additional internal arguments
#'
#' @return A list of class "pca" with the following:
#' \itemize{
#'   \item{projectionMatrix -- }{the q eigenvectors corresponding to the q
#'     largest eigenvalues as a p x q projection matrix}
#'   \item{LambdaInv -- }{the diagonal matrix of inverse eigenvalues}
#'   \item{SPE -- }{the vector of SPE test statistic values for each of the n
#'     observations contained in "data"}
#'   \item{T2 -- }{the vector of Hotelling's T2 test statistic for each of the
#'     same n observations}
#' }
#'
#'
#' @details This function takes in a training data matrix, without the label
#'   column, and the energy preservation proportion, which defaults to 95
#'   percent per Kazor et al (2016). This proportion is the sum of the q largest
#'   eigenvalues divided by the sum of all p eigenvalues, where q is the number
#'   of columns of the p x q projection matrix P. This function then returns the
#'   projection matrix P, a diagonal matrix of the reciprocal eigenvalues
#'   (LambdaInv), a vector of the SPE test statistic values corresponding to the
#'   rows of the data matrix, and a T2 test statistic vector similar to the SPE
#'   vector.
#'
#'   This internal function is called by faultFilter().
#'
#' @seealso Called by: \code{\link{faultFilter}}.
#'
#' @export
#'
#' @examples
#' nrml <- mspProcessData(faults = "NOC")
#' scaledData <- scale(nrml[,-1])
#' pca(scaledData)
#'
pca <- function(data, var.amnt = 0.90, ...){
UseMethod("pca")
}

#' @keywords internal
#' @export
#'
#' @importFrom stats cor
#'
pca.matrix <- function(data, var.amnt = 0.90, ...){

        R <- cor(data, use = "pairwise.complete.obs")
        eigenR <- eigen(R)
        evalR <- eigenR$values
        evecR <- eigenR$vectors
        prop.var <- as.matrix(cumsum(evalR) / sum(evalR) * 100)

        if(var.amnt != 1){
        comps <- which(prop.var - (var.amnt * 100) > 0)[1]

        P <- as.matrix(evecR[, 1:comps]) # Transformation matrix

        Lambda <- diag(evalR[1:comps], ncol = length(1:comps))
        } else{
         P <- as.matrix(evecR) # Transformation matrix

        Lambda <- diag(evalR, ncol = length(evalR))
}

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
#' @importFrom stats cor
#'
pca.xts <- pca.matrix
