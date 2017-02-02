#' Three-Dimensional Rotation and Scaling Matrix
#'
#' @description Render a 3-Dimensional projection matrix given positive or
#' negative degree changes in yaw, pitch, and / or roll and increment or
#' decrement feature scales.
#'
#' @param rot_angles a list or vector containg the rotation angles in the order
#' following: yaw, pitch, roll. Defaults to <0,0,0>.
#' @param scale_factors a list or vector containing the values by which to
#' multiply each dimension. Defaults to <1,1,1>.
#'
#' @return A 3 * 3 projection matrix of the degree and scale changes entered.
#'
#' @details See the function commentary of "rotate_3D" for a brief explination
#' of how these angles behave in scatterplot3d functionality (from package
#' scatterplot3d).
#'
#' This function is used only in data generation of the package vignette. This
#' function calls rotate3D().
#'
#' @export
#'
#' @examples
#' data("normal_switch_xts")
#' angles <- list(yaw = -10, pitch = 0, roll = 15)
#' featureScales <- c(0.2, 1, 5)
#' normal_switch_xts[,-1] %*% rotateScale3D(rot_angles = angles,
#'                                          scale_factors = featureScales)
rotateScale3D <- function(rot_angles = c(0,0,0),
                            scale_factors = c(1,1,1)){

  rot_angles <- unlist(rot_angles)
  yaw <- rot_angles[1]
  pitch <- rot_angles[2]
  roll <- rot_angles[3]

  scale_factors <- unlist(scale_factors)
  V <- diag(scale_factors, nrow = 3, ncol = 3)

  rotate3D(yaw, pitch, roll) %*% V
}
