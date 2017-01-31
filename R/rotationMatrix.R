#' Three-Dimensional Rotation Matrix
#'
#' @description Render a 3-Dimensional projection matrix given positive or
#' negative degree changes in yaw, pitch, and / or roll.
#'
#' @param yaw z-axis change in degrees; look left (+) or right (-). Consider
#' this a rotation on the x-y plane.
#' @param pitch y-axis change in degrees; look up (-) or down (+). Consider
#' this a rotation on the x-z plane.
#' @param roll x-axis change in degrees; this change appears as if you touch
#' head to shoulders: right roll (+) and left roll (-).
#'
#' @return A 3 * 3 projection matrix of the degree changes entered.
#'
#' @details When plotting with the package scatterplot3d, the default perpective
#' is such that the pitch action appears as a roll while the roll action appears
#' as a pitch.
#'
#' @export
#'
#' @examples
rotate_3D <- function(yaw, pitch, roll){

  thetaX <- roll * pi / 180
  thetaY <- pitch * pi / 180
  thetaZ <- yaw * pi / 180

  Rx <- c(1,           0,            0,
          0, cos(thetaX), -sin(thetaX),
          0, sin(thetaX),  cos(thetaX))
  Rx <- matrix(Rx, ncol = 3, nrow = 3)

  Ry <- c( cos(thetaY),  0, sin(thetaY),
           0,            1,           0,
           -sin(thetaY), 0, cos(thetaY))
  Ry <- matrix(Ry, ncol = 3, nrow = 3)

  Rz <- c(cos(thetaZ), -sin(thetaZ), 0,
          sin(thetaZ),  cos(thetaZ), 0,
          0,            0,           1)
  Rz <- matrix(Rz, ncol = 3, nrow = 3)

  Rz %*% Ry %*% Rx
}
