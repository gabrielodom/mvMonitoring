#' Three-Dimensional Rotation Matrix
#'
#' @description Render a 3-Dimensional projection matrix given positive or
#'   negative degree changes in yaw, pitch, and / or roll.
#'
#' @param yaw z-axis change in degrees; look left (+) or right (-). Consider
#'   this a rotation on the x-y plane.
#' @param pitch y-axis change in degrees; look up (-) or down (+). Consider this
#'   a rotation on the x-z plane.
#' @param roll x-axis change in degrees; this change appears as if you touch
#'   head to shoulders: right roll (+) and left roll (-).
#'
#' @return A 3 x 3 projection matrix corresponding to the degree changes entered.
#'
#' @details When plotting with the package scatterplot3d, the default perpective
#'   is such that the pitch action appears as a roll while the roll action
#'   appears as a pitch.
#'
#'   This function is used only in data generation of the package vignette. This
#'   function is called by rotateScale3D().
#'
#' @seealso Called by: \code{\link{rotateScale3D}}.
#'
#' @export
#'
#' @examples
#' rotate3D(yaw = -10, pitch = 0, roll = 15)
#'
rotate3D <- function(yaw, pitch, roll){

  if(is.list(yaw)){
    yaw_ls <- yaw
    yaw <- yaw_ls[[1]]
    pitch <- yaw_ls[[2]]
    roll <- yaw_ls[[3]]
  }

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

  # For the full form of this multiplication, see wolfram alpha with this:
  # {{1,0, 0},{0,cos(a),-sin(a)},{0,sin(a),cos(a)}} *
  #     {{cos(b),0,sin(b)},{0,1,0},{-sin(b),0,cos(b)}} *
  #         {{cos(c),-sin(c),0},{sin(c),cos(c),0},{0,0,1}}

  Rz %*% Ry %*% Rx
}
