# Playing with Rotation Matrices

######  2 Dimensions  #########################################################
# x <- seq(-10, 10, by = 0.1)
# y <- 0.05 * x ^ 2
# 
# # We have created a parabola. On seconds thought, we'd probably need a shape
# # that isn't symmetric. 
# y <- 0.005 * 2 ^ x
# 
# # Make sure to fix the plot window to square.
# par(pty = "s")
# plot(x,y)
# 
# # Now create a rotation matrix:
# R_2 <- function(angle){
#   theta <- angle * pi / 180
#   c(cos(theta), -sin(theta),
#     sin(theta), cos(theta)) %>% matrix(ncol = 2, nrow = 2)
# }
# R_2(90)
# 
# # Create the data frame to rotate:
# df <- data.frame(y = y, x = x)
# 
# # Rotate x and y clockwise by alpha degrees
# rot_df <- as.matrix(df) %*% R_2(45) %>% as.data.frame()
# plot(rot_df$V2, rot_df$V1)

######  3 Dimensions  #########################################################
# Create the rotation function
R_3 <- function(yaw, pitch, roll){
  # yaw - z-axis; look left (+) or right (-). Consider this a rotation on the
  #     x-y plane.
  # pitch - y-axis; look up (-) or down (+). Consider this a rotation on the
  #     x-z plane. NOTE: in scatterplot3d, the default perpective is such that
  #     this action appears as a roll. 
  # roll - x-axis; touch head to shoulders: right roll (+) and left roll (-).
  #     NOTE: in scatterplot3d, the default perpective is such that this action
  #     appears as a pitch.
  thetaX <- roll * pi / 180
  thetaY <- pitch * pi / 180
  thetaZ <- yaw * pi / 180
  Rx <- c(1,           0,            0,
          0, cos(thetaX), -sin(thetaX),
          0, sin(thetaX),  cos(thetaX)) %>% matrix(ncol = 3, nrow = 3)
  Ry <- c( cos(thetaY), 0, sin(thetaY),
                     0, 1,           0,
          -sin(thetaY), 0, cos(thetaY)) %>% matrix(ncol = 3, nrow = 3)
  Rz <- c(cos(thetaZ), -sin(thetaZ), 0,
          sin(thetaZ),  cos(thetaZ), 0,
                    0,            0, 1) %>% matrix(ncol = 3, nrow = 3)
  Rz %*% Ry %*% Rx
}

# # Generate the data to be rotated
# data3d <- data.frame(x = rep(seq(-5, 5, length.out = 20), 20),
#                      y = rep(seq(-5, 5, length.out = 20), each = 20))
# data3d %<>% mutate(z = 0.25 * y * cos(x * pi / 2) + log(y + 5.000001))
# scatterplot3d(data3d)
# 
# # Rotate that data
# rot3d_df <- as.matrix(data3d) %*% R_3(yaw = -10, pitch = 0, roll = 15) %>% 
#   as.data.frame()
# scatterplot3d(rot3d_df)

######  Full Rotation and Scale Matrix  #######################################
rot_and_scale3d <- function(rot_angles = c(0,0,0),
                          scale_factors = c(1,1,1)){
  # rot_angles - a list or vector containg the rotation angles in the order 
  #     following: yaw, pitch, roll. See the function commentary of "R_3" for a
  #     brief explination of how these angles behave in scatterplot3d
  #     functionality.
  # scale_factors - a list or vector containing the values by which to multiply
  #     each dimension.
  rot_angles <- unlist(rot_angles)
  yaw <- rot_angles[1]
  pitch <- rot_angles[2]
  roll <- rot_angles[3]
  
  scale_factors <- unlist(scale_factors)
  V <- diag(scale_factors, nrow = 3, ncol = 3)
  
  R_3(yaw, pitch, roll) %*% V
}

# # Test out the function
# angles <- list(yaw = 0, pitch = 0, roll = 0)
# scales <- c(0.2, 1, 5)
# rot_scale_data3d <- as.matrix(data3d) %*%
#   rot_and_scale3d(rot_angles = angles,
#                   scale_factors = scales) %>% 
#   as.data.frame()
# scatterplot3d(rot_scale_data3d)