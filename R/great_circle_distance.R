#' Great circle distance based on angle
#'
#' @param tilt Angle, in degrees, down from the horizontal plane.
#' @param H Height of the observer, in meters.
#' @param R Radius of the earth, in kilometers (default is `R` at latitude 53 degrees).
#'
#' @return A vector of distances.
#' @export
#'
great_circle_distance <- function(tilt,H,R=6364.5){
  R <- R*1000
  tilt <- as.numeric(as.character(tilt))
  tilt <- (pi/180)*(tilt)
  beta <-  (pi/2) - tilt
  D <-  ((R+H)*cos(beta)) - sqrt( (((R+H)^2)*(cos(beta)^2)) - ((2*H*R)+(H^2)) )
  delt <- asin((sin(beta)*(D/R)))
  D <- delt*R
  if(!is.finite(D)){D <- NA}
  return(D)
}
