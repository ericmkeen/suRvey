#' Calculate distance to horizon
#'
#' @param h Height of observer, in meters
#' @param R Radius of the earth, in kilometers (default is `R` at latitude 53 degrees).
#'
#' @return A list with two elements: `$a` is the angle to the horizon,
#' in degrees from a horizontal plane,
#' and `$km` is the distance, in kilometers, from the observer to the horizon.
#'
#' @export
#'
km_to_horizon <- function(h,R=6364.5){
  # provide h in m
  # provide R in kilometers

  R <- R*1000
  a <- atan(sqrt((2*h*R)+(h^2))/R)
  a_radians <- a
  a <- a*(180/pi)
  a # returns in degrees

  a <- a*(pi/180) ; a
  H <- (R*a)/1000
  H # returns in km

  return(list(a = a,
              km = H))
}
