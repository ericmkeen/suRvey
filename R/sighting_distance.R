#' Calculate sighting distance
#'
#' @param platform_height  Height, in meters, of observation deck (or eye height, if possible) above sea level.
#' @param km_ref The distance, in kilometers, to the boundary used as a reference for reticles (horizon or a distant shoreline).
#' @param reticles Reticle reading.
#' @param degrees_per_reticle The degrees represented by each full reticle.
#' @param R Radius of the earth, in kilometers (default is `R` at latitude 53 degrees).
#' @import dplyr
#'
#' @return The distance to the sighting, in km.
#' @export
#'
sighting_distance <- function(platform_height,
                              km_ref,
                              reticles,
                              degrees_per_reticle = 0.279,
                              R = 6364.5){

  if(FALSE){
    km_ref <- 5.87083
    deg.per.ret <- 0.057296
    reticles <- 0.5
    R <- 6364.5
    h <- 6.392

    km_ref <- 9.868
    deg.per.ret <- 1
    reticles <- 7.58333
    R <- 6364.5
    h <- 120
  }

  # convert reticles to radians
  deg <- reticles*degrees_per_reticle
  theta <- deg * (pi / 180)

  # convert height to km
  h <- platform_height/1000
  h

  # Determine distance
  a <- km_ref/R
  Lo <- sqrt((R^2) + ((R + h)^2) - 2 * R * (R + h) * cos(a))
  B <- acos(((2 * h * R) + (h^2) + (Lo^2))/(2 * (R + h) * Lo)) - theta

  if(is.na(theta) | theta==0){
    # Assume nothing is actually on the shore, but 50m off shore
    sitdist <- km_ref - .05
  }else{
    Do <- (R + h) * cos(B) - sqrt(((R + h)^2) * (cos(B)^2) - (2 * h * R + (h^2))) ; Do
    delt <- asin(sin(B) * (Do/R)) ; delt
    sitdist <- delt * R ; sitdist
  }
  sitdist

  return(sitdist)
}
