#' Get sighting locations
#'
#' @param bearings Numeric vector of bearings, in degrees.
#' @param kms Numeric vector of sightings distances, in km, the same length as `bearings`.
#' @param platform_x Longitude of observation platform, in decimal degrees (the default pertains to Fin Island Research Station).
#' @param platform_y Latitude to observation platform. in decimal degrees.
#'
#' @import dplyr
#' @return A dataframe of coordinates.
#' @export
#'
sighting_locations <- function(bearings,
                               kms,
                               platform_x = -129.3720833,
                               platform_y = 53.23690){
  xy <-
    swfscMisc::destination(lat = platform_y,
                         lon = platform_x,
                         brng = bearings,
                         distance = kms,
                         units='km') %>%
    as.data.frame

  xy %>% head
  return(xy)
}
