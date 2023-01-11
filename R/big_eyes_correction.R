#' Correct Big Eyes bearing readings
#'
#' @param bearing Big Eyes bearing reading, in degrees.
#'
#' @return Corrected bearing.
#' @export
#'
big_eyes_correction <- function(bearing){

  if(bearing > 180){ # sighting is to right of pimple, toward Pitt, counting from 360 down
    corr <- 360 - bearing
    bearing <- 180 + corr
  }
  if(bearing < 180){ # Means sighting is to left of pimple, toward Gil, counting from 0 up
    bearing <- 180 - bearing
  }

  return(bearing)
}
