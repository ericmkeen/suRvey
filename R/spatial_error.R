#' Determine spatial error of a sighting localization
#'
#' This function uses the platform height & degrees down to the sighting
#' from the horizontal plane in order to determine the spatial error of the survey instrument, e.g., a theodolite.
#'
#' @param p Platform height in meters.
#' @param d Degrees down from the horizontal plane to the waterline of the sighting.
#' @param t The precision of the survey instrument, e.g., a theodolite, in degrees. The default is the degrees-equivalent of plus-or-minus 2 arc-seconds, which is a fairly common error rating for digital theodolites.
#'
#' @return A `data.frame` with all input values as well as true distance to the sighting and spatial error details.
#'
#' @author Mary Margaret Lemburg and Eric Keen Ezell, May 2024
#' @example
#' spatial_error(p = 20, d = 0.015)
#' @export
#'
spatial_error <- function(p,
                          d,
                          t=0.000555556){

    # Calculate distance to horizon based on platform height
    (km_ref <- suRvey::km_to_horizon(h = p)$km)

    # Determine distance to sighting

    # TRUE distance
    km_true <- suRvey::sighting_distance(platform_height = p,
                                 km_ref = km_ref,
                                 reticles = d,
                                 degrees_per_reticle = 1)

    # Minimum distance (based on theodolite error)
    reticle_with_error <- d - t
    km_min <- suRvey::sighting_distance(platform_height = p,
                                km_ref = km_ref,
                                reticles = reticle_with_error,
                                degrees_per_reticle = 1)

    # Maximum distance (based on theodolite error)
    reticle_with_error <- d + t

    # Make sure error is not accidentally above horizon
    reticle_with_error <- ifelse(reticle_with_error < 0,
                                 0.000000001,
                                 reticle_with_error)

    km_max <- suRvey::sighting_distance(platform_height = p,
                                km_ref = km_ref,
                                reticles = d + t,
                                degrees_per_reticle = 1)

    # Estimate error
    error_km <- abs(km_max - km_min) # in km
    error_m <- error_km * 1000

    df <- data.frame(platfrom_height_m=c(p),
                     degrees_below_horizon=c(d),
                     precision = t,
                     km_true=c(km_true),
                     km_min=c(km_min),
                     km_max=c(km_max),
                     error_m=c(error_m))
    return(df)
  }

