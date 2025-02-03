#' Find closest shoreline along bearing
#'
#' @param shoreline An `sf` `MULTIPOLYGON` of shoreline boundaries to test.
#' @param bearing A numeric vector of the bearings of interest, in degrees (0-360).
#' @param platform_x Longitude of observation platform, in decimal degrees (the default pertains to Fin Island Research Station).
#' @param platform_y Latitude to observation platform. in decimal degrees.
#' @param min_km The minimum km to consider;
#' this allows you to ignore shoreline within a certain distance of your platform.
#' @param sightline_km The length of the sightline along your bearing to consider.
#' Large numbers are safer, but could slow things down if your shoreline is complex.
#' @param verbose Boolean: Print updates to Console?
#' @param toplot boolean: Create diagnostic plots?
#'
#' @import dplyr
#' @return Vector of coordinates of closest shoreline along bearing.
#' @export
#'
shoreline_finder <- function(shoreline,
                             bearing,
                             platform_x = -129.3720833,
                             platform_y = 53.23690,
                             min_km = 0.5,
                             sightline_km = 50,
                             verbose = TRUE,
                             toplot = FALSE){

  if(toplot){
    plot(shoreline)
    platform_x = -129.3720833
    platform_y = 53.23690
    min_km = 0.5
    sightline_km = 50
    verbose = TRUE
    toplot = FALSE
  }

  if(verbose & length(bearing)>3){pb <- txtProgressBar(1, length(bearing), style=3)} # setup progress bar
  closests <- c()
  for(i in 1:length(bearing)){

    if(verbose & length(bearing)>3){setTxtProgressBar(pb, i)} # update progress bar

    horline_end <- swfscMisc::destination(lat = platform_y,
                                          lon = platform_x,
                                          brng = bearing[i],
                                          distance = sightline_km)
    horline_end

    # Determine boundary type & distance
    xy <- data.frame(x=c(platform_x, horline_end[2]),
                     y=c(platform_y, horline_end[1]))
    xy %>% head
    horline <-
      xy %>%
      st_as_sf(coords = c("x", "y"), na.fail = FALSE, dim = 'XY', crs = "epsg:4326") %>%
      st_combine %>%
      st_cast('LINESTRING')
    horline

    # Get intersections
    suppressMessages({
      st_crs(shoreline) <- st_crs(horline)
      st_crs(shoreline)
      intersections <-
      sf::st_intersection(horline,
                          st_cast(shoreline, "MULTILINESTRING", group_or_split = FALSE))
    })


    # Plot it
    if(toplot){
      plot(horline, add = TRUE)
      plot(intersections, add = TRUE, col = "red", pch = 21)
    }

    # Get coordinates of intersections
    (intersections <- sf::st_coordinates(intersections) %>% as.data.frame)

    # Get great-circle-distance to each of these points from fin island
    intersections$km <-
      apply(data.frame(intersections$Y, intersections$X), 1,
            function(yx){
              swfscMisc::distance(lat1 = platform_y,
                                  lon1 = platform_x,
                                  lat2 = yx[1],
                                  lon2 = yx[2],
                                  units = 'nm') * 1.852
            })
    # intersections$km <-
    #   swfscMisc::distance(lat1 = platform_y,
    #                       lon1 = platform_x,
    #                       lat2 = intersections$Y,
    #                       lon2 = intersections$X,
    #                       units = 'nm') * 1.852

    # Filter to intersections that are beyond min acceptable distance
    intersections <- intersections %>% dplyr::filter(km > min_km)
    if(verbose & length(bearing) <= 3){message(nrow(intersections),' intersections with shoreline found along sightline...')}

    # Get closest of these intersections
    (closest_intersection <- min(intersections$km))

    # Add to group
    closests <- c(closests, closest_intersection)
  }
  message('')

  return(closests)
}
