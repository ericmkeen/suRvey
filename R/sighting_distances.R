#' Calculate distances to sightings
#'
#' This is a wrapper of `sighting_distance()`
#'
#' @param sits A `data.frame` of sightings, produced by `survey_overview()` or
#' `survey_overviews()`, with an additional column: `km_ref` (km to reference boundary).
#' @param optics_degrees A named list specifying the degrees-per-reticle conversion
#' for each optics instrument used in the survey. Spaces and capitalizations will be ignored.
#' @param default_method The default optics method to use, in the event that it is missing for
#' a sighting.
#' @param R Radius of the earth, in kilometers (default is `R` at latitude 53 degrees).
#' @param verbose Boolean: Print updates to Console?
#' @import dplyr
#'
#' @return A vector of distances, in km.
#' @export
#'
sighting_distances <- function(sits,
                               optics_degrees = list('bigeyes' = 0.057296,
                                                          'binocs' = 0.279,
                                                          'eyes' = 0.279,
                                                          'spotbox' = .00060585),
                               default_method = 'bigeyes',
                               R = 6364.5,
                               verbose = TRUE){

  if(verbose & nrow(sits)>3){pb <- txtProgressBar(1, nrow(sits), style=3)} # setup progress bar
  kms <- c()
  i=100
  for(i in 1:nrow(sits)){
    if(verbose & nrow(sits)>3){setTxtProgressBar(pb, i)} # update progress bar
    siti <- sits[i,]
    (howi <- gsub(' ','',siti$how) %>% tolower)
    (reti <- siti$reticle)
    how_match <- which(names(optics_degrees) == howi)
    if(length(how_match)==0){
      how_match <- which(names(optics_degrees) == default_method)
    }
    (deg_per_reti <- optics_degrees[[how_match]][1])

    kmi <-
      sighting_distance(platform_height = siti$ht,
                        km_ref = siti$km_ref,
                        reticles = reti,
                        degrees_per_reticle = deg_per_reti,
                        R = R)
    kmi
    kms <- c(kms, kmi)
  }
  kms

  return(kms)
}
