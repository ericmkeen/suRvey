#' Association conditions to a detection
#'
#' @param df A `data.frame` with at least these columns: `scan_key` (unique ID for the scan),
#' `bearing` (in degrees), and `km_sit` (distance to detection in km).
#' @param sea A `data.frame` produced by `survey_overviews()` or `survey_overview()`.
#' @param verbose Boolean: print progress updates to Console?
#'
#' @return Expanded dataframe.
#' @import dplyr
#' @export
#'
conditions_sync <- function(df,
                              sea,
                              verbose=TRUE){
  df %>% head
  sea %>% head

  # Make a default NA set of conditions for use when SEA is missing
  sea_default <- sea[1,] %>% select(bft, wave, vis, precip, fog, haze, smear, glare)
  sea_default[1,] <- NA
  sea_default

  # Loop through each DF row
  if(verbose){pb <- txtProgressBar(1, nrow(df), style=3)} # setup progress bar
  df_new <- data.frame()
  i=1
  for(i in 1:nrow(df)){
    if(verbose){setTxtProgressBar(pb, i)} # update progress bar

    (dfi <- df[i,])
    (sea_scan <- sea %>% filter(scan_key == dfi$scan_key))

    if(nrow(sea_scan) == 0){
      sei <- sea_default

    }else{
      if(nrow(sea_scan) > 1){ # multiple associated scans

        # Find the broadest condition zone, to use as a default
        (sea_scan$range <- sea_scan$right - sea_scan$left)
        (maxi <- which.max(sea_scan$range))
        (sei <- sea_scan[maxi,])

        # Loop through each scan, test if zone applies to sighting
        test_results <- c()
        for(si in 1:nrow(sea_scan)){
          sea_scani <- sea_scan[si,]
          (left_test <- dfi$bearing >= sea_scani$left)
          (right_test <- dfi$bearing <= sea_scani$right)
          (near_test <- dfi$km_sit >= sea_scani$near)
          (far_test <- dfi$km_sit <= sea_scani$far)
          testi <- all(left_test, right_test, near_test, far_test)
          test_results <- c(test_results, testi)
        }
        test_results

        # Filter by test results
        sea_scan <- sea_scan[test_results,]

        # Of all zones that apply, find one with smallest left-right range
        if(nrow(sea_scan) > 0){
          (mini <- which.min(sea_scan$range))
          (sei <- sea_scan[mini,])
        }

        # Keep only the fitting zone
        sea_scan <- sei
      }

      (sei <- sea_scan %>% select(bft, wave, vis, precip, fog, haze, smear, glare))

      # Handle glare
      if(dfi$bearing >= sea_scan$glare_left &
         dfi$bearing <= sea_scan$glare_right){
        # don't change glare
      }else{
        sei$glare <- 'None'
      }
    }

    # Add to sei to dfi
    (dfi <- data.frame(dfi, sei))

    # Build up new df
    df_new <- rbind(df_new, dfi)
  }

  # Check it out
  df_new %>% head

  return(df_new)
}
