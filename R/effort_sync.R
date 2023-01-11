#' Association scan effort to a detection
#'
#' @param df A `data.frame` with at least these columns: `scan_key` (unique ID for the scan),
#' `bearing` (in degrees), and `km_sit` (distance to detection in km).
#' @param sea A `data.frame` produced by `survey_overviews()` or `survey_overview()`.
#' @param verbose Boolean: print progress updates to Console?
#'
#' @return The `df` input with new conditions columns.
#' @import dplyr
#' @export
#'
effort_sync <- function(df,
                        scans,
                        verbose=TRUE){
  df %>% head
  scans %>% head

  # Make a default NA set of conditions for use when SEA is missing
  scan_default <- scans[1,] %>% select(scan_length = duration)
  scan_default[1,] <- NA
  scan_default

  # Loop through each DF row
  if(verbose){pb <- txtProgressBar(1, nrow(df), style=3)} # setup progress bar
  df_new <- data.frame()
  i=1
  for(i in 1:nrow(df)){
    if(verbose){setTxtProgressBar(pb, i)} # update progress bar

    (dfi <- df[i,])
    (scani <- scans %>% filter(scan_key == dfi$scan_key))

    if(nrow(sea_scan) == 0){
      sei <- scan_default
    }else{
      (sei <- scani[1,] %>% select(scan_length = duration))
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
