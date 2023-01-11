#' Find tidal height for survey timestamps
#'
#' @param df A `dataframe` with a `date` column (`lubridate::as_datetime()`) in the same
#' time zone as the tidal data below.
#'
#' @param tides A `data.frame` of tidal date, which must have
#' a column with date-time information,
#' format `YYYY-MM-DD hh:mm:ss`, in the same time zone as `df`,
#' and a column with tide height information,
#' in meters above mean low low tide.
#' For the Gitga'at First Nation area, we retrieve tidal date from the data portal
#' on the [Fisheries & Oceans Website](https://www.pac.dfo-mpo.gc.ca/science/charts-cartes/obs-app/observed-eng.aspx?StationID=09130),
#' making sure to set timestamp to local (UTC+8) before saving the `csv`'s.
#' We have supplied this dataset built-in to the package: `data(tides)`.
#'
#' @param date_col The index of the column with datetime data in the tide `csv`'s.
#'
#' @param tide_col The index of the column with tide height data in the tide `csv`'s.
#'
#' @param t_offset A time offset, in seconds, to apply to the tide data before synching with `df`.
#'
#' @param platform_height A height offset, in meters, to apply to the tide data before synching with `df`.
#' This is a shortcut for applying a platform's or observer's height above mean low low tide.
#'
#' @param verbose Boolean: Print updates to Console?
#'
#' @return The same `df` that was supplied, now with three new columns:
#' `tide_ht` (the tide height for each row),
#' `tide_diff` (the time difference, in seconds, between the `df` timestamp and
#' the row of tidal data referenced for tide height), and
#' `ht` (the height of the platform above the tidally-adjusted sea level)
#' @import dplyr
#'
#'
#' @export
#'
tide_sync <- function(df,
                      tide,
                      date_col = 2,
                      tide_col = 3,
                      t_offset = 0,
                      platform_height = 0,
                      verbose = TRUE){


  # Read & format tides ==========================================================

  # if(verbose){message('Reading in tidal data...')}
  # suppressMessages({
  #   (tides <- dir('./tides'))
  #   tide <- data.frame()
  #   i=1
  #   for(i in 1:length(tides)){
  #     (tidi <- paste0('tides/',tides[i]))
  #     tidi <- readr::read_csv(tidi)
  #     tidi %>% head
  #     tide <- rbind(tide, tidi)
  #   }
  # })
  # tide %>% head
  names(tide)[date_col] <- 'date'
  names(tide)[tide_col] <- 'tide'

  tide <- tide %>%
    select(date, tide) %>%
    mutate(daten = as.numeric(date) + t_offset)

  tide %>% head

  # Find closest tide reading ====================================================

  if(verbose){message('Matching tide data to supplied dataframe...')}
  df$tide_ht <- NA
  df$tide_diff <- NA
  if(verbose){pb <- txtProgressBar(1, nrow(df), style=3)} # setup progress bar
  i=2
  for(i in 1:nrow(df)){
    (dfi <- df[i,])
    diffs <- abs(dfi$daten - tide$daten)
    #plot(diffs, type='l')
    (mini <- which.min(diffs))
    df$tide_ht[i] <- tide[mini[1],]$tide
    df$tide_diff[i] <- diffs[mini[1]]
    if(verbose){setTxtProgressBar(pb, i)} # update progress bar
  }

  df %>% head
  df$ht <- platform_height - df$tide_ht

  return(df)
}
