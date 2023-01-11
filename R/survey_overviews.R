#' Summarize survey data for a multiple dates
#'
#' A wrapper for `survey_overview()`.
#'
#' @param survey_dates A character vector of survey date, in format `"yyyy-mm-dd"`.
#' If not provided or `NULL`, the function will process all dates within the `data` subfolder of
#' your working directory.
#'
#' @param verbose Boolean: Print updates to Console?
#'
#' @return This function will look for survey data within the `data` subfolder
#' of your working directory and return a list with various summary tables:
#'
#' \enumerate{
#' \item `scans`: A summary table of scans and their duration.
#' \item `sighting_summary`: Summary metrics for each species observed.
#' \item `sightings`: All data for all sightings, with intuitive column names added.
#' \item `conditions`: All data for all condition zones, with intuitive column names added.
#' \item `comments`: All comments.
#' \item `data`: All data, with two fields added: `effort` (indicating whether each row occurs
#' during systematic effort, with value `1`, or off-scan, value `0`) and `scan_id`
#' (with a numeric indicator of which scan of the day the row of day correspnds to;
#' if the row of data occurs outside of a scan, the value with be `NA`).
#' }
#' @export
#' @import dplyr
#'
survey_overviews <- function(survey_dates = NULL,
                             verbose = TRUE){

  if(FALSE){ # for debugging -- not run!
    survey_dates <- NULL
    verbose <- TRUE
    result <- survey_overviews()
    result
    result$scans
    result$sighting_summary
    result$sightings
    result$conditions
    result$comments
    result$data
  } # end debugging

  if(is.null(survey_dates)){
    (survey_dates <- gsub('.csv','',dir('./data')))
  }

  # Loop through each date
  if(verbose){message('Looping through each date ...')}
  i=1
  surveys <- list()
  for(i in 1:length(survey_dates)){
    survey_date <- survey_dates[i]
    if(verbose){message('--- ',survey_date)}
    survi <- survey_overview(survey_date)
    survi %>% names

    if(!is.null(survi)){
      surveys[[length(surveys)+1]] <- survi
    }
  }

  # Combine into sepearate lists
  surveys %>% length
  (scans <- lapply(surveys,'[[', 1) %>% bind_rows) %>% head
  (sightings <- lapply(surveys,'[[', 3) %>% bind_rows) %>% head
  (conditions <- lapply(surveys,'[[', 4) %>% bind_rows) %>% head
  (comments <- lapply(surveys,'[[', 5) %>% bind_rows) %>% head

  # Compile sighting summary
  suppressMessages({
    sit_summ <-
      sightings %>%
      dplyr::group_by(type, species) %>%
      summarize(n = n(),
                n_scan = length(which(effort == 1)),
                n_off = length(which(effort == 0)),
                max_ind = sum(as.numeric(max[effort==1])),
                min_ind = sum(as.numeric(min[effort==1])),
                best_ind = sum(as.numeric(best[effort==1])),
                grp = mean(as.numeric(best)),
                grp_scan = mean(as.numeric(best[effort==1])),
                grp_off = mean(as.numeric(best[effort==0])))
  })

  # Concatenate raw data
  df <- lapply(surveys,'[[', 6) #%>% bind_rows) %>% head
  i=1
  for(i in 1:length(df)){
    dfi <- df[[i]]
    dfi[] <- lapply(dfi, as.character)
    df[[i]] <- dfi
  }
  df <- df %>% bind_rows
  df %>% head

  # Compile result
  result <-
    list(scans = scans,
         sighting_summary = sit_summ,
         sightings = sightings,
         conditions = conditions,
         comments = comments,
         data = df)

  return(result)

}

