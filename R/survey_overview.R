#' Summarize survey data for a single date
#'
#' @param survey_date Survey date, in format `"yyyy-mm-dd"`. If `NULL` the current date
#' will be used (according to your system's time).
#'
#' @param time_zone The time zone to which you wish to force all date-times in the data
#' (the data numbers will not be changed at all, just R's interpretation of the time zone using
#' the `lubridate` package).
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
#' @import dplyr
#' @export
#'
survey_overview <- function(survey_date = NULL,
                            time_zone = "Canada/Pacific"){

  if(FALSE){ # for debugging -- not run!
    survey_date <- '2022-08-01'
    time_zone <- 'Canada/Pacific'
    result <- survey_overview(survey_date)
    survey_date <- NULL

    result <- survey_overview()
    result
    result$scans
    result$sighting_summary
    result$sightings
    result$conditions
    result$comments
    result$data
  } # end debugging

  if(is.null(survey_date)){
    survey_date <- Sys.Date() %>% as.character
  }

  (fn <- paste0('data/',survey_date,'.csv'))
  if(! file.exists(fn)){return(NULL)}
  df <- read.csv(fn, stringsAsFactors = FALSE, header=FALSE)
  df

  if(nrow(df)<=1){
    return(NULL)
    #stop('No survey data available on this date!')
  }

  names(df)[1:2] <- c('date','event')
  head(df)

  # Expand effort status =======================================================

  effs <- c()
  eff_status <- 0
  i=1
  for(i in 1:nrow(df)){
    (dfi <- df[i,])
    status <- eff_status
    if(dfi$event == 'B'){ status <- 0 }
    if(dfi$event == 'EFF' & as.character(dfi$V3) == '0'){ status <- 0 }
    if(dfi$event == 'EFF' & as.character(dfi$V3) == '1'){ status <- 1 }
    if(dfi$event == 'E'){ status <- 0 }
    effs[i] <- status
    eff_status <- status
  }
  effs
  df$effort <- effs

  # Scans ======================================================================

  starts <- ends <- c()
  scan_id <- c()
  scan_no <- 0
  eff_status <- 0
  for(i in 1:nrow(df)){
    (dfi <- df[i,])
    if(eff_status == 0 & dfi$effort == 1){
      starts <- c(starts, i)
      eff_status <- 1
      scan_no <- scan_no + 1
    }
    if(eff_status == 1 & dfi$effort == 0){
      ends <- c(ends, i)
      eff_status <- 0
    }
    if(i == nrow(df) & eff_status == 1){
      ends <- c(ends, i)
    }
    if(eff_status == 1 & dfi$effort == 1){
      scan_id[i] <- scan_no
    }
    if(eff_status == 0 & dfi$effort == 0){
      scan_id[i] <- NA
    }

  }
  scan_id
  df$scan_id <- scan_id
  starts
  ends

  if(length(starts) != length(ends)){
    message('Number of scan starts does not match number of scan ends! Fix please!')
    message('--- for now just truncating start/end times so that their lengths match ...')
    (min_length <- min(c(length(starts), length(ends))))
    starts <- starts[1:min_length]
    ends <- ends[1:min_length]
  }

  (starts <- df$date[starts])
  (ends <- df$date[ends])

  scans <- NULL
  if(length(starts)>0){
    (scans <- data.frame(scan_id = 1:length(starts), start = starts, end = ends))
    (scans$duration <- difftime(ends, starts, units='mins') %>% as.numeric)
  }
  scans

  if(!is.null(scans)){
    if(nrow(scans)>0){
      # Format timezones
      scans <-
        scans %>%
        mutate(start = lubridate::as_datetime(start) %>%
                 lubridate::force_tz(tzone=time_zone),
               end = lubridate::as_datetime(end) %>%
                 lubridate::force_tz(tzone=time_zone))

      # Add scan_key
      scans$scan_key <- paste0(date(scans$start), '-', scans$scan_id)
    }
  }

  # MARMAMs ====================================================================

  sits <- df %>% dplyr::filter(event == 'SIT')
  head(sits)
  sits <-
    sits %>%
    dplyr::rename(sit = V3,
                  bearing = V4,
                  reticle = V5,
                  how = V6,
                  km = V7,
                  landmark = V8,
                  cue = V9,
                  max = V10,
                  min = V11,
                  best = V12,
                  type = V13,
                  species = V14,
                  bhvr1 = V15,
                  bhvr2 = V16,
                  bhvr3 = V17,
                  dir = V18,
                  threat = V19,
                  calves = V20,
                  males = V21,
                  acoustic = V22,
                  photo = V23) %>%
    dplyr::mutate(date = lubridate::as_datetime(date) %>%
                    lubridate::force_tz(tzone=time_zone),
                  sit = as.numeric(sit),
                  bearing = as.numeric(as.character(bearing)),
                  reticle = as.numeric(as.character(reticle)),
                  how = as.character(how),
                  km = as.numeric(as.character(km)),
                  landmark = as.character(landmark),
                  cue = as.character(cue),
                  max = as.numeric(as.character(max)),
                  min = as.numeric(as.character(min)),
                  best = as.numeric(as.character(best)),
                  type = as.character(type),
                  species = as.character(species),
                  bhvr1 = as.character(bhvr1),
                  bhvr2 = as.character(bhvr2),
                  bhvr3 = as.character(bhvr3),
                  dir = as.character(dir),
                  threat = as.character(threat),
                  calves = as.character(calves),
                  males = as.character(males),
                  acoustic = as.character(acoustic),
                  photo = as.character(photo)) #%>%
    #dplyr::select(-V23)

  sits

  if(!is.null(sits)){
    if(nrow(sits)>0){
      # Add scan_key
      sits$scan_key <- paste0(date(sits$date), '-', sits$scan_id)
    }
  }

  suppressMessages({
    sit_summ <-
      sits %>%
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

  sit_summ


  # Condition zones ============================================================

  (sea <- df %>% dplyr::filter(event == 'SEA'))
  sea <-
    sea %>%
    dplyr::select(date,
                  scan_id,
                  effort,
                  left = V3,
                  right = V4,
                  near = V5,
                  far = V6,
                  bft = V7,
                  wave = V8,
                  vis = V9,
                  precip = V10,
                  fog = V11,
                  haze = V12,
                  smear = V13,
                  glare = V14,
                  glare_left = V15,
                  glare_right = V16) %>%
    dplyr::mutate(date = lubridate::as_datetime(date) %>%
                    lubridate::force_tz(tzone=time_zone),
                  scan_id,
                  effort,
                  left = as.numeric(left),
                  right = as.numeric(right),
                  near = as.numeric(near),
                  far = as.numeric(far),
                  bft = as.numeric(bft),
                  wave = as.character(wave),
                  vis = as.character(vis),
                  precip = as.character(precip),
                  fog = as.character(fog),
                  haze = as.character(haze),
                  smear = as.character(smear),
                  glare = as.character(glare),
                  glare_left = as.numeric(glare_left),
                  glare_right = as.numeric(glare_right))
  sea

  if(!is.null(sea) & !is.null(scans)){
    if(nrow(sea)>0 & nrow(scans)>0){
      # Fix scan IDs that may be missing
      i=5
      for(i in 1:nrow(sea)){
        (sei <- sea[i,])
        if(is.na(sei$scan_id)){
          tz(sei$date)
          tz(scans$start)
          (ts <- as.numeric(sei$date))
          (ts_scan <- as.numeric(scans$start))
          (diffs <- abs(ts - ts_scan))
          mini <- which.min(diffs)[1]
          sea$scan_id[i] <- scans$scan_id[mini]
        }
      }
      sea
      sea$scan_key <- paste0(date(sea$date), '-', sea$scan_id)
    }
  }

  # Comments ===================================================================

  comments <- df %>% dplyr::filter(event == 'COM')
  comments <-
    comments %>%
    dplyr::select(date, scan_id, effort, sit=V3, comment = V4) %>%
    dplyr::mutate(date = lubridate::as_datetime(date) %>%
                    lubridate::force_tz(tzone=time_zone),
                  comment = as.character(comment))

  # Compile result  ============================================================

  df$date <- df$date %>%
    lubridate::as_datetime() %>%
    lubridate::force_tz(tzone=time_zone)

  result <-
    list(scans = scans,
         sighting_summary = sit_summ,
         sightings = sits,
         conditions = sea,
         comments = comments,
         data = df)

  return(result)

}

