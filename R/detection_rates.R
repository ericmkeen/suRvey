#' Calculate weekly detection rates
#'
#' @param scans A `data.frame` produced by `survey_overviews()` or `survey_overview()` and
#' processed with `effort_conditions()`.
#' @param sits A `data.frame` produced by `survey_overviews()` or `survey_overview()`.
#' @param min_vis Minimum acceptable percent-visibility for a scan in order to
#' include in a detection rate calculation.
#' @param max_bft Maximum acceptable Beaufort sea state for a scan in order to
#' include in a detection rate calculation.
#'
#' @return a `data.frame` with detection summaries and rates )detections per hour) for each species.
#'
#' @import dplyr
#' @import lubridate
#' @export
#'
detection_rates <- function(scans,
                            sits,
                            min_vis = 50,
                            max_bft = 3){

  suppressMessages({

    siti <- sits
    scani <- scans
    scani %>% head
    siti %>% head
    (siti$week <- lubridate::week(siti$date))
    (scani$week <- lubridate::week(scani$start))

    # Filter scans
    scani <- scani %>% filter(vis_percent >= 50,
                              bft <= max_bft)

    # Filter sightings to viable scans
    (viable_scans <- scani$scan_key %>% unique)
    (siti <- siti %>% filter(scan_key %in% viable_scans))

    # Summarize weeks
    scanwk <- scani %>%
      group_by(wk=week) %>%
      summarize(dt = lubridate::date(start[1]),
                effort = sum(duration))
    scanwk

    sitwk <- siti %>%
      group_by(type, species, wk=week) %>%
      summarize(n = n(),
                ss_mean = mean(best,na.rm=TRUE),
                ss_sd = mean(best,na.rm=TRUE)) %>%
      mutate(n_ind = n*ss_mean) %>%
      ungroup()
    sitwk

    # Make sure sighting data re complete
    sitwk <-
      tidyr::complete(sitwk, wk, species,
               fill=list(n=0,
                         ss_mean = NA,
                         ss_sd = NA,
                         n_ind = 0))

    er <- dplyr::inner_join(scanwk, sitwk)
    er %>% head

    # Convert to encounter rate
    er$er <- er$n / (er$effort/60)
    er$er_ind <- er$n_ind / (er$effort/60)

    er
  })
  return(er)
}
