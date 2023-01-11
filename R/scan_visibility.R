#' Find average conditions of a scan
#'
#' @param scans A `data.frame` produced by `survey_overviews()` or `survey_overview()`.
#' @param sea A `data.frame` produced by `survey_overviews()` or `survey_overview()`.
#' @param verbose Boolean: print progress updates to Console?
#'
#' @return The `scans` input with new conditions columns.
#' @import dplyr
#' @export
#'
scan_conditions <- function(scans,
                            sea,
                            max_vis = 15,
                            verbose=TRUE){
  #max_vis <- 15
  scans %>% head
  sea %>% head

  sea$vis <- as.numeric(sea$vis)
  (sea$vis[is.na(sea$vis)] <-  max(sea$vis,na.rm=TRUE))
  sea$vis[sea$vis > max_vis] <- max_vis
  (sea$wave <- as.numeric(sea$wave))
  (sea$range_lr <- sea$right - sea$left)
  (sea$range_nf <- sea$far - sea$near)

  # Loop through each scan row
  if(verbose){pb <- txtProgressBar(1, nrow(scans), style=3)} # setup progress bar
  scans_new <- data.frame()
  i=71
  for(i in 1:nrow(scans)){
    if(verbose){setTxtProgressBar(pb, i)} # update progress bar

    (scani <- scans[i,])
    (sei <- sea %>% filter(scan_key == scani$scan_key))
    sei$weight <- sei$range_lr * sei$range_nf

    # Handle visibility
    sei
    sei$vis[sei$near > 0 & sei$vis > 0] <- sei$near
    sei
    (vis <- stats::weighted.mean(sei$vis, sei$weight))
    (vis_percent <- round(100*(vis / max_vis)))

    # Other conditions
    (bft <- stats::weighted.mean(sei$bft, sei$weight, na.rm=TRUE))
    (wave <- stats::weighted.mean(sei$wave, sei$wave, na.rm=TRUE))
    (precip <- paste(sort(unique(sei$precip)), collapse='-'))
    (fog <- paste(sort(unique(sei$fog)), collapse='-'))
    (smear <- paste(sort(unique(sei$smear)), collapse='-'))
    (glares <- sei$glare %>% unique)
    if(length(glares) > 1){glares <- glares[glares != 'None']}
    glares
    (glare <- paste(sort(glares), collapse='-'))

    (scani <- data.frame(scani,
                         bft, wave, vis, vis_percent, precip, fog, smear, glare))

    # Build up new df
    scans_new <- rbind(scans_new, scani)
  }

  message('')

  # Check it out
  scans_new %>% head
  scans_new$bft
  scans_new$wave
  scans_new$vis
  scans_new$bft %>% hist
  scans_new[71,]

  return(scans_new)
}
