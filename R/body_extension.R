#' Title
#'
#' @param steps  desc
#' @param reference  desc
#' @param extension_factor  desc
#' @param one_sided  desc
#' @param color  desc
#' @param line_width  desc
#' @param lty  desc
#'
#' @return desc
#' @export
#' @import dplyr
#'
body_extension <- function(steps,
                           reference,
                           extension_factor = 4,
                           one_sided = 0,
                           color = 'black',
                           line_width = 1,
                           lty=1){

  if(FALSE){
    library(dplyr)
    reference <- c('left eye', 'right eye')
    extension_factor = 2
    color = 'black'
    line_width = 1
    lty=1
  }

  r1 <- steps %>% filter(measure == reference[1])
  r2 <- steps %>% filter(measure == reference[2])

  # Get distance of line
  (d <- sqrt(((r2$x - r1$x)^2 + (r2$y - r1$y)^2)))
  (d_extended <- d*extension_factor)

  # Get midpt
  (x_mean <- (r2$x + r1$x)/2)
  (y_mean <- (r2$y + r1$y)/2)
  #points(x_mean, y_mean)

  # Get slope
  (slope = (r2$y - y_mean)/(r2$x - x_mean))
  (alpha <- atan(slope))

  # Check one-sided section
  if(one_sided == 1){
    x_mean <- r1$x
    y_mean <- r1$y
  }
  if(one_sided == 2){
    x_mean <- r2$x
    y_mean <- r2$y
  }

    # Line going one way
    (xx = x_mean + ((d_extended/2) * cos(alpha)))
    (yy = y_mean + ((d_extended/2) * sin(alpha)))
    segments(x0=x_mean,
             y0=y_mean,
             x1=xx,
             y1=yy,
             col=color,
             lwd=line_width)

    # Line going other way
    (xx = x_mean - ((d_extended/2) * cos(alpha)))
    (yy = y_mean - ((d_extended/2) * sin(alpha)))
    segments(x0=x_mean,
             y0=y_mean,
             x1=xx,
             y1=yy,
             col=color,
             lwd=line_width)
}
