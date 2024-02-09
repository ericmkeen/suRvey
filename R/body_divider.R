#' Title
#'
#' @param steps  desc
#' @param from  desc
#' @param to  desc
#' @param n  desc
#' @param width  desc
#' @param height  desc
#' @param length_frac  desc
#' @param color  desc
#' @param line_width  desc
#' @param lty  desc
#'
#' @return desc
#' @export
#' @import dplyr
#'
body_divider <- function(steps,
                         from,
                         to,
                         n,
                         width = 3860,
                         height = 2700,
                         length_frac = .2,
                         color = 'black',
                         line_width = 1,
                         lty=1){

  if(FALSE){
    library(dplyr)
    from = 'blowhole'
    to = 'dorsal fin'
    length_frac = .2
    color = 'black'
    line_width = 1
    lty=1
    width = 3860
    height = 2700
    n <- 5
  }

  (from_xy <- steps %>% filter(measure == from))
  (to_xy <- steps %>% filter(measure == to))

  # Get slope
  (slope = (from_xy$y - to_xy$y)/(from_xy$x - to_xy$x))
  (alpha <- atan(slope))
  alpha_ortho <- alpha + (pi/2)

  # Get distance of new line
  dim_diagonal <- sqrt(width^2 + height^2)
  d_use <- length_frac*dim_diagonal

  # Get x locations of dividers
  (x_diff <- from_xy$x - to_xy$x)
  (x_step <- x_diff / (n+1))
  (xs <- x_step*(1:(n)))
  (xs <- from_xy$x - xs)

  # Get x locations of dividers
  (y_diff <- from_xy$y - to_xy$y)
  (y_step <- y_diff / (n+1))
  (ys <- y_step*(1:(n)))
  (ys <- from_xy$y - ys)

  df <- data.frame()
  for(i in 1:length(xs)){

    # Draw from intersection point - one way
    (xx = xs[i] + ((d_use/2) * cos(alpha_ortho)))
    (yy = ys[i] + ((d_use/2) * sin(alpha_ortho)))
    segments(x0=xs[i],
             y0=ys[i],
             x1=xx,
             y1=yy,
             col=color,
             lwd=line_width)

    dfi <- data.frame(from = from,
                      to = to,
                      divider = i,
                      side = 'A',
                      x_start = xs[i],
                      y_start = ys[i],
                      x_end = xx,
                      y_end = yy,
                      pixels = d_use,
                      radians = alpha_ortho)
    df <- rbind(df, dfi)

    # Draw from intersection point -- other way
    (xx = xs[i] - ((d_use/2) * cos(alpha_ortho)))
    (yy = ys[i] - ((d_use/2) * sin(alpha_ortho)))
    segments(x0=xs[i],
             y0=ys[i],
             x1=xx,
             y1=yy,
             col=color,
             lwd=line_width)

    dfi <- data.frame(from = from,
                      to = to,
                      divider = i,
                      side = 'B',
                      x_start = xs[i],
                      y_start = ys[i],
                      x_end = xx,
                      y_end = yy,
                      pixels = d_use,
                      radians = alpha_ortho)
    df <- rbind(df, dfi)

  }

  df

  return(df)
}
