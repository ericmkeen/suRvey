#' Title
#'
#' @param steps  desc
#' @param intersection  desc
#' @param reference1  desc
#' @param reference2  desc
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
body_orthogonal <- function(steps,
                            intersection,
                            reference1 = NULL,
                            reference2 = NULL,
                            width = 3860,
                            height = 2700,
                            length_frac = .2,
                            color = 'black',
                            line_width = 1,
                            lty=1){

  if(FALSE){
    library(dplyr)
    intersection = 'dorsal fin'
    reference1 <- c('blowhole', 'dorsal fin')
    reference2 <- c('dorsal fin', 'peduncle')
    length_frac = .2
    color = 'black'
    line_width = 1
    lty=1
    width = 3860
    height = 2700
  }

  # Get measure coordinates
  (ixy<- steps %>% filter(measure == intersection))

  alpha1 <- NA
  if(!is.null(reference1)){
    (a1 <- steps %>% filter(measure == reference1[1]))
    (b1 <- steps %>% filter(measure == reference1[2]))

    # Get slope of reference 1
    (slope = (b1$y - a1$y)/(b1$x - a1$x))
    (alpha1 <- atan(slope))
  }

  alpha2 <- NA
  if(!is.null(reference2)){
    (a2 <- steps %>% filter(measure == reference2[1]))
    (b2 <- steps %>% filter(measure == reference2[2]))

    # Get slope of reference 2
    (slope = (b2$y - a2$y)/(b2$x - a2$x))
    (alpha2 <- atan(slope))
  }

  # Get average slope
  (alpha_mean <- mean(c(alpha1, alpha2), na.rm=TRUE))

  # Find orthogonal slope
  (alpha_ortho <- alpha_mean + pi/2)

  # Get distance of new line
  dim_diagonal <- sqrt(width^2 + height^2)
  d_use <- length_frac*dim_diagonal

  # Draw from intersection point - one way
  (xx = ixy$x + ((d_use/2) * cos(alpha_ortho)))
  (yy = ixy$y + ((d_use/2) * sin(alpha_ortho)))
  segments(x0=ixy$x,
           y0=ixy$y,
           x1=xx,
           y1=yy,
           col=color,
           lwd=line_width)

  df <- data.frame()
  dfi <- data.frame(from = intersection,
                    to = intersection,
                    divider = 1,
                    side = 'A',
                    x_start = ixy$x,
                    y_start = ixy$y,
                    x_end = xx,
                    y_end = yy,
                    pixels = d_use,
                    radians = alpha_ortho)
  df <- rbind(df, dfi)

  # Draw from intersection point -- other way
  (xx = ixy$x - ((d_use/2) * cos(alpha_ortho)))
  (yy = ixy$y - ((d_use/2) * sin(alpha_ortho)))
  segments(x0=ixy$x,
           y0=ixy$y,
           x1=xx,
           y1=yy,
           col=color,
           lwd=line_width)
  dfi <- data.frame(from = intersection,
                    to = intersection,
                    divider = 1,
                    side = 'B',
                    x_start = ixy$x,
                    y_start = ixy$y,
                    x_end = xx,
                    y_end = yy,
                    pixels = d_use,
                    radians = alpha_ortho)
  df <- rbind(df, dfi)
  return(df)
}
