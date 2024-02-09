sectionize <- function(steps,
                       width = 3860,
                       height = 2730,
                       color = 'white',
                       size = 1,
                       line_width = 1,
                       base_plot = FALSE){

  if(FALSE){
    url_steps <- 'https://docs.google.com/spreadsheets/d/12a4QNUR4mjv-Nzn3oImYlch6lMqT1Jj5a5UNSh7poDw/edit#gid=0'
    steps <- gsheet::gsheet2tbl(url_steps)
    steps

    width <- 3860
    height <- 2730
    base_plot = TRUE
    color = 'black'
    size = 1
    line_width = 1
  }

  # Base plot
  if(base_plot){
    suppressWarnings({
      plot(1~1, col='white', cex = 1, xlim=c(0, width), ylim=c(height, 0), xlab=NA, ylab=NA)
    })
  }

  # Plot all the features as points
  points(steps$y ~ steps$x, pch=16, col=color, cex=size)

  # Draw body line
  body_line(steps,
            measure_sequence = c('rostrum', 'blowhole', 'dorsal fin', 'peduncle', 'insertion point'),
            color = color, line_width = line_width)

  # Draw eye line
  body_extension(steps,
                 reference = c('left eye', 'right eye'),
                 extension_factor = 4, one_sided = 0, color = color, line_width = line_width)

  # Extend insertion point
  body_extension(steps,
                 reference = c('peduncle', 'insertion point'),
                 one_sided = 2,
                 extension_factor = 2, color = color, line_width = line_width)

  # Section dividers =======================================================

  mr <- data.frame()
  mri <- body_orthogonal(steps,
                         intersection = 'dorsal fin',
                         reference1 = c('blowhole', 'dorsal fin'),
                         reference2 = c('dorsal fin', 'peduncle'),
                         width = width,
                         height = height,
                         length_frac = .2,
                         color = color,
                         line_width = line_width,
                         lty=1)
  mr <- rbind(mr, mri)

  # mri <- body_orthogonal(steps,
  #                 intersection = 'blowhole',
  #                 reference1 = c('rostrum', 'blowhole'),
  #                 reference2 = c('blowhole', 'dorsal fin'),
  #                 width = width,
  #                 height = height,
  #                 length_frac = .2,
  #                 color = color,
  #                 line_width = line_width,
  #                 lty=1)
  # mr <- rbind(mr, mri)

  # mri <- body_orthogonal(steps,
  #                 intersection = 'peduncle',
  #                 reference1 = c('dorsal fin', 'peduncle'),
  #                 reference2 = c('peduncle', 'insertion point'),
  #                 width = width,
  #                 height = height,
  #                 length_frac = .2,
  #                 color = color,
  #                 line_width = line_width,
  #                 lty=1)
  # mr <- rbind(mr, mri)

  # mri <- body_orthogonal(steps,
  #                 intersection = 'insertion point',
  #                 reference1 = c('peduncle', 'insertion point'),
  #                 width = width,
  #                 height = height,
  #                 length_frac = .2,
  #                 color = color,
  #                 line_width = line_width,
  #                 lty=1)
  # mr <- rbind(mr, mri)

  if(FALSE){
    # Dividers within sections ===============================================

    mri <- body_divider(steps, from = 'rostrum', to='blowhole', n=5,
                        width = width,
                        height = height,
                        length_frac = .2,
                        color = color,
                        line_width = line_width,
                        lty=1)
    mr <- rbind(mr, mri)

    mri <- body_divider(steps, from = 'blowhole', to='dorsal fin', n=10,
                        width = width,
                        height = height,
                        length_frac = .2,
                        color = color,
                        line_width = line_width,
                        lty=1)
    mr <- rbind(mr, mri)

    mri <- body_divider(steps, from = 'dorsal fin', to='peduncle', n=5,
                        width = width,
                        height = height,
                        length_frac = .2,
                        color = color,
                        line_width = line_width,
                        lty=1)
    mr <- rbind(mr, mri)

    mri <-  body_divider(steps, from = 'peduncle', to='insertion point', n=5,
                         width = width,
                         height = height,
                         length_frac = .2,
                         color = color,
                         line_width = line_width,
                         lty=1)
    mr <- rbind(mr, mri)
  }
  return(mr)
}
