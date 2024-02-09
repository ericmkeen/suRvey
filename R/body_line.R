#' Title
#'
#' @param steps desc
#' @param measure_sequence desc
#' @param color desc
#' @param line_width desc
#'
#' @return desc
#' @export
#'
body_line <- function(steps,
                      measure_sequence = c('rostrum', 'blowhole', 'dorsal fin', 'peduncle', 'insertion point'),
                      color = 'black',
                      line_width = 1){
  for(i in 1:(length(measure_sequence)-1)){
    segments(x0= steps$x[steps$measure == measure_sequence[i]],
             y0= steps$y[steps$measure == measure_sequence[i]],
             x1= steps$x[steps$measure == measure_sequence[i+1]],
             y1= steps$y[steps$measure == measure_sequence[i + 1]],
             col=color, lwd = line_width)
  }
}
