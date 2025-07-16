#' Measure pixels in a set of images
#'
#' This function launches a Shiny app that allows you to quickly measure the pixel location
#' of the waterline of a whale/boat and the horizon/shoreline directly behind it in a set of images.
#'
#' @param img_path Path to the folder of images. The default assumption is that there is
#' a folder named "`images`" within your working directory.
#'
#' @param log_path Path to the `csv` file in which to keep pixel measurements.
#' The default assumption is a file, `measures.csv`, within your working directoy.
#' If this file does not exist in the specified path, the function will create it.
#'
#' @param start_index Index of file to start on.
#'
#' @param features Character vector of features to choose from. You can measure multiple features on the same image.
#'
#' @param zoom_options Numeric vector of zoom options to choose from; the default will be the first one in your vector.
#'
#' @param scroll_width Width, in pixels, of image scrolling portion of the app, to customize the app for your specific screen.
#'
#' @param scroll_height Height, in pixels, of image scrolling portion of the app, to customize the app for your specific screen.
#'
#' @param filter_to_unmeasured Filter images to those without any measurements yet?
#'
#' @details This function uses the `magick` package to read and display images.
#'
#' Workflow notes:
#' \itemize{
#' \item If your photos are large, you can adjust the zoom options and scroll around
#' the image using your mousepad.
#' \item To zoom, click-and-drag to create a rectangle, then single-click within it.
#' \item To unzoom, single-click anywhere on image.
#' \item Double-click on the whale/boat waterline to grab its pixel location.
#' \item Double-click on the horizon/shore line to grab its pixel location.
#' \item To save these measurements, click the "Save" button that appears.
#' \item Advance to the next image without saving measurements by clicking the "Skip" button.
#' \item Close the app by exiting the app window; you may also have to click the 'Stop sign' in
#' `R` or `RStudio` to end the command completely.
#' }
#'
#' @return This function will launch a `Shiny` app. As you store measurements,
#' the `log_path` file will be updated with those new measurements. Each row of
#' measurements follows this comma-separated format:
#' \enumerate{
#' \item Image filename,
#' \item Y pixel of horizon/shore
#' \item Y pixel of whale/boat waterline
#' \item X pixel of whale/boat (can be useful for refining bearing)
#' \item System time of when measurement was made
#' \item Feature (optionally entered manually by user)
#' \item Latitude (optionally entered manually by user)
#' \item Longitude (optionally entered manually by user)
#' \item Elevation (e.g., of drone; optionally entered manually by user)
#' \item Focal length (optionally entered manually by user)
#' \item Comment field (optional)
#' }
#'
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import magick
#' @export
#'
image_measure <- function(img_path='images/',
                          log_path='measures.csv',
                          start_index = 1,
                          features = c('Other',
                                       'Whale (certain)', 'Whale (maybe)',
                                       'LNG carrier (bow)', 'LNG carrier (stern)',
                                       'Tug (bow)', 'Tug (stern)',
                                       'Vessel (bow)', 'Vessel (stern)'),
                          zoom_options = c(30, 50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 300),
                          scroll_width = 1700,
                          scroll_height = 400,
                          filter_to_unmeasured = FALSE){

  if(FALSE){ #==================================================================

    #library(BiocManager)
    #if(! 'EBImage' %in% installed.packages()){
    #  BiocManager::install("EBImage")
    #}
    #library(shiny)
    #library(dplyr)
    #library(shinydashboard)
    img_path <- 'images/'
    log_path <- 'measures.csv'
    scroll_height = 400
    scroll_width = 1700
    start_index = 1
    features = c('Other',
                 'Whale (certain)', 'Whale (maybe)',
                 'LNG carrier (bow)', 'LNG carrier (stern)',
                 'Tug (bow)', 'Tug (stern)',
                 'Vessel (bow)', 'Vessel (stern)')

    zoom_options = c(35, 50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 300)

    # Try it
    image_measure()

  } #===========================================================================

  #if(! 'EBImage' %in% installed.packages()){
  #  BiocManager::install("EBImage")
  #}

  if(! file.exists(log_path)){
    previous_measures <- data.frame()
  }else{
    suppressWarnings({suppressMessages({
      previous_measures <- readr::read_csv(log_path, col_names=FALSE)
    })})
  }

  if(! dir.exists(img_path)){ stop('Path to images folder does not exist!') }
  (image_files <- dir(img_path))
  if(length(image_files) == 0){ stop('No images were found within the images folder!')}
  (image_files <- paste0(img_path, image_files))
  #image_read(image_files[1]) %>% image_info

  # Filter
  if(filter_to_unmeasured && nrow(previous_measures)>0){
    measured_files <- previous_measures$X1
    not_yet_measured <- ! image_files %in% measured_files
    image_files <- image_files[not_yet_measured]
    message(length(image_files),' unmeasured files!')
    if(length(image_files) == 0){ stop('After filtering, no images need to be measured!')}
  }

  ######################################################################
  ######################################################################
  ######################################################################
  ######################################################################

  ui <- fluidPage(
    br(),
    fluidRow(
      column(2, selectInput('feature', label=h4('Feature measured:'),
                            choices = features,
                            selected = features[1], width='100%')),
      column(2,uiOutput('save')),
      column(2,actionButton(inputId="skip",label=h4(HTML("Next<br/>image")),width="95%")),
      column(4,htmlOutput("imgname")),
      column(2,selectInput('zoom', label='Set widest zoom %',
                           choices=as.character(zoom_options),
                           selected=as.character(zoom_options[1]), width='100%'))),
    fluidRow(column(12, helpText(HTML('<p style="font-size:11px"><b>Double-click</b> on <span style="color:red"><b>whale/boat waterline</b></span>, then <b>double-click</b> on the <span style="color:green"><b>horizon/shore</b></span> directly above & beyond it.
                                  Drag & click to zoom; click again to unzoom. Double-click to erase the current measurement.</p>')))),
    hr(),
    fluidRow(column(1,h4('Image metadata:')),
             column(2, textInput('lon', label='Lon:', value='-129.', width='100%')),
             column(2, textInput('lat', label='Lat:', value='53.', width='100%')),
             column(1, textInput('elev', label='Elev:', value='120', width='100%')),
             column(2, textInput('focal', label='Foc. Len. (mm):', value='24', width='100%')),
             column(3, textInput('comm', label='Comment:', value='', width='100%'))),
    fluidRow(column(12,
                    shinydashboard::box(
                      style=paste0('width:',
                                   scroll_width,
                                   'px; overflow-x: scroll; height:',
                                   scroll_height,
                                   'px; overflow-y: scroll;'),
                      uiOutput('img_show'),
                      width=12))),
    fluidRow(column(12, verbatimTextOutput('pixels'))),
    hr(),
    fluidRow(column(12,
                    h5('Measures already on file for this image:'),
                    br(),
                    DT::dataTableOutput("table1"))),
    br()
  )

  ######################################################################
  ######################################################################
  ######################################################################
  ######################################################################

  server <- function(input, output) {

    # -------------------------------------------------------------------
    rv <- reactiveValues()
    rv$previous_measures <- previous_measures
    rv$reload <- 0
    rv$images <- image_files
    rv$img <- NULL
    rv$imi <- start_index
    rv$imi_measures <- data.frame()
    rv$brng <- NULL # y pixel to whale
    rv$whale <- NULL # x pixel to water line
    rv$shore <- NULL # x pixel to shore/horizon

    # -------------------------------------------------------------------
    # Update measures

    observeEvent(rv$reload,{
      if(file.exists(log_path)){
        suppressWarnings({
          suppressMessages({
            rv$previous_measures <- readr::read_csv(log_path, col_names=FALSE)
            print(rv$previous_measures)
            isolate({
              if(nrow(rv$previous_measures)>0){
                rv$imi_measures <- rv$previous_measures %>% filter(X1 == rv$images[rv$imi])
                print(rv$imi_measures)
              }
            })
          })
        })
      }else{
        rv$previous_measures <- data.frame()
      }
    })

    # -------------------------------------------------------------------
    # Display the image

    output$img_show <- renderUI({
      zoom <- input$zoom %>% as.numeric
      zoom <- as.numeric(zoom / 100)
      f <- rv$images[rv$imi] %>% as.character
      img1 <- magick::image_read(f)
      plotOutput("plot1",
                 width=(image_info(img1)$width * zoom),
                 height=(image_info(img1)$height * zoom),
                 dblclick = dblclickOpts(id = "plot1_dblclick"),
                 click = 'plot1_click',
                 hover = 'plot1_hover',
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE
                 ))
    })

    # -------------------------------------------------------------------
    ranges <- reactiveValues(x = NULL, y = NULL)

    output$plot1 <- renderPlot({
      f <- rv$images[rv$imi] %>% as.character
      img1 <- magick::image_read(f)

      p <- image_ggplot(img1)

      if(nrow(rv$imi_measures)>0){
        p <- p +
          geom_point(data=rv$imi_measures, mapping=aes(x=X6, y=X4), color='yellow', size=3, pch='+') +
          geom_point(data=rv$imi_measures, mapping=aes(x=X6, y=X5), color='yellow', size=3, pch='+') +
          geom_segment(data=rv$imi_measures, mapping = aes(x=X6, xend=X6, y=X4, yend=X5), color='yellow', lwd=.25)
      }

      if(!is.null(rv$brng)){
        message('rv$brng is ', rv$brng)
        p <- p + geom_point(mapping=aes(x=rv$brng, y=rv$whale), color='red', size=5, pch='+')

        if(is.null(rv$shore)){
          p <- p + geom_segment(mapping = aes(x=rv$brng, xend=rv$brng,
                                              y=rv$whale, yend=image_info(img1)$height), color='red', lwd=.5)
        }else{
          p <- p + geom_segment(mapping = aes(x=rv$brng, xend=rv$brng,
                                              y=rv$whale, yend=rv$shore), color='red', lwd=.5) +
            geom_point(mapping=aes(x=rv$brng, y=rv$shore), color='green', size=5, pch='+')
        }
      }

      if(is.null(ranges$x)){
        p
      }else{
        p +
          coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
      }
    })

    # When a click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_click, {
      brush <- input$plot1_brush
      print(brush)
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- ranges$y <- NULL
      }
    })

    # -------------------------------------------------------------------
    # Get measurements

    # Store data on DOUBLE CLICK
    observeEvent(input$plot1_dblclick, {
      if (!is.null(input$plot1_dblclick)) {
        dblclick <- input$plot1_dblclick
        message('New double click!')
        message('X = ', print(dblclick$x),
                '\nY = ', print(dblclick$y))
        isolate({
          if(is.null(rv$brng)){
            rv$brng <- dblclick$x
            rv$whale <- dblclick$y
          }else{
            if(is.null(rv$shore)){
              rv$shore <- dblclick$y
            }else{
              # reset all values
              rv$brng <- rv$whale <- rv$shore <- NULL
            }
          }
        })
      }
    })

    observeEvent(input$img_click, {
      isolate({
        if(is.null(rv$shore)){
          if(!is.null(input$img_click)){
            rv$shore <- input$img_click$y
          }
        }
      })
    })

    # -------------------------------------------------------------------
    # Show info

    output$imgname <- renderText({
      paste0('Image ',rv$imi,' out of ',length(rv$images),
             ' (filtered from ',length(image_files),
             ' image files) :: ','<br><br>',
             '<B><p style="font-size:12px">',rv$images[rv$imi],'</p></B>')

    })

    # -------------------------------------------------------------------
    # Navigation

    observeEvent(input$clear, {
      rv$brng <- NULL
      rv$whale <- NULL
      rv$shore <- NULL
    })

    output$save <- renderUI({
      null_tests <- c(!is.null(rv$brng),
                      !is.null(rv$whale),
                      !is.null(rv$brng))
      if(all(null_tests)){
        actionButton(inputId="save",label=h3(HTML("Save measure")),width="95%")
      }else{
        helpText(HTML('Nothing measured yet.'))
      }
    })

    observeEvent(input$save, {
      isolate({
        img_file <- rv$images[rv$imi]
        img1 <- magick::image_read(img_file)

        newline <- c(img_file,
                     image_info(img1)$width,
                     image_info(img1)$height,
                     round(rv$shore),
                     round(rv$whale),
                     round(rv$brng),
                     as.character(Sys.time()),
                     input$feature,
                     input$lat,
                     input$lon,
                     input$elev,
                     input$focal,
                     input$comm)
        newline <- paste(newline, collapse=',')
        newline <- paste0(newline,'\n')
        #print(newline)
        cat(newline, file=log_path, append = TRUE)
        rv$reload <- rv$reload + 1
        #if(!input$filter){rv$imi <- rv$imi + 1}
        rv$brng <- NULL
        rv$whale <- NULL
        rv$shore <- NULL
        #shinyjs::reset('zoom')
      })
    })

    observeEvent(input$cant, {
      isolate({
        # Save measurements
        if(is.null(rv$brng)){rv$brng <- NA}
        if(is.null(rv$whale)){rv$whale <- NA}
        if(is.null(rv$shore)){rv$shore <- NA}

        img_file <- rv$images[rv$imi]
        img1 <- magick::image_read(img_file)
        newline <- c(img_file,
                     image_info(img1)$width,
                     image_info(img1)$height,
                     round(rv$shore),
                     round(rv$whale),
                     round(rv$brng),
                     as.character(Sys.time()),
                     input$feature,
                     input$lat,
                     input$lon,
                     input$elev,
                     input$focal,
                     input$comm)
        newline <- paste(newline, collapse=',')
        newline <- paste0(newline,'\n')
        #print(newline)
        cat(newline, file=log_path, append = TRUE)
        rv$reload <- rv$reload + 1
        if(!input$filter){rv$imi <- rv$imi + 1}
        rv$brng <- NULL
        rv$whale <- NULL
        rv$shore <- NULL
        #shinyjs::reset('zoom')
      })
    })

    observeEvent(input$skip, {
      isolate({ rv$imi <- rv$imi + 1 })
      rv$brng <- NULL
      rv$whale <- NULL
      rv$shore <- NULL
      rv$imi_measures <- data.frame()
      shinyjs::reset('feature')
      shinyjs::reset('lat')
      shinyjs::reset('lon')
      shinyjs::reset('elev')
      shinyjs::reset('focal')
      shinyjs::reset('comm')
      shinyjs::reset('zoom')
    })

    observeEvent(rv$imi, {
      isolate({
        if(rv$imi > length(rv$images)){
          rv$imi <- 1
        }
        isolate({
          if(nrow(previous_measures)>0){
            rv$imi_measures <- rv$previous_measures %>% filter(X1 == rv$images[rv$imi])
          }
        })
      })
    })

    output$table1 <- renderDataTable({
      DT::datatable(rv$imi_measures)
    })
  }

  ######################################################################
  ######################################################################
  ######################################################################

  shinyApp(ui, server)

}
