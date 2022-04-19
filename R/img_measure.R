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
#' @param scroll_width Width, in pixels, of image scrolling portion of the app, to customize the app for your specific screen.
#' @param scroll_height Height, in pixels, of image scrolling portion of the app, to customize the app for your specific screen.
#'
#' @details This function uses `EBImage` to read and display images. It uses `exifr` and `exiftoolr` to
#' read image metadata.
#'
#' Workflow notes:
#' \itemize{
#' \item If your photos are large, you can adjust the zoom options and scroll around
#' the image using your mousepad.
#' \item Double-click on the whale/boat waterline to grab its pixel location.
#' \item Single-click on the horizon/shore line to grab its pixel location.
#' \item To save these measurements, click the "Save" button that appears.
#' \item The app will automatically advance to the next image.
#' \item If you can't take both of those measurements for an image, click the "Can't" button to mark
#' that image as processed as advance to the next image.
#' \item Advance to the next image without saving measurements by clicking the "Skip" button.
#' \item Note that by default, the app will only show images that do not yet
#' have measurements within the `log_path` file.
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
#' }
#'
#' @import shiny
#' @import dplyr
#' @export
#'
image_measure <- function(img_path='images/',
                          log_path='measures.csv',
                          scroll_width = 1700,
                          scroll_height = 400){

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

    # Try it
    image_measure()

  } #===========================================================================

  if(! 'EBImage' %in% installed.packages()){
    BiocManager::install("EBImage")
  }

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


  ######################################################################
  ######################################################################
  ######################################################################
  ######################################################################

  ui <- fluidPage(
    br(),
    fluidRow(
      column(2,uiOutput('save')),
      column(2,actionButton(inputId="cant",label=h4(HTML("Can't<br/>measure")),width="95%")),
      column(2,actionButton(inputId="skip",label=h4(HTML("Skip<br/>for now")),width="95%")),
      column(2,actionButton(inputId="clear",label=h4(HTML("Clear<br/>measures")),width="95%")),
      column(4,textOutput("imgname"))),
    hr(),
    fluidRow(column(4,
                    helpText(HTML('<b>Double-click</b> on <span style="color:red">whale/boat waterline</span>')),
                    helpText(HTML('then <b>single-click</b> on the <span style="color:green">horizon/shore</span> directly behind it.'))),
             column(4,radioButtons('zoom', label='Set zoom %',
                                    choices=as.character(seq(10,100,by=10)),
                                    selected='30', width='100%', inline=TRUE)),
             column(4,checkboxInput('filter', label='Filter to unmeasured images',
                                    value=TRUE,width='100%'))),
    hr(),
    fluidRow(column(12,
                    shinydashboard::box(
                      style=paste0('width:',
                                   scroll_width,
                                   'px; overflow-x: scroll; height:',
                                   scroll_height,
                                   'px; overflow-y: scroll;'),
                      uiOutput('img_show'),
                      width=12))),
    hr(),
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
    rv$imi <- 1
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
          })
        })
      }else{
        rv$previous_measures <- data.frame()
      }
    })

    observe({
      if(input$filter){
        measured_files <- rv$previous_measures$X1
        not_yet_measured <- ! image_files %in% measured_files
        images <- image_files[not_yet_measured]
      }else{
        images <- image_files
      }
      rv$images <- images

      if(length(rv$images)==0){
        showModal(modalDialog(
          'No more images to measure!',
          title = 'Finished!', footer = modalButton("Dismiss"),
          size = c("m"), easyClose = TRUE, fade = TRUE
        ))
      }
    })

    # -------------------------------------------------------------------
    # Display the image

    output$img_show <- renderUI({
      zoom <- input$zoom %>% as.numeric
      zoom <- as.numeric(zoom / 100)
      f <- rv$images[rv$imi] %>% as.character
      img <- EBImage::readImage(f)
      #img <- OpenImageR::readImage(f)
      plotOutput("plot3",
                 width=(dim(img)[1] * zoom),
                 height=(dim(img)[2]) * zoom,
                 click = clickOpts(id="img_click"),
                 dblclick = clickOpts(id='img_double'))
    })

    output$plot3 <- renderPlot({
      if(!is.null(rv$images)){
        if(length(rv$images)>0){
        f <- rv$images[rv$imi] %>% as.character
        img <- EBImage::readImage(f)
        #img <- OpenImageR::readImage(f)
        #OpenImageR::imageShow(img)
        plot(img)
        if(!is.null(rv$brng)){ abline(v=rv$brng, col='yellow', lwd=3) }
        if(!is.null(rv$whale)){ abline(h=rv$whale, col='red', lwd=3) }
        if(!is.null(rv$shore)){ abline(h=rv$shore, col='green', lwd=3) }
        }
      }
    })

    # -------------------------------------------------------------------
    # Get measurements

    observeEvent(input$img_double, {
      isolate({
        if(is.null(rv$brng) & is.null(rv$whale)){
          if(!is.null(input$img_double)){
            rv$brng <- input$img_double$x
            rv$whale <- input$img_double$y
          }
        }
      })
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
              ' (filtered from ',length(image_files),' image files) :: ',rv$images[rv$imi])
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
        actionButton(inputId="save",label=h3(HTML("Save & Next")),width="95%")
      }else{
        helpText(HTML('Cannot save until <b>both</b> the horizon/shore <b>and</b> the whale/boat waterline have been measured.'))
      }
    })

    observeEvent(input$save, {
      isolate({
        img_file <- rv$images[rv$imi]
        newline <- c(img_file, round(rv$shore), round(rv$whale), round(rv$brng), as.character(Sys.time()))
        newline <- paste(newline, collapse=',')
        newline <- paste0(newline,'\n')
        #print(newline)
        cat(newline, file=log_path, append = TRUE)
        rv$reload <- rv$reload + 1
        if(!input$filter){rv$imi <- rv$imi + 1}
        rv$brng <- NULL
        rv$whale <- NULL
        rv$shore <- NULL
        shinyjs::reset('zoom')
      })
    })

    observeEvent(input$cant, {
      isolate({
        # Save measurements
        if(is.null(rv$brng)){rv$brng <- NA}
        if(is.null(rv$whale)){rv$whale <- NA}
        if(is.null(rv$shore)){rv$shore <- NA}

        img_file <- rv$images[rv$imi]
        newline <- c(img_file, round(rv$shore), round(rv$whale), round(rv$brng), as.character(Sys.time()))
        newline <- paste(newline, collapse=',')
        newline <- paste0(newline,'\n')
        #print(newline)
        cat(newline, file=log_path, append = TRUE)
        rv$reload <- rv$reload + 1
        if(!input$filter){rv$imi <- rv$imi + 1}
        rv$brng <- NULL
        rv$whale <- NULL
        rv$shore <- NULL
        shinyjs::reset('zoom')
      })
    })

    observeEvent(input$skip, {
      isolate({ rv$imi <- rv$imi + 1 })
      rv$brng <- NULL
      rv$whale <- NULL
      rv$shore <- NULL
      shinyjs::reset('zoom')
    })

    observeEvent(rv$imi, {
      isolate({
        if(rv$imi > length(rv$images)){
          rv$imi <- 1
        }
      })
    })

  }

  ######################################################################
  ######################################################################
  ######################################################################

  shinyApp(ui, server)

}
