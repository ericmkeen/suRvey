#' Title
#'
#' @param url_steps  desc
#' @param url_sections  desc
#' @param url_cats  desc
#' @param section_plotter desc
#' @param analyst  desc
#' @param images  desc
#' @param log_path  desc
#' @param scroll_width  desc
#' @param scroll_height  desc
#'
#' @return desc
#' @export
#' @import dplyr
#'
img_sections <- function(url_steps,
                         url_sections,
                         url_cats,
                         section_plotter,
                         analyst,
                         images,
                         log_path='measures.csv',
                         scroll_width = 1000,
                         scroll_height = 500){

  if(FALSE){ #==================================================================

    # NEXT STEPS
    # make a function that takes stage 1 measurements and adds lines/points to an image
    # change img_path input to take a vector of image paths

    library(BiocManager)
    library(shiny)
    library(dplyr)
    library(shinydashboard)
    img_path <- 'data_raw/images/'
    log_path <- 'data_raw/measures.csv'
    scroll_height = 400
    scroll_width = 1700

    url_steps <- 'https://docs.google.com/spreadsheets/d/12a4QNUR4mjv-Nzn3oImYlch6lMqT1Jj5a5UNSh7poDw/edit#gid=0'
    url_sections <- 'https://docs.google.com/spreadsheets/d/12a4QNUR4mjv-Nzn3oImYlch6lMqT1Jj5a5UNSh7poDw/edit#gid=932485904'
    url_cats <- 'https://docs.google.com/spreadsheets/d/12a4QNUR4mjv-Nzn3oImYlch6lMqT1Jj5a5UNSh7poDw/edit#gid=957420469'

    steps <- gsheet::gsheet2tbl(url_steps)
    steps

    source('data_raw/sectionize.R')
    sectionize(steps, color = 'black', base_plot = TRUE)

    (lf <- dir('data_raw/images/'))
    (images <- paste0('data_raw/images/', lf))

    # Try it
    img_sections(url_steps,
                 url_sections,
                 url_cats,
                 section_plotter = sectionize,
                 analyst = 'Eric',
                 images = images,
                 log_path = 'data_raw/measures.csv')

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

  # if(! dir.exists(img_path)){ stop('Path to images folder does not exist!') }
  # (image_files <- dir(img_path))
  # if(length(image_files) == 0){ stop('No images were found within the images folder!')}
  # (image_files <- paste0(img_path, image_files))
  image_files <- images

  # Load GoogleSheet ===========================================================
  # Load steps
  steps_all <- gsheet::gsheet2tbl(url_steps)
  steps_all$instructions <- tidyr::replace_na(steps_all$instructions, replace = '')

  # Load sections
  sections <- gsheet::gsheet2tbl(url_sections)
  sections

  # Load categories
  cats <- gsheet::gsheet2tbl(url_cats)
  names(cats) <- gsub(' ','_',names(cats))
  cats$guidance <- tidyr::replace_na(cats$guidance, replace = '')
  head(cats)

  ######################################################################
  ######################################################################
  ######################################################################
  ######################################################################

  ui <- fluidPage(
    br(),
    fluidRow(
      column(4,uiOutput('save')),
      column(1,actionButton(inputId="skip",label=h4(HTML("Skip")),width="95%")),
      column(1,actionButton(inputId="reset",label=h4(HTML("Reset")),width="95%")),
      column(2,actionButton(inputId="cancel_last",label=h4(HTML("Cancel last")),width="95%")),
      column(3, sliderInput('zoom', label = 'Set zoom %', min = 10, max= 100, value=30, step=5, width = '100%')),
      column(1,checkboxInput('filter', label='Filter to unmeasured images',
                             value=TRUE,width='100%'))),
    hr(),
    uiOutput('stage1'),
    uiOutput('stage2'),
    hr(),
    fluidRow(column(9,
                    shinydashboard::box(
                      style=paste0('width:',
                                   scroll_width,
                                   'px; overflow-x: scroll; height:',
                                   scroll_height,
                                   'px; overflow-y: scroll;'),
                      uiOutput('img_show'),
                      width=12)),
             column(3,
                    br(),
                    br(),
                    textAreaInput('comment',
                                  'Any other comments on this whale:',
                                  rows = 6,
                                  width = '95%'))),
    br(),
    fluidRow(column(12,textOutput("imgname"))),
    hr(),
    br()
  )

  ######################################################################
  ######################################################################
  ######################################################################
  ######################################################################

  server <- function(input, output, session) {

    # -------------------------------------------------------------------
    rv <- reactiveValues()
    rv$previous_measures <- previous_measures
    rv$reload <- 0
    rv$images <- image_files
    rv$img <- NULL
    rv$imi <- 1
    rv$img_dim <- c(NA, NA)
    rv$x <- NA
    rv$y <- NA
    rv$steps <- data.frame()
    rv$stepi <- NULL
    rv$sections <- data.frame()
    rv$stage <- 1
    rv$cat_logger <- 1

    # Stage 1 work =============================================================

    output$stage1 <- renderUI({
      if(rv$stage == 1){
        fluidRow(column(4, htmlOutput('steps')),
                 column(4, uiOutput('steps_cant')),
                 column(2))
      }
    })

    output$steps <- renderText({
      toprint <- ''
      if(rv$stage == 1){
        if(nrow(rv$steps) == nrow(steps_all)){
          rv$stage <- 2
        }else{
          stepi <- nrow(rv$steps) + 1
          #stepi <- 2
          (rv$stepi <- steps_all[stepi,])
          toprint <- paste0('<h3>Click on the ', rv$stepi$measure,'</h3>',
                            '<em>',rv$stepi$instructions,'</em> ' )
        }
      }
      toprint
    })

    output$steps_cant <- renderUI({
      if(rv$stage == 1){
        actionButton('cant_this', h4(HTML('Unable to measure')), width='95%')
      }
    })

    # Unable to measure
    observeEvent(input$cant_this, {
      isolate({
        if(!is.null(rv$steps) &
           !is.null(rv$stepi)){

          # Add to steps df
          stepi <- data.frame(image = rv$images[rv$imi],
                              width = rv$img_dim[1],
                              height = rv$img_dim[2],
                              stage = rv$stepi$stage,
                              measure = rv$stepi$measure,
                              x = NA,
                              y = NA,
                              section = NA,
                              z1 = NA,
                              z2 = NA,
                              z3 = NA,
                              analyst = analyst,
                              logged = Sys.time() %>% as.character())
          names(stepi)[names(stepi) == 'z1'] <- gsub(' ','_',names(cats)[1]) %>% tolower
          names(stepi)[names(stepi) == 'z2'] <- gsub(' ','_',names(cats)[2]) %>% tolower
          names(stepi)[names(stepi) == 'z3'] <- gsub(' ','_',names(cats)[3]) %>% tolower
          print(stepi)
          rv$steps <- rbind(rv$steps, stepi)
        }
      })})

    # Single click
    observeEvent(input$img_click, {
      isolate({
        if(!is.null(rv$steps) &
           !is.null(input$img_click) &
           !is.null(rv$stepi)){

          # Add to steps df
          if(rv$stage == 1){
            stepi <- data.frame(image = rv$images[rv$imi],
                                width = rv$img_dim[1],
                                height = rv$img_dim[2],
                                stage = rv$stepi$stage,
                                measure = rv$stepi$measure,
                                x = round(input$img_click$x, 3),
                                y = round(input$img_click$y, 3),
                                section = NA,
                                z1 = NA,
                                z2 = NA,
                                z3 = NA,
                                analyst = analyst,
                                logged = Sys.time() %>% as.character())
            names(stepi)[names(stepi) == 'z1'] <- gsub(' ','_',names(cats)[1]) %>% tolower
            names(stepi)[names(stepi) == 'z2'] <- gsub(' ','_',names(cats)[2]) %>% tolower
            names(stepi)[names(stepi) == 'z3'] <- gsub(' ','_',names(cats)[3]) %>% tolower
            print(stepi)
            rv$steps <- rbind(rv$steps, stepi)
          }
        }
      })})

    # Stage 2 work =============================================================

    output$stage2 <- renderUI({
      if(rv$stage == 2){
        fluidRow(column(2, htmlOutput('sections')),
                 uiOutput('cats'),
                 column(2, uiOutput('cats_save')))
      }
    })

    output$sections <- renderText({
      toprint <- ''
      if(rv$stage == 2){
        if(rv$cat_logger == nrow(sections)){
          toprint <- '<h3>Measuring complete! Click Save!</h3>'
        }else{
          secti <- rv$cat_logger
          rv$secti <- sections[secti, ]
          toprint <- paste0('<h3>',rv$secti$section,'</h3> <br>',
                            '<em>Help:</em> ', rv$secti$guidance)
        }
      }
      toprint
    })

    output$cats <- shiny::renderUI({
      rv$cat_logger # place here to trigger re-run

      if(rv$stage == 2){
        lapply(1:nrow(cats),
               function(x){
                 shiny::column(2,
                               h4(cats[x, 1]),
                               shiny::selectInput(paste0('cat_',cats[x, 1],'_',names(cats)[2]),
                                                  label=names(cats)[2],
                                                  choices=stringr::str_split(cats[x,2],', ')[[1]],
                                                  selectize=FALSE,
                                                  size=1),
                               shiny::selectInput(paste0('cat_',cats[x, 1],'_',names(cats)[3]),
                                                  label=names(cats)[3],
                                                  choices=stringr::str_split(cats[x,3],', ')[[1]],
                                                  selectize=FALSE,
                                                  size=1))
               })
      }
    })

    output$cats_save <- renderUI({
      if(rv$stage == 2 & rv$cat_logger < nrow(sections)){
        actionButton('section_save', h4(HTML('Save for<br/>this section')), width='95%')
      }
    })

    observeEvent(input$section_save, {
      # add actions here
      if(rv$stage == 2){
        inputs <- shiny::reactiveValuesToList(input) # get list of all inputs
        sections <- data.frame()
        x=1
        for(x in 1:nrow(cats)){
          input1 <- cats[x,1]

          # Save col 2
          input_name <- paste0('cat_',cats[x, 1],'_',names(cats)[2])
          input_x <- which(names(inputs) == input_name)
          inputx <- inputs[[input_x]]
          z2 <- inputx

          # Save col 3
          input_name <- paste0('cat_',cats[x, 1],'_',names(cats)[3])
          input_x <- which(names(inputs) == input_name)
          inputx <- inputs[[input_x]]
          z3 <- inputx

          # Setup row
          stepi <- data.frame(image = rv$images[rv$imi],
                              width = rv$img_dim[1],
                              height = rv$img_dim[2],
                              stage = rv$stepi$stage,
                              measure = NA,
                              x = NA,
                              y = NA,
                              section = rv$secti$section,
                              z1 = input1,
                              z2 = z2,
                              z3 = z3,
                              analyst = analyst,
                              logged = Sys.time() %>% as.character())
          names(stepi)[names(stepi) == 'z1'] <- gsub(' ','_',names(cats)[1]) %>% tolower
          names(stepi)[names(stepi) == 'z2'] <- gsub(' ','_',names(cats)[2]) %>% tolower
          names(stepi)[names(stepi) == 'z3'] <- gsub(' ','_',names(cats)[3]) %>% tolower
          names(stepi) <- tolower(names(stepi))
          print(stepi)
          sections <- rbind(sections, stepi)
        }
        print(sections)
        rv$sections <- rbind(rv$sections, sections)
        rv$cat_logger <- rv$cat_logger + 1

        #output$cats <- shiny::renderUI(NULL)
        # x=1
        # inputs <- shiny::reactiveValuesToList(input) # get list of all inputs
        # print(inputs)
        # for(x in 1:nrow(cats)){
        #   input1 <- cats[x,1]
        #   input_name <- paste0('cat_',cats[x, 1],'_',names(cats)[2]) # col 2
        #   print(input_name)
        #   updateSelectInput(session, input_name)
        #   #shinyjs::reset(input_name)
        #   input_name <- paste0('cat_',cats[x, 1],'_',names(cats)[3]) # col 3
        #   print(input_name)
        #   updateSelectInput(session, input_name)
        #   #shinyjs::reset(input_name)
        # }
      }
    })


    ############################################################################
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

    dezoom <- reactive({
      input$zoom
    }) %>% debounce(1000)

    output$img_show <- renderUI({
      zoom <- dezoom() %>% as.numeric
      #zoom <- input$zoom %>% as.numeric
      zoom <- as.numeric(zoom / 100)
      f <- rv$images[rv$imi] %>% as.character
      img <- EBImage::readImage(f)
      rv$img_dim <- dim(img)
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
          if(rv$stage == 2){
            section_plotter(rv$steps)
          }
        }
      }
    })

    # -------------------------------------------------------------------
    # Show info

    output$imgname <- renderText({
      paste0('Image ',rv$imi,' out of ',length(rv$images),
             ' (filtered from ',length(image_files),' image files) :: ',rv$images[rv$imi])
    })

    # -------------------------------------------------------------------
    # Navigation

    observeEvent(input$reset, {
      rv$steps <- data.frame()
    })

    observeEvent(input$cancel_last, {
      if(rv$stage == 1){
        if(nrow(rv$steps) > 0){
          rv$steps <- rv$steps[1:(nrow(rv$steps)-1), ]
        }
      }
      if(rv$stage == 2){
        if(nrow(rv$sections) > 0){
          print(rv$sections)
          rv$sections <- rv$sections[1:(nrow(rv$sections)-nrow(cats)), ]
          rv$cat_logger <- 1
          print(rv$sections)
        }
      }

    })

    output$save <- renderUI({
      null_tests <- c(nrow(rv$steps) == nrow(steps_all),
                      rv$cat_logger == nrow(sections))
      if(all(null_tests)){
        actionButton(inputId="save",label=h3(HTML("Save & Next")),width="95%")
      }else{
        helpText(HTML('Cannot save until <b>all</b> measurements have been logged.'))
      }
    })

    observeEvent(input$save, {
      isolate({
        img_file <- rv$images[rv$imi]

        # Handle stage 1
        rvsteps <- rv$steps
        rvsteps$comment <- ifelse(is.null(input$comment), '', input$comment)
        for(i in 1:nrow(rvsteps)){
          (stepi <- rvsteps[i, ])
          newline <- paste((stepi), collapse = ',')
          newline <- paste0(newline,'\n')
          print(newline)
          cat(newline, file=log_path, append = TRUE)
        }

        # Handle stage 2
        rvsteps <- rv$sections
        rvsteps$comment <- ifelse(is.null(input$comment), '', input$comment)
        for(i in 1:nrow(rvsteps)){
          (stepi <- rvsteps[i, ])
          newline <- paste((stepi), collapse = ',')
          newline <- paste0(newline,'\n')
          print(newline)
          cat(newline, file=log_path, append = TRUE)
        }

        # Handle resets
        rv$reload <- rv$reload + 1
        if(!input$filter){rv$imi <- rv$imi + 1}
        rv$steps <- data.frame()
        rv$sections <- data.frame()
        rv$cat_logger <- 1
        rv$stage <- 1
        rv$stepi <- NULL
        rv$secti <- NULL
        shinyjs::reset('zoom')
        shinyjs::reset('comment')
      })
    })

    observeEvent(input$skip, {
      isolate({ rv$imi <- rv$imi + 1 })
      shinyjs::reset('zoom')
      shinyjs::reset('comment')
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


