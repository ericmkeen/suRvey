survey_app <- function(observers,
                       platforms,
                       optics,
                       landmarks,
                       cues,
                       species,
                       behaviours,
                       scan_target = 15,
                       gps_interval = 10,
                       data_width = 20,
                       scroll_height = 475,
                       button_size = 200,
                       button_padding = 30,
                       keypad_size = 250,
                       keypad_padding = 20,
                       tab_size = 150,
                       tab_width = 150,
                       comment_1 = 'Photo-ID acquired.',
                       comment_2 = 'Memorable sighting!',
                       comment_3 = 'Conditions changed dramatically during scan.',
                       comment_4 = 'Scan cut short early.',
                       comment_5 = 'Severe revision needed -- fix manually later!',
                       comment_6 = 'App crashed -- trying again.'
                       ){

if(FALSE){
  library(shiny)

  observers <- c('Grace','Janie','Ben')
  platforms <- c('Inside','Outside')
  optics <- c('N/A','Big Eyes', 'Binocs', 'Scope', 'Naked Eye')
  landmarks <- c('N/A','Gil Mtn', 'CMP peaks', 'Otter', 'Twartz', 'Farrant', 'N Fin shore')
  cues <- c('N/A','Blow','Vessel','Body','Fluke','Splash','Sound')

  species <- list('MarMam' = c('Humpback', 'Fin', 'Dalls porpoise', 'Harbour seal', 'Stellars sea lion', 'Elephant seal', 'Killer whale'),
                  'Vessel' = c('Large rec', 'Small rec', 'CFV', 'Sailing', 'Tug only', 'Tug+barge', 'Gitgaat', 'Research', 'Cruise', 'Tanker'))

  behaviours <- list('MarMam' = c('Active', 'Sleep', 'Feeding', 'BNF', 'Milling', 'Robust', 'Fast travel', 'RE-TR'),
                     'Vessel' = c('Fast travel', 'Slow travel', 'Fishing', 'With whales', 'Anchored'))

  # Try it
  survey_app(observers,
             platforms,
             optics,
             landmarks,
             cues,
             species,
             behaviours)

  data_width = 20
  scroll_height <- 475
  button_size <- 200
  button_padding <- 30
  keypad_size <- 250
  keypad_padding <- 20
  tab_size <- 150
  tab_width <- 150
  scan_target <- 15
  gps_interval = 10
  grabit <- 'store'

  comment_1 <- 'Photo-ID acquired.'
  comment_2 <- 'Memorable sighting!'
  comment_3 <- 'Conditions changed dramatically during scan.'
  comment_4 <- 'Scan cut short early.'
  comment_5 <- 'Severe revision needed -- fix manually later!'
  comment_6 <- 'App crashed -- trying again.'
  # GPS sensor info:
  # https://github.com/AugustT/shiny_geolocation/blob/master/accuracy_and_dynamic/server.r

  # figure out DT data fixing
  # further develop & package

  # Image processing: stabilize and make into package

  survey_app()
}
##############################################################################
##############################################################################
# Defaults

grabit <- 'store'


##############################################################################
##############################################################################
# Functions

get_filename <- function(dt=NULL){
  if(is.null(dt)){ dt <- Sys.Date() } ; dt
  new_filename <- paste0('data/',dt,'.csv') ; new_filename
  return(new_filename)
}

log_line <- function(event, event_data = ''){
  new_filename <- get_filename() ; new_filename
  if(!dir.exists('data')){dir.create('data')}
  new_line <- paste0(Sys.time(),',',event,',',paste(event_data, collapse=','),'\n') ; new_line
  cat(new_line, file=new_filename, append=TRUE)
  df <- read.csv(new_filename, header=FALSE)
  print(df)
  return(df)
}

load_all_days <- function(){
  lf <- list.files('data/') ; lf
  dfs <- data.frame()
  if(length(lf)>0){
    lf <- paste0('data/',lf) ; lf
    i=5
    for(i in 1:length(lf)){
      dfi <- read.csv(lf[i],stringsAsFactors=FALSE,header=FALSE, row.names=NULL) ; head(data)
      dfs <- plyr::rbind.fill(dfs,dfi)
    }
  }
  dfs
  return(dfs)
}

onStart <- function(){
  #log_line('B')

  onStop(function() {
    log_line('E')
  })
}

textstyle <- function(size, padding){
  paste0('padding:',padding,'px; font-size:',size,'%')
}


radio <- function(id, label_text, choices, default_choices = 'Other',
                  direction = 'vertical', width='95%', height='180px'){
  shinydashboard::box(width=12, style=paste0('height:',height,';overflow-y: scroll; overflow-x: scroll;'),
                      shinyWidgets::radioGroupButtons(
                        inputId = id,
                        label = h4(label_text),
                        choices = c(choices, default_choices),
                        size='lg', justified = TRUE, direction = direction,
                        width = width,
                        checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
                                         no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")))
  )
}

picker <- function(id, label_text, choices, default_choices = 'Other'){
  shinyWidgets::pickerInput(
    inputId = id,
    label = h4(label_text),
    choices = c(choices, default_choices),
    width='100%',
    options = list(
      size = 10)
  )
}

##############################################################################
##############################################################################
# Get current sighting number at start up

dfall <- load_all_days()
(sits <- dfall %>% dplyr::filter(V2 == 'SIT'))
(maxsitno <- max(as.numeric(sits$V3), na.rm=TRUE))

##############################################################################
##############################################################################
# UI

ui <- fluidPage(
  shinyjs::useShinyjs(),
  br(),
  fluidRow(column(12,

                  ##############################################################################
                  ##############################################################################
                  # Keypad


                  sidebarPanel(
                    #br(),
                    fluidRow(column(1), column(11, span(textOutput('keypad'), style=paste0('font-size:',keypad_size,'%')))),
                    #br(),
                    fluidRow(column(12,
                                    actionButton('b1','1', style=textstyle(keypad_size, keypad_padding), width= '30%'),
                                    actionButton('b2','2', style=textstyle(keypad_size, keypad_padding), width= '30%'),
                                    actionButton('b3','3', style=textstyle(keypad_size, keypad_padding), width= '30%')
                    )),
                    fluidRow(column(12,
                                    actionButton('b4','4', style=textstyle(keypad_size, keypad_padding), width= '30%'),
                                    actionButton('b5','5', style=textstyle(keypad_size, keypad_padding), width= '30%'),
                                    actionButton('b6','6', style=textstyle(keypad_size, keypad_padding), width= '30%')
                    )),
                    fluidRow(column(12,
                                    actionButton('b7','7', style=textstyle(keypad_size, keypad_padding), width= '30%'),
                                    actionButton('b8','8', style=textstyle(keypad_size, keypad_padding), width= '30%'),
                                    actionButton('b9','9', style=textstyle(keypad_size, keypad_padding), width= '30%')
                    )),
                    fluidRow(column(12,
                                    actionButton('b0','0', style=textstyle(keypad_size, keypad_padding), width= '30%'),
                                    actionButton('bdec','.', style=textstyle(keypad_size, keypad_padding), width= '30%')
                    )),
                    br(),
                    fluidRow(column(12,
                                    actionButton('bclr','clear', style=textstyle(keypad_size*.7, keypad_padding*.7), width= '95%')
                    )),
                    br(),
                    width = 3
                  ),

                  ##############################################################################
                  ##############################################################################

                  mainPanel(

                    tags$style(HTML(paste0("
    .tabbable > .nav > li > a {font-size:",tab_size,"%; width: ",tab_width,"px;}
  "))),

                    tabsetPanel(

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Effort',
                               br(),
                               fluidRow(
                                 column(3, radio('obs_data', 'On data entry:', observers)),
                                 column(2, radio('obs_left','On Left:', c('None', observers))),
                                 column(2, radio('obs_right','On Right:', c('None', observers))),
                                 column(2, radio('obs_io','Ind. obs.:', c('None', observers))),
                                 column(3, radio('platform','Platform:', platforms))),
                               br(),
                               fluidRow(column(1),
                                        column(10,
                                               helpText('Update conditions *before* starting your scan!'),
                                               uiOutput('scan'),
                                               br(),
                                               textOutput('scan_duration')
                                        ),
                                        column(1))
                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Conditions',
                               br(),
                               shinydashboard::box(
                                 style=paste0('height:', scroll_height,'px; overflow-y: scroll;'),
                                 fluidRow(column(3, h4('Left edge (degrees)'), uiOutput('cz_left')),
                                          column(3, h4('Right edge (degrees)'), uiOutput('cz_right')),
                                          column(3, h4('Near edge (km)'), uiOutput('cz_close')),
                                          column(3, h4('Far edge (km)'), uiOutput('cz_far'))),
                                 hr(),
                                 fluidRow(column(3, h4('Beaufort scale:'), radio('bft', NULL, choices=c('N/A',0:6), default_choices=NULL, height='150px')),
                                          column(3, h4('Wave height (ft):'), radio('wave', NULL, choices=c('N/A',0,.5,1:10), default_choices=NULL, height='150px')),
                                          column(3, h4('Visibility (km):'), radio('viz', NULL, choices=c('N/A',30:0), default_choices=NULL, height='150px')),
                                          column(3, h4('% cloud cover:'), radio('bft', NULL, choices=c('N/A',100:0), default_choices=NULL, height='150px'))),
                                 hr(),
                                 fluidRow(column(3, h4('Precipitation:'), radio('precip', NULL, choices=c('N/A', 'Clear','Drizzle','Pouring'), default_choices=NULL, height='180px')),
                                          column(3, h4('Fog:'), radio('fog', NULL, choices=c('None','Thin','Thick'), default_choices=NULL, height='180px')),
                                          column(3, h4('Haze:'),  radio('haze', NULL, choices=c('None', 'Mild','Severe'), default_choices=NULL, height='180px')),
                                          column(3, h4('Horizon smear:'),  radio('smear', NULL, choices=c('None', 'Mild','Severe'), default_choices=NULL, height='180px'))),
                                 hr(),
                                 fluidRow(column(3, h4('Glare present?'), radio('glare', NULL, choices=c('None', 'Mild','Severe'), default_choices=NULL, height='180px')),
                                          column(3,
                                                 h4('Left edge (degrees)'), uiOutput('glare_left'),
                                                 h4('Right edge (degrees)'), uiOutput('glare_right')),
                                          column(6, br(), br(), br(),
                                                 actionButton('cz_store','Store condition zone', style=textstyle(button_size*.8, button_padding*1.4), width="95%"))),
                                 width=12
                               ) # end of box
                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Sighting',
                               br(),
                               shinydashboard::box(
                                 style=paste0('height:', scroll_height,'px; overflow-y: scroll;'),
                                 fluidRow(column(3,
                                                 h4('Bearing*'),
                                                 uiOutput('bearing'),
                                                 br(),
                                                 picker('inline', 'In line with*', c(landmarks)),
                                                 br(),
                                                 picker('cue','Detection cue*', c(cues))),
                                          column(3,
                                                 h4('Reticle*'), uiOutput('reticle'),
                                                 br(),
                                                 picker('reticle_how', 'Measured with*', c(optics)),
                                                 br(),
                                                 h4('Est. distance (km)*'), uiOutput('distance')),
                                          column(3,
                                                 h4('Max. group size*'), uiOutput('grp_max'),
                                                 br(),
                                                 h4('Min. group size*'), uiOutput('grp_min'),
                                                 br(),
                                                 h4('Best estimate*'), uiOutput('grp_best')),
                                          column(3,
                                                 radio('species_type', 'Sighting of ...*', names(species), default_choices = NULL, height='150px'),
                                                 h4('Primary species*'), uiOutput('species'))
                                 ),
                                 hr(),
                                 fluidRow(column(3, h4('Primary bhvr:'), uiOutput('bhvr_primary')),
                                          column(3, h4('Secondary:'), uiOutput('bhvr_secondary')),
                                          column(3, h4('Tertiary:'), uiOutput('bhvr_tertiary')),
                                          column(3, h4('Direction:'), radio('direction', NULL, c('N/A','None','N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'), default_choices = NULL, height='150px'))),
                                 hr(),
                                 fluidRow(column(3, h4('Threat interaction?'), radio('threat', NULL, c('FALSE','TRUE'), default_choices = NULL, height='100px')),
                                          column(3, h4('# calves:'), radio('calves', NULL, c('N/A',0:10), default_choices = NULL, height='100px')),
                                          column(3, h4('# males: '), radio('direction', NULL, c('N/A',0:10), default_choices = NULL, height='100px')),
                                          column(3, h4('Acoustics:'), radio('acoustics', NULL, c('N/A', 'Cannot hear', 'Maybe', 'Yes'), default_choices = NULL, height='100px'))),
                                 hr(),
                                 fluidRow(column(1),
                                          column(10, actionButton('sit_store','Store new sighting',style=textstyle(button_size*.8, button_padding*.6), width="100%")),
                                          column(1)),
                                 width = 12) # end of box

                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Update',
                               br(),
                               shinydashboard::box(
                                 style=paste0('height:', scroll_height,'px; overflow-y: scroll;'),
                                 fluidRow(column(12, DTOutput('up_spp_table'))),
                                 hr(),
                                 fluidRow(column(4,
                                                 h4('Update bearing'), uiOutput('up_bearing'),
                                                 h4('New reticle'), uiOutput('up_reticle')),
                                          column(4,
                                                 h4('Measured with'), radio('up_reticle_how', NULL, c('N/A', optics), height='120px')),
                                          column(4,
                                                 h4('New distance (km)'), uiOutput('up_distance'))),
                                 hr(),
                                 fluidRow(column(3,
                                                 h4('Add estimate from:'), radio('up_obs', NULL, observers, height='100px')),
                                          column(3,
                                                 h4('Has group size changed?'), radio('up_grp_type', NULL, choices=c('Unchanged', 'Changed'), default_choices = NULL, height='100px')),
                                          column(2, h4('Max. group size'), uiOutput('up_max')),
                                          column(2, h4('Min. group size'), uiOutput('up_min')),
                                          column(2, h4('Best estimate'), uiOutput('up_best'))),
                                 hr(),
                                 fluidRow(column(4,
                                                 h4('Mixed-spp group?'), radio('up_mixed', NULL, choices=c('No', 'Mixed'), default_choices = NULL, height='120px')),
                                          column(2, h4('2nd spp:'), uiOutput('up_spp2')),
                                          column(2, h4('% of group:'), radio('up_per2', NULL, choices=round(seq(99,1,by=-5)), default_choices= NULL, height='150px')),
                                          column(2, h4('3rd spp:'), uiOutput('up_spp3')),
                                          column(2, h4('% of group:'), radio('up_per3', NULL, choices=round(seq(99,1,by=-5)), default_choices= NULL, height='150px'))),
                                 hr(),
                                 fluidRow(column(3, h4('Update primary bhvr:'), uiOutput('up_primary')),
                                          column(3, h4('Update secondary:'), uiOutput('up_secondary')),
                                          column(3, h4('Update tertiary:'), uiOutput('up_tertiary')),
                                          column(3, h4('Update direction:'), radio('up_direction', NULL, c('N/A','None','N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'), default_choices = NULL, height='150px'))),
                                 hr(),
                                 fluidRow(column(3, h4('New threat interaction?'), radio('up_threat', NULL, c('FALSE','TRUE'), default_choices = NULL, height='100px')),
                                          column(3, h4('Update acoustics:'), radio('up_acoustics', NULL, c('N/A', 'Cannot hear', 'Maybe', 'Yes'), default_choices = NULL, height='100px')),
                                          column(6, actionButton('update','Update sighting', style=textstyle(button_size, button_padding), width="95%"), br(), br())),
                                 width = 12), # end of box
                               fluidRow(column(12, helpText('Looking to correct a mistake? Edit the data directly under the "Review" tab.'))),

                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Comment',
                               br(),
                               shinydashboard::box(
                                 style=paste0('height:', scroll_height,'px; overflow-y: scroll;'),
                                 fluidRow(column(12,
                                                 h4('Relate this comment to a sighting:'),
                                                 DTOutput('com_spp_table'))),
                                 hr(),
                                 fluidRow(column(12,
                                                 textInput('com_manual',h4('Type comment and press "Store":'),value='',width='100%'),
                                                 actionButton('com_store', 'Store comment',style=textstyle(button_size*.8, button_padding*.6), width="95%"))),
                                 hr(),
                                 fluidRow(column(12,
                                                 h4('... or choose a canned comment below:'),
                                                 actionButton('com_1', comment_1, style=textstyle(button_size*.8, button_padding*.4), width='95%'), br(),
                                                 actionButton('com_2', comment_2, style=textstyle(button_size*.8, button_padding*.4), width='95%'), br(),
                                                 actionButton('com_3', comment_3, style=textstyle(button_size*.8, button_padding*.4), width='95%'), br(),
                                                 actionButton('com_4', comment_4, style=textstyle(button_size*.8, button_padding*.4), width='95%'), br(),
                                                 actionButton('com_5', comment_5, style=textstyle(button_size*.8, button_padding*.4), width='95%'), br(),
                                                 actionButton('com_6', comment_6, style=textstyle(button_size*.8, button_padding*.4), width='95%'))),
                                 width = 12), # end of box
                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Review',
                               tabsetPanel(
                                 tabPanel(h4('Data'),
                                          br(),
                                          DTOutput('data'),
                                          br(),
                                          fluidRow(column(3,uiOutput("editclearscan")),
                                                   column(3,uiOutput("remove_row")),
                                                   column(3,uiOutput("copypaste")),
                                                   column(3,uiOutput("undo")))
                                 ),

                                 tabPanel(h4('Map'),
                                          br(),
                                          'hello')
                               )
                      )
                    ),

                    width= 9
                  )

  )),

  ##############################################################################
  ##############################################################################
  # GPS

  br(),
  fluidRow(
    tags$script(paste0('
              $(document).ready(function () {

                function getLocation(callback){
                var options = {
                  enableHighAccuracy: true,
                  timeout: 5000,
                  maximumAge: 0
                };

                navigator.geolocation.getCurrentPosition(onSuccess, onError);

                function onError (err) {
                  Shiny.onInputChange("geolocation", false);
                }

                function onSuccess (position) {
                  setTimeout(function () {
                    var coords = position.coords;
                    var timestamp = new Date();

                    console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
                    Shiny.onInputChange("geolocation", true);
                    Shiny.onInputChange("lat", coords.latitude);
                    Shiny.onInputChange("long", coords.longitude);
                    Shiny.onInputChange("accuracy", coords.accuracy);
                    Shiny.onInputChange("time", timestamp)

                    console.log(timestamp);

                    if (callback) {
                      callback();
                    }
                  }, 1100)
                }
              }

              var TIMEOUT = ',1000,'; //SPECIFY
              var started = false;
              function getLocationRepeat(){
                //first time only - no delay needed
                if (!started) {
                  started = true;
                  getLocation(getLocationRepeat);
                  return;
                }

                setTimeout(function () {
                  getLocation(getLocationRepeat);
                }, TIMEOUT);

              };

              getLocationRepeat();

            });
            ')), # end of tag
    column(3, h4('Latitude'), verbatimTextOutput("lat")),
    column(3, h4('Longitude'), verbatimTextOutput("long")),
    column(3, h4('Speed (kmh)'), verbatimTextOutput("speed")),
    column(3, h4('Time'), verbatimTextOutput("time")))

)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

server <- function(input, output, session) {

  ##############################################################################
  ##############################################################################
  # Setup reactive values

  rv <- reactiveValues()
  rv$df <- log_line('B', event_data = paste(rep(',',times=data_width), collapse=''))
  rv$df_backup <- NULL
  rv$keypad <- '0'
  gps_timer <- reactiveTimer(1000*gps_interval)

  # Effort =========================
  rv$scan <- 0
  rv$scan_start <- Sys.time()
  rv$scan_duration <- NULL
  scan_timer <- reactiveTimer(1000)

  # Conditions =======================
  rv$cz_left <- 1
  rv$cz_right <- 359
  rv$cz_close <- 0
  rv$cz_far <- 30
  rv$glare_left <- 170
  rv$glare_right <- 200

  # Sightings =========================
  rv$next_sit <- maxsitno + 1
  rv$bearing <- NULL
  rv$inline <- NULL
  rv$cue <- NULL
  rv$reticle <- NULL
  rv$reticle_how <- NULL
  rv$distance <- NULL
  rv$grp_max <- 1
  rv$grp_min <- 1
  rv$grp_best <- 1
  rv$species_type <- NULL
  rv$species <- NULL

  # Update sighting ===================
  rv$up_bearing <- NULL
  rv$up_reticle <- NULL
  rv$up_reticle_how <- NULL
  rv$up_distance <- NULL
  rv$up_max <- 0
  rv$up_min <- 0
  rv$up_best <- 0


  # Data review =========================
  rv$row <- NULL
  rv$row_data <- NULL

  ##############################################################################
  ##############################################################################
  # Keypad

  output$keypad <- renderText(rv$keypad)
  shiny::observeEvent(input$b1, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '1', paste0(rv$keypad, '1')) }) })
  shiny::observeEvent(input$b2, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '2', paste0(rv$keypad, '2')) }) })
  shiny::observeEvent(input$b3, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '3', paste0(rv$keypad, '3')) }) })
  shiny::observeEvent(input$b4, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '4', paste0(rv$keypad, '4')) }) })
  shiny::observeEvent(input$b5, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '5', paste0(rv$keypad, '5')) }) })
  shiny::observeEvent(input$b6, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '6', paste0(rv$keypad, '6')) }) })
  shiny::observeEvent(input$b7, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '7', paste0(rv$keypad, '7')) }) })
  shiny::observeEvent(input$b8, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '8', paste0(rv$keypad, '8')) }) })
  shiny::observeEvent(input$b9, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '9', paste0(rv$keypad, '9')) }) })
  shiny::observeEvent(input$b0, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '0', paste0(rv$keypad, '0')) }) })
  shiny::observeEvent(input$bdec, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '0.', paste0(rv$keypad, '.')) }) })
  shiny::observeEvent(input$bclr, { shiny::isolate({ rv$keypad <- '0' }) })

  ##############################################################################
  ##############################################################################
  # GPS

  output$lat <- renderPrint({ input$lat })
  output$long <- renderPrint({ input$long })
  output$geolocation <- renderPrint({ input$geolocation })
  output$accuracy <- renderPrint({ input$accuracy })
  output$time <- renderPrint({ input$time })
  output$speed <- renderPrint({ input$speed })

  # Log GPS position
  observeEvent(gps_timer(), {
    isolate({
      gps_test1 <- c(exists('input$lat'), exists('input$long'), exists('input$time'))
      #print(gps_test1)
      if(all(gps_test1)){
        gps_test2 <- c(!is.null(input$lat), !is.null(input$long), !is.null(input$time))
        #print(gps_test2)
        if(all(gps_test2)){
          gps_test3 <- c(!is.na(as.numeric((input$lat))), !is.na(as.numeric((input$long))), !is.na(as.numeric((input$time))))
          # print(gps_test3)
          if(all(gps_test3)){
            gps_data <- c(input$lat, input$long, input$time)
            rv$df <- log_line('*', gps_data)
          }
        }
      }
    })
  })

  ##############################################################################
  ##############################################################################
  # Effort

  # Button to begin/end scan
  output$scan <- renderUI({
    if(rv$scan == 1){
      actionButton("scan_off",label="End systematic scan", style=textstyle(button_size, button_padding), width="100%")
    }else{
      actionButton("scan_on",label="Start systematic scan",style=textstyle(button_size, button_padding), width="100%")
    }
  })

  # When scan begins
  observeEvent(input$scan_on,{ isolate({
    rv$scan <- 1
    rv$scan_start <- Sys.time()
    scan_data <- c(rv$scan, input$platform, input$obs_data, input$obs_left, input$obs_right, input$obs_io)
    rv$df <- log_line('EFF', scan_data)
    beepr::beep(10)
  }) })

  # When scan ends
  observeEvent(input$scan_off,{
    rv$scan <- 0
    scan_timer <- NULL
    scan_data <- c(rv$scan, input$platform, input$obs_data, input$obs_left, input$obs_right, input$obs_io)
    rv$df <- log_line('EFF', scan_data)
    beepr::beep(3)
  })

  # Begin scan timer
  observe({
    if(rv$scan == 1){
      scan_timer()
      secs <- difftime(Sys.time(), rv$scan_start, units='secs')
      rv$scan_duration <- secs
    }
  })

  # Print scan duration
  output$scan_duration <- renderText({
    if(rv$scan){
      secs <- rv$scan_duration
      mins <- secs / 60
      mins_show <- floor(mins)
      secs_show <- round( (mins - mins_show) * 60)
      paste0('Scan duration: ', mins_show,' minutes ',secs_show, ' seconds')
    }else{
      ''
    }
  })

  # Announce end of scan
  observe({
    if(!is.null(rv$scan_duration)){
      if(!is.null(scan_target)){
        if(round(as.numeric(rv$scan_duration)) == round((scan_target*60))){
          beepr::beep(3)
        }
      }
    }
  })

  ##############################################################################
  ##############################################################################
  # Conditions

  output$cz_left <- renderUI({
    button_label <- ifelse(is.null(rv$cz_left), grabit, rv$cz_left)
    actionButton("cz_left", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$cz_left,{ isolate({ rv$cz_left <- rv$keypad  ; rv$keypad <- '0' }) })

  output$cz_right <- renderUI({
    button_label <- ifelse(is.null(rv$cz_right), grabit, rv$cz_right)
    actionButton("cz_right", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$cz_right,{ isolate({ rv$cz_right <- rv$keypad  ; rv$keypad <- '0' }) })

  output$cz_close <- renderUI({
    button_label <- ifelse(is.null(rv$cz_close), grabit, rv$cz_close)
    actionButton("cz_close", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$cz_close,{ isolate({ rv$cz_close <- rv$keypad  ; rv$keypad <- '0' }) })

  output$cz_far <- renderUI({
    button_label <- ifelse(is.null(rv$cz_far), grabit, rv$cz_far)
    actionButton("cz_far", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$cz_far,{ isolate({ rv$cz_far <- rv$keypad  ; rv$keypad <- '0' }) })

  output$glare_left <- renderUI({
    button_label <- ifelse(is.null(rv$glare_left), grabit, rv$glare_left)
    actionButton("glare_left", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$glare_left,{ isolate({ rv$glare_left <- rv$keypad  ; rv$keypad <- '0' }) })

  output$glare_right <- renderUI({
    button_label <- ifelse(is.null(rv$glare_right), grabit, rv$glare_right)
    actionButton("glare_right", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$glare_right,{ isolate({ rv$glare_right <- rv$keypad  ; rv$keypad <- '0' }) })

  # Store condition zone ====================================
  observeEvent(input$cz_store,{
    throw_alert <- TRUE
    cz_check1 <- c(as.numeric(rv$cz_left) < as.numeric(rv$cz_right),
                   as.numeric(rv$cz_left) <= 360,
                   as.numeric(rv$cz_right) <= 360,
                   as.numeric(rv$cz_close) < as.numeric(rv$cz_far),
                   as.numeric(rv$glare_left) < as.numeric(rv$glare_right),
                   as.numeric(rv$glare_left) <= 360,
                   as.numeric(rv$glare_right) <= 360)
    print(cz_check1)
    if(all(cz_check1)){
      throw_alert <- FALSE

      # Gather data
      cz_data <- c(rv$cz_left, rv$cz_right, rv$cz_close, rv$cz_far,
                   input$bft, input$wave, input$viz, input$cloud,
                   input$precip, input$fog, input$haze,input$smear,
                   input$glare, rv$glare_left, rv$glare_right)

      # Log it
      rv$df <- log_line('SEA', cz_data)
      beepr::beep()

      # Manage aftermath (update buttons, fields, etc.)
      rv$cz_left <- 1 ; rv$cz_right <- 359 ; rv$cz_close <- 0 ; rv$cz_far <- 30
    }

    if(throw_alert){
      showModal(modalDialog(
        'You must provide for valid boundaries for the condition zone (and its glare, if any)',
        title = 'Invalid condition zone boundaries!',
        footer = modalButton("Dismiss"),
        size = c("m"),
        easyClose = TRUE,
        fade = TRUE
      ))
    }

  })

  ##############################################################################
  ##############################################################################
  # Sighting

  # Buttons ================================

  # Bearing
  output$bearing <- renderUI({
    button_label <- ifelse(is.null(rv$bearing), grabit, rv$bearing)
    actionButton("bearing", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$bearing,{ isolate({ rv$bearing <- rv$keypad  ; rv$keypad <- '0' }) })

  # Reticle
  output$reticle <- renderUI({
    button_label <- ifelse(is.null(rv$reticle), grabit, rv$reticle)
    actionButton("reticle", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$reticle,{ isolate({ rv$reticle <- rv$keypad ; rv$keypad <- '0' }) })

  # Group max
  output$grp_max <- renderUI({
    button_label <- ifelse(is.null(rv$grp_max), grabit, rv$grp_max)
    actionButton("grp_max", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$grp_max,{ isolate({ rv$grp_max <- rv$keypad  ; rv$keypad <- '0' ; rv$grp_min <- rv$grp_best <- rv$grp_max }) })

  # Group min
  output$grp_min <- renderUI({
    button_label <- ifelse(is.null(rv$grp_min), grabit, rv$grp_min)
    actionButton("grp_min", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$grp_min,{ isolate({ rv$grp_min <- min(c(rv$grp_max, rv$keypad))  ; rv$keypad <- '0' ; rv$grp_best <- round(mean(as.numeric(c(rv$grp_min, rv$grp_max)))) }) })

  # Group best
  output$grp_best <- renderUI({
    button_label <- ifelse(is.null(rv$grp_best), grabit, rv$grp_best)
    actionButton("grp_best", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$grp_best,{ isolate({ rv$grp_best <- max(as.numeric(c(rv$grp_min, min(as.numeric(c(rv$keypad, rv$grp_max))))))  ; rv$keypad <- '0' }) })

  # Estimated distance
  output$distance <- renderUI({
    button_label <- ifelse(is.null(rv$distance), grabit, rv$distance)
    actionButton("distance", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$distance,{ isolate({ rv$distance <- rv$keypad  ; rv$keypad <- '0' }) })

  # Menus ==================================

  output$species <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('species', NULL, c('N/A', species[[list_id]]), width='95%', height='160px')
  })

  output$bhvr_primary <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('bhvr_primary', NULL, c('N/A', behaviours[[list_id]]), width='95%', height='160px')
  })

  output$bhvr_secondary <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('bhvr_secondary', NULL, c('N/A', behaviours[[list_id]]), width='95%', height='160px')
  })

  output$bhvr_tertiary <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('bhvr_tertiary', NULL, c('N/A', behaviours[[list_id]]), width='95%', height='160px')
  })

  # Store sightings ==============================
  observeEvent(input$sit_store,{
    throw_alert <- TRUE
    sit_check1 <- c(!is.null(rv$bearing),
                    !is.null(rv$reticle),
                    !is.null(rv$distance))
    print(input$bearing)
    print(sit_check1)
    if(all(sit_check1)){
      sit_check2 <- c(as.numeric(rv$bearing) <= 360,
                      as.numeric(rv$grp_max) > 0,
                      as.numeric(rv$grp_min) > 0,
                      as.numeric(rv$grp_best) > 0,
                      input$inline != 'N/A',
                      input$reticle_how != 'N/A',
                      input$cue != 'N/A',
                      input$species != 'N/A')
      print(sit_check2)
      if(all(sit_check2)){
        throw_alert <- FALSE

        # Gather data
        sit_data <- c(rv$next_sit, input$bearing,
                      input$reticle, input$reticle_how, input$distance,
                      input$inline, input$cue,
                      input$grp_max, input$grp_min, input$grp_best,
                      input$sit_type, input$species,
                      input$bhvr_primary, input$bhvr_secondary, input$bhvr_tertiary, input$direction,
                      input$threat, input$calves, input$males, input$acoustics)

        # Log it
        rv$df <- log_line('SIT', sit_data)
        beepr::beep()

        # Manage aftermath (update buttons, fields, etc.)
        rv$next_sit <- rv$next_sit + 1
        rv$bearing <- NULL ; rv$reticle <- NULL ; rv$distance <- NULL
        rv$grp_max <- 1 ; rv$grp_min <- 1 ; rv$grp_best <- 1
        shinyjs::reset('inline') ; shinyjs::reset('cue') ; shinyjs::reset('reticle_how')
        shinyjs::reset('sit_type') ; shinyjs::reset('species')
        shinyjs::reset('bhvr_primary') ; shinyjs::reset('bhvr_secondary') ; shinyjs::reset('bhvr_tertiary') ; shinyjs::reset('direction')
        shinyjs::reset('threat') ; shinyjs::reset('calves') ; shinyjs::reset('males') ; shinyjs::reset('acoustics')
      }
    }

    if(throw_alert){
      showModal(modalDialog(
        'You must provide for valid entries for the required fields (*) at the top',
        title = 'Missing / invalid data!',
        footer = modalButton("Dismiss"),
        size = c("m"),
        easyClose = TRUE,
        fade = TRUE
      ))
    }

  })

  ##############################################################################
  ##############################################################################
  # Update sighting

  # Show species table
  output$up_spp_table = DT::renderDT( rv$df %>% dplyr::filter(V2=='SIT'),
                                      extensions = 'Scroller',
                                      options=list(searching = TRUE,
                                                   autoWidth = TRUE,
                                                   columnDefs = list(list(width = '50px', targets = "_all")),
                                                   rownames = FALSE,
                                                   scroller = TRUE,
                                                   scrollX = "400px",
                                                   scrollY = "100px",
                                                   fixedHeader = TRUE,
                                                   class = 'cell-border stripe',
                                                   fixedColumns = list(
                                                     leftColumns = 3,
                                                     heightMatch = 'none'
                                                   )),
                                      editable=FALSE)

  # Update Bearing
  output$up_bearing <- renderUI({
    button_label <- ifelse(is.null(rv$up_bearing), grabit, rv$up_bearing)
    actionButton("up_bearing", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$up_bearing,{ isolate({ rv$up_bearing <- rv$keypad  ; rv$keypad <- '0' }) })

  # Update Reticle
  output$up_reticle <- renderUI({
    button_label <- ifelse(is.null(rv$up_reticle), grabit, rv$up_reticle)
    actionButton("up_reticle", label=button_label, style=textstyle(button_size*.7, button_padding*.5), width="95%")
  })
  observeEvent(input$up_reticle,{ isolate({ rv$up_reticle <- rv$keypad ; rv$keypad <- '0' }) })

  # Update Group max
  output$up_max <- renderUI({
    button_label <- ifelse(is.null(rv$up_max), grabit, rv$up_max)
    actionButton("up_max", label=button_label, style=textstyle(button_size*.7, button_padding*.7), width="95%")
  })
  observeEvent(input$up_max,{ isolate({ rv$up_max <- rv$keypad  ; rv$keypad <- '0' ; rv$up_min <- rv$up_best <- rv$up_max }) })

  # Update Group min
  output$up_min <- renderUI({
    button_label <- ifelse(is.null(rv$up_min), grabit, rv$up_min)
    actionButton("up_min", label=button_label, style=textstyle(button_size*.7, button_padding*.7), width="95%")
  })
  observeEvent(input$up_min,{ isolate({ rv$up_min <- min(c(rv$up_max, rv$keypad))  ; rv$keypad <- '0' ; rv$up_best <- round(mean(as.numeric(c(rv$up_min, rv$up_max)))) }) })

  # Update Group best
  output$up_best <- renderUI({
    button_label <- ifelse(is.null(rv$up_best), grabit, rv$up_best)
    actionButton("up_best", label=button_label, style=textstyle(button_size*.7, button_padding*.7), width="95%")
  })
  observeEvent(input$up_best,{ isolate({ rv$up_best <- max(as.numeric(c(rv$up_min, min(as.numeric(c(rv$keypad, rv$up_max))))))  ; rv$keypad <- '0' }) })

  # Update Estimated distance
  output$up_distance <- renderUI({
    button_label <- ifelse(is.null(rv$up_distance), grabit, rv$up_distance)
    actionButton("up_distance", label=button_label, style=textstyle(button_size*.7, button_padding*.7), width="95%")
  })
  observeEvent(input$up_distance,{ isolate({ rv$up_distance <- rv$keypad  ; rv$keypad <- '0' }) })

  # Update Behaviors
  output$up_primary <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('up_primary', NULL, c('N/A', behaviours[[list_id]]), width='95%', height='160px')
  })

  output$up_secondary <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('up_secondary', NULL, c('N/A', behaviours[[list_id]]), width='95%', height='160px')
  })

  output$up_tertiary <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('up_tertiary', NULL, c('N/A', behaviours[[list_id]]), width='95%', height='160px')
  })

  output$up_spp2 <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('up_spp2', NULL, c('N/A', species[[list_id]]), width='95%', height='160px')
  })

  output$up_spp3 <- renderUI({
    list_id <- which(names(species) == input$species_type)
    radio('up_spp3', NULL, c('N/A', species[[list_id]]), width='95%', height='160px')
  })


  # Store update ==============================
  observeEvent(input$update,{
    tab_row <- input$up_spp_table_rows_selected
    print(tab_row)
    sits <- rv$df %>% dplyr::filter(V2 == 'SIT')

    throw_alert <- TRUE
    sit_check1 <- c(!is.null(tab_row))
    if(all(sit_check1)){
      throw_alert <- FALSE

      bearing <- ifelse(is.null(rv$up_bearing), NA, rv$up_bearing)
      reticle <- ifelse(is.null(rv$up_reticle), NA, rv$up_reticle)
      distance <- ifelse(is.null(rv$up_distance), NA, rv$up_distance)

      # Gather data
      upd_data <- c(as.character(sits$V3[tab_row]), # sitno
                    bearing, reticle,
                    input$up_reticle_how,
                    distance,
                    input$up_obs, input$up_changed,
                    input$up_max, input$up_min, input$up_best,
                    input$up_mixed, input$up_spp2, input$up_per2, input$up_spp3, input$up_per3,
                    input$up_primary, input$up_secondary, input$up_tertiary, input$up_direction,
                    input$up_threat, input$up_acoustics)

      # Log it
      rv$df <- log_line('UPD', upd_data)
      beepr::beep(2)

      # Manage aftermath (update buttons, fields, etc.)
      rv$up_bearing <- NULL ; rv$up_reticle <- NULL ; rv$up_distance <- NULL
      rv$up_max <- 0 ; rv$up_min <- 0 ; rv$up_best <- 0
      shinyjs::reset('up_reticle_how')
      shinyjs::reset('up_primary') ; shinyjs::reset('up_secondary') ; shinyjs::reset('up_tertiary') ; shinyjs::reset('up_direction')
      shinyjs::reset('up_mixed') ;
      shinyjs::reset('up_threat') ; shinyjs::reset('up_acoustics')
    }


    if(throw_alert){
      showModal(modalDialog(
        'Gotta select a sighting to update before you can update it!',
        title = 'Sighting not specified!',
        footer = modalButton("Dismiss"),
        size = c("m"),
        easyClose = TRUE,
        fade = TRUE
      ))
    }
  })

  ##############################################################################
  ##############################################################################
  # Comment

  # Show species table
  output$com_spp_table = DT::renderDT( rv$df %>% dplyr::filter(V2=='SIT'),
                                       extensions = 'Scroller',
                                       options=list(searching = TRUE,
                                                    autoWidth = TRUE,
                                                    columnDefs = list(list(width = '50px', targets = "_all")),
                                                    rownames = FALSE,
                                                    scroller = TRUE,
                                                    scrollX = "400px",
                                                    scrollY = "100px",
                                                    fixedHeader = TRUE,
                                                    class = 'cell-border stripe',
                                                    fixedColumns = list(
                                                      leftColumns = 3,
                                                      heightMatch = 'none'
                                                    )),
                                       editable=FALSE)

  # Store update - manual comment
  observeEvent(input$com_store,{
    sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
    sit_row <- input$com_spp_table_rows_selected
    sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
    com_txt <- input$com_manual ; com_txt <- gsub(',',';',com_txt)
    com_data <- c(sitno, com_txt)
    rv$df <- log_line('COM', com_data)
    beepr::beep(2) ; shinyjs::reset('com_spp_table') ; shinyjs::reset('com_manual')
  })

  observeEvent(input$com_1,{
    com_txt <- comment_1
    sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
    sit_row <- input$com_spp_table_rows_selected
    sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
    com_data <- c(sitno, com_txt)
    rv$df <- log_line('COM', com_data)
    beepr::beep(2) ; shinyjs::reset('com_spp_table')
  })

  observeEvent(input$com_2,{
    com_txt <- comment_2
    sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
    sit_row <- input$com_spp_table_rows_selected
    sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
    com_data <- c(sitno, com_txt)
    rv$df <- log_line('COM', com_data)
    beepr::beep(2) ; shinyjs::reset('com_spp_table')
  })

  observeEvent(input$com_3,{
    com_txt <-  comment_3
    sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
    sit_row <- input$com_spp_table_rows_selected
    sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
    com_data <- c(sitno, com_txt)
    rv$df <- log_line('COM', com_data)
    beepr::beep(2) ; shinyjs::reset('com_spp_table')
  })

  observeEvent(input$com_4,{
    com_txt <- comment_4
    sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
    sit_row <- input$com_spp_table_rows_selected
    sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
    com_data <- c(sitno, com_txt)
    rv$df <- log_line('COM', com_data)
    beepr::beep(2) ; shinyjs::reset('com_spp_table')
  })

  observeEvent(input$com_5,{
    com_txt <- comment_5
    sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
    sit_row <- input$com_spp_table_rows_selected
    sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
    com_data <- c(sitno, com_txt)
    rv$df <- log_line('COM', com_data)
    beepr::beep(2) ; shinyjs::reset('com_spp_table')
  })

  observeEvent(input$com_6,{
    com_txt <- comment_6
    sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
    sit_row <- input$com_spp_table_rows_selected
    sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
    com_data <- c(sitno, com_txt)
    rv$df <- log_line('COM', com_data)
    beepr::beep(2) ; shinyjs::reset('com_spp_table')
  })

  ##############################################################################
  ##############################################################################
  # Review - Data

  # Show data table
  output$data = DT::renderDT( rv$df,
                              extensions = 'Scroller',
                              options=list(searching = TRUE,
                                           autoWidth = TRUE,
                                           columnDefs = list(list(width = '50px', targets = "_all")),
                                           rownames = FALSE,
                                           scroller = TRUE,
                                           scrollX = "400px",
                                           scrollY = "300px",
                                           fixedHeader = TRUE,
                                           class = 'cell-border stripe',
                                           fixedColumns = list(
                                             leftColumns = 3,
                                             heightMatch = 'none'
                                           )),
                              editable=TRUE)

  # Select row
  observe({ rv$row <- input$data_rows_selected ; print(rv$row) })

  # Remove row  =======================================

  output$remove_row <- renderUI({
    if(length(rv$row) > 0){ actionButton("remove_row",h4("Remove row(s)"),width="95%") }
  })
  observeEvent(input$remove_row,{
    isolate({
      rv$df_backup <- rv$df
      keeps <- which(! 1:nrow(rv$df) %in% rv$row)
      new_df <- rv$df[keeps,]
      new_filename <- get_filename()
      readr::write_csv(x=new_df, file=new_filename, quote='none', col_names=FALSE, append=FALSE)
      rv$df <- new_df
    })
  })

  # Copy / paste =======================================

  output$copypaste <- renderUI({
    if(length(rv$row)>0){
      if(is.null(rv$row_data)){
        actionButton("copy",h4("Copy row"),width="95%")
      }else{
        actionButton("paste",h4("Paste below current row"),width="95%")
      }
    }
  })

  observeEvent(input$copy,{ isolate({ rv$row_data <- rv$df[rv$row,] }) })

  observeEvent(input$paste,{ isolate({
    rv$df_backup <- rv$df
    new_row <- (rv$row[length(rv$row)]+1)
    if(new_row >= nrow(rv$df)){ new_row <- NULL}
    new_df <- DataCombine::InsertRow(rv$df, NewRow = rv$row_data, RowNum = new_row)
    new_filename <- get_filename()
    readr::write_csv(x=new_df, file=new_filename, quote='none', col_names=FALSE, append=FALSE)
    rv$df <- new_df
    rv$row_data <- NULL
  }) })

  # Edit a row ========================

  proxy5 = dataTableProxy('data')
  observeEvent(input$data_cell_edit, {
    info = input$data_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    dti <- rv$df
    rv$df_backup <- rv$df
    new_df <- rv$df
    new_df[i, j] <- isolate(DT::coerceValue(v, new_df[i, j]))
    readr::write_csv(x=new_df, file=new_filename, quote='none', col_names=FALSE, append=FALSE)
    rv$df <- new_df
  })

  # Undo edit  ========================================

  output$undo <- renderUI({
    if(!is.null(rv$df_backup)){
      if(!identical(rv$df, rv$df_backup)){ actionButton("undo",h4("Undo last edit"),width="95%") }
    }
  })
  observeEvent(input$undo,{
    isolate({
      new_df <- rv$df_backup
      readr::write_csv(x=new_df, file=new_filename, quote='none', col_names=FALSE, append=FALSE)
      rv$df <- new_df
    })
  })

  ##############################################################################
  ##############################################################################
  # Review - Map

}

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

shinyApp(ui,
         server,
         onStart)


}
