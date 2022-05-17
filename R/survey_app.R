#' Launch data entry app
#'
#' This function launches a Shiny app designed to optimize data entry during wildlife surveys.
#' The app was designed with marine mammal surveys in mind, but should be generalizable for other contexts
#' and target species. The app should be useful for both stationary and transect-based surveys.
#' Layout has been optimized for touch screens.
#'
#' @param observers A character vector of observer names, initials, or codes.
#' @param platforms A character vector of platform names or sampling locations.
#' @param optics A character vector of optics used to detect wildlife (e.g., `"Binocs"` or `"Naked eye"`).
#' **Option handling:** If this isn't relevant to your study, provide a single value, e.g., 'eyes'.
#' If you want to make sure that this value is always specified with each sighting,
#' make the first value `"N/A"`. When you do this, you will not be allowed to log a
#' new sighting until a value *other than* `"N/A"` is selected.
#' @param landmarks A character vector of landmarks that a detection can be in line with.
#' This is useful in stationary surveys, to keep track of multiple sightings at once,
#' and to sanity-check bearing angles during analysis. See *option handling* above.
#' @param cues A character vector of detection cues that triggered the sighting.
#' See *option handling* above.
#' @param species A named list of character vectors, each specifying species options for
#' a single type or category of species. For example, in a marine mammal survey, you may have
#' two main types of detection: marine mammals or vessels. You would then have a `species` input like so:
#' `list('MarMam' = c('species1', 'species2', 'etc'), 'Vessel' = c('type1', type2', 'etc')`.
#' @param behaviours A named list of character vectors, each specifying the behavior options
#' for each type/category in your `species` list. This list needs to be the same
#' length as the `species` list.
#' @param scan_target Beginning of default inputs: target duration of systematic scans,
#' with a default of 15 minutes.
#' When a scan is started, a timer begins. When the `scan_target` duration is exceeded,
#' a sound will indicate that the observer may end the scan. If you do not want to use
#' this feature, set to `Inf`.
#' @param gps_interval Interval between GPS position updates, in seconds (default = 10 sec).
#' This feature is only available for devices with GPS enabled (e.g., tablets and the newest laptops).
#' If the device you are using does not support GPS, position updates will be turned off.
#' This feature is in *beta* -- not yet guaranteed to work. The code used to develop this was adapted from
#' [this Github account.](https://github.com/AugustT/shiny_geolocation/blob/master/accuracy_and_dynamic/server.r)
#' @param data_width The number of data fields expected in your survey data.
#' The default is 20, meaning total `.csv` width will be 23 columns
#' (timestamp and event code account for the other two columns), which will work
#' for the packaged version of this app. We included this as an input in the event
#' that you wish to download and customize the function code; this input makes it easier
#' to adjust the width of your survey data files.
#' @param scroll_height This and the next several inputs relate to optimizing display
#' on your specific device. This input defines how 'tall' a scroll box should be,
#' in pixels. Smaller values will work for smaller screens or screens with lower resolution.
#' @param button_size Percentage by which to scale font size of buttons in the app.
#' Larger values create more visible button text.
#' @param button_padding The margin for buttons, in pixels. Larger values make the buttons larger.
#' @param keypad_size Like `button_size` above, the percentage by which to scale the numbers on the app's
#' number keypad.
#' @param keypad_padding Like `button_padding` above, this lets you adjust
#' the size of the number keypad buttons.
#' @param tab_size The percentage by which to scale the font size of the tab titles.
#' @param tab_width The width of tabs, in pixels. If the size or resolution of screen causes the tabset
#' to wrap onto two lines, you can prevent that by adjusting this value down.
#' @param comment_1 This and the next several inputs allow you to define 'canned' comments
#' that can be efficiently logged on the "Comments" tab using a single touch of a button.
#' This feature can be useful if you regularly log the same type of comment throughout the day
#' during surveys. These canned comments will appear as buttons in the order you specify here.
#' Note that the comment text cannot accepts commas; any commas you enter will be removed
#' before storing the comment in the data file.
#' @param comment_2 Second canned comment.
#' @param comment_3 Third canned comment.
#' @param comment_4 Fourth canned comment.
#' @param comment_5 Fifth canned comment.
#' @param comment_6 Sixth canned comment.
#'
#' @details Typical workflow:
#'
#' \itemize{
#' \item Open an `R` script in your working directory and setup your `survey_app()` function call.
#' \item Run it to launch the app.
#' \item On the "Effort" tab, update observer positions and "Start scan".
#' \item On the "Conditions" tab, update sighting conditions. You will not be able to store
#' conditions until the boundaries of the conditions have been specified. This app
#' uses a concept of 'condition zones' to allow you to describe varying conditions in different
#' regions of your viewshed.
#' A condition zone is a portion of searchable area that has
#' left and right boundary edges (described using compass bearing) and
#' close and far boundary edges (described using an estimated distance).
#' You can store as many condition zones as you need in order to describe condition zones.
#' Subsequent processing functions will sort multiple condition zones from widest to narrowest,
#' which means that you can specify a broad condition zone (1 - 359 degrees, 0 km to 30 km away)
#' then specify narrow condition zones to describe small exceptions to the most prevalent conditions.
#' \item On the "Sightings" tab, enter detections while you search. You will not be able to store
#' a sighting unless all required fields at the top of page have been modified.
#' \item Use the "Update" tab to update or elaborate upon a sighitng you have already logged.
#' On this tab, you can update your position estimate, add additional group size estimates from other observers,
#' specify other species in the sighting if it is a mixed-species group, and update behaviors.
#' Note that this tab should *not* be used to correct or fix erroneous data entries.
#' You can do that directly under the "Review" tab.
#' Also note that you will not be able to log an update until you select the sighting you are updating
#' from the table provided in the tab.
#' \item On the "Review" tab, check on the data you are entering.
#' Scroll down to see the most recent entries.
#' When you click on a row (or multiple rows), you will have the option to delete it
#' or to copy it. You can then click on another row and paste that row below it.
#' You can then click on individual fields and edit them directly.
#' Each time you make an edit, the data file will be updated. (So be careful!).
#' \item When you are ready to end the scan, go back to the "Effort" tab and end the scan.
#' \item When you are ready to close the app, just 'X' out of the app window.
#' }
#'
#' @return This function will launch a `Shiny` app.
#'
#' The first time you run the app,
#' a folder named `"data"` will be created within your working directory.
#' The app stores data in `.csv` files within that folder, one `.csv` for each
#' date (format: `yyyy-mm-dd.csv`). The first time the app is launched on a given date,
#' that data file will be created. In subsequent launches on the same date, the pre-existing file
#' will be appended with new data.
#'
#' Rows of data are added to this file as you work within the app.
#' You can close the app at any time without affecting your data file.
#' To close the app, simply 'X' out of the app window. You may also need to
#' press the "Stop" sign in the `R`/`RStudio` GUI.
#'
#' The app logs 6 types of data, described in detail below:
#' \itemize{
#'   \item **Effort updates**, with the following data fields:
#'   \enumerate{
#'   \item System timestamp (`yyyy-mm-dd hh:mm:ss`)
#'   \item Event code (`EFF`)
#'   \item Effort status (1 = On systematic effort; 2 = Off systematic effort)
#'   \item Observer on data entry duty / primary observer
#'   \item Left observer, if any
#'   \item Right observer, if any
#'   \item Independent observer, if any
#'   \item Research platform / sampling location
#'   }
#'   \item **Condition updates**:
#'   \enumerate{
#'   \item System timestamp (`yyyy-mm-dd hh:mm:ss`)
#'   \item Event code (`SEA`)
#'   \item Left edge of condition zone (degrees)
#'   \item Right edge of condition zone (degrees)
#'   \item Close/near edge of condition zone (estimated km)
#'   \item Far/distance edge of condition zone (estimated km)
#'   \item Beaufort sea state
#'   \item Visibility (km)
#'   \item Precipitation status
#'   \item Fog status
#'   \item Haze status
#'   \item Horizon smear status (distortion caused by radiant heat)
#'   \item Glare status
#'   \item Left edge of glare, if any (degrees)
#'   \item Right edge of glare, if any (degrees)
#'   }
#'   \item **New sightings**:
#'   \enumerate{
#'   \item System timestamp (`yyyy-mm-dd hh:mm:ss`)
#'   \item Event code (`SIT`)
#'   \item Unique sighting number (based on all sightings within data files within the `"data"` folder).
#'   \item Bearing
#'   \item Reticle reading
#'   \item Optical instrument used to measure reticle
#'   \item Estimated distane (km)
#'   \item Landmark in line with the sighting
#'   \item Detection cue
#'   \item Maximum estimate of group size
#'   \item Minimum estimate of group size
#'   \item Best estimate of group size
#'   \item Species type/category
#'   \item Species name
#'   \item Primary behaviour
#'   \item Secondary behaviour
#'   \item Tertiary behaviour
#'   \item Direction of movement
#'   \item Threat interaction? (TRUE or FALSE)
#'   \item Number of calves in group
#'   \item Number of males in group
#'   \item Acoustic status
#'   \item System timestamp of photograph taken of detection at given bearing
#'   (useful if, for example, you want to measure pixels in an image to triangulate the position
#'   of a detection within a photograph)
#'   }
#'   \item **Sighting updates**:
#'   \enumerate{
#'   \item System timestamp (`yyyy-mm-dd hh:mm:ss`)
#'   \item Event code (`UPD`)
#'   \item Sighting number being updated
#'   \item New bearing
#'   \item New reticle
#'   \item How reticle was measured (optics)
#'   \item New estimated distance
#'   \item Observer contributing new estimate
#'   \item Whether group size has changed or is just being estimated again
#'   \item Maximum group size estimate
#'   \item Minimum group size estimate
#'   \item Best group size estimate
#'   \item Whether or not this is a mixed-species group
#'   \item Species name of second species in group
#'   \item Percent of group represented by this second species
#'   \item Species name of third species in group
#'   \item Percent of group represented by this second species (ensure that this plus the percentage for species 2 does not exceed 99)
#'   \item Updated primary behaviour
#'   \item Updated secondary behaviour
#'   \item Updated tertiary behaviour
#'   \item Updated threat interaction
#'   \item Updated acoustic status
#'   \item System timestamp of photograph taken of detection at given bearing
#'   (useful if, for example, you want to measure pixels in an image to triangulate the position
#'   of a detection within a photograph)

#'   }
#'   \item **Comments**:
#'   \enumerate{
#'   \item System timestamp (`yyyy-mm-dd hh:mm:ss`)
#'   \item Event code (`COM`)
#'   \item Sighting number associated with this sighting, if any
#'   \item The comment
#'   }
#'   \item **Position updates**:
#'   \enumerate{
#'   \item System timestamp (`yyyy-mm-dd hh:mm:ss`)
#'   \item Event code (`*`)
#'   \item Latitude, decimal degrees
#'   \item Longitude, decimal degrees
#'   \item GPS time
#'   }
#'   }
#'
#' @import magrittr
#' @import dplyr
#' @import shiny
#' @import DT
#' @export
#'
#' @examples
#' # Customize settings =======================================
#'
#' observers <- c('Grace','Janie','Chenoah', 'Ben')
#'
#' platforms <- c('Inside','Outside')
#'
#' optics <- c('N/A','Big Eyes', 'Binocs', 'Scope', 'Naked Eye')
#'
#' landmarks <- c('N/A','Gil Mtn', 'CMP peaks', 'Otter',
#'                'Twartz', 'Farrant', 'N Fin shore')
#'
#' cues <- c('N/A','Blow','Vessel','Body','Fluke','Splash','Sound')
#'
#' species <- list('MarMam' = c('Humpback', 'Fin', 'Dalls porpoise',
#'                              'Harbour seal', 'Stellars sea lion',
#'                              'Elephant seal', 'Killer whale'),
#'                 'Vessel' = c('Large rec', 'Small rec', 'CFV', 'Sailing',
#'                              'Tug only', 'Tug+barge', 'Gitgaat',
#'                              'Research', 'Cruise', 'Tanker'))
#'
#' behaviours <- list('MarMam' = c('Active', 'Sleep', 'Feeding',
#'                                 'BNF', 'Milling', 'Robust', 'Fast travel',
#'                                 'RE-TR'),
#'                     'Vessel' = c('Fast travel', 'Slow travel', 'Fishing',
#'                                  'With whales', 'Anchored'))
#'
#' # Launch app =======================================
#'
#' if(FALSE){ #not run
#' survey_app(observers,
#'            platforms,
#'            optics,
#'            landmarks,
#'            cues,
#'            species,
#'            behaviours)
#' }
#'
survey_app <- function(observers,
                       platforms,
                       optics,
                       landmarks,
                       cues,
                       species,
                       behaviours,
                       scan_target = 15,
                       gps_interval = 10,
                       data_width = 21,
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
                       comment_6 = 'App crashed -- trying again.',
                       beeps = FALSE
){


  ##############################################################################
  # For debugging only -- not Run!
  if(FALSE){

    observers <- c('Grace','Janie', 'Chenoah', 'Ben')
    platforms <- c('Inside','Outside')
    optics <- c('N/A','Big Eyes', 'Binocs', 'Scope', 'Naked Eye')
    landmarks <- c('N/A','Gil Mtn', 'CMP peaks', 'Otter', 'Twartz', 'Farrant', 'N Fin shore')
    cues <- c('N/A','Blow','Vessel','Body','Fluke','Splash','Sound')

    species <- list('MarMam' = c('Humpback', 'Fin', 'Dalls porpoise', 'Harbour seal', 'Stellars sea lion', 'Elephant seal', 'Killer whale'),
                    'Vessel' = c('Large rec', 'Small rec', 'CFV', 'Sailing', 'Tug only', 'Tug+barge', 'Gitgaat', 'Research', 'Cruise', 'Tanker'))

    behaviours <- list('MarMam' = c('Active', 'Sleep', 'Feeding', 'BNF', 'Milling', 'Robust', 'Fast travel', 'RE-TR'),
                       'Vessel' = c('Fast travel', 'Slow travel', 'Fishing', 'With whales', 'Anchored'))

    # Try it
    suRvey::survey_app(observers,
               platforms,
               optics,
               landmarks,
               cues,
               species,
               behaviours)
    #

    # function defaults
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
    comment_1 <- 'Photo-ID acquired.'
    comment_2 <- 'Memorable sighting!'
    comment_3 <- 'Conditions changed dramatically during scan.'
    comment_4 <- 'Scan cut short early.'
    comment_5 <- 'Severe revision needed -- fix manually later!'
    comment_6 <- 'App crashed -- trying again.'

    # GPS sensor info:
    # https://github.com/AugustT/shiny_geolocation/blob/master/accuracy_and_dynamic/server.r

    library(remotes)
    remotes::install_github("ericmkeen/suRvey")
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
                    direction = 'vertical', width='100%', height='180px'){
    shinydashboard::box(width=12, style=paste0('height:',height,
                                               ';overflow-y: scroll; overflow-x: scroll;',
                                               ' -ms-overflow-style: none;'),
                        shinyWidgets::radioGroupButtons(
                          inputId = id,
                          label = h4(label_text),
                          choices = c(choices, default_choices),
                          size='normal', justified = TRUE, direction = direction,
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
  if(nrow(dfall)>0){
    (sits <- dfall %>% dplyr::filter(V2 == 'SIT'))
    (maxsitno <- max(as.numeric(sits$V3), na.rm=TRUE))
  }else{
    sits <- data.frame()
    maxsitno <- 0
  }

  print(sits)
  print(maxsitno)

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
                                   column(6, radio('obs_data', 'On data entry:', observers)),
                                   column(6, radio('platform','Platform:', platforms))),
                                 br(),
                                 fluidRow(
                                   column(4, radio('obs_left','On Left:', c('None', observers))),
                                   column(4, radio('obs_right','On Right:', c('None', observers))),
                                   column(4, radio('obs_io','Ind. obs.:', c('None', observers)))),
                                 br(),
                                 fluidRow(column(1),
                                          column(10,
                                                 helpText('Update conditions *immediately upon* starting your scan!'),
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
                                   fluidRow(column(3, h4('Left edge (deg.)'), uiOutput('cz_left')),
                                            column(3, h4('Right edge (deg.)'), uiOutput('cz_right')),
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
                                                   h4('Left edge (deg.)'), uiOutput('glare_left'),
                                                   h4('Right edge (deg.)'), uiOutput('glare_right')),
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
                                                   h4('Best group size*'), uiOutput('grp_best')),
                                            column(3,
                                                   radio('species_type', 'Type:*', names(species), default_choices = NULL, height='150px'),
                                                   h4('Species*'), uiOutput('species'))
                                   ),
                                   hr(),
                                   fluidRow(column(3, h4('Primary bhvr:'), uiOutput('bhvr_primary')),
                                            column(3, h4('Secondary:'), uiOutput('bhvr_secondary')),
                                            column(3, h4('Tertiary:'), uiOutput('bhvr_tertiary')),
                                            column(3, h4('Direction:'), radio('direction', NULL, c('N/A','None','N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'), default_choices = NULL, height='150px'))),
                                   hr(),
                                   fluidRow(column(3, h4('Threat interaction?'), radio('threat', NULL, c('FALSE','TRUE'), default_choices = NULL, height='100px')),
                                            column(3, h4('# calves:'), radio('calves', NULL, c('N/A',0:10), default_choices = NULL, height='100px')),
                                            column(3, h4('# males: '), radio('males', NULL, c('N/A',0:10), default_choices = NULL, height='100px')),
                                            column(3, h4('Acoustics:'), radio('acoustics', NULL, c('N/A', 'Cannot hear', 'Maybe', 'Yes'), default_choices = NULL, height='100px'))),
                                   hr(),
                                   fluidRow(column(4, actionButton('sit_photo','Got photo @ this bearing',style=textstyle(button_size*.6, button_padding*.6), width="100%")),
                                            column(1),
                                            column(7, actionButton('sit_store','Store new sighting',style=textstyle(button_size*.8, button_padding*.6), width="100%")),
                                            ),
                                   br(),
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
                                                   h4('Measured with'), radio('up_reticle_how', NULL, c(optics), height='120px')),
                                            column(4,
                                                   h4('New distance (km)'), uiOutput('up_distance'),
                                                  br(),
                                                  actionButton('up_photo', 'Got photo @ this bearing', style=textstyle(button_size*.5, button_padding*.6), width="95%"))),
                                   hr(),
                                   fluidRow(column(3,
                                                   h4('Add estimate from:'), radio('up_obs', NULL, observers, height='100px')),
                                            column(3,
                                                   h4('Has group size changed?'), radio('up_changed', NULL, choices=c('Unchanged', 'Changed'), default_choices = NULL, height='100px')),
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
                                   fluidRow(column(3, h4('Update 1st bhvr:'), uiOutput('up_primary')),
                                            column(3, h4('Update 2nd:'), uiOutput('up_secondary')),
                                            column(3, h4('Update 3rd:'), uiOutput('up_tertiary')),
                                            column(3, h4('Update dir.:'), radio('up_direction', NULL, c('N/A','None','N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'), default_choices = NULL, height='150px'))),
                                   hr(),
                                   fluidRow(column(3, h4('New threat interaction?'), radio('up_threat', NULL, c('FALSE','TRUE'), default_choices = NULL, height='100px')),
                                            column(3, h4('Acoustics:'), radio('up_acoustics', NULL, c('N/A', 'Cannot hear', 'Maybe', 'Yes'), default_choices = NULL, height='100px')),
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
                                   br(),
                                   br(),
                                   width = 12), # end of box
                        ),

                        ##############################################################################
                        ##############################################################################
                        tabPanel('Review',
                                 tabsetPanel(
                                   tabPanel(h4('Overview'),
                                            br(),
                                            tabsetPanel(
                                              tabPanel(h5('Scans'),
                                                       br(), DTOutput('scans')),
                                              tabPanel(h5('Sightings'),
                                                       br(), DTOutput('sits')),
                                              tabPanel(h5('Condition zones'),
                                                       br(), DTOutput('sea')),
                                              tabPanel(h5('Comments'),
                                                       br(), DTOutput('comments'))
                                              )),
                                   tabPanel(h4('Raw data'),
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
                                            'Coming someday!')
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
    #rv$overview <- NULL
    rv$df_backup <- NULL
    rv$keypad <- '0'
    gps_timer <- reactiveTimer(1000*gps_interval)

    # Survey overview
    observeEvent(rv$df,{
      rv$overview <- survey_overview()
    })

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
    rv$sit_photo <- NULL

    # Update sighting ===================
    rv$up_bearing <- NULL
    rv$up_reticle <- NULL
    rv$up_reticle_how <- NULL
    rv$up_distance <- NULL
    rv$up_max <- 0
    rv$up_min <- 0
    rv$up_best <- 0
    rv$up_photo <- NULL

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
      if(beeps){beepr::beep(10)}
    }) })

    # When scan ends
    observeEvent(input$scan_off,{
      rv$scan <- 0
      scan_timer <- NULL
      scan_data <- c(rv$scan, input$platform, input$obs_data, input$obs_left, input$obs_right, input$obs_io)
      rv$df <- log_line('EFF', scan_data)
      if(beeps){beepr::beep(3)}
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
          if(round(as.numeric(rv$scan_duration)) > round((scan_target*60))){
            if(beeps){beepr::beep(10)}
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
        if(beeps){beepr::beep()}

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

    # Photo
    observeEvent(input$sit_photo,{
      rv$sit_photo <- as.character(Sys.time())
    })

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
          sit_data <- c(rv$next_sit, rv$bearing,
                        rv$reticle, input$reticle_how, rv$distance,
                        input$inline, input$cue,
                        rv$grp_max, rv$grp_min, rv$grp_best,
                        input$species_type, input$species,
                        input$bhvr_primary, input$bhvr_secondary, input$bhvr_tertiary, input$direction,
                        input$threat, input$calves, input$males, input$acoustics,
                        rv$sit_photo)

          # Log it
          rv$df <- log_line('SIT', sit_data)
          if(beeps){beepr::beep()}

          # Manage aftermath (update buttons, fields, etc.)
          rv$next_sit <- rv$next_sit + 1
          rv$bearing <- NULL ; rv$reticle <- NULL ; rv$distance <- NULL
          rv$grp_max <- 1 ; rv$grp_min <- 1 ; rv$grp_best <- 1
          rv$sit_photo <- NULL
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

    # Photo
    observeEvent(input$up_photo,{
      rv$up_photo <- as.character(Sys.time())
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
                      input$up_threat, input$up_acoustics,
                      rv$up_photo)

        # Log it
        rv$df <- log_line('UPD', upd_data)
        if(beeps){beepr::beep(2)}

        # Manage aftermath (update buttons, fields, etc.)
        rv$up_bearing <- NULL ; rv$up_reticle <- NULL ; rv$up_distance <- NULL
        rv$up_max <- 0 ; rv$up_min <- 0 ; rv$up_best <- 0
        rv$up_photo <- NULL
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
      if(beeps){beepr::beep(2)}
      shinyjs::reset('com_spp_table') ; shinyjs::reset('com_manual')
    })

    observeEvent(input$com_1,{
      com_txt <- comment_1
      sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
      sit_row <- input$com_spp_table_rows_selected
      sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
      com_data <- c(sitno, com_txt)
      rv$df <- log_line('COM', com_data)
      if(beeps){beepr::beep(2)}
      shinyjs::reset('com_spp_table')
    })

    observeEvent(input$com_2,{
      com_txt <- comment_2
      sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
      sit_row <- input$com_spp_table_rows_selected
      sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
      com_data <- c(sitno, com_txt)
      rv$df <- log_line('COM', com_data)
      if(beeps){beepr::beep(2)}
      shinyjs::reset('com_spp_table')
    })

    observeEvent(input$com_3,{
      com_txt <-  comment_3
      sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
      sit_row <- input$com_spp_table_rows_selected
      sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
      com_data <- c(sitno, com_txt)
      rv$df <- log_line('COM', com_data)
      if(beeps){beepr::beep(2)}
      shinyjs::reset('com_spp_table')
    })

    observeEvent(input$com_4,{
      com_txt <- comment_4
      sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
      sit_row <- input$com_spp_table_rows_selected
      sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
      com_data <- c(sitno, com_txt)
      rv$df <- log_line('COM', com_data)
      if(beeps){beepr::beep(2)}
      shinyjs::reset('com_spp_table')
    })

    observeEvent(input$com_5,{
      com_txt <- comment_5
      sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
      sit_row <- input$com_spp_table_rows_selected
      sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
      com_data <- c(sitno, com_txt)
      rv$df <- log_line('COM', com_data)
      if(beeps){beepr::beep(2)}
      shinyjs::reset('com_spp_table')
    })

    observeEvent(input$com_6,{
      com_txt <- comment_6
      sits <- rv$df %>% dplyr::filter(V2 == 'SIT')
      sit_row <- input$com_spp_table_rows_selected
      sitno <- ifelse(is.null(sit_row), NA, sits$V3[sit_row])
      com_data <- c(sitno, com_txt)
      rv$df <- log_line('COM', com_data)
      if(beeps){beepr::beep(2)}
      shinyjs::reset('com_spp_table')
    })

    ##############################################################################
    ##############################################################################
    # Review - Overview

    output$scans = DT::renderDT( rv$overview$scans,
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
                                editable=FALSE)


    output$sits = DT::renderDT( rv$overview$sightings,
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
                                 editable=FALSE)

    output$sea = DT::renderDT( rv$overview$conditions,
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
                                editable=FALSE)

    output$comments = DT::renderDT( rv$overview$comments,
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
                                editable=FALSE)

    ##############################################################################
    ##############################################################################
    # Review - Raw Data

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
      new_filename <- get_filename() ; new_filename
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
