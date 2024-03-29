# Super quick load (no install)
library(devtools) ; document() #; load_all()

# Quick install
#library(devtools) ; remove.packages(hyfer) ; document() ; install() ; library(stationary)

# Full load and check
library(devtools) ; document() ; load_all() ; check() ; install() ; library(suRvey)

# Create package environment

#library(devtools)
#setwd('../')
#create_package('/Users/erickeen/repos/suRvey')

use_pipe(export=TRUE)

# Import packages
if(FALSE){
  use_package('magrittr')
  use_package('plyr')
  use_package('dplyr')
  use_package('readr')
  use_package('stringr')
  use_package('lubridate')
  use_package('usethis')
  use_package('devtools')
  use_package('shiny')
  use_package('shinyjs')
  use_package('shinydashboard')
  use_package('shinythemes')
  use_package('shinyWidgets')
  use_package('rintrojs')
  use_package('DataCombine')
  use_package('DT')
  use_package('wesanderson')
  use_package('bangarang')
  use_package('suncalc')
  use_package('truncnorm')
  use_package('plotrix')
  use_package('sp')
  use_package('sf')
  use_package('beepr')
  use_package('BiocManager')
  use_package('EBImage')
  use_package('readr')
  use_package('exifr')
  use_package('exiftoolr')
  use_package('OpenImageR')
}

#use_mit_license()

#### Try it out


# Install suRvey
library(remotes)
remotes::install_github('ericmkeen/suRvey',
                         force=TRUE)
library(suRvey)

# Test image_measure()
image_measure()

survey_overview()




# Customize settings =======================================

observers <- c('Grace','Janie','Chenoah', 'Ben')

platforms <- c('Inside','Outside')

optics <- c('N/A','Big Eyes', 'Binocs', 'Scope', 'Naked Eye')

landmarks <- c('N/A','Gil Mtn', 'CMP peaks', 'Otter',
               'Twartz', 'Farrant', 'N Fin shore')

cues <- c('N/A','Blow','Vessel','Body','Fluke','Splash','Sound')

species <- list('MarMam' = c('Humpback', 'Fin', 'Dalls porpoise',
                             'Harbour seal', 'Stellars sea lion',
                             'Elephant seal', 'Killer whale'),
                'Vessel' = c('Large rec', 'Small rec', 'CFV', 'Sailing',
                             'Tug only', 'Tug+barge', 'Gitgaat',
                             'Research', 'Cruise', 'Tanker'))

behaviours <- list('MarMam' = c('Active', 'Sleep', 'Feeding',
                                'BNF', 'Milling', 'Robust', 'Fast travel',
                                'RE-TR'),
                   'Vessel' = c('Fast travel', 'Slow travel', 'Fishing',
                                'With whales', 'Anchored'))
# Launch app =======================================

survey_app(observers,
           platforms,
           optics,
           landmarks,
           cues,
           species,
           behaviours,
           scan_target = 1,
           beeps = TRUE)




