# suRvey
Data entry app for wildlife surveys, optimized for touchscreens.

## Install the package

```
# install.packages("remotes") # uncomment if you need to install
remotes::install_github("ericmkeen/suRvey")
library(suRvey)
```

## Touchscreen data entry app

To build and launch your survey app, see the documentation here:

```
?survey_app
```

This function launches a Shiny app designed to optimize data entry during wildlife surveys.
The app was designed with marine mammal surveys in mind, but should be generalizable for other contexts
and target species. GPS position updates are possible in this app, if the GPS feature is enabled on your device. 
The app should be useful for both stationary and transect-based surveys.
Layout has been optimized for touch screens.


https://user-images.githubusercontent.com/10263731/159953772-5f319a74-546d-4e39-a393-b27469dc10a3.mov


Here is the code used to produce this demo:

```
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
           behaviours)
```

## Image analysis tool 

If your survey involves taking fine-scale measurements of detection locations based upon photographs, you can use the `image_measure()` function to batch-process your photographs. 

```
?image_measure()
```

To try out this function, you can download a demo "images" folder [here](https://www.dropbox.com/sh/gv1tz7a0juj3cxx/AABsibIkIio0SnsEKrxVZtNCa?dl=0). Place it in your working directory and extract/unzip it. The app should then work using the four demo photographs within that "images" folder. 

You can then try out the function by simply running:

```
image_measure()
```

The first time you run it, make sure you are connected to the internet; you will be prompted to allow `R` to download the package `EBImage` through the function of another package (`BiocManager`).  


## Coming soon

Next in the development pipeline:

- **`survey_process()`**: a function for processing raw data into an easily summarized and analyzed format.
- **`survey_summarize()`**: a function for summarizing effort and sightings.
- **`survey_map()`**: a spatial mapping function.  

