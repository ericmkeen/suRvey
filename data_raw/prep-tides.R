################################################################################
################################################################################
# AIS data processing
################################################################################
################################################################################

library(readr)
library(dplyr)
library(bangarang)
library(devtools)
library(lubridate)

# do not rerun the years you have already processed
if(FALSE){

  ################################################################################
  ################################################################################
  # 2022

  t202205 <- read_csv("~/Dropbox/Other WIPs/NCCS/Projects/land-based/2022/tides/2022-05.csv")
  t202206 <- read_csv("~/Dropbox/Other WIPs/NCCS/Projects/land-based/2022/tides/2022-06.csv")
  t202207 <- read_csv("~/Dropbox/Other WIPs/NCCS/Projects/land-based/2022/tides/2022-07.csv")
  t202208 <- read_csv("~/Dropbox/Other WIPs/NCCS/Projects/land-based/2022/tides/2022-08.csv")
  t202209 <- read_csv("~/Dropbox/Other WIPs/NCCS/Projects/land-based/2022/tides/2022-09.csv")
  t202210 <- read_csv("~/Dropbox/Other WIPs/NCCS/Projects/land-based/2022/tides/2022-10.csv")

  tides <-
    rbind(t202205, t202206, t202207, t202208, t202209, t202210) %>%
    select(station = STATION_ID,
           date = `TIME_TAG PST (Z+8)`,
           tide = CFB)

  tides$date <-  lubridate::as_datetime(tides$date)
  tides$date <-  lubridate::force_tz(tides$date, tzone='Canada/Pacific')


  ################################################################################
  ################################################################################
  # 2023

  subdir <- "/Users/ekezell/Desktop/projects/suRvey/fin 2023/tides/"
  (lf <- list.files(subdir))
  (lfs <- paste0(subdir, lf))

  mr <- data.frame()
  for(i in 1:length(lfs)){
    (lfi <- lfs[i])
    message(lfi)
    mri <- read.csv(lfi)
    mr <- rbind(mr, mri)
  }

  nrow(mr)

  mr %>% head

  mr <-
    mr %>%
    mutate(station = 'HARTLEY BAY',
           date = lubridate::as_datetime(paste0(Date,':00')),
           tide = `predictions..m.`) %>%
    select(station, date, tide)

  mr$date <-  lubridate::force_tz(mr$date, tzone='Canada/Pacific')
  mr %>% head

} # end not run

################################################################################
################################################################################
# Add 2024

subdir <- "/Users/ekezell/Desktop/projects/suRvey/fin 2024/tides/"
(lf <- list.files(subdir))
(lfs <- paste0(subdir, lf))
lfs

mr <- data.frame()
for(i in 1:length(lfs)){
  (lfi <- lfs[i])
  message(lfi)
  mri <- read.csv(lfi)
  mr <- rbind(mr, mri)
}

nrow(mr)

mr %>% head

mr <-
  mr %>%
  mutate(station = 'HARTLEY BAY') %>%
  mutate(date = lubridate::as_datetime(paste0(Date,':00'))) %>%
  mutate(tide = `observations..m.`) %>%
  select(station, date, tide)

mr$date <-  lubridate::force_tz(mr$date, tzone='Canada/Pacific')
mr %>% head

################################################################################
################################################################################
# Add 2025

subdir <- "/Users/ekezell/Desktop/projects/suRvey/fin 2025/tides/"
(lf <- list.files(subdir))
(lfs <- paste0(subdir, lf))
lfs
mr <- data.frame()
for(i in 1:length(lfs)){
  (lfi <- lfs[i])
  message(lfi)
  mri <- read.csv(lfi)
  mr <- rbind(mr, mri)
}
nrow(mr)
mr %>% head

mr <-
  mr %>%
  mutate(station = 'HARTLEY BAY') %>%
  mutate(date = lubridate::as_datetime(paste0(Date,':00'))) %>%
  mutate(tide = `predictions..m.`) %>%
  select(station, date, tide) %>%
  group_by(station, date) %>%
  summarize(tide = tide[1] %>% as.numeric()) %>%
  ungroup()
mr %>% head
mr$date <-  lubridate::force_tz(mr$date, tzone='Canada/Pacific')
ggplot(mr, aes(x=date, y=tide)) + geom_point(size=.01)
mr %>% head

################################################################################
################################################################################
# Save dataset

data(tides)
tides %>% head
tides %>% tail
tides <- rbind(tides, mr)
#tides$date %>% plot

# Make sure there is only one tide entry per minute
tides <-
  tides %>%
  group_by(station, date) %>%
  summarize(tide = tide[1]) %>%
  arrange(station, date) %>%
  filter(!is.na(tide))

head(tides)
tail(tides)

usethis::use_data(tides, overwrite = TRUE)


