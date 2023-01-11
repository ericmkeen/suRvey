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

usethis::use_data(tides, overwrite = TRUE)

