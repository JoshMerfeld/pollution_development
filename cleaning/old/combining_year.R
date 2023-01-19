# Purpose: This script combines the yearly NTL data and the pollution data
# Author: Josh Merfeld
# Date: December 26th, 2022

rm(list = ls())

library(tidyverse)
library(sf)
library(fixest)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check


# village nTL data

### Load raw data ----------------------------------------------------------------------------------------------------------------------------------------------
ntl <- as_tibble(read.csv("data/raw/shrug-v1.5.samosa-nl-csv/shrug_nl_wide.csv"))
ntl <- ntl %>% select(-num_cells)
ntl <- ntl %>% mutate(shrid = substr(shrid, 4, 20))

# Just keep those within 30 km
centroids <- read_sf("data/spatial/centroids_villages_30km")
ntl <- as_tibble(centroids) %>% select(shrid) %>% left_join(ntl, by = "shrid")
rm(centroids)

# Gather, one variable at a time (we want long by year but wide by variables)
ntl <- ntl %>% select(-c(starts_with("total_light_cal"), starts_with("max_"))) %>%
                                gather(
                                       "variable",
                                       "ntl",
                                       -shrid
                                       )
# Do some cleaning
ntl <- ntl %>% mutate(
                        year = substr(variable, str_length(variable) - 3, str_length(variable))
                        ) %>%
                  select(shrid, year, total_light = ntl)

# Looks like missings should be ZEROS (according to the csv). This makes sense; there shouldn't be missing values.
ntl <- ntl %>% mutate(total_light = ifelse(is.na(total_light), 0, total_light))


# Now go through yearly values of wind direction
winds <- read_csv("data/clean/wind_ntl/years/y1990.csv")
winds$year <- 1990
for (year in 1991:2013){
  temp <- read_csv(paste0("data/clean/wind_ntl/years/y", year, ".csv"))
  temp$year <- year
  winds <- rbind(winds, temp)
}
ntl <- ntl %>% mutate(year = as.numeric(year)) %>% left_join(winds, by = c("shrid", "year"))
# and precip
precip <- read_csv("data/clean/terra/precip1990.csv")
precip$rain <- rowSums(precip[,2:13], na.rm = T)
precip$year <- 1990
precip <- precip %>% select(shrid, year, rain)
for (year in 1991:2013){
  temp <- read_csv(paste0("data/clean/terra/precip", year, ".csv"))
  temp$rain <- rowSums(temp[,2:13], na.rm = T)
  temp$year <- year
  temp <- temp %>% select(shrid, year, rain)
  precip <- rbind(precip, temp)
}
# Why are there sometimes mlutiple here?
precip <- precip %>% group_by(shrid, year) %>% filter(row_number()==1) %>% ungroup()

ntl <- ntl %>% left_join(precip, by = c("shrid", "year"))
rm(list = c("temp", "winds", "precip"))
write.csv(ntl, "data/clean/village_ntl.csv")


ntl <- read_csv("data/clean/village_ntl.csv")
ntl <- ntl %>% mutate(state = substr(shrid, 1, 2),
                      ln_sums = log(sums + 1),
                      ln_total_light = log(total_light + 1),
                      ln_rain = log(rain + 1))
ntl <- panel(ntl, ~shrid + year, duplicate.method = 'first')


summary(feols(ln_total_light ~ ln_sums | shrid + year, data = ntl, cluster = c("shrid")))
summary(feols(ln_total_light ~ ln_sums + ln_rain | shrid + year, data = ntl, cluster = c("shrid")))
summary(feols(ln_total_light ~ l(ln_sums, 0:2) | shrid + year, data = ntl, cluster = c("shrid")))
summary(feols(ln_total_light ~ l(ln_sums, 0:2) + l(ln_rain, 0:2) | shrid + year, data = ntl, cluster = c("shrid")))











