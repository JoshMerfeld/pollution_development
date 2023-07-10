# Purpose: This script cleans the nightlights data
# Author: Josh Merfeld
# Date: December 12th, 2022

rm(list = ls())

library(sf)
library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(openxlsx)
library(tidyverse)
library(ncdf4)
library(lubridate)
library(R.utils)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check


### Load raw data ----------------------------------------------------------------------------------------------------------------------------------------------
ntl <- as_tibble(read.csv("data/raw/shrug-v1.5.samosa-nl-csv/shrug_nl_wide.csv"))
ntl <- ntl %>% select(-num_cells)

# Gather, one variable at a time (we want long by year but wide by variables)
ntl2 <- ntl %>% select(-c(starts_with("total_light_cal"), starts_with("max_"))) %>%
                                gather(
                                       "variable",
                                       "ntl",
                                       -shrid
                                       )
# Do some cleaning
ntl2 <- ntl2 %>% mutate(
                        year = substr(variable, str_length(variable) - 3, str_length(variable))
                        ) %>%
                  select(shrid, year, total_light = ntl)

# Same thing for nezt var
ntl3 <- ntl %>% select(c(starts_with("total_light_cal"), "shrid")) %>%
                                gather(
                                       "variable",
                                       "ntl",
                                       -shrid
                                       )
# Do some cleaning
ntl3 <- ntl3 %>% mutate(
                        year = substr(variable, str_length(variable) - 3, str_length(variable))
                        ) %>%
                  select(shrid, year, total_light_cal = ntl)

# And the last one
ntl4 <- ntl %>% select(c(starts_with("max"), "shrid")) %>%
                                gather(
                                       "variable",
                                       "ntl",
                                       -shrid
                                       )
# Do some cleaning
ntl4 <- ntl4 %>% mutate(
                        year = substr(variable, str_length(variable) - 3, str_length(variable))
                        ) %>%
                  select(shrid, year, max_light = ntl)


ntl <- ntl2 %>% left_join(ntl3, by = c("shrid", "year")) %>% 
                left_join(ntl4, by = c("shrid", "year"))

write.csv(ntl, "data/clean/village_ntl.csv")





