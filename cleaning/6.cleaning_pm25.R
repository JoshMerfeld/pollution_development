# Purpose: This script cleans 
# Author: Josh Merfeld
# Date: December 12th, 2022

rm(list = ls())

# Load packages
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(ncdf4)
library(lubridate)
library(exactextractr)  


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check



villages_centroids <- read_sf("data/spatial/centroids_villages_30km")
villages_centroids <- as_tibble(villages_centroids) %>% dplyr::select(shrid)
villages_centroids$within30 <- 1
villages <- read_sf("data/spatial/shapefiles/village.shp")
villages$shrid <- paste0(villages$pc11_s_id, "-", villages$pc11_tv_id)
villages <- villages %>% left_join(villages_centroids, by = "shrid")
villages <- villages %>% filter(within30==1) %>% dplyr::select(shrid)
rm(villages_centroids)



months <- c()
for (month in 1:9){
  months <- c(months, paste0("0", month))
}
months <- c(months, "11", "12")
pollution <- c()
for (year in 1998:2015){
  for (month in months){
    # MONSOON --------------------------------------------------------------------
    # This changes by year
    r <- raster(paste0("data/raw/pm25/V5GL03.HybridPM25.Asia.", year, month, "-", year, month, ".nc"))
    
    # crop first (makes extraction MUCH faster)
    r <- crop(r, villages)
    
    # extract
    extracted_values <- exact_extract(r, villages, fun = "mean", append_cols = "shrid")
    extracted_values <- as_tibble(extracted_values) %>% 
                        group_by(shrid) %>% 
                        mutate(mean = mean(mean)) %>% 
                        filter(row_number()==1) %>%
                        ungroup()
    
    # And wind direction
    village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year, "m", as.numeric(month), ".csv")) %>% as_tibble()
    
    # Merger
    extracted_values <- extracted_values %>% left_join(village_wind, by = "shrid")
    extracted_values$year <- year
    extracted_values$month <- as.numeric(month)
    
    # save
    write_csv(extracted_values, paste0("data/clean/pm25/y", year, "m", as.numeric(month), ".csv"))
    
    pollution <- rbind(pollution, as_tibble(extracted_values))
  }
}
write_csv(pollution, paste0("data/clean/pm25/all_combined.csv"))


# Now clean to merge into ag productivity (try IV?)
pollution <- read_csv(paste0("data/clean/pm25/all_combined.csv"))
pollution$season <- ""
pollution$season[pollution$month %in% c(6:10)] <- "monsoon"
pollution$season[pollution$month %in% c(11:12) | pollution$month %in% c(1:3)] <- "winter"
pollution$year[pollution$season=="winter"] <- pollution$year[pollution$season=="winter"] - 1

pollution$m1 <- NA
pollution$m1[pollution$month==6 | pollution$month==11] <- pollution$mean[pollution$month==6 | pollution$month==11]
pollution$m2 <- NA
pollution$m2[pollution$month==7 | pollution$month==12] <- pollution$mean[pollution$month==7 | pollution$month==12]
pollution$m3 <- NA
pollution$m3[pollution$month==8 | pollution$month==1] <- pollution$mean[pollution$month==8 | pollution$month==1]
pollution$m4 <- NA
pollution$m4[pollution$month==9 | pollution$month==2] <- pollution$mean[pollution$month==9 | pollution$month==2]
pollution$m5 <- NA
pollution$m5[pollution$month==10 | pollution$month==3] <- pollution$mean[pollution$month==10 | pollution$month==3]

# Now shrid, year, season
pollution <- pollution %>% 
                group_by(shrid, year, season) %>% 
                mutate(
                       pm25 = mean(mean),
                       pm1 <- mean(m1),
                       pm2 <- mean(m2),
                       pm3 <- mean(m3),
                       pm4 <- mean(m4),
                       pm5 <- mean(m5)
                       ) %>%
                filter(row_number()==1) %>%
                ungroup() %>%
                dplyr::select(shrid, year, season, pm25,
                              pm1, pm2, pm3, pm4, pm5)

write_csv(pollution, paste0("data/clean/pm25/ag_merge.csv"))




# Now clean to merge into NTL
pollution <- read_csv(paste0("data/clean/pm25/all_combined.csv"))

# Now shrid, year, season
pollution <- pollution %>% 
                group_by(shrid, year) %>% 
                mutate(
                       pm25 = mean(mean)
                       ) %>%
                filter(row_number()==1) %>%
                ungroup() %>%
                dplyr::select(shrid, year, pm25)

write_csv(pollution, paste0("data/clean/pm25/ntl_merge.csv"))







