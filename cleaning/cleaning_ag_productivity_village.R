# Purpose: This script doespulls EVI
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




for (year in 2002:2013){
  
  # MONSOON --------------------------------------------------------------------
  # This changes by year
  r <- raster(paste0("data/raw/ag_productivity/NGPPCY-IN-M", year, ".tif"))
  
  # crop first (makes extraction MUCH faster)
  r <- crop(r, villages)
  
  # extract
  extracted_values <- exact_extract(r, villages, fun = "mean", append_cols = "shrid")
  
  # save
  write_csv(extracted_values, paste0("data/clean/ag_productivity/monsoon", year, ".csv"))
  
  # WINTER --------------------------------------------------------------------
  # This changes by year
  r <- raster(paste0("data/raw/ag_productivity/NGPPCY-IN-W", year, ".tif"))
  
  # crop first (makes extraction MUCH faster)
  r <- crop(r, villages)
  
  # extract
  extracted_values <- exact_extract(r, villages, fun = "mean", append_cols = "shrid")
  
  # save
  write_csv(extracted_values, paste0("data/clean/ag_productivity/winter", year, ".csv"))
  
}


# The months for monsoon are Jun to Oct
# The months for Winter are Nov to Mar
# 2002 to 2013
for (year in 2002:2013){
  # Monsoon is this year only
  rainfall <- read_csv(paste0("data/clean/terra/precip", year, ".csv"))
  rainfall$rain_total <- apply(rainfall[,c(7:11)], 1, sum)
  rainfall <- rainfall %>% dplyr::select(shrid, rain_total)
  rainfall$season <- "monsoon"
  # Load the extracted values from above
  extracted_values <- read_csv(paste0("data/clean/ag_productivity/monsoon", year, ".csv"))
  # Fix some mistakes in the extraction
  extracted_values <- extracted_values %>% 
                        group_by(shrid) %>% 
                        mutate(mean = mean(mean)) %>% 
                        filter(row_number()==1) %>% 
                        ungroup()
  extracted_values <- extracted_values %>% left_join(rainfall, by = "shrid")
  
  # Winter is TWO years
  rainfall1 <- read_csv(paste0("data/clean/terra/precip", year, ".csv"))
  rainfall1$rain_total1 <- apply(rainfall1[,c(12:13)], 1, sum)
  rainfall1 <- rainfall1 %>% dplyr::select(shrid, rain_total1)
  # Second year
  rainfall2 <- read_csv(paste0("data/clean/terra/precip", year + 1, ".csv"))
  rainfall2$rain_total2 <- apply(rainfall2[,c(2:4)], 1, sum)
  rainfall2 <- rainfall2 %>% dplyr::select(shrid, rain_total2)
  rainfall <- cbind(rainfall1, rainfall2[,-1])
  rainfall <- rainfall %>% mutate(rain_total = rain_total1 + rain_total2)
  rainfall <- rainfall %>% dplyr::select(shrid, rain_total)
  rainfall$season <- "winter"
  
  # Load the extracted values from above
  extracted_values_winter <- read_csv(paste0("data/clean/ag_productivity/winter", year, ".csv"))
  extracted_values_winter <- extracted_values_winter %>% 
                              group_by(shrid) %>% 
                              mutate(mean = mean(mean)) %>% 
                              filter(row_number()==1) %>% 
                              ungroup()
  extracted_values_winter <- extracted_values_winter %>% left_join(rainfall, by = "shrid")
  
  
  extracted_values <- rbind(extracted_values, extracted_values_winter)
  write_csv(extracted_values, paste0("data/clean/ag_productivity/rain_merged_both", year, ".csv"))
}


# Now let's merge these with the pollution data
for (year in 2002:2013){
  # Monsoon is this year only
  village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year, "m1.csv")) %>% as_tibble()
  villages <- village_wind %>% dplyr::select(shrid)
  for (month in 6:10){
    village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month))
    villages <- villages %>% left_join(village_wind, by = "shrid")
  }
  villages$season <- "monsoon"
  villages <- villages %>% mutate(days_sums = rowSums(villages[,2:6]))
  villages <- villages %>% dplyr::select(shrid, days_sums, season)
  
  villages2 <- villages %>% dplyr::select(shrid)
  for (month in 11:12){
    village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month))
    villages2 <- villages2 %>% left_join(village_wind, by = "shrid")
  }
  for (month in 1:3){
    village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year + 1, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month))
    villages2 <- villages2 %>% left_join(village_wind, by = "shrid")
  }
  villages2 <- villages2 %>% mutate(days_sums = rowSums(villages2[,2:6]))
  villages2 <- villages2 %>% dplyr::select(shrid, days_sums)
  villages2$season <- "winter"
  
  villages <- rbind(villages, villages2)
  
  
  extracted_values <- read_csv(paste0("data/clean/ag_productivity/rain_merged_both", year, ".csv"))
  extracted_values <- extracted_values %>% left_join(villages, by = c("shrid", "season"))
  extracted_values$year <- year
  
  write_csv(extracted_values, paste0("data/clean/ag_productivity/rain_pollution_merged_both", year, ".csv"))
}



df <- c()
for (year in 2002:2013){
  extracted_values <- read_csv(paste0("data/clean/ag_productivity/rain_pollution_merged_both", year, ".csv"))
  df <- rbind(df, extracted_values)
}
write_csv(df, paste0("data/clean/ag_productivity/all_combined.csv"))





summary(feols(log(mean) ~ days_sums | shrid^season + year, data = df, cluster = c("shrid")))
summary(feols(log(mean) ~ days_sums + log(rain_total) | shrid^season + year, data = df, cluster = c("shrid")))
summary(feols(log(mean) ~ days_sums + log(rain_total) | shrid^season + year^state, data = df, cluster = c("shrid")))








