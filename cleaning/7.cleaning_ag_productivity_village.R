# Purpose: This script cleans data for estimates of agricultural productivity
# data from here: https://springernature.figshare.com/collections/A_new_two-decade_2001-2019_high-resolution_agricultural_primary_productivity_dataset_for_India/6079104
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
villages_centroids <- villages_centroids %>%
                          group_by(shrid) %>%
                          filter(row_number()==1) %>%
                          ungroup()
villages <- read_sf("data/spatial/shapefiles/village.shp")
villages$shrid <- paste0(villages$pc11_s_id, "-", villages$pc11_tv_id)
villages <- villages %>%
              group_by(shrid) %>%
              filter(row_number()==1) %>%
              ungroup()
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

# date of coal plant opening
dist_matrix <- read.csv(paste0("data/clean/wind_ntl/dist_matrix_angles.csv"))

# The months for monsoon are Jun to Oct
# The months for Winter are Nov to Mar
# 2002 to 2013
for (year in 2002:2013){
  # Is there a single coal plant within 30 km that year?
  
  # Monsoon is this year only
  rainfall <- read_csv(paste0("data/clean/terra/precip", year, ".csv"))
  rainfall <- rainfall[, c(1, 7:11)]
  rainfall$rain_total <- apply(rainfall[,c(2:6)], 1, sum)
  colnames(rainfall) <- c("shrid", "rain_m1", "rain_m2", "rain_m3", "rain_m4", "rain_m5", "rain_total")
  rainfall$season <- "monsoon"
  # temp
  tempMax <- read_csv(paste0("data/clean/terra/tmax", year, ".csv"))
  tempMax <- tempMax[, c(1, 7:11)]
  tempMax$tmax_mean <- apply(tempMax[,c(2:6)], 1, mean)
  colnames(tempMax) <- c("shrid", "tmax_m1", "tmax_m2", "tmax_m3", "tmax_m4", "tmax_m5", "tmax_mean")
  tempMax$season <- "monsoon"
  tempMin <- read_csv(paste0("data/clean/terra/tmax", year, ".csv"))
  tempMin <- tempMin[, c(1, 7:11)]
  tempMin$tmin_mean <- apply(tempMin[,c(2:6)], 1, mean)
  colnames(tempMin) <- c("shrid", "tmin_m1", "tmin_m2", "tmin_m3", "tmin_m4", "tmin_m5", "tmin_mean")
  tempMin$season <- "monsoon"
  # Load the extracted values from above
  extracted_values <- read_csv(paste0("data/clean/ag_productivity/monsoon", year, ".csv"))
  # Fix some mistakes in the extraction
  extracted_values <- extracted_values %>% 
                        group_by(shrid) %>% 
                        mutate(mean = mean(mean)) %>% 
                        filter(row_number()==1) %>% 
                        ungroup()
  rainfall <- rainfall %>% 
              group_by(shrid) %>% 
              mutate(
                     rain_m1 = mean(rain_m1),
                     rain_m2 = mean(rain_m2),
                     rain_m3 = mean(rain_m3),
                     rain_m4 = mean(rain_m4),
                     rain_m5 = mean(rain_m5),
                     rain_total = mean(rain_total)
                     ) %>% 
              filter(row_number()==1) %>% 
              ungroup()
  tempMax <- tempMax %>% 
              group_by(shrid) %>% 
              mutate(
                     tmax_m1 = mean(tmax_m1),
                     tmax_m2 = mean(tmax_m2),
                     tmax_m3 = mean(tmax_m3),
                     tmax_m4 = mean(tmax_m4),
                     tmax_m5 = mean(tmax_m5),
                     tmax_mean = mean(tmax_mean)
                     ) %>% 
              filter(row_number()==1) %>% 
              ungroup()
  tempMin <- tempMin %>% 
              group_by(shrid) %>% 
              mutate(
                     tmin_m1 = mean(tmin_m1),
                     tmin_m2 = mean(tmin_m2),
                     tmin_m3 = mean(tmin_m3),
                     tmin_m4 = mean(tmin_m4),
                     tmin_m5 = mean(tmin_m5),
                     tmin_mean = mean(tmin_mean)
                     ) %>% 
              filter(row_number()==1) %>% 
              ungroup()
  extracted_values <- extracted_values %>% left_join(rainfall, by = "shrid")
  extracted_values <- extracted_values %>% left_join(tempMax, by = "shrid")
  extracted_values <- extracted_values %>% left_join(tempMin, by = "shrid")
  
  # Winter is TWO years
  rainfall1 <- read_csv(paste0("data/clean/terra/precip", year, ".csv"))
  rainfall1 <- rainfall1[, c(1, 12:13)]
  # Second year
  rainfall2 <- read_csv(paste0("data/clean/terra/precip", year + 1, ".csv"))
  rainfall2 <- rainfall2[, c(1, 2:4)]
  
  rainfall <- cbind(rainfall1, rainfall2[,-1])
  rainfall$rain_total <- apply(rainfall[,c(2:6)], 1, sum)
  colnames(rainfall) <- c("shrid", "rain_m1", "rain_m2", "rain_m3", "rain_m4", "rain_m5", "rain_total")
  rainfall$season <- "winter"
  # Temp
  tmax1 <- read_csv(paste0("data/clean/terra/tmax", year, ".csv"))
  tmax1 <- tmax1[, c(1, 12:13)]
  # Second year
  tmax2 <- read_csv(paste0("data/clean/terra/tmax", year + 1, ".csv"))
  tmax2 <- tmax2[, c(1, 2:4)]
  
  tempMax <- cbind(tmax1, tmax2[,-1])
  tempMax$tmax_mean <- apply(tempMax[,c(2:6)], 1, mean)
  colnames(tempMax) <- c("shrid", "tmax_m1", "tmax_m2", "tmax_m3", "tmax_m4", "tmax_m5", "tmax_mean")
  tempMax$season <- "winter"
  
  tmin1 <- read_csv(paste0("data/clean/terra/tmin", year, ".csv"))
  tmin1 <- tmin1[, c(1, 12:13)]
  # Second year
  tmin2 <- read_csv(paste0("data/clean/terra/tmin", year + 1, ".csv"))
  tmin2 <- tmin2[, c(1, 2:4)]
  
  tempMin <- cbind(tmin1, tmin2[,-1])
  tempMin$tmin_mean <- apply(tempMin[,c(2:6)], 1, mean)
  colnames(tempMin) <- c("shrid", "tmin_m1", "tmin_m2", "tmin_m3", "tmin_m4", "tmin_m5", "tmin_mean")
  tempMin$season <- "winter"
  
  
  # Load the extracted values from above
  extracted_values_winter <- read_csv(paste0("data/clean/ag_productivity/winter", year, ".csv"))
  extracted_values_winter <- extracted_values_winter %>% 
                              group_by(shrid) %>% 
                              mutate(mean = mean(mean)) %>% 
                              filter(row_number()==1) %>% 
                              ungroup()
  rainfall <- rainfall %>% 
              group_by(shrid) %>% 
              mutate(
                     rain_m1 = mean(rain_m1),
                     rain_m2 = mean(rain_m2),
                     rain_m3 = mean(rain_m3),
                     rain_m4 = mean(rain_m4),
                     rain_m5 = mean(rain_m5),
                     rain_total = mean(rain_total)
                     ) %>% 
              filter(row_number()==1) %>% 
              ungroup()
  tempMax <- tempMax %>% 
              group_by(shrid) %>% 
              mutate(
                     tmax_m1 = mean(tmax_m1),
                     tmax_m2 = mean(tmax_m2),
                     tmax_m3 = mean(tmax_m3),
                     tmax_m4 = mean(tmax_m4),
                     tmax_m5 = mean(tmax_m5),
                     tmax_mean = mean(tmax_mean)
                     ) %>% 
              filter(row_number()==1) %>% 
              ungroup()
  tempMin <- tempMin %>% 
              group_by(shrid) %>% 
              mutate(
                     tmin_m1 = mean(tmin_m1),
                     tmin_m2 = mean(tmin_m2),
                     tmin_m3 = mean(tmin_m3),
                     tmin_m4 = mean(tmin_m4),
                     tmin_m5 = mean(tmin_m5),
                     tmin_mean = mean(tmin_mean)
                     ) %>% 
              filter(row_number()==1) %>% 
              ungroup()
  extracted_values_winter <- extracted_values_winter %>% left_join(rainfall, by = "shrid")
  extracted_values_winter <- extracted_values_winter %>% left_join(tempMax, by = "shrid")
  extracted_values_winter <- extracted_values_winter %>% left_join(tempMin, by = "shrid")
  
  
  extracted_values <- rbind(extracted_values, extracted_values_winter)
  write_csv(extracted_values, paste0("data/clean/ag_productivity/rain_merged_both", year, ".csv"))
}


# Now let's merge these with the pollution data
for (year in 2002:2013){
  # Monsoon is this year only
  village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year, "m1.csv")) %>% as_tibble()
  villages <- village_wind %>% dplyr::select(shrid)
  month_temp <- 0
  for (month in 5:10){
    village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month_temp))
    villages <- villages %>% left_join(village_wind, by = "shrid")
    month_temp <- month_temp + 1
  }
  villages$season <- "monsoon"
  villages <- villages %>% mutate(days_sums = rowSums(villages[,3:7]))
  # Just make sure (should already be named this)
  colnames(villages) <- c("shrid", "m0", "m1", "m2", "m3", "m4", "m5", "season", "days_sums")
  
  # Now values for plants NOT OPEN
  village_wind <- read_csv(paste0("data/clean/wind_ntl_NOT/months/y", year, "m1.csv")) %>% as_tibble()
  villages_TEMP <- village_wind %>% dplyr::select(shrid)
  month_temp <- 0
  for (month in 5:10){
    village_wind <- read_csv(paste0("data/clean/wind_ntl_NOT/months/y", year, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month_temp, "NOT"))
    villages_TEMP <- villages_TEMP %>% left_join(village_wind, by = "shrid")
    month_temp <- month_temp + 1
  }
  villages_TEMP$season <- "monsoon"
  villages_TEMP <- villages_TEMP %>% mutate(days_sums_NOT = rowSums(villages[,3:7]))
  # Just make sure (should already be named this)
  colnames(villages_TEMP) <- c("shrid", "m0_NOT", "m1_NOT", "m2_NOT", "m3_NOT", "m4_NOT", "m5_NOT", "season", "days_sums_NOT")
  
  # join with villages
  villages <- villages %>% left_join(villages_TEMP, by = c("shrid", "season"))
  
  
  # Winter season
  villages2 <- villages %>% dplyr::select(shrid)
  month_temp <- 0
  for (month in 10:12){
    village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month_temp))
    villages2 <- villages2 %>% left_join(village_wind, by = "shrid")
    month_temp <- month_temp + 1
  }
  for (month in 1:3){
    village_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year + 1, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month_temp))
    villages2 <- villages2 %>% left_join(village_wind, by = "shrid")
    month_temp <- month_temp + 1
  }
  villages2$season <- "winter"
  villages2 <- villages2 %>% mutate(days_sums = rowSums(villages2[,3:7]))
  # Just make sure (should already be named this)
  colnames(villages2) <- c("shrid", "m0", "m1", "m2", "m3", "m4", "m5", "season", "days_sums")
  
  # Now values for plants NOT YET OPEN
  villages_TEMP <- villages %>% dplyr::select(shrid)
  month_temp <- 0
  for (month in 10:12){
    village_wind <- read_csv(paste0("data/clean/wind_ntl_NOT/months/y", year, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month_temp, "NOT"))
    villages_TEMP <- villages_TEMP %>% left_join(village_wind, by = "shrid")
    month_temp <- month_temp + 1
  }
  for (month in 1:3){
    village_wind <- read_csv(paste0("data/clean/wind_ntl_NOT/months/y", year + 1, "m", month, ".csv")) %>% as_tibble()
    colnames(village_wind) <- c("shrid", paste0("m", month_temp, "NOT"))
    villages_TEMP <- villages_TEMP %>% left_join(village_wind, by = "shrid")
    month_temp <- month_temp + 1
  }
  villages_TEMP$season <- "winter"
  villages_TEMP <- villages_TEMP %>% mutate(days_sums = rowSums(villages_TEMP[,3:7]))
  # Just make sure (should already be named this)
  colnames(villages_TEMP) <- c("shrid", "m0_NOT", "m1_NOT", "m2_NOT", "m3_NOT", "m4_NOT", "m5_NOT", "season", "days_sums_NOT")
  
  # join with villages2
  villages2 <- villages2 %>% left_join(villages_TEMP, by = c("shrid", "season"))
  
  # rbind vilalges and villages2
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



# harvested area
crops_all <- as_tibble(villages) %>% dplyr::select("shrid")
crops <- c("rice", "wheat", "maize", "sugarcane", "cotton", "sorghum", "millet", "groundnut", "bean")
c <- crops[1]
temp <- raster(paste0("data/raw/croparea/GeoTiff/", c, "/", c, "_HarvestedAreaFraction.tif"))
temp <- exact_extract(temp, villages, fun = "mean", append_cols = "shrid")
crops_all$most_common <- paste0(c)
crops_all$most_common_area <- temp$mean
crops_all <- crops_all %>%
              left_join(temp, by = c("shrid"))
colnames(crops_all)[ncol(crops_all)] <- paste0("area_", c)
for (c in crops[2:length(crops)]){
  temp <- raster(paste0("data/raw/croparea/GeoTiff/", c, "/", c, "_HarvestedAreaFraction.tif"))
  temp <- exact_extract(temp, villages, fun = "mean", append_cols = "shrid")
  colnames(temp)[2] <- paste0("area_", c)
  crops_all <- crops_all %>%
                left_join(temp, by = c("shrid"))
  # replace most common crop if area of new crop is higher than previous highest
  crops_all$most_common[crops_all[,ncol(crops_all)]>crops_all$most_common_area] <- paste0(c)
  crops_all$most_common_area[crops_all[,ncol(crops_all)]>crops_all$most_common_area] <- crops_all[,ncol(crops_all)][crops_all[,ncol(crops_all)]>crops_all$most_common_area]
}
# get proportion of area in each village that belongs to each crop
crops_all$area_tot <- apply(crops_all[4:ncol(crops_all)], 1, sum)
crops_all[4:(ncol(crops_all)-1)] <- crops_all[4:(ncol(crops_all)-1)]/crops_all$area_tot
crops_all <- crops_all %>% dplyr::select(-c("area_tot"))
write_csv(crops_all, paste0("data/clean/ag_productivity/crop_area.csv"))



villages_shp <- villages %>%
                  left_join(crops_all, by = c("shrid"))
ggplot(villages_shp) +
  geom_sf(aes(fill = most_common), lwd = 0) +
  scale_fill_brewer("Most common crop\nby area harvested", palette = "Set3") +
  theme_minimal()
ggsave("pollution_development/draft/tables/most_common_crop.png", bg = "white")



