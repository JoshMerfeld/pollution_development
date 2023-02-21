# Purpose: This script pulls temp and precip
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


# Always using this
url <- "thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/"
# PRECIP --------------------------------------------------------------------
for (year in 1990:2020){
  # This changes by year
  file <- paste0("TerraClimate_ppt_", year, ".nc")
  curl::curl_download(file.path(url, file), file)
  r <- stack(file, varname = "ppt")
  
  extracted_values <- exact_extract(r, villages, fun = "mean", append_cols = "shrid")
  
  write_csv(extracted_values, paste0("data/clean/terra/precip", year, ".csv"))
  file.remove(file)
  print(year)
}
# MAX TEMP --------------------------------------------------------------------
for (year in 1990:2020){
  # This changes by year
  file <- paste0("TerraClimate_tmax_", year, ".nc")
  curl::curl_download(file.path(url, file), file)
  r <- stack(file, varname = "tmax")
  
  extracted_values <- exact_extract(r, villages, fun = "mean", append_cols = "shrid")
  
  write_csv(extracted_values, paste0("data/clean/terra/tmax", year, ".csv"))
  file.remove(file)
  print(year)
}
# MIN TEMP --------------------------------------------------------------------
for (year in 1990:2020){
  # This changes by year
  file <- paste0("TerraClimate_tmin_", year, ".nc")
  curl::curl_download(file.path(url, file), file)
  r <- stack(file, varname = "tmin")
  
  extracted_values <- exact_extract(r, villages, fun = "mean", append_cols = "shrid")
  
  write_csv(extracted_values, paste0("data/clean/terra/tmin", year, ".csv"))
  file.remove(file)
  print(year)
}

ntl_temp <- c()
for (year in 1990:2020){
   tmax <- read_csv(paste0("data/clean/terra/tmax", year, ".csv"))
   tmin <- read_csv(paste0("data/clean/terra/tmin", year, ".csv"))
   tmax[,2:13] <- (tmax[,2:13] + tmin[,2:13])/2
   tmax <- tmax %>%
            mutate(temp = rowMeans(tmax[,2:13]),
                   year = year) %>%
            dplyr::select(shrid, temp, year)
   
   
   ntl_temp <- rbind(ntl_temp, tmax)
}
write_csv(ntl_temp, paste0("data/clean/village_temp.csv"))
  
  