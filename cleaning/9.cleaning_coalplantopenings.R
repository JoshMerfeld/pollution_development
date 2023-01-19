# Purpose: This script cleans the data for looking at where coal plants open
# Author: Josh Merfeld
# Date: December 12th, 2022

rm(list = ls())

library(tidyverse)
library(sf)
library(rgeos)
library(sp)
library(rgdal)
library(raster)
library(openxlsx)
library(exactextractr)  


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check


# Shapefile
villages <- read_sf("data/spatial/shapefiles/village.shp")
villages <- villages %>% mutate(shrid = paste0(pc11_s_id, "-", pc11_tv_id)) %>% dplyr::select(shrid)

# Extract productivity just for FIRST season
r <- raster(paste0("data/raw/ag_productivity/NGPPCY-IN-M2002.tif"))
# extract
monsoon <- exact_extract(r, villages, fun = "mean", append_cols = "shrid")
write_csv(monsoon, "data/clean/census_plants/monsoon2002.csv")
rm(r, monsoon)



# centroids
centroids <- gCentroid( as_Spatial(villages), byid = TRUE )
# Project to meters
centroids <- st_transform(st_as_sf(centroids), "EPSG:24378")

# coal plants
plants <- read.xlsx("data/raw/coal_plants.xlsx", sheet = "Units")
# India only
plants <- plants %>% filter(Country=="India")
# Also want only things with a non-missing year
plants <- plants %>% filter(is.na(Year)==F)

# Just keep what we want 
plants <- plants %>% dplyr::select(plant_id = ParentID, 
                                   unit_id = Tracker.ID, 
                                   capacity = `Capacity.(MW)`, 
                                   year_built = Year, 
                                   year_retired = RETIRED, 
                                   lat = Latitude, 
                                   lon = Longitude)
# Plants exist IN 1991
plants90 <- plants %>% filter(year_built<1991 & (year_retired>1991 | is.na(year_retired)==T))
gps <- plants90 %>% dplyr::select(lon, lat)
plants90 <- st_as_sf(SpatialPointsDataFrame(gps, plants90 %>% dplyr::select(-c(lon, lat)), proj4string = CRS("EPSG:4326")))
plants90$plant90 <- "yes"
plants90 <- plants90 %>% select(plant90)
plants90 <- st_transform(plants90, "EPSG:24378")
# Plants exist IN 2001
plants00 <- plants %>% filter(year_built<2001 & (year_retired>2001 | is.na(year_retired)==T))
gps <- plants00 %>% dplyr::select(lon, lat)
plants00 <- st_as_sf(SpatialPointsDataFrame(gps, plants00 %>% dplyr::select(-c(lon, lat)), proj4string = CRS("EPSG:4326")))
plants00$plant00 <- "yes"
plants00 <- plants00 %>% select(plant00)
plants00 <- st_transform(plants00, "EPSG:24378")
# Plants exist IN 2011
plants10 <- plants %>% filter(year_built<2011 & (year_retired>2011 | is.na(year_retired)==T))
gps <- plants10 %>% dplyr::select(lon, lat)
plants10 <- st_as_sf(SpatialPointsDataFrame(gps, plants10 %>% dplyr::select(-c(lon, lat)), proj4string = CRS("EPSG:4326")))
plants10$plant10 <- "yes"
plants10 <- plants10 %>% select(plant10)
plants10 <- st_transform(plants10, "EPSG:24378")

# buffer of 30km around plants
plants90 <- gBuffer( as_Spatial(plants90), width = 30000, byid = TRUE )
plants90 <- st_as_sf(plants90)
plants00 <- gBuffer( as_Spatial(plants00), width = 30000, byid = TRUE )
plants00 <- st_as_sf(plants00)
plants10 <- gBuffer( as_Spatial(plants10), width = 30000, byid = TRUE )
plants10 <- st_as_sf(plants10)

# Now let's figure out which centroids are within the 30km buffer
centroids_intersects <- st_intersects(centroids, plants90)
centroids_intersects <- (lengths(centroids_intersects) > 0)==T 
# Just keep those that fall within 30km (replacing the centroids_villages_30km object)
villages_plants90 <- villages[centroids_intersects,]
# 2001
centroids_intersects <- st_intersects(centroids, plants00)
centroids_intersects <- (lengths(centroids_intersects) > 0)==T 
# Just keep those that fall within 30km (replacing the centroids_villages_30km object)
villages_plants00 <- villages[centroids_intersects,]
# 2011
centroids_intersects <- st_intersects(centroids, plants10)
centroids_intersects <- (lengths(centroids_intersects) > 0)==T 
# Just keep those that fall within 30km (replacing the centroids_villages_30km object)
villages_plants10 <- villages[centroids_intersects,]

# A little cleaning.
villages_plants90 <- as_tibble(villages_plants90) %>% select(shrid) %>% mutate(plants90 = "yes")
villages_plants00 <- as_tibble(villages_plants00) %>% select(shrid) %>% mutate(plants00 = "yes")
villages_plants10 <- as_tibble(villages_plants10) %>% select(shrid) %>% mutate(plants10 = "yes")




# 1991 census
villages90 <- as_tibble(read_csv("data/raw/census_data/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc91.csv"))
villages90 <- villages90 %>% mutate(shrid = substr(shrid, 4, str_length(shrid)))
# 2001 census
villages00 <- as_tibble(read_csv("data/raw/census_data/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc01.csv"))
villages00 <- villages00 %>% mutate(shrid = substr(shrid, 4, str_length(shrid)))


villages90 <- villages90 %>% left_join(villages_plants90, by = "shrid")
villages90 <- villages90 %>% left_join(villages_plants00, by = "shrid")
villages90 <- villages90 %>% left_join(villages_plants10, by = "shrid")
villages90 <- villages90 %>% mutate(
                                    plants90 = as.numeric(is.na(plants90)==F),
                                    plants00 = as.numeric(is.na(plants00)==F),
                                    plants10 = as.numeric(is.na(plants10)==F)
                                    )
villages00 <- villages00 %>% left_join(villages_plants90, by = "shrid")
villages00 <- villages00 %>% left_join(villages_plants00, by = "shrid")
villages00 <- villages00 %>% left_join(villages_plants10, by = "shrid")
villages00 <- villages00 %>% mutate(
                                    plants90 = as.numeric(is.na(plants90)==F),
                                    plants00 = as.numeric(is.na(plants00)==F),
                                    plants10 = as.numeric(is.na(plants10)==F)
                                    )

# Save
write_csv(villages90, "data/clean/census_plants/villages90.csv")
write_csv(villages00, "data/clean/census_plants/villages00.csv")







