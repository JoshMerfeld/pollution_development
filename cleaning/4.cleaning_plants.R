# Purpose: This script cleans the coalplant location data
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


### Load raw data ----------------------------------------------------------------------------------------------------------------------------------------------------------
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


gps_points_plants <- plants %>% dplyr::select(lon, lat)
plants <- st_as_sf(SpatialPointsDataFrame(gps_points_plants, plants %>% dplyr::select(-c(lon, lat)), proj4string = CRS("EPSG:4326")))
plants$year_built <- as.numeric(plants$year_built)
plants$year_retired <- as.numeric(plants$year_retired)

# Add villages
villages <- read_sf("data/spatial/shapefiles/village.shp")
villages$shrid <- paste(villages$pc11_s_id, villages$pc11_tv_id, sep = "-")

# Also want to transform both to a better crs for calculating distances
plants <- st_transform(plants, "EPSG:24378")
villages <- st_transform(villages, "EPSG:24378")

# Create buffer of 10 km around the plants
buffer10km <- gBuffer( as_Spatial(plants), width = 10000, byid = TRUE )
buffer10km <- st_as_sf(buffer10km)
# Create buffer of 30 km around the plants
buffer30km <- gBuffer( as_Spatial(plants), width = 30000, byid = TRUE )
buffer30km <- st_as_sf(buffer30km)
# Create buffer of 100 km around the plants
buffer100km <- gBuffer( as_Spatial(plants), width = 100000, byid = TRUE )
buffer100km <- st_as_sf(buffer100km)

# Create centroids of villages
centroids_villages <- gCentroid( as_Spatial(villages), byid = TRUE )
centroids_villages <- st_as_sf(centroids_villages)
centroids_villages$pc11_s_id <- NA
centroids_villages$pc11_d_id <- NA
centroids_villages$pc11_sd_id <- NA
centroids_villages$pc11_tv_id <- NA
centroids_villages$tv_name <- NA
centroids_villages$shrid <- villages$shrid

# Now let's figure out which are within the 30km buffer
centroids_villages_30km <- st_intersects(centroids_villages, buffer30km)
village_within_30km <- (lengths(centroids_villages_30km) > 0)==T 

# Just keep those that fall within 30km (replacing the centroids_villages_30km object)
centroids_villages_30km <- centroids_villages[village_within_30km,]

# Now let's figure out which are within the 100km buffer
centroids_villages_100km <- st_intersects(centroids_villages, buffer100km)
village_within_100km <- (lengths(centroids_villages_100km) > 0)==T 

# Just keep those that fall within 100km (replacing the centroids_villages_100km object)
centroids_villages_100km <- centroids_villages[village_within_100km,]


# Save villages
st_write(centroids_villages_30km, "data/spatial/centroids_villages_30km", 
           driver = "ESRI Shapefile", append = F)
st_write(centroids_villages_100km, "data/spatial/centroids_villages_100km", 
           driver = "ESRI Shapefile", append = F)



####### Loaded villages and plants into QGIS to do distances (much faster than doing it here)
dist_matrix <- read.csv(paste0("data/clean/wind_ntl/distances.csv"))

# Only keep the rows where distance is within 30km
dist_matrix <- as_tibble(dist_matrix[dist_matrix$Distance<=30000,])
colnames(dist_matrix) <- c("shrid", "plant_id", "distance")

# Merge in coordinates
centroids_villages_30km <- centroids_villages_30km %>% dplyr::select(shrid)
centroids_villages_30km$x_vil <- NA
centroids_villages_30km$y_vil <- NA
for (row in 1:nrow(centroids_villages_30km)){
  centroids_villages_30km$x_vil[row] = extent(centroids_villages_30km[row,])[1]
  centroids_villages_30km$y_vil[row] = extent(centroids_villages_30km[row,])[3]
  print(row/nrow(centroids_villages_30km))
}
# And plants
plants$x_plants <- NA
plants$y_plants <- NA
for (row in 1:nrow(plants)){
  plants$x_plants[row] = extent(plants[row,])[1]
  plants$y_plants[row] = extent(plants[row,])[3]
  print(row/nrow(plants))
}
# And join
merging <- as_tibble(centroids_villages_30km) %>% dplyr::select(shrid, x_vil, y_vil)
merging <- merging %>%
            group_by(shrid) %>%
            filter(row_number()==1) %>%
            ungroup()
dist_matrix <- dist_matrix %>% left_join(
                                         merging,
                                         by = "shrid"
                                         )
# Plants
merging <- as_tibble(plants) %>% dplyr::select(plant_id, x_plants, y_plants, year_built, year_retired)
# Keep only one per "plant_id"
merging <- merging %>% group_by(plant_id) %>% mutate(
                                                     year_built = min(year_built),
                                                     year_retired = ifelse(is.na(year_retired)==T, 2030, year_retired),
                                                     year_retired = max(year_retired)
                                                     ) %>%
                                              filter(row_number()==1) %>%
                                              ungroup()
dist_matrix <- dist_matrix %>%  left_join(
                                          merging,
                                          by = "plant_id"
                                          )
head(dist_matrix)


######
# Create matrix with rows equal to villages and columns equal to plants. Create angle 
# Let's turn it into degrees (instead of radians)
rad_deg <- function(rad){
  deg <- (rad * 180) / (pi)
  return(deg)
}
# Note that this is the angle FROM THE PLANT TO THE VILLAGE, with 0 being directly north (and going clockwise)
dist_matrix <- dist_matrix %>% mutate(angle_original = rad_deg(
                                                                atan2(
                                                                      x_vil - x_plants,
                                                                      y_vil - y_plants
                                                                      )
                                                                )
                                                )
dist_matrix$angle <- dist_matrix$angle_original
dist_matrix$angle[dist_matrix$angle<0] <- dist_matrix$angle[dist_matrix$angle<0] + 360

dist_matrix <- dist_matrix %>% dplyr::select(-c("x_vil", "y_vil", "x_plants", "y_plants"))
write.csv(dist_matrix, paste0("data/clean/wind_ntl/dist_matrix_angles.csv"))

# Takes a while to get here. Save so we can start here if we want
dist_matrix <- read.csv(paste0("data/clean/wind_ntl/dist_matrix_angles.csv"))
centroids_villages_30km <- st_read("data/spatial/centroids_villages_30km")



# Wind directions by day ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Now load the tables from GEE, which are hourly wind directions
# Add india
india_wind <- read_sf("data/spatial/shapefiles/state.shp")

rad_deg <- function(rad){
  deg <- (rad * 180) / (pi)
  return(deg)
}

# GET PREVIOUS SEVEN DAYS (before start of loop)
# Date
date <- as.Date("1990-01-01", "%Y-%m-%d")
final_date <- as.Date("2015-03-10", "%Y-%m-%d")
# date <- as.Date("2014-01-01", "%Y-%m-%d")
# final_date <- as.Date("2017-12-31", "%Y-%m-%d")

wind_last_week <- dist_matrix %>% dplyr::select("shrid", "plant_id", "angle", "angle_original", "year_built", "year_retired")
wind_last_week <- wind_last_week %>% group_by(shrid) %>% filter(row_number()==1) %>% ungroup()
wind_last_week_ALL <- wind_last_week
for (lag in 1:7){
  wind_last_week_temp <- wind_last_week
  wind_last_week_temp$sum <- 0
  
  dl_year <- year(date - lag)
  dl_month <- month(date - lag)
  dl_day <- day(date - lag)
  if (str_length(dl_month)==1){
    dl_month <- paste0("0", dl_month)
  }
  if (str_length(dl_day)==1){
    dl_day <- paste0("0", dl_day)
  }
  temp_nc1 <- fs::file_temp(ext = ".nc.gz")
  url1 <- paste0("https://data.remss.com/ccmp/v02.0/Y", dl_year, "/M", dl_month, "/CCMP_Wind_Analysis_", dl_year, dl_month, dl_day, "_V02.0_L3.0_RSS.nc")
  tries <- NULL
  while (is.null(tries)){
    try(
      tries <- download.file(url1, destfile = temp_nc1, mode = "wb")
    )
  }
  nc_path1 <- gunzip(temp_nc1)
  data <- ncdf4::nc_open(nc_path1)
  
  
  # Keep only those that cover India
  lon_start <- max(which( data$dim$lon$vals<=extent(india_wind)[1] ))
  lon_count <- min(which( data$dim$lon$vals>=extent(india_wind)[2] )) - lon_start + 1
  lat_start <- max(which( data$dim$lat$vals<=extent(india_wind)[3] ))
  lat_count <- min(which( data$dim$lat$vals>=extent(india_wind)[4] )) - lat_start + 1
  
  # Extract values
  east <- ncvar_get(data, "uwnd", start = c(lon_start, lat_start, 1), count = c(lon_count, lat_count, 4))
  north <- ncvar_get(data, "vwnd", start = c(lon_start, lat_start, 1), count = c(lon_count, lat_count, 4))
  direction_original <- rad_deg(
                               atan2(
                                     east,
                                     north
                                    )
                              )
  direction <- direction_original
  # Finally, if negative add 360
  direction[direction<0] <- direction[direction<0] + 360
  # Get lon and lat values of cells
  lon <- data$dim$longitude$vals[lon_start:(lon_start + lon_count - 1)]
  lat <- data$dim$latitude$vals[lat_start:(lat_start + lat_count - 1)]
  r <- brick(
              direction,
              xmn = range(lon)[1], xmx = range(lon)[2],
              ymn = range(lat)[1], ymx = range(lat)[2],
              crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
              )
  
  r2 <- brick(
              direction_original,
              xmn = range(lon)[1], xmx = range(lon)[2],
              ymn = range(lat)[1], ymx = range(lat)[2],
              crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
              )
  
  r <- projectRaster(
                     r,
                     crs = crs(plants)
                     )
  r2 <- projectRaster(
                     r2,
                     crs = crs(plants)
                     )
  
  extracted_values <- as_tibble(cbind(raster::extract(r, plants), plants$plant_id))
  extracted_values_original <- as_tibble(cbind(raster::extract(r2, plants), plants$plant_id))
  colnames(extracted_values) <- c("plant_angle1", "plant_angle2", "plant_angle3", "plant_angle4", "plant_id")
  extracted_values <- extracted_values %>% group_by(plant_id) %>% filter(row_number()==1) %>% ungroup()
  colnames(extracted_values_original) <- c("plant_angle_original1", "plant_angle_original2", "plant_angle_original3", "plant_angle_original4", "plant_id")
  extracted_values_original <- extracted_values_original %>% group_by(plant_id) %>% filter(row_number()==1) %>% ungroup()
  extracted_values <- extracted_values %>% mutate(
                                                  plant_angle1 = as.numeric(plant_angle1),
                                                  plant_angle2 = as.numeric(plant_angle2),
                                                  plant_angle3 = as.numeric(plant_angle3),
                                                  plant_angle4 = as.numeric(plant_angle4)
                                                  )
  extracted_values_original <- extracted_values_original %>% mutate(
                                                                    plant_angle_original1 = as.numeric(plant_angle_original1),
                                                                    plant_angle_original2 = as.numeric(plant_angle_original2),
                                                                    plant_angle_original3 = as.numeric(plant_angle_original3),
                                                                    plant_angle_original4 = as.numeric(plant_angle_original4)
                                                                    )
  
  # Only those plants in existence at the time
  #wind_last_week_temp <- wind_last_week_temp %>% filter(dl_year>=wind_last_week_temp$year_built & dl_year<=wind_last_week_temp$year_retired)
  
  wind_last_week_temp <- wind_last_week_temp %>% left_join(extracted_values, by = "plant_id") 
  wind_last_week_temp <- wind_last_week_temp %>% left_join(extracted_values_original, by = "plant_id") 
  
  # Add one when meeting conditions
  condition <- wind_last_week_temp$angle>=5 & wind_last_week_temp$angle<=355
  
  # figure out how often pointing at vilalge
  wind_last_week_temp$sum[condition] <- wind_last_week_temp$sum[condition] + 
                                (as.numeric(
                                          (wind_last_week_temp$plant_angle1[condition]>=wind_last_week_temp$angle[condition] - 5) 
                                          &
                                          (wind_last_week_temp$plant_angle1[condition]<=wind_last_week_temp$angle[condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_last_week_temp$plant_angle2[condition]>=wind_last_week_temp$angle[condition] - 5) 
                                          &
                                          (wind_last_week_temp$plant_angle2[condition]<=wind_last_week_temp$angle[condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_last_week_temp$plant_angle3[condition]>=wind_last_week_temp$angle[condition] - 5) 
                                          &
                                          (wind_last_week_temp$plant_angle3[condition]<=wind_last_week_temp$angle[condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_last_week_temp$plant_angle4[condition]>=wind_last_week_temp$angle[condition] - 5) 
                                          &
                                          (wind_last_week_temp$plant_angle4[condition]<=wind_last_week_temp$angle[condition] + 5)
                                          ))/4
  wind_last_week_temp$sum[!condition] <- wind_last_week_temp$sum[!condition] + 
                                (as.numeric(
                                          (wind_last_week_temp$plant_angle_original1[!condition]>=wind_last_week_temp$angle_original[!condition] - 5) 
                                          &
                                          (wind_last_week_temp$plant_angle_original1[!condition]<=wind_last_week_temp$angle_original[!condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_last_week_temp$plant_angle_original2[!condition]>=wind_last_week_temp$angle_original[!condition] - 5) 
                                          &
                                          (wind_last_week_temp$plant_angle_original2[!condition]<=wind_last_week_temp$angle_original[!condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_last_week_temp$plant_angle_original3[!condition]>=wind_last_week_temp$angle_original[!condition] - 5) 
                                          &
                                          (wind_last_week_temp$plant_angle_original3[!condition]<=wind_last_week_temp$angle_original[!condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_last_week_temp$plant_angle_original4[!condition]>=wind_last_week_temp$angle_original[!condition] - 5) 
                                          &
                                          (wind_last_week_temp$plant_angle_original4[!condition]<=wind_last_week_temp$angle_original[!condition] + 5)
                                          ))/4
  
  # First do ALL
  wind_last_week_temp2 <- wind_last_week_temp %>% group_by(shrid) %>% mutate(sum = max(sum)) %>% filter(row_number()==1) %>% ungroup() %>% dplyr::select("shrid", "sum")
  wind_last_week_merge <- as_tibble(wind_last_week[,1])
  colnames(wind_last_week_merge) <- "shrid"
  wind_last_week_merge <- wind_last_week_merge %>% left_join(wind_last_week_temp2, by = "shrid")
  wind_last_week_merge$sum[is.na(wind_last_week_merge$sum)==T] <- 0
  wind_last_week_ALL[, 6 + lag] <- wind_last_week_merge$sum
  
  # now those during years coal plant actually in operation
  wind_last_week_temp <- wind_last_week_temp[dl_year>=wind_last_week_temp$year_built & dl_year<=wind_last_week_temp$year_retired,]
  wind_last_week_temp <- wind_last_week_temp %>% group_by(shrid) %>% mutate(sum = max(sum)) %>% filter(row_number()==1) %>% ungroup() %>% dplyr::select("shrid", "sum")
  wind_last_week_merge <- as_tibble(wind_last_week[,1])
  colnames(wind_last_week_merge) <- "shrid"
  wind_last_week_merge <- wind_last_week_merge %>% left_join(wind_last_week_temp, by = "shrid")
  wind_last_week_merge$sum[is.na(wind_last_week_merge$sum)==T] <- 0
  wind_last_week[, 6 + lag] <- wind_last_week_merge$sum
}
write_csv(wind_last_week_ALL[, c(1, 7:ncol(wind_last_week))], paste0("data/clean/wind_ntl_NOT/days/date_", year(date), "-", month(date), "-", day(date), ".csv"))
write_csv(wind_last_week[, c(1, 7:ncol(wind_last_week))], paste0("data/clean/wind_ntl/days/date_", year(date), "-", month(date), "-", day(date), ".csv"))




# Now do all the others until we get to the final date (defined above)
while (date<=final_date){
  year <- year(date)
  month <- month(date)
  day <- day(date)
  # Need strings of length two (because of the URL)
  if (str_length(month)==1){
    month <- paste0("0", month)
  }
  if (str_length(day)==1){
    day <- paste0("0", day)
  }
  
  # At the start of the year:
  # if (month=="01" & day=="01"){
  #   days_year <- as.double(1)
  #   
  #   # new matrix that is just dist_matrix
  #   wind_year_sums <- as_tibble(unique(dist_matrix$shrid))
  #   colnames(wind_year_sums) <- "shrid"
  #   wind_year_sums$sum_all <- as.double(0)
  # }
  
  wind_year <- dist_matrix %>% dplyr::select("shrid", "plant_id", "angle", "angle_original", "year_built", "year_retired")
  wind_year$sum <- 0

  # Download (https://climatedataguide.ucar.edu/climate-data/ccmp-cross-calibrated-multi-platform-wind-vector-analysis)
  temp_nc1 <- fs::file_temp(ext = ".nc.gz")
  url1 <- paste0("https://data.remss.com/ccmp/v02.0/Y", year, "/M", month, "/CCMP_Wind_Analysis_", year, month, day, "_V02.0_L3.0_RSS.nc")
  
  tries <- NULL
  while (is.null(tries)){
    try(
      tries <- download.file(url1, destfile = temp_nc1, mode = "wb")
    )
  }
  nc_path1 <- gunzip(temp_nc1)
  data <- ncdf4::nc_open(nc_path1)
  
  # Keep only those that cover India
  lon_start <- max(which( data$dim$lon$vals<=extent(india_wind)[1] ))
  lon_count <- min(which( data$dim$lon$vals>=extent(india_wind)[2] )) - lon_start + 1
  lat_start <- max(which( data$dim$lat$vals<=extent(india_wind)[3] ))
  lat_count <- min(which( data$dim$lat$vals>=extent(india_wind)[4] )) - lat_start + 1
  
  east <- ncvar_get(data, "uwnd", start = c(lon_start, lat_start, 1), count = c(lon_count, lat_count, 4))
  north <- ncvar_get(data, "vwnd", start = c(lon_start, lat_start, 1), count = c(lon_count, lat_count, 4))
  direction_original <- rad_deg(
                               atan2(
                                     east,
                                     north
                                    )
                              )
  direction <- direction_original
  # Finally, if negative add 360
  direction[direction<0] <- direction[direction<0] + 360
  # Get lon and lat values of cells
  lon <- data$dim$longitude$vals[lon_start:(lon_start + lon_count - 1)]
  lat <- data$dim$latitude$vals[lat_start:(lat_start + lat_count - 1)]
  r <- brick(
              direction,
              xmn = range(lon)[1], xmx = range(lon)[2],
              ymn = range(lat)[1], ymx = range(lat)[2],
              crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
              )
  
  r2 <- brick(
              direction_original,
              xmn = range(lon)[1], xmx = range(lon)[2],
              ymn = range(lat)[1], ymx = range(lat)[2],
              crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
              )
  
  r <- projectRaster(
                     r,
                     crs = crs(plants)
                     )
  r2 <- projectRaster(
                     r2,
                     crs = crs(plants)
                     )
  
  extracted_values <- as_tibble(cbind(raster::extract(r, plants), plants$plant_id))
  extracted_values_original <- as_tibble(cbind(raster::extract(r2, plants), plants$plant_id))
  colnames(extracted_values) <- c("plant_angle1", "plant_angle2", "plant_angle3", "plant_angle4", "plant_id")
  extracted_values <- extracted_values %>% group_by(plant_id) %>% filter(row_number()==1) %>% ungroup()
  colnames(extracted_values_original) <- c("plant_angle_original1", "plant_angle_original2", "plant_angle_original3", "plant_angle_original4", "plant_id")
  extracted_values_original <- extracted_values_original %>% group_by(plant_id) %>% filter(row_number()==1) %>% ungroup()
  
  # Only those plants in existence at the time
  #wind_year <- wind_year %>% filter(year>=wind_year$year_built & year<=wind_year$year_retired)
  
  wind_year <- wind_year %>% left_join(extracted_values, by = "plant_id") 
  wind_year <- wind_year %>% left_join(extracted_values_original, by = "plant_id") 
  
  # Add one when meeting conditions
  condition <- wind_year$angle>=5 & wind_year$angle<=355
  
  # figure out whether ANY are a one for a village
  wind_year$sum[condition] <- wind_year$sum[condition] + 
                                (as.numeric(
                                          (wind_year$plant_angle1[condition]>=wind_year$angle[condition] - 5) 
                                          &
                                          (wind_year$plant_angle1[condition]<=wind_year$angle[condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_year$plant_angle2[condition]>=wind_year$angle[condition] - 5) 
                                          &
                                          (wind_year$plant_angle2[condition]<=wind_year$angle[condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_year$plant_angle3[condition]>=wind_year$angle[condition] - 5) 
                                          &
                                          (wind_year$plant_angle3[condition]<=wind_year$angle[condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_year$plant_angle4[condition]>=wind_year$angle[condition] - 5) 
                                          &
                                          (wind_year$plant_angle4[condition]<=wind_year$angle[condition] + 5)
                                          ))/4
  wind_year$sum[!condition] <- wind_year$sum[!condition] + 
                                (as.numeric(
                                          (wind_year$plant_angle_original1[!condition]>=wind_year$angle_original[!condition] - 5) 
                                          &
                                          (wind_year$plant_angle_original1[!condition]<=wind_year$angle_original[!condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_year$plant_angle_original2[!condition]>=wind_year$angle_original[!condition] - 5) 
                                          &
                                          (wind_year$plant_angle_original2[!condition]<=wind_year$angle_original[!condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_year$plant_angle_original3[!condition]>=wind_year$angle_original[!condition] - 5) 
                                          &
                                          (wind_year$plant_angle_original3[!condition]<=wind_year$angle_original[!condition] + 5)
                                          ) + 
                                as.numeric(
                                          (wind_year$plant_angle_original4[!condition]>=wind_year$angle_original[!condition] - 5) 
                                          &
                                          (wind_year$plant_angle_original4[!condition]<=wind_year$angle_original[!condition] + 5)
                                          ))/4
  
  
  temp <- wind_year %>% group_by(shrid) %>% mutate(sum = max(sum)) %>% filter(row_number()==1) %>% ungroup() %>% dplyr::select("shrid", "sum")
  # wind_year_sums <- wind_year_sums %>% left_join(temp, by = "shrid")
  # wind_year_sums$sum[is.na(wind_year_sums$sum)==T] <- 0  # 0 if missing (might not be in a specific year)
  # wind_year_sums$sum_all <- wind_year_sums$sum_all + wind_year_sums$sum
  # wind_year_sums <- wind_year_sums %>% dplyr::select(-c("sum"))
  
  
  # And the daily value
  # FIRST, reassign previous days to next column
  wind_last_week[, 8:13] <- wind_last_week[, 7:12]
  wind_last_week_ALL[, 8:13] <- wind_last_week_ALL[, 7:12]
  
  # ALL
  wind_last_week_merge <- as_tibble(wind_last_week[,1])
  colnames(wind_last_week_merge) <- "shrid"
  wind_last_week_merge <- wind_last_week_merge %>% left_join(temp, by = "shrid")
  wind_last_week_merge$sum[is.na(wind_last_week_merge$sum)==T] <- 0
  
  wind_last_week_ALL[, 7] <- wind_last_week_merge$sum
  
  
  # Only plants in operation
  wind_year <- wind_year[dl_year>=wind_year$year_built & dl_year<=wind_year$year_retired,]
  temp <- wind_year %>% group_by(shrid) %>% mutate(sum = max(sum)) %>% filter(row_number()==1) %>% ungroup() %>% dplyr::select("shrid", "sum")
  wind_last_week_merge <- as_tibble(wind_last_week[,1])
  colnames(wind_last_week_merge) <- "shrid"
  wind_last_week_merge <- wind_last_week_merge %>% left_join(temp, by = "shrid")
  wind_last_week_merge$sum[is.na(wind_last_week_merge$sum)==T] <- 0
  
  wind_last_week[, 7] <- wind_last_week_merge$sum
  # days_year <- days_year + 1
  
  # Add one to date
  date <- date + 1
  # Save values for LAST SEVEN DAYS (this is for the NSS); note that this is saved for the NEXT DAY, not the current day
  write_csv(wind_last_week_ALL[, c(1, 7:ncol(wind_last_week))], 
            paste0("data/clean/wind_ntl_NOT/days/date_", year(date), "-", month(date), "-", day(date), ".csv"))
  write_csv(wind_last_week[, c(1, 7:ncol(wind_last_week))], 
            paste0("data/clean/wind_ntl/days/date_", year(date), "-", month(date), "-", day(date), ".csv"))
  
  print(date)
  # And on the last day of the year...
  # ... divide the total by the number of days in the year (generally will be 365, but leap years is 366)
  # if (month=="12" & day=="31"){
  #   wind_year_sums$sum_all = wind_year_sums$sum_all/as.numeric(days_year)  # divide by total number of days
  #   # And save this month
  #   write_csv(wind_year_sums, paste0("data/clean/wind_ntl/years/y", year, ".csv"))
  # }
}
  





## Aggregating to month/year ------------------------------------------------------------------------------------------
### PHEW! IT'S DONE! Now we need to pull all those daily csvs and aggregate them into months and years.
# From 1990 through 2015
today <- as.Date("2002-01-01", "%Y-%m-%d")
final_date <- as.Date("2015-12-31", "%Y-%m-%d")

while (today<=final_date){
  # First day of month...
  if (day(today)==1){
    month_sum <- dist_matrix %>% dplyr::select(shrid) %>% group_by(shrid) %>% filter(row_number()==1) %>% ungroup
    month_sum <- as_tibble(month_sum)
    month_sum$sums <- 0
    # First day of year...
    if (yday(today)==1){
      year_sum <- dist_matrix %>% dplyr::select(shrid) %>% group_by(shrid) %>% filter(row_number()==1) %>% ungroup
      year_sum$sums <- 0
      year_sum <- as_tibble(year_sum)
      colnames(year_sum) <- c("shrid", "sums")
    }
  }
  
  today_value_ALL <- read_csv(paste0("data/clean/wind_ntl_NOT/days/date_", year(today + 1), "-", month(today + 1), "-", day(today + 1), ".csv"))
  today_value <- read_csv(paste0("data/clean/wind_ntl/days/date_", year(today + 1), "-", month(today + 1), "-", day(today + 1), ".csv"))
# 
#   # Going to set this up to redownload if there is an issue reading the csv
#   today_value <- NULL
#   try(
#     
#   )
#   if (is.null(today_value)==T){
#     wind_last_week <- dist_matrix %>% dplyr::select("shrid", "plant_id", "angle", "angle_original", "year_built", "year_retired")
#     wind_last_week <- wind_last_week %>% group_by(shrid) %>% filter(row_number()==1) %>% ungroup()
#     for (lag in 1:7){
#       wind_last_week_temp <- wind_last_week
#       wind_last_week_temp$sum <- 0
#       
#       # do it for TOMORROW
#       dl_year <- year(today + 1 - lag)
#       dl_month <- month(today + 1 - lag)
#       dl_day <- day(today + 1 - lag)
#       if (str_length(dl_month)==1){
#         dl_month <- paste0("0", dl_month)
#       }
#       if (str_length(dl_day)==1){
#         dl_day <- paste0("0", dl_day)
#       }
#       temp_nc1 <- fs::file_temp(ext = ".nc.gz")
#       url1 <- paste0("https://data.remss.com/ccmp/v02.0/Y", dl_year, "/M", dl_month, "/CCMP_Wind_Analysis_", dl_year, dl_month, dl_day, "_V02.0_L3.0_RSS.nc")
#       download.file(url1, destfile = temp_nc1, mode = "wb")
#       nc_path1 <- gunzip(temp_nc1)
#       data <- ncdf4::nc_open(nc_path1)
#       
#       
#       # Keep only those that cover India
#       lon_start <- max(which( data$dim$lon$vals<=extent(india_wind)[1] ))
#       lon_count <- min(which( data$dim$lon$vals>=extent(india_wind)[2] )) - lon_start + 1
#       lat_start <- max(which( data$dim$lat$vals<=extent(india_wind)[3] ))
#       lat_count <- min(which( data$dim$lat$vals>=extent(india_wind)[4] )) - lat_start + 1
#       
#       east <- ncvar_get(data, "uwnd", start = c(lon_start, lat_start, 1), count = c(lon_count, lat_count, 4))
#       north <- ncvar_get(data, "vwnd", start = c(lon_start, lat_start, 1), count = c(lon_count, lat_count, 4))
#       direction_original <- rad_deg(
#                                    atan2(
#                                          east,
#                                          north
#                                         )
#                                   )
#       direction <- direction_original
#       # Finally, if negative add 360
#       direction[direction<0] <- direction[direction<0] + 360
#       # Get lon and lat values of cells
#       lon <- data$dim$longitude$vals[lon_start:(lon_start + lon_count - 1)]
#       lat <- data$dim$latitude$vals[lat_start:(lat_start + lat_count - 1)]
#       r <- raster(
#                   direction,
#                   xmn = range(lon)[1], xmx = range(lon)[2],
#                   ymn = range(lat)[1], ymx = range(lat)[2],
#                   crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#                   )
#       
#       r2 <- raster(
#                   direction_original,
#                   xmn = range(lon)[1], xmx = range(lon)[2],
#                   ymn = range(lat)[1], ymx = range(lat)[2],
#                   crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#                   )
#       
#       r <- projectRaster(
#                          r,
#                          crs = crs(plants)
#                          )
#       r2 <- projectRaster(
#                          r2,
#                          crs = crs(plants)
#                          )
#       
#       extracted_values <- as_tibble(cbind(raster::extract(r, plants), plants$plant_id))
#       extracted_values_original <- as_tibble(cbind(raster::extract(r2, plants), plants$plant_id))
#       colnames(extracted_values) <- c("plant_angle", "plant_id")
#       extracted_values <- extracted_values %>% group_by(plant_id) %>% filter(row_number()==1) %>% ungroup()
#       colnames(extracted_values_original) <- c("plant_angle_original", "plant_id")
#       extracted_values_original <- extracted_values_original %>% group_by(plant_id) %>% filter(row_number()==1) %>% ungroup()
#       
#       # Only those plants NOT IN EXISTENCE at the time
#       wind_last_week_ALL <- wind_last_week_temp %>% filter(dl_year<wind_last_week_temp$year_built | dl_year>wind_last_week_temp$year_retired)
#       wind_last_week_ALL <- wind_last_week_ALL %>% left_join(extracted_values, by = "plant_id") 
#       wind_last_week_ALL <- wind_last_week_ALL %>% left_join(extracted_values_original, by = "plant_id") 
#       
#       # Only those plants in existence at the time
#       wind_last_week_temp <- wind_last_week_temp %>% filter(dl_year>=wind_last_week_temp$year_built & dl_year<=wind_last_week_temp$year_retired)
#       wind_last_week_temp <- wind_last_week_temp %>% left_join(extracted_values, by = "plant_id") 
#       wind_last_week_temp <- wind_last_week_temp %>% left_join(extracted_values_original, by = "plant_id") 
#       
#       # Add one when meeting conditions
#       condition_ALL <- wind_last_week_ALL$angle>=5 & wind_last_week_ALL$angle<=355
#       condition <- wind_last_week_temp$angle>=5 & wind_last_week_temp$angle<=355
#       
#       # figure out whether ANY are a one for a village
#       wind_last_week_ALL$sum[condition_ALL] <- wind_last_week_ALL$sum[condition_ALL] + 
#                                     as.numeric(
#                                               (wind_last_week_ALL$plant_angle[condition_ALL]>=wind_last_week_ALL$angle[condition_ALL] - 5) 
#                                               &
#                                               (wind_last_week_ALL$plant_angle[condition_ALL]<=wind_last_week_ALL$angle[condition_ALL] + 5)
#                                               )
#       wind_last_week_ALL$sum[!condition_ALL] <- wind_last_week_ALL$sum[!condition_ALL] + 
#                                     as.numeric(
#                                               (wind_last_week_ALL$plant_angle_original[!condition_ALL]>=wind_last_week_ALL$angle_original[!condition_ALL] - 5) 
#                                               &
#                                               (wind_last_week_ALL$plant_angle_original[!condition_ALL]<=wind_last_week_ALL$angle_original[!condition_ALL] + 5)
#                                               )
#       
#       wind_last_week_ALL <- wind_last_week_ALL %>% group_by(shrid) %>% mutate(sum = max(sum)) %>% filter(row_number()==1) %>% ungroup() %>% dplyr::select("shrid", "sum")
#       wind_last_week_merge_ALL <- wind_last_week_ALL[,1]
#       wind_last_week_merge_ALL <- wind_last_week_merge_ALL %>% left_join(wind_last_week_ALL, by = "shrid")
#       wind_last_week_merge_ALL$sum[is.na(wind_last_week_merge_ALL$sum)==T] <- 0
#       
#       wind_last_week_ALL[, 6 + lag] <- wind_last_week_merge_ALL$sum
#       
#       # figure out whether ANY are a one for a village
#       wind_last_week_temp$sum[condition] <- wind_last_week_temp$sum[condition] + 
#                                     as.numeric(
#                                               (wind_last_week_temp$plant_angle[condition]>=wind_last_week_temp$angle[condition] - 5) 
#                                               &
#                                               (wind_last_week_temp$plant_angle[condition]<=wind_last_week_temp$angle[condition] + 5)
#                                               )
#       wind_last_week_temp$sum[!condition] <- wind_last_week_temp$sum[!condition] + 
#                                     as.numeric(
#                                               (wind_last_week_temp$plant_angle_original[!condition]>=wind_last_week_temp$angle_original[!condition] - 5) 
#                                               &
#                                               (wind_last_week_temp$plant_angle_original[!condition]<=wind_last_week_temp$angle_original[!condition] + 5)
#                                               )
#       
#       wind_last_week_temp <- wind_last_week_temp %>% group_by(shrid) %>% mutate(sum = max(sum)) %>% filter(row_number()==1) %>% ungroup() %>% dplyr::select("shrid", "sum")
#       wind_last_week_merge <- wind_last_week[,1]
#       wind_last_week_merge <- wind_last_week_merge %>% left_join(wind_last_week_temp, by = "shrid")
#       wind_last_week_merge$sum[is.na(wind_last_week_merge$sum)==T] <- 0
#       
#       wind_last_week[, 6 + lag] <- wind_last_week_merge$sum
#     }
  #   
  #   write_csv(wind_last_week_ALL[, c(1, 7:ncol(wind_last_week_ALL))], paste0("data/clean/wind_ntl_NOT/days/date_", year(today + 1), "-", month(today + 1), "-", day(today + 1), ".csv"))
  #   today_value_ALL <- wind_last_week_ALL[, c(1, 7:ncol(wind_last_week_ALL))]
  #   
  #   write_csv(wind_last_week[, c(1, 7:ncol(wind_last_week))], paste0("data/clean/wind_ntl/days/date_", year(today + 1), "-", month(today + 1), "-", day(today + 1), ".csv"))
  #   today_value <- wind_last_week[, c(1, 7:ncol(wind_last_week))]
  # }
  
  # ALL
  today_value_ALL <- today_value_ALL[,1:2]
  colnames(today_value_ALL ) <- c("shrid", "today")
  # Okay, so now add to sums
  year_sum_ALL <- year_sum %>% left_join(today_value_ALL, by = "shrid")
  year_sum_ALL <- year_sum_ALL %>% mutate(sums = sums + today)
  year_sum_ALL <- year_sum_ALL %>% dplyr::select("shrid", "sums")
  month_sum_ALL <- month_sum %>% left_join(today_value_ALL, by = "shrid")
  month_sum_ALL <- month_sum_ALL %>% mutate(sums = sums + today)
  month_sum_ALL <- month_sum_ALL %>% dplyr::select("shrid", "sums")
  
  # Saving in last day
  if (day(today)==days_in_month(today)){
    write_csv(month_sum_ALL, paste0("data/clean/wind_ntl_NOT/months/y", year(today), "m", month(today), ".csv"))
    # And year
    if (month(today)==12){
      write_csv(year_sum_ALL, paste0("data/clean/wind_ntl_NOT/years/y", year(today), ".csv"))
    }
  }
  
  # Just years with plant
  today_value <- today_value[,1:2]
  colnames(today_value) <- c("shrid", "today")
  # Okay, so now add to sums
  year_sum <- year_sum %>% left_join(today_value, by = "shrid")
  year_sum <- year_sum %>% mutate(sums = sums + today)
  year_sum <- year_sum %>% dplyr::select("shrid", "sums")
  month_sum <- month_sum %>% left_join(today_value, by = "shrid")
  month_sum <- month_sum %>% mutate(sums = sums + today)
  month_sum <- month_sum %>% dplyr::select("shrid", "sums")
  
  # Saving in last day
  if (day(today)==days_in_month(today)){
    write_csv(month_sum, paste0("data/clean/wind_ntl/months/y", year(today), "m", month(today), ".csv"))
    # And year
    if (month(today)==12){
      write_csv(year_sum, paste0("data/clean/wind_ntl/years/y", year(today), ".csv"))
    }
  }
  
  # And tomorrow!
  print(today)
  today <- today + 1
}







