# Purpose: This script cleans the coalplant location data
# Author: Josh Merfeld
# Date: December 12th, 2022

rm(list = ls())

library(sf)
library(sp)
library(raster)
library(rgeos)
library(openxlsx)
library(tidyverse)
library(ncdf4)
library(lubridate)


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

# Add india
india <- read_sf("data/spatial/shapefiles/state.shp")

# Also want to transform both to a better crs for calculating distances
plants <- st_transform(plants, "EPSG:24378")
india <- st_transform(india, "EPSG:24378")

# 
# ggplot() +
#   geom_sf(data = plants, aes(color = year_built)) +
#   scale_color_viridis_c("Year built", option = "magma") +
#   geom_sf(data = india, lwd = 0.25, fill = NA)
# # Looks good


# Now get the GPS locations for VDSA villages
villages <- read.xlsx("data/vdsa/gps_east.xlsx")
villages <- villages %>% dplyr::select(lon = LON, lat = LAT, village = Village, district = District, state = State)
villages2 <- read.xlsx("data/vdsa/gps_sat.xlsx")
villages2 <- villages2 %>% dplyr::select(lon = LON, lat = LAT, village = VILLAGE, district = DISTRICT, state = STATE)
villages <- rbind(villages, villages2)
rm(villages2)

# Now just one observation per village
villages <- villages %>% filter(is.na(lat)==F)
gps_points_villages <- villages %>% dplyr::select(lon, lat)
villages <- st_as_sf(SpatialPointsDataFrame(gps_points_villages, villages %>% dplyr::select(-c(lon, lat)), proj4string = CRS("EPSG:4326")))
# And finally to proper CRS
villages <- st_transform(villages, "EPSG:24378")


# Save plants
st_write(plants, "data/spatial/plants", 
           driver = "ESRI Shapefile", append = F)
# Save villages
st_write(villages, "data/spatial/villages", 
           driver = "ESRI Shapefile", append = F)







# Just double check everything looks good
ggplot() +
  theme_void() +
  geom_sf(data = plants, aes(color = year_built)) +
  scale_color_viridis_c("Year built", option = "magma") +
  geom_sf(data = india, lwd = 0.25, fill = NA) + 
  geom_sf(data = villages, shape = 4, color = "green")






# Create matrices ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a matrix that has rows of villages and columns of plants

# First distances
dist_matrix <- as_tibble(matrix(NA, nrow = nrow(villages), ncol = nrow(plants)))
for (vil in 1:nrow(villages)){
  for (plant in 1:nrow(plants)){
    dist_matrix[vil, plant] <- pointDistance(villages[vil,], plants[plant,], lonlat = F)
  }
}
# Save this
write.csv(dist_matrix, paste0("data/clean/wind/distances.csv"))


# And angle as well
# Let's turn it into degrees (instead of radians)
rad_deg <- function(rad){
  deg <- (rad * 180) / (pi)
  return(deg)
}
# Note that this is the angle FROM THE PLANT TO THE VILLAGE, with 0 being directly north (and going clockwise)
angle_matrix_original <- as_tibble(matrix(NA, nrow = nrow(villages), ncol = nrow(plants)))
angle_matrix <- as_tibble(matrix(NA, nrow = nrow(villages), ncol = nrow(plants)))
for (vil in 1:nrow(villages)){
  for (plant in 1:nrow(plants)){
    angle_matrix_original[vil, plant] <- rad_deg(
                                                  atan2(
                                                        villages$geometry[vil][[1]][[1]] - plants$geometry[plant][[1]][[1]],
                                                        villages$geometry[vil][[1]][[2]] - plants$geometry[plant][[1]][[2]]
                                                        )
                                                  )
    angle_matrix[vil, plant] <- angle_matrix_original[vil, plant]
    if (angle_matrix[vil, plant]<0){
      angle_matrix[vil, plant] <- angle_matrix[vil, plant] + 360
    }
  }
}

  





# Wind directions by day ----------------------------------------------------------------------------------------------------------------------------------------------------------
# Now load the tables from GEE, which are hourly wind directions
# The goal is to figure out if wind directions at a given time (hourly) 
# Add india
india_wind <- read_sf("data/spatial/shapefiles/state.shp")

# NTOE: have since learned I could do this much easier just using lubridate. It is what it is. This still works.
years <- c("2011", "2012", "2013", "2014", "2015")
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
month_days <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")
# Loop over years
for (year in years){
  
  # Loop over months
  for (month in months){
    # Need this to figure out how many days in the month
    date <- as.Date(paste0(year, "-", month, "-01"), "%Y-%m-%d")
    days_in_month <- lubridate::days_in_month(date)
    # from 10 to the number of days, add to the vector
    days <- month_days
    for (day in seq(from = 10, to = days_in_month)){
      days <- c(days, paste0(day))
    }
    
    # Create matrix for the MONTH that will be the sum of days with wind in given direction. Create this for every new month
    wind_month <- matrix(0, nrow = nrow(angle_matrix), ncol = ncol(angle_matrix))

    # Now loop over days
    for (day in days){
      # Download (https://climatedataguide.ucar.edu/climate-data/ccmp-cross-calibrated-multi-platform-wind-vector-analysis)
      temp_nc1 <- fs::file_temp(ext = ".nc.gz")
      url1 <- paste0("https://data.remss.com/ccmp/v02.0/Y", year, "/M", month, "/CCMP_Wind_Analysis_", year, month, day, "_V02.0_L3.0_RSS.nc")
      download.file(url1, destfile = temp_nc1, mode = "wb")
      suppressMessages(library(R.utils))
      isGzipped(temp_nc1)
      nc_path1 <- gunzip(temp_nc1)
      data <- ncdf4::nc_open(nc_path1)
      
      # Keep only those that cover India
      lon_start <- max(which( data$dim$lon$vals<=extent(india_wind)[1] ))
      lon_count <- min(which( data$dim$lon$vals>=extent(india_wind)[2] )) - lon_start + 1
      lat_start <- max(which( data$dim$lat$vals<=extent(india_wind)[3] ))
      lat_count <- min(which( data$dim$lat$vals>=extent(india_wind)[4] )) - lat_start + 1
      
      east <- ncvar_get(data, "uwnd", start = c(lon_start, lat_start, 1), count = c(lon_count, lat_count, 1))
      north <- ncvar_get(data, "vwnd", start = c(lon_start, lat_start, 1), count = c(lon_count, lat_count, 1))
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
      r <- raster(
                  direction,
                  xmn = range(lon)[1], xmx = range(lon)[2],
                  ymn = range(lat)[1], ymx = range(lat)[2],
                  crs = "EPSG:4326"
                  )
      r2 <- raster(
                  direction_original,
                  xmn = range(lon)[1], xmx = range(lon)[2],
                  ymn = range(lat)[1], ymx = range(lat)[2],
                  crs = "EPSG:4326"
                  )
      
      r <- projectRaster(
                         r,
                         crs = crs(villages)
                         )
      r2 <- projectRaster(
                         r2,
                         crs = crs(villages)
                         )
      
      extracted_values <- raster::extract(r, plants)
      extracted_values_original <- raster::extract(r2, plants)
      
      # Create matrix 
      for (vil in 1:nrow(wind_month)){
        # First do those between 5 and 355 degrees
        condition <- angle_matrix[vil,]>=5 & angle_matrix[vil,]<=355
        wind_month[vil, condition] <- wind_month[vil, condition] +
                                      as.numeric(
                                        (extracted_values[condition]>=angle_matrix[vil, as.vector(condition)] - 5) 
                                        &
                                        (extracted_values[condition]<=angle_matrix[vil, as.vector(condition)] + 5)
                                        )
        # Now others < 5
        wind_month[vil, !condition] <- wind_month[vil, !condition] +
                                      as.numeric(
                                        (extracted_values_original[!condition]>=angle_matrix_original[vil, as.vector(!condition)] - 5) 
                                        &
                                        (extracted_values_original[!condition]<=angle_matrix_original[vil, as.vector(!condition)] + 5)
                                        )
      }
      
      # If it's the last one...
      if (day==days[length(days)]){
        wind_month = wind_month/as.numeric(day)  # divide by total number of days
        # And save this month
        write.csv(wind_month, paste0("data/clean/wind/y", year, "m", month, ".csv"))
      }
    }
  }
}
# Save last example raster
writeRaster(r, paste0("data/clean/wind/example_raster.tif"))









