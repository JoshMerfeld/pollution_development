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
library(exactextractr)

# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
# Double check
getwd()    # check


### Load raw data ----------------------------------------------------------------------------------------------------------------------------------------------
villages <- read_sf("data/spatial/villages_overlap/villages_overlap.shp") %>% st_set_crs(st_crs("EPSG:24378"))
villages$shrid <- paste0(villages$pc11_s_id, "-", villages$pc11_tv_id)
villages <- villages %>%
              group_by(shrid) %>%
              filter(row_number()==1) %>%
              ungroup()
# put in same crs as nightlights (much easier than transforming each year of nightlights)
villages <- st_transform(villages, crs = st_crs("EPSG:4326"))


# go through nightlights, one year at a time
# from https://www.nature.com/articles/s41597-020-0510-y#code-availability
for (year in 1999:2013){
      nightlights <- raster(paste0("data/raw/nightlights/Harmonized_DN_NTL_", year, "_calDMSP.tif"))
      nighlights <- exact_extract(nightlights, villages, fun = "mean", append_cols = "shrid")
      nighlights <- nighlights %>% rename(ntl = mean)
      # add year
      nighlights$year <- year
      # save
      if (year==1999){
            ntl_all <- nighlights
      } else ntl_all <- rbind(ntl_all, nighlights)

      print(year)
}

# save
write.csv(ntl_all, "data/clean/village_ntl.csv")





