# Purpose: This script cleans and merges the district/village overlap with the NSS data
# Author: Josh Merfeld
# Date: December 26th, 2022

rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(sf)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check




# Village overlap data
villages <- read_sf(paste0("data/spatial/villages_overlap/villages_overlap.shp"))
villages <- villages %>% mutate(shrid = paste0(pc11_s_id, "-", pc11_tv_id),
                                area_overlap = area,
                                area_district = exp(area_2)) %>%
                          dplyr::select(shrid, state = ST_CEN_CD, district = DT_CEN_CD, state_name = ST_NM, district_name = DISTRICT, area_overlap, area_district)
villages <- as_tibble(villages) %>% dplyr::select(-geometry)
villages <- villages %>% 
              group_by(state, district) %>% 
              mutate(
                     area_weight = (area_overlap/area_district),
                     area_weight_alt = (area_overlap/sum(area_overlap))
                     ) %>%
              ungroup()
villages$area_weight[is.na(villages$area_weight)==T] <- villages$area_weight_alt[is.na(villages$area_weight)==T]
villages$area_weight[villages$area_weight>villages$area_weight_alt] <- villages$area_weight_alt[villages$area_weight>villages$area_weight_alt]



# WEEKLY --------------------------------------------------------------------------------
df_labor <- c()
for (nsswave in c(61, 62, 64, 66, 68)){
  temp <- read_csv(paste0("data/clean/nss/nss", nsswave, ".csv"))
  temp$wave <- nsswave
  
  temp$hid <- paste0(temp$hid)
  temp$pid <- paste0(temp$pid)
  
  df_labor <- bind_rows(df_labor, temp)
}
rm(temp)
# Here are the dates
date_vec <- unique(df_labor$date[is.na(df_labor$date)==F])


# First, make the df smaller by dropping districts that do not match to the villages
village_merge <- villages %>% mutate(state_merge = as.numeric(state),
                                     district_merge = as.numeric(district)) %>%
                              select("state_merge", "district_merge")
village_merge$merge <- 1
village_merge <- village_merge %>% group_by(state_merge, district_merge) %>%
                                    filter(row_number()==1) %>%
                                    ungroup()

df_labor <- df_labor %>% mutate(state_merge = as.numeric(state_merge),
                                     district_merge = as.numeric(district_merge)) %>% left_join(village_merge, by = c("state_merge", "district_merge"))
df_labor <- df_labor %>% filter(is.na(merge)==F)



pollution <- read_csv(paste0("data/clean/pm25/all_combined.csv"))
pollution <- pollution %>% rename(pm25 = mean)


# Going to go through the dates and process date by date
df_labor_merged <- c()
for (day in 1:length(date_vec)){
  # First, load villages
  village_wind <- read_csv(paste0("data/clean/wind_ntl/days/date_", year(date_vec[day]), "-", month(date_vec[day]), "-", day(date_vec[day]), ".csv")) %>% as_tibble()
  village_wind$days_sum <- apply(village_wind[,2:8], 1, FUN = "sum")
  village_wind <- village_wind %>% dplyr::select(shrid, days_sum)
  
  village_wind <- villages %>% left_join(village_wind, by = "shrid")
  # Missings as zero
  village_wind$days_sum[is.na(village_wind$days_sum)==T] <- 0
  # And also pm
  pollution_merge <- pollution %>% filter(year==year(date_vec[day]) & month==month(date_vec[day]))
  village_wind <- village_wind %>% left_join(pollution, by = "shrid")
  
  
  # Weighted mean by AREA. Any parts of the district without villages are ZEROS
  village_wind <- village_wind %>% 
                  group_by(state, district) %>%
                  mutate(days_sum = weighted.mean(days_sum, area_weight),
                         pm25_sum = weighted.mean(pm25, area_weight),
                         state_merge = as.numeric(state),
                         district_merge = as.numeric(district),
                         tot_weight = sum(area_weight),
                         days_sum = days_sum*tot_weight,
                         pm25_sum = days_sum*tot_weight) %>% # Last one adjusts for the zeros for areas not here
                  filter(row_number()==1) %>%
                  ungroup() %>%
                  select(state_merge, district_merge, days_sum, pm25_sum)
  
  
  temp <- df_labor %>% filter(date==date_vec[day])
  temp <- temp %>% left_join(village_wind, by = c("state_merge", "district_merge"))
  temp <- temp[is.na(temp$days_sum)==F,]
  df_labor_merged <- rbind(df_labor_merged, temp)
  
  print(day/length(date_vec))
}
df_labor_merged <- df_labor_merged %>% mutate(distfe = paste0("s", state_merge, "-d", district_merge))
write.csv(df_labor_merged, "data/clean/nss/merged_week.csv")






# And also pm; all years covering the nss data
df_labor_merged <- read_csv("data/clean/nss/merged_week.csv")
pollution_merged <- c()
for (y in year(min(df_labor_merged$date)):year(max(df_labor_merged$date))){
  for (m in 1:12){
    pollution_merge <- pollution %>% filter(year==y & month==m)
    
    village_wind <- villages %>% left_join(pollution_merge, by = "shrid")
    # Weighted mean by AREA. Any parts of the district without villages are ZEROS
    village_wind <- village_wind %>% 
                    group_by(state, district) %>%
                    mutate(pm25 = weighted.mean(pm25, area_weight, na.rm = T),
                           state_merge = as.numeric(state),
                           district_merge = as.numeric(district),
                           tot_weight = sum(area_weight),
                           pm25 = pm25*tot_weight,
                           year = y, month_int = m) %>% # Last one adjusts for the zeros for areas not here
                    filter(row_number()==1) %>%
                    ungroup() %>%
                    select(state_merge, district_merge, year, month_int, pm25)
    
    pollution_merged <- rbind(pollution_merged, village_wind)
    
    print(paste0(y, "-", m))
  }
}
write_csv(pollution_merged, paste0("data/clean/pm25/nss_merge.csv"))










