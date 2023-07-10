# Purpose: This script cleans and merges the district/village overlap with the NSS data
# Author: Josh Merfeld
# Date: December 26th, 2022

rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(rgeos)
library(sf)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../")
# Double check
getwd()    # check


# Village overlap data
villages <- read_sf(paste0("data/spatial/villages_overlap/villages_overlap.shp"))
villages <- villages %>% mutate(shrid = paste0(pc11_s_id, "-", pc11_tv_id),
                                district_id = paste0(pc11_s_id, "-", pc11_d_id),
                                area_overlap = area,
                                area_district = exp(area_2)) %>%
                          dplyr::select(shrid, state = ST_CEN_CD, district = DT_CEN_CD, state_name = ST_NM, district_name = DISTRICT, area_overlap, area_district, district_id)

# now clean villages more 
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
                              dplyr::select("state_merge", "district_merge")
village_merge$merge <- 1
village_merge <- village_merge %>% group_by(state_merge, district_merge) %>%
                                    filter(row_number()==1) %>%
                                    ungroup()

df_labor <- df_labor %>% mutate(state_merge = as.numeric(state_merge),
                                     district_merge = as.numeric(district_merge)) %>% left_join(village_merge, by = c("state_merge", "district_merge"))
df_labor <- df_labor %>% filter(is.na(merge)==F)


# now get averages for each district
all <- read_csv(paste0("data/clean/terra_district/precip1990.csv"))
all <- all[,1:2]
for (month in 6:11){
       temp <- read_csv(paste0("data/clean/terra_district/precip1990.csv"))
       temp <- temp[,c(1:2, 2+month)]
       for (year in 1991:2003){
              temp2 <- read_csv(paste0("data/clean/terra_district/precip", year, ".csv"))
              temp2$sum <- temp2[,c(8:(2+month))]
              temp2 <- temp2[,c(1:2, ncol(temp2))]
              temp <- temp %>% left_join(temp2, by = c("ST_CEN_CD", "DT_CEN_CD"))
       }
       temp[[paste0("precip_mean_month", month)]] <- apply(temp[,3:ncol(temp)], 1, mean)
       temp[[paste0("precip_sd_month", month)]] <- apply(temp[,3:ncol(temp)], 1, sd)

       temp <- temp %>%
                dplyr::select(c("ST_CEN_CD", "DT_CEN_CD", starts_with(c("precip_"))))

       all <- all %>% left_join(temp, by = c("ST_CEN_CD", "DT_CEN_CD"))
}
write_csv(all, paste0("data/clean/terra_district/precip_mean_monthALL.csv"))


# district pollution
pollution <- as_tibble(read_csv("data/clean/pm25_district/all_combined_district.csv"))


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
  # there are a couple mistakes in terms of duplicate ids. fix those
  village_wind <- village_wind %>% group_by(shrid) %>% filter(row_number()==1) %>% ungroup()
  # And also pm
  pollution_merge <- pollution %>% filter(year==year(date_vec[day]) & month==month(date_vec[day]))
  # there are a couple mistakes in terms of duplicate ids. fix those
  pollution_merge <- pollution_merge %>% group_by(district_id) %>% filter(row_number()==1) %>% ungroup()
  pollution_merge <- pollution_merge %>% dplyr::select(district_id, pm25_district)
  village_wind <- village_wind %>% left_join(pollution_merge, by = "district_id")
  
  # monthly wind
  village_month_wind <- read_csv(paste0("data/clean/wind_ntl/months/y", year(date_vec[day]), "m", month(date_vec[day]), ".csv")) %>% as_tibble()
  # there are a couple mistakes in terms of duplicate ids. fix those
  village_month_wind <- village_month_wind %>% group_by(shrid) %>% filter(row_number()==1) %>% ungroup()
  # rename
  colnames(village_month_wind) <- c("shrid", "month_sum")
  village_month_wind <- village_month_wind %>% dplyr::select(shrid, month_sum)
  village_wind <- village_wind %>% left_join(village_month_wind, by = "shrid")
  
  # Weighted mean by AREA. Any parts of the district without villages are ZEROS
  village_wind <- village_wind %>% 
                  group_by(state, district) %>%
                  mutate(days_sum = weighted.mean(days_sum, area_weight, na.rm = T),
                         month_sum = weighted.mean(month_sum, area_weight, na.rm = T),
                         pm25_district_weighted = weighted.mean(pm25, area_weight, na.rm = T),
                         state_merge = as.numeric(state),
                         district_merge = as.numeric(district),
                         tot_weight = sum(area_weight, na.rm = T),
                         days_sum = days_sum*tot_weight,
                         month_sum = month_sum*tot_weight,
                         pm25_sum = days_sum*tot_weight
                         ) %>% # Last one adjusts for the zeros for areas not here (not pm, though, since it is the entire district)
                  filter(row_number()==1) %>%
                  ungroup() %>%
                  dplyr::select(state_merge, district_merge, days_sum, pm25_district, month_sum)
  
  
  temp <- df_labor %>% filter(date==date_vec[day])
  temp <- temp %>% left_join(village_wind, by = c("state_merge", "district_merge"))
  temp <- temp[is.na(temp$days_sum)==F,]

  # monthly weather
  tmin <- read_csv(paste0("data/clean/terra_district/tmin", year(date_vec[day]), ".csv"))
  tmin <- tmin[, c(1, 2, 2 + month(date_vec[day]))]
  colnames(tmin) <- c("state_merge", "district_merge", "month_tmin")
  tmax <- read_csv(paste0("data/clean/terra_district/tmax", year(date_vec[day]), ".csv"))
  tmax <- tmax[, c(1, 2, 2 + month(date_vec[day]))]
  colnames(tmax) <- c("state_merge", "district_merge", "month_tmax")
  precip <- read_csv(paste0("data/clean/terra_district/precip", year(date_vec[day]), ".csv"))
  precip <- precip[, c(1, 2, 2 + month(date_vec[day]))]
  colnames(precip) <- c("state_merge", "district_merge", "month_precip")
  
  tmin_all <- read_csv(paste0("data/clean/terra_district/tmin", year(date_vec[day]), ".csv"))
  tmin_all <- tmin_all[, c(1, 2, 8:12)]
  colnames(tmin_all) <- c("state_merge", "district_merge", "month6_tmin", "month7_tmin", "month8_tmin", "month9_tmin", "month10_tmin")
  tmax_all <- read_csv(paste0("data/clean/terra_district/tmax", year(date_vec[day]), ".csv"))
  tmax_all <- tmax_all[, c(1, 2, 8:12)]
  colnames(tmax_all) <- c("state_merge", "district_merge", "month6_tmax", "month7_tmax", "month8_tmax", "month9_tmax", "month10_tmax")
  precip_all <- read_csv(paste0("data/clean/terra_district/precip", year(date_vec[day]), ".csv"))
  precip_all <- precip_all[, c(1, 2, 8:12)]
  colnames(precip_all) <- c("state_merge", "district_merge", "month6_precip", "month7_precip", "month8_precip", "month9_precip", "month10_precip")

  # merge into temp
  temp <- temp %>% left_join(tmin %>% mutate(state_merge = as.numeric(state_merge),
                                              district_merge = as.numeric(district_merge)),
                              by = c("state_merge", "district_merge"))
  temp <- temp %>% left_join(tmax %>% mutate(state_merge = as.numeric(state_merge),
                                              district_merge = as.numeric(district_merge)),
                              by = c("state_merge", "district_merge"))
  temp <- temp %>% left_join(precip %>% mutate(state_merge = as.numeric(state_merge),
                                      district_merge = as.numeric(district_merge)),
                              by = c("state_merge", "district_merge"))
  temp <- temp %>% left_join(tmin_all %>% mutate(state_merge = as.numeric(state_merge),
                                              district_merge = as.numeric(district_merge)),
                              by = c("state_merge", "district_merge"))
  temp <- temp %>% left_join(tmax_all %>% mutate(state_merge = as.numeric(state_merge),
                                              district_merge = as.numeric(district_66merge)),
                              by = c("state_merge", "district_merge"))
  temp <- temp %>% left_join(precip_all %>% mutate(state_merge = as.numeric(state_merge),
                                      district_merge = as.numeric(district_merge)),
                              by = c("state_merge", "district_merge"))

  df_labor_merged <- rbind(df_labor_merged, temp)
  
  print(day/length(date_vec))
}
df_labor_merged <- df_labor_merged %>% mutate(distfe = paste0("s", state_merge, "-d", district_merge))
write.csv(df_labor_merged, "data/clean/nss/merged_week.csv")













