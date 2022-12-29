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
date_vec <- unique(df_labor$date)



# Going to go through the dates and process date by date
for (day in 1:length(date_vec)){
  # First, load villages
  village_wind <- read_csv(paste0("data/clean/wind_ntl/days/date_", year(date_vec[day]), "-", month(date_vec[day]), "-", day(date_vec[day]), ".csv")) %>% as_tibble()
  village_wind$days_sum <- apply(village_wind[,2:8], 1, FUN = "sum")
  village_wind <- village_wind %>% dplyr::select(shrid, days_sum)
  
  village_wind <- villages %>% left_join(village_wind, by = "shrid")
  # Missings as zero
  village_wind$days_sum[is.na(village_wind$days_sum)==T] <- 0
  
  # Weighted mean by AREA. Any parts of the district without villages are ZEROS
  village_wind <- village_wind %>% 
                  group_by(state, district) %>%
                  mutate(days_sum = weighted.mean(days_sum, area_weight),
                         state_merge = as.numeric(state),
                         district_merge = as.numeric(district)) %>%
                  filter(row_number()==1) %>%
                  ungroup() %>%
                  select(state_merge, district_merge, days_sum)
  
  
  temp <- df_labor %>% filter(date==date_vec[day])
  temp <- temp %>% left_join(village_wind, by = c("state_merge", "district_merge"))
  temp <- temp[is.na(temp$days_sum)==F,]
  df_labor_merged <- rbind(df_labor_merged, temp)
  
  print(day/length(date_vec))
}
df_labor_merged <- df_labor_merged %>% mutate(distfe = paste0("s", state_merge, "-d", district_merge))
write.csv(df_labor_merged, "data/clean/nss/merged_week.csv")




# NOW DO DAILY

df_labor <- c()
for (nsswave in c(61, 62, 64, 66, 68)){
  temp <- read_csv(paste0("data/clean/nss/nss", nsswave, "_daily.csv"))
  temp$wave <- nsswave
  
  temp$hid <- paste0(temp$hid)
  temp$pid <- paste0(temp$pid)
  
  df_labor <- bind_rows(df_labor, temp)
}
rm(temp)
# Here are the dates
df_labor$day_date <- as_date(df_labor$day_date)
df_labor$day_date_l1 <- as_date(df_labor$day_date_l1)
df_labor$day_date_l2 <- as_date(df_labor$day_date_l2)

date_vec <- unique(df_labor$day_date)


df_labor$days_sum <- NA
df_labor$days_sum_l1 <- NA
df_labor$days_sum_l2 <- NA

# Going to go through the dates and process date by date
for (day in 1:length(date_vec)){
  # First, load villages
  village_wind <- read_csv(paste0("data/clean/wind_ntl/days/date_", year(date_vec[day]), "-", month(date_vec[day]), "-", day(date_vec[day]), ".csv")) %>% as_tibble()
  village_wind <- village_wind %>% mutate(days_sum = ...2)
  village_wind <- village_wind %>% dplyr::select(shrid, days_sum)
  
  village_wind <- villages %>% left_join(village_wind, by = "shrid")
  # Missings as zero
  village_wind$days_sum[is.na(village_wind$days_sum)==T] <- 0
  
  # Weighted mean by AREA. Any parts of the district without villages are ZEROS
  village_wind <- village_wind %>% 
                  group_by(state, district) %>%
                  mutate(days_sum = weighted.mean(days_sum, area_weight),
                         state_merge = as.numeric(state),
                         district_merge = as.numeric(district)) %>%
                  filter(row_number()==1) %>%
                  ungroup() %>%
                  select(state_merge, district_merge, days_sum)
  
  temp <- df_labor[df_labor$date==date_vec[day],]
  temp <- temp %>% select(-c("days_sum", "days_sum_l1", "days_sum_l2"))
  temp <- temp %>% left_join(village_wind, by = c("state_merge", "district_merge"))
  
  # Now one lag
  village_wind <- read_csv(paste0("data/clean/wind_ntl/days/date_", 
                                  year(date_vec[day] - 1), "-", 
                                  month(date_vec[day] - 1), "-", 
                                  day(date_vec[day] - 1), ".csv")) %>% as_tibble()
  village_wind <- village_wind %>% mutate(days_sum = ...2)
  village_wind <- village_wind %>% dplyr::select(shrid, days_sum)
  
  village_wind <- villages %>% left_join(village_wind, by = "shrid")
  # Missings as zero
  village_wind$days_sum[is.na(village_wind$days_sum)==T] <- 0
  village_wind$area_weight[is.na(village_wind$area_weight)==T] <- 0
  
  # Weighted mean by AREA. Any parts of the district without villages are ZEROS
  village_wind <- village_wind %>% 
                  group_by(state, district) %>%
                  mutate(days_sum = weighted.mean(days_sum, area_weight),
                         state_merge = as.numeric(state),
                         district_merge = as.numeric(district)) %>%
                  filter(row_number()==1) %>%
                  ungroup() %>%
                  select(state_merge, district_merge, days_sum_l1 = days_sum)
  
  temp <- temp %>% left_join(village_wind, by = c("state_merge", "district_merge"))
  
  
  # And second lag
  village_wind <- read_csv(paste0("data/clean/wind_ntl/days/date_", 
                                  year(date_vec[day] - 2), "-", 
                                  month(date_vec[day] - 2), "-", 
                                  day(date_vec[day] - 2), ".csv")) %>% as_tibble()
  village_wind <- village_wind %>% mutate(days_sum = ...2)
  village_wind <- village_wind %>% dplyr::select(shrid, days_sum)
  
  village_wind <- villages %>% left_join(village_wind, by = "shrid")
  # Missings as zero
  village_wind$days_sum[is.na(village_wind$days_sum)==T] <- 0
  
  # Weighted mean by AREA. Any parts of the district without villages are ZEROS
  village_wind <- village_wind %>% 
                  group_by(state, district) %>%
                  mutate(days_sum = weighted.mean(days_sum, area_weight),
                         state_merge = as.numeric(state),
                         district_merge = as.numeric(district)) %>%
                  filter(row_number()==1) %>%
                  ungroup() %>%
                  select(state_merge, district_merge, days_sum_l2 = days_sum)
  
  temp <- temp %>% left_join(village_wind, by = c("state_merge", "district_merge"))
  
  
  # And finally rbind
  #df_labor_merged <- rbind(df_labor_merged, temp)
  df_labor[df_labor$date==date_vec[day],] <- temp
  
  
  print(day/length(date_vec))
}
df_labor <- df_labor[is.na(df_labor$days_sum)==F,]


df_labor_merged <- df_labor_merged %>% mutate(distfe = paste0("s", state_merge, "-d", district_merge),
                                              pidfe = paste0("h", hid, "-p", pid, "-s", state_merge, "-d", district_merge))

write.csv(df_labor_merged, "data/clean/nss/merged_daily.csv")













df_labor_merged$month <- month(df_labor_merged$date)
df_labor_merged$year <- year(df_labor_merged$date)


df_labor_merged <- df_labor_merged %>% filter(age>=15)


summary(feols((days_self + days_wage) ~ days_sum + age + age^2 + female | distfe + wave, 
              data = df_labor_merged, 
              cluster = c("distfe")))
summary(feols((days_self + days_wage) ~ days_sum + age + age^2 + female | distfe + year^month^state_merge, 
              data = df_labor_merged, 
              cluster = c("distfe")))


summary(feols((days_wage) ~ days_sum + age + age^2 + female | distfe + wave, 
              data = df_labor_merged, 
              cluster = c("distfe")))
summary(feols((days_wage) ~ days_sum + age + age^2 + female | distfe + year^month^state_merge, 
              data = df_labor_merged, 
              cluster = c("distfe")))


summary(feols((days_self) ~ days_sum + age + age^2 + female | distfe + wave, 
              data = df_labor_merged, 
              cluster = c("distfe")))
summary(feols((days_self) ~ days_sum + age + age^2 + female | distfe + year^month^state_merge, 
              data = df_labor_merged, 
              cluster = c("distfe")))


summary(feols(intensity ~ days_sum | pidfe, 
              data = df_labor_merged, 
              cluster = c("distfe")))






