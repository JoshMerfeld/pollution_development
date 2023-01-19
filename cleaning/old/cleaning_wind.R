# Purpose: This script cleans the wind data
# Author: Josh Merfeld
# Date: December 12th, 2022

rm(list = ls())

library(sf)
library(sp)
library(raster)
library(tidyverse)
library(stars)
library(lubridate)
library(RColorBrewer)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check


# Load villages
villages <- st_read("data/spatial/villages")
# Convert to tibble and remove geometry (don't need it)
villages <- as_tibble(villages) %>% select(-geometry)
# And distances
dist_matrix <- read.csv(paste0("data/clean/wind/distances.csv"))
dist_matrix <- dist_matrix[,-1]
min(dist_matrix)  # Min is 5.8 km
# Let's say within 30km = exposure
dist_matrix_30km <- dist_matrix<=30000
sum(dist_matrix_30km)   # 93
# Let's start easy and say within 50km = exposure
dist_matrix_50km <- dist_matrix<=50000
sum(dist_matrix_50km)   # 93
# Also try 75
dist_matrix_75km <- dist_matrix<=75000
sum(dist_matrix_75km)   # 233
# And 100
dist_matrix_100km <- dist_matrix<=100000
sum(dist_matrix_100km)   # 388


# Need to load monthly wind direction and decide how to aggregate to village
years <- "2009"
for (y in seq(from = 2010, to = 2015)){
  years <- c(years, paste0(y))
}
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
villages30 <- villages
villages50 <- villages
villages75 <- villages
villages100 <- villages
for (year in years){
  for (month in months){
    wind_month <- read.csv(paste0("data/clean/wind/y", year, "m", month, ".csv"))
    
    wind_month30 <- wind_month*dist_matrix_30km
    village_exposure <- apply(wind_month30, 1, max)
    villages30 <- cbind(villages30, village_exposure)
    wind_month50 <- wind_month*dist_matrix_50km
    village_exposure <- apply(wind_month50, 1, max)
    villages50 <- cbind(villages50, village_exposure)
    wind_month75 <- wind_month*dist_matrix_75km
    village_exposure <- apply(wind_month75, 1, max)
    villages75 <- cbind(villages75, village_exposure)
    wind_month100 <- wind_month*dist_matrix_100km
    village_exposure <- apply(wind_month100, 1, max)
    villages100 <- cbind(villages100, village_exposure)
  }
}
new_names <- c(colnames(villages)[1:3])
for (year in years){
  for (month in months){
    new_names <- c(new_names, paste0(year, "_", month))
  }
}

colnames(villages30) <- new_names
colnames(villages50) <- new_names
colnames(villages75) <- new_names
colnames(villages100) <- new_names
villages_id <- unique(villages[,1:3]) %>% mutate(id = row_number())

villages30 <- gather(villages30, "month", "exposure", -c(1, 2, 3))
villages30 <- villages30 %>% left_join(villages_id, by = c(colnames(villages)[1:3]))
villages50 <- gather(villages50, "month", "exposure", -c(1, 2, 3))
villages50 <- villages50 %>% left_join(villages_id, by = c(colnames(villages)[1:3]))
villages75 <- gather(villages75, "month", "exposure", -c(1, 2, 3))
villages75 <- villages75 %>% left_join(villages_id, by = c(colnames(villages)[1:3]))
villages100 <- gather(villages100, "month", "exposure", -c(1, 2, 3))
villages100 <- villages100 %>% left_join(villages_id, by = c(colnames(villages)[1:3]))


villages30 <- villages30 %>% mutate(
                                    month = ym(month),
                                    month_int = month(month),
                                    year_int = month(month)
                                    )
villages50 <- villages50 %>% mutate(
                                    month = ym(month),
                                    month_int = month(month),
                                    year_int = month(month)
                                    )
villages75 <- villages75 %>% mutate(
                                    month = ym(month),
                                    month_int = month(month),
                                    year_int = month(month)
                                    )
villages100 <- villages100 %>% mutate(
                                      month = ym(month),
                                      month_int = month(month),
                                      year_int = month(month)
                                      )
# save these
write.csv(villages30, paste0("data/clean/wind/villages30.csv"))
write.csv(villages50, paste0("data/clean/wind/villages50.csv"))
write.csv(villages75, paste0("data/clean/wind/villages75.csv"))
write.csv(villages100, paste0("data/clean/wind/villages100.csv"))



pal <- brewer.pal(6, "Set2")
ggplot(villages100 %>% filter(id==2 & month_int<=6)) +
  geom_line(aes(x = month, y = exposure, color = as.factor(month_int))) +
  labs(color = "month") +
  scale_color_manual(values = pal) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "12 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ggplot(villages100 %>% filter(id==2 & month_int>=7)) +
  geom_line(aes(x = month, y = exposure, color = as.factor(month_int))) +
  labs(color = "month") +
  scale_color_manual(values = pal) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "12 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


villages100 %>% group_by(id, month_int) %>% mutate(temp = sd(exposure)) %>% summary()
summary(villages100$exposure)





















