# Purpose: This script estimates effects with the NSS data. I save the results to load into the markdown script
# Author: Josh Merfeld
# Date: February 25th, 2023

rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)



# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check



df <- read_xlsx("/Users/Josh/Dropbox/Datasets/VDSA/EastIndia/2011/Cultivation/Cult_ip.xlsx")
df <- rbind(df, read_xlsx("/Users/Josh/Dropbox/Datasets/VDSA/EastIndia/2012/Cultivation/Cult_ip.xlsx"))
df$year <- paste0("20", substr(df$Sur_mon_yr, 4, 5))
df$month <- substr(df$Sur_mon_yr, 1, 2)
df$day <- "01"
df$date <- as_date(paste0(df$year, "-", df$month, "-", df$day))




# create new variables based on operations
# first to lower
df$Operation <- str_to_lower(df$Operation)
df$op_plough <- str_detect(df$Operation, "plough")
df$op_transplant <- str_detect(df$Operation, "transpl")
df$op_sowing <- str_detect(df$Operation, "sow")
df$op_fertilizer <- str_detect(df$Operation, "fertiliz")
df$op_weeding <- str_detect(df$Operation, "weed")
df$op_threshing <- str_detect(df$Operation, "threshing")
df$op_harvest <- str_detect(df$Operation, "harvest")
df <- df %>% filter(Season=="Kharif")

df <- df %>%
      group_by(date) %>%
      mutate(
             op_plough = mean(op_plough),
             op_transplant = mean(op_transplant),
             op_sowing = mean(op_sowing),
             op_fertilizer = mean(op_fertilizer),
             op_weeding = mean(op_weeding),
             op_threshing = mean(op_threshing),
             op_harvest = mean(op_harvest)
             ) %>%
      filter(row_number()==1) %>%
      ungroup() %>%
      dplyr::select(date, op_plough, op_transplant, op_sowing, op_fertilizer, op_weeding, op_threshing, op_harvest)





cols <- c("ploughing" = viridis(6)[1], "sowing" = viridis(6)[2], "fertilizer" = viridis(6)[3], 
          "weeding" = viridis(6)[4], "harvesting" = viridis(6)[5], "threshing" = viridis(6)[6])
opplot <- ggplot(data = df %>% filter(year(date)==2012)) + 
              geom_line(aes(x = month(date), y = op_plough, color = "ploughing")) + 
              geom_line(aes(x = month(date), y = op_sowing, color = "sowing")) + 
              geom_line(aes(x = month(date), y = op_fertilizer, color = "fertilizer")) + 
              geom_line(aes(x = month(date), y = op_weeding, color = "weeding")) + 
              geom_line(aes(x = month(date), y = op_harvest, color = "harvesting")) +
              geom_line(aes(x = month(date), y = op_threshing, color = "threshing")) +
              theme_minimal() +
              xlab("month") + ylab("proportion of all operations") +
              scale_x_continuous(breaks = seq(from = 1, to = 12, by = 1)) +
              scale_color_manual("operation", values = cols) +
              theme(legend.position = (c(0.8,0.8)))
ggsave("pollution_development/draft/tables/opplot.png")






