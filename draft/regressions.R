# Purpose: This script estimates effects
# Author: Josh Merfeld
# Date: December 12th, 2022

rm(list = ls())

library(tidyverse)
library(lubridate)
library(fixest)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check


df_final <- read.csv(paste0("data/clean/vdsa/labor_sat.csv"))
east <- read.csv(paste0("data/clean/vdsa/labor_east.csv"))
df_final <- rbind(df_final, east)
rm(east)
df_final <- df_final %>% select(-c("X.1", "X"))
# Only 15+ for now
df_final <- df_final %>% filter(age>=15)

village <- read.csv(paste0("data/clean/wind/villages50.csv"))
village <- village %>% select(village, month, exposure) %>% arrange(village, month)

df_final <- df_final %>% left_join(village, by = c("village", "month"))
df_final <- df_final %>% mutate(pidfe = paste0(hid, "-", pid))
df_final <- df_final %>% mutate(
                                month_int = month(month)
                                )




feols(days_wage ~ exposure | pidfe + state^month, data = df_final, cluster = c("pidfe", "month"))
feols(days_wage ~ exposure | pidfe + state^month_int, data = df_final, cluster = c("pidfe", "month"))

feols(days_wage_nf ~ exposure | pidfe + state^month, data = df_final, cluster = c("pidfe", "month"))
feols(days_wage_nf ~ exposure | pidfe + state^month_int, data = df_final, cluster = c("pidfe", "month"))

feols(days_wage_f ~ exposure | pidfe + state^month, data = df_final, cluster = c("pidfe", "month"))
feols(days_wage_f ~ exposure | pidfe + state^month_int, data = df_final, cluster = c("pidfe", "month"))

feols(days_own_farm ~ exposure | pidfe + state^month, data = df_final, cluster = c("pidfe", "month"))
feols(days_own_farm ~ exposure | pidfe + state^month_int, data = df_final, cluster = c("pidfe", "month"))

feols((days_wage_f + days_own_farm) ~ exposure | pidfe + state^month, data = df_final, cluster = c("pidfe", "month"))
feols((days_wage_f + days_own_farm) ~ exposure | pidfe + state^month_int, data = df_final, cluster = c("pidfe", "month"))














