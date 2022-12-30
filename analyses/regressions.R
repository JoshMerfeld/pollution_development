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


# Nightlights
ntl <- read_csv("data/clean/village_ntl.csv")
ntl <- ntl %>% mutate(state = substr(shrid, 1, 2), 
                      sums_proportion = sums/366)
ntl <- panel(ntl, ~ shrid + year, duplicate.method = "first")





summary(feols(log(total_light + 1) ~ l(sums, 0) | shrid[year] + year,
              data = ntl,
              cluster = "shrid"))

summary(feols(log(total_light + 1) ~ l(sums, 0) + l(log(rain), 0) | shrid[year] + year,
              data = ntl,
              cluster = "shrid"))

summary(feols(log(total_light + 1) ~ l(sums, 0:1) + l(log(rain), 0) | shrid[year] + year,
              data = ntl,
              cluster = "shrid"))





summary(feols(log(total_light + 1) ~ l(sums, 0) + l(log(rain), 0) | shrid + year,
              data = ntl,
              cluster = "shrid"))

summary(feols(log(total_light + 1) ~ l(sums, 0:1) + l(log(rain), 0:1) | shrid + year,
              data = ntl,
              cluster = "shrid"))

summary(feols(log(total_light + 1) ~ l(sums, 0:1) + l(log(rain), 0:1) | shrid + year^state,
              data = ntl,
              cluster = "shrid"))











df <- read_csv("data/clean/nss/merged_week.csv")
df <- df %>% filter(age>=15)


summary(feols((days_self + days_wage) ~ days_sum | distfe + wave,
              data = df,
              cluster = "distfe"))

summary(feols((days_self + days_wage) ~ days_sum + female + age + age^2 + as.factor(educ) | distfe + wave,
              data = df,
              cluster = "distfe"))

summary(feols((days_self + days_wage) ~ days_sum + female + age + age^2 + as.factor(educ) | distfe[wave] + wave,
              data = df,
              cluster = "distfe"))




