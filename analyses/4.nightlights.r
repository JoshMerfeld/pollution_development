# Purpose: This script estimates effects on nightlights
# Author: Josh Merfeld
# Date: February 25th, 2023

rm(list = ls())

library(tidyverse)
library(lubridate)
library(viridis)
library(fixest)
library(sf)
library(openxlsx)
library(parallel)


# Cleaning ---------------------------------

df <- read_csv("data/clean/ag_productivity/all_combined.csv")
pollution <- read_csv(paste0("data/clean/pm25/ag_merge.csv"))
df <- df %>% 
        left_join(pollution, by = c("shrid", "year", "season"))
rm(pollution)
df <- df %>% group_by(shrid, season) %>%
              mutate(
                     yield = mean,
                     rain_z = (log(rain_total + 1) - mean(log(rain_total + 1)))/sd(log(rain_total + 1)),
                     rain_z2 = ((rain_total) - mean((rain_total)))/sd((rain_total)),
                     wind_abs_dev = abs(days_sums - mean(days_sums)),
                     pm25_abs_dev = abs(pm25 - mean(pm25)),
                     wind = days_sums/10,
                     pmavg1 = log(pm1),
                     pmavg2 = log((pm1 + pm2)/2),
                     pmavg3 = log((pm1 + pm2 + pm3)/3),
                     pmavg4 = log((pm1 + pm2 + pm3 + pm4)/4),
                     pmavg5 = log((pm1 + pm2 + pm3 + pm4 + pm5)/5),
                     pm1 = log(pm1),
                     pm2 = log(pm2),
                     pm3 = log(pm3),
                     pm4 = log(pm4),
                     pm5 = log(pm5),
                     windavg1 = m1,
                     windavg2 = (m1 + m2)/2,
                     windavg3 = (m1 + m2 + m3)/3,
                     windavg4 = (m1 + m2 + m3 + m4)/4,
                     windavg5 = (m1 + m2 + m3 + m4 + m5)/5,
                     temp_m1 = (tmax_m1 + tmin_m1)/20,
                     temp_m2 = (tmax_m2 + tmin_m2)/20,
                     temp_m3 = (tmax_m3 + tmin_m3)/20,
                     temp_m4 = (tmax_m4 + tmin_m4)/20,
                     temp_m5 = (tmax_m5 + tmin_m5)/20,
                     rain1_z = (log(rain_m1 + 1) - mean(log(rain_m1 + 1))/sd(log(rain_m1 + 1))),
                     rain2_z = (log(rain_m2 + 1 + rain_m1) - mean(log(rain_m2 + 1 + rain_m1))/sd(log(rain_m2 + 1 + rain_m1))),
                     rain3_z = (log(rain_m3 + 1 + rain_m1 + rain_m2) - mean(log(rain_m3 + 1 + rain_m1 + rain_m2))/sd(log(rain_m3 + 1 + rain_m1 + rain_m2))),
                     rain4_z = (log(rain_m4 + 1 + rain_m1 + rain_m2 + rain_m3) - mean(log(rain_m4 + 1 + rain_m1 + rain_m2 + rain_m3))/sd(log(rain_m4 + 1 + rain_m1 + rain_m2 + rain_m3))),
                     rain5_z = (log(rain_m5 + 1 + rain_m1 + rain_m2 + rain_m3 + rain_m4) - mean(log(rain_m5 + 1 + rain_m1 + rain_m2 + rain_m3 + rain_m4))/sd(log(rain_m5 + 1 + rain_m1 + rain_m2 + rain_m3 + rain_m4))), # nolint: line_length_linter.
                     rain1_zbin0 = as.numeric(rain1_z>-1 & rain1_z<1),
                     rain1_zbin1 = as.numeric(rain1_z>=1),
                     rain2_zbin0 = as.numeric(rain2_z>-1 & rain2_z<1),
                     rain2_zbin1 = as.numeric(rain2_z>=1),
                     rain3_zbin0 = as.numeric(rain3_z>-1 & rain3_z<1),
                     rain3_zbin1 = as.numeric(rain3_z>=1),
                     rain4_zbin0 = as.numeric(rain4_z>-1 & rain4_z<1),
                     rain4_zbin1 = as.numeric(rain4_z>=1),
                     rain5_zbin0 = as.numeric(rain5_z>-1 & rain5_z<1),
                     rain5_zbin1 = as.numeric(rain5_z>=1)
                     )

# yield in first observation
df <- df %>% group_by(shrid, season) %>% 
                arrange(shrid, season, year) %>% 
                mutate(original_yield = log(yield[1])) %>%
                ungroup()
df <- df %>% filter(season=="monsoon")

# Mean, divided by ten (for presentation)
df <- df %>% mutate(
                    temp_mean = (tmax_mean + tmin_mean)/20,
                    pm25 = log(pm25)
                    )

# nightlights
ntl_all <- read.csv("data/clean/village_ntl.csv")
# we want to connect to PREVIOUS year's pollution
ntl_all$year <- ntl_all$year - 1
df <- df %>% left_join(ntl_all, by = c("shrid", "year"))

df <- df %>% filter(is.na(yield)==F, is.na(ntl)==F, year>=2002 & year<=2013)


# let's see
setFixest_fml(..ctrl1 = ~ rain_z + temp_mean)
setFixest_fml(..ctrl2 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)
setFixest_fml(..ctrl3 = ~ rain1_zbin0 + rain1_zbin1 + rain2_zbin0 + rain2_zbin1 + rain3_zbin0 + rain3_zbin1 +
                rain4_zbin0 + rain4_zbin1 + rain5_zbin0 + rain5_zbin1 +
                temp_mean + 
                rain1_zbin0*temp_mean + rain1_zbin1*temp_mean + rain2_zbin0*temp_mean + rain2_zbin1*temp_mean + rain3_zbin0*temp_mean + rain3_zbin1*temp_mean +
                rain4_zbin0*temp_mean + rain4_zbin1*temp_mean + rain5_zbin0*temp_mean + rain5_zbin1*temp_mean +
                temp_mean^2 + 
                rain1_zbin0*temp_mean^2 + rain1_zbin1*temp_mean^2 + rain2_zbin0*temp_mean^2 + rain2_zbin1*temp_mean^2 + rain3_zbin0*temp_mean^2 + rain3_zbin1*temp_mean^2 +
                rain4_zbin0*temp_mean^2 + rain4_zbin1*temp_mean^2 + rain5_zbin0*temp_mean^2 + rain5_zbin1*temp_mean^2)




reg1 <- feols(ntl ~ 1 | shrid + year | pm25 ~ wind, data = df, cluster = c("shrid"))
reg2 <- feols(ntl ~ ..ctrl1 | shrid + year | pm25 ~ wind, data = df, cluster = c("shrid"))
reg3 <- feols(ntl ~ ..ctrl2 | shrid + year | pm25 ~ wind, data = df, cluster = c("shrid"))
reg4 <- feols(ntl ~ ..ctrl3 | shrid + year | pm25 ~ wind, data = df, cluster = c("shrid"))

ntltable <- etable(
                      reg1, reg2, reg3, reg4,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r0",
                      fitstat = c("ivwald", "n"),
                      coefstat = "se",
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain_z square",
                                   conrols_expanded2 = "rain1_zbin0"),
                      keep = c("pm25")
                    )






reg1 <- feols(ntl ~ pm25 | shrid + year, data = df, cluster = c("shrid"))
reg2 <- feols(ntl ~ pm25 + ..ctrl1 | shrid + year, data = df, cluster = c("shrid"))
reg3 <- feols(ntl ~ pm25 + ..ctrl2 | shrid + year, data = df, cluster = c("shrid"))
reg4 <- feols(ntl ~ pm25 + ..ctrl3 | shrid + year, data = df, cluster = c("shrid"))

ntltable <- etable(
                      reg1, reg2, reg3, reg4,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r0",
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain_z square",
                                   conrols_expanded2 = "rain1_zbin0"),
                      keep = c("pm25")
                    )







# standardize wind
df <- df %>% mutate(wind = (wind - mean(wind))/sd(wind))
# standardize pm25
df <- df %>% mutate(pm25 = (pm25 - mean(pm25))/sd(pm25))

reg1 <- feglm(ntl ~ wind | shrid + year, data = df, cluster = c("shrid"), family = poisson(link = "log"))
reg2 <- feglm(ntl ~ wind + ..ctrl1 | shrid + year, data = df, cluster = c("shrid"), family = poisson(link = "log"))
reg3 <- feglm(ntl ~ wind + ..ctrl2 | shrid + year, data = df, cluster = c("shrid"), family = poisson(link = "log"))
reg4 <- feglm(ntl ~ wind + ..ctrl3 | shrid + year, data = df, cluster = c("shrid"), family = poisson(link = "log"))

ntltable <- etable(
                      reg1, reg2, reg3, reg4,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r0",
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain_z square",
                                   conrols_expanded2 = "rain1_zbin0"),
                      keep = c("wind")
                    )






reg1 <- feglm(ntl ~ pm25 | shrid + year, data = df, cluster = c("shrid"), family = poisson(link = "log"))
reg2 <- feglm(ntl ~ pm25 + ..ctrl1 | shrid + year, data = df, cluster = c("shrid"), family = poisson(link = "log"))
reg3 <- feglm(ntl ~ pm25 + ..ctrl2 | shrid + year, data = df, cluster = c("shrid"), family = poisson(link = "log"))
reg4 <- feglm(ntl ~ pm25 + ..ctrl3 | shrid + year, data = df, cluster = c("shrid"), family = poisson(link = "log"))

ntltable <- etable(
                      reg1, reg2, reg3, reg4,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r0",
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain_z square",
                                   conrols_expanded2 = "rain1_zbin0"),
                      keep = c("pm25")
                    )











