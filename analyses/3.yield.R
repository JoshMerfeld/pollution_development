# Purpose: This script estimates effects for yields. I save the results to load into the markdown script
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
library(pbmcapply)

# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
# Double check
getwd()    # check



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
                     rain5_zbin1 = as.numeric(rain5_z>=1),
                     state = substr(shrid, 1, 2)
                     )


# yield in first observation
df <- df %>% group_by(shrid, season) %>% 
                arrange(shrid, season, year) %>% 
                mutate(original_yield = log(yield[1])) %>%
                ungroup()


# And district identifiers
villages_overlap <- read_sf("data/spatial/villages_overlap/villages_overlap.shp") %>% st_set_crs(st_crs("EPSG:24378"))
villages_overlap <- villages_overlap %>% 
                      mutate(
                             shrid = paste0(pc11_s_id, "-", pc11_tv_id),
                             distfe = paste0(pc11_s_id, "-", pc11_d_id)
                             ) %>%
                      as_tibble() %>%
                      dplyr::select(shrid, distfe) %>%
                      group_by(shrid) %>%
                      filter(row_number()==1) %>%
                      ungroup()
df <- df %>% left_join(villages_overlap, by = "shrid")

# Mean, divided by ten (for presentation)
df <- df %>% mutate(
                    temp_mean = (tmax_mean + tmin_mean)/20,
                    pm25 = log(pm25)
                    )


# Figure out which year plant opens
villages_plants <- read.csv(paste0("data/clean/wind_ntl/dist_matrix_angles.csv"))
villages_plants <- villages_plants %>%
                      dplyr::select(shrid, plant_id, year_built, year_retired)
unique_shrid <- unique(villages_plants$shrid)
                      
# create new dataset
plant_data <- c()
for (y in 1999:2015){
  temp <- as_tibble(unique_shrid)
  temp$year <- y
  colnames(temp) <- c("shrid", "year")
  plant_data <- rbind(plant_data, temp)
}
# continue...
plants <- c()
for (y in 1999:2015){
  temp <- villages_plants %>%
            filter(year_built<=y & year_retired>=y) %>%
            dplyr::select(shrid) %>%
            group_by(shrid) %>%
            filter(row_number()==1) %>%
            ungroup()
  temp$year <- y
  plants <- rbind(plants, temp)
}
plants$open <- 1
plant_data <- plant_data %>%
                left_join(plants, by = c("shrid", "year"))
rm(plants)
plant_data$open <- as.numeric(is.na(plant_data$open)==F)

# add to df
df <- df %>%
      left_join(plant_data, by = c("shrid", "year"))
rm(plant_data)

# MONSOON ONLY
df <- df %>%
      filter(season=="monsoon")





summary(df$wind_abs_dev)
summary(df$pm25_abs_dev)
summary(exp(df$pm25))

crops <- read_csv(paste0("data/clean/ag_productivity/crop_area.csv"))
df <- df %>%
      left_join(crops, by = c("shrid"))



# # get lat/lon
# villages_overlap <- read_sf("data/spatial/villages_overlap/villages_overlap.shp") %>% st_set_crs(st_crs("EPSG:24378"))
# villages_overlap <- villages_overlap %>% 
#                       mutate(
#                              shrid = paste0(pc11_s_id, "-", pc11_tv_id)
#                              ) %>%
#                       dplyr::select(shrid) %>%
#                       group_by(shrid) %>%
#                       filter(row_number()==1) %>%
#                       ungroup()
# # to centroid
# villages_overlap <- villages_overlap %>% st_centroid() 
# villages_overlap <- st_transform(villages_overlap, crs = 4326)
# shrid <- villages_overlap$shrid
# # to lat/lon
# villages_overlap <- st_coordinates(villages_overlap)
# villages_overlap <- as_tibble(cbind(shrid, villages_overlap))
# colnames(villages_overlap) <- c("shrid", "lon", "lat")

# villages_overlap$lon <- as.numeric(villages_overlap$lon)
# villages_overlap$lat <- as.numeric(villages_overlap$lat)

# df <- df %>%
#       left_join(villages_overlap, by = c("shrid"))



################################################################################################################################################
################################################################################################################################################
################################################################################################################################################





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


# Regressions ---------------------------------

## reduced form ---------------------------------

yield1 <- feols(log(yield) ~ wind | shrid + year, data = df, cluster = c("shrid"))
yield2 <- feols(log(yield) ~ wind + ..ctrl1 | shrid + year, data = df, cluster = c("shrid"))
yield3 <- feols(log(yield) ~ wind + ..ctrl2 | shrid + year, data = df, cluster = c("shrid"))
yield4 <- feols(log(yield) ~ wind + ..ctrl3 | shrid + year, data = df, cluster = c("shrid"))

yieldtable <- etable(
                     yield1, yield2, yield3, yield4,
                     se.below = TRUE,
                     depvar = FALSE,
                     signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                     digits = "r4",
                     fitstat = c("n"),
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain_z square",
                                   conrols_expanded2 = "rain1_zbin0"),
                     coefstat = "se",
                     keep = c("wind")
                     )
# rename
yieldtable <- yieldtable[-c(9:10),]
yieldtable[6,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("wind days", " ", 
                          "weather", "weather (expanded)", "weather (expanded, bins)",
                          "fixed effects:", "village", "year",
                          "observations")
yieldtable <- yieldtable[,-1]
saveRDS(yieldtable, "pollution_development/draft/tables/yield1reducedform.rds")








## naive regression ---------------------------------

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

yield1 <- feols(log(yield) ~ pm25 | shrid + year, data = df, cluster = c("shrid"))
yield2 <- feols(log(yield) ~ pm25 + ..ctrl1 | shrid + year, data = df, cluster = c("shrid"))
yield3 <- feols(log(yield) ~ pm25 + ..ctrl2 | shrid + year, data = df, cluster = c("shrid"))
yield4 <- feols(log(yield) ~ pm25 + ..ctrl3 | shrid + year, data = df, cluster = c("shrid"))

yieldtable <- etable(
                      yield1, yield2, yield3, yield4,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain_z square",
                                   conrols_expanded2 = "rain1_zbin0"),
                      keep = c("pm25")
                    )
# rename
yieldtable <- yieldtable[-c(9:10),]
yieldtable[6,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(log PM 2.5)", 
                          "weather", "weather (expanded)", "weather (expanded, bins)",
                          "fixed effects:", "village", "year",
                          "observations")
yieldtable <- yieldtable[,-1]
saveRDS(yieldtable, "pollution_development/draft/tables/yield2naive.rds")






## main IV results ---------------------------------

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

yield1 <- feols(log(yield) ~ 1 | shrid + year | pm25 ~ wind, 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ ..ctrl1 | shrid + year | pm25 ~ wind, 
                data = df, 
                cluster = c("shrid"))
yield3 <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 ~ wind, 
                data = df, 
                cluster = c("shrid"))
yield4 <- feols(log(yield) ~ ..ctrl3 | shrid + year | pm25 ~ wind, 
                data = df, 
                cluster = c("shrid"))


yieldtable1 <- etable(
                      yield1, yield2, yield3, yield4,
                      stage = 1,
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
                      keep = c("wind")
                    )


# rename
yieldtable1 <- yieldtable1[-c(9:10),]
yieldtable1[6,] <- " "
yieldtable1 <- as.matrix(yieldtable1)
rownames(yieldtable1) <- c("wind", "",
                          "weather", "weather (expanded)", "weather (expanded, bins)",
                          "fixed effects:", "village", "year",
                          "F", 
                          "observations")
saveRDS(yieldtable1, "pollution_development/draft/tables/yield3ivmain_firststage.rds")


yieldtable <- etable(
                      yield1, yield2, yield3, yield4,
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
# rename
yieldtable <- yieldtable[-c(9:10),]
yieldtable[6,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(log PM 2.5)",
                          "weather", "weather (expanded)", "weather (expanded, bins)",
                          "fixed effects:", "village", "year",
                          "F", 
                          "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yield3ivmain.rds")









##################
## placebo test ##
##################
setFixest_fml(..ctrl3 = ~ rain1_zbin0 + rain1_zbin1 + rain2_zbin0 + rain2_zbin1 + rain3_zbin0 + rain3_zbin1 +
                rain4_zbin0 + rain4_zbin1 + rain5_zbin0 + rain5_zbin1 +
                temp_mean + 
                rain1_zbin0*temp_mean + rain1_zbin1*temp_mean + rain2_zbin0*temp_mean + rain2_zbin1*temp_mean + rain3_zbin0*temp_mean + rain3_zbin1*temp_mean +
                rain4_zbin0*temp_mean + rain4_zbin1*temp_mean + rain5_zbin0*temp_mean + rain5_zbin1*temp_mean +
                temp_mean^2 + 
                rain1_zbin0*temp_mean^2 + rain1_zbin1*temp_mean^2 + rain2_zbin0*temp_mean^2 + rain2_zbin1*temp_mean^2 + rain3_zbin0*temp_mean^2 + rain3_zbin1*temp_mean^2 +
                rain4_zbin0*temp_mean^2 + rain4_zbin1*temp_mean^2 + rain5_zbin0*temp_mean^2 + rain5_zbin1*temp_mean^2)


# create function to sample
sample_pre_fun <- function(a) {
  sample(x = min(a):max(a), size = 1, replace = T)
}

# now wrap that function in apply
sample_fun <- function(a) {
  sample_vec <- apply(a, 1, sample_pre_fun)
}




wind_year <- df %>%
  dplyr::select(wind, pm25, distfe, year) %>%
  # just districts with at least 50 observations
  arrange(year, distfe) %>% # order by year/distfe
  mutate(row = row_number()) %>%
  group_by(year, distfe) %>%
  mutate(
    row_min = min(row), # min row number
    row_max = max(row) # max
  ) %>%
  ungroup() %>%
  arrange(year, distfe) # order by year/distfe
df_placebo <- df %>%
  ungroup() %>%
  dplyr::select(-c(wind, pm25)) %>%
  arrange(year, distfe) # order by year/distfe
# double check same order
min(wind_year$distfe==df_placebo$distfe)

samplefrom <- cbind(wind_year$row_min, wind_year$row_max)

f <- function(i) {
  set.seed(i)
  # create vector with random sampling
  # take those rows
  wind_all <- wind_year[sample_fun(samplefrom), c("wind", "pm25")]
  # wind_all is in same order as df_placebo
  # just cbind the random wind column
  temp <- cbind(df_placebo, wind_all)
  # get coefficient
  coefs <- feols(log(yield) ~ ..ctrl3 | shrid + year | pm25 ~ wind,  data = temp)
  # return
  return(coefs$coeftable[1,1])
}

# use mclapply to take advantage of the cores on this computer
# should take around an hour
coefs_dist <- pbmclapply(1:500, f, mc.cores = 10)
coefs_dist <- as_vector(coefs_dist)




# get true value (with df)
true_value <- feols(log(yield) ~ ..ctrl3 | shrid + year | pm25 ~ wind, 
                    data = df, 
                    cluster = c("shrid"))
true_value <- true_value$coeftable[1,1]

# true: -0.63
# lowest from placebo: -0.59

mean(coefs_dist<true_value) # 0.003 (3 of 1,000)



pal <- viridis(4)
colors <- c(paste0(pal[1]))

# plot
gg1 <- ggplot() + 
  geom_density(aes(x = coefs_dist, fill = "within district"), alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(
    name = "randomization:",
    values = colors
  ) +
  theme_minimal() +
  xlab("distribution of coefficients")

colors <- c(paste0(pal[3]))
labels <- c("actual")

gg1 <- gg1 + 
  geom_vline(aes(xintercept = true_value, color = "actual")) +
  scale_color_manual(
    name = "",
    values = colors,
    labels = labels
  ) + 
  geom_vline(aes(xintercept = -0.8), color = "transparent")
saveRDS(gg1, "pollution_development/draft/tables/randomization.rds")
saveRDS(coefs_dist, "pollution_development/draft/tables/randomization_coefs_distfe.rds")
saveRDS(true_value, "pollution_development/draft/tables/randomization_true_value.rds")





# leads
df <- panel(df, ~ shrid + year)

setFixest_fml(..ctrl3 = ~ rain1_zbin0 + rain1_zbin1 + rain2_zbin0 + rain2_zbin1 + rain3_zbin0 + rain3_zbin1 +
                rain4_zbin0 + rain4_zbin1 + rain5_zbin0 + rain5_zbin1 +
                temp_mean + 
                rain1_zbin0*temp_mean + rain1_zbin1*temp_mean + rain2_zbin0*temp_mean + rain2_zbin1*temp_mean + rain3_zbin0*temp_mean + rain3_zbin1*temp_mean +
                rain4_zbin0*temp_mean + rain4_zbin1*temp_mean + rain5_zbin0*temp_mean + rain5_zbin1*temp_mean +
                temp_mean^2 + 
                rain1_zbin0*temp_mean^2 + rain1_zbin1*temp_mean^2 + rain2_zbin0*temp_mean^2 + rain2_zbin1*temp_mean^2 + rain3_zbin0*temp_mean^2 + rain3_zbin1*temp_mean^2 +
                rain4_zbin0*temp_mean^2 + rain4_zbin1*temp_mean^2 + rain5_zbin0*temp_mean^2 + rain5_zbin1*temp_mean^2)

setFixest_fml(..ctrl1 = ~ rain_z + temp_mean + rain_z*temp_mean)

setFixest_fml(..ctrl1 = ~ f(rain_z, 0:1) + f(temp_mean, 0:1) + f(rain_z*temp_mean, 0:1))
setFixest_fml(..ctrl2 = ~ f(rain_z, 0:2) + f(temp_mean, 0:2) + f(rain_z*temp_mean, 0:2))


yield1 <- feols(log(yield) ~ f(..ctrl3, 0:1) | shrid + year | f(pm25, 1) ~ f(wind, 0:1), 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ f(..ctrl3, 0:2) | shrid + year | f(pm25, 2) ~ f(wind, 0:2), 
                data = df, 
                cluster = c("shrid"))


yieldtable <- etable(
                     yield1, yield2,
                     se.below = TRUE,
                     depvar = FALSE,
                     signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                     digits = "r3",
                     digits.stats = "r0",
                     fitstat = c("ivwald", "n"),
                     coefstat = "se",
                     group = list(controls = "rain_z",
                                  conrols_expanded = "rain_z square"),
                     keep = c("pm25")
                     )
# rename
yieldtable <- yieldtable[-c(5,10:11),]
yieldtable[6,] <- " "
yieldtable[9,3] <- yieldtable[10,3]
yieldtable <- yieldtable[-10,]
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(one-year lead)",
                          "particulate matter", "(two-year lead)",
                          "weather (expanded)",
                          "fixed effects:", "village", "year",
                          "F", 
                          "observations")
yieldtable <- yieldtable[,-1]
saveRDS(yieldtable, "pollution_development/draft/tables/yield3ivmain_lead.rds")



coefs <- c()
for (i in -2:2){
  yield1 <- feols(log(yield) ~ f(wind, i) + f(..ctrl3, c(i, 0)) | shrid + year^state, 
                data = df, 
                cluster = c("shrid"))
  coefs <- c(coefs, coefficients(yield1)[1])
}
coefsstate <- coefs
coefs <- c()
for (i in -2:2){
  yield1 <- feols(log(yield) ~ f(wind, i) + f(..ctrl3, c(i, 0)) | shrid[year] + year, 
                data = df, 
                cluster = c("shrid"))
  coefs <- c(coefs, coefficients(yield1)[1])
}
coefstrends <- coefs
coefs <- c()
for (i in -2:2){
  yield1 <- feols(log(yield) ~ f(wind, i) + f(..ctrl3, c(i, 0)) | shrid + year, 
                data = df, 
                cluster = c("shrid"))
  coefs <- c(coefs, coefficients(yield1)[1])
}
coefsboth <- c()
for (i in -2:2){
  yield1 <- feols(log(yield) ~ f(wind, i) + f(..ctrl3, c(i, 0)) | shrid[year] + year^state, 
                data = df, 
                cluster = c("shrid"))
  coefsboth <- c(coefsboth, coefficients(yield1)[1])
}


pal <- viridis(4)

coeflist <- list(coefsstate, coefstrends, coefsboth, coefs)
saveRDS(coeflist, "pollution_development/draft/tables/yield3ivmain_lead_lag_coefs.rds")
ggplot() +
  geom_point(aes(x = c(-2:2), y = coefsstate, color = "State-by-year FE", shape = "State-by-year FE")) +
  geom_point(aes(x = c(-2:2), y = coefstrends, color = "Village-year trends", shape = "Village-year trends")) +
  geom_point(aes(x = c(-2:2), y = coefsboth, color = "Both", shape = "Both")) +
  geom_point(aes(x = c(-2:2), y = coefs, color = "Baseline", shape = "Baseline")) +
  theme_minimal() +
  scale_color_manual(" ", breaks = c("Baseline", "State-by-year FE", "Village-year trends", "Both"),
                      values = c("State-by-year FE" = paste0(pal[1]), "Village-year trends" = paste0(pal[2]), 
                                  "Both" = paste0(pal[3]), "Baseline" = paste0(pal[4]))) +
  scale_shape_manual(" ", breaks = c("Baseline", "State-by-year FE", "Village-year trends", "Both"),
                      values = c("State-by-year FE" = 1, "Village-year trends" = 2, 
                                  "Both" = 3, "Baseline" = 4)) +
  labs(x = " ", y = "Reduced-form coefficient") +
  theme(legend.position = c(0.8, 0.9)) +
  scale_x_continuous(labels = c("Lag (2 years)", "Lag (1 year)", "Current", "Lead (1 year)", "Lead (2 years)"))






## placebo test
wind_year <- df %>%
              dplyr::select(wind, pm25, year) %>%
              arrange(year) %>%
              mutate(row = row_number()) %>% # row number in entire thing
              group_by(year) %>%
              mutate(
                row_min = min(row), # min row number (in entire column)
                row_max = max(row) # max
              ) %>%
              ungroup() %>%
              arrange(year)
df_placebo <- df %>%
              dplyr::select(-c(wind, pm25)) %>%
              arrange(year)

samplefrom <- cbind(wind_year$row_min, wind_year$row_max)


# create function to sample
sample_pre_fun <- function(a) {
  sample(x = min(a):max(a), size = 1, replace = T)
}

# now wrap that function in apply
sample_fun <- function(a) {
  sample_vec <- apply(a, 1, sample_pre_fun)
}

f <- function(i) {
  # create vector with random sampling
  temp_sample <- sample_fun(samplefrom)
  # take those rows
  wind_all <- wind_year[temp_sample,]
  # sort by year, district
  wind_all <- wind_all %>%
                  arrange(year)
  # sort placebo by year and district
  df_placebo <- df_placebo %>%
                  arrange(year)
  # should be in same order, so add wind column
  temp <- cbind(df_placebo, wind_all %>% dplyr::select(wind, pm25))
  # get coefficient
  coefs <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 ~ wind, data = temp, cluster = c("shrid"))
  # return
  return(coefs$coeftable[1,1])
}
RNGkind("L'Ecuyer-CMRG")
set.seed(234678) #resetting seed just to make sure this works correctly when using parallel
coef_vec <- mclapply(1:1000, f, mc.set.seed = TRUE)
coef_vec <- as_vector(coef_vec)



# Also get distribution if we resample wind from WITHIN DISTRICTS ONLY
## placebo test
wind_year <- df %>%
              dplyr::select(wind, pm25, distfe, year) %>%
              arrange(year, distfe) %>%
              mutate(row = row_number()) %>% # row number in entire thing
              group_by(year, distfe) %>%
              mutate(
                row_min = min(row), # min row number (in entire column)
                row_max = max(row), # max
                num = max(row_number()) # row number WITHIN 
              ) %>%
              ungroup() %>%
              arrange(year, distfe)
df_placebo <- df %>%
              dplyr::select(-c(wind, pm25)) %>%
              arrange(year, distfe)

samplefrom <- cbind(wind_year$row_min, wind_year$row_max)

f <- function(i) {
  # create vector with random sampling
  temp_sample <- sample_fun(samplefrom)
  # take those rows
  wind_all <- wind_year[temp_sample,]
  # sort by year, district
  wind_all <- wind_all %>%
                arrange(year, distfe)
  # sort placebo by year and district
  df_placebo <- df_placebo %>%
                  arrange(year, distfe)
  # should be in same order, so add wind column
  temp <- cbind(df_placebo, wind_all %>% dplyr::select(wind, pm25))
  # get coefficient
  coefs <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 ~ wind, data = temp, cluster = c("shrid"))
  # return
  return(coefs$coeftable[1,1])
}
# use mclapply to take advantage of the cores on this computer
# should take around an hour
set.seed(67891235) #resetting seed just to make sure this works correctly when using parallel
coefs_distfe <- mclapply(1:1000, f, mc.set.seed = TRUE)
coefs_distfe <- as_vector(coefs_distfe)




# get true value (with df)
true_value <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 ~ wind, 
                    data = df, 
                    cluster = c("shrid"))
true_value <- true_value$coeftable[1,1]




pal <- viridis(4)
colors <- c(paste0(pal[1]), paste0(pal[2]))
labels <- c("all", "within district")

# plot
gg1 <- ggplot() + 
        geom_density(aes(x = coef_vec, fill = "all"), alpha = 0.5) +
        geom_density(aes(x = coefs_distfe, fill = "within district"), alpha = 0.5) +
        scale_fill_manual(
          name = "randomization:",
          values = colors,
          labels = labels
        ) +
        theme_minimal() +
        xlab("distribution of coefficients")

colors <- c(paste0(pal[3]))
labels <- c("actual")

gg1 <- gg1 + 
        geom_vline(aes(xintercept = true_value, color = "actual")) +
        scale_color_manual(
          name = "",
          values = colors,
          labels = labels
        ) +
        theme(legend.position = c(0.4, 0.6))
gg1
ggsave("pollution_development/draft/tables/randomization_iv.png")
saveRDS(coef_vec, "pollution_development/draft/tables/randomization_coef_vec.rds")
saveRDS(coefs_distfe, "pollution_development/draft/tables/randomization_coefs_distfe.rds")
saveRDS(true_value, "pollution_development/draft/tables/randomization_true_value.rds")



# get percentile
coefs_distfe <- sort(coefs_distfe)
row <- 1
while (row<=length(coefs_distfe)){
  if (true_value<coefs_distfe[row]){
    row <- row + 1
  }else{
    break
  }
}
row # larger than every single randomized value!











# randomization for current/lead
# create function to sample
sample_pre_fun <- function(a) {
  sample(x = min(a):max(a), size = 1, replace = T)
}

# now wrap that function in apply
sample_fun <- function(a) {
  sample_vec <- apply(a, 1, sample_pre_fun)
}

# Also get distribution if we resample wind from WITHIN DISTRICTS ONLY
## placebo test
df <- as_tibble(df)
wind_year <- df %>%
  dplyr::select(wind, pm25, distfe, year) %>%
  arrange(year, distfe) %>%
  mutate(row = row_number()) %>% # row number in entire thing
  group_by(year, distfe) %>%
  mutate(
    row_min = min(row), # min row number (in entire column)
    row_max = max(row), # max
    num = max(row_number()) # row number WITHIN 
  ) %>%
  ungroup() %>%
  arrange(year, distfe)
df_placebo <- df %>%
  dplyr::select(-wind) %>%
  arrange(year, distfe)

samplefrom <- cbind(wind_year$row_min, wind_year$row_max)

f <- function(i) {
  # create vector with random sampling
  temp_sample <- sample_fun(samplefrom)
  # take those rows
  wind_all <- wind_year[temp_sample,]
  # sort by year, district
  wind_all <- wind_all %>%
    arrange(year, distfe)
  # sort placebo by year and district
  df_placebo <- df_placebo %>%
    arrange(year, distfe)
  # should be in same order, so add wind column
  temp <- cbind(df_placebo, wind_all %>% dplyr::select(wind, pm25))
  # get coefficient
  temp <- panel(temp, ~ shrid + year)
  coefs <- feols(log(yield) ~ ..ctrl2 | shrid + year | f(pm25, 0:1) ~ f(wind, 0:1), 
                 data = temp, 
                 cluster = c("shrid"))
  # return
  return(coefs$coeftable[1:2,1])
}

# use mclapply to take advantage of the cores on this computer
# should take around an hour
set.seed(8732946) #resetting seed just to make sure this works correctly when using parallel
coefs_distfe <- mclapply(1:1000, f, mc.set.seed = TRUE)
coef_current <- c()
coef_lead <- c()
for (num in 1:1000){
  coef_current <- c(coef_current, coefs_distfe[[num]][1])
  coef_lead <- c(coef_lead, coefs_distfe[[num]][2])
}
coef_current <- as_vector(coef_current)
coef_lead <- as_vector(coef_lead)

saveRDS(coef_current, "pollution_development/draft/tables/coef_current.rds")
saveRDS(coef_lead, "pollution_development/draft/tables/coef_lead.rds")

# get true value (with df)
true_value <- feols(log(yield) ~ ..ctrl2 | shrid + year | f(pm25, 0:1) ~ f(wind, 0:1), 
                    data = df, 
                    cluster = c("shrid"))
true_value_current <- true_value$coeftable[1,1]
true_value_lead <- true_value$coeftable[2,1]


pal <- viridis(4)
colors <- c(paste0(pal[1]), paste0(pal[2]))
labels <- c("current", "lead")

# plot
ggplot() + 
    geom_density(aes(x = coef_current, fill = "current"), alpha = 0.5) +
    geom_density(aes(x = coef_lead, fill = "lead"), alpha = 0.5) +
    scale_fill_manual(
      name = "coefficient:",
      values = colors,
      labels = labels
    ) +
    theme_minimal() +
    xlab("distribution of coefficients")

# what percentile is zero?
coef_lead <- sort(coef_lead)
i <- 1
while (i<=length(coef_lead)){
  if (coef_lead[i]>=0){
    print(i)
    break
  }
  i <- i + 1
}











## by crop ---------------------------------
df$rice <- as.numeric(df$most_common=="rice")
df$pmrice <- df$rice*df$pm25
df$windrice <- df$rice*df$wind

setFixest_fml(..ctrl2 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)
setFixest_fml(..ctrl3 = ~ rain1_zbin0 + rain1_zbin1 + rain2_zbin0 + rain2_zbin1 + rain3_zbin0 + rain3_zbin1 +
                rain4_zbin0 + rain4_zbin1 + rain5_zbin0 + rain5_zbin1 +
                temp_mean + 
                rain1_zbin0*temp_mean + rain1_zbin1*temp_mean + rain2_zbin0*temp_mean + rain2_zbin1*temp_mean + rain3_zbin0*temp_mean + rain3_zbin1*temp_mean +
                rain4_zbin0*temp_mean + rain4_zbin1*temp_mean + rain5_zbin0*temp_mean + rain5_zbin1*temp_mean +
                temp_mean^2 + 
                rain1_zbin0*temp_mean^2 + rain1_zbin1*temp_mean^2 + rain2_zbin0*temp_mean^2 + rain2_zbin1*temp_mean^2 + rain3_zbin0*temp_mean^2 + rain3_zbin1*temp_mean^2 +
                rain4_zbin0*temp_mean^2 + rain4_zbin1*temp_mean^2 + rain5_zbin0*temp_mean^2 + rain5_zbin1*temp_mean^2)


yield1 <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 + pmrice ~ wind + windrice, 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ ..ctrl3 | shrid + year | pm25 + pmrice ~ wind + windrice, 
                data = df, 
                cluster = c("shrid"))


yieldtable <- etable(
                      yield1, yield2,
                      stage = 2,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r0",
                      fitstat = c("ivwald", "n"),
                      coefstat = "se",
                      group = list(conrols_expanded = "rain_z square",
                                   conrols_expanded2 = "rain1_zbin0"),
                      keep = c("pm25", "pmrice")
                    )

# rename
yieldtable <- yieldtable[-c(10:11),]
yieldtable[7,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(log PM 2.5)",
                          "PM 2.5 times rice", " ",
                          "weather (expanded)", "weather (expanded, bins)",
                          "fixed effects:", "village", "year",
                          "F (PM 2.5)", "F (PM 2.5 times rice)", 
                          "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yieldrice.rds")











### with state FE ---------------------------------
df$state <- substr(df$shrid, 1, 2)

setFixest_fml(..ctrl2 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)
setFixest_fml(..ctrl3 = ~ rain1_zbin0 + rain1_zbin1 + rain2_zbin0 + rain2_zbin1 + rain3_zbin0 + rain3_zbin1 +
                rain4_zbin0 + rain4_zbin1 + rain5_zbin0 + rain5_zbin1 +
                temp_mean + 
                rain1_zbin0*temp_mean + rain1_zbin1*temp_mean + rain2_zbin0*temp_mean + rain2_zbin1*temp_mean + rain3_zbin0*temp_mean + rain3_zbin1*temp_mean +
                rain4_zbin0*temp_mean + rain4_zbin1*temp_mean + rain5_zbin0*temp_mean + rain5_zbin1*temp_mean +
                temp_mean^2 + 
                rain1_zbin0*temp_mean^2 + rain1_zbin1*temp_mean^2 + rain2_zbin0*temp_mean^2 + rain2_zbin1*temp_mean^2 + rain3_zbin0*temp_mean^2 + rain3_zbin1*temp_mean^2 +
                rain4_zbin0*temp_mean^2 + rain4_zbin1*temp_mean^2 + rain5_zbin0*temp_mean^2 + rain5_zbin1*temp_mean^2)


yield1 <- feols(log(yield) ~ ..ctrl2 | shrid + year^state | pm25 + pmrice ~ wind + windrice, 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ ..ctrl3 | shrid + year^state | pm25 + pmrice ~ wind + windrice, 
                data = df, 
                cluster = c("shrid"))


yieldtable <- etable(
                      yield1, yield2,
                      stage = 2,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r0",
                      fitstat = c("ivwald", "n"),
                      coefstat = "se",
                      group = list(conrols_expanded = "rain_z square",
                                   conrols_expanded2 = "rain1_zbin0"),
                      keep = c("pm25", "pmrice")
                    )

# rename
yieldtable <- yieldtable[-c(10:11),]
yieldtable[7,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(log PM 2.5)",
                          "PM 2.5 times rice", " ",
                          "weather (expanded)", "weather (expanded, bins)",
                          "fixed effects:", "village", "state-by-byear",
                          "F (PM 2.5)", "F (PM 2.5 times rice)", 
                          "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yieldricestatefe.rds")





























## monthly results ---------------------------------

setFixest_fml(..ctrl2 = ~ rain1_zbin0 + rain1_zbin1 + rain2_zbin0 + rain2_zbin1 + rain3_zbin0 + rain3_zbin1 +
                rain4_zbin0 + rain4_zbin1 + rain5_zbin0 + rain5_zbin1 +
                temp_mean + 
                rain1_zbin0*temp_mean + rain1_zbin1*temp_mean + rain2_zbin0*temp_mean + rain2_zbin1*temp_mean + rain3_zbin0*temp_mean + rain3_zbin1*temp_mean +
                rain4_zbin0*temp_mean + rain4_zbin1*temp_mean + rain5_zbin0*temp_mean + rain5_zbin1*temp_mean +
                temp_mean^2 + 
                rain1_zbin0*temp_mean^2 + rain1_zbin1*temp_mean^2 + rain2_zbin0*temp_mean^2 + rain2_zbin1*temp_mean^2 + rain3_zbin0*temp_mean^2 + rain3_zbin1*temp_mean^2 +
                rain4_zbin0*temp_mean^2 + rain4_zbin1*temp_mean^2 + rain5_zbin0*temp_mean^2 + rain5_zbin1*temp_mean^2)

yield1 <- feols(log(yield) ~ ..ctrl2 | 
                  shrid + year | 
                  pm1 + pm2 + pm3 + pm4 + pm5 ~ m1 + m2 + m3 + m4 + m5,
                data = df, 
                cluster = "shrid")

confint <- confint(yield1)[1:5,]
coefs <- coefficients(yield1)[1:5]
savelist <- list(confint = confint, coefs = coefs)
# save
saveRDS(savelist, "pollution_development/draft/tables/monthcoefestimates.rds")









## heterogeneity ---------------------------------

df <- df %>% group_by(shrid, season) %>% 
                mutate(
                       wind_med = median(wind, na.rm = T),
                       yield_med = median(yield, na.rm = T)
                       ) %>% 
                ungroup() %>% 
                mutate(
                       wind_med_median = median(wind_med, na.rm = T),
                       yield_med_median = median(yield_med, na.rm = T)
                       )


df$pmrain <- (df$pm25*df$rain_z)*0.1 # divided by ten for the coefficient
df$windrain <- df$wind*df$rain_z
df$pmyield <- df$pm25*df$original_yield
df$windyield <- df$wind*df$original_yield

setFixest_fml(..ctrl1 = ~ rain_z + temp_mean)
setFixest_fml(..ctrl2 = ~ rain1_zbin0 + rain1_zbin1 + rain2_zbin0 + rain2_zbin1 + rain3_zbin0 + rain3_zbin1 +
                rain4_zbin0 + rain4_zbin1 + rain5_zbin0 + rain5_zbin1 +
                temp_mean + 
                rain1_zbin0*temp_mean + rain1_zbin1*temp_mean + rain2_zbin0*temp_mean + rain2_zbin1*temp_mean + rain3_zbin0*temp_mean + rain3_zbin1*temp_mean +
                rain4_zbin0*temp_mean + rain4_zbin1*temp_mean + rain5_zbin0*temp_mean + rain5_zbin1*temp_mean +
                temp_mean^2 + 
                rain1_zbin0*temp_mean^2 + rain1_zbin1*temp_mean^2 + rain2_zbin0*temp_mean^2 + rain2_zbin1*temp_mean^2 + rain3_zbin0*temp_mean^2 + rain3_zbin1*temp_mean^2 +
                rain4_zbin0*temp_mean^2 + rain4_zbin1*temp_mean^2 + rain5_zbin0*temp_mean^2 + rain5_zbin1*temp_mean^2)

yield1 <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 ~ wind, 
                data = df %>% filter(wind_med>wind_med_median),
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 ~ wind, 
                data = df %>% filter(wind_med<=wind_med_median),
                cluster = c("shrid"))
yield3 <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 ~ wind, 
                data = df %>% filter(yield_med>yield_med_median),
                cluster = c("shrid"))
yield4 <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 ~ wind, 
                data = df %>% filter(yield_med<=yield_med_median),
                cluster = c("shrid"))
yield5 <- feols(log(yield) ~ ..ctrl2 | shrid + year | pm25 + pmrain ~ wind + windrain, 
                data = df, 
                cluster = c("shrid"))

yieldtablehet <- etable(
                        yield1, yield2, yield3, yield4, yield5,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = "r3",
                        digits.stats = "r0",
                        fitstat = c("ivwald", "n"),
                        coefstat = "se",
                        #headers = list("wind" = 2, "yield" = 2, ""),
                        group = list(weather = "rain1_zbin0"),
                        keep = c("pm25", "pmrain")
                        )
yieldtablehet <- yieldtablehet[,-1]
yieldtablehet <- yieldtablehet[-c(9,10),]
yieldtablehet[5,] <- "Yes"
yieldtablehet[6,] <- ""
yieldtablehet <- as.matrix(yieldtablehet)
rownames(yieldtablehet) <- c("pm (log)", "", "pm times rain", "", "weather (expanded, bins)",
                             "fixed effects:", "village", "year", 
                             "F (1st stage, PM)", "F (1st stage, PM times rain)", "observations")
yieldtablehet[10,1:4] <- " "
colnames(yieldtablehet) <- c(">p(50)", "<=p(50)", ">p(50)", "<=p(50)", "")
saveRDS(yieldtablehet, "pollution_development/draft/tables/yield5heterogeneity.rds")


















## diff-in-diff IV ---------------------------------

df$pmopen <- df$pm25*df$open
df$windopen <- df$wind*df$open
df$pmyear <- df$pm25*df$year
df$windyear <- df$wind*df$year

setFixest_fml(..ctrl1 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)
setFixest_fml(..ctrl2 = ~ rain1_zbin0 + rain1_zbin1 + rain2_zbin0 + rain2_zbin1 + rain3_zbin0 + rain3_zbin1 +
                rain4_zbin0 + rain4_zbin1 + rain5_zbin0 + rain5_zbin1 +
                temp_mean + 
                rain1_zbin0*temp_mean + rain1_zbin1*temp_mean + rain2_zbin0*temp_mean + rain2_zbin1*temp_mean + rain3_zbin0*temp_mean + rain3_zbin1*temp_mean +
                rain4_zbin0*temp_mean + rain4_zbin1*temp_mean + rain5_zbin0*temp_mean + rain5_zbin1*temp_mean +
                temp_mean^2 + 
                rain1_zbin0*temp_mean^2 + rain1_zbin1*temp_mean^2 + rain2_zbin0*temp_mean^2 + rain2_zbin1*temp_mean^2 + rain3_zbin0*temp_mean^2 + rain3_zbin1*temp_mean^2 +
                rain4_zbin0*temp_mean^2 + rain4_zbin1*temp_mean^2 + rain5_zbin0*temp_mean^2 + rain5_zbin1*temp_mean^2)


yield1 <- feols(log(yield) ~ open + ..ctrl1  | shrid + year | pm25 + pmopen ~ wind + windopen, 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ open + ..ctrl2 | shrid + year | pm25 + pmopen ~ wind + windopen, 
                data = df, 
                cluster = c("shrid"))



yieldtable <- etable(
                      yield1, yield2,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r0",
                      fitstat = c("ivwald", "n"),
                      coefstat = "se",
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain1_zbin0"),
                      keep = c("pm25", "pmopen")
                    )
yieldtable <- yieldtable[,-1]
yieldtable <- yieldtable[-c(10,11),]
yieldtable[c(7),] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("PM 2.5", "", "PM 2.5 times Coal", "",
                          "weather (expanded)", "weather (expanded, bins)",
                          "fixed effects:", "year", "village",
                          "F (pm)", "F (pm times open)", "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yield7ivdiffindiff.rds")










