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


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
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
                     rain_z = (log(rain_total) - mean(log(rain_total))/sd(log(rain_total))),
                     wind_abs_dev = abs(days_sums - mean(days_sums)),
                     pm25_abs_dev = abs(pm25 - mean(pm25)),
                     wind = days_sums/10,
                     pm1 = log(pm1),
                     pm2 = log(pm2),
                     pm3 = log(pm3),
                     pm4 = log(pm4),
                     pm5 = log(pm5),
                     pmavg1 = log(pm1),
                     pmavg2 = log((pm1 + pm2)/2),
                     pmavg3 = log((pm1 + pm2 + pm3)/3),
                     pmavg4 = log((pm1 + pm2 + pm3 + pm4)/4),
                     pmavg5 = log((pm1 + pm2 + pm3 + pm4 + pm5)/5),
                     temp_m1 = (tmax_m1 + tmin_m1)/20,
                     temp_m2 = (tmax_m2 + tmin_m2)/20,
                     temp_m3 = (tmax_m3 + tmin_m3)/20,
                     temp_m4 = (tmax_m4 + tmin_m4)/20,
                     temp_m5 = (tmax_m5 + tmin_m5)/20,
                     rain1_z = (log(rain_m1) - mean(log(rain_m1))/sd(log(rain_m1))),
                     rain2_z = (log(rain_m2 + rain_m1) - mean(log(rain_m2 + rain_m1))/sd(log(rain_m2 + rain_m1))),
                     rain3_z = (log(rain_m3 + rain_m1 + rain_m2) - mean(log(rain_m3 + rain_m1 + rain_m2))/sd(log(rain_m3 + rain_m1 + rain_m2))),
                     rain4_z = (log(rain_m4 + rain_m1 + rain_m2 + rain_m3) - mean(log(rain_m4 + rain_m1 + rain_m2 + rain_m3))/sd(log(rain_m4 + rain_m1 + rain_m2 + rain_m3))),
                     rain5_z = (log(rain_m5 + rain_m1 + rain_m2 + rain_m3 + rain_m4) - mean(log(rain_m5 + rain_m1 + rain_m2 + rain_m3 + rain_m4))/sd(log(rain_m5 + rain_m1 + rain_m2 + rain_m3 + rain_m4)))
                     )


# yield in first observation
df <- df %>% group_by(shrid, season) %>% 
                arrange(shrid, season, year) %>% 
                mutate(original_yield = log(yield[1])) %>%
                ungroup()


# And district identifiers
villages_overlap <- read_sf("data/spatial/villages_overlap/villages_overlap.shp")
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
summary(exp(df$pm25))
summary(df$pm25_abs_dev)




setFixest_fml(..ctrl1 = ~ rain_z + temp_mean)
setFixest_fml(..ctrl2 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)




# Regressions ---------------------------------

## reduced form ---------------------------------

yield1 <- feols(log(yield) ~ wind | shrid + year, data = df, cluster = c("shrid"))
yield2 <- feols(log(yield) ~ wind + ..ctrl1 | shrid + year, data = df, cluster = c("shrid"))
yield3 <- feols(log(yield) ~ wind + ..ctrl1 + ..ctrl2 | shrid + year, data = df, cluster = c("shrid"))

yieldtable <- etable(
                     yield1, yield2, yield3,
                     se.below = TRUE,
                     depvar = FALSE,
                     signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                     digits = "r4",
                     fitstat = c("n"),
                     coefstat = "se"
                     )
# rename
yieldtable <- yieldtable[-c(16:17),]
yieldtable[13,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("wind days", " ", "rain (z)", "", "mean temp (10s)", "",
                          "rain square", " ", "temp square", " ", "rain times temp", " ",
                          "fixed effects:", "village", "year",
                          "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yield1reducedform.rds")





## placebo test
wind_year <- df %>%
              dplyr::select(wind, year) %>%
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
              dplyr::select(-wind) %>%
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
  temp <- cbind(df_placebo, wind_all %>% dplyr::select(wind))
  # get coefficient
  coefs <- feols(log(yield) ~ wind + ..ctrl1 + ..ctrl2 | shrid + year, data = temp, cluster = c("shrid"))
  # return
  return(coefs$coeftable[1,1])
}
RNGkind("L'Ecuyer-CMRG")
set.seed(2430986) #resetting seed just to make sure this works correctly when using parallel
coef_vec <- mclapply(1:1000, f, mc.set.seed = TRUE)
coef_vec <- as_vector(coef_vec)



# Also get distribution if we resample wind from WITHIN DISTRICTS ONLY
## placebo test
wind_year <- df %>%
              dplyr::select(wind, distfe, year) %>%
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
  coefs <- feols(log(yield) ~ wind + ..ctrl1 + ..ctrl2 | shrid + year, data = temp, cluster = c("shrid"))
  # return
  return(coefs$coeftable[1,1])
}

# use mclapply to take advantage of the cores on this computer
# should take around an hour
set.seed(23847906) #resetting seed just to make sure this works correctly when using parallel
coefs_distfe <- mclapply(1:1000, f, mc.set.seed = TRUE)
coefs_distfe <- as_vector(coefs_distfe)




# get true value (with df)
true_value <- feols(log(yield) ~ wind + ..ctrl1 + ..ctrl2 | shrid + year, data = df, cluster = c("shrid"))
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
                          )
saveRDS(gg1, "pollution_development/draft/tables/randomization.rds")


# 
# # get percentile
# coefs_distfe <- sort(coefs_distfe)
# row <- 1
# while (row<=length(coefs_distfe)){
#   if (true_value<coefs_distfe[row]){
#     row <- row + 1
#   }else{
#     break
#   }
# }
# row # larger than every single randomized value!






## naive regression ---------------------------------

setFixest_fml(..ctrl1 = ~ rain_z + temp_mean)
setFixest_fml(..ctrl2 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)

yield1 <- feols(log(yield) ~ pm25 | shrid + year, data = df, cluster = c("shrid"))
yield2 <- feols(log(yield) ~ pm25 + ..ctrl1 | shrid + year, data = df, cluster = c("shrid"))
yield3 <- feols(log(yield) ~ pm25 + ..ctrl1 + ..ctrl2 | shrid + year, data = df, cluster = c("shrid"))

yieldtable <- etable(
                      yield1, yield2, yield3,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain_z square"),
                      keep = c("pm25")
                    )
# rename
yieldtable <- yieldtable[-c(8:9),]
yieldtable[5,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(log PM 2.5)", 
                          "weather", "weather (expanded)",
                          "fixed effects:", "village", "year",
                          "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yield2naive.rds")







## main IV results ---------------------------------

setFixest_fml(..ctrl1 = ~ rain_z + temp_mean)
setFixest_fml(..ctrl2 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)

yield1 <- feols(log(yield) ~ 1 | shrid + year | pm25 ~ wind, 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ ..ctrl1 | shrid + year | pm25 ~ wind, 
                data = df, 
                cluster = c("shrid"))
yield3 <- feols(log(yield) ~ ..ctrl1 + ..ctrl2 | shrid + year | pm25 ~ wind, 
                data = df, 
                cluster = c("shrid"))

yieldtable <- etable(
                      yield1, yield2, yield3,
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
yieldtable <- yieldtable[-c(8:9),]
yieldtable[5,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(log PM 2.5)",
                          "weather", "weather (expanded)",
                          "fixed effects:", "village", "year",
                          "F", 
                          "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yield3ivmain.rds")





# leads
df <- panel(df, ~ shrid + year)

setFixest_fml(..ctrl1 = ~ f(rain_z, 0:1) + f(temp_mean, 0:1))


yield1 <- feols(log(yield) ~ 1 | shrid + year | f(pm25, 0:1) ~ f(wind, 0:1), 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ ..ctrl1 | shrid + year | f(pm25, 0:1) ~ f(wind, 0:1), 
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
                     group = list(controls = "rain_z"),
                     keep = c("pm25")
                     )
# rename
yieldtable <- yieldtable[,-1]
yieldtable <- yieldtable[-c(9:10),]
yieldtable[6,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(log PM 2.5)",
                          "particulate matter", "(one-year lead)",
                          "weather",
                          "fixed effects:", "village", "year",
                          "F (current)", "F (lead)", 
                          "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yield3ivmain_lead.rds")












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
  coefs <- feols(log(yield) ~ ..ctrl1 + ..ctrl2 | shrid + year | pm25 ~ wind, data = temp, cluster = c("shrid"))
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
  coefs <- feols(log(yield) ~ ..ctrl1 + ..ctrl2 | shrid + year | pm25 ~ wind, data = temp, cluster = c("shrid"))
  # return
  return(coefs$coeftable[1,1])
}
# use mclapply to take advantage of the cores on this computer
# should take around an hour
set.seed(67891235) #resetting seed just to make sure this works correctly when using parallel
coefs_distfe <- mclapply(1:1000, f, mc.set.seed = TRUE)
coefs_distfe <- as_vector(coefs_distfe)




# get true value (with df)
true_value <- feols(log(yield) ~ ..ctrl1 + ..ctrl2 | shrid + year | pm25 ~ wind, 
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









## monthly results ---------------------------------

setFixest_fml(..ctrl1 = ~ rain1_z + rain2_z + rain3_z + rain4_z + rain5_z + temp_m1 + temp_m2 + temp_m3 + temp_m4 + temp_m5)


yield1 <- feols(log(yield) ~ 1 | 
                  shrid + year | 
                  pm1 + pm2 + pm3 + pm4 + pm5 ~ m1 + m2 + m3 + m4 + m5, 
                data = df, 
                cluster = "shrid")

yield2 <- feols(log(yield) ~ ..ctrl1 | 
                  shrid + year | 
                  pm1 + pm2 + pm3 + pm4 + pm5 ~ m1 + m2 + m3 + m4 + m5,
                data = df, 
                cluster = "shrid")

df2 <- df %>%
        mutate(
               pm1 = pmavg1,
               pm2 = pmavg2,
               pm3 = pmavg3,
               pm4 = pmavg4,
               pm5 = pmavg5,
               )



yieldtable <- etable(
                     yield1, yield2,
                     se.below = TRUE,
                     depvar = FALSE,
                     signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                     digits = "r3",
                     digits.stats = "r0",
                     fitstat = c("ivwald", "n"),
                     keep = c("pm1", "pm2", "pm3", "pm4", "pm5"),
                     coefstat = "se",
                     group = list(weather = "rain1_z")
                     )

yieldtable <- yieldtable[-c(15,16),]
yieldtable[12,] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("June", " ", "July", " ", "Aug.", " ", "Sept.", " ", "Oct.", " ",
                          "weather",
                          "fixed effects:", "village", "year", 
                          "F (June)", "F (July)", "F (Aug.)", "F (Sept.)", "F (Oct.)",
                          "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yield4monthly.rds")











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

yield1 <- feols(log(yield) ~ ..ctrl1 | shrid + year | pm25 ~ wind, 
                data = df %>% filter(wind_med>wind_med_median),
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ ..ctrl1 | shrid + year | pm25 ~ wind, 
                data = df %>% filter(wind_med<=wind_med_median),
                cluster = c("shrid"))
yield3 <- feols(log(yield) ~ ..ctrl1 | shrid + year | pm25 ~ wind, 
                data = df %>% filter(yield_med>yield_med_median),
                cluster = c("shrid"))
yield4 <- feols(log(yield) ~ ..ctrl1 | shrid + year | pm25 ~ wind, 
                data = df %>% filter(yield_med<=yield_med_median),
                cluster = c("shrid"))
yield5 <- feols(log(yield) ~ ..ctrl1 | shrid + year | pm25 + pmrain ~ wind + windrain, 
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
                        group = list(weather = "rain1_z"),
                        keep = c("pm25", "pmrain")
                        )
yieldtablehet <- yieldtablehet[-c(9,10),]
yieldtablehet[5,] <- "Yes"
yieldtablehet[6,] <- ""
yieldtablehet <- as.matrix(yieldtablehet)
rownames(yieldtablehet) <- c("pm (log)", "", "pm times rain", "", "weather",
                             "fixed effects:", "village", "year", 
                             "F (1st stage, PM)", "F (1st stage, PM times rain)", "observations")
yieldtablehet[10,1:4] <- " "
colnames(yieldtablehet) <- c(">p(50)", "<=p(50)", ">p(50)", "<=p(50)", "")
saveRDS(yieldtablehet, "pollution_development/draft/tables/yield5heterogeneity.rds")








## naive regression ---------------------------------

df <- panel(df, ~ shrid + year, duplicate.method = "first")

setFixest_fml(..ctrl1 = ~ rain_z + temp_mean)
setFixest_fml(..ctrl2 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)

yield1 <- feols(log(yield) ~ 1 | shrid + year | f(pm25, 0:1) ~ f(wind, 0:1), 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ rain_z + temp_mean | shrid + year | f(pm25, 0:1) ~ f(wind, 0:1), 
                data = df, 
                cluster = c("shrid"))
yield3 <- feols(log(yield) ~ rain_z*temp_mean + rain_z^2 + temp_mean^2 | shrid + year | f(pm25, 0:1) ~ f(wind, 0:1), 
                data = df, 
                cluster = c("shrid"))


yieldtableleads <- etable(
                          yield1, yield2, yield3,
                          se.below = TRUE,
                          depvar = FALSE,
                          signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                          digits = "r4",
                          digits.stats = "r0",
                          fitstat = c("ivwald", "n"),
                          coefstat = "se",
                          group = list(controls = "rain_z",
                                       conrols_expanded = "rain_z square"),
                          keep = c("pm25", "f(pm25,1)")
                          )
yieldtableleads <- yieldtableleads[-c(10,11),]
yieldtableleads[7,] <- ""
yieldtableleads <- as.matrix(yieldtableleads)
rownames(yieldtableleads) <- c("PM", "", "PM (lead)", "",
                               "weather", "weather (expanded)",
                               "fixed effects:", "village", "year", 
                               "F", "F (lead)",
                               "observations")
saveRDS(yieldtableleads, "pollution_development/draft/tables/yield6leads.rds")









## diff-in-diff IV ---------------------------------

df$pmopen <- df$pm25*df$open
df$windopen <- df$wind*df$open
df$pmyear <- df$pm25*df$year
df$windyear <- df$wind*df$year

setFixest_fml(..ctrl1 = ~ rain_z + temp_mean)
setFixest_fml(..ctrl2 = ~ rain_z + temp_mean + rain_z^2 + temp_mean^2 + rain_z*temp_mean)


yield1 <- feols(log(yield) ~ open | year | pm25 + pmopen ~ wind + windopen, 
                data = df, 
                cluster = c("shrid"))
yield2 <- feols(log(yield) ~ open | shrid + year | pm25 + pmopen ~ wind + windopen, 
                data = df, 
                cluster = c("shrid"))
yield3 <- feols(log(yield) ~ open + ..ctrl1 | shrid + year | pm25 + pmopen ~ wind + windopen, 
                data = df, 
                cluster = c("shrid"))
yield4 <- feols(log(yield) ~ open + ..ctrl1 + ..ctrl2 | shrid + year | pm25 + pmopen ~ wind + windopen, 
                data = df, 
                cluster = c("shrid"))
yield5 <- feols(log(yield) ~ open + ..ctrl1 + ..ctrl2 | shrid[year] + year | pm25 + pmopen ~ wind + windopen, 
                data = df, 
                cluster = c("shrid"))



yieldtable <- etable(
                      yield2, yield3, yield4,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r0",
                      fitstat = c("ivwald", "n"),
                      coefstat = "se",
                      group = list(controls = "rain_z",
                                   conrols_expanded = "rain_z square"),
                      keep = c("pm25", "pmopen")
                    )
yieldtable <- yieldtable[-c(10,11),]
yieldtable[c(7),] <- " "
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("PM 2.5", "", "PM 2.5 times coal", "",
                          "weather", "weather (expanded)",
                          "fixed effects:", "year", "village",
                          "F (pm)", "F (pm times open)", "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yield7ivdiffindiff.rds")











