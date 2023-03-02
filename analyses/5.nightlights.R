# Purpose: This script estimates effects. I save the results to load into the markdown script
# Author: Josh Merfeld
# Date: December 12th, 2022

rm(list = ls())

library(tidyverse)
library(lubridate)
library(viridis)
library(fixest)
library(sf)


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
                      wind = sums/365,
                      rain_z = (log(rain) - mean(log(rain))/sd(log(rain))),
                      village = shrid
                      )

pollution <- as_tibble(read_csv(paste0("data/clean/pm25/ntl_merge.csv")))
ntl <- pollution %>% left_join(ntl, by = c("shrid", "year"))
# ntltemp <- read_csv(paste0("data/clean/village_temp.csv"))
# ntl <- ntl %>% left_join(ntltemp, by = c("shrid", "year"))

# And district identifiers
villages_overlap <- read_sf("data/spatial/villages_overlap/villages_overlap.shp")
villages_overlap <- villages_overlap %>% mutate(shrid = paste0(pc11_s_id, "-", pc11_tv_id),
                                                distfe = paste0(pc11_s_id, "-", pc11_d_id)) %>%
                                          as_tibble() %>%
                                          dplyr::select(shrid, distfe) %>%
                                          group_by(shrid) %>%
                                          filter(row_number()==1) %>%
                                          ungroup()
ntl <- ntl %>% left_join(villages_overlap, by = "shrid")
ntl$pm25 <- log(ntl$pm25)

ntl <- panel(ntl, ~ shrid + year, duplicate.method = "first")





ntl1 <- feols(log(total_light + 1) ~ 1 | village + year | l(pm25, 1) ~ l(wind, 1),
              data = ntl,
              cluster = "village")
ntl2 <- feols(log(total_light + 1) ~ l(log(rain), 1) | village + year | l(pm25, 1) ~ l(wind, 1),
              data = ntl,
              cluster = "village")
ntl3 <- feols(log(total_light + 1) ~ 1 | village + year | l(pm25, -1) ~ l(wind, -1),
              data = ntl,
              cluster = "village")
ntl4 <- feols(log(total_light + 1) ~ l(rain_z, 1) + l(temp, 1) | village + year| l(pm25, -1) ~ l(wind, -1),
              data = ntl,
              cluster = "village")

ntltable <- etable(
                    ntl1, ntl2, ntl3, ntl4,
                    se.below = TRUE,
                    depvar = FALSE,
                    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                    digits = "r3",
                    digits.stats = "r3",
                    fitstat = c("ivwald", "n"),
                    coefstat = "se"
                    )
ntltable <- ntltable[-c(3, 4, 12, 13),]
ntltable <- as.matrix(ntltable)
rownames(ntltable) <- c("wind (lag)", "", "wind (lead)", "",
                        "fixed effects:", "village", "year", 
                        "varying slopes:", "year (by village)",
                        "observations")
ntltable[c(5,8),] <- " "
saveRDS(ntltable, "pollution_development/draft/tables/ntltable.rds")















