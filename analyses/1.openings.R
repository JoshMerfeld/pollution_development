# Purpose: This script estimates correlates of where plants open. save the results to load into the markdown script.
# Author: Josh Merfeld
# Date: February 25th, 2023

rm(list = ls())

library(tidyverse)
library(lubridate)
library(viridis)
library(fixest)
library(sf)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check



villages <- read_sf(paste0("data/spatial/village_new/village_new.shp"))


### Load raw plants data
plants <- read.xlsx("data/raw/coal_plants.xlsx", sheet = "Units")
# India only
plants <- plants %>% filter(Country=="India")
# Also want only things with a non-missing year
plants <- plants %>% filter(is.na(Year)==F)

# Just keep what we want 
plants <- plants %>% dplyr::select(plant_id = ParentID, 
                                   unit_id = Tracker.ID, 
                                   lat = Latitude, 
                                   lon = Longitude) %>%
                      group_by(plant_id) %>%
                      filter(row_number()==1) %>%
                      ungroup()

gps_points_plants <- plants %>% dplyr::select(lon, lat)
plants <- st_as_sf(SpatialPointsDataFrame(gps_points_plants, plants, proj4string = CRS("EPSG:4258")))
plants <- st_transform(plants, crs = crs(villages))
overlap <- st_join(plants, villages, join = st_intersects)

overlap <- as_tibble(overlap) %>% mutate(shrid = paste0(pc11_s_id, "-", pc11_tv_id), has_plant = 1) %>%
                                  dplyr::select(shrid, has_plant)
overlap <- overlap %>%
            group_by(shrid) %>%
            filter(row_number()==1) %>%
            ungroup()



villages <- as_tibble(villages) %>% mutate(shrid = paste0(pc11_s_id, "-", pc11_tv_id),
                                district_id = paste0(pc11_s_id, "-", pc11_d_id),
                                area_overlap = area) %>%
                          dplyr::select(shrid, district_id) %>%
                          group_by(shrid) %>%
                          filter(row_number()==1) %>%
                          ungroup()


# Where do plants open?
villages90 <- read_csv("data/clean/census_plants/villages90.csv")
villages90$pc91_pca_tot_p_u[is.na(villages90$pc91_pca_tot_p_u)] <- 0
villages90 <- villages90 %>% mutate(
                                    literacy = pc91_pca_p_lit/pc91_pca_tot_p,
                                    pop = pc91_pca_tot_p,
                                    scst_prop = (pc91_pca_p_sc + pc91_pca_p_st)/pop,
                                    tar_road = pc91_vd_tar_road,
                                    urban_pop = pc91_pca_tot_p_u/pc91_pca_tot_p
                                    ) %>%
                              group_by(shrid) %>%
                              filter(row_number()==1) %>%
                              ungroup() %>%
                              left_join(villages, by = c("shrid"))
villages00 <- read_csv("data/clean/census_plants/villages00.csv")
villages00$pc01_pca_tot_p_u[is.na(villages00$pc01_pca_tot_p_u)] <- 0
villages00 <- villages00 %>% group_by(shrid) %>%
                             mutate(
                                    literacy = sum(pc01_pca_p_lit)/sum(pc01_pca_tot_p),
                                    pop = sum(pc01_pca_tot_p),
                                    scst_prop = (sum(pc01_pca_p_sc) + sum(pc01_pca_p_st))/pop,
                                    tar_road = max(pc01_vd_tar_road),
                                    urban_pop = pc01_pca_tot_p_u/pc01_pca_tot_p
                                    ) %>%
                              filter(row_number()==1) %>%
                              ungroup() %>%
                              left_join(villages, by = c("shrid"))
# monsoon at beginning
monsoon <- read_csv("data/clean/census_plants/monsoon2002.csv")
monsoon <- monsoon %>% group_by(shrid) %>% 
                        mutate(ag_prod = log(mean(mean))) %>%
                        filter(row_number()==1) %>%
                        ungroup()
villages00 <- villages00 %>% left_join(monsoon, by = "shrid")
# rename to get same row in table
villages90 <- villages90 %>% rename(area = pc91_vd_area)
villages00 <- villages00 %>% rename(area = pc01_vd_area)
# pollution
pollution <- read_csv(paste0("data/clean/pm25/plant_openings199801.csv"))
pollution <- pollution %>%
              dplyr::select(shrid)
for (year in c("1998", "1999", "2000")){
  for (month in c("01", "02", "03", "04", "05", "06",
                  "07", "08", "09", "10", "11", "12")){
    extracted_values <- read_csv(paste0("data/clean/pm25/plant_openings", year, month, ".csv"))
    colnames(extracted_values) <- c("shrid", paste0(year, month))
    pollution <- pollution %>%
                  left_join(extracted_values, by = "shrid")
  }
}
pollution <- pollution[, c(1, 7:11, 19:23, 31:35)]
pollution$pm25 <- apply(pollution[,-1], 1, FUN = mean)
pollution <- pollution %>%
              dplyr::select(shrid, pm25)
villages00 <- villages00 %>% left_join(pollution, by = "shrid")

villages90$state <- substr(villages90$shrid, 1, 2)
villages00$state <- substr(villages00$shrid, 1, 2)

villages90 <- villages90 %>% left_join(overlap, by = "shrid")
villages00 <- villages00 %>% left_join(overlap, by = "shrid")

villages90$plants90[is.na(villages90$has_plant)] <- 0
villages90$plants00[is.na(villages90$has_plant)] <- 0
villages00$plants00[is.na(villages00$has_plant)] <- 0
villages00$plants10[is.na(villages00$has_plant)] <- 0





reg1 <- feglm(plants90 ~ log(pop) + log(area) | state, data = villages90, vcov = "hetero", family = binomial(link = "probit"))
reg2 <- feglm(plants00 ~ log(pop) + log(area) + plants90 | state, data = villages90, vcov = "hetero", family = binomial(link = "probit"))
reg3 <- feglm(plants00 ~ ag_prod + log(pm25) + log(pop) + log(area) | state, data = villages00, vcov = "hetero", family = binomial(link = "probit"))
reg4 <- feglm(plants10 ~ ag_prod + log(pm25) + log(pop) + log(area) + plants00 | state, data = villages00, vcov = "hetero", family = binomial(link = "probit"))

plantresultstable <- etable(
                          reg1, reg2, reg3, reg4,
                          se.below = TRUE,
                          depvar = FALSE,
                          signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                          digits = "r3",
                          digits.stats = "r3",
                          fitstat = c("n"),
                          coefstat = "se",
                          extralines = list("Sub-sample" = c("all", "no plant", "all", "no plant"))
                          )

plantresultstable[11:12,3] <- plantresultstable[5:6, 3]
plantresultstable <- plantresultstable[-c(5:6),]
plantresultstable <- as.matrix(plantresultstable)
plantresultstable <- plantresultstable[-c(11:15),]
rownames(plantresultstable) <- c("pop (log)", "", "area (log)", "", "ag productivity", "", "pollution", " ", "has plant?", "",
                                "observations")
plantresultstable <- plantresultstable[,-c(1)]
saveRDS(plantresultstable, "pollution_development/draft/tables/plantresultstable.rds")





