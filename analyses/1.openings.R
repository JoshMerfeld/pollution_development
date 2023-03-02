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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check



# Where do plants open?
villages90 <- read_csv("data/clean/census_plants/villages90.csv")
villages90 <- villages90 %>% mutate(
                                    literacy = pc91_pca_p_lit/pc91_pca_tot_p,
                                    pop = pc91_pca_tot_p,
                                    scst_prop = (pc91_pca_p_sc + pc91_pca_p_st)/pop,
                                    tar_road = pc91_vd_tar_road
                                    )
villages00 <- read_csv("data/clean/census_plants/villages00.csv")
villages00 <- villages00 %>% group_by(shrid) %>%
                             mutate(
                                    literacy = sum(pc01_pca_p_lit)/sum(pc01_pca_tot_p),
                                    pop = sum(pc01_pca_tot_p),
                                    scst_prop = (sum(pc01_pca_p_sc) + sum(pc01_pca_p_st))/pop,
                                    tar_road = max(pc01_vd_tar_road)
                                    ) %>%
                              filter(row_number()==1) %>%
                              ungroup()
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


reg1 <- feols(plants90 ~ log(pop) + log(area) + literacy, data = villages90, vcov = "hetero")
reg2 <- feols(plants00 ~ log(pop) + log(area) + literacy, data = villages90 %>% filter(plants90==0), vcov = "hetero")
reg3 <- feols(plants00 ~ ag_prod + log(pop) + log(area) + literacy, data = villages00, vcov = "hetero")
reg4 <- feols(plants10 ~ ag_prod + log(pop) + log(area) + literacy, data = villages00 %>% filter(plants00==0), vcov = "hetero")

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
plantresultstable <- plantresultstable[-c(1:2,12:13),]
plantresultstable <- as.matrix(plantresultstable)
rownames(plantresultstable) <- c("pop (log)", "", "area (log)", "", "literacy (prop)", "", "ag productivity", "", 
                                "sub-sample", "observations")
saveRDS(plantresultstable, "pollution_development/draft/tables/plantresultstable.rds")





