# Purpose: This script shows that wind does indeed predict pollution. I save the results to load into the markdown script
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



pollution <- read_csv(paste0("data/clean/pm25/all_combined.csv"))
pollution <- pollution %>% mutate(
                                  date = as_date(paste0(year, "-", month, "-01")),
                                  state = substr(shrid, 1, 2),
                                  village = shrid,
                                  month = as.numeric(date),
                                  wind = sums
                                  )
pollution <- pollution %>% mutate()
pollution <- panel(pollution, ~ village + date)

# And district identifiers
villages_overlap <- read_sf("data/spatial/villages_overlap/villages_overlap.shp")
villages_overlap <- villages_overlap %>% mutate(shrid = paste0(pc11_s_id, "-", pc11_tv_id),
                                                distfe = paste0(pc11_s_id, "-", pc11_d_id)) %>%
                                          as_tibble() %>%
                                          dplyr::select(shrid, distfe) %>%
                                          group_by(shrid) %>%
                                          filter(row_number()==1) %>%
                                          ungroup()
pollution <- pollution %>% left_join(villages_overlap, by = "shrid")

pollution <- pollution %>% group_by(shrid) %>%
                              mutate(pm_abs_dev = abs(mean - mean(mean)),
                                     pm_dev = (mean - mean(mean)),
                                     wind_dev = (wind - mean(wind))) %>%
                              ungroup()
pollution$month_int <- month(pollution$date)
pollution <- pollution %>% group_by(shrid, month_int) %>%
                              mutate(m_pm_dev = (mean - mean(mean)),
                                     m_wind_dev = (wind - mean(wind))) %>%
                              ungroup()
summary(pollution$pm_abs_dev)
summary(pollution$wind_dev)
summary(pollution$m_wind_dev)
summary(pollution$wind)



# figures
# Random 10 percent sample
set.seed(12350)
ggplot(data = pollution[sample(1:nrow(pollution), nrow(pollution)/10, replace = F),]) +
      geom_smooth(aes(x = m_wind_dev, y = m_pm_dev), color = "#440154FF", se = FALSE, alpha = 0.75) +
      labs(x = "wind",
           y = "PM") +
      theme_minimal()
# won't save as RDS because it's simply too large
ggsave("pollution_development/draft/tables/devplot.png")



pollution <- pollution %>% mutate(wind = wind*0.1)
pol1 <- feols(log(mean) ~ wind | village + month, data = pollution, cluster = c("village"))
pol2 <- feols(log(mean) ~ wind | village + month^distfe, data = pollution, cluster = c("village"))
pol3 <- feols(log(mean) ~ wind | village + month, data = pollution %>% filter(year>=2002 & year<=2013), cluster = c("village"))
pol4 <- feols(log(mean) ~ wind | village + month^distfe, data = pollution %>% filter(year>=2002 & year<=2013), cluster = c("village"))

pollutiontable <- etable(
                          pol1, pol2, pol3, pol4, 
                          se.below = TRUE,
                          depvar = FALSE,
                          signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                          digits = "r4",
                          digits.stats = "r3",
                          fitstat = c("n"),
                          coefstat = "se"
                          )
pollutiontable <- pollutiontable[-c(7:8),]
pollutiontable <- as.matrix(pollutiontable)
rownames(pollutiontable) <- c("wind (10s)", "", "fixed effects:",
                               "village", "month", "district-month", "observations")
pollutiontable[c(3),] <- " "
saveRDS(pollutiontable, "pollution_development/draft/tables/pollutiontable.rds") 





