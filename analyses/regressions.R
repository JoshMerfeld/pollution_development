# Purpose: This script estimates effects. I save the results to load into the markdown script
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


pol1 <- feols(mean ~ wind | village + month, data = pollution, cluster = c("village"))
pol2 <- feols(mean ~ wind | village[month] + month, data = pollution, cluster = c("village"))
pol3 <- feols(mean ~ wind | village + month, data = pollution %>% filter(year>=2002 & year<=2013), cluster = c("village"))
pol4 <- feols(mean ~ wind | village[month] + month, data = pollution %>% filter(year>=2002 & year<=2013), cluster = c("village"))

pollutiontable <- etable(
                          pol1, pol2, pol3, pol4, 
                          se.below = TRUE,
                          depvar = FALSE,
                          signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                          digits = 3,
                          fitstat = c("n"),
                          coefstat = "se"
                          )
pollutiontable <- pollutiontable[-9,]
pollutiontable <- pollutiontable[-8,]
pollutiontable <- as.matrix(pollutiontable)
rownames(pollutiontable) <- c("wind", "", "fixed effects:",
                               "village", "month", "varying slopes:",
                               "month (village)", "", "observations")
saveRDS(pollutiontable, "pollution_development/draft/tables/pollutiontable.rds") 











df <- read_csv("data/clean/ag_productivity/all_combined.csv")
df <- df %>% group_by(shrid) %>% mutate(rain_z = (log(rain_total) - mean(log(rain_total))/sd(log(rain_total))),
                                        rain1_z = (log(rain_m1) - mean(log(rain_m1))/sd(log(rain_m1))),
                                        rain2_z = (log(rain_m2) - mean(log(rain_m2))/sd(log(rain_m2))),
                                        rain3_z = (log(rain_m3) - mean(log(rain_m3))/sd(log(rain_m3))),
                                        rain4_z = (log(rain_m4) - mean(log(rain_m4))/sd(log(rain_m4))),
                                        rain5_z = (log(rain_m5) - mean(log(rain_m5))/sd(log(rain_m5))),
                                        rain_early_z = (log(rain_m1 + rain_m2) - mean(log(rain_m1 + rain_m2))/sd(log(rain_m1 + rain_m2))),
                                        rain_late_z = (log(rain_m3 + rain_m4 + rain_m5) - mean(log(rain_m3 + rain_m4 + rain_m5))/sd(log(rain_m3 + rain_m4 + rain_m5))),
                                        wind_early_z = m1 + m2,
                                        wind_late_z = m3 + m4 + m5,
                                        yield = mean,
                                        wind = days_sums/10,
                                        village = shrid
                                        )

pollution <- read_csv(paste0("data/clean/pm25/ag_merge.csv"))
df <- df %>% left_join(pollution, by = c("shrid", "year", "season"))


yield1 <- feols(log(yield) ~ wind | village^season + year, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ wind + rain_z | village^season + year, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ wind + rain_z | village + year, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield4 <- feols(log(yield) ~ wind + rain_z | village + year, data = df %>% filter(season=="winter"), cluster = c("village"))

yieldtable <- etable(
                      yield1, yield2, yield3, yield4, 
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = 3,
                      fitstat = c("n"),
                      coefstat = "se",
                      extralines = list("Sub-sample" = c("all", "all", "monsoon", "winter"))
                      )
yieldtable <- yieldtable[-11,]
yieldtable <- yieldtable[-10,]
# Turn to matrix so that duplicate row names are allowed
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("wind", "", "rain (z)", "", "sub-sample", 
                          "fixed effects:", "village-season", "year", "village", "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yieldtable.rds")




yield1 <- feols(log(yield) ~ 1 | village^season + year | pm25 ~ wind, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ rain_z | village^season + year | pm25 ~ wind, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ rain_z | village + year | pm25 ~ wind, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield4 <- feols(log(yield) ~ rain_z | village + year | pm25 ~ wind, data = df %>% filter(season=="winter"), cluster = c("village"))


yieldtabletwo <- etable(
                        yield1, yield2, yield3, yield4, 
                        stage = 2,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = 3,
                        fitstat = c("n"),
                        coefstat = "se",
                        extralines = list("Sub-sample" = c("all", "all", "monsoon", "winter"))
                        )
yieldtabletwo1 <- etable(
                        yield1, yield2, yield3, yield4, 
                        stage = 1,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = 3,
                        fitstat = c("n"),
                        coefstat = "se",
                        extralines = list("Sub-sample" = c("all", "all", "monsoon", "winter"))
                        )
# Add together
new_row <- as_tibble(matrix(data = " ", nrow = 1, ncol = 4))
rownames(new_row) <- c("First stage:")
colnames(new_row) <- colnames(yieldtabletwo)


yieldtabletwo_merge <- rbind(yieldtabletwo[-11,], new_row, yieldtabletwo1[1:4,])
yieldtabletwo_merge <- yieldtabletwo_merge[-10,]
# Turn to matrix so that duplicate row names are allowed
yieldtabletwo_merge <- as.matrix(yieldtabletwo_merge)
rownames(yieldtabletwo_merge) <- c("pm25", " ", "rain (z)", " ", "sub-sample",
                             "fixed effects:", "village-season", "year", "village",
                             "observations",
                             "first stage:", "wind", " ", "rain (z)", " ")
saveRDS(yieldtabletwo_merge, "pollution_development/draft/tables/yieldtabletwo.rds")







df <- df %>% mutate(pm = (pm25/1000))
yield1 <- feols(log(yield) ~ pm | village^season + year, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ pm + rain_z | village^season + year, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ pm + rain_z | village + year, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield4 <- feols(log(yield) ~ pm + rain_z | village + year, data = df %>% filter(season=="winter"), cluster = c("village"))

yieldtable <- etable(
                      yield1, yield2, yield3, yield4, 
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = 3,
                      fitstat = c("n"),
                      coefstat = "se",
                      extralines = list("Sub-sample" = c("all", "all", "monsoon", "winter"))
                      )
yieldtable <- yieldtable[-11,]
yieldtable <- yieldtable[-10,]
# Turn to matrix so that duplicate row names are allowed
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(PM 2.5, '000s)", "rain (z)", "", "sub-sample", 
                          "fixed effects:", "village-season", "year", "village", "observations")
saveRDS(yieldtable, "pollution_development/draft/tables/yieldtablePM.rds")













df <- read_csv("data/clean/nss/merged_week.csv")
df <- df %>% filter(age>=15)
df <- df %>% mutate(
                    month_int = month(date),
                    year = year(date),
                    month = as.numeric(date),
                    educ = as.factor(educ),
                    wind = days_sum,
                    district = distfe
                    )

# control variables
setFixest_fml(..ctrl = ~ poly(female, 1) + poly(age, 2) + educ)


labor1 <- feols((days_self + days_wage) ~ wind | district + year,
                  data = df,
                  cluster = "district")
labor2 <- feols((days_self + days_wage) ~ wind + ..ctrl | district + year,
                  data = df,
                  cluster = "district")
labor3 <- feols((days_self + days_wage) ~ wind + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")

labortable <- etable(
                      labor1, labor2, labor3,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = 3,
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "poly"), drop = "educ"
                      )
labortable <- labortable[-c(9:10),]
labortable <- as.matrix(labortable)
rownames(labortable) <- c("wind", "", "controls", "fixed effects:",
                               "district", "year", "varying slopes:",
                               "year (district)", "observations")
saveRDS(labortable, "pollution_development/draft/tables/labortable.rds")




labor4 <- feols(days_self ~ wind + ..ctrl | district + year,
                  data = df,
                  cluster = "district")
labor5 <- feols(days_wage ~ wind + ..ctrl | district + year,
                  data = df,
                  cluster = "district")
labor6 <- feols(days_f ~ wind + ..ctrl | district + year,
                  data = df,
                  cluster = "district")
labor7 <- feols(days_nf ~ wind + ..ctrl | district + year,
                  data = df,
                  cluster = "district")

labortable_two <- etable(
                          labor4, labor5, labor6, labor7,
                          se.below = TRUE,
                          depvar = FALSE,
                          signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                          digits = 3,
                          fitstat = c("r2", "n"),
                          coefstat = "se",
                          group = list(controls = "poly"), drop = "educ",
                          headers = c("self", "wage", "farm", "non-farm")
                          )
saveRDS(labortable_two, "pollution_development/draft/tables/labortable_two.rds")



















# Nightlights
ntl <- read_csv("data/clean/village_ntl.csv")
ntl <- ntl %>% mutate(state = substr(shrid, 1, 2), 
                      wind = sums/365,
                      rain_z = (log(rain) - mean(log(rain))/sd(log(rain))),
                      village = shrid
                      )

pollution <- as_tibble(read_csv(paste0("data/clean/pm25/ntl_merge.csv")))
ntl <- ntl %>% left_join(pollution, by = c("shrid", "year"))
ntl <- panel(ntl, ~ shrid + year, duplicate.method = "first")




ntl1 <- feols(log(total_light + 1) ~ l(wind, 1) | village[year] + year,
              data = ntl,
              cluster = "village")
ntl2 <- feols(log(total_light + 1) ~ l(wind, 1) + l(rain_z, 1) | village[year] + year,
              data = ntl,
              cluster = "village")
ntl3 <- feols(log(total_light + 1) ~ l(wind, 1:2) + l(rain_z, 1:2) | village[year] + year,
              data = ntl,
              cluster = "village")

ntltable <- etable(
                    ntl1, ntl2, ntl3,
                    se.below = TRUE,
                    depvar = FALSE,
                    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                    digits = 3,
                    fitstat = c("r2", "n"),
                    coefstat = "se",
                    headers = c("(1)", "(2)", "(3)")
                    )
saveRDS(ntltable, "pollution_development/draft/tables/ntltable.rds")




ntl1 <- feols(log(total_light + 1) ~ 1 | village[year] + year | l(pm25, 1) ~ l(wind, 1),
              data = ntl,
              cluster = "village")
ntl2 <- feols(log(total_light + 1) ~ l(rain, 1) | village[year] + year | l(pm25, 1) ~ l(wind, 1),
              data = ntl,
              cluster = "village")
etable(ntl1, ntl2, stage = 2)




ntl1 <- feols(log(total_light + 1) ~ l(pm25, 1) | village[year] + year,
              data = ntl,
              cluster = "village")
ntl2 <- feols(log(total_light + 1) ~ l(pm25, 1) + l(rain, 1) | village[year] + year,
              data = ntl,
              cluster = "village")
etable(ntl1, ntl2)






