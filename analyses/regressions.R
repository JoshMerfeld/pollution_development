# Purpose: This script estimates effects. I save the results to load into the markdown script
# Author: Josh Merfeld
# Date: December 12th, 2022

rm(list = ls())

library(tidyverse)
library(lubridate)
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
                              mutate(pm_abs_dev = abs(mean - mean(mean))) %>%
                              ungroup()
summary(pollution$pm_abs_dev)
# mean 21.1

pol1 <- feols(mean ~ wind | village + month, data = pollution, cluster = c("village"))
pol2 <- feols(mean ~ wind | village + month^distfe, data = pollution, cluster = c("village"))
pol3 <- feols(mean ~ wind | village + month, data = pollution %>% filter(year>=2002 & year<=2013), cluster = c("village"))
pol4 <- feols(mean ~ wind | village + month^distfe, data = pollution %>% filter(year>=2002 & year<=2013), cluster = c("village"))

pollutiontable <- etable(
                          pol1, pol2, pol3, pol4, 
                          se.below = TRUE,
                          depvar = FALSE,
                          signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                          digits = 3,
                          fitstat = c("n"),
                          coefstat = "se"
                          )
pollutiontable <- pollutiontable[-c(7:8),]
pollutiontable <- as.matrix(pollutiontable)
rownames(pollutiontable) <- c("wind", "", "fixed effects:",
                               "village", "month", "district-month", "observations")
pollutiontable[c(3),] <- " "
saveRDS(pollutiontable, "pollution_development/draft/tables/pollutiontable.rds") 











df <- read_csv("data/clean/ag_productivity/all_combined.csv")
df <- df %>% group_by(shrid) %>% mutate(rain_z = (log(rain_total) - mean(log(rain_total))/sd(log(rain_total))),
                                        rain1_z = (log(rain_m1 + 1) - mean(log(rain_m1 + 1))/sd(log(rain_m1 + 1))),
                                        rain2_z = (log(rain_m2 + 1) - mean(log(rain_m2 + 1))/sd(log(rain_m2 + 1))),
                                        rain3_z = (log(rain_m3 + 1) - mean(log(rain_m3 + 1))/sd(log(rain_m3 + 1))),
                                        rain4_z = (log(rain_m4 + 1) - mean(log(rain_m4 + 1))/sd(log(rain_m4 + 1))),
                                        rain5_z = (log(rain_m5 + 1) - mean(log(rain_m5 + 1))/sd(log(rain_m5 + 1))),
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

df <- df %>% group_by(shrid, season) %>% 
                arrange(shrid, season, year) %>% 
                mutate(original_yield = log(yield[1])) %>%
                ungroup()

# And district identifiers
villages_overlap <- read_sf("data/spatial/villages_overlap/villages_overlap.shp")
villages_overlap <- villages_overlap %>% mutate(shrid = paste0(pc11_s_id, "-", pc11_tv_id),
                                                distfe = paste0(pc11_s_id, "-", pc11_d_id)) %>%
                                          as_tibble() %>%
                                          dplyr::select(shrid, distfe) %>%
                                          group_by(shrid) %>%
                                          filter(row_number()==1) %>%
                                          ungroup()
df <- df %>% left_join(villages_overlap, by = "shrid")


df <- df %>% group_by(shrid) %>%
                mutate(wind_abs_dev = abs(wind*10 - mean(wind*10))) %>%
                ungroup()
summary(df$wind_abs_dev)
# mean is 8.056, max is 131.58

summary(df$wind*10)
# Min. 1st Qu.  Median    Mean  3rd Qu.   Max. 
# 0.00    0.00    2.00   14.33   14.00  153.00 


yield1 <- feols(log(yield) ~ wind | village^season + year, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ wind + rain_z | village^season + year, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ wind + rain_z | village^season + year^season^distfe, data = df, cluster = c("village"))
yield4 <- feols(log(yield) ~ wind + rain_z | village + year, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield5 <- feols(log(yield) ~ wind + rain_z | village + year, data = df %>% filter(season=="winter"), cluster = c("village"))

yieldtable <- etable(
                      yield1, yield2, yield3, yield4, yield5,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = 3,
                      fitstat = c("n"),
                      coefstat = "se"
                      #extralines = list("Sub-sample" = c("all", "all", "all", "monsoon", "winter"))
                      )
yieldtable <- yieldtable[-c(10,11),]
# Turn to matrix so that duplicate row names are allowed
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("wind", "", "rain (z)", "", 
                          "fixed effects:", "village-season", "year", "district-year-season", "village", "observations")
yieldtable[5,] <- ""
saveRDS(yieldtable, "pollution_development/draft/tables/yieldtable.rds")




yield1 <- feols(log(yield) ~ 1 | village^season + year | pm25 ~ wind, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ rain_z | village^season + year | pm25 ~ wind, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ rain_z | village^season + year^season^distfe | pm25 ~ wind, data = df, cluster = c("village"))
yield4 <- feols(log(yield) ~ rain_z | village + year | pm25 ~ wind, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield5 <- feols(log(yield) ~ rain_z | village + year | pm25 ~ wind, data = df %>% filter(season=="winter"), cluster = c("village"))


yieldtabletwo <- etable(
                        yield1, yield2, yield3, yield4, yield5,
                        stage = 2,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = 3,
                        fitstat = c("n"),
                        coefstat = "se"
                        #extralines = list("Sub-sample" = c("all", "all", "monsoon", "winter"))
                        )
yieldtabletwo1 <- etable(
                        yield1, yield2, yield3, yield4, yield5,
                        stage = 1,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = 3,
                        fitstat = c("n"),
                        coefstat = "se"
                        #extralines = list("Sub-sample" = c("all", "all", "monsoon", "winter"))
                        )
# Add together
new_row <- as_tibble(matrix(data = " ", nrow = 1, ncol = 5))
rownames(new_row) <- c("First stage:")
colnames(new_row) <- colnames(yieldtabletwo)


yieldtabletwo_merge <- rbind(yieldtabletwo[-c(10:11),], new_row, yieldtabletwo1[1:4,])
# Turn to matrix so that duplicate row names are allowed
yieldtabletwo_merge <- as.matrix(yieldtabletwo_merge)

rownames(yieldtabletwo_merge) <- c("particulate matter", "(PM 2.5)", "rain (z)", "",
                                   "fixed effects:", "village-season", "year", "district-year-season", "village", 
                                   "observations",
                                   "first stage:", "wind", " ", "rain (z)", " ")
yieldtabletwo_merge[5,] <- " "
saveRDS(yieldtabletwo_merge, "pollution_development/draft/tables/yieldtabletwo.rds")







df <- df %>% mutate(pm = (pm25/1000))
yield1 <- feols(log(yield) ~ pm | village^season + year, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ pm + rain_z | village^season + year, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ pm + rain_z | village^season + year^season^distfe, data = df, cluster = c("village"))
yield4 <- feols(log(yield) ~ pm + rain_z | village + year, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield5 <- feols(log(yield) ~ pm + rain_z | village + year, data = df %>% filter(season=="winter"), cluster = c("village"))

yieldtable <- etable(
                      yield1, yield2, yield3, yield4, yield5, 
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = 3,
                      fitstat = c("n"),
                      coefstat = "se"
                      #extralines = list("Sub-sample" = c("all", "all", "monsoon", "winter"))
                      )
yieldtable <- yieldtable[-11,]
yieldtable <- yieldtable[-10,]
# Turn to matrix so that duplicate row names are allowed
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(PM 2.5, '000s)", "rain (z)", "", "sub-sample", 
                          "fixed effects:", "village-season", "year", "village", "observations")
yieldtable[6,] <- " "
saveRDS(yieldtable, "pollution_development/draft/tables/yieldtablePM.rds")






df <- df %>% group_by(shrid) %>% 
                mutate(wind_max = max(wind)) %>% 
                ungroup() %>% 
                mutate(wind_max_median = median(wind_max))



yield1 <- feols(log(yield) ~ wind + rain_z | village^season + year, data = df %>% filter(wind_max>wind_max_median), cluster = c("village"))
yield2 <- feols(log(yield) ~ wind + rain_z | village^season + year, data = df %>% filter(wind_max<=wind_max_median), cluster = c("village"))
yield3 <- feols(log(yield) ~ wind*rain_z | village^season + year, data = df, cluster = c("village"))
yield4 <- feols(log(yield) ~ wind + wind^2 + rain_z | village^season + year, data = df, cluster = c("village"))
yield5 <- feols(log(yield) ~ wind*original_yield + rain_z | village^season + year, data = df, cluster = c("village"))

yieldtablehet <- etable(
                        yield1, yield2, yield3, yield4, yield5,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = 3,
                        fitstat = c("n"),
                        coefstat = "se"
                        )
yieldtablehet <- yieldtablehet[-c(14,15),]
yieldtablehet[11,] <- ""
yieldtablehet <- as.matrix(yieldtablehet)
rownames(yieldtablehet) <- c("wind", "", "rain (z)", "", "wind x rain", "", "wind squared", "", "wind x starting yield", "",
                             "fixed effects:", "village-season", "year", "observations")
saveRDS(yieldtablehet, "pollution_development/draft/tables/yieldtablehet.rds")








df <- df %>% mutate(shrid_season = paste0(shrid, "-", season))
df <- panel(df, ~ shrid_season + year, duplicate.method = "first")

yield1 <- feols(log(yield) ~ f(wind, -1:1) + rain_z | village^season + year, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ f(wind, -1:1) + rain_z | village^season + year^season^distfe, data = df, cluster = c("village"))


yieldtableleads <- etable(
                        yield1, yield2,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = 3,
                        fitstat = c("n"),
                        coefstat = "se"
                        )
yieldtableleads <- yieldtableleads[-c(13,14),]
yieldtableleads[9,] <- ""
yieldtableleads <- as.matrix(yieldtableleads)
rownames(yieldtableleads) <- c("wind (lag)", "", "wind", "", "wind (lead)", "", "rain (z)", "",
                             "fixed effects:", "village-season", "year", "district-year-season", "observations")
saveRDS(yieldtableleads, "pollution_development/draft/tables/yieldtableleads.rds")










# Create a dataset that is only POST plant building
df_new <- df %>% group_by(shrid) %>% 
                    mutate(yearly_max = max(wind)) %>% 
                    ungroup()
df_new$year_plant_post <- NA
df_new$year_plant_post[df_new$yearly_max>0] <- df_new$year[df_new$yearly_max>0]
df_new <- df_new %>% group_by(shrid) %>% 
                        mutate(
                               year_first_wind = min(year_plant_post),
                               shrid_season = paste0(shrid, "-", season)
                               ) %>% 
                        filter(year>=(year_first_wind + 1)) %>% 
                        ungroup()



yield1 <- feols(log(yield) ~ wind | village^season + year, data = df_new, cluster = c("village"))
yield2 <- feols(log(yield) ~ wind + rain_z | village^season + year, data = df_new, cluster = c("village"))
yield3 <- feols(log(yield) ~ wind + rain_z | village^season + year^season^distfe, data = df_new, cluster = c("village"))
yield4 <- feols(log(yield) ~ wind + rain_z | village + year, data = df_new %>% filter(season=="monsoon"), cluster = c("village"))
yield5 <- feols(log(yield) ~ wind + rain_z | village + year, data = df_new %>% filter(season=="winter"), cluster = c("village"))

yieldtable <- etable(
                      yield1, yield2, yield3, yield4, yield5,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = 3,
                      fitstat = c("n"),
                      coefstat = "se"
                      #extralines = list("Sub-sample" = c("all", "all", "all", "monsoon", "winter"))
                      )
yieldtable <- yieldtable[-c(10,11),]
# Turn to matrix so that duplicate row names are allowed
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("wind", "", "rain (z)", "", 
                          "fixed effects:", "village-season", "year", "district-year-season", "village", "observations")
yieldtable[5,] <- ""
saveRDS(yieldtable, "pollution_development/draft/tables/yieldtablepostplant.rds")















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
pollution <- read_csv(paste0("data/clean/pm25/nss_merge.csv"))
df <- df %>% left_join(pollution, by = c("state_merge", "district_merge", "year", "month_int"))



laborplot <- ggplot(data = df) +
                geom_histogram(aes(x = date), binwidth = 7, color = "gray") + 
                labs(x = "date",
                     y = "households interviewed")
                theme_minimal()
saveRDS(laborplot, "pollution_development/draft/tables/laborplot.rds")



# control variables
setFixest_fml(..ctrl = ~ poly(female, 1) + poly(age, 2) + educ)




labor1 <- feols((days_self + days_wage) ~ wind | district[year] + year,
                  data = df,
                  cluster = "district")
labor2 <- feols((days_self + days_wage) ~ wind + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")
labor3 <- feols(days_self ~ wind + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")
labor5 <- feols(days_f ~ wind + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")


labortable <- etable(
                      labor1, labor2, labor3, labor4, labor5, labor6,
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
                          "district", "year", "varying slopes:", "year (by district)", 
                          "observations")
labortable[c(4,7),] <- " "
saveRDS(labortable, "pollution_development/draft/tables/labortable.rds")









# Only during the agricultuarl season
df_monsoon <- df %>% filter((month_int %in% c(6:10)))
df_winter <- df %>% filter((month_int %in% c(11, 12, 1:3)))

labor1 <- feols((days_self + days_wage) ~ wind | district[year] + year,
                  data = df_monsoon,
                  cluster = "district")
labor2 <- feols((days_self + days_wage) ~ wind + ..ctrl | district[year] + year,
                  data = df_monsoon,
                  cluster = "district")
labor3 <- feols(days_self ~ wind + ..ctrl | district[year] + year,
                  data = df_monsoon,
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + ..ctrl | district[year] + year,
                  data = df_monsoon,
                  cluster = "district")
labor5 <- feols(days_f ~ wind + ..ctrl | district[year] + year,
                  data = df_monsoon,
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + ..ctrl | district[year] + year,
                  data = df_monsoon,
                  cluster = "district")


labortable <- etable(
                      labor1, labor2, labor3, labor4, labor5, labor6,
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
                          "district", "year", "varying slopes:", "year (by district)", 
                          "observations")
labortable[c(4,7),] <- " "
saveRDS(labortable, "pollution_development/draft/tables/labortablemonsoon.rds")





labor1 <- feols((days_self + days_wage) ~ wind | district[year] + year,
                  data = df_winter,
                  cluster = "district")
labor2 <- feols((days_self + days_wage) ~ wind + ..ctrl | district[year] + year,
                  data = df_winter,
                  cluster = "district")
labor3 <- feols(days_self ~ wind + ..ctrl | district[year] + year,
                  data = df_winter,
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + ..ctrl | district[year] + year,
                  data = df_winter,
                  cluster = "district")
labor5 <- feols(days_f ~ wind + ..ctrl | district[year] + year,
                  data = df_winter,
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + ..ctrl | district[year] + year,
                  data = df_winter,
                  cluster = "district")


labortable <- etable(
                      labor1, labor2, labor3, labor4, labor5, labor6,
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
                          "district", "year", "varying slopes:", "year (by district)", 
                          "observations")
labortable[c(4,7),] <- " "
saveRDS(labortable, "pollution_development/draft/tables/labortablewinter.rds")








# control variables
setFixest_fml(..ctrl = ~ poly(female, 1) + poly(age, 2) + educ)
df <- df %>% mutate(old = as.numeric(age>37.5),
                    wind_old = wind*old)

labor1 <- feols((days_self + days_wage) ~ wind+ wind_old | district[year] + year,
                  data = df,
                  cluster = "district")
labor2 <- feols((days_self + days_wage) ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")
labor3 <- feols(days_self ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")
labor5 <- feols(days_f ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df,
                  cluster = "district")


labortable <- etable(
                      labor1, labor2, labor3, labor4, labor5, labor6,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = 3,
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "poly"), drop = "educ"
                      )
labortable <- labortable[-c(11:12),]
labortable <- as.matrix(labortable)
rownames(labortable) <- c("wind", "", "wind times age>=38", "", "controls", "fixed effects:",
                          "district", "year", "varying slopes:", "year (by district)", 
                          "observations")
labortable[c(6,9),] <- " "
saveRDS(labortable, "pollution_development/draft/tables/labortableold.rds")















# Nightlights
ntl <- read_csv("data/clean/village_ntl.csv")
ntl <- ntl %>% mutate(state = substr(shrid, 1, 2), 
                      wind = sums/365,
                      rain_z = (log(rain) - mean(log(rain))/sd(log(rain))),
                      village = shrid
                      )

pollution <- as_tibble(read_csv(paste0("data/clean/pm25/ntl_merge.csv")))
ntl <- ntl %>% left_join(pollution, by = c("shrid", "year"))

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

ntl <- panel(ntl, ~ shrid + year, duplicate.method = "first")





ntl1 <- feols(log(total_light + 1) ~ l(wind, 1) | village[year] + year,
              data = ntl,
              cluster = "village")
ntl2 <- feols(log(total_light + 1) ~ l(wind, 1) + l(rain_z, 1) | village[year] + year,
              data = ntl,
              cluster = "village")

ntltable <- etable(
                    ntl1, ntl2,
                    se.below = TRUE,
                    depvar = FALSE,
                    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                    digits = 3,
                    fitstat = c("n"),
                    coefstat = "se"
                    )
ntltable <- ntltable[-c(10, 11),]
ntltable <- as.matrix(ntltable)
rownames(ntltable) <- c("wind (lagged)", "", "rain (z, lagged)", "",
                        "fixed effects:", "village", "year", 
                        "varying slopes:", "year (by village)",
                        "observations")
ntltable[c(5,8),] <- " "
saveRDS(ntltable, "pollution_development/draft/tables/ntltable.rds")







df_new <- ntl %>% group_by(shrid) %>% 
                    mutate(yearly_max = max(wind)) %>% 
                    ungroup()
df_new$year_plant_post <- NA
df_new$year_plant_post[df_new$yearly_max>0] <- df_new$year[df_new$yearly_max>0]
df_new <- df_new %>% group_by(shrid) %>% 
                        mutate(
                               year_first_wind = min(year_plant_post)
                               ) %>% 
                        filter(year>=year_first_wind) %>% 
                        ungroup()

df_new <- panel(df_new, ~ shrid + year, duplicate.method = "first")



ntl1 <- feols(log(total_light + 1) ~ l(wind, 1) | village[year] + year,
              data = ntl,
              cluster = "village")
ntl2 <- feols(log(total_light + 1) ~ l(wind, 1) + l(rain_z, 1) | village[year] + year,
              data = ntl,
              cluster = "village")

ntltable <- etable(
                    ntl1, ntl2,
                    se.below = TRUE,
                    depvar = FALSE,
                    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                    digits = 3,
                    fitstat = c("n"),
                    coefstat = "se"
                    )
ntltable <- ntltable[-c(10, 11),]
ntltable <- as.matrix(ntltable)
rownames(ntltable) <- c("wind (lagged)", "", "rain (z, lagged)", "",
                        "fixed effects:", "village", "year", 
                        "varying slopes:", "year (by village)",
                        "observations")
ntltable[c(5,8),] <- " "
saveRDS(ntltable, "pollution_development/draft/tables/ntltable.rds")




