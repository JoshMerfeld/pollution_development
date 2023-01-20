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
monsoon <- read_csv("data/clean/census_plants/monsoon2002.csv")
monsoon <- monsoon %>% group_by(shrid) %>% 
                        mutate(ag_prod = log(mean(mean))) %>%
                        filter(row_number()==1) %>%
                        ungroup()
villages00 <- villages00 %>% left_join(monsoon, by = "shrid")


reg1 <- feols(plants90 ~ log(pop) + literacy + tar_road, data = villages90, vcov = "hetero")
reg2 <- feols(plants00 ~ log(pop) + literacy + tar_road, data = villages90 %>% filter(plants90==0), vcov = "hetero")
reg3 <- feols(plants00 ~ ag_prod + log(pop) + literacy + tar_road, data = villages00, vcov = "hetero")
reg4 <- feols(plants10 ~ ag_prod + log(pop) + literacy + tar_road, data = villages00 %>% filter(plants00==0), vcov = "hetero")

plantresultstable <- etable(
                          reg1, reg2, reg3, reg4,
                          se.below = TRUE,
                          depvar = FALSE,
                          signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                          digits = 3,
                          fitstat = c("n"),
                          coefstat = "se",
                          extralines = list("Sub-sample" = c("all", "no plant", "all", "no plant"))
                          )
plantresultstable <- plantresultstable[-c(1:2,12:13),]
plantresultstable <- as.matrix(plantresultstable)
rownames(plantresultstable) <- c("pop (log)", "", "literacy (prop)", "", "has paved road", "", "ag productivity", "", 
                                "sub-sample", "observations")
saveRDS(plantresultstable, "pollution_development/draft/tables/plantresultstable.rds") 









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

# Mean, divided by ten (for presentation)
df <- df %>% mutate(temp_mean = (tmax_mean + tmin_mean)/20)


yield1 <- feols(log(yield) ~ wind | village^season + year, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ wind + rain_z + temp_mean | village^season + year, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ wind + rain_z + temp_mean | village^season + year^season^distfe, data = df, cluster = c("village"))
yield4 <- feols(log(yield) ~ wind + rain_z + temp_mean | village + year, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield5 <- feols(log(yield) ~ wind + rain_z + temp_mean | village + year, data = df %>% filter(season=="winter"), cluster = c("village"))

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
yieldtable <- yieldtable[-c(12,13),]
# Turn to matrix so that duplicate row names are allowed
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("wind", "", "rain (z)", "", "mean temp (10s)", "",
                          "fixed effects:", "village-season", "year", "district-year-season", "village", "observations")
yieldtable[7,] <- ""
saveRDS(yieldtable, "pollution_development/draft/tables/yieldtable.rds")







yield1 <- feols(log(yield) ~ 1 | village^season + year | pm25 ~ wind, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ rain_z + temp_mean | village^season + year | pm25 ~ wind, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ rain_z + temp_mean | village^season + year^season^distfe | pm25 ~ wind, data = df, cluster = c("village"))
yield4 <- feols(log(yield) ~ rain_z + temp_mean | village + year | pm25 ~ wind, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield5 <- feols(log(yield) ~ rain_z + temp_mean | village + year | pm25 ~ wind, data = df %>% filter(season=="winter"), cluster = c("village"))


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
                        fitstat = c("ivwald"),
                        coefstat = "se"
                        #extralines = list("Sub-sample" = c("all", "all", "monsoon", "winter"))
                        )
# Add together
new_row <- as_tibble(matrix(data = " ", nrow = 1, ncol = 5))
rownames(new_row) <- c("First stage:")
colnames(new_row) <- colnames(yieldtabletwo)


yieldtabletwo_merge <- rbind(yieldtabletwo[-c(12:13),], new_row, yieldtabletwo1[1:6,])
# Turn to matrix so that duplicate row names are allowed
yieldtabletwo_merge <- as.matrix(yieldtabletwo_merge)

rownames(yieldtabletwo_merge) <- c("particulate matter", "(PM 2.5)", "rain (z)", "", "mean temp (10s)", "",
                                   "fixed effects:", "village-season", "year", "district-year-season", "village", 
                                   "observations",
                                   "first stage:", "wind", " ", "rain (z)", " ", "mean temp (10s)", "")
yieldtabletwo_merge[7,] <- " "
saveRDS(yieldtabletwo_merge, "pollution_development/draft/tables/yieldtabletwo.rds")





df <- df %>% mutate(pm = (pm25/1000))
yield1 <- feols(log(yield) ~ pm25 | village^season + year, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ pm25 + rain_z + temp_mean | village^season + year, data = df, cluster = c("village"))
yield3 <- feols(log(yield) ~ pm25 + rain_z + temp_mean | village^season + year^season^distfe, data = df, cluster = c("village"))
yield4 <- feols(log(yield) ~ pm25 + rain_z + temp_mean | village + year, data = df %>% filter(season=="monsoon"), cluster = c("village"))
yield5 <- feols(log(yield) ~ pm25 + rain_z + temp_mean | village + year, data = df %>% filter(season=="winter"), cluster = c("village"))

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
yieldtable <- yieldtable[-c(12,13),]
# Turn to matrix so that duplicate row names are allowed
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("particulate matter", "(PM 2.5, '000s)", "rain (z)", "", "mean temp (10s)", "",
                          "fixed effects:", "village-season", "year", "district-year-season", "village", "observations")
yieldtable[7,] <- ""
yieldtable[1,1] <- "0.0000"
yieldtable[2,1] <- "(0.0000)"
saveRDS(yieldtable, "pollution_development/draft/tables/yieldtablepm.rds")








df <- df %>% group_by(shrid) %>% 
                mutate(wind_max = max(wind)) %>% 
                ungroup() %>% 
                mutate(wind_max_median = median(wind_max))



yield1 <- feols(log(yield) ~ wind + rain_z + temp_mean | village^season + year, data = df %>% filter(wind_max>wind_max_median), cluster = c("village"))
yield2 <- feols(log(yield) ~ wind + rain_z + temp_mean | village^season + year, data = df %>% filter(wind_max<=wind_max_median), cluster = c("village"))
yield3 <- feols(log(yield) ~ wind*rain_z + temp_mean | village^season + year, data = df, cluster = c("village"))
yield4 <- feols(log(yield) ~ wind + wind^2 + rain_z + temp_mean | village^season + year, data = df, cluster = c("village"))
yield5 <- feols(log(yield) ~ wind*original_yield + rain_z + temp_mean | village^season + year, data = df, cluster = c("village"))

yieldtablehet <- etable(
                        yield1, yield2, yield3, yield4, yield5,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = 3,
                        fitstat = c("n"),
                        coefstat = "se"
                        )
yieldtablehet <- yieldtablehet[-c(16,17),]
yieldtablehet[13,] <- ""
yieldtablehet <- as.matrix(yieldtablehet)
rownames(yieldtablehet) <- c("wind", "", "rain (z)", "", "mean temp (10s)", "", "wind x rain", "", "wind squared", "", "wind x starting yield", "",
                             "fixed effects:", "village-season", "year", "observations")
saveRDS(yieldtablehet, "pollution_development/draft/tables/yieldtablehet.rds")








df <- df %>% mutate(shrid_season = paste0(shrid, "-", season))
df <- panel(df, ~ shrid_season + year, duplicate.method = "first")

yield1 <- feols(log(yield) ~ f(wind, -1:1) + rain_z + temp_mean | village^season + year, data = df, cluster = c("village"))
yield2 <- feols(log(yield) ~ f(wind, -1:1) + rain_z + temp_mean | village^season + year^season^distfe, data = df, cluster = c("village"))


yieldtableleads <- etable(
                        yield1, yield2,
                        se.below = TRUE,
                        depvar = FALSE,
                        signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                        digits = 3,
                        fitstat = c("n"),
                        coefstat = "se"
                        )
yieldtableleads <- yieldtableleads[-c(15,16),]
yieldtableleads[11,] <- ""
yieldtableleads[5,2] <- "0.0000"
yieldtableleads[6,2] <- "(0.0000)"
yieldtableleads <- as.matrix(yieldtableleads)
rownames(yieldtableleads) <- c("wind (lag)", "", "wind", "", "wind (lead)", "", "rain (z)", "", "mean temp (10s)", "",
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
yield2 <- feols(log(yield) ~ wind + rain_z + temp_mean | village^season + year, data = df_new, cluster = c("village"))
yield3 <- feols(log(yield) ~ wind + rain_z + temp_mean | village^season + year^season^distfe, data = df_new, cluster = c("village"))
yield4 <- feols(log(yield) ~ wind + rain_z + temp_mean | village + year, data = df_new %>% filter(season=="monsoon"), cluster = c("village"))
yield5 <- feols(log(yield) ~ wind + rain_z + temp_mean | village + year, data = df_new %>% filter(season=="winter"), cluster = c("village"))

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
yieldtable <- yieldtable[-c(12,13),]
# Turn to matrix so that duplicate row names are allowed
yieldtable <- as.matrix(yieldtable)
rownames(yieldtable) <- c("wind", "", "rain (z)", "", "mean temp (10s)", "",
                          "fixed effects:", "village-season", "year", "district-year-season", "village", "observations")
yieldtable[7,] <- ""
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
                     y = "households interviewed") +
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





# WAGES
df$f_wages_all <- df$f_wages
df$f_wages_all[is.na(df$f_wages_all)==T] <- 0
df$nf_wages_all <- df$nf_wages
df$nf_wages_all[is.na(df$nf_wages_all)==T] <- 0

labor2 <- feols(log(f_wages/days_f) ~ wind + ..ctrl | district[year] + year,
                  data = df %>% filter(f_wages_all>0),
                  cluster = "district")
labor3 <- feols(log(nf_wages/days_nf) ~ wind + ..ctrl | district[year] + year,
                  data = df %>% filter(nf_wages_all>0),
                  cluster = "district")
labor4 <- feols(log((f_wages_all + nf_wages_all)/(days_f + days_nf)) ~ wind + ..ctrl | district[year] + year,
                  data = df %>% filter(f_wages_all>0 | nf_wages_all>0),
                  cluster = "district")


labortable <- etable(
                      labor2, labor3, labor4,
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
saveRDS(labortable, "pollution_development/draft/tables/wagestable.rds")







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

summary(df$days_wage[df$age<37.5])
summary(df$days_self[df$age<37.5])
summary(df$days_f[df$age<37.5])
summary(df$days_nf[df$age<37.5])


summary(df$days_wage[df$age>37.5])
summary(df$days_self[df$age>37.5])
summary(df$days_f[df$age>37.5])
summary(df$days_nf[df$age>37.5])

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








labor2 <- feols((days_self + days_wage) ~ wind + wind_old + ..ctrl | district^year^month_int,
                  data = df,
                  cluster = "district")
labor3 <- feols(days_self ~ wind + wind_old + ..ctrl | district^year^month_int,
                  data = df,
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + wind_old + ..ctrl | district^year^month_int,
                  data = df,
                  cluster = "district")
labor5 <- feols(days_f ~ wind + wind_old + ..ctrl | district^year^month_int,
                  data = df,
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + wind_old + ..ctrl | district^year^month_int,
                  data = df,
                  cluster = "district")


labortable <- etable(
                     labor2, labor3, labor4, labor5, labor6,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = 3,
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "poly"), drop = "educ"
                      )
labortable <- labortable[-c(8:9),]
labortable <- as.matrix(labortable)
rownames(labortable) <- c("wind", "", "wind times age>=38", "", "controls", "fixed effects:",
                          "district-year-month",
                          "observations")
labortable[c(6),] <- " "
saveRDS(labortable, "pollution_development/draft/tables/labortableoldstate.rds")










labor1 <- feols((days_self + days_wage) ~ wind+ wind_old | district[year] + year,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor2 <- feols((days_self + days_wage) ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor3 <- feols(days_self ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor5 <- feols(days_f ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==1),
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






labor1 <- feols((days_self + days_wage) ~ wind+ wind_old | district[year] + year,
                  data = df %>% filter(rural==0),
                  cluster = "district")
labor2 <- feols((days_self + days_wage) ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==0),
                  cluster = "district")
labor3 <- feols(days_self ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==0),
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==0),
                  cluster = "district")
labor5 <- feols(days_f ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==0),
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + wind_old + ..ctrl | district[year] + year,
                  data = df %>% filter(rural==0 ),
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




