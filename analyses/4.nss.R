# Purpose: This script estimates effects with the NSS data. I save the results to load into the markdown script
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
pollution <- pollution %>% mutate(
                                  temp_mean = (month_tmin + month_tmax)/200,
                                  season_temp_mean = (season_tmin + season_tmax)/200,
                                  month_precip = log(month_precip + 1),
                                  season_precip = log(season_precip + 1),
                                  pm25 = log(pm25)
                                  )
df <- df %>% left_join(pollution, by = c("state_merge", "district_merge", "year", "month_int"))

# MONSOON ONLY
df <- df %>% filter(month_int %in% c(5,6,7,8,9,10,11))

# control variables
# NOTE: "season' variables are defined as the season UP TO THAT POINT, so interact with month_int factor
setFixest_fml(..ctrl = ~ poly(female, 1) + poly(age, 2) + educ + temp_mean*month_precip + temp_mean^2 + month_precip^2 + 
                as_factor(month_int)*season_temp_mean*season_precip + as_factor(month_int)*season_temp_mean^2 + as_factor(month_int)*season_precip^2)




# means
summary(df$days_f[df$month_int==6 & df$rural==1])
summary(df$days_f[df$month_int==7 & df$rural==1])
summary(df$days_f[df$month_int==8 & df$rural==1])
summary(df$days_f[df$month_int==9 & df$rural==1])
summary(df$days_f[df$month_int==10 & df$rural==1])





labor1 <- feols((days_self + days_wage) ~ wind | district + month,
                  data = df,
                  cluster = "district")
labor2 <- feols((days_f) ~ wind + ..ctrl | district + month,
                  data = df,
                  cluster = "district")
labor3 <- feols(days_self ~ wind + ..ctrl | district + month,
                  data = df,
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + ..ctrl | district + month,
                  data = df,
                  cluster = "district")
labor5 <- feols(days_f ~ wind + ..ctrl | district + month,
                  data = df,
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + ..ctrl | district + month,
                  data = df,
                  cluster = "district")


labortable <- etable(
                      labor1, labor2, labor3, labor4, labor5, labor6,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r3",
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "poly"), 
                      keep = "wind"
                      )
labortable <- labortable[-c(7:8),]
labortable <- as.matrix(labortable)
labortable[c(4),] <- " "
# Add means of DV
means.row <- c(
               format(mean(df$days_self + df$days_wage), digits = 3, nsmall = 3),
               format(mean(df$days_self + df$days_wage), digits = 3, nsmall = 3),
               format(mean(df$days_self), digits = 3, nsmall = 3),
               format(mean(df$days_wage), digits = 3, nsmall = 3),
               format(mean(df$days_f), digits = 3, nsmall = 3),
               format(mean(df$days_nf), digits = 3, nsmall = 3)
               )
means.row <- matrix(means.row, nrow = 1)
colnames(means.row) <- c("labor1", "labor2", "labor3", "labor4", "labor5", "labor6")
labortable <- rbind(labortable[1:(nrow(labortable)-1),], means.row, labortable[nrow(labortable),])
rownames(labortable) <- c("wind", "", "controls", "fixed effects:",
                          "district", "state-month",
                          "DV mean", "observations")
saveRDS(labortable, "pollution_development/draft/tables/labor1all.rds")









labor1 <- feols((days_self + days_wage) ~ wind | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor2 <- feols((days_self + days_wage) ~ wind + ..ctrl | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor3 <- feols(days_self ~ wind + ..ctrl | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor4 <- feols(days_wage ~ wind + ..ctrl | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor5 <- feols(days_f ~ wind + ..ctrl | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor6 <- feols(days_nf ~ wind + ..ctrl | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")


labortable <- etable(
                      labor1, labor2, labor3, labor4, labor5, labor6,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r3",
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "poly"), 
                      keep = "wind"
                      )
labortable <- labortable[-c(7:8),]
labortable <- as.matrix(labortable)
rownames(labortable) <- c("wind", "", "controls", "fixed effects:",
                          "district", "state-month",
                          "observations")
labortable[c(4),] <- " "
# Add means of DV
means.row <- c(
               format(mean(df$days_self[df$rural==1] + df$days_wage[df$rural==1]), digits = 3, nsmall = 3),
               format(mean(df$days_self[df$rural==1] + df$days_wage[df$rural==1]), digits = 3, nsmall = 3),
               format(mean(df$days_self[df$rural==1]), digits = 3, nsmall = 3),
               format(mean(df$days_wage[df$rural==1]), digits = 3, nsmall = 3),
               format(mean(df$days_f[df$rural==1]), digits = 3, nsmall = 3),
               format(mean(df$days_nf[df$rural==1]), digits = 3, nsmall = 3)
               )
means.row <- matrix(means.row, nrow = 1)
colnames(means.row) <- c("labor1", "labor2", "labor3", "labor4", "labor5", "labor6")
labortable <- rbind(labortable[1:(nrow(labortable)-1),], means.row, labortable[nrow(labortable),])
rownames(labortable) <- c("wind", "", "controls", "fixed effects:",
                          "district", "state-month",
                          "DV mean", "observations")
saveRDS(labortable, "pollution_development/draft/tables/labor2rural.rds")












df <- df %>% mutate(
  wind0 = wind*(month_int==5),
  wind1 = wind*(month_int==6),
  wind2 = wind*(month_int==7),
  wind3 = wind*(month_int==8),
  wind4 = wind*(month_int==9),
  wind5 = wind*(month_int==10),
  wind6 = wind*(month_int==11)
)

labor1 <- feols(days_f ~ wind0 + wind1 + wind2 + wind3 + wind4 + wind5 + wind6 + ..ctrl | district + month,
                data = df %>% filter(rural==1),
                cluster = "district")
labor2 <- feols(days_nf ~ wind0 + wind1 + wind2 + wind3 + wind4 + wind5 + wind6 + ..ctrl | district + month,
                data = df %>% filter(rural==1),
                cluster = "district")

labortable <- etable(
                      labor1, labor2,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r3",
                      fitstat = c("n"),
                      coefstat = "se",
                      group = list(controls = "poly"), 
                      keep = "wind"
                    )
labortable <- labortable[-c(19:20),]
labortable <- as.matrix(labortable)
rownames(labortable) <- c("May", " ", "June", " ", "July", " ", "Aug.", " ", "Sept.", " ", "Oct.", " ", "Nov.", " ",
                          "controls",
                          "fixed effects:", "district", "year-month",
                          "observations")
labortable[c(16),] <- " "
saveRDS(labortable, "pollution_development/draft/tables/labor3month.rds")

















### Plots


monthplot <- df %>% 
  group_by(month_int, rural) %>%
  mutate(
    days_f = mean(days_f),
    days_nf = mean(days_nf)
  ) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  mutate(
    rural_str = ifelse(rural==0, "no", "yes")
  ) %>%
  dplyr::select(month = month_int, rural = rural_str, days_f, days_nf)

monthplot <- ggplot() +
  geom_line(data = monthplot %>% filter(rural=="yes"), aes(y = days_f, x = month)) +
  #scale_color_viridis_d(option = "viridis", begin = 0.25, end = 0.75) +
  labs(x = "month of the season",
       y = "mean farm days") +
  scale_x_continuous(breaks = seq(from = 6, to = 10, by = 1)) +
  scale_y_continuous(limits = c(0,2)) +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.8))
saveRDS(monthplot, "pollution_development/draft/tables/monthplot.rds")



laborplot <- ggplot(data = df) +
  geom_histogram(aes(x = date), binwidth = 7, color = "gray") + 
  labs(x = "date",
       y = "households interviewed") +
  theme_minimal()
saveRDS(laborplot, "pollution_development/draft/tables/laborplot.rds")







