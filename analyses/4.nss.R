# Purpose: This script estimates effects with the NSS data. I save the results to load into the markdown script
# Author: Josh Merfeld
# Date: February 25th, 2023

rm(list = ls())

library(tidyverse)
library(lubridate)
library(viridis)
library(fixest)
library(sf)
library(tree)
library(marginaleffects)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../")
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

# MONSOON ONLY
df <- df %>% filter(month_int %in% c(6,7,8,9,10))

df$precip_sum <- df$month6_precip
df$precip_sum[month(df$date)==7] <- df$month6_precip[month(df$date)==7] + df$month7_precip[month(df$date)==7]
df$precip_sum[month(df$date)==8] <- df$month6_precip[month(df$date)==8] + df$month7_precip[month(df$date)==8] + df$month8_precip[month(df$date)==8]
df$precip_sum[month(df$date)==9] <- df$month6_precip[month(df$date)==9] + df$month7_precip[month(df$date)==9] + df$month8_precip[month(df$date)==9] + df$month9_precip[month(df$date)==9]
df$precip_sum[month(df$date)==10] <- df$month6_precip[month(df$date)==10] + df$month7_precip[month(df$date)==10] + df$month8_precip[month(df$date)==10] + df$month9_precip[month(df$date)==10] + df$month_precip[month(df$date)==10]

# create seasons means
df <- df %>% 
      group_by(year, district) %>%
      mutate(temp_mean = mean((month_tmin + month_tmax)/2),
             precip_sum_season = mean(month_precip)) %>%
      ungroup()

# add mean of rainfall
means <- read_csv(paste0("data/clean/terra_district/precip_mean_monthALL.csv"))
means <- means %>%
          rename(state_merge = ST_CEN_CD, district_merge = DT_CEN_CD) %>%
          mutate(state_merge = as.numeric(state_merge),
                 district_merge = as.numeric(district_merge))
df <- df %>% 
        left_join(means, by = c("state_merge", "district_merge"))
df$precip_sum_z <- (df$precip_sum - df$precip_mean_month6)/df$precip_sd_month6
df$precip_sum_z[month(df$date)==7] <- (df$precip_sum[month(df$date)==7] - df$precip_mean_month7[month(df$date)==7])/df$precip_sd_month7[month(df$date)==7]
df$precip_sum_z[month(df$date)==8] <- (df$precip_sum[month(df$date)==8] - df$precip_mean_month8[month(df$date)==8])/df$precip_sd_month8[month(df$date)==8]
df$precip_sum_z[month(df$date)==9] <- (df$precip_sum[month(df$date)==9] - df$precip_mean_month9[month(df$date)==9])/df$precip_sd_month9[month(df$date)==9]
df$precip_sum_z[month(df$date)==10] <- (df$precip_sum[month(df$date)==10] - df$precip_mean_month10[month(df$date)==10])/df$precip_sd_month10[month(df$date)==10]

df$bin0 <- 0
df$bin0[df$precip_sum_z<(-1)] <- 1
df$bin1 <- 0
df$bin1[df$precip_sum_z>1] <- 1

df$monthofyear <- month(df$date)
df$weekofyear <- week(df$date)

# control variables
# NOTE: "season' variables are defined as the season UP TO THAT POINT, so interact with month_int factor
setFixest_fml(..ctrl1 = ~  poly(female, 1) + poly(age, 2) + educ + temp_mean*precip_sum_z + month_precip + temp_mean^2 + precip_sum_z^2)
setFixest_fml(..ctrl2 = ~ poly(female, 1) + poly(age, 2) + educ + as_factor(weekofyear)*(temp_mean*precip_sum_z + precip_sum_z*temp_mean^2))



labor1 <- feols(days_self ~ wind + ..ctrl2 | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor2 <- feols(days_wage ~ wind + ..ctrl2 | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor3 <- feols(days_f ~ wind + ..ctrl2 | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")
labor4 <- feols(days_nf ~ wind + ..ctrl2 | district + month,
                  data = df %>% filter(rural==1),
                  cluster = "district")


labortable <- etable(
                      labor3, labor4, labor1, labor2,
                      se.below = TRUE,
                      depvar = FALSE,
                      signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
                      digits = "r3",
                      digits.stats = "r3",
                      fitstat = c("ivwald", "n"),
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






setFixest_fml(..ctrl2 = ~ poly(female, 1) + poly(age, 2) + educ + as_factor(weekofyear)*(temp_mean*precip_sum_z + precip_sum_z*temp_mean^2))
df$weekofyear <- month(df$date)
funcall <- paste0("days_f ~ wind")
for (w in 7:10){
  df[[paste0("windtimesweek", w)]] <- df$wind*as.numeric(df$weekofyear==w)
  funcall <- paste0(funcall, " + ", paste0("windtimesweek", w))
}
funcall <- paste0(funcall, " + ..ctrl2 | district + month")
funcall <- as.formula(funcall)
funcall

daysresid <- feols(funcall,
                    data = df %>% filter(rural==1),
                    cluster = "district")
vcovmat <- vcov(daysresid, cluster = "district")

coefs <- hypotheses(daysresid, "wind = 0", vcov = vcovmat)$statistic[1]
for (w in 7:10){
  coefs <- c(coefs, hypotheses(daysresid, paste0("wind + windtimesweek", w, " = 0"), vcov = vcovmat)$statistic[1])
}

ggplot() + 
  geom_point(aes(x = unique(df$weekofyear), y = coefs)) +
  theme_minimal()



ggplot() +
  geom_smooth(aes(x = windresid, y = dayofyear))





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

labor1 <- feols(days_f ~ wind1 + wind2 + wind3 + wind4 + wind5 + ..ctrl2 | district + month,
                data = df %>% filter(rural==1),
                cluster = "district")
labor2 <- feols(days_nf ~ wind1 + wind2 + wind3 + wind4 + wind5 + ..ctrl2 | district + month,
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
labortable <- labortable[-c(15:16),]
labortable <- as.matrix(labortable)
labortable <- labortable[,-1]
rownames(labortable) <- c("June", " ", "July", " ", "Aug.", " ", "Sept.", " ", "Oct.", " ",
                          "controls",
                          "fixed effects:", "district", "year-month",
                          "observations")
labortable[c(12),] <- " "
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









# treatment effect heterogeneity ------------------------------------
# first get centered values
df_centered <- df %>%
                group_by(district) %>%
                mutate(
                       month1 = as.numeric(month_int==6),
                       month2 = as.numeric(month_int==7),
                       month3 = as.numeric(month_int==8),
                       month4 = as.numeric(month_int==9),
                       month5 = as.numeric(month_int==10),
                       wind = wind - mean(wind),
                       wind1 = wind*(month_int==6) - mean(wind*(month_int==6)),
                       wind2 = wind*(month_int==7) - mean(wind*(month_int==7)),
                       wind3 = wind*(month_int==8) - mean(wind*(month_int==8)),
                       wind4 = wind*(month_int==9) - mean(wind*(month_int==9)),
                       wind5 = wind*(month_int==10) - mean(wind*(month_int==10)),
                       educ = as.numeric(educ) - mean(as.numeric(educ)),
                       distfe = min(row_number())
                       ) %>%
                ungroup() %>%
                dplyr::select("month_int", "female", "age", "educ", "temp_mean", "precip_sum",
                              "wind1", "wind2", "wind3", "wind4", "wind5",
                              "days_f", "days_nf", "wind", "district", "distfe") %>%
                filter(month_int %in% c(6, 7, 8, 9, 10))
df_centered <- df_centered[complete.cases(df_centered),]
xgboost <- xgboost(data = as.matrix(df_centered[,c("wind1", "wind2", "wind3", "wind4", "wind5", 
                                             "female", "age", "educ", "temp_mean", "precip_sum")]),
                   label = df_centered$days_f,
                   nrounds = 250,
                   subsample = 0.6,
                   colsample_bytree = 0.6
                   )







