# Purpose: This script cleans the NSS data
# Author: Josh Merfeld
# Date: December 26th, 2022

rm(list = ls())

library(tidyverse)
library(haven)
library(lubridate)
library(readxl)


# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# We want to get the wd up to the main folder
# Need to go up two levels
setwd("../../")
# Double check
getwd()    # check


# NSS 61 --------------------------------------------------------------------------------------------------------------
# surveytiming -------------------------------------------------------
df_hh <- read_dta("data/nss/nss61/Block_1_2_and_3_level_01.dta")
df_hh <- df_hh %>% mutate(hid = paste(Sector, State_region, District, Stratum, Sub_stratum, Sub_round, Sub_sample, FOD_sub_region, 
                                      Hamlet_group_sub_block_no, Second_stage_stratum, Sample_hhld_no, sep = "-"),
                          date = as.Date(paste0("20", substr(Date_survey, 5, 6), "-", substr(Date_survey, 3, 4), "-", substr(Date_survey, 1, 2)), "%Y-%m-%d"),
                          state61 = STATE_CODE,
                          district61 = DISTRICT_CODE,
                          weight_combined = WEIGHT_COMBINED,
                          weight_subround = WEIGHT_SUB_ROUND,
                          weight_subsample = WEIGHT_SUB_SAMPLE) %>%
                    select(hid, state61, district61, date, weight_combined, weight_subround, weight_subsample)
df_hh <- df_hh[complete.cases(df_hh),]

# To join, with concordance files, need to do some changes
df_hh <- df_hh %>% mutate(state61 = as.numeric(state61), 
                          district61 = as.numeric(substr(district61, str_length(district61)-1, str_length(district61))))

# demographics -------------------------------------------------------
df_ind <- read_dta("data/nss/nss61/Block_4_level_03.dta")
df_ind <- df_ind %>% mutate(hid = paste(Sector, State_region, District, Stratum, Sub_stratum, Sub_round, Sub_sample, FOD_sub_region, 
                                        Hamlet_group, Second_stage_stratum, Sampe_hhld_no, sep = "-"),
                            pid = Personal_serial_no,
                            female = as.numeric(Sex=="2"),
                            age = Age,
                            educ = as.numeric(General_education),
                            relation = as.numeric(Relation_to_head)) %>%
                      select(hid, pid, female, age, educ, relation) %>%
                      group_by(hid) %>% arrange(hid, relation) %>%
                      mutate(head_female = female[1],
                             head_age = age[1],
                             head_educ = educ[1],
                             hhsize = max(row_number())) %>%
                      ungroup()

# labor -------------------------------------------------------
df_labor <- read_dta("data/nss/nss61/Block_5pt3_level_06.dta")

# Some individual cleaning
# NAs for days should be missings
df_labor$Total_no_of_days_in_current_acti[is.na(df_labor$Total_no_of_days_in_current_acti)==T] <- 0
# Days in different types of labor
df_labor$days_self <- 0
df_labor$days_wage <- 0
df_labor$days_domestic <- 0
# Now labor types
df_labor$days_self[df_labor$Current_day_activity_Status %in% c("11", "12", "21")] <- df_labor$days_self[df_labor$Current_day_activity_Status %in% c("11", "12", "21")] + 
                                                                                      df_labor$Total_no_of_days_in_current_acti[df_labor$Current_day_activity_Status %in% c("11", "12", "21")]
df_labor$days_wage[df_labor$Current_day_activity_Status %in% c("31", "41", "51")] <- df_labor$days_wage[df_labor$Current_day_activity_Status %in% c("31", "41", "51")] + 
                                                                                      df_labor$Total_no_of_days_in_current_acti[df_labor$Current_day_activity_Status %in% c("31", "41", "51")] 
df_labor$days_domestic[df_labor$Current_day_activity_Status %in% c("92", "93")] <- df_labor$days_domestic[df_labor$Current_day_activity_Status %in% c("92", "93")] + 
                                                                                          df_labor$Total_no_of_days_in_current_acti[df_labor$Current_day_activity_Status %in% c("92", "93")]
# Now farm/non-farm
df_labor$days_f <- 0
df_labor$days_f[df_labor$Current_day_activity_NIC_1998_co %in% c("01", "02")] <- df_labor$days_f[df_labor$Current_day_activity_NIC_1998_co %in% c("01", "02")] + 
                                                                                  df_labor$Total_no_of_days_in_current_acti[df_labor$Current_day_activity_NIC_1998_co %in% c("01", "02")]
df_labor$days_nf <- (df_labor$days_self + df_labor$days_wage - df_labor$days_f)

# Wages
df_labor$f_wages <- NA
df_labor$f_wages[df_labor$days_f>0] <- df_labor$Wage_salary_earnings_total_durin[df_labor$days_f>0]
df_labor$nf_wages <- NA
df_labor$nf_wages[df_labor$days_nf>0] <- df_labor$Wage_salary_earnings_total_durin[df_labor$days_nf>0]


# DAILY - use below
df_labor_daily <- df_labor


df_labor <- df_labor %>% mutate(hid = paste(Sector, State_region, District, Stratum, Sub_stratum, Sub_round, Sub_sample, FOD, 
                                        Hamlet, Second_stratum, Sample_hhld_no, sep = "-"),
                                pid = Personal_srl_no) %>%
                          select(hid, pid, days_self, days_wage, days_domestic, days_f, days_nf, f_wages, nf_wages) %>%
                          group_by(hid, pid) %>%
                          mutate(days_self = sum(days_self), 
                                 days_wage = sum(days_wage), 
                                 days_domestic = sum(days_domestic), 
                                 days_f = sum(days_f), 
                                 days_nf = sum(days_nf),
                                 f_wages = sum(f_wages), 
                                 nf_wages = sum(nf_wages)) %>%
                          filter(row_number()==1) %>%
                          ungroup()

# Daily again
# Intensity
df_labor_daily$Current_day_activity_intensity_7[is.na(df_labor_daily$Current_day_activity_intensity_7)==T | 
                                                  !(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$Current_day_activity_intensity_6[is.na(df_labor_daily$Current_day_activity_intensity_6)==T | 
                                                  !(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$Current_day_activity_intensity_5[is.na(df_labor_daily$Current_day_activity_intensity_5)==T | 
                                                  !(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$Current_day_activity_intensity_4[is.na(df_labor_daily$Current_day_activity_intensity_4)==T | 
                                                  !(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$Current_day_activity_intensity_3[is.na(df_labor_daily$Current_day_activity_intensity_3)==T | 
                                                  !(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$Current_day_activity_intensity_2[is.na(df_labor_daily$Current_day_activity_intensity_2)==T | 
                                                  !(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$Current_day_activity_intensity_1[is.na(df_labor_daily$Current_day_activity_intensity_1)==T | 
                                                  !(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
# Wage
df_labor_daily$wage_day1 <- df_labor_daily$Current_day_activity_intensity_1
df_labor_daily$wage_day1[!(df_labor_daily$Current_day_activity_Status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day2 <- df_labor_daily$Current_day_activity_intensity_2
df_labor_daily$wage_day2[!(df_labor_daily$Current_day_activity_Status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day3 <- df_labor_daily$Current_day_activity_intensity_3
df_labor_daily$wage_day3[!(df_labor_daily$Current_day_activity_Status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day4 <- df_labor_daily$Current_day_activity_intensity_4
df_labor_daily$wage_day4[!(df_labor_daily$Current_day_activity_Status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day5 <- df_labor_daily$Current_day_activity_intensity_5
df_labor_daily$wage_day5[!(df_labor_daily$Current_day_activity_Status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day6 <- df_labor_daily$Current_day_activity_intensity_6
df_labor_daily$wage_day6[!(df_labor_daily$Current_day_activity_Status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day7 <- df_labor_daily$Current_day_activity_intensity_7
df_labor_daily$wage_day7[!(df_labor_daily$Current_day_activity_Status %in% c("31", "41", "51"))] <- 0
# Self
df_labor_daily$self_day1 <- df_labor_daily$Current_day_activity_intensity_1
df_labor_daily$self_day1[!(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day2 <- df_labor_daily$Current_day_activity_intensity_2
df_labor_daily$self_day2[!(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day3 <- df_labor_daily$Current_day_activity_intensity_3
df_labor_daily$self_day3[!(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day4 <- df_labor_daily$Current_day_activity_intensity_4
df_labor_daily$self_day4[!(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day5 <- df_labor_daily$Current_day_activity_intensity_5
df_labor_daily$self_day5[!(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day6 <- df_labor_daily$Current_day_activity_intensity_6
df_labor_daily$self_day6[!(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day7 <- df_labor_daily$Current_day_activity_intensity_7
df_labor_daily$self_day7[!(df_labor_daily$Current_day_activity_Status %in% c("11", "12", "21"))] <- 0
# non-farm
df_labor_daily$nf_day1 <- df_labor_daily$Current_day_activity_intensity_1
df_labor_daily$nf_day1[(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$nf_day2 <- df_labor_daily$Current_day_activity_intensity_2
df_labor_daily$nf_day2[(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$nf_day3 <- df_labor_daily$Current_day_activity_intensity_3
df_labor_daily$nf_day3[(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$nf_day4 <- df_labor_daily$Current_day_activity_intensity_4
df_labor_daily$nf_day4[(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$nf_day5 <- df_labor_daily$Current_day_activity_intensity_5
df_labor_daily$nf_day5[(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$nf_day6 <- df_labor_daily$Current_day_activity_intensity_6
df_labor_daily$nf_day6[(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$nf_day7 <- df_labor_daily$Current_day_activity_intensity_7
df_labor_daily$nf_day7[(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
# farm
df_labor_daily$f_day1 <- df_labor_daily$Current_day_activity_intensity_1
df_labor_daily$f_day1[!(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$f_day2 <- df_labor_daily$Current_day_activity_intensity_2
df_labor_daily$f_day2[!(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$f_day3 <- df_labor_daily$Current_day_activity_intensity_3
df_labor_daily$f_day3[!(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$f_day4 <- df_labor_daily$Current_day_activity_intensity_4
df_labor_daily$f_day4[!(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$f_day5 <- df_labor_daily$Current_day_activity_intensity_5
df_labor_daily$f_day5[!(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$f_day6 <- df_labor_daily$Current_day_activity_intensity_6
df_labor_daily$f_day6[!(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0
df_labor_daily$f_day7 <- df_labor_daily$Current_day_activity_intensity_7
df_labor_daily$f_day7[!(df_labor_daily$Current_day_activity_NIC_1998_co %in% c("01", "02"))] <- 0

df_labor_daily <- df_labor_daily %>% mutate(hid = paste(Sector, State_region, District, Stratum, Sub_stratum, Sub_round, Sub_sample, FOD, 
                                                        Hamlet, Second_stratum, Sample_hhld_no, sep = "-"),
                                            pid = Personal_srl_no) %>%
                                      select(hid, pid, day7 = Current_day_activity_intensity_7, day6 = Current_day_activity_intensity_6,
                                             day5 = Current_day_activity_intensity_5, day4 = Current_day_activity_intensity_4,
                                             day3 = Current_day_activity_intensity_3, day2 = Current_day_activity_intensity_2,
                                             day1 = Current_day_activity_intensity_1,
                                             starts_with("f_day"), starts_with("nf_day"), starts_with("wage_day"), starts_with("self_day")) %>%
                                      group_by(hid, pid) %>%
                                      mutate(day7 = sum(day7), day6 = sum(day6), day5 = sum(day5),
                                             day4 = sum(day4), day3 = sum(day3), day2 = sum(day2),
                                             day1 = sum(day1),
                                             wage_day7 = sum(wage_day7), wage_day6 = sum(wage_day6),
                                             wage_day5 = sum(wage_day5), wage_day4 = sum(wage_day4),
                                             wage_day3 = sum(wage_day3), wage_day2 = sum(wage_day2),
                                             wage_day1 = sum(wage_day1),
                                             self_day7 = sum(self_day7), self_day6 = sum(self_day6),
                                             self_day5 = sum(self_day5), self_day4 = sum(self_day4),
                                             self_day3 = sum(self_day3), self_day2 = sum(self_day2),
                                             self_day1 = sum(self_day1),
                                             nf_day7 = sum(nf_day7), nf_day6 = sum(nf_day6),
                                             nf_day5 = sum(nf_day5), nf_day4 = sum(nf_day4),
                                             nf_day3 = sum(nf_day3), nf_day2 = sum(nf_day2),
                                             nf_day1 = sum(nf_day1),
                                             f_day7 = sum(f_day7), f_day6 = sum(f_day6),
                                             f_day5 = sum(f_day5), f_day4 = sum(f_day4),
                                             f_day3 = sum(f_day3), f_day2 = sum(f_day2),
                                             f_day1 = sum(f_day1)) %>%
                                      filter(row_number()==1) %>%
                                      ungroup()

# Now need to gather
df_labor_daily <- df_labor_daily %>% gather(
                                             "days",
                                             "intensity",
                                             day7, day6, day5, day4, day3, day2, day1
                                             )

# Some cleaning
df_labor_daily$wage_days <- NA
df_labor_daily$wage_days[df_labor_daily$days=="day7"] <- df_labor_daily$wage_day7[df_labor_daily$days=="day7"]
df_labor_daily$wage_days[df_labor_daily$days=="day6"] <- df_labor_daily$wage_day6[df_labor_daily$days=="day6"]
df_labor_daily$wage_days[df_labor_daily$days=="day5"] <- df_labor_daily$wage_day5[df_labor_daily$days=="day5"]
df_labor_daily$wage_days[df_labor_daily$days=="day4"] <- df_labor_daily$wage_day4[df_labor_daily$days=="day4"]
df_labor_daily$wage_days[df_labor_daily$days=="day3"] <- df_labor_daily$wage_day3[df_labor_daily$days=="day3"]
df_labor_daily$wage_days[df_labor_daily$days=="day2"] <- df_labor_daily$wage_day2[df_labor_daily$days=="day2"]
df_labor_daily$wage_days[df_labor_daily$days=="day1"] <- df_labor_daily$wage_day1[df_labor_daily$days=="day1"]
df_labor_daily$self_days <- NA
df_labor_daily$self_days[df_labor_daily$days=="day7"] <- df_labor_daily$self_day7[df_labor_daily$days=="day7"]
df_labor_daily$self_days[df_labor_daily$days=="day6"] <- df_labor_daily$self_day6[df_labor_daily$days=="day6"]
df_labor_daily$self_days[df_labor_daily$days=="day5"] <- df_labor_daily$self_day5[df_labor_daily$days=="day5"]
df_labor_daily$self_days[df_labor_daily$days=="day4"] <- df_labor_daily$self_day4[df_labor_daily$days=="day4"]
df_labor_daily$self_days[df_labor_daily$days=="day3"] <- df_labor_daily$self_day3[df_labor_daily$days=="day3"]
df_labor_daily$self_days[df_labor_daily$days=="day2"] <- df_labor_daily$self_day2[df_labor_daily$days=="day2"]
df_labor_daily$self_days[df_labor_daily$days=="day1"] <- df_labor_daily$self_day1[df_labor_daily$days=="day1"]
df_labor_daily$f_days <- NA
df_labor_daily$f_days[df_labor_daily$days=="day7"] <- df_labor_daily$f_day7[df_labor_daily$days=="day7"]
df_labor_daily$f_days[df_labor_daily$days=="day6"] <- df_labor_daily$f_day6[df_labor_daily$days=="day6"]
df_labor_daily$f_days[df_labor_daily$days=="day5"] <- df_labor_daily$f_day5[df_labor_daily$days=="day5"]
df_labor_daily$f_days[df_labor_daily$days=="day4"] <- df_labor_daily$f_day4[df_labor_daily$days=="day4"]
df_labor_daily$f_days[df_labor_daily$days=="day3"] <- df_labor_daily$f_day3[df_labor_daily$days=="day3"]
df_labor_daily$f_days[df_labor_daily$days=="day2"] <- df_labor_daily$f_day2[df_labor_daily$days=="day2"]
df_labor_daily$f_days[df_labor_daily$days=="day1"] <- df_labor_daily$f_day1[df_labor_daily$days=="day1"]
df_labor_daily$nf_days <- NA
df_labor_daily$nf_days[df_labor_daily$days=="day7"] <- df_labor_daily$nf_day7[df_labor_daily$days=="day7"]
df_labor_daily$nf_days[df_labor_daily$days=="day6"] <- df_labor_daily$nf_day6[df_labor_daily$days=="day6"]
df_labor_daily$nf_days[df_labor_daily$days=="day5"] <- df_labor_daily$nf_day5[df_labor_daily$days=="day5"]
df_labor_daily$nf_days[df_labor_daily$days=="day4"] <- df_labor_daily$nf_day4[df_labor_daily$days=="day4"]
df_labor_daily$nf_days[df_labor_daily$days=="day3"] <- df_labor_daily$nf_day3[df_labor_daily$days=="day3"]
df_labor_daily$nf_days[df_labor_daily$days=="day2"] <- df_labor_daily$nf_day2[df_labor_daily$days=="day2"]
df_labor_daily$nf_days[df_labor_daily$days=="day1"] <- df_labor_daily$nf_day1[df_labor_daily$days=="day1"]
df_labor_daily <- df_labor_daily %>% select(-c(f_day7, f_day6, f_day5, f_day4, f_day3, f_day2, f_day1,
                                               nf_day7, nf_day6, nf_day5, nf_day4, nf_day3, nf_day2, nf_day1,
                                               wage_day7, wage_day6, wage_day5, wage_day4, wage_day3, wage_day2, wage_day1,
                                               self_day7, self_day6, self_day5, self_day4, self_day3, self_day2, self_day1))


# merging -------------------------------------------------------
# here are the concordance identifiers
concordance <- as_tibble(read_xls("data/nss/concordance_nochange.xls"))
# Do some renaming
concordance <- concordance %>% rename(state61 = state_code_source, district61 = district_code_source)

df_labor <- df_labor %>% 
              left_join(df_ind, by = c("hid", "pid")) %>% 
              left_join(df_hh, by = c("hid"))

df_labor <- df_labor %>% left_join(concordance, by = c("state61", "district61"))
df_labor$state_merge <- df_labor$state61
df_labor$district_merge <- df_labor$district61
#df_labor <- df_labor[complete.cases(df_labor),]
write.csv(df_labor, "data/clean/nss/nss61.csv")

# And daily
df_labor_daily <- df_labor_daily %>% 
                    left_join(df_ind, by = c("hid", "pid")) %>% 
                    left_join(df_hh, by = c("hid"))
# And replace day with date
df_labor_daily$day_date <- NA
df_labor_daily$day_date[df_labor_daily$days=="day1"] <- as.Date(df_labor_daily$date[df_labor_daily$days=="day1"] - 1)
df_labor_daily$day_date[df_labor_daily$days=="day2"] <- as.Date(df_labor_daily$date[df_labor_daily$days=="day2"] - 2)
df_labor_daily$day_date[df_labor_daily$days=="day3"] <- as.Date(df_labor_daily$date[df_labor_daily$days=="day3"] - 3)
df_labor_daily$day_date[df_labor_daily$days=="day4"] <- as.Date(df_labor_daily$date[df_labor_daily$days=="day4"] - 4)
df_labor_daily$day_date[df_labor_daily$days=="day5"] <- as.Date(df_labor_daily$date[df_labor_daily$days=="day5"] - 5)
df_labor_daily$day_date[df_labor_daily$days=="day6"] <- as.Date(df_labor_daily$date[df_labor_daily$days=="day6"] - 6)
df_labor_daily$day_date[df_labor_daily$days=="day7"] <- as.Date(df_labor_daily$date[df_labor_daily$days=="day7"] - 7)
df_labor_daily$day_date_l1 <- NA
df_labor_daily$day_date_l1[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 2
df_labor_daily$day_date_l1[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 3
df_labor_daily$day_date_l1[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 4
df_labor_daily$day_date_l1[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 5
df_labor_daily$day_date_l1[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 6
df_labor_daily$day_date_l1[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 7
df_labor_daily$day_date_l1[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 8
df_labor_daily$day_date_l2 <- NA
df_labor_daily$day_date_l2[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 3
df_labor_daily$day_date_l2[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 4
df_labor_daily$day_date_l2[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 5
df_labor_daily$day_date_l2[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 6
df_labor_daily$day_date_l2[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 7
df_labor_daily$day_date_l2[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 8
df_labor_daily$day_date_l2[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 9
df_labor_daily <- df_labor_daily %>% left_join(concordance, by = c("state61", "district61"))
df_labor_daily$state_merge <- df_labor_daily$state61
df_labor_daily$district_merge <- df_labor_daily$district61
df_labor_daily <- df_labor_daily[complete.cases(df_labor_daily),]
write.csv(df_labor_daily, "data/clean/nss/nss61_daily.csv")








# NSS 62 --------------------------------------------------------------------------------------------------------------
# surveytiming -------------------------------------------------------
df_hh <- read_dta("data/nss/nss62/NSS62_Sch10_bk_1_2.dta")
# Date needs to be of length six
df_hh <- df_hh[is.na(df_hh$S10B2_v02i)==F,]
df_hh$S10B2_v02i[str_length(df_hh$S10B2_v02i)==5] <- paste0("0", df_hh$S10B2_v02i[str_length(df_hh$S10B2_v02i)==5])
df_hh <- df_hh %>% mutate(hid = hhid,
                          date = as.Date(paste0("20", substr(S10B2_v02i, 5, 6), "-", substr(S10B2_v02i, 3, 4), "-", substr(S10B2_v02i, 1, 2)), "%Y-%m-%d"),
                          state62 = state,
                          district62 = district,
                          weight_combined = weight) %>%
                    select(hid, state62, district62, date, weight_combined)
df_hh <- df_hh[complete.cases(df_hh),]

# To join, with concordance files, need to do some changes
df_hh <- df_hh %>% mutate(state62 = as.numeric(state62), 
                          district62 = as.numeric(district62))

# demographics -------------------------------------------------------
df_ind <- read_dta("data/nss/nss62/NSS62_Sch10_bk_4.dta")
df_ind <- df_ind %>% mutate(hid = hhid,
                            pid = S10B4_v01,
                            female = as.numeric(S10B4_v04==2),
                            age = S10B4_v05,
                            educ = as.numeric(S10B4_v07),
                            relation = as.numeric(S10B4_v03)) %>%
                      select(hid, pid, female, age, educ, relation) %>%
                      group_by(hid) %>% arrange(hid, relation) %>%
                      mutate(head_female = female[1],
                             head_age = age[1],
                             head_educ = educ[1],
                             hhsize = max(row_number())) %>%
                      ungroup()

# labor -------------------------------------------------------
df_labor <- read_dta("data/nss/nss62/NSS62_Sch10_bk_6.dta")

# Some individual cleaning
# NAs for days should be missings
df_labor$S10B6_v14[is.na(df_labor$S10B6_v14)==T] <- 0
# Days in different types of labor
df_labor$days_self <- 0
df_labor$days_wage <- 0
df_labor$days_domestic <- 0
# Now labor types
df_labor$days_self[df_labor$S10B6_v04 %in% c("11", "12", "21")] <- df_labor$days_self[df_labor$S10B6_v04 %in% c("11", "12", "21")] + 
                                                                                      df_labor$S10B6_v14[df_labor$S10B6_v04 %in% c("11", "12", "21")]
df_labor$days_wage[df_labor$S10B6_v04 %in% c("31", "41", "51")] <- df_labor$days_wage[df_labor$S10B6_v04 %in% c("31", "41", "51")] + 
                                                                                      df_labor$S10B6_v14[df_labor$S10B6_v04 %in% c("31", "41", "51")] 
df_labor$days_domestic[df_labor$S10B6_v04 %in% c("92", "93")] <- df_labor$days_domestic[df_labor$S10B6_v04 %in% c("92", "93")] + 
                                                                                          df_labor$S10B6_v14[df_labor$S10B6_v04 %in% c("92", "93")]
# Now farm/non-farm
df_labor$days_f <- 0
df_labor$days_f[df_labor$S10B6_v05 %in% c("01", "02")] <- df_labor$days_f[df_labor$S10B6_v05 %in% c("01", "02")] + 
                                                              df_labor$S10B6_v14[df_labor$S10B6_v05 %in% c("01", "02")]
df_labor$days_nf <- (df_labor$days_self + df_labor$days_wage - df_labor$days_f)

# Wages
df_labor$f_wages <- NA
df_labor$f_wages[df_labor$days_f>0] <- df_labor$S10B6_v17[df_labor$days_f>0]
df_labor$nf_wages <- NA
df_labor$nf_wages[df_labor$days_nf>0] <- df_labor$S10B6_v17[df_labor$days_nf>0]

# DAILY - use below
df_labor_daily <- df_labor


df_labor <- df_labor %>% mutate(hid = hhid,
                                pid = S10B6_v01) %>%
                          select(hid, pid, days_self, days_wage, days_domestic, days_f, days_nf, f_wages, nf_wages) %>%
                          group_by(hid, pid) %>%
                          mutate(days_self = sum(days_self), 
                                 days_wage = sum(days_wage), 
                                 days_domestic = sum(days_domestic), 
                                 days_f = sum(days_f), 
                                 days_nf = sum(days_nf),
                                 f_wages = sum(f_wages), 
                                 nf_wages = sum(nf_wages)) %>%
                          filter(row_number()==1) %>%
                          ungroup()

# Daily again
df_labor_daily$S10B6_v07[is.na(df_labor_daily$S10B6_v07)==T | 
                                                  !(df_labor_daily$S10B6_v04 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$S10B6_v08[is.na(df_labor_daily$S10B6_v08)==T | 
                                                  !(df_labor_daily$S10B6_v04 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$S10B6_v09[is.na(df_labor_daily$S10B6_v09)==T | 
                                                  !(df_labor_daily$S10B6_v04 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$S10B6_v10[is.na(df_labor_daily$S10B6_v10)==T | 
                                                  !(df_labor_daily$S10B6_v04 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$S10B6_v11[is.na(df_labor_daily$S10B6_v11)==T | 
                                                  !(df_labor_daily$S10B6_v04 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$S10B6_v12[is.na(df_labor_daily$S10B6_v12)==T | 
                                                  !(df_labor_daily$S10B6_v04 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$S10B6_v13[is.na(df_labor_daily$S10B6_v13)==T | 
                                                  !(df_labor_daily$S10B6_v04 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
# Wage
df_labor_daily$wage_day1 <- df_labor_daily$S10B6_v13
df_labor_daily$wage_day1[!(df_labor_daily$S10B6_v04 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day2 <- df_labor_daily$S10B6_v12
df_labor_daily$wage_day2[!(df_labor_daily$S10B6_v04 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day3 <- df_labor_daily$S10B6_v11
df_labor_daily$wage_day3[!(df_labor_daily$S10B6_v04 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day4 <- df_labor_daily$S10B6_v10
df_labor_daily$wage_day4[!(df_labor_daily$S10B6_v04 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day5 <- df_labor_daily$S10B6_v09
df_labor_daily$wage_day5[!(df_labor_daily$S10B6_v04 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day6 <- df_labor_daily$S10B6_v08
df_labor_daily$wage_day6[!(df_labor_daily$S10B6_v04 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day7 <- df_labor_daily$S10B6_v07
df_labor_daily$wage_day7[!(df_labor_daily$S10B6_v04 %in% c("31", "41", "51"))] <- 0
# Self
df_labor_daily$self_day1 <- df_labor_daily$S10B6_v13
df_labor_daily$self_day1[!(df_labor_daily$S10B6_v04 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day2 <- df_labor_daily$S10B6_v12
df_labor_daily$self_day2[!(df_labor_daily$S10B6_v04 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day3 <- df_labor_daily$S10B6_v11
df_labor_daily$self_day3[!(df_labor_daily$S10B6_v04 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day4 <- df_labor_daily$S10B6_v10
df_labor_daily$self_day4[!(df_labor_daily$S10B6_v04 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day5 <- df_labor_daily$S10B6_v09
df_labor_daily$self_day5[!(df_labor_daily$S10B6_v04 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day6 <- df_labor_daily$S10B6_v08
df_labor_daily$self_day6[!(df_labor_daily$S10B6_v04 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day7 <- df_labor_daily$S10B6_v07
df_labor_daily$self_day7[!(df_labor_daily$S10B6_v04 %in% c("11", "12", "21"))] <- 0
# non-farm
df_labor_daily$nf_day1 <- df_labor_daily$S10B6_v13
df_labor_daily$nf_day1[(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day2 <- df_labor_daily$S10B6_v12
df_labor_daily$nf_day2[(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day3 <- df_labor_daily$S10B6_v11
df_labor_daily$nf_day3[(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day4 <- df_labor_daily$S10B6_v10
df_labor_daily$nf_day4[(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day5 <- df_labor_daily$S10B6_v09
df_labor_daily$nf_day5[(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day6 <- df_labor_daily$S10B6_v08
df_labor_daily$nf_day6[(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day7 <- df_labor_daily$S10B6_v07
df_labor_daily$nf_day7[(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
# farm
df_labor_daily$f_day1 <- df_labor_daily$S10B6_v13
df_labor_daily$f_day1[!(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$f_day2 <- df_labor_daily$S10B6_v12
df_labor_daily$f_day2[!(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$f_day3 <- df_labor_daily$S10B6_v11
df_labor_daily$f_day3[!(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$f_day4 <- df_labor_daily$S10B6_v10
df_labor_daily$f_day4[!(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$f_day5 <- df_labor_daily$S10B6_v09
df_labor_daily$f_day5[!(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$f_day6 <- df_labor_daily$S10B6_v08
df_labor_daily$f_day6[!(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0
df_labor_daily$f_day7 <- df_labor_daily$S10B6_v07
df_labor_daily$f_day7[!(df_labor_daily$S10B6_v05 %in% c("01", "02"))] <- 0

df_labor_daily <- df_labor_daily %>% mutate(hid =  hhid,
                                            pid = S10B6_v01) %>%
                                      select(hid, pid, day7 = S10B6_v07, day6 = S10B6_v08,
                                             day5 = S10B6_v09, day4 = S10B6_v10,
                                             day3 = S10B6_v11, day2 = S10B6_v12,
                                             day1 = S10B6_v13,
                                             starts_with("f_day"), starts_with("nf_day"), starts_with("wage_day"), starts_with("self_day")) %>%
                                      group_by(hid, pid) %>%
                                      mutate(day7 = sum(day7), day6 = sum(day6), day5 = sum(day5),
                                             day4 = sum(day4), day3 = sum(day3), day2 = sum(day2),
                                             day1 = sum(day1),
                                             wage_day7 = sum(wage_day7), wage_day6 = sum(wage_day6),
                                             wage_day5 = sum(wage_day5), wage_day4 = sum(wage_day4),
                                             wage_day3 = sum(wage_day3), wage_day2 = sum(wage_day2),
                                             wage_day1 = sum(wage_day1),
                                             self_day7 = sum(self_day7), self_day6 = sum(self_day6),
                                             self_day5 = sum(self_day5), self_day4 = sum(self_day4),
                                             self_day3 = sum(self_day3), self_day2 = sum(self_day2),
                                             self_day1 = sum(self_day1),
                                             nf_day7 = sum(nf_day7), nf_day6 = sum(nf_day6),
                                             nf_day5 = sum(nf_day5), nf_day4 = sum(nf_day4),
                                             nf_day3 = sum(nf_day3), nf_day2 = sum(nf_day2),
                                             nf_day1 = sum(nf_day1),
                                             f_day7 = sum(f_day7), f_day6 = sum(f_day6),
                                             f_day5 = sum(f_day5), f_day4 = sum(f_day4),
                                             f_day3 = sum(f_day3), f_day2 = sum(f_day2),
                                             f_day1 = sum(f_day1)) %>%
                                      filter(row_number()==1) %>%
                                      ungroup()

# Now need to gather
df_labor_daily <- df_labor_daily %>% gather(
                                             "days",
                                             "intensity",
                                             day7, day6, day5, day4, day3, day2, day1
                                             )

# Some cleaning
df_labor_daily$wage_days <- NA
df_labor_daily$wage_days[df_labor_daily$days=="day7"] <- df_labor_daily$wage_day7[df_labor_daily$days=="day7"]
df_labor_daily$wage_days[df_labor_daily$days=="day6"] <- df_labor_daily$wage_day6[df_labor_daily$days=="day6"]
df_labor_daily$wage_days[df_labor_daily$days=="day5"] <- df_labor_daily$wage_day5[df_labor_daily$days=="day5"]
df_labor_daily$wage_days[df_labor_daily$days=="day4"] <- df_labor_daily$wage_day4[df_labor_daily$days=="day4"]
df_labor_daily$wage_days[df_labor_daily$days=="day3"] <- df_labor_daily$wage_day3[df_labor_daily$days=="day3"]
df_labor_daily$wage_days[df_labor_daily$days=="day2"] <- df_labor_daily$wage_day2[df_labor_daily$days=="day2"]
df_labor_daily$wage_days[df_labor_daily$days=="day1"] <- df_labor_daily$wage_day1[df_labor_daily$days=="day1"]
df_labor_daily$self_days <- NA
df_labor_daily$self_days[df_labor_daily$days=="day7"] <- df_labor_daily$self_day7[df_labor_daily$days=="day7"]
df_labor_daily$self_days[df_labor_daily$days=="day6"] <- df_labor_daily$self_day6[df_labor_daily$days=="day6"]
df_labor_daily$self_days[df_labor_daily$days=="day5"] <- df_labor_daily$self_day5[df_labor_daily$days=="day5"]
df_labor_daily$self_days[df_labor_daily$days=="day4"] <- df_labor_daily$self_day4[df_labor_daily$days=="day4"]
df_labor_daily$self_days[df_labor_daily$days=="day3"] <- df_labor_daily$self_day3[df_labor_daily$days=="day3"]
df_labor_daily$self_days[df_labor_daily$days=="day2"] <- df_labor_daily$self_day2[df_labor_daily$days=="day2"]
df_labor_daily$self_days[df_labor_daily$days=="day1"] <- df_labor_daily$self_day1[df_labor_daily$days=="day1"]
df_labor_daily$f_days <- NA
df_labor_daily$f_days[df_labor_daily$days=="day7"] <- df_labor_daily$f_day7[df_labor_daily$days=="day7"]
df_labor_daily$f_days[df_labor_daily$days=="day6"] <- df_labor_daily$f_day6[df_labor_daily$days=="day6"]
df_labor_daily$f_days[df_labor_daily$days=="day5"] <- df_labor_daily$f_day5[df_labor_daily$days=="day5"]
df_labor_daily$f_days[df_labor_daily$days=="day4"] <- df_labor_daily$f_day4[df_labor_daily$days=="day4"]
df_labor_daily$f_days[df_labor_daily$days=="day3"] <- df_labor_daily$f_day3[df_labor_daily$days=="day3"]
df_labor_daily$f_days[df_labor_daily$days=="day2"] <- df_labor_daily$f_day2[df_labor_daily$days=="day2"]
df_labor_daily$f_days[df_labor_daily$days=="day1"] <- df_labor_daily$f_day1[df_labor_daily$days=="day1"]
df_labor_daily$nf_days <- NA
df_labor_daily$nf_days[df_labor_daily$days=="day7"] <- df_labor_daily$nf_day7[df_labor_daily$days=="day7"]
df_labor_daily$nf_days[df_labor_daily$days=="day6"] <- df_labor_daily$nf_day6[df_labor_daily$days=="day6"]
df_labor_daily$nf_days[df_labor_daily$days=="day5"] <- df_labor_daily$nf_day5[df_labor_daily$days=="day5"]
df_labor_daily$nf_days[df_labor_daily$days=="day4"] <- df_labor_daily$nf_day4[df_labor_daily$days=="day4"]
df_labor_daily$nf_days[df_labor_daily$days=="day3"] <- df_labor_daily$nf_day3[df_labor_daily$days=="day3"]
df_labor_daily$nf_days[df_labor_daily$days=="day2"] <- df_labor_daily$nf_day2[df_labor_daily$days=="day2"]
df_labor_daily$nf_days[df_labor_daily$days=="day1"] <- df_labor_daily$nf_day1[df_labor_daily$days=="day1"]
df_labor_daily <- df_labor_daily %>% select(-c(f_day7, f_day6, f_day5, f_day4, f_day3, f_day2, f_day1,
                                               nf_day7, nf_day6, nf_day5, nf_day4, nf_day3, nf_day2, nf_day1,
                                               wage_day7, wage_day6, wage_day5, wage_day4, wage_day3, wage_day2, wage_day1,
                                               self_day7, self_day6, self_day5, self_day4, self_day3, self_day2, self_day1))

# merging -------------------------------------------------------
# here are the concordance identifiers
concordance <- as_tibble(read_xls("data/nss/concordance_nochange.xls"))
# Do some renaming
concordance <- concordance %>% rename(state62 = state_code_source, district62 = district_code_source)

df_labor <- df_labor %>% 
              left_join(df_ind, by = c("hid", "pid")) %>% 
              left_join(df_hh, by = c("hid"))

df_labor <- df_labor %>% left_join(concordance, by = c("state62", "district62"))
df_labor$state_merge <- df_labor$state62
df_labor$district_merge <- df_labor$district62
#df_labor <- df_labor[complete.cases(df_labor),]
write.csv(df_labor, "data/clean/nss/nss62.csv")

# And daily
df_labor_daily <- df_labor_daily %>% 
                    left_join(df_ind, by = c("hid", "pid")) %>% 
                    left_join(df_hh, by = c("hid"))
# And replace day with date
df_labor_daily$day_date <- NA
df_labor_daily$day_date[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 1
df_labor_daily$day_date[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 2
df_labor_daily$day_date[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 3
df_labor_daily$day_date[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 4
df_labor_daily$day_date[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 5
df_labor_daily$day_date[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 6
df_labor_daily$day_date[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 7
df_labor_daily$day_date_l1 <- NA
df_labor_daily$day_date_l1[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 2
df_labor_daily$day_date_l1[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 3
df_labor_daily$day_date_l1[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 4
df_labor_daily$day_date_l1[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 5
df_labor_daily$day_date_l1[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 6
df_labor_daily$day_date_l1[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 7
df_labor_daily$day_date_l1[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 8
df_labor_daily$day_date_l2 <- NA
df_labor_daily$day_date_l2[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 3
df_labor_daily$day_date_l2[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 4
df_labor_daily$day_date_l2[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 5
df_labor_daily$day_date_l2[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 6
df_labor_daily$day_date_l2[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 7
df_labor_daily$day_date_l2[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 8
df_labor_daily$day_date_l2[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 9
df_labor_daily <- df_labor_daily %>% left_join(concordance, by = c("state62", "district62"))
df_labor_daily$state_merge <- df_labor_daily$state62
df_labor_daily$district_merge <- df_labor_daily$district62
df_labor_daily <- df_labor_daily[complete.cases(df_labor_daily),]
write.csv(df_labor_daily, "data/clean/nss/nss62_daily.csv")










# NSS 64 --------------------------------------------------------------------------------------------------------------
# surveytiming -------------------------------------------------------
df_hh <- read_dta("data/nss/nss64/Block-1-sample-household-identification-records.dta")
# Date needs to be of length six
df_hh <- df_hh[is.na(df_hh$B2_q2iv)==F,]
df_hh$B2_q2iv[str_length(df_hh$B2_q2iv)==5] <- paste0("0", df_hh$B2_q2iv[str_length(df_hh$B2_q2iv)==5])
df_hh <- df_hh %>% mutate(hid = key_Hhold,
                          date = as.Date(paste0("20", substr(B2_q2iv, 5, 6), "-", substr(B2_q2iv, 3, 4), "-", substr(B2_q2iv, 1, 2)), "%Y-%m-%d"),
                          state64 = state,
                          district64 = District,
                          weight_combined = wgt_combined,
                          weight = wgt,
                          weight_subsample = wgt_ss) %>%
                    select(hid, state64, district64, date, weight_combined, weight, weight_subsample)
df_hh <- df_hh[complete.cases(df_hh),]

# To join, with concordance files, need to do some changes
df_hh <- df_hh %>% mutate(state64 = as.numeric(state64), 
                          district64 = as.numeric(district64))

# demographics -------------------------------------------------------
df_ind <- read_dta("data/nss/nss64/Block-4-demographic-usual-activity-members-records.dta")
df_ind <- df_ind %>% mutate(hid = key_hhold,
                            pid = B4_c1,
                            female = as.numeric(B4_c4=="2"),
                            age = B4_c5,
                            educ = as.numeric(B4_c7),
                            relation = as.numeric(B4_c3)) %>%
                      select(hid, pid, female, age, educ, relation) %>%
                      group_by(hid) %>% arrange(hid, relation) %>%
                      mutate(head_female = female[1],
                             head_age = age[1],
                             head_educ = educ[1],
                             hhsize = max(row_number())) %>%
                      ungroup()

# labor -------------------------------------------------------
df_labor <- read_dta("data/nss/nss64/Block-5-members-time-disposition-records.dta")

# Some individual cleaning
# NAs for days should be missings
df_labor$B5_c14[is.na(df_labor$B5_c14)==T] <- 0
# Days in different types of labor
df_labor$days_self <- 0
df_labor$days_wage <- 0
df_labor$days_domestic <- 0
# Now labor types
df_labor$days_self[df_labor$B5_c4 %in% c("11", "12", "21")] <- df_labor$days_self[df_labor$B5_c4 %in% c("11", "12", "21")] + 
                                                                  df_labor$B5_c14[df_labor$B5_c4 %in% c("11", "12", "21")]
df_labor$days_wage[df_labor$B5_c4 %in% c("31", "41", "51")] <- df_labor$days_wage[df_labor$B5_c4 %in% c("31", "41", "51")] + 
                                                                  df_labor$B5_c14[df_labor$B5_c4 %in% c("31", "41", "51")]
df_labor$days_domestic[df_labor$B5_c4 %in% c("92", "93")] <- df_labor$days_domestic[df_labor$B5_c4 %in% c("92", "93")] + 
                                                                  df_labor$B5_c14[df_labor$B5_c4 %in% c("92", "93")]
# Now farm/non-farm
df_labor$days_f <- 0
df_labor$days_f[df_labor$B5_c5 %in% c("01", "02")] <- df_labor$days_f[df_labor$B5_c5 %in% c("01", "02")] + 
                                                          df_labor$B5_c14[df_labor$B5_c5 %in% c("01", "02")]
df_labor$days_nf <- (df_labor$days_self + df_labor$days_wage - df_labor$days_f)

# Wages
df_labor$f_wages <- NA
df_labor$f_wages[df_labor$days_f>0] <- df_labor$B5_c17[df_labor$days_f>0]
df_labor$nf_wages <- NA
df_labor$nf_wages[df_labor$days_nf>0] <- df_labor$B5_c17[df_labor$days_nf>0]

# DAILY - use below
df_labor_daily <- df_labor


df_labor <- df_labor %>% mutate(hid = key_hhold,
                                pid = B5_c1) %>%
                          select(hid, pid, days_self, days_wage, days_domestic, days_f, days_nf, f_wages, nf_wages) %>%
                          group_by(hid, pid) %>%
                          mutate(days_self = sum(days_self), 
                                 days_wage = sum(days_wage), 
                                 days_domestic = sum(days_domestic), 
                                 days_f = sum(days_f), 
                                 days_nf = sum(days_nf),
                                 f_wages = sum(f_wages), 
                                 nf_wages = sum(nf_wages)) %>%
                          filter(row_number()==1) %>%
                          ungroup()

# Daily again
df_labor_daily$B5_c7[is.na(df_labor_daily$B5_c7)==T | 
                                                  !(df_labor_daily$B5_c4 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$B5_c8[is.na(df_labor_daily$B5_c8)==T | 
                                                  !(df_labor_daily$B5_c4 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$B5_c9[is.na(df_labor_daily$B5_c9)==T | 
                                                  !(df_labor_daily$B5_c4 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$B5_c10[is.na(df_labor_daily$B5_c10)==T | 
                                                  !(df_labor_daily$B5_c4 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$B5_c11[is.na(df_labor_daily$B5_c11)==T | 
                                                  !(df_labor_daily$B5_c4 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$B5_c12[is.na(df_labor_daily$B5_c12)==T | 
                                                  !(df_labor_daily$B5_c4 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$B5_c13[is.na(df_labor_daily$B5_c13)==T | 
                                                  !(df_labor_daily$B5_c4 %in% c("11", "12", "21", "31", "41", "51"))] <- 0
# Wage
df_labor_daily$wage_day1 <- df_labor_daily$B5_c13
df_labor_daily$wage_day1[!(df_labor_daily$B5_c4 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day2 <- df_labor_daily$B5_c12
df_labor_daily$wage_day2[!(df_labor_daily$B5_c4 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day3 <- df_labor_daily$B5_c11
df_labor_daily$wage_day3[!(df_labor_daily$B5_c4 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day4 <- df_labor_daily$B5_c10
df_labor_daily$wage_day4[!(df_labor_daily$B5_c4 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day5 <- df_labor_daily$B5_c9
df_labor_daily$wage_day5[!(df_labor_daily$B5_c4 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day6 <- df_labor_daily$B5_c8
df_labor_daily$wage_day6[!(df_labor_daily$B5_c4 %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day7 <- df_labor_daily$B5_c7
df_labor_daily$wage_day7[!(df_labor_daily$B5_c4 %in% c("31", "41", "51"))] <- 0
# Self
df_labor_daily$self_day1 <- df_labor_daily$B5_c13
df_labor_daily$self_day1[!(df_labor_daily$B5_c4 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day2 <- df_labor_daily$B5_c12
df_labor_daily$self_day2[!(df_labor_daily$B5_c4 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day3 <- df_labor_daily$B5_c11
df_labor_daily$self_day3[!(df_labor_daily$B5_c4 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day4 <- df_labor_daily$B5_c10
df_labor_daily$self_day4[!(df_labor_daily$B5_c4 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day5 <- df_labor_daily$B5_c9
df_labor_daily$self_day5[!(df_labor_daily$B5_c4 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day6 <- df_labor_daily$B5_c8
df_labor_daily$self_day6[!(df_labor_daily$B5_c4 %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day7 <- df_labor_daily$B5_c7
df_labor_daily$self_day7[!(df_labor_daily$B5_c4 %in% c("11", "12", "21"))] <- 0
# non-farm
df_labor_daily$nf_day1 <- df_labor_daily$B5_c13
df_labor_daily$nf_day1[(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day2 <- df_labor_daily$B5_c12
df_labor_daily$nf_day2[(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day3 <- df_labor_daily$B5_c11
df_labor_daily$nf_day3[(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day4 <- df_labor_daily$B5_c10
df_labor_daily$nf_day4[(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day5 <- df_labor_daily$B5_c9
df_labor_daily$nf_day5[(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day6 <- df_labor_daily$B5_c8
df_labor_daily$nf_day6[(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$nf_day7 <- df_labor_daily$B5_c7
df_labor_daily$nf_day7[(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
# farm
df_labor_daily$f_day1 <- df_labor_daily$B5_c13
df_labor_daily$f_day1[!(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$f_day2 <- df_labor_daily$B5_c12
df_labor_daily$f_day2[!(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$f_day3 <- df_labor_daily$B5_c11
df_labor_daily$f_day3[!(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$f_day4 <- df_labor_daily$B5_c10
df_labor_daily$f_day4[!(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$f_day5 <- df_labor_daily$B5_c9
df_labor_daily$f_day5[!(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$f_day6 <- df_labor_daily$B5_c8
df_labor_daily$f_day6[!(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0
df_labor_daily$f_day7 <- df_labor_daily$B5_c7
df_labor_daily$f_day7[!(df_labor_daily$B5_c5 %in% c("01", "02"))] <- 0

df_labor_daily <- df_labor_daily %>% mutate(hid =  key_hhold,
                                            pid = B5_c1) %>%
                                      select(hid, pid, day7 = B5_c7, day6 = B5_c8,
                                             day5 = B5_c9, day4 = B5_c10,
                                             day3 = B5_c11, day2 = B5_c12,
                                             day1 = B5_c13,
                                             starts_with("f_day"), starts_with("nf_day"), starts_with("wage_day"), starts_with("self_day")) %>%
                                      group_by(hid, pid) %>%
                                      mutate(day7 = sum(day7), day6 = sum(day6), day5 = sum(day5),
                                             day4 = sum(day4), day3 = sum(day3), day2 = sum(day2),
                                             day1 = sum(day1),
                                             wage_day7 = sum(wage_day7), wage_day6 = sum(wage_day6),
                                             wage_day5 = sum(wage_day5), wage_day4 = sum(wage_day4),
                                             wage_day3 = sum(wage_day3), wage_day2 = sum(wage_day2),
                                             wage_day1 = sum(wage_day1),
                                             self_day7 = sum(self_day7), self_day6 = sum(self_day6),
                                             self_day5 = sum(self_day5), self_day4 = sum(self_day4),
                                             self_day3 = sum(self_day3), self_day2 = sum(self_day2),
                                             self_day1 = sum(self_day1),
                                             nf_day7 = sum(nf_day7), nf_day6 = sum(nf_day6),
                                             nf_day5 = sum(nf_day5), nf_day4 = sum(nf_day4),
                                             nf_day3 = sum(nf_day3), nf_day2 = sum(nf_day2),
                                             nf_day1 = sum(nf_day1),
                                             f_day7 = sum(f_day7), f_day6 = sum(f_day6),
                                             f_day5 = sum(f_day5), f_day4 = sum(f_day4),
                                             f_day3 = sum(f_day3), f_day2 = sum(f_day2),
                                             f_day1 = sum(f_day1)) %>%
                                      filter(row_number()==1) %>%
                                      ungroup()

# Now need to gather
df_labor_daily <- df_labor_daily %>% gather(
                                             "days",
                                             "intensity",
                                             day7, day6, day5, day4, day3, day2, day1
                                             )

# Some cleaning
df_labor_daily$wage_days <- NA
df_labor_daily$wage_days[df_labor_daily$days=="day7"] <- df_labor_daily$wage_day7[df_labor_daily$days=="day7"]
df_labor_daily$wage_days[df_labor_daily$days=="day6"] <- df_labor_daily$wage_day6[df_labor_daily$days=="day6"]
df_labor_daily$wage_days[df_labor_daily$days=="day5"] <- df_labor_daily$wage_day5[df_labor_daily$days=="day5"]
df_labor_daily$wage_days[df_labor_daily$days=="day4"] <- df_labor_daily$wage_day4[df_labor_daily$days=="day4"]
df_labor_daily$wage_days[df_labor_daily$days=="day3"] <- df_labor_daily$wage_day3[df_labor_daily$days=="day3"]
df_labor_daily$wage_days[df_labor_daily$days=="day2"] <- df_labor_daily$wage_day2[df_labor_daily$days=="day2"]
df_labor_daily$wage_days[df_labor_daily$days=="day1"] <- df_labor_daily$wage_day1[df_labor_daily$days=="day1"]
df_labor_daily$self_days <- NA
df_labor_daily$self_days[df_labor_daily$days=="day7"] <- df_labor_daily$self_day7[df_labor_daily$days=="day7"]
df_labor_daily$self_days[df_labor_daily$days=="day6"] <- df_labor_daily$self_day6[df_labor_daily$days=="day6"]
df_labor_daily$self_days[df_labor_daily$days=="day5"] <- df_labor_daily$self_day5[df_labor_daily$days=="day5"]
df_labor_daily$self_days[df_labor_daily$days=="day4"] <- df_labor_daily$self_day4[df_labor_daily$days=="day4"]
df_labor_daily$self_days[df_labor_daily$days=="day3"] <- df_labor_daily$self_day3[df_labor_daily$days=="day3"]
df_labor_daily$self_days[df_labor_daily$days=="day2"] <- df_labor_daily$self_day2[df_labor_daily$days=="day2"]
df_labor_daily$self_days[df_labor_daily$days=="day1"] <- df_labor_daily$self_day1[df_labor_daily$days=="day1"]
df_labor_daily$f_days <- NA
df_labor_daily$f_days[df_labor_daily$days=="day7"] <- df_labor_daily$f_day7[df_labor_daily$days=="day7"]
df_labor_daily$f_days[df_labor_daily$days=="day6"] <- df_labor_daily$f_day6[df_labor_daily$days=="day6"]
df_labor_daily$f_days[df_labor_daily$days=="day5"] <- df_labor_daily$f_day5[df_labor_daily$days=="day5"]
df_labor_daily$f_days[df_labor_daily$days=="day4"] <- df_labor_daily$f_day4[df_labor_daily$days=="day4"]
df_labor_daily$f_days[df_labor_daily$days=="day3"] <- df_labor_daily$f_day3[df_labor_daily$days=="day3"]
df_labor_daily$f_days[df_labor_daily$days=="day2"] <- df_labor_daily$f_day2[df_labor_daily$days=="day2"]
df_labor_daily$f_days[df_labor_daily$days=="day1"] <- df_labor_daily$f_day1[df_labor_daily$days=="day1"]
df_labor_daily$nf_days <- NA
df_labor_daily$nf_days[df_labor_daily$days=="day7"] <- df_labor_daily$nf_day7[df_labor_daily$days=="day7"]
df_labor_daily$nf_days[df_labor_daily$days=="day6"] <- df_labor_daily$nf_day6[df_labor_daily$days=="day6"]
df_labor_daily$nf_days[df_labor_daily$days=="day5"] <- df_labor_daily$nf_day5[df_labor_daily$days=="day5"]
df_labor_daily$nf_days[df_labor_daily$days=="day4"] <- df_labor_daily$nf_day4[df_labor_daily$days=="day4"]
df_labor_daily$nf_days[df_labor_daily$days=="day3"] <- df_labor_daily$nf_day3[df_labor_daily$days=="day3"]
df_labor_daily$nf_days[df_labor_daily$days=="day2"] <- df_labor_daily$nf_day2[df_labor_daily$days=="day2"]
df_labor_daily$nf_days[df_labor_daily$days=="day1"] <- df_labor_daily$nf_day1[df_labor_daily$days=="day1"]
df_labor_daily <- df_labor_daily %>% select(-c(f_day7, f_day6, f_day5, f_day4, f_day3, f_day2, f_day1,
                                               nf_day7, nf_day6, nf_day5, nf_day4, nf_day3, nf_day2, nf_day1,
                                               wage_day7, wage_day6, wage_day5, wage_day4, wage_day3, wage_day2, wage_day1,
                                               self_day7, self_day6, self_day5, self_day4, self_day3, self_day2, self_day1))

# merging -------------------------------------------------------
# here are the concordance identifiers
concordance <- as_tibble(read_xls("data/nss/concordance_nochange.xls"))
# Do some renaming
concordance <- concordance %>% rename(state64 = state_code_source, district64 = district_code_source)

df_labor <- df_labor %>% 
              left_join(df_ind, by = c("hid", "pid")) %>% 
              left_join(df_hh, by = c("hid"))

df_labor <- df_labor %>% left_join(concordance, by = c("state64", "district64"))
df_labor$state_merge <- df_labor$state64
df_labor$district_merge <- df_labor$district64
#df_labor <- df_labor[complete.cases(df_labor),]
write.csv(df_labor, "data/clean/nss/nss64.csv")

# And daily
df_labor_daily <- df_labor_daily %>% 
                    left_join(df_ind, by = c("hid", "pid")) %>% 
                    left_join(df_hh, by = c("hid"))
# And replace day with date
df_labor_daily$day_date <- NA
df_labor_daily$day_date[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 1
df_labor_daily$day_date[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 2
df_labor_daily$day_date[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 3
df_labor_daily$day_date[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 4
df_labor_daily$day_date[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 5
df_labor_daily$day_date[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 6
df_labor_daily$day_date[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 7
df_labor_daily$day_date_l1 <- NA
df_labor_daily$day_date_l1[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 2
df_labor_daily$day_date_l1[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 3
df_labor_daily$day_date_l1[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 4
df_labor_daily$day_date_l1[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 5
df_labor_daily$day_date_l1[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 6
df_labor_daily$day_date_l1[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 7
df_labor_daily$day_date_l1[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 8
df_labor_daily$day_date_l2 <- NA
df_labor_daily$day_date_l2[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 3
df_labor_daily$day_date_l2[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 4
df_labor_daily$day_date_l2[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 5
df_labor_daily$day_date_l2[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 6
df_labor_daily$day_date_l2[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 7
df_labor_daily$day_date_l2[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 8
df_labor_daily$day_date_l2[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 9
df_labor_daily <- df_labor_daily %>% left_join(concordance, by = c("state64", "district64"))
df_labor_daily$state_merge <- df_labor_daily$state64
df_labor_daily$district_merge <- df_labor_daily$district64
df_labor_daily <- df_labor_daily[complete.cases(df_labor_daily),]
write.csv(df_labor_daily, "data/clean/nss/nss64_daily.csv")







# NSS 66 --------------------------------------------------------------------------------------------------------------
# surveytiming -------------------------------------------------------
df_hh <- read_dta("data/nss/nss66/nss66_level01.dta")
# Date needs to be of length six
df_hh <- df_hh[is.na(df_hh$survey_date)==F,]
df_hh$survey_date[str_length(df_hh$survey_date)==5] <- paste0("0", df_hh$survey_date[str_length(df_hh$survey_date)==5])
df_hh <- df_hh %>% mutate(hid = paste(fsu, sector, state, district, stratum, sub_round, sub_sample, 
                                      sub_region, sub_block, second_stage_stratum, hhld, sep = "-"),
                          date = as.Date(paste0("20", substr(survey_date, 5, 6), "-", substr(survey_date, 3, 4), "-", substr(survey_date, 1, 2)), "%Y-%m-%d"),
                          state66 = state,
                          district66 = district,
                          weight = mlt,
                          weight_subsample = mlt_sr) %>%
                    select(hid, state66, district66, date, weight, weight_subsample)
df_hh <- df_hh[complete.cases(df_hh),]

# Some changes to state
df_hh$state66 <- paste0(df_hh$state66)
df_hh$state66[str_length(df_hh$state66)==2] <- substr(df_hh$state66[str_length(df_hh$state66)==2],
                                                      1, 1)
df_hh$state66[str_length(df_hh$state66)==3] <- substr(df_hh$state66[str_length(df_hh$state66)==3],
                                                      1, 2)
df_hh$state66 <- as.numeric(df_hh$state66)

# demographics -------------------------------------------------------
df_ind <- read_dta("data/nss/nss66/nss66_level04.dta")
df_ind <- df_ind %>% mutate(hid = paste(fsu, sector, state, district, stratum, sub_round, sub_sample, 
                                      sub_region, sub_block, second_stage_stratum, hhld, sep = "-"),
                            pid = person_id,
                            female = as.numeric(sex==2),
                            age = age,
                            educ = as.numeric(education_general),
                            relation = as.numeric(relation_to_head)) %>%
                      select(hid, pid, female, age, educ, relation) %>%
                      group_by(hid) %>% arrange(hid, relation) %>%
                      mutate(head_female = female[1],
                             head_age = age[1],
                             head_educ = educ[1],
                             hhsize = max(row_number())) %>%
                      ungroup()

# labor -------------------------------------------------------
df_labor <- read_dta("data/nss/nss66/nss66_level07.dta")

# Some individual cleaning
# NAs for days should be missings
df_labor$total_days[is.na(df_labor$total_days)==T] <- 0
# Days in different types of labor
df_labor$days_self <- 0
df_labor$days_wage <- 0
df_labor$days_domestic <- 0
# Now labor types
df_labor$days_self[df_labor$status %in% c("11", "12", "21")] <- df_labor$days_self[df_labor$status %in% c("11", "12", "21")] + 
                                                                  df_labor$total_days[df_labor$status %in% c("11", "12", "21")]/10
df_labor$days_wage[df_labor$status %in% c("31", "41", "51")] <- df_labor$days_wage[df_labor$status %in% c("31", "41", "51")] + 
                                                                  df_labor$total_days[df_labor$status %in% c("31", "41", "51")]/10
df_labor$days_domestic[df_labor$status %in% c("92", "93")] <- df_labor$days_domestic[df_labor$status %in% c("92", "93")] + 
                                                                 df_labor$total_days[df_labor$status %in% c("92", "93")]/10
# Now farm/non-farm
df_labor$days_f <- 0
df_labor$days_f[df_labor$nic_code %in% c("01", "02")] <- df_labor$days_f[df_labor$nic_code %in% c("01", "02")] + 
                                                          df_labor$total_days[df_labor$nic_code %in% c("01", "02")]/10
df_labor$days_nf <- (df_labor$days_self + df_labor$days_wage - df_labor$days_f)

# Wages
df_labor$f_wages <- NA
df_labor$f_wages[df_labor$days_f>0] <- df_labor$wages_earning_total[df_labor$days_f>0]
df_labor$nf_wages <- NA
df_labor$nf_wages[df_labor$days_nf>0] <- df_labor$wages_earning_total[df_labor$days_nf>0]

# DAILY - use below
df_labor_daily <- df_labor


df_labor <- df_labor %>% mutate(hid = paste(fsu, sector, state, district, stratum, sub_round, sub_sample, 
                                      sub_region, sub_block, second_stage_stratum, hhld, sep = "-"),
                                pid = person_id) %>%
                          select(hid, pid, days_self, days_wage, days_domestic, days_f, days_nf, f_wages, nf_wages) %>%
                          group_by(hid, pid) %>%
                          mutate(days_self = sum(days_self), 
                                 days_wage = sum(days_wage), 
                                 days_domestic = sum(days_domestic), 
                                 days_f = sum(days_f), 
                                 days_nf = sum(days_nf),
                                 f_wages = sum(f_wages), 
                                 nf_wages = sum(nf_wages)) %>%
                          filter(row_number()==1) %>%
                          ungroup()

# Daily again
df_labor_daily$intensity_day7[is.na(df_labor_daily$intensity_day7)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day6[is.na(df_labor_daily$intensity_day6)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day5[is.na(df_labor_daily$intensity_day5)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day4[is.na(df_labor_daily$intensity_day4)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day3[is.na(df_labor_daily$intensity_day3)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day2[is.na(df_labor_daily$intensity_day2)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day1[is.na(df_labor_daily$intensity_day1)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
# Wage
df_labor_daily$wage_day1 <- df_labor_daily$intensity_day1
df_labor_daily$wage_day1[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day2 <- df_labor_daily$intensity_day2
df_labor_daily$wage_day2[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day3 <- df_labor_daily$intensity_day3
df_labor_daily$wage_day3[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day4 <- df_labor_daily$intensity_day4
df_labor_daily$wage_day4[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day5 <- df_labor_daily$intensity_day5
df_labor_daily$wage_day5[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day6 <- df_labor_daily$intensity_day6
df_labor_daily$wage_day6[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day7 <- df_labor_daily$intensity_day7
df_labor_daily$wage_day7[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
# Self
df_labor_daily$self_day1 <- df_labor_daily$intensity_day1
df_labor_daily$self_day1[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day2 <- df_labor_daily$intensity_day2
df_labor_daily$self_day2[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day3 <- df_labor_daily$intensity_day3
df_labor_daily$self_day3[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day4 <- df_labor_daily$intensity_day4
df_labor_daily$self_day4[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day5 <- df_labor_daily$intensity_day5
df_labor_daily$self_day5[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day6 <- df_labor_daily$intensity_day6
df_labor_daily$self_day6[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day7 <- df_labor_daily$intensity_day7
df_labor_daily$self_day7[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
# non-farm
df_labor_daily$nf_day1 <- df_labor_daily$intensity_day1
df_labor_daily$nf_day1[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day2 <- df_labor_daily$intensity_day2
df_labor_daily$nf_day2[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day3 <- df_labor_daily$intensity_day3
df_labor_daily$nf_day3[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day4 <- df_labor_daily$intensity_day4
df_labor_daily$nf_day4[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day5 <- df_labor_daily$intensity_day5
df_labor_daily$nf_day5[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day6 <- df_labor_daily$intensity_day6
df_labor_daily$nf_day6[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day7 <- df_labor_daily$intensity_day7
df_labor_daily$nf_day7[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
# farm
df_labor_daily$f_day1 <- df_labor_daily$intensity_day1
df_labor_daily$f_day1[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day2 <- df_labor_daily$intensity_day2
df_labor_daily$f_day2[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day3 <- df_labor_daily$intensity_day3
df_labor_daily$f_day3[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day4 <- df_labor_daily$intensity_day4
df_labor_daily$f_day4[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day5 <- df_labor_daily$intensity_day5
df_labor_daily$f_day5[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day6 <- df_labor_daily$intensity_day6
df_labor_daily$f_day6[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day7 <- df_labor_daily$intensity_day7
df_labor_daily$f_day7[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0

df_labor_daily <- df_labor_daily %>% mutate(hid =  paste(fsu, sector, state, district, stratum, sub_round, sub_sample, 
                                                   sub_region, sub_block, second_stage_stratum, hhld, sep = "-"),
                                            pid = person_id) %>%
                                      select(hid, pid, day7 = intensity_day7, day6 = intensity_day6,
                                             day5 = intensity_day5, day4 = intensity_day4,
                                             day3 = intensity_day3, day2 = intensity_day2,
                                             day1 = intensity_day1,
                                             starts_with("f_day"), starts_with("nf_day"), starts_with("wage_day"), starts_with("self_day")) %>%
                                      group_by(hid, pid) %>%
                                      mutate(day7 = sum(day7/10), day6 = sum(day6/10), day5 = sum(day5/10),
                                             day4 = sum(day4/10), day3 = sum(day3/10), day2 = sum(day2/10),
                                             day1 = sum(day1/10),
                                             wage_day7 = sum(wage_day7/10), wage_day6 = sum(wage_day6/10),
                                             wage_day5 = sum(wage_day5/10), wage_day4 = sum(wage_day4/10),
                                             wage_day3 = sum(wage_day3/10), wage_day2 = sum(wage_day2/10),
                                             wage_day1 = sum(wage_day1/10),
                                             self_day7 = sum(self_day7/10), self_day6 = sum(self_day6/10),
                                             self_day5 = sum(self_day5/10), self_day4 = sum(self_day4/10),
                                             self_day3 = sum(self_day3/10), self_day2 = sum(self_day2/10),
                                             self_day1 = sum(self_day1/10),
                                             nf_day7 = sum(nf_day7/10), nf_day6 = sum(nf_day6/10),
                                             nf_day5 = sum(nf_day5/10), nf_day4 = sum(nf_day4/10),
                                             nf_day3 = sum(nf_day3/10), nf_day2 = sum(nf_day2/10),
                                             nf_day1 = sum(nf_day1/10),
                                             f_day7 = sum(f_day7/10), f_day6 = sum(f_day6/10),
                                             f_day5 = sum(f_day5/10), f_day4 = sum(f_day4/10),
                                             f_day3 = sum(f_day3/10), f_day2 = sum(f_day2/10),
                                             f_day1 = sum(f_day1/10)) %>%
                                      filter(row_number()==1) %>%
                                      ungroup()

# Now need to gather
df_labor_daily <- df_labor_daily %>% gather(
                                             "days",
                                             "intensity",
                                             day7, day6, day5, day4, day3, day2, day1
                                             )

# Some cleaning
df_labor_daily$wage_days <- NA
df_labor_daily$wage_days[df_labor_daily$days=="day7"] <- df_labor_daily$wage_day7[df_labor_daily$days=="day7"]
df_labor_daily$wage_days[df_labor_daily$days=="day6"] <- df_labor_daily$wage_day6[df_labor_daily$days=="day6"]
df_labor_daily$wage_days[df_labor_daily$days=="day5"] <- df_labor_daily$wage_day5[df_labor_daily$days=="day5"]
df_labor_daily$wage_days[df_labor_daily$days=="day4"] <- df_labor_daily$wage_day4[df_labor_daily$days=="day4"]
df_labor_daily$wage_days[df_labor_daily$days=="day3"] <- df_labor_daily$wage_day3[df_labor_daily$days=="day3"]
df_labor_daily$wage_days[df_labor_daily$days=="day2"] <- df_labor_daily$wage_day2[df_labor_daily$days=="day2"]
df_labor_daily$wage_days[df_labor_daily$days=="day1"] <- df_labor_daily$wage_day1[df_labor_daily$days=="day1"]
df_labor_daily$self_days <- NA
df_labor_daily$self_days[df_labor_daily$days=="day7"] <- df_labor_daily$self_day7[df_labor_daily$days=="day7"]
df_labor_daily$self_days[df_labor_daily$days=="day6"] <- df_labor_daily$self_day6[df_labor_daily$days=="day6"]
df_labor_daily$self_days[df_labor_daily$days=="day5"] <- df_labor_daily$self_day5[df_labor_daily$days=="day5"]
df_labor_daily$self_days[df_labor_daily$days=="day4"] <- df_labor_daily$self_day4[df_labor_daily$days=="day4"]
df_labor_daily$self_days[df_labor_daily$days=="day3"] <- df_labor_daily$self_day3[df_labor_daily$days=="day3"]
df_labor_daily$self_days[df_labor_daily$days=="day2"] <- df_labor_daily$self_day2[df_labor_daily$days=="day2"]
df_labor_daily$self_days[df_labor_daily$days=="day1"] <- df_labor_daily$self_day1[df_labor_daily$days=="day1"]
df_labor_daily$f_days <- NA
df_labor_daily$f_days[df_labor_daily$days=="day7"] <- df_labor_daily$f_day7[df_labor_daily$days=="day7"]
df_labor_daily$f_days[df_labor_daily$days=="day6"] <- df_labor_daily$f_day6[df_labor_daily$days=="day6"]
df_labor_daily$f_days[df_labor_daily$days=="day5"] <- df_labor_daily$f_day5[df_labor_daily$days=="day5"]
df_labor_daily$f_days[df_labor_daily$days=="day4"] <- df_labor_daily$f_day4[df_labor_daily$days=="day4"]
df_labor_daily$f_days[df_labor_daily$days=="day3"] <- df_labor_daily$f_day3[df_labor_daily$days=="day3"]
df_labor_daily$f_days[df_labor_daily$days=="day2"] <- df_labor_daily$f_day2[df_labor_daily$days=="day2"]
df_labor_daily$f_days[df_labor_daily$days=="day1"] <- df_labor_daily$f_day1[df_labor_daily$days=="day1"]
df_labor_daily$nf_days <- NA
df_labor_daily$nf_days[df_labor_daily$days=="day7"] <- df_labor_daily$nf_day7[df_labor_daily$days=="day7"]
df_labor_daily$nf_days[df_labor_daily$days=="day6"] <- df_labor_daily$nf_day6[df_labor_daily$days=="day6"]
df_labor_daily$nf_days[df_labor_daily$days=="day5"] <- df_labor_daily$nf_day5[df_labor_daily$days=="day5"]
df_labor_daily$nf_days[df_labor_daily$days=="day4"] <- df_labor_daily$nf_day4[df_labor_daily$days=="day4"]
df_labor_daily$nf_days[df_labor_daily$days=="day3"] <- df_labor_daily$nf_day3[df_labor_daily$days=="day3"]
df_labor_daily$nf_days[df_labor_daily$days=="day2"] <- df_labor_daily$nf_day2[df_labor_daily$days=="day2"]
df_labor_daily$nf_days[df_labor_daily$days=="day1"] <- df_labor_daily$nf_day1[df_labor_daily$days=="day1"]
df_labor_daily <- df_labor_daily %>% select(-c(f_day7, f_day6, f_day5, f_day4, f_day3, f_day2, f_day1,
                                               nf_day7, nf_day6, nf_day5, nf_day4, nf_day3, nf_day2, nf_day1,
                                               wage_day7, wage_day6, wage_day5, wage_day4, wage_day3, wage_day2, wage_day1,
                                               self_day7, self_day6, self_day5, self_day4, self_day3, self_day2, self_day1))

# merging -------------------------------------------------------
# here are the concordance identifiers
concordance <- as_tibble(read_xls("data/nss/concordance_nochange.xls"))
# Do some renaming
concordance <- concordance %>% rename(state66 = state_code_source, district66 = district_code_source)

df_labor <- df_labor %>% 
              left_join(df_ind, by = c("hid", "pid")) %>% 
              left_join(df_hh, by = c("hid"))

df_labor <- df_labor %>% left_join(concordance, by = c("state66", "district66"))
df_labor$state_merge <- df_labor$state66
df_labor$district_merge <- df_labor$district66
#df_labor <- df_labor[complete.cases(df_labor),]
write.csv(df_labor, "data/clean/nss/nss66.csv")

# And daily
df_labor_daily <- df_labor_daily %>% 
                    left_join(df_ind, by = c("hid", "pid")) %>% 
                    left_join(df_hh, by = c("hid"))
# And replace day with date
df_labor_daily$day_date <- NA
df_labor_daily$day_date[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 1
df_labor_daily$day_date[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 2
df_labor_daily$day_date[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 3
df_labor_daily$day_date[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 4
df_labor_daily$day_date[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 5
df_labor_daily$day_date[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 6
df_labor_daily$day_date[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 7
df_labor_daily$day_date_l1 <- NA
df_labor_daily$day_date_l1[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 2
df_labor_daily$day_date_l1[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 3
df_labor_daily$day_date_l1[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 4
df_labor_daily$day_date_l1[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 5
df_labor_daily$day_date_l1[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 6
df_labor_daily$day_date_l1[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 7
df_labor_daily$day_date_l1[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 8
df_labor_daily$day_date_l2 <- NA
df_labor_daily$day_date_l2[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 3
df_labor_daily$day_date_l2[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 4
df_labor_daily$day_date_l2[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 5
df_labor_daily$day_date_l2[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 6
df_labor_daily$day_date_l2[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 7
df_labor_daily$day_date_l2[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 8
df_labor_daily$day_date_l2[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 9
df_labor_daily <- df_labor_daily %>% left_join(concordance, by = c("state66", "district66"))
df_labor_daily$state_merge <- df_labor_daily$state66
df_labor_daily$district_merge <- df_labor_daily$district66
df_labor_daily <- df_labor_daily[complete.cases(df_labor_daily),]
write.csv(df_labor_daily, "data/clean/nss/nss66_daily.csv")







# NSS 68 --------------------------------------------------------------------------------------------------------------
# surveytiming -------------------------------------------------------
df_hh <- read_dta("data/nss/nss68/nss68_level01.dta")
df_hh <- df_hh %>% mutate(state_new = as.numeric(substr(state, 1, 2)))
# Date needs to be of length six
df_hh <- df_hh[is.na(df_hh$survey_date)==F,]
df_hh$survey_date[str_length(df_hh$survey_date)==5] <- paste0("0", df_hh$survey_date[str_length(df_hh$survey_date)==5])
df_hh <- df_hh %>% mutate(hid = paste(fsu, sector, state, district, stratum, sub_round, sub_sample, 
                                      sub_region, sub_block, second_stage_stratum, hhld, sep = "-"),
                          date = as.Date(paste0("20", substr(survey_date, 5, 6), "-", substr(survey_date, 3, 4), "-", substr(survey_date, 1, 2)), "%Y-%m-%d"),
                          state68 = state_new,
                          district68 = district,
                          weight = mlt,
                          weight_subsample = mlt_sr) %>%
                    select(hid, state68, district68, date, weight, weight_subsample)
df_hh <- df_hh[complete.cases(df_hh),]


# demographics -------------------------------------------------------
df_ind <- read_dta("data/nss/nss68/nss68_level03.dta")
df_ind <- df_ind %>% mutate(hid = paste(fsu, sector, state, district, stratum, sub_round, sub_sample, 
                                      sub_region, sub_block, second_stage_stratum, hhld, sep = "-"),
                            pid = person_id,
                            female = as.numeric(sex==2),
                            age = age,
                            educ = as.numeric(education_general),
                            relation = as.numeric(relation_to_head)) %>%
                      select(hid, pid, female, age, educ, relation) %>%
                      group_by(hid) %>% arrange(hid, relation) %>%
                      mutate(head_female = female[1],
                             head_age = age[1],
                             head_educ = educ[1],
                             hhsize = max(row_number())) %>%
                      ungroup()

# labor -------------------------------------------------------
df_labor <- read_dta("data/nss/nss68/nss68_level06.dta")

# Some individual cleaning
# NAs for days should be missings
df_labor$total_days[is.na(df_labor$total_days)==T] <- 0
# Days in different types of labor
df_labor$days_self <- 0
df_labor$days_wage <- 0
df_labor$days_domestic <- 0
# Now labor types
df_labor$days_self[df_labor$status %in% c("11", "12", "21")] <- df_labor$days_self[df_labor$status %in% c("11", "12", "21")] + 
                                                                  df_labor$total_days[df_labor$status %in% c("11", "12", "21")]/10
df_labor$days_wage[df_labor$status %in% c("31", "41", "51")] <- df_labor$days_wage[df_labor$status %in% c("31", "41", "51")] + 
                                                                  df_labor$total_days[df_labor$status %in% c("31", "41", "51")]/10
df_labor$days_domestic[df_labor$status %in% c("92", "93")] <- df_labor$days_domestic[df_labor$status %in% c("92", "93")] + 
                                                                 df_labor$total_days[df_labor$status %in% c("92", "93")]/10
# Now farm/non-farm
df_labor$days_f <- 0
df_labor$days_f[df_labor$nic_code %in% c("01", "02")] <- df_labor$days_f[df_labor$nic_code %in% c("01", "02")] + 
                                                          df_labor$total_days[df_labor$nic_code %in% c("01", "02")]/10
df_labor$days_nf <- (df_labor$days_self + df_labor$days_wage - df_labor$days_f)

# Wages
df_labor$f_wages <- NA
df_labor$f_wages[df_labor$days_f>0] <- df_labor$wages_earning_total[df_labor$days_f>0]
df_labor$nf_wages <- NA
df_labor$nf_wages[df_labor$days_nf>0] <- df_labor$wages_earning_total[df_labor$days_nf>0]

# DAILY - use below
df_labor_daily <- df_labor


df_labor <- df_labor %>% mutate(hid = paste(fsu, sector, state, district, stratum, sub_round, sub_sample, 
                                      sub_region, sub_block, second_stage_stratum, hhld, sep = "-"),
                                pid = person_id) %>%
                          select(hid, pid, days_self, days_wage, days_domestic, days_f, days_nf, f_wages, nf_wages) %>%
                          group_by(hid, pid) %>%
                          mutate(days_self = sum(days_self), 
                                 days_wage = sum(days_wage), 
                                 days_domestic = sum(days_domestic), 
                                 days_f = sum(days_f), 
                                 days_nf = sum(days_nf),
                                 f_wages = sum(f_wages), 
                                 nf_wages = sum(nf_wages)) %>%
                          filter(row_number()==1) %>%
                          ungroup()

# Daily again
df_labor_daily$intensity_day7[is.na(df_labor_daily$intensity_day7)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day6[is.na(df_labor_daily$intensity_day6)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day5[is.na(df_labor_daily$intensity_day5)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day4[is.na(df_labor_daily$intensity_day4)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day3[is.na(df_labor_daily$intensity_day3)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day2[is.na(df_labor_daily$intensity_day2)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
df_labor_daily$intensity_day1[is.na(df_labor_daily$intensity_day1)==T | 
                                                  !(df_labor_daily$status %in% c("11", "12", "21", "31", "41", "51"))] <- 0
# Wage
df_labor_daily$wage_day1 <- df_labor_daily$intensity_day1
df_labor_daily$wage_day1[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day2 <- df_labor_daily$intensity_day2
df_labor_daily$wage_day2[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day3 <- df_labor_daily$intensity_day3
df_labor_daily$wage_day3[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day4 <- df_labor_daily$intensity_day4
df_labor_daily$wage_day4[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day5 <- df_labor_daily$intensity_day5
df_labor_daily$wage_day5[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day6 <- df_labor_daily$intensity_day6
df_labor_daily$wage_day6[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
df_labor_daily$wage_day7 <- df_labor_daily$intensity_day7
df_labor_daily$wage_day7[!(df_labor_daily$status %in% c("31", "41", "51"))] <- 0
# Self
df_labor_daily$self_day1 <- df_labor_daily$intensity_day1
df_labor_daily$self_day1[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day2 <- df_labor_daily$intensity_day2
df_labor_daily$self_day2[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day3 <- df_labor_daily$intensity_day3
df_labor_daily$self_day3[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day4 <- df_labor_daily$intensity_day4
df_labor_daily$self_day4[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day5 <- df_labor_daily$intensity_day5
df_labor_daily$self_day5[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day6 <- df_labor_daily$intensity_day6
df_labor_daily$self_day6[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
df_labor_daily$self_day7 <- df_labor_daily$intensity_day7
df_labor_daily$self_day7[!(df_labor_daily$status %in% c("11", "12", "21"))] <- 0
# non-farm
df_labor_daily$nf_day1 <- df_labor_daily$intensity_day1
df_labor_daily$nf_day1[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day2 <- df_labor_daily$intensity_day2
df_labor_daily$nf_day2[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day3 <- df_labor_daily$intensity_day3
df_labor_daily$nf_day3[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day4 <- df_labor_daily$intensity_day4
df_labor_daily$nf_day4[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day5 <- df_labor_daily$intensity_day5
df_labor_daily$nf_day5[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day6 <- df_labor_daily$intensity_day6
df_labor_daily$nf_day6[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$nf_day7 <- df_labor_daily$intensity_day7
df_labor_daily$nf_day7[(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
# farm
df_labor_daily$f_day1 <- df_labor_daily$intensity_day1
df_labor_daily$f_day1[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day2 <- df_labor_daily$intensity_day2
df_labor_daily$f_day2[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day3 <- df_labor_daily$intensity_day3
df_labor_daily$f_day3[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day4 <- df_labor_daily$intensity_day4
df_labor_daily$f_day4[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day5 <- df_labor_daily$intensity_day5
df_labor_daily$f_day5[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day6 <- df_labor_daily$intensity_day6
df_labor_daily$f_day6[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0
df_labor_daily$f_day7 <- df_labor_daily$intensity_day7
df_labor_daily$f_day7[!(df_labor_daily$nic_code %in% c("01", "02"))] <- 0

df_labor_daily <- df_labor_daily %>% mutate(hid = paste(fsu, sector, state, district, stratum, sub_round, sub_sample, 
                                                  sub_region, sub_block, second_stage_stratum, hhld, sep = "-"),
                                            pid = person_id) %>%
                                      select(hid, pid, day7 = intensity_day7, day6 = intensity_day6,
                                             day5 = intensity_day5, day4 = intensity_day4,
                                             day3 = intensity_day3, day2 = intensity_day2,
                                             day1 = intensity_day1,
                                             starts_with("f_day"), starts_with("nf_day"), starts_with("wage_day"), starts_with("self_day")) %>%
                                      group_by(hid, pid) %>%
                                      mutate(day7 = sum(day7/10), day6 = sum(day6/10), day5 = sum(day5/10),
                                             day4 = sum(day4/10), day3 = sum(day3/10), day2 = sum(day2/10),
                                             day1 = sum(day1/10),
                                             wage_day7 = sum(wage_day7/10), wage_day6 = sum(wage_day6/10),
                                             wage_day5 = sum(wage_day5/10), wage_day4 = sum(wage_day4/10),
                                             wage_day3 = sum(wage_day3/10), wage_day2 = sum(wage_day2/10),
                                             wage_day1 = sum(wage_day1/10),
                                             self_day7 = sum(self_day7/10), self_day6 = sum(self_day6/10),
                                             self_day5 = sum(self_day5/10), self_day4 = sum(self_day4/10),
                                             self_day3 = sum(self_day3/10), self_day2 = sum(self_day2/10),
                                             self_day1 = sum(self_day1/10),
                                             nf_day7 = sum(nf_day7/10), nf_day6 = sum(nf_day6/10),
                                             nf_day5 = sum(nf_day5/10), nf_day4 = sum(nf_day4/10),
                                             nf_day3 = sum(nf_day3/10), nf_day2 = sum(nf_day2/10),
                                             nf_day1 = sum(nf_day1/10),
                                             f_day7 = sum(f_day7/10), f_day6 = sum(f_day6/10),
                                             f_day5 = sum(f_day5/10), f_day4 = sum(f_day4/10),
                                             f_day3 = sum(f_day3/10), f_day2 = sum(f_day2/10),
                                             f_day1 = sum(f_day1/10)) %>%
                                      filter(row_number()==1) %>%
                                      ungroup()

# Now need to gather
df_labor_daily <- df_labor_daily %>% gather(
                                             "days",
                                             "intensity",
                                             day7, day6, day5, day4, day3, day2, day1
                                             )

# Some cleaning
df_labor_daily$wage_days <- NA
df_labor_daily$wage_days[df_labor_daily$days=="day7"] <- df_labor_daily$wage_day7[df_labor_daily$days=="day7"]
df_labor_daily$wage_days[df_labor_daily$days=="day6"] <- df_labor_daily$wage_day6[df_labor_daily$days=="day6"]
df_labor_daily$wage_days[df_labor_daily$days=="day5"] <- df_labor_daily$wage_day5[df_labor_daily$days=="day5"]
df_labor_daily$wage_days[df_labor_daily$days=="day4"] <- df_labor_daily$wage_day4[df_labor_daily$days=="day4"]
df_labor_daily$wage_days[df_labor_daily$days=="day3"] <- df_labor_daily$wage_day3[df_labor_daily$days=="day3"]
df_labor_daily$wage_days[df_labor_daily$days=="day2"] <- df_labor_daily$wage_day2[df_labor_daily$days=="day2"]
df_labor_daily$wage_days[df_labor_daily$days=="day1"] <- df_labor_daily$wage_day1[df_labor_daily$days=="day1"]
df_labor_daily$self_days <- NA
df_labor_daily$self_days[df_labor_daily$days=="day7"] <- df_labor_daily$self_day7[df_labor_daily$days=="day7"]
df_labor_daily$self_days[df_labor_daily$days=="day6"] <- df_labor_daily$self_day6[df_labor_daily$days=="day6"]
df_labor_daily$self_days[df_labor_daily$days=="day5"] <- df_labor_daily$self_day5[df_labor_daily$days=="day5"]
df_labor_daily$self_days[df_labor_daily$days=="day4"] <- df_labor_daily$self_day4[df_labor_daily$days=="day4"]
df_labor_daily$self_days[df_labor_daily$days=="day3"] <- df_labor_daily$self_day3[df_labor_daily$days=="day3"]
df_labor_daily$self_days[df_labor_daily$days=="day2"] <- df_labor_daily$self_day2[df_labor_daily$days=="day2"]
df_labor_daily$self_days[df_labor_daily$days=="day1"] <- df_labor_daily$self_day1[df_labor_daily$days=="day1"]
df_labor_daily$f_days <- NA
df_labor_daily$f_days[df_labor_daily$days=="day7"] <- df_labor_daily$f_day7[df_labor_daily$days=="day7"]
df_labor_daily$f_days[df_labor_daily$days=="day6"] <- df_labor_daily$f_day6[df_labor_daily$days=="day6"]
df_labor_daily$f_days[df_labor_daily$days=="day5"] <- df_labor_daily$f_day5[df_labor_daily$days=="day5"]
df_labor_daily$f_days[df_labor_daily$days=="day4"] <- df_labor_daily$f_day4[df_labor_daily$days=="day4"]
df_labor_daily$f_days[df_labor_daily$days=="day3"] <- df_labor_daily$f_day3[df_labor_daily$days=="day3"]
df_labor_daily$f_days[df_labor_daily$days=="day2"] <- df_labor_daily$f_day2[df_labor_daily$days=="day2"]
df_labor_daily$f_days[df_labor_daily$days=="day1"] <- df_labor_daily$f_day1[df_labor_daily$days=="day1"]
df_labor_daily$nf_days <- NA
df_labor_daily$nf_days[df_labor_daily$days=="day7"] <- df_labor_daily$nf_day7[df_labor_daily$days=="day7"]
df_labor_daily$nf_days[df_labor_daily$days=="day6"] <- df_labor_daily$nf_day6[df_labor_daily$days=="day6"]
df_labor_daily$nf_days[df_labor_daily$days=="day5"] <- df_labor_daily$nf_day5[df_labor_daily$days=="day5"]
df_labor_daily$nf_days[df_labor_daily$days=="day4"] <- df_labor_daily$nf_day4[df_labor_daily$days=="day4"]
df_labor_daily$nf_days[df_labor_daily$days=="day3"] <- df_labor_daily$nf_day3[df_labor_daily$days=="day3"]
df_labor_daily$nf_days[df_labor_daily$days=="day2"] <- df_labor_daily$nf_day2[df_labor_daily$days=="day2"]
df_labor_daily$nf_days[df_labor_daily$days=="day1"] <- df_labor_daily$nf_day1[df_labor_daily$days=="day1"]
df_labor_daily <- df_labor_daily %>% select(-c(f_day7, f_day6, f_day5, f_day4, f_day3, f_day2, f_day1,
                                               nf_day7, nf_day6, nf_day5, nf_day4, nf_day3, nf_day2, nf_day1,
                                               wage_day7, wage_day6, wage_day5, wage_day4, wage_day3, wage_day2, wage_day1,
                                               self_day7, self_day6, self_day5, self_day4, self_day3, self_day2, self_day1))


# merging -------------------------------------------------------
# here are the concordance identifiers
concordance <- as_tibble(read_xls("data/nss/concordance_nochange.xls"))
# Do some renaming
concordance <- concordance %>% rename(state68 = state_code_source, district68 = district_code_source)

df_labor <- df_labor %>% 
              left_join(df_ind, by = c("hid", "pid")) %>% 
              left_join(df_hh, by = c("hid"))

df_labor <- df_labor %>% left_join(concordance, by = c("state68", "district68"))
df_labor$state_merge <- df_labor$state68
df_labor$district_merge <- df_labor$district68
#df_labor <- df_labor[complete.cases(df_labor),]
write.csv(df_labor, "data/clean/nss/nss68.csv")

# And daily
df_labor_daily <- df_labor_daily %>% 
                    left_join(df_ind, by = c("hid", "pid")) %>% 
                    left_join(df_hh, by = c("hid"))
# And replace day with date
df_labor_daily$day_date <- NA
df_labor_daily$day_date[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 1
df_labor_daily$day_date[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 2
df_labor_daily$day_date[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 3
df_labor_daily$day_date[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 4
df_labor_daily$day_date[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 5
df_labor_daily$day_date[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 6
df_labor_daily$day_date[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 7
df_labor_daily$day_date_l1 <- NA
df_labor_daily$day_date_l1[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 2
df_labor_daily$day_date_l1[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 3
df_labor_daily$day_date_l1[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 4
df_labor_daily$day_date_l1[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 5
df_labor_daily$day_date_l1[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 6
df_labor_daily$day_date_l1[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 7
df_labor_daily$day_date_l1[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 8
df_labor_daily$day_date_l2 <- NA
df_labor_daily$day_date_l2[df_labor_daily$days=="day1"] <- df_labor_daily$date[df_labor_daily$days=="day1"] - 3
df_labor_daily$day_date_l2[df_labor_daily$days=="day2"] <- df_labor_daily$date[df_labor_daily$days=="day2"] - 4
df_labor_daily$day_date_l2[df_labor_daily$days=="day3"] <- df_labor_daily$date[df_labor_daily$days=="day3"] - 5
df_labor_daily$day_date_l2[df_labor_daily$days=="day4"] <- df_labor_daily$date[df_labor_daily$days=="day4"] - 6
df_labor_daily$day_date_l2[df_labor_daily$days=="day5"] <- df_labor_daily$date[df_labor_daily$days=="day5"] - 7
df_labor_daily$day_date_l2[df_labor_daily$days=="day6"] <- df_labor_daily$date[df_labor_daily$days=="day6"] - 8
df_labor_daily$day_date_l2[df_labor_daily$days=="day7"] <- df_labor_daily$date[df_labor_daily$days=="day7"] - 9
df_labor_daily <- df_labor_daily %>% left_join(concordance, by = c("state68", "district68"))
df_labor_daily$state_merge <- df_labor_daily$state68
df_labor_daily$district_merge <- df_labor_daily$district68
df_labor_daily <- df_labor_daily[complete.cases(df_labor_daily),]
write.csv(df_labor_daily, "data/clean/nss/nss68_daily.csv")






