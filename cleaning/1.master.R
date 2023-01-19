# Purpose: This script runs all of the other scripts in the proper order
# Author: Josh Merfeld
# Date: January 19th, 2023

rm(list = ls())

# Sets wd to folder this script is in so we can create relative paths instead of absolute paths
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# just make sure WD is the same as current:
getwd()    # check





##################
# This cleans the nightlight data, taken from SHRUG
source("2.cleaning_ntl.R")


##################
# This cleans the NSS data
source("3.cleaning_nss.R")


##################
# This calculates the directions from plants to villages and downloads/cleans wind data.
# This takes a LONG TIME TO RUN. Like a week.
source("4.cleaning_plants.R")


##################
# This cleans the precip/temp data (using rasters)
source("5.cleaning_precip_temp.R")


##################
# This cleans the particulate matter data, and also adds wind to it.
source("6.cleaning_pm25.R")


##################
# This cleans the agricultural productivity data and adds rain/temp to it so it is ready for analysis at season level
source("7.cleaning_ag_productivity_village.R")


##################
# This adds the wind data to the NSS data, which requires some aggregation
source("8.cleaning_overlap_merging_nss.R")


##################
# This gets data ready for analyzing characteristics of villages correlated with plant openings. 1991 and 2001 census data from SHRUG.
source("9.cleaning_coalplantopenings.R")









