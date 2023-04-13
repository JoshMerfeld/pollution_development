# Air pollution and agricultural productivity in a developing country
This project uses the following datasets:
  - National Sample Survey (NSS)
  - Village-level shapefiles and census data from [The SHRUG](https://www.devdatalab.org/shrug)
  - The data on coal plants is from [Global Energy Monitor](globalenergymonitor.org/projects/global-coal-plant-tracker/)
  - I use agricultural yield estimates from 2001 through 2013 from [Gangopadhyay et al. (2022)](https://www.nature.com/articles/s41597-022-01828-y)
  - The pollution data is from [Gangopadhyay et al. (2022)](https://www.nature.com/articles/s41597-022-01828-y)
  - Daily wind data (with four estimates per day) is from [the National Center for Atmospheric Research](climatedataguide.ucar.edu/)
  - Monthly pollution (particulate) estimates are from [Hammer et al. (2020)][https://pubs.acs.org/doi/full/10.1021/acs.est.0c01764]

## Cleaning scripts
- I clean the National Sample Survey (NSS) data in [this script](cleaning/3.cleaning_nss.R).
- I clean the coal plants data in [this script](cleaning/4.cleaning_plants.R). This script does the following:
  - Creates the location of coal plants (lat/lon)
  - Creates a matrix of distances between coal plants and all the villages in the SHRUG data
  - Calculates the angle from each coal plant to each village
  - Downloads daily wind data from 1990-01-01 through 2015 that has information on the previous WEEK
    - For each of these data points, calculates whether wind is blowing in the direction of any given village (four wind values per day, so it is a proportion of the day wind blows in that direction)
  - Aggregates these daily values to the month level
- [This script](cleaning/5.cleaning_precip_temp.R) downloads the temperature and precipitation data.
- I extract the pollution data to villages in [this script](cleaning/6.cleaning_pm25.R).
- I extract the agricultural productivity data to villages in [this script](cleaning/7.cleaning_ag_productivity_village.R).
- I match the village-level data and aggregate up to NSS districts in [this script](cleaning/8.cleaning_overlap_merging_nss.R).

## Analysis scripts
- I estimate regressions looking at correlations between some variables and coal plant openings in [this script](analysis/1.openings.R).
- I validate that wind direction predicts pollution levels in [this script](analysis/2.pollution_wind_validation.R).
- All regressions related to agricultural yield are in [this script](analysis/3.yield.R).
- All regressions related to NSS data are in [this script](analysis/4.nss.R).

## Slides
- You can find my most recent html slides [here](presentation/slides/slides.html).


