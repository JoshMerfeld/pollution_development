# create counterfactual of yield

library(tidyverse)
library(sf)
library(terra)
library(exactextractr)



# district shapefile
dist <- st_read("data/counterfactual/Census_2011/2011_Dist.shp")
# rename
dist <- dist %>%
            dplyr::select(dist_merge = censuscode, geometry)

# now we want pollution levels for each district in 2001 and 2011
months <- c()
for (month in 1:9){
  months <- c(months, paste0("0", month))
}
months <- c(months, "10", "11", "12")
rastlist2001 <- c()
for (month in months){
# MONSOON --------------------------------------------------------------------
# This changes by year
    r <- rast(paste0("data/raw/pm25/V5GL03.HybridPM25.Asia.2001", month, "-2001", month, ".nc"))
    # extract
    r <- exact_extract(r, dist, "mean", append_cols = "dist_merge")

    rastlist2001 <- rbind(rastlist2001, r)
}
rastlist2011 <- c()
for (month in months){
# MONSOON --------------------------------------------------------------------
# This changes by year
    r <- rast(paste0("data/raw/pm25/V5GL03.HybridPM25.Asia.2011", month, "-2011", month, ".nc"))
    # extract
    r <- exact_extract(r, dist, "mean", append_cols = "dist_merge")

    rastlist2011 <- rbind(rastlist2011, r)
}
# take means at dist_merge by each year
rastlist2001 <- rastlist2001 %>%
                    group_by(dist_merge) %>%
                    summarise(pm25 = mean(mean, na.rm = TRUE))
rastlist2001$year <- 2001
rastlist2011 <- rastlist2011 %>%
                    group_by(dist_merge) %>%
                    summarise(pm25 = mean(mean, na.rm = TRUE))
rastlist2011$year <- 2011
# rbind
rastlist <- rbind(rastlist2001, rastlist2011)













# load yield data (http://data.icrisat.org/dld/index.html)
yield <- read_csv("data/counterfactual/yield.csv")
colnames(yield) <- c("dist", "year", "state", "statename", "distname", "riceproduction", "wheatproduction", "sorghumproduction",
                        "pearlmilletproduction", "maizeproduction", "fingermilletproduction")
# add peral and finger together
yield$milletproduction <- yield$pearlmilletproduction + yield$fingermilletproduction
yield <- yield %>%
            dplyr::select(dist, state, year, riceproduction, wheatproduction, maizeproduction)

# if production is zero, make NA
yield$riceproduction[yield$riceproduction == 0] <- NA
yield$wheatproduction[yield$wheatproduction == 0] <- NA
yield$maizeproduction[yield$maizeproduction == 0] <- NA

# load identifiers
id <- read_csv("data/counterfactual/id.csv")
# some renaming
id <- id %>%
        dplyr::select(state = `State Code`, state_merge = `Indian Census State Code 2011`, 
                        dist = `District Code`, dist_merge = `Indian Census District Code 2011`)
# keep first observations
id <- id %>%
        group_by(state, dist) %>%
        slice(1) %>%
        ungroup()

# merge into yield data
yield <- yield %>%
            left_join(id, by = c("state", "dist"))






# merge pm into yield
# only keep districts in yield data
rastlist <- rastlist %>% filter(dist_merge %in% yield$dist_merge)

yield <- yield %>%
            left_join(rastlist, by = c("dist_merge", "year"))

# save rastlist and yield
write_csv(rastlist, "data/counterfactual/rastlistclean.csv")
write_sf(dist, "data/counterfactual/distclean.shp")


# for each district, find change from 2001 to 2011 for both pollution and outputs
yield <- yield %>%
                group_by(dist_merge) %>%
                arrange(dist_merge, year) %>%
                mutate(pm25_change = log(pm25[n()]) - log(pm25[1]),
                        rice_change = log(riceproduction[n()]) - log(riceproduction[1]),
                        wheat_change = log(wheatproduction[n()]) - log(wheatproduction[1]),
                        maize_change = log(maizeproduction[n()]) - log(maizeproduction[1]),
                        rice_final = log(riceproduction[n()]),
                        wheat_final = log(wheatproduction[n()]),
                        maize_final = log(maizeproduction[n()])) %>% 
                filter(year==2001) %>%
                filter(!is.na(pm25_change)) %>%
                ungroup()


# most conservation coefficient is -0.223
# change is log(final) - log(initial)
# due to pm, counterfactual - 0.223*pmchange is the actual change in crop
yield$ricecounterfactual <- yield$rice_change + 0.223 * yield$pm25_change
yield$wheatcounterfactual <- yield$wheat_change + 0.223 * yield$pm25_change
yield$maizecounterfactual <- yield$maize_change + 0.223 * yield$pm25_change

# now the total that would have been
yield$ricecounterfactualtons <- yield$ricecounterfactual + log(yield$riceproduction)
yield$wheatcounterfactualtons <- yield$wheatcounterfactual + log(yield$wheatproduction)
yield$maizecounterfactualtons <- yield$maizecounterfactual + log(yield$maizeproduction)

# winsorize the top 2.5 percent
yield$ricecounterfactualtons[yield$ricecounterfactualtons > quantile(yield$ricecounterfactualtons, 0.95, na.rm = T)] <- quantile(yield$ricecounterfactualtons, 0.95, na.rm = T)
yield$wheatcounterfactualtons[yield$wheatcounterfactualtons > quantile(yield$wheatcounterfactualtons, 0.95, na.rm = T)] <- quantile(yield$wheatcounterfactualtons, 0.95, na.rm = T)
yield$maizecounterfactualtons[yield$maizecounterfactualtons > quantile(yield$maizecounterfactualtons, 0.95, na.rm = T)] <- quantile(yield$maizecounterfactualtons, 0.95, na.rm = T)

# actual final as well
yield$ricefinaltons <- exp(yield$rice_final)
yield$wheatfinaltons <- exp(yield$wheat_final)
yield$maizefinaltons <- exp(yield$maize_final)

write_csv(yield, "data/counterfactual/yieldclean.csv")

# sum up actual and counterfactual
sums <- yield %>%
            summarise(riceproduction = sum(riceproduction, na.rm = T),
                        wheatproduction = sum(wheatproduction, na.rm = T),
                        maizeproduction = sum(maizeproduction, na.rm = T),
                        ricefinaltons = sum(ricefinaltons, na.rm = T),
                        wheatfinaltons = sum(wheatfinaltons, na.rm = T),
                        maizefinaltons = sum(maizefinaltons, na.rm = T),
                        ricecounterfactualtons = sum(exp(ricecounterfactualtons), na.rm = T),
                        wheatcounterfactualtons = sum(exp(wheatcounterfactualtons), na.rm = T),
                        maizecounterfactualtons = sum(exp(maizecounterfactualtons), na.rm = T)) %>%
            ungroup()

sums$riceeffect <- sums$ricecounterfactualtons/sums$ricefinaltons
sums$wheateffect <- sums$wheatcounterfactualtons/sums$wheatfinaltons
sums$maizeeffect <- sums$maizecounterfactualtons/sums$maizefinaltons
summary(sums)
# save
write_csv(sums, "data/counterfactual/sums.csv")



ggplot(yield) + 
        geom_point(aes(x = pm25_change, y = log(ricefinaltons), color = "Rice"), alpha = 0.6) +
        geom_smooth(aes(x = pm25_change, y = log(ricefinaltons), color = "Rice"), alpha = 0.3, se = FALSE) +
        geom_point(aes(x = pm25_change, y = log(wheatfinaltons), color = "Wheat"), alpha = 0.6) +
        geom_smooth(aes(x = pm25_change, y = log(wheatfinaltons), color = "Wheat"), alpha = 0.3, se = FALSE) +
        geom_point(aes(x = pm25_change, y = log(maizefinaltons), color = "Maize"), alpha = 0.6) +
        geom_smooth(aes(x = pm25_change, y = log(maizefinaltons), color = "Maize"), alpha = 0.3, se = FALSE) +
        labs(x = "Change in PM2.5", y = "Change in output (log)") +
        scale_color_manual(values = c("Rice" = "red", "Wheat" = "blue", "Maize" = "green")) +
        theme_minimal()

