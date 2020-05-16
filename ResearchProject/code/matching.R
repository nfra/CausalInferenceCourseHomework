library(tidyverse)
library(sf)
library(data.table)
library(Matching)

#####################
# import shapefiles #
#####################

# load PUMA shapefiles
pumas = st_read("./data/ipums_usa_data/geographic/ipums_puma_2010/ipums_puma_2010.shp")

# remove non-state PUMA shapes
pumas = subset(pumas, as.numeric(as.character(STATEFIP)) <= 56)

#######################
# import lithium data #
#######################

# read lithium data
interpolated_lithium_values = as.data.frame(read.csv("./data/lithium_water_data/interp_values.csv", 
                                                              stringsAsFactors = FALSE))
# turn geometry back into coordinates
coords_flat = unlist(strsplit(substr(interpolated_lithium_values$geometry, 3, nchar(interpolated_lithium_values$geometry) - 1), ', '))
interpolated_lithium_values$longitude = as.numeric(coords_flat[1:length(coords_flat)%%2 == 1])
interpolated_lithium_values$latitude = as.numeric(coords_flat[1:length(coords_flat)%%2 == 0])

# make sf object for "contains" calculation
interpolated_lithium_values_sf = st_as_sf(interpolated_lithium_values, coords = 3:4, crs = st_crs(pumas))

#################################
# calculate PUMA lithium levels #
#################################

# run contains calculation for average lithium level calculation below
grid_in_puma_list = st_contains(pumas, interpolated_lithium_values_sf)

# calculate average lithium level in each PUMA
for(puma_number_local in seq(nrow(pumas)))
{
  grid_points_in_puma = interpolated_lithium_values_sf[unlist(grid_in_puma_list[puma_number_local]),]
  pumas[puma_number_local, 'lithium_value'] = mean(grid_points_in_puma$lithium_value, na.rm = T)
}

# make treatment variable per PUMA, lithium value > 70 mcg/l
pumas$treatment_value = (pumas$lithium_value > 70)

# remove unnecessary objects from the environment to free up space in memory
rm(grid_in_puma_list, grid_points_in_puma, interpolated_lithium_values, interpolated_lithium_values_sf, coords_flat, puma_number_local)

# a few plots to include in write-up
ggplot(data = pumas) +
  geom_histogram(aes(x = lithium_value), binwidth = 5)

ggplot(data = pumas) +
  geom_sf(aes(fill = lithium_value, color = lithium_value))

ggplot(data = pumas) +
  geom_sf(aes(fill = treatment_value, color = treatment_value))

##############
# import ACS #
##############

acs = as.data.table(read.csv("./data/ipums_usa_data/usa_00005.csv"))

Match(Y = )


summary(acs$WKSWORK2)

summary(acs$MOVEDIN)