library(tidyverse)
library(sf)
library(gstat)
library(data.table)

library(sp)
library(rgdal)
library(dismo)

##################
# import lithium #
##################

# load lithium data

raw_lithium_data = as.data.table(read.csv("./data/lithium_water_data/result.csv"))


# clean lithium data

# remove non-numeric data in measurement column
lithium_data = subset(raw_lithium_data, (startsWith(as.character(raw_lithium_data$ResultMeasureValue), "0") | 
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "1") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "2") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "3") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "4") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "5") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "6") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "7") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "8") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), "9") |
                                           startsWith(as.character(raw_lithium_data$ResultMeasureValue), ".")
                                         )
                      )

# remove non-concentration units
# TODO: convert instead
lithium_data = subset(lithium_data, !(lithium_data$ResultMeasure.MeasureUnitCode == "ppb" |
                                        lithium_data$ResultMeasure.MeasureUnitCode == "mg/kg" |
                                        lithium_data$ResultMeasure.MeasureUnitCode == "count"
                                      )
                      )

# change type of measurement column from factor to numeric
lithium_data$ResultMeasureValue = as.numeric(as.character(lithium_data$ResultMeasureValue))

# standardize mg to ug
lithium_data$lithium.level.mcg.l = lithium_data$ResultMeasureValue
lithium_data[ResultMeasure.MeasureUnitCode == 'mg/l', 'lithium.level.mcg.l'] = lithium_data[ResultMeasure.MeasureUnitCode == 'mg/l', 1000 * lithium.level.mcg.l]

# correct date type
lithium_data$ActivityStartDate = as.Date(as.character(lithium_data$ActivityStartDate), format = "%Y-%m-%d")

# histogram of lithium levels, cut off at 500 mcg/l
ggplot(data = subset(lithium_data, lithium_data$lithium.level.mcg.l < 500), aes(x = lithium.level.mcg.l)) +
  geom_histogram(binwidth=10) + 
  geom_vline(xintercept = 50, color = 'red')


# load measurement station data
raw_station_data = as.data.table(read.csv("./data/lithium_water_data/station.csv"))

# add measurement location columns to lithium data
measurement_location_vector = match(lithium_data$MonitoringLocationIdentifier, raw_station_data$MonitoringLocationIdentifier)
lithium_data[['MeasurementLocation.Latitude']] = raw_station_data[measurement_location_vector, LatitudeMeasure]
lithium_data[['MeasurementLocation.Longitude']] = raw_station_data[measurement_location_vector, LongitudeMeasure]
lithium_data[['MeasurementLocation.StateCode']] = raw_station_data[measurement_location_vector, StateCode]
lithium_data[['MeasurementLocation.CountyCode']] = raw_station_data[measurement_location_vector, CountyCode]
lithium_data = subset(lithium_data, MeasurementLocation.Latitude != 0)

measurements_sf = st_as_sf(as.data.frame(lithium_data[,64:68]), coords = 3:2, crs = 4269, dim = 'XY')
measurements_sf = subset(measurements_sf, MeasurementLocation.StateCode <= 56)

# TODO: compare measurement to detection quantitation limit in resdetectqntlmt.csv

# calculate mean measurements for each site
lithium_averaged_by_sites = subset(lithium_data, lithium.level.mcg.l <= 1000) %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarize(mean.lithium.level.mcg.l = mean(lithium.level.mcg.l))

# add monitoring site location columns
monitoring_site_location_vector = match(lithium_averaged_by_sites$MonitoringLocationIdentifier, raw_station_data$MonitoringLocationIdentifier)

monitoring_sites = lithium_averaged_by_sites
monitoring_sites[['MonitoringSiteLocation.Latitude']] = raw_station_data[monitoring_site_location_vector, LatitudeMeasure]
monitoring_sites[['MonitoringSiteLocation.Longitude']] = raw_station_data[monitoring_site_location_vector, LongitudeMeasure]
monitoring_sites[['MonitoringSiteLocation.StateCode']] = raw_station_data[monitoring_site_location_vector, StateCode]
monitoring_sites[['MonitoringSiteLocation.CountyCode']] = raw_station_data[monitoring_site_location_vector, CountyCode]

# make sf object for average lithium level at each monitoring site
monitoring_sites_sf = st_as_sf(monitoring_sites, coords = 4:3, crs = 4269, dim = 'XY')
monitoring_sites_sf = subset(monitoring_sites_sf, MonitoringSiteLocation.StateCode <= 56)

# plot color-scaled monitoring sites from Florida
ggplot(data = subset(monitoring_sites_sf, MonitoringSiteLocation.StateCode == 17)) +
  geom_sf(aes(color = mean.lithium.level.mcg.l))


#####################
# import shapefiles #
#####################

# load US county shapefiles
counties = st_read("./data/tl_2017_us_county/tl_2017_us_county.shp")

# remove non-state county shapes
counties = subset(counties, as.numeric(as.character(STATEFP)) <= 56)

# load PUMA shapefiles
pumas = st_read("./data/ipums_usa_data/geographic/ipums_puma_2010/ipums_puma_2010.shp")

# remove non-state PUMA shapes
pumas = subset(pumas, as.numeric(as.character(STATEFIP)) <= 56)

# create lists of PUMA and county contents
monitoring_sites_puma_list = st_contains(st_transform(pumas, crs = 4269), monitoring_sites_sf)
monitoring_sites_county_list = st_contains(counties, monitoring_sites_sf)

# create grid of points based on PUMAs and convert them to proper sf type
puma_grid = st_make_grid(pumas, n = c(1500, 1500), what = 'centers')
puma_grid_df = as.data.frame(puma_grid)
puma_grid_df$latitude = subset(unlist(puma_grid_df$geometry), seq(1, nrow(puma_grid_df)*2)%%2 == 0)
puma_grid_df$longitude = subset(unlist(puma_grid_df$geometry), seq(1, nrow(puma_grid_df)*2)%%2 == 1)

puma_grid_in_puma = st_contains(pumas, puma_grid_sf)

puma_grid_sf = st_as_sf(puma_grid_df, coords = c(3,2), crs = st_crs(pumas))

# to check number of pumas containing a grid entry 
# sum(apply(st_contains(pumas, puma_grid_sf, sparse = FALSE), MARGIN = 1, FUN = any))

# loop through PUMAs and save which PUMA the grid points are in
for (puma_number_local in seq(nrow(pumas)))
{
  puma_contents_local = unlist(puma_grid_in_puma[puma_number_local])
  puma_grid_sf[puma_contents_local, 'PUMA'] = pumas$PUMA[puma_number_local]
}

# identify k nearest neighbors for all grid points and perform IDW on them, with inverse distance squared
d = 100000

monitoring_sites_trans = st_transform(monitoring_sites_sf, crs = st_crs(pumas))
monitoring_sites_trans$row_number = seq(nrow(monitoring_sites_trans))


within_list = st_is_within_distance(puma_grid_sf, monitoring_sites_trans, d)




for (grid_point_number in seq(nrow(puma_grid_sf)))
{
  grid_point = puma_grid_sf[grid_point_number,]
  closest_vector = numeric(0)
  for (i in seq(k))
  {
    sites_not_in_closest_vector = setdiff(seq(nrow(monitoring_sites_trans)), closest_vector)
    next_closest_site = which.min(st_distance(grid_point, 
                                              monitoring_sites_trans[sites_not_in_closest_vector, ]
                                              )
                                  )
    next_closest_site_corrected = as.data.frame(monitoring_sites_trans[sites_not_in_closest_vector, ])[next_closest_site, 'row_number']
    closest_vector = append(closest_vector, next_closest_site_corrected)
  }
  
  closest_site_distances = st_distance(grid_point, monitoring_sites_trans[closest_vector, ])
  closest_site_lithium_values = as.data.frame(monitoring_sites_trans)[closest_vector, 'mean.lithium.level.mcg.l']
  puma_grid_sf[grid_point_number,'lithium_idw_interpolation'] = sum(closest_site_lithium_values/closest_site_distances^2)
}

# plot monitoring site locations in PUMAs and in counties (projected like pumas)
ggplot() +
  geom_sf(data = pumas) +
  geom_sf(data = monitoring_sites_trans)

ggplot() +
  geom_sf(data = st_transform(counties, crs = st_crs(pumas))) +
  geom_sf(data = monitoring_sites_trans)

ggplot() +
  geom_sf(data = monitoring_sites_trans[1:1000,], alpha = 0.1, size = 0.1)




 
# loop through PUMAs and save which PUMA the monitoring sites are in
for (puma_number_local in seq(nrow(pumas)))
{
  puma_contents_local = unlist(monitoring_sites_puma_list[puma_number_local])
  monitoring_sites_trans[puma_contents_local, 'PUMA'] = pumas$PUMA[puma_number_local]
}

# loop through counties and save which county the monitoring sites are in
for (county_number_local in seq(nrow(counties)))
{
  county_contents_local = unlist(monitoring_sites_county_list[county_number_local])
  monitoring_sites_sf[county_contents_local, 'state'] = as.character(counties$STATEFP[county_number_local])
  monitoring_sites_sf[county_contents_local, 'county'] = as.character(counties$COUNTYFP[county_number_local])
}

# generate naive estimates for PUMAs by averaging measurements
puma_crude_data = monitoring_sites_trans %>%
  group_by(as.character(PUMA)) %>%
  summarize(mean.lithium.level.mcg.l = mean(mean.lithium.level.mcg.l))

# generate naive estimates for PUMAs by averaging measurements
county_crude_data = monitoring_sites_sf %>%
  group_by(MonitoringSiteLocation.StateCode, MonitoringSiteLocation.CountyCode) %>%
  summarize(mean.lithium.level.mcg.l = mean(mean.lithium.level.mcg.l))

##################
# import suicide #
##################

suicide = as.data.table(read.csv("./data/county_suicide_data/usa_00005.csv"))




##############
# import ACS #
##############

acs = as.data.table(read.csv("./data/ipums_usa_data/usa_00005.csv"))


summary(acs$WKSWORK2)

summary(acs$MOVEDIN)



