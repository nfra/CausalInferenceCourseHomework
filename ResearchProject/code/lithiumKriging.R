library(tidyverse)
library(sf)
library(gstat)
library(data.table)
library(maps)

##################
# import lithium #
##################

# load lithium data

raw_lithium_data = as.data.table(read.csv("./data/lithium_water_data/result.csv"))

summary(lithium_data)

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

# cut off sample to <500 mcg for histogram
lithium_data_500 = subset(lithium_data, lithium_data$lithium.level.mcg.l < 500)

# histogram of lithium levels
ggplot(data = lithium_data_500, aes(x = lithium.level.mcg.l)) +
  geom_histogram(binwidth=10) + 
  geom_vline(xintercept = 50, color = 'red')


# load measurement station data
raw_station_data = as.data.table(read.csv("./data/lithium_water_data/station.csv"))

# add monitoring location columns to lithium data
measurement_location_vector = match(lithium_data$MonitoringLocationIdentifier, raw_station_data$MonitoringLocationIdentifier)
lithium_data[['MonitoringLocation.Latitude']] = raw_station_data[measurement_location_vector, LatitudeMeasure]
lithium_data[['MonitoringLocation.Longitude']] = raw_station_data[measurement_location_vector, LongitudeMeasure]
lithium_data[['MonitoringLocation.StateCode']] = raw_station_data[measurement_location_vector, StateCode]
lithium_data[['MonitoringLocation.CountyCode']] = raw_station_data[measurement_location_vector, CountyCode]
lithium_data = subset(lithium_data, MonitoringLocation.Latitude != 0)

# TODO: compare measurement to detection quantitation limit in resdetectqntlmt.csv

# make sf object with mean measurements for each site
mean_lithium_at_sites = subset(lithium_data, lithium.level.mcg.l <= 1000) %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarize(mean.lithium.level.mcg.l = mean(lithium.level.mcg.l))
monitoring_location_vector = match(mean_lithium_at_sites$MonitoringLocationIdentifier, raw_station_data$MonitoringLocationIdentifier)

measurement_sites = mean_lithium_at_sites
measurement_sites[['MonitoringLocation.Latitude']] = raw_station_data[monitoring_location_vector, LatitudeMeasure]
measurement_sites[['MonitoringLocation.Longitude']] = raw_station_data[monitoring_location_vector, LongitudeMeasure]
measurement_sites[['MonitoringLocation.StateCode']] = raw_station_data[monitoring_location_vector, StateCode]
measurement_sites[['MonitoringLocation.CountyCode']] = raw_station_data[monitoring_location_vector, CountyCode]


measurement_sites_sf = st_as_sf(measurement_sites, coords = 4:3, crs = 4269, dim = 'XY')
measurement_sites_sf = subset(measurement_sites_sf, MonitoringLocation.StateCode <= 56)

ggplot(data = subset(measurement_sites_sf, MonitoringLocation.StateCode == 12)) +
  geom_sf(aes(color = mean.lithium.level.mcg.l))

# try to make sf object
measurement_location_sites = st_as_sf(as.data.frame(lithium_data[,64:68]), coords = 3:2, crs = 4269, dim = 'XY')
measurement_location_sites = subset(measurement_location_sites, MonitoringLocation.StateCode <= 56)

####################
# import shapefile #
####################

# load US county shapefiles
counties = st_read("./data/tl_2017_us_county/tl_2017_us_county.shp")

# remove non-state shapefiles
counties = subset(counties, as.numeric(as.character(STATEFP)) <= 56)

ggplot() +
  geom_sf(data = counties)

st_contains(counties, monitoring_location_sites)[[1]]

lithium_data[MonitoringLocation.Latitude == 0 & MonitoringLocation.Longitude == 0,]

ggplot(data = subset(measurement_location_sites, lithium.level.mcg.l <= 100)) +
  geom_sf(aes(color = lithium.level.mcg.l))


##################
# make variogram #
##################

test_variogram = variogram(mean.lithium.level.mcg.l ~ 1, data = measurement_sites_sf, cutoff = 20)
test_vario_fit = fit.variogram(test_variogram, vgm(1, "Exp"))
plot(test_variogram)






