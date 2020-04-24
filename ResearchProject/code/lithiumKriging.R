library(ggplot2)
library(sf)
library(gstat)
library(data.table)

##################
# lithium import #
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
lithium_data$ActivityStartDate.Date = as.Date(lithium_data$ActivityStartDate, format = "%m/%d/%Y")

# cut off sample to <1000 mcg
lithium_data_500 = subset(lithium_data, lithium_data$lithium.level.mcg.l < 500)

# histogram of lithium levels
ggplot(data = lithium_data_500, aes(x = lithium.level.mcg.l)) +
  geom_histogram(binwidth=10) + 
  geom_vline(xintercept = 50, color = 'red')



# load measurement station data
raw_station_data = as.data.table(read.csv("./data/lithium_water_data/station.csv"))

# add location columns to lithium data


####################
# shapefile import #
####################

# load US counties shapefile




