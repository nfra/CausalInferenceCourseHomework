library(tidyverse)


# Remove strange units
lithiumSamples_Units = subset(lithiumSamples, !(lithiumSamples$ResultMeasure.MeasureUnitCode == "ppb"))
lithiumSamples_Units = subset(lithiumSamples_Units, !(lithiumSamples_Units$ResultMeasure.MeasureUnitCode == "mg/kg"))
lithiumSamples_Units = subset(lithiumSamples_Units, !(lithiumSamples_Units$ResultMeasure.MeasureUnitCode == "count"))

# Correct mg to ug
lithiumSamples_Units$ResultMeasureValue.ug = lithiumSamples_Units$ResultMeasureValue
lithiumSamples_Units <- within(lithiumSamples_Units, ResultMeasureValue.ug[ResultMeasure.MeasureUnitCode == "mg/l"] 
                         <- ResultMeasureValue[ResultMeasure.MeasureUnitCode == "mg/l"]*1000)

# Correct date type
lithiumSamples_Units$ActivityStartDate.Date = as.Date(lithiumSamples_Units$ActivityStartDate, format = "%m/%d/%Y")

# Cut off sample to <1000 ug
lithiumSamples_1000 = subset(lithiumSamples_Units, lithiumSamples_Units$ResultMeasureValue.ug < 1000)

# Histogram of lithium levels
ggplot(data = lithiumSamples_1000, aes(x = ResultMeasureValue.ug)) +
  geom_histogram(binwidth=10)



# Summarize sub-1000ug samples
lithSummary = lithiumSamples_1000 %>%
  group_by(Org = lithiumSamples_1000[,1], MonitoringLocationIdentifier) %>%
  summarize(
    MinLevel = min(ResultMeasureValue.ug),
    MaxLevel = max(ResultMeasureValue.ug),
    MeanLevel = mean(ResultMeasureValue.ug),
    ChangeInLevel = max(ResultMeasureValue.ug)-min(ResultMeasureValue.ug),
    NumberOfObs = n(),
    LengthOfTime = max(ActivityStartDate.Date)-min(ActivityStartDate.Date)
    )

# Further reduce
lithSummary_Minus = subset(lithSummary, lithSummary$NumberOfObs > 50)
lithSummary_Minus = subset(lithSummary_Minus, lithSummary_Minus$LengthOfTime > 600)
lithSummary_Minus = subset(lithSummary_Minus, lithSummary_Minus$LengthOfTime/lithSummary_Minus$NumberOfObs < 100)

# Investigate USGS-11303500 - San Joaquin
lithium_investigation = subset(lithiumSamples_1000, MonitoringLocationIdentifier == "USGS-11303500") 

ggplot(data = lithium_investigation, aes(x = ResultMeasureValue.ug)) +
  geom_histogram(binwidth=1)


# Investigate USGS-08178700 - San Antonio
lithium_investigation = subset(lithiumSamples_1000, MonitoringLocationIdentifier == "USGS-08178700") 

ggplot(data = lithium_investigation, aes(x = ResultMeasureValue.ug)) +
  geom_histogram(binwidth=10)

ggplot(data = lithium_investigation) +
  geom_point(aes(x = ActivityStartDate.Date, y = ResultMeasureValue.ug, group = ))


# Investigate USGS-08364000 - El Paso
lithium_investigation = subset(lithiumSamples_1000, MonitoringLocationIdentifier == "USGS-08364000") 

ggplot(data = lithium_investigation, aes(x = ResultMeasureValue.ug)) +
  geom_histogram(binwidth=10)

ggplot(data = lithium_investigation) +
  geom_point(aes(x = ActivityStartDate.Date, y = ResultMeasureValue.ug, group = ))


# Investigate USGS-08470400 - Harlingen
lithium_investigation = subset(lithiumSamples_1000, MonitoringLocationIdentifier == "USGS-08470400") 

ggplot(data = lithium_investigation, aes(x = ResultMeasureValue.ug)) +
  geom_histogram(binwidth=10)

ggplot(data = lithium_investigation) +
  geom_point(aes(x = ActivityStartDate.Date, y = ResultMeasureValue.ug, group = ))


# Investigate USGS-11261100
lithium_investigation = subset(lithiumSamples_1000, MonitoringLocationIdentifier == "USGS-08470400") 

ggplot(data = lithium_investigation, aes(x = ResultMeasureValue.ug)) +
  geom_histogram(binwidth=10)

ggplot(data = lithium_investigation) +
  geom_point(aes(x = ActivityStartDate.Date, y = ResultMeasureValue.ug, group = ))
