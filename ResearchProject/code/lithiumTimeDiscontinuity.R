# identifying in-site discontinuities in lithium level over time
library(tidyverse)

# Import data
levels <- read.csv("result.csv")

# Take subset of data with numerical level
levels_clean <- subset(levels, as.character(ResultMeasureValue) != '')
levels_clean <- subset(levels_clean, as.character(ResultMeasureValue) != 'ND')
levels_clean <- subset(levels_clean, (substr(as.character(ResultMeasureValue), 1, 1) != "<"))

# Correct ResultMeasureValue to numeric type
levels_clean$ResultMeasureValue.Character <- as.character(levels_clean$ResultMeasureValue)
levels_clean$ResultMeasureValue.Numeric <- as.numeric(levels_clean$ResultMeasureValue.Character)

# Correct date type
levels_clean$ActivityStartDate.Date <- as.Date(levels_clean$ActivityStartDate, format = "%Y-%m-%d")
levels_clean$ActivityEndDate.Date <- as.Date(levels_clean$ActivityEndDate, format = "%Y-%m-%d")

# Correct mg to ug (approximately, in case of mg/kg)
levels_clean$ResultMeasureValue.ug <- levels_clean$ResultMeasureValue.Numeric
levels_clean <- within(levels_clean, ResultMeasureValue.ug[ResultMeasure.MeasureUnitCode == "mg/l"] 
                               <- ResultMeasureValue.Numeric[ResultMeasure.MeasureUnitCode == "mg/l"]*1000)
levels_clean <- within(levels_clean, ResultMeasureValue.ug[ResultMeasure.MeasureUnitCode == "mg/kg"] 
                       <- ResultMeasureValue.Numeric[ResultMeasure.MeasureUnitCode == "mg/kg"]*1000)

# Remove levels greater than 1000
levels_clean_1000 <- subset(levels_clean, ResultMeasureValue.ug <= 1000)

# Find average before (including) and after (excluding) sample
levels_clean_1000 = arrange(levels_clean_1000, ActivityStartDate.Date)

levels_clean_1000$PrecedingLevelMean <- NA
levels_clean_1000$FollowingLevelMean <- NA
levels_clean_1000$LevelDiscontinuity <- NA
levels_clean_1000$MonitoringLocation.ActivityOrder <- NA
levels_clean_1000$MonitoringLocation.NumberObs<- NA

# CODE BELOW RUNS TOY MODEL
#
# test1 <- data.frame(type = c(1,2,1,2,1,2,1,2), level = c(0,3,0,3,1,5,1,5))
#
# test1$mean_below = c(0,0,0,0,0,0,0,0)
# test1[test1$type == 1,]$mean_below <- cummean(test1[test1$type == 1,]$level)
# test1[test1$type == 2,]$mean_below <- cummean(test1[test1$type == 2,]$level)
#
# test1$mean_above = c(0,0,0,0,0,0,0,0)
# test1[test1$type == 1,]$mean_above <- rev(cummean(rev(test1[test1$type == 1,]$level)))
# test1[test1$type == 2,]$mean_above <- rev(cummean(rev(test1[test1$type == 2,]$level)))
#
# test1$discontinuity = c(0,0,0,0,0,0,0,0)
# test1[test1$type == 1,]$discontinuity <- lead(test1[test1$type == 1,]$mean_above, 1) - test1[test1$type == 1,]$mean_below
# test1[test1$type == 2,]$discontinuity <- lead(test1[test1$type == 2,]$mean_above, 1) - test1[test1$type == 2,]$mean_below

counter = 0
for (id in unique(levels_clean_1000$MonitoringLocationIdentifier)){
  levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$PrecedingLevelMean <- cummean(levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$ResultMeasureValue.ug)
  levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$FollowingLevelMean <- rev(cummean(rev(levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$ResultMeasureValue.ug)))
  levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$LevelDiscontinuity <- lead(levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$FollowingLevelMean, 1) - levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$PrecedingLevelMean
  levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$MonitoringLocation.ActivityOrder <- seq(nrow(levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]))
  levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,]$MonitoringLocation.NumberObs <- nrow(levels_clean_1000[levels_clean_1000$MonitoringLocationIdentifier == id,])
  counter = counter + 1
  print(paste(counter, "out of 2866"))
}

levels_accepted <- subset(levels_clean_1000, ResultStatusIdentifier == "Accepted")

counter = 0
for (id in unique(levels_accepted$MonitoringLocationIdentifier)){
  levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$PrecedingLevelMean <- cummean(levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$ResultMeasureValue.ug)
  levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$FollowingLevelMean <- rev(cummean(rev(levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$ResultMeasureValue.ug)))
  levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$LevelDiscontinuity <- lead(levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$FollowingLevelMean, 1) - levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$PrecedingLevelMean
  levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$MonitoringLocation.ActivityOrder <- seq(nrow(levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]))
  levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,]$MonitoringLocation.NumberObs <- nrow(levels_accepted[levels_accepted$MonitoringLocationIdentifier == id,])
  counter = counter + 1
  print(paste(counter, "out of 988"))
}

levels_accepted <- subset(levels_accepted, MonitoringLocation.NumberObs > 7)
levels_accepted_disco <- subset(levels_accepted, (MonitoringLocation.ActivityOrder > 3 & MonitoringLocation.ActivityOrder < MonitoringLocation.NumberObs - 3))

DiscontinuitySummary_accepted <- levels_accepted_disco %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarize(
    MinLevelDiscontinuity = LevelDiscontinuity[which.min(LevelDiscontinuity)],
    MinLD_Index = MonitoringLocation.ActivityOrder[which.min(LevelDiscontinuity)],
    MaxLevelDiscontinuity = LevelDiscontinuity[which.max(LevelDiscontinuity)],
    MaxLD_Index = MonitoringLocation.ActivityOrder[which.max(LevelDiscontinuity)],
    NumberObs = max(MonitoringLocation.NumberObs),
    MinLithLevel = min(ResultMeasureValue.ug),
    MaxLithLevel = max(ResultMeasureValue.ug)
  )

DiscoSumm_Ac <- subset(DiscontinuitySummary_accepted, (MinLevelDiscontinuity < -40 | MaxLevelDiscontinuity > 40))
levels_discosumm_AC <- levels_accepted[levels_accepted$MonitoringLocationIdentifier %in% DiscoSumm_Ac$MonitoringLocationIdentifier,]


ggplot(data = levels_discosumm_AC) +
  geom_point(aes(x = ActivityStartDate.Date, y = ResultMeasureValue.ug)) +
  ylim(0, 200)+
  facet_wrap(vars(MonitoringLocationIdentifier))




# Remove sites with fewer than 8 measurements
levels_final <- subset(levels_clean_1000, MonitoringLocation.NumberObs > 7)
levels_final <- subset(levels_final, (MonitoringLocation.ActivityOrder > 3 & MonitoringLocation.ActivityOrder < MonitoringLocation.NumberObs - 3))

# Summarize discontinuity statistics
DiscontinuitySummary <- levels_final %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarize(
    MinLevelDiscontinuity = LevelDiscontinuity[which.min(LevelDiscontinuity)],
    MinLD_Index = MonitoringLocation.ActivityOrder[which.min(LevelDiscontinuity)],
    MaxLevelDiscontinuity = LevelDiscontinuity[which.max(LevelDiscontinuity)],
    MaxLD_Index = MonitoringLocation.ActivityOrder[which.max(LevelDiscontinuity)],
    NumberObs = max(MonitoringLocation.NumberObs),
    MinLithLevel = min(ResultMeasureValue.ug),
    MaxLithLevel = max(ResultMeasureValue.ug)
    )

DiscoSumm <- subset(DiscontinuitySummary, (MinLevelDiscontinuity < -50 | MaxLevelDiscontinuity > 50))
DiscoSumm <- subset(DiscoSumm, NumberObs > 30)
DiscoSumm <- subset(DiscoSumm, MinLithLevel < 30)

levels_discosumm <- levels_final[levels_final$MonitoringLocationIdentifier %in% DiscoSumm$MonitoringLocationIdentifier,]

ggplot(data = levels_discosumm) +
  geom_point(aes(x = ActivityStartDate.Date, y = ResultMeasureValue.ug)) +
  ylim(0, 200)+
  facet_wrap(vars(MonitoringLocationIdentifier))

# Investigate USGS-08210000
lithium_investigation = subset(levels_clean_1000, MonitoringLocationIdentifier == "USGS-08210000" & ResultStatusIdentifier == "Accepted") 

ggplot(data = lithium_investigation, aes(x = ResultMeasureValue.ug)) +
  geom_histogram(binwidth=10)

ggplot(data = lithium_investigation) +
  geom_point(aes(x = ActivityStartDate.Date, y = ResultMeasureValue.ug, group = ))



# Investigate 	USGS-06338490
lithium_investigation = subset(levels_final, MonitoringLocationIdentifier == "USGS-06338490") 

ggplot(data = lithium_investigation, aes(x = ResultMeasureValue.ug)) +
  geom_histogram(binwidth=10)

ggplot(data = lithium_investigation) +
  geom_point(aes(x = ActivityStartDate.Date, y = ResultMeasureValue.ug, group = ))
