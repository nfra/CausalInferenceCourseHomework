library(tidyverse)
library(urbnmapr)
library(plotly)

lithium_fips <- read.csv("~/UT Courses/3. Spring 2019/Causal Inference/CausalInferenceCourseHomework/ResearchProject/data/lithium_water_data/US_groundwater_lithium_withFIPS.csv", stringsAsFactors=FALSE, colClasses = c(NA, NA, NA, NA, "character"))

lithium_fips <- US_groundwater_lithium_withFIPS[US_groundwater_lithium_withFIPS$Primary.water.use %in% c("DOMESTIC", "PUBLIC SUPPLY"),]

lithium_fips_summary <- lithium_fips %>%
  group_by(county_fips) %>%
  summarize(mean.lithiumlevel = mean(Lithium.mcg.L))



lithium_data <- left_join(lithium_fips_summary, counties, by = "county_fips") 

counties
ggplotly(
  lithium_data %>%
  ggplot(aes(long, lat, group = group, fill = mean.lithiumlevel)) +
  geom_polygon(color = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Groundwater Lithium Levels, mcg/L, top-coded")
  )

summary(lithium_fips_summary)

lithium_fips_summary[lithium_fips_summary$mean.lithiumlevel > 100, 2] = 100
