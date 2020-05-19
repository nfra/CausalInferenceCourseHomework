library(tidyverse)
library(sf)
library(data.table)
library(Matching)
library(stringr)

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
interpolated_lithium_values = as.data.table(read.csv("./data/lithium_water_data/interp_values.csv", 
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

# make geometry-less object for matching
pumas_df = st_drop_geometry(pumas[,c(1,10)])

# remove unnecessary objects from the environment to free up space in memory
rm(grid_in_puma_list, grid_points_in_puma, interpolated_lithium_values, interpolated_lithium_values_sf, coords_flat, puma_number_local)

# a few plots to include in write-up
#ggplot(data = pumas) +
#  geom_histogram(aes(x = lithium_value), binwidth = 5)

#ggplot(data = pumas) +
#  geom_sf(aes(fill = lithium_value, color = lithium_value))

#ggplot(data = pumas) +
#  geom_sf(aes(fill = treatment_value, color = treatment_value))

##############
# import ACS #
##############

# load acs 2012-2013 for time's sake
acs = as.data.table(rbind(read.csv("./data/ipums_usa_data/usa_00010.csv"),read.csv("./data/ipums_usa_data/usa_00011.csv")))

# take subsets (dropping NAs mostly)
acs = subset(acs, MOVEDIN > 0)
acs = subset(acs, RACE < 7)
acs = subset(acs, SPEAKENG < 7)
acs = subset(acs, MARRINYR > 0)
acs = subset(acs, DIVINYR > 0)
acs = subset(acs, FERTYR > 0)
acs = subset(acs, SCHOOL == 1 | SCHOOL == 2)
acs = subset(acs, METRO > 0)

# create puma code variable
acs$pumacode = as.numeric(paste0(acs$STATEFIP, str_pad(acs$PUMA, 5, side='left', pad='0')))

# create treatment variable per person in PUMA
acs$treatment = pumas_df[match(acs$pumacode, pumas_df$GISMATCH), 'treatment_value']

# take subset without NA in treatment
acs = subset(acs, !is.na(acs$treatment))





############
# matching #
############

prop_score = glm(treatment ~ YEAR + REGION + , family = binomial, data = acs)


match = Match(
  Y = acs[EMPSTAT == 1 | EMPSTAT == 2, 'EMPSTAT']$EMPSTAT,
  Tr = acs[EMPSTAT == 1 | EMPSTAT == 2, 'treatment']$treatment,
  X = acs[EMPSTAT == 1 | EMPSTAT == 2, c(1,9,62)],
  version = 'fast',
  ties = FALSE
)

summary(match)
summary.Match(match)
mb = MatchBalance(treatment ~ (YEAR + REGION + MOVEDIN)^2, data = subset(acs, EMPSTAT == 1 | EMPSTAT == 2), match)


glm(treatment, method = 'binomial')