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

# load acs 2011-2018
acs = as.data.table(read.csv("./data/ipums_usa_data/usa_00012.csv"))

# create puma code variable
acs$pumacode = as.numeric(paste0(acs$STATEFIP, str_pad(acs$PUMA, 5, side='left', pad='0')))

# create treatment variable per person in PUMA
acs$treatment = pumas_df[match(acs$pumacode, pumas_df$GISMATCH), 'treatment_value']
rm(pumas, pumas_df)

# take subset without NA in treatment
acs = subset(acs, !is.na(acs$treatment))

# generalize MOVEDIN to rest of household
acs$MOVEDIN.gen = subset(acs, MOVEDIN != 0)[match(acs$SERIAL, subset(acs, MOVEDIN != 0)$SERIAL), 'MOVEDIN']

# drop education level less than high school
acs = subset(acs, EDUC > 2)

# drop those with disabilities
acs = subset(acs, DIFFREM == 1 & DIFFPHYS == 1 & DIFFMOB == 1 & DIFFCARE == 1 & DIFFSENS == 1)

# turn numeric into factors as necessary
acs$YEAR = as.factor(acs$YEAR)
acs$REGION = as.factor(acs$REGION)
acs$METRO = as.factor(acs$METRO)
acs$RELATE = as.factor(acs$RELATE)
acs$SEX = as.factor(acs$SEX)
acs$MARST = as.factor(acs$MARST)
acs$MARRINYR = as.factor(acs$MARRINYR)
acs$DIVINYR = as.factor(acs$DIVINYR)
acs$WIDINYR = as.factor(acs$WIDINYR)
acs$FERTYR = as.factor(acs$FERTYR)
acs$RACE = as.factor(acs$RACE)
acs$HISPAN = as.factor(acs$HISPAN)
acs$SPEAKENG = as.factor(acs$SPEAKENG)
acs$HCOVANY = as.factor(acs$HCOVANY)
acs$SCHOOL = as.factor(acs$SCHOOL)
acs$EMPSTAT = as.factor(acs$EMPSTAT)
acs$LABFORCE = as.factor(acs$LABFORCE)
acs$CLASSWKR = as.factor(acs$CLASSWKR)
acs$WKSWORK2 = as.factor(acs$WKSWORK2)
acs$LOOKING = as.factor(acs$LOOKING)
acs$WORKEDYR = as.factor(acs$WORKEDYR)
acs$WRKLSTWK = as.factor(acs$WRKLSTWK)
acs$ABSENT = as.factor(acs$ABSENT)
acs$MOVEDIN = as.factor(acs$MOVEDIN)
acs$MOVEDIN.gen = as.factor(acs$MOVEDIN.gen)
acs$DIFFREM = as.factor(acs$DIFFREM)
acs$DIFFPHYS = as.factor(acs$DIFFPHYS)
acs$DIFFMOB = as.factor(acs$DIFFMOB)
acs$DIFFCARE = as.factor(acs$DIFFCARE)
acs$DIFFSENS = as.factor(acs$DIFFSENS)

acs = subset(acs, !is.na(MOVEDIN.gen))




############
# matching #
############

# define matching covars
matching_covars = c(20,22,34)
matching_categoricals = c(1,9,11,17,19,20,21,23,24,25,26,27,29,31,61)

# run matching on whether they looked for work last week
# looking_match = Matchby(
#   Y = subset(acs, LOOKING == 1 | LOOKING == 2)$LOOKING,
#   Tr = subset(acs, LOOKING == 1 | LOOKING == 2)$treatment,
#   X = subset(acs, LOOKING == 1 | LOOKING == 2, select = matching_covars),
#   by = subset(acs, LOOKING == 1 | LOOKING == 2, select = matching_categoricals),
#   estimand = "ATE",
#   print.level = 0
# )
# 
# looking_mb_out = capture.output(MatchBalance(treatment ~ YEAR + REGION + METRO + RELATE + GQ + AGE + MARST + MARRINYR + DIVINYR + WIDINYR + FERTYR + RACE + HISPAN + SPEAKENG + EDUC,
#                           data = subset(acs, LOOKING == 1 | LOOKING == 2),
#                           match.out = looking_match))
# 
# fileConn = file('./data/looking_match_balance.txt')
# writeLines(looking_mb_out, fileConn)
# close(fileConn)
# rm(looking_match, looking_mb_out)

# run matching on whether they worked last week
matching_covars_worklstwk = append(matching_covars, c(44))
matching_categoricals_worklstwk = append(matching_categoricals, c(36, 43))

worked_last_week_match = Matchby(
  Y = subset(acs, LABFORCE == 2 & CLASSWKR == 2 & WKSWORK2 != 0 & ABSENT == 3 & (WRKLSTWK == 1 | WRKLSTWK == 2))$WRKLSTWK,
  Tr = subset(acs, LABFORCE == 2 & CLASSWKR == 2 & WKSWORK2 != 0 & ABSENT == 3 & (WRKLSTWK == 1 | WRKLSTWK == 2))$treatment,
  X = subset(acs, LABFORCE == 2 & CLASSWKR == 2 & WKSWORK2 != 0 & ABSENT == 3 & (WRKLSTWK == 1 | WRKLSTWK == 2), select = matching_covars_worklstwk),
  by = subset(acs, LABFORCE == 2 & CLASSWKR == 2 & WKSWORK2 != 0 & ABSENT == 3 & (WRKLSTWK == 1 | WRKLSTWK == 2), select = matching_categoricals_worklstwk),
  estimand = "ATE",
  print.level = 0
)

worked_last_week_mb_out = capture.output(MatchBalance(treatment ~ YEAR + REGION + METRO + RELATE + GQ + AGE + MARST + MARRINYR + DIVINYR + WIDINYR + FERTYR + RACE + HISPAN + SPEAKENG + EDUC + UHRSWORK + EMPSTAT + WKSWORK2,
                          data = subset(acs, LABFORCE == 2 & CLASSWKR == 2 & WKSWORK2 != 0 & ABSENT == 3 & (WRKLSTWK == 1 | WRKLSTWK == 2)),
                          match.out = worked_last_week_match))

fileConn = file('./data/worked_last_week_match_balance.txt')
writeLines(worked_last_week_mb_out, fileConn)
close(fileConn)
rm(worked_last_week_match, worked_last_week_mb_out)

# take random subsample because my computer is shit
set.seed(76334264)
acs = acs[sample(c(1:nrow(acs)), size = 5000000, replace = F),]

# run matching on employment status
# employment_status_match = Matchby(
#   Y = subset(acs, EMPSTAT == 1 | EMPSTAT == 2)$EMPSTAT,
#   Tr = subset(acs, EMPSTAT == 1 | EMPSTAT == 2)$treatment,
#   X = subset(acs, EMPSTAT == 1 | EMPSTAT == 2, select = matching_covars),
#   by = subset(acs, EMPSTAT == 1 | EMPSTAT == 2, select = matching_categoricals),
#   estimand = "ATE",
#   print.level = 0
# )
# 
# employment_status_mb_out = capture.output(MatchBalance(treatment ~ YEAR + REGION + METRO + RELATE + GQ + AGE + MARST + MARRINYR + DIVINYR + WIDINYR + FERTYR + RACE + HISPAN + SPEAKENG + EDUC,
#                                     data = subset(acs, EMPSTAT == 1 | EMPSTAT == 2),
#                                     match.out = employment_status_match))
# 
# fileConn = file('./data/employment_status_match_balance.txt')
# writeLines(employment_status_mb_out, fileConn)
# close(fileConn)
# rm(employment_status_match, employment_status_mb_out)

# run matching on labor force participation
labor_force_match = Matchby(
  Y = subset(acs, LABFORCE == 1 | LABFORCE == 2)$LABFORCE,
  Tr = subset(acs, LABFORCE == 1 | LABFORCE == 2)$treatment,
  X = subset(acs, LABFORCE == 1 | LABFORCE == 2 , select = matching_covars),
  by = subset(acs, LABFORCE == 1 | LABFORCE == 2 , select = matching_categoricals),
  estimand = "ATE",
  print.level = 0
)

labor_force_mb_out = capture.output(MatchBalance(treatment ~ YEAR + REGION + METRO + RELATE + GQ + AGE + MARST + MARRINYR + DIVINYR + WIDINYR + FERTYR + RACE + HISPAN + SPEAKENG + EDUC,
                              data = subset(acs, LABFORCE == 1 | LABFORCE == 2),
                              match.out = labor_force_match))

fileConn = file('./data/labor_force_match_balance.txt')
writeLines(labor_force_mb_out, fileConn)
close(fileConn)
rm(labor_force_match, labor_force_mb_out)

# run matching on whether they worked last year
worked_last_year_match = Matchby(
  Y = subset(acs, WORKEDYR == 1 | WORKEDYR == 3)$WORKEDYR,
  Tr = subset(acs, WORKEDYR == 1 | WORKEDYR == 3)$treatment,
  X = subset(acs, WORKEDYR == 1 | WORKEDYR == 3, select = matching_covars),
  by = subset(acs, WORKEDYR == 1 | WORKEDYR == 3, select = matching_categoricals),
  estimand = "ATE",
  print.level = 0
)

worked_last_year_mb_out = capture.output(MatchBalance(treatment ~ YEAR + REGION + METRO + RELATE + GQ + AGE + MARST + MARRINYR + DIVINYR + WIDINYR + FERTYR + RACE + HISPAN + SPEAKENG + EDUC,
                                                      data = subset(acs, WORKEDYR == 1 | WORKEDYR == 3),
                                                      match.out = worked_last_year_match))

worked_last_year_match_out = unlist(stringr::str_split(capture.output(summary(worked_last_year_match)), "  "))

fileConn = file('./data/worked_last_year_match_balance.txt')
writeLines(worked_last_year_mb_out, fileConn)
close(fileConn)
fileConn = file('./data/worked_last_year_match.txt')
writeLines(worked_last_year_match_out, fileConn)
close(fileConn)
rm(worked_last_year_match, worked_last_year_mb_out)

# create summary chracter vector
match_summaries = unlist(str_split(c(capture.output(summary(employment_status_match)),
                             capture.output(summary(labor_force_match)),
                             capture.output(summary(looking_match)),
                             capture.output(summary(work_last_week_match)),
                             capture.output(summary(worked_last_year_match))), "  "))

# create results table
match_table_dt = data.table(' ' = c("Estimate", "Standard Error", "T-statistic", "P-value", " ", "Original Num. of Observations", "Original Num. of Treated Obs.", "Matched Num. of Observations", "Num. of Treated Obs. Dropped"),
                            'Employment Status' = match_summaries[c(3,5,7,9,1,12,14,16,1)],
                            'Lab. Force Part.' = match_summaries[c(23,25,27,29,1,32,34,36,1)],
                            'Looked for Work' = match_summaries[c(43,45,47,49,1,52,54,56,1)],
                            'Worked Last Week' = match_summaries[c(63,65,67,69,1,72,74,76,82)],
                            'Worked Last Year' = match_summaries[c(86,88,90,92,1,95,97,99,1)]
                            )

# save results to CSV for reference
write.csv(match_table_dt, "F:/match_results_table.csv")
