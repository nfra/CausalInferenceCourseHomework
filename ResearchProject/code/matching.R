library(tidyverse)
library(sf)
library(data.table)
library(Matching)


##############
# import ACS #
##############

acs = as.data.table(read.csv("./data/ipums_usa_data/usa_00005.csv"))


summary(acs$WKSWORK2)

summary(acs$MOVEDIN)