library(tidyverse)
library(data.table)
library(haven)
library(AER)
library(stargazer)


# load Angrist & Evans data
pums80 <- as.data.table(read_dta('data/pums80.dta'))
summary(pums80)


# make a table of charaterstics of the different groups of women

pums80_3pluskids <- pums80[pums80$kidcount > 2,]


row_names_column <- c('Mean children ever born',
                      'Percent with more than 2 children',
                      'Percent worked last year',
                      'Observations')
values_column <- c(round(mean(pums80$kidcount), 2),
                   round(nrow(pums80_3pluskids)/nrow(pums80)*100, 2),
                   round(sum(pums80$workedm)/nrow(pums80)*100, 2),
                   nrow(pums80)
)

characteristics_table <- data.frame('PUMS 1980' = values_column)
row.names(characteristics_table) <- row_names_column

stargazer(
  characteristics_table, 
  header = FALSE,
  summary = FALSE, 
  style = 'AER',
  title = 'Fertility and Labor-Supply Measures, 1980 PUMS Data',
  covariate.labels = c('Women aged 21-35 with 2 or more children', '')
)

# run the IV regression

ivreg(|, data=pums80)
                        

