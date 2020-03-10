library(tidyverse)
library(data.table)
library(haven)
library(AER)
library(stargazer)
library(ivpack)
library(estimatr)


# load Angrist & Evans data
pums80 <- as.data.table(read_dta('data/pums80.dta'))
summary(pums80)

# define helper function for robust std errors
get_robust_ses <- function(fit) {
  if(inherits(fit, "numeric")){return(NA)}
  sqrt(
    diag(
      sandwich::vcovHC(fit, type = "HC1")
    )
  )
}


# make a table of fertility and labor-supply measures of the women

# add necessary variables
pums80$threepluskids <- as.numeric(eval(pums80$kidcount >= 3))

# create vectors for each column of the table
row_names_column <- c('Mean children ever born',
                      'Percent with more than 2 children',
                      'Percent worked last year',
                      'Observations')
values_column <- c(round(mean(pums80$kidcount), 2),
                   round(sum(pums80$threepluskids)/nrow(pums80)*100, 2),
                   round(sum(pums80$workedm)/nrow(pums80)*100, 2),
                   nrow(pums80)
)

# create the data frame to feed to stargazer
characteristics_table <- data.frame('PUMS 1980' = values_column)
row.names(characteristics_table) <- row_names_column

# create the Latex table
stargazer(
  characteristics_table, 
  header = FALSE,
  summary = FALSE, 
  style = 'AER',
  title = 'Fertility and Labor-Supply Measures, 1980 PUMS Data',
  covariate.labels = c('Women aged 21-35 with 2 or more children', '')
)

view_stargazer(characteristics_table, 
               summary = FALSE, 
               style = 'AER',
               title = 'Fertility and Labor-Supply Measures, 1980 PUMS Data',
               covariate.labels = c('Women aged 21-35 with 2 or more children', ''))

# make a table of descriptive statistics of the women

# add necessary variables
pums80$twoboys <- as.numeric(eval(pums80$boy1st + pums80$boy2nd == 2))
pums80$twogirls <- as.numeric(eval(pums80$boy1st + pums80$boy2nd == 0))

# create vectors for each column of the table
row_names_column_2 <- c('Children ever born',
                        ' ',
                        '  ',
                        'More than 2 children',
                        '   ',
                        '    ',
                        'Boy 1st',
                        '     ',
                        '      ',
                        'Boy 2nd',
                        '       ',
                        '        ',
                        'Two boys',
                        '         ',
                        '          ',
                        'Two girls',
                        '           ',
                        '            ',
                        'Same sex',
                        '             ',
                        '              ',
                        'Twins-2',
                        '               ',
                        '                ',
                        'Age',
                        '                 ',
                        '                  ',
                        'Age at first birth',
                        '                   ',
                        '                    ',
                        'Worked for pay',
                        '                     ',
                        '                      ',
                        'Weeks worked',
                        '                       ',
                        '                        ',
                        'Hours/week',
                        '                         ',
                        '                          ',
                        'Labor income',
                        '                           ',
                        '                            ',
                        'Number of observations')
values_column_2 <- c(round(mean(pums80$kidcount), 2),
                     paste('(', round(sd(pums80$kidcount), 3), ')', sep=''),
                     '',
                     round(mean(pums80$threepluskids), 3),
                     paste('(', round(sd(pums80$threepluskids), 3), ')', sep=''),
                     '',
                     round(mean(pums80$boy1st), 3),
                     paste('(', round(sd(pums80$boy1st), 3), ')', sep=''),
                     '',
                     round(mean(pums80$boy2nd), 3),
                     paste('(', round(sd(pums80$boy2nd), 3), ')', sep=''),
                     '',
                     round(mean(pums80$twoboys), 3),
                     paste('(', round(sd(pums80$twoboys), 3), ')', sep=''),
                     '',
                     round(mean(pums80$twogirls), 3),
                     paste('(', round(sd(pums80$twogirls), 3), ')', sep=''),
                     '',
                     round(mean(pums80$samesex), 3),
                     paste('(', round(sd(pums80$samesex), 3), ')', sep=''),
                     '',
                     round(mean(pums80$multi2nd), 4),
                     paste('(', round(sd(pums80$multi2nd), 4), ')', sep=''),
                     '',
                     round(mean(pums80$agem1), 1),
                     paste('(', round(sd(pums80$agem1), 1), ')', sep=''), 
                     '',
                     round(mean(pums80$agefstm), 1),
                     paste('(', round(sd(pums80$agefstm), 1), ')', sep=''),
                     '',
                     round(mean(pums80$workedm), 3),
                     paste('(', round(sd(pums80$workedm), 3), ')', sep=''), 
                     '',
                     round(mean(pums80$weeksm1), 1),
                     paste('(', round(sd(pums80$weeksm1), 1), ')', sep=''),
                     '',
                     round(mean(pums80$hourswm), 1),
                     paste('(', round(sd(pums80$hourswm), 1), ')', sep=''),
                     '',
                     round(mean(pums80$incomem), 0),
                     paste('(', round(sd(pums80$incomem), 0), ')', sep=''),
                     '',
                     nrow(pums80)
)

# create the data frame to feed to stargazer
characteristics_table_2 <- data.frame('PUMS 1980' = values_column_2)
row.names(characteristics_table_2) <- row_names_column_2

# create the Latex table
stargazer(
  characteristics_table_2, 
  header = FALSE,
  summary = FALSE, 
  style = 'AER',
  title = 'Descriptive Statistics, 1980 PUMS Data',
  covariate.labels = c('Married women aged 21-35 with 2 or more children', 'Mean (Standard Deviation)'),
  notes = c('Notes: This data includes women with two or more children except for women',  
            'whose second birth was less than a year old when surveyed.')
)

# run the regressions of workedm on threepluskids

workedm_ols <- lm(workedm ~ threepluskids, data=pums80)
workedm_red <- lm(workedm ~ samesex, data=pums80)
workedm_1ststage <- lm_robust(threepluskids ~ samesex, data=pums80)

pums80_2ndstage <- as.data.table(pums80[,list(samesex, workedm)])
pums80_2ndstage$threepluskids <- predict(workedm_1ststage, newdata = pums80_2ndstage)
workedm_2ndstage <- lm(workedm ~ threepluskids, data=pums80_2ndstage)

workedm_iv <- ivreg(workedm ~ threepluskids | samesex, data=pums80)


view_stargazer <- function(...) {
  tf <- tempfile(fileext = ".html")
  tab <- capture.output(stargazer::stargazer(...,outfile = tf, header = F, type = "html"))
  writeLines(tab,tf)
  rstudioapi::viewer(tf)
}


models = list(workedm_ols, workedm_iv, workedm_2ndstage, workedm_iv)
models_se = starprep(workedm_ols, workedm_iv, workedm_2ndstage, workedm_iv)
models_se[2] = c(0,0)

view_stargazer(models,
               style = 'AER',
               title = 'OLS and 2SLS Regressions, 1980 PUMS Data',
               omit = 'Constant',
               coef = list(NULL, c(0, as.numeric(workedm_red$coefficients['samesex']/workedm_1ststage$coefficients['samesex'])), NULL, NULL),
               se = models_se,
               omit.stat = c('ser', 'adj.rsq'),
               
               column.labels = c('OLS', 'CIV', 'M2SLS', '2SlS'),
               model.names = F,
               dep.var.labels = c('Worked in past year'),
               covariate.labels = c('More than 2 kids')
               
)

stargazer(
  models, 
  header = FALSE,
  summary = FALSE, 
  style = 'AER',
  omit = 'Constant',
  omit.stat = 'adj.rsq',
  coef = list(NULL, c(0, as.numeric(workedm_red$coefficients['samesex']/workedm_1ststage$coefficients['samesex'])), NULL, NULL),
  se = models_se,
  notes = c('Notes: This data includes women with two or more children except for women',  
            'whose second birth was less than a year old when surveyed.')
)

sqrt(
  diag(
    sandwich::vcovHC(workedm_iv, type = "HC1")
  )
)
