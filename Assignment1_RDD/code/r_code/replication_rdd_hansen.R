# Name: replication_rdd_hansen
# Written by: Nathan Franz
# Description: A replication of several main figures and tables from the 2015 
# 			   article "Punishment and Deterrence: Evidence from Drunk Driving"
# 			   by Benjamin Hansen
# Last edited: 2/25/2020

library(tidyverse)
library(ggthemes)
library(gridExtra)
library(stargazer)
library(rdrobust)
library(rdd)

setwd("C:/Users/Nathan/Documents/UT Courses/3. Spring 2019/Causal Inference/CausalInferenceCourseHomework/Assignment1_RDD") 

# Load Hansen's data

dwi = read.csv("./data/hansen_dwi.csv")

# add new vars: 
#   over_limit is an indicator variable for whether BAC is over the 0.08% limit

dwi$over_limit <- as.numeric(eval(dwi$bac1 >= 0.08)) 

# recreate figure 1, histogram of bac1

manipulation_histogram <- ggplot(data = dwi) +
  theme_clean() +
  theme(plot.background = element_blank()) +
  geom_histogram(aes(x=bac1), binwidth = 0.001) +
  geom_vline(xintercept = 0.08) +
  labs(title="BAC histogram") +
  xlab("BAC") +
  ylab("Frequency") +
  ylim(0,2000)
manipulation_histogram

# check for covariate balance


# run regressions of covariates on running variable
lm_male_on_bac <- lm(male ~ bac1*over_limit, data=dwi)
lm_white_on_bac <- lm(white ~ bac1*over_limit, data=dwi)
lm_aged_on_bac <- lm(aged ~ bac1*over_limit, data=dwi)
lm_acc_on_bac <- lm(acc ~ bac1*over_limit, data=dwi)

models <- list(lm_male_on_bac, lm_white_on_bac, lm_aged_on_bac, lm_acc_on_bac)


# calculate robust standard errors for each regression

get_robust_ses <- function(fit) {
  sqrt(
    diag(
      sandwich::vcovHC(fit, type = "HC1")
    )
  )
}

# create Latex table for R Markdown

tbl1 <- stargazer(models, header = FALSE, style = "aer", 
          title = "Regression Discontinuity Estimates for Covariates",
          column.labels = c("Male", "White","Age", "Accident"),
          covariate.labels = c("DUI"),
          omit = c("Constant", "bac1", "bac1:over_limit"),
          se = lapply(models, get_robust_ses),
          dep.var.caption = '',
          dep.var.labels.include = FALSE)


# recreate figure 2, panels A-D

# Find mean of covariates for 0.002 width buckets of BAC
dwi$bac_bucket <- cut(dwi$bac1, seq(from=0, to=0.500, by=0.002), 
                      labels=(seq(from=0.001, to=0.499, by=0.002)),
                      include.lowest=TRUE)

dwi_sum <- dwi %>%
  group_by(bac_bucket) %>%
  summarize(
    male = mean(male),
    white = mean(white),
    acc = mean(acc),
    aged = mean(aged),
    over_limit = unique(as.numeric(eval(bac1 >= 0.08)))
  )

dwi_sum$bac1 <- as.numeric(as.character(dwi_sum$bac_bucket))

# panel A - accident at scene

panel_a <- ggplot(data = dwi, 
       aes(x = bac1, 
           y = acc, 
           group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Panel A. Accident at scene") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(0.05, 0.25))
panel_a

# panel B - male

panel_b <- ggplot(data = dwi, 
                  aes(x = bac1, 
                      y = male, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Panel B. Male") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(0.74, 0.83))
panel_b

# panel C - age

panel_c <- ggplot(data = dwi, 
                  aes(x = bac1, 
                      y = aged, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Panel C. Age") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(33, 38))
panel_c

# panel D - white

panel_d <- ggplot(data = dwi, 
                  aes(x = bac1, 
                      y = white, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Panel D. White") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim=c(0.03, 0.2), ylim=c(0.8, 0.9))
panel_d

# Assemble panels into figure 2 replication
grid.arrange(panel_a, panel_b, panel_c, panel_d, nrow = 2)



# recreate figure 2, panels A-D, but with quadratic model

# panel A - accident at scene

panel_a_quadratic <- ggplot(data = dwi, 
                  aes(x = bac1, 
                      y = acc, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Panel A. Accident at scene") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(0.05, 0.25))
panel_a_quadratic

# panel B - male

panel_b_quadratic <- ggplot(data = dwi, 
                  aes(x = bac1, 
                      y = male, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Panel B. Male") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(0.74, 0.83))
panel_b_quadratic

# panel C - age

panel_c_quadratic <- ggplot(data = dwi, 
                  aes(x = bac1, 
                      y = aged, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Panel C. Age") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(33, 38))
panel_c_quadratic

# panel D - white

panel_d_quadratic <- ggplot(data = dwi, 
                  aes(x = bac1, 
                      y = white, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Panel D. White") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim=c(0.03, 0.2), ylim=c(0.8, 0.9))
panel_d_quadratic

# Assemble panels into figure 2 replication
grid.arrange(panel_a_quadratic, panel_b_quadratic, 
             panel_c_quadratic, panel_d_quadratic, nrow = 2)


# Estimate equation 1 with recidivism as the outcome, 
#   using BAC data in [0.03,0.13]

# Make linear models, control for bac1 linearly, 
#                     interact bac1 with cutoff linearly, 
#                     interact bac1 with cutoff linearly and quadratically

linear_control <- lm(recidivism ~ 1 + male + white + aged + acc + 
                       bac1 + over_limit,
   data = dwi[dwi$bac1 >= 0.03 & dwi$bac1 <= 0.13,])

linear_interact <- lm(recidivism ~ 1 + male + white + aged + acc + 
                        bac1*over_limit,
   data = dwi[dwi$bac1 >= 0.03 & dwi$bac1 <= 0.13,])

quadratic_interact <- lm(recidivism ~ 1 + male + white + aged + acc + 
                           bac1*over_limit + I(bac1^2)*over_limit,
   data = dwi[dwi$bac1 >= 0.03 & dwi$bac1 <= 0.13,])

# Make a list of the models from above for table

rd_panel_a_models = list(linear_control, linear_interact, quadratic_interact)

# Create Latex table for R Markdown

tbl2 <- stargazer(rd_panel_a_models, header = FALSE, style = "aer", 
          title = "Regression Discontinuity Estimates for the Effect of DUI on Recidivism, BAC in [0.03, 0.13]",
          column.labels = c("Linear Control", "With Interaction","With Quadratic Interaction"),
          covariate.labels = c("DUI"),
          omit = c("Constant", "male", "white", "aged", "acc", "bac1",
                   "I(bac1^2)", "bac1:over_limit", "over_limit:I(bac1^2)"),
          se = lapply(rd_panel_a_models, get_robust_ses),
          dep.var.caption = '',
          dep.var.labels.include = FALSE)





# Estimate equation 1 with recidivism as the outcome, 
#   using BAC data in [0.055,0.105]

# Make linear models, control for bac1 linearly, 
#                     interact bac1 with cutoff linearly, 
#                     interact bac1 with cutoff linearly and quadratically

linear_control_narrow <- lm(recidivism ~ 1 + male + white + aged + acc + 
                       bac1 + over_limit,
                     data = dwi[dwi$bac1 >= 0.055 & dwi$bac1 <= 0.105,])

linear_interact_narrow <- lm(recidivism ~ 1 + male + white + aged + acc + 
                        bac1*over_limit,
                      data = dwi[dwi$bac1 >= 0.055 & dwi$bac1 <= 0.105,])

quadratic_interact_narrow <- lm(recidivism ~ 1 + male + white + aged + acc + 
                           bac1*over_limit + I(bac1^2)*over_limit,
                         data = dwi[dwi$bac1 >= 0.055 & dwi$bac1 <= 0.105,])

# Make a list of the models from above for table

rd_panel_b_models = list(linear_control_narrow, linear_interact_narrow, 
                         quadratic_interact_narrow)

# Create Latex table for R Markdown

tbl3 <- stargazer(rd_panel_b_models, header = FALSE, style = "aer", 
                  title = "Regression Discontinuity Estimates for the Effect of DUI on Recidivism, BAC in [0.055, 0.105]",
                  column.labels = c("Linear Control", "With Interaction","With Quadratic Interaction"),
                  covariate.labels = c("DUI"),
                  omit = c("Constant", "male", "white", "aged", "acc", "bac1",
                           "I(bac1^2)", "bac1:over_limit", "over_limit:I(bac1^2)"),
                  se = lapply(rd_panel_b_models, get_robust_ses),
                  dep.var.caption = '',
                  dep.var.labels.include = FALSE)





# Replicate Panel A of figure 3 from paper
dwi_rd_sum <- dwi[dwi$bac1<0.15,] %>%
  group_by(bac_bucket) %>%
  summarize(
    recidivism = mean(recidivism),
    over_limit = unique(as.numeric(eval(bac1 >= 0.08)))
  )

dwi_rd_sum$bac1 <- as.numeric(as.character(dwi_rd_sum$bac_bucket))

# RD - panel A, linear
rd_linear <- ggplot(data = dwi[dwi$bac1<0.15,], 
                  aes(x = bac1, 
                      y = recidivism, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_rd_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Regression Discontinuity for All Offenders, Linear") +
  xlab("BAC") +
  ylab("Recidivism") +
  coord_cartesian(xlim = c(0.03, 0.15), ylim = c(0.08, 0.16))
rd_linear


# RD - panel A, quadratic
rd_quadratic <- ggplot(data = dwi[dwi$bac1<0.15,], 
                            aes(x = bac1, 
                                y = recidivism, 
                                group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_rd_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  labs(title="Regression Discontinuity for All Offenders, Quadratic") +
  xlab("BAC") +
  ylab("Recidivism") +
  coord_cartesian(xlim = c(0.03, 0.15), ylim = c(0.08, 0.16))
rd_quadratic

