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
library(sandwich)

setwd("C:/Users/Nathan/Documents/UT Courses/3. Spring 2019/Causal Inference/CausalInferenceCourseHomework/Assignment1_RDD") 

# Load Hansen's data

dwi = read.csv("./data/hansen_dwi.csv")

# add new vars: 
#   bac_min is the minimum of the two measured BACs (as used in the paper)
#   bac_overlimit is an indicator variable for whether BAC is over the 0.08% limit

dwi$bac_min <- apply(dwi[,c('bac1', 'bac2')], 1, min)
dwi$over_limit <- as.numeric(eval(dwi$bac_min >= 0.08)) #+ as.numeric(eval(dwi$bac_min >= 0.15)) 

# recreate figure 1, histogram of bac_min

manipulation_histogram <- ggplot(data = dwi) +
  theme_clean() +
  theme(plot.background = element_blank()) +
  geom_histogram(aes(x=bac_min), binwidth = 0.001) +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  labs(title="BAC histogram") +
  xlab("BAC") +
  ylab("Frequency") +
  ylim(0,2000)
manipulation_histogram

# check for covariate balance

# create running variable centered at 0
dwi$bac_centered <- dwi$bac_min - 0.08

# run regressions of covariates on running variable
lm_male_on_bac <- lm(male ~ bac_centered*over_limit, data=dwi)
lm_white_on_bac <- lm(white ~ bac_centered*over_limit, data=dwi)
lm_aged_on_bac <- lm(aged ~ bac_centered*over_limit, data=dwi)
lm_acc_on_bac <- lm(acc ~ bac_centered*over_limit, data=dwi)

models <- list(lm_male_on_bac, lm_white_on_bac, lm_aged_on_bac, lm_acc_on_bac)


# calculate robust standard errors for each regression

cov_male <- vcovHC(lm_male_on_bac, type = "HC")
robust_se_male <- sqrt(diag(cov_male))
cov_white <- vcovHC(lm_white_on_bac, type = "HC")
robust_se_white <- sqrt(diag(cov_white))
cov_aged <- vcovHC(lm_aged_on_bac, type = "HC")
robust_se_aged <- sqrt(diag(cov_aged))
cov_acc <- vcovHC(lm_acc_on_bac, type = "HC")
robust_se_acc <- sqrt(diag(cov_acc))

robust_se_list <- list(robust_se_male, robust_se_white, 
                      robust_se_aged, robust_se_acc)


# create Latex table
table_1_cvar_bal <- stargazer(models, 
                              header = FALSE, 
                              style = "aer", 
                              title = "Regression Discontinuity Estimates for the Effect of Exceeding BAC Thresholds on Predetermined Characteristics",
                              column.labels = c("Male", "White","Age", "Accident"),
                              covariate.labels = c("DUI"),
                              omit = c("Constant", "bac_centered", "bac_centered:over_limit"),
                              se = robust_se_list,
                              dep.var.caption  = "abc",
                              dep.var.labels.include = FALSE)


# recreate figure 2, panels A-D

dwi$bac_bucket <- cut(dwi$bac_min, seq(from=0, to=0.436, by=0.002), 
                      labels=(seq(from=0.001, to=0.435, by=0.002)),
                      include.lowest=TRUE)

dwi_accident_sum <- dwi %>%
  group_by(bac_bucket) %>%
  summarize(
    mean_male = mean(male),
    mean_white = mean(white),
    mean_acc = mean(acc),
    mean_aged = mean(aged),
    number_obs = n(),
    over_limit = unique(as.numeric(eval(bac_min >= 0.08)))
  )

dwi_accident_sum$bac_min <- as.numeric(as.character(dwi_accident_sum$bac_bucket))

# panel A - accident at scene

dwi_accident_sum$acc <- dwi_accident_sum$mean_acc

panel_a <- ggplot(data = dwi, 
       aes(x = bac_min, 
           y = acc, 
           group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_accident_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  labs(title="Panel A. Accident at scene") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(0.05, 0.25))
panel_a

# panel B - male
  
dwi_accident_sum$male <- dwi_accident_sum$mean_male

panel_b <- ggplot(data = dwi, 
                  aes(x = bac_min, 
                      y = male, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_accident_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  labs(title="Panel B. Male") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(0.74, 0.83))
panel_b

# panel C - age

dwi_accident_sum$aged <- dwi_accident_sum$mean_aged

panel_c <- ggplot(data = dwi, 
                  aes(x = bac_min, 
                      y = aged, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_accident_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  labs(title="Panel C. Age") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(33, 38))
panel_c

# panel D - white

dwi_accident_sum$white <- dwi_accident_sum$mean_white

panel_d <- ggplot(data = dwi, 
                  aes(x = bac_min, 
                      y = white, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_accident_sum, shape = 1) +
  geom_smooth(method = 'lm', color = 'black') +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
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
                  aes(x = bac_min, 
                      y = acc, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_accident_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  labs(title="Panel A. Accident at scene") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(0.05, 0.25))
panel_a_quadratic

# panel B - male

panel_b_quadratic <- ggplot(data = dwi, 
                  aes(x = bac_min, 
                      y = male, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_accident_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  labs(title="Panel B. Male") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(0.74, 0.83))
panel_b_quadratic

# panel C - age

panel_c_quadratic <- ggplot(data = dwi, 
                  aes(x = bac_min, 
                      y = aged, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_accident_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  labs(title="Panel C. Age") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim = c(0.03, 0.2), ylim = c(33, 38))
panel_c_quadratic

# panel D - white

panel_d_quadratic <- ggplot(data = dwi, 
                  aes(x = bac_min, 
                      y = white, 
                      group = over_limit)) +
  theme_clean() +
  theme(plot.background=element_blank()) +
  geom_point(data = dwi_accident_sum, shape = 1) +
  geom_smooth(method = 'lm', formula= y ~ poly(x,2), color = 'black') +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  labs(title="Panel D. White") +
  xlab("BAC") +
  ylab("") +
  coord_cartesian(xlim=c(0.03, 0.2), ylim=c(0.8, 0.9))
panel_d_quadratic

# Assemble panels into figure 2 replication
grid.arrange(panel_a_quadratic, panel_b_quadratic, 
             panel_c_quadratic, panel_d_quadratic, nrow = 2)


# extras

lm_recid <- lm(recidivism ~ male + white + aged + acc +
                 over_limit + bac_min + over_limit*bac_min, 
               data=dwi)
summary(lm_recid)

