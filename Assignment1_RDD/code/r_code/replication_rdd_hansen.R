# Name: replication_rdd_hansen
# Written by: Nathan Franz
# Description: A replication of several main figures and tables from the 2015 
# 			   article "Punishment and Deterrence: Evidence from Drunk Driving"
# 			   by Benjamin Hansen
# Last edited: 2/25/2020

library(tidyverse)
library(ggthemes)

setwd("C:/Users/Nathan/Documents/UT Courses/3. Spring 2019/Causal Inference/CausalInferenceCourseHomework/Assignment1_RDD") 

# Load Hansen's data

dwi = read.csv("./data/hansen_dwi.csv")

# add new vars: 
#   bac_min is the minimum of the two measured BACs (as used in the paper)
#   bac_overlimit is an indicator variable for whether BAC is over the 0.08% limit

dwi$bac_min <- apply(dwi[,c('bac1', 'bac2')], 1, min)
dwi$overlimit <- eval(dwi$bac_min >= 0.08)

# investigate raw data for evidence of manipulation, using histogram

manipulation_histogram <- ggplot(data = dwi) +
  theme_clean() +
  geom_histogram(aes(x=bac_min), binwidth = 0.001) +
  geom_vline(xintercept = 0.08) +
  geom_vline(xintercept = 0.15) +
  xlab("BAC") +
  ylab("Frequency")+
  ylim(0,2000)
manipulation_histogram  

