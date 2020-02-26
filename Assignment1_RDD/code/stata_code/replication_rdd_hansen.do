* Name: replication_rdd_hansen
* Written by: Nathan Franz
* Description: A replication of several main figures and tables from the 2015 
* 			   article "Punishment and Deterrence: Evidence from Drunk Driving"
* 			   by Benjamin Hansen.
* Last edited: 2/25/2020

clear

cd "C:/Users/Nathan/Documents/UT Courses/3. Spring 2019/Causal Inference/CausalInferenceCourseHomework/Assignment1_RDD"

* use Hansen's data

use data/hansen_dwi.dta, replace

* generate new vars: 
* 	bac_min is the minimum of the two measured BACs (as used in the paper)
* 	bac_overlimit is an indicator variable for whether BAC is over the 0.08% limit
gen bac_min = min(bac1, bac2)
gen bac_overlimit = bac_min >= 0.08

*

hist bac_min

