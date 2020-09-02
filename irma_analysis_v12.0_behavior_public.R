##9/1/2020 by Anthony Steven Dick and Wesley K. Thompson

rm(list=ls())
setwd("<path>")
list.files()

####################
####################
## Load libraries ##
####################
####################

library(ggplot2)
library(gamm4)
library(MASS)
library(car)
library(plyr)
library(dplyr)
library(forcats)
library(QuantPsyc)
library(data.table)
library(reshape2)
library(psych)
library(jtools)
library(interactions)
library(psycho)
library(tidyverse)
library(simpleboot)
library(DescTools)
library(magrittr)
library(emmeans)

options(max.print=10000)

###################################
###################################
## Load and manipulate ABCD data ##
###################################
###################################
###

###################################
##Data sets and variables used

#This analysis begins after merging of data and recovery of categorical variables,
#recoding of core demographic variables. The
#scripts to conduct this preliminary analysis are available at https://github.com/ABCD-STUDY/analysis-nda17.

###End Missing Data Summary############

nda2.0.1 =  readRDS("nda2.0.1.Rds")
newdat<-subset(nda2.0.1, eventname == "baseline_year_1_arm_1") # if you get Error: vector memory exhausted (limit reached?), try this fix: https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
nda2.0.1.edit<-subset(newdat, newdat$irma_gatea_p == 'No' | newdat$irma_gatea_p == 'Yes' | newdat$irma_gatea_c == 'No' | newdat$irma_gatea_c == 'Yes')
nda2.0.1.edit<-subset(nda2.0.1.edit, nda2.0.1.edit$abcd_site == "site03" | nda2.0.1.edit$abcd_site == "site05" | nda2.0.1.edit$abcd_site == "site10" | nda2.0.1.edit$abcd_site == "site11")
nda2.0.1.edit$abcd_site <- nda2.0.1.edit$abcd_site[,drop=TRUE]
saveRDS(nda2.0.1.edit, "nda2.0.1.edit.Rds") #at this point you should have data from four sites for just the baseline visit, plus the Irma substudy. You can save this smaller data file.
nda2.0.1.edit<-data.frame(nda2.0.1.edit,completedData) # After running any imputation (see remaining code), add the imputed variables
saveRDS(nda2.0.1.edit, "nda2.0.1.edit.Rds")
nda2.0.1.edit<-readRDS("nda2.0.1.edit.Rds") # load the dataframe if you come back to the analysis at a later point so you can start here
nda2.0.1.edit %>% glimpse

#####
#####
#####
###Define PTS Exposure at Baseline from KSADS###

nda2.0.1.edit$ksads_ptsd_total<-(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_770_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_769_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_768_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_767_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_766_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_765_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_764_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_763_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_762_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_761_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_760_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_759_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_758_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_757_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_756_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_755_p)-1) +
(as.numeric(nda2.0.1.edit$ksads_ptsd_raw_754_p)-1)

###Convert factors to numerics for calculations below. Subtract 1 because the factor level will start at 1, not zero.

nda2.0.1.edit$irma_dsm5_2_num <- as.numeric(nda2.0.1.edit$irma_dsm5_2) - 1
nda2.0.1.edit$irma_dsm5_9_num <- as.numeric(nda2.0.1.edit$irma_dsm5_9) - 1
nda2.0.1.edit$irma_dsm5_16_num <- as.numeric(nda2.0.1.edit$irma_dsm5_16) - 1

nda2.0.1.edit$irma_dsm5_15_num <- as.numeric(nda2.0.1.edit$irma_dsm5_15) - 1
nda2.0.1.edit$irma_dsm5_19_num <- as.numeric(nda2.0.1.edit$irma_dsm5_19) - 1

nda2.0.1.edit$irma_dsm5_6_num <- as.numeric(nda2.0.1.edit$irma_dsm5_6) - 1
nda2.0.1.edit$irma_dsm5_22_num <- as.numeric(nda2.0.1.edit$irma_dsm5_22) - 1
nda2.0.1.edit$irma_dsm5_25_num <- as.numeric(nda2.0.1.edit$irma_dsm5_25) - 1
nda2.0.1.edit$irma_dsm5_27_num <- as.numeric(nda2.0.1.edit$irma_dsm5_27) - 1

nda2.0.1.edit$irma_dsm5_20_num <- as.numeric(nda2.0.1.edit$irma_dsm5_20) - 1
nda2.0.1.edit$irma_dsm5_26_num <- as.numeric(nda2.0.1.edit$irma_dsm5_26) - 1

nda2.0.1.edit$irma_dsm5_28_num <- as.numeric(nda2.0.1.edit$irma_dsm5_28) - 1
nda2.0.1.edit$irma_dsm5_29_num <- as.numeric(nda2.0.1.edit$irma_dsm5_29) - 1

nda2.0.1.edit$irma_dsm5_30_num <- as.numeric(nda2.0.1.edit$irma_dsm5_30) - 1
nda2.0.1.edit$irma_dsm5_31_num <- as.numeric(nda2.0.1.edit$irma_dsm5_31) - 1

nda2.0.1.edit$irma_dsm5_5_num <- as.numeric(nda2.0.1.edit$irma_dsm5_5) - 1
nda2.0.1.edit$irma_dsm5_10_num <- as.numeric(nda2.0.1.edit$irma_dsm5_10) - 1
nda2.0.1.edit$irma_dsm5_11_num <- as.numeric(nda2.0.1.edit$irma_dsm5_11) - 1
nda2.0.1.edit$irma_dsm5_14_num <- as.numeric(nda2.0.1.edit$irma_dsm5_14) - 1
nda2.0.1.edit$irma_dsm5_18_num <- as.numeric(nda2.0.1.edit$irma_dsm5_18) - 1

nda2.0.1.edit$irma_dsm5_3_num <- as.numeric(nda2.0.1.edit$irma_dsm5_3) - 1
nda2.0.1.edit$irma_dsm5_13_num <- as.numeric(nda2.0.1.edit$irma_dsm5_13) - 1

nda2.0.1.edit$irma_dsm5_7_num <- as.numeric(nda2.0.1.edit$irma_dsm5_7) - 1
nda2.0.1.edit$irma_dsm5_12_num <- as.numeric(nda2.0.1.edit$irma_dsm5_12) - 1
nda2.0.1.edit$irma_dsm5_17_num <- as.numeric(nda2.0.1.edit$irma_dsm5_17) - 1
nda2.0.1.edit$irma_dsm5_23_num <- as.numeric(nda2.0.1.edit$irma_dsm5_23) - 1

nda2.0.1.edit$irma_dsm5_1_num <- as.numeric(nda2.0.1.edit$irma_dsm5_1) - 1
nda2.0.1.edit$irma_dsm5_4_num <- as.numeric(nda2.0.1.edit$irma_dsm5_4) - 1
nda2.0.1.edit$irma_dsm5_8_num <- as.numeric(nda2.0.1.edit$irma_dsm5_8) - 1
nda2.0.1.edit$irma_dsm5_21_num <- as.numeric(nda2.0.1.edit$irma_dsm5_21) - 1
nda2.0.1.edit$irma_dsm5_24_num <- as.numeric(nda2.0.1.edit$irma_dsm5_24) - 1

nda2.0.1.edit$irma_meqc_2_num <- as.numeric(nda2.0.1.edit$irma_meqc_2) - 1
nda2.0.1.edit$irma_meqc_3_num <- as.numeric(nda2.0.1.edit$irma_meqc_3) - 1
nda2.0.1.edit$irma_meqc_4_num <- as.numeric(nda2.0.1.edit$irma_meqc_4) - 1

nda2.0.1.edit$irma_meqc_7_num <- as.numeric(nda2.0.1.edit$irma_meqc_7) - 1
nda2.0.1.edit$irma_meqc_8_num <- as.numeric(nda2.0.1.edit$irma_meqc_8) - 1
nda2.0.1.edit$irma_meqc_9_num <- as.numeric(nda2.0.1.edit$irma_meqc_9) - 1

nda2.0.1.edit$irma_meqc_12_num <- as.numeric(nda2.0.1.edit$irma_meqc_12) - 1
nda2.0.1.edit$irma_meqc_13_num <- as.numeric(nda2.0.1.edit$irma_meqc_13) - 1
nda2.0.1.edit$irma_meqc_14_num <- as.numeric(nda2.0.1.edit$irma_meqc_14) - 1

nda2.0.1.edit$irma_hurtep_d2_num <- as.numeric(nda2.0.1.edit$irma_hurtep_d2) - 1
nda2.0.1.edit$irma_hurtep_d3_num <- as.numeric(nda2.0.1.edit$irma_hurtep_d3) - 1
nda2.0.1.edit$irma_hurtep_d4_num <- as.numeric(nda2.0.1.edit$irma_hurtep_d4) - 1
nda2.0.1.edit$irma_hurtep_d6_num <- as.numeric(nda2.0.1.edit$irma_hurtep_d5) - 1
nda2.0.1.edit$irma_hurtep_d7_num <- as.numeric(nda2.0.1.edit$irma_hurtep_d6) - 1
nda2.0.1.edit$irma_hurtep_d8_num <- as.numeric(nda2.0.1.edit$irma_hurtep_d7) - 1
nda2.0.1.edit$irma_hurtep_d9_num <- as.numeric(nda2.0.1.edit$irma_hurtep_d8) - 1

nda2.0.1.edit$irma_hurtep_a1_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a1) - 1
nda2.0.1.edit$irma_hurtep_a2_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a2) - 1
nda2.0.1.edit$irma_hurtep_a3_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a3) - 1
nda2.0.1.edit$irma_hurtep_a4_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a4) - 1
nda2.0.1.edit$irma_hurtep_a5_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a5) - 1
nda2.0.1.edit$irma_hurtep_a6_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a6) - 1
nda2.0.1.edit$irma_hurtep_a7_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a7) - 1
nda2.0.1.edit$irma_hurtep_a8_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a8) - 1
nda2.0.1.edit$irma_hurtep_a9_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a9) - 1
nda2.0.1.edit$irma_hurtep_a10_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a10) - 1
nda2.0.1.edit$irma_hurtep_a11_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a11) - 1
nda2.0.1.edit$irma_hurtep_a12_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a12) - 1
nda2.0.1.edit$irma_hurtep_a13_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a13) - 1
nda2.0.1.edit$irma_hurtep_a14_num <- as.numeric(nda2.0.1.edit$irma_hurtep_a14) - 1

nda2.0.1.edit$irma_hurtep_b1a_num <- as.numeric(nda2.0.1.edit$irma_hurtep_b1a) - 1

#############################################
##                                        ###
##                                        ###
##      Define PTS Outcomes etc.         ###
##                                        ###
#############################################

### PTS RI DSM5 SCORING ###
### PRELIMINARY COMPUTATIONS ###
### Pooling the Either/OR items ###

nda2.0.1.edit$irma_dsm5_2_9_16 <- ifelse(nda2.0.1.edit$irma_dsm5_2_num >= nda2.0.1.edit$irma_dsm5_9_num & nda2.0.1.edit$irma_dsm5_2_num >= nda2.0.1.edit$irma_dsm5_16_num, nda2.0.1.edit$irma_dsm5_2_num,
ifelse(nda2.0.1.edit$irma_dsm5_9_num > nda2.0.1.edit$irma_dsm5_2_num & nda2.0.1.edit$irma_dsm5_9_num >= nda2.0.1.edit$irma_dsm5_16_num, nda2.0.1.edit$irma_dsm5_9_num,
ifelse(nda2.0.1.edit$irma_dsm5_16_num >= nda2.0.1.edit$irma_dsm5_9_num & nda2.0.1.edit$irma_dsm5_16_num >= nda2.0.1.edit$irma_dsm5_2_num, nda2.0.1.edit$irma_dsm5_16_num,
nda2.0.1.edit$irma_dsm5_16_num)))

nda2.0.1.edit$irma_dsm5_15_19<-ifelse(nda2.0.1.edit$irma_dsm5_15_num >= nda2.0.1.edit$irma_dsm5_19_num, nda2.0.1.edit$irma_dsm5_15_num, nda2.0.1.edit$irma_dsm5_19_num)

nda2.0.1.edit$irma_dsm5_6_22_25_27<-ifelse(nda2.0.1.edit$irma_dsm5_6_num >= nda2.0.1.edit$irma_dsm5_22_num & nda2.0.1.edit$irma_dsm5_6_num >= nda2.0.1.edit$irma_dsm5_25_num & nda2.0.1.edit$irma_dsm5_6_num >= nda2.0.1.edit$irma_dsm5_27_num, nda2.0.1.edit$irma_dsm5_6_num,
ifelse(nda2.0.1.edit$irma_dsm5_22_num >= nda2.0.1.edit$irma_dsm5_6_num & nda2.0.1.edit$irma_dsm5_22_num >= nda2.0.1.edit$irma_dsm5_25_num & nda2.0.1.edit$irma_dsm5_22_num >= nda2.0.1.edit$irma_dsm5_27_num, nda2.0.1.edit$irma_dsm5_22_num,
ifelse(nda2.0.1.edit$irma_dsm5_25_num >= nda2.0.1.edit$irma_dsm5_22_num & nda2.0.1.edit$irma_dsm5_25_num >= nda2.0.1.edit$irma_dsm5_6_num & nda2.0.1.edit$irma_dsm5_25_num >= nda2.0.1.edit$irma_dsm5_27_num, nda2.0.1.edit$irma_dsm5_25_num,
ifelse(nda2.0.1.edit$irma_dsm5_27_num >= nda2.0.1.edit$irma_dsm5_22_num & nda2.0.1.edit$irma_dsm5_27_num >= nda2.0.1.edit$irma_dsm5_25_num & nda2.0.1.edit$irma_dsm5_27_num >= nda2.0.1.edit$irma_dsm5_6_num, nda2.0.1.edit$irma_dsm5_27_num, nda2.0.1.edit$irma_dsm5_27_num))))

nda2.0.1.edit$irma_dsm5_20_26 <- ifelse(nda2.0.1.edit$irma_dsm5_20_num >= nda2.0.1.edit$irma_dsm5_26_num, nda2.0.1.edit$irma_dsm5_20_num, nda2.0.1.edit$irma_dsm5_26_num)

nda2.0.1.edit$irma_dsm5_28_29 <- ifelse(nda2.0.1.edit$irma_dsm5_28_num >= nda2.0.1.edit$irma_dsm5_29_num, nda2.0.1.edit$irma_dsm5_28_num, nda2.0.1.edit$irma_dsm5_29_num)

nda2.0.1.edit$irma_dsm5_30_31 <- ifelse(nda2.0.1.edit$irma_dsm5_30_num >= nda2.0.1.edit$irma_dsm5_31_num, nda2.0.1.edit$irma_dsm5_30_num, nda2.0.1.edit$irma_dsm5_31_num)

### INTRUSION (Cluster B) ***

nda2.0.1.edit$PTS_Intrusion <- nda2.0.1.edit$irma_dsm5_18_num + nda2.0.1.edit$irma_dsm5_10_num + nda2.0.1.edit$irma_dsm5_5_num + nda2.0.1.edit$irma_dsm5_11_num + nda2.0.1.edit$irma_dsm5_14_num

### AVOIDANCE  (Cluster C) ***

nda2.0.1.edit$PTS_Avoidance <- nda2.0.1.edit$irma_dsm5_3_num + nda2.0.1.edit$irma_dsm5_13_num

### NEGATIVE ALTERNATIONS IN COGNITION AND MOOD  (Cluster D) ***

nda2.0.1.edit$PTS_NegAltCogMood <- nda2.0.1.edit$irma_dsm5_23_num + nda2.0.1.edit$irma_dsm5_7_num + nda2.0.1.edit$irma_dsm5_17_num + nda2.0.1.edit$irma_dsm5_12_num + nda2.0.1.edit$irma_dsm5_2_9_16 + nda2.0.1.edit$irma_dsm5_15_19 + nda2.0.1.edit$irma_dsm5_6_22_25_27

### HYPERAROUSAL (Cluster E) ***

nda2.0.1.edit$PTS_Hyperarousal <- nda2.0.1.edit$irma_dsm5_4_num + nda2.0.1.edit$irma_dsm5_20_26 + nda2.0.1.edit$irma_dsm5_1_num + nda2.0.1.edit$irma_dsm5_24_num + nda2.0.1.edit$irma_dsm5_8_num + nda2.0.1.edit$irma_dsm5_21_num

### DISSOCIATIVE SYMPTOMS ****

nda2.0.1.edit$PTS_Dissociative <- nda2.0.1.edit$irma_dsm5_28_29 + nda2.0.1.edit$irma_dsm5_30_31

### PTS RI DSM5 TOTAL ****

nda2.0.1.edit$PTS_Total <- nda2.0.1.edit$PTS_Intrusion + nda2.0.1.edit$PTS_Avoidance + nda2.0.1.edit$PTS_NegAltCogMood + nda2.0.1.edit$PTS_Hyperarousal

#nda2.0.1.edit$PTS_Total <- nda2.0.1.edit$PTS_Total %>% dplyr::na_if(78) #remove severe outlier
nda2.0.1.edit$wins_PTS_Total <- Winsorize(nda2.0.1.edit$PTS_Total, probs = c(0.025, 0.975), na.rm = TRUE) #winsorize


############
############
#####################################################
##                                                ###
##                                                ###
##      Define IRMA-RELATED MEDIA EXPOSURE        ###
##                                                ###
#####################################################

### Total Irma-related TV exposure (before + during + after) ***

nda2.0.1.edit$Total_IrmaTV <- nda2.0.1.edit$irma_meqc_2_num + nda2.0.1.edit$irma_meqc_3_num + nda2.0.1.edit$irma_meqc_4_num

### Total Irma-related Internet news exposure (before + during + after) ***

nda2.0.1.edit$Total_IrmaInternet <- nda2.0.1.edit$irma_meqc_7_num + nda2.0.1.edit$irma_meqc_8_num + nda2.0.1.edit$irma_meqc_9_num

### Total Irma-related social media activity (before + during + after) ***

nda2.0.1.edit$Total_IrmaSocialMedia <- nda2.0.1.edit$irma_meqc_12_num + nda2.0.1.edit$irma_meqc_13_num + nda2.0.1.edit$irma_meqc_14_num

### Total Irma-related media exposure (Before Irma)  ***

nda2.0.1.edit$Total_IrmaMedia_Before <- nda2.0.1.edit$irma_meqc_2_num + nda2.0.1.edit$irma_meqc_7_num + nda2.0.1.edit$irma_meqc_12_num

### Total Irma-related media exposure (During Irma)  ***

nda2.0.1.edit$Total_IrmaMedia_During <- nda2.0.1.edit$irma_meqc_3_num + nda2.0.1.edit$irma_meqc_8_num + nda2.0.1.edit$irma_meqc_13_num

### Total Irma-related media exposure  (After Irma) ***

nda2.0.1.edit$Total_IrmaMedia_After <- nda2.0.1.edit$irma_meqc_4_num + nda2.0.1.edit$irma_meqc_9_num + nda2.0.1.edit$irma_meqc_14_num
#nda2.0.1.edit$logTotal_IrmaMedia_Before<-log1p(nda2.0.1.edit$Total_IrmaMedia_Before)
nda2.0.1.edit$wins_Total_IrmaMedia_Before<-Winsorize(nda2.0.1.edit$Total_IrmaMedia_Before, na.rm = TRUE)

### Total Irma-related media exposure (total TV + total Internet + Total Social Media)   ***

nda2.0.1.edit$Irma_MedT <- nda2.0.1.edit$Total_IrmaMedia_Before + nda2.0.1.edit$Total_IrmaMedia_During + nda2.0.1.edit$Total_IrmaMedia_After

#####################################################
###### Define Objective Exposure Variables ##########
######                                     ##########
#####################################################

###Objective Exposure During Irma ***

nda2.0.1.edit$Exp_T_D <- nda2.0.1.edit$irma_hurtep_d2_num + nda2.0.1.edit$irma_hurtep_d3_num + nda2.0.1.edit$irma_hurtep_d4_num + nda2.0.1.edit$irma_hurtep_d6_num + nda2.0.1.edit$irma_hurtep_d7_num + nda2.0.1.edit$irma_hurtep_d8_num + nda2.0.1.edit$irma_hurtep_d9_num

###Objective Exposure After Irma ***

nda2.0.1.edit$Exp_T_A <- nda2.0.1.edit$irma_hurtep_a1_num + nda2.0.1.edit$irma_hurtep_a2_num + nda2.0.1.edit$irma_hurtep_a3_num + nda2.0.1.edit$irma_hurtep_a4_num + nda2.0.1.edit$irma_hurtep_a5_num + nda2.0.1.edit$irma_hurtep_a6_num + nda2.0.1.edit$irma_hurtep_a7_num + nda2.0.1.edit$irma_hurtep_a8_num + nda2.0.1.edit$irma_hurtep_a9_num + nda2.0.1.edit$irma_hurtep_a10_num + nda2.0.1.edit$irma_hurtep_a11_num + nda2.0.1.edit$irma_hurtep_a12_num + nda2.0.1.edit$irma_hurtep_a13_num + nda2.0.1.edit$irma_hurtep_a14_num

### Total Objective Exposure (Child Evacuation + Objective Exposure During Irma + Objective Exposure After Irma)

nda2.0.1.edit$ObjExp <- nda2.0.1.edit$irma_hurtep_b1a_num + nda2.0.1.edit$Exp_T_D + nda2.0.1.edit$Exp_T_A
nda2.0.1.edit$wins_ObjExp<-Winsorize(nda2.0.1.edit$ObjExp, probs = c(0.025, 0.975), na.rm = TRUE)


#impute missing ksads data and other missing demographics

###Multiple Imputation###
######
if (!('data.table' %in% installed.packages()[,"Package"]))  install.packages('data.table')
if (!('mice' %in% installed.packages()[,"Package"]))  install.packages('mice')
library(data.table)
library(mice)

nda2.0.1.edit$ksads_ptsd_total[ nda2.0.1.edit$ksads_ptsd_total == ""] = NA

dat_nms = c("src_subject_id", "abcd_site", "age", "female", "race_ethnicity", "high.educ", "married", "household.income", "ksads_ptsd_total", "cbcl_scr_syn_anxdep_r")
if (sum(as.numeric(dat_nms %in% names(nda2.0.1.edit))) != length(dat_nms)) print("Error: missing core demographics. Add those first")
dat = data.table(nda2.0.1.edit[,dat_nms])
for (m in dat_nms) print(paste("number missing for ",m,": ",sum(is.na(nda2.0.1.edit[[m]]) | (nda2.0.1.edit[m] == "")),sep=""))

# Number of multiple imputed datasets & maximum number of iterations
n.imp = 5
n.iter = 5

var.ls <- c("src_subject_id", "age", "female", "race_ethnicity", "household.income", "high.educ","married", "ksads_ptsd_total", "cbcl_scr_syn_anxdep_r")
dat0 <- dat[, var.ls, with = FALSE ]
dat0[, table(age, useNA = "if") ]
dat0[, table(female, useNA = "if") ]
dat0[, table(race_ethnicity, useNA = "if") ]
dat0[, table(household.income, useNA = "if") ]
dat0[, table(high.educ, useNA = "if") ]
dat0[, table(married, useNA = "if") ]
dat0[, table(ksads_ptsd_total, useNA = "if") ]
dat0[, table(cbcl_scr_syn_anxdep_r, useNA = "if") ]

ini <- mice( dat0, m = 1, maxit = 0 )
meth = ini$meth

meth["female"]     <- "logreg"
meth["married"] <- "logreg"
meth["race_ethnicity"]   <- "polyreg"
meth["household.income"]      <- "polyreg"
meth["high.educ"]  <- "polyreg"
meth["ksads_ptsd_total"]  <- "pmm"
meth["cbcl_scr_syn_anxdep_r"]  <- "pmm"

pred = ini$pred

# Excluding variables from the imputation models
pred[, c("src_subject_id") ] <- 0
pred

# Specifying parameters for the imputation
post <- mice( dat0, meth = meth, pred = pred, seed = 111,
              m = 1, maxit = 0)$post

dat.imp <- mice( dat0, meth = meth, pred = pred, post = post,
                 seed = 1111,
                 m = n.imp, maxit = n.iter)
rm(dat0)

# get one imputed dataset out
completedData <- complete(dat.imp,1)
colnames(completedData)<-c("src_subject_id.imp", "age.imp", "female.imp", "race_ethnicity.imp", "household.income.imp", "high.educ.imp", "married.imp", "ksads_ptsd_total.imp", "cbcl_scr_syn_anxdep_r")
ksads_ptsd_total.imp<-completedData$ksads_ptsd_total.imp
nda2.0.1.edit<-data.frame(nda2.0.1.edit, ksads_ptsd_total.imp)
####

tapply(nda2.0.1.edit$female.imp, nda2.0.1.edit$abcd_site, summary)
tapply(nda2.0.1.edit$race_ethnicity.imp, nda2.0.1.edit$abcd_site, summary)
tapply(nda2.0.1.edit$household.income.imp, nda2.0.1.edit$abcd_site, summary)
tapply(nda2.0.1.edit$high.educ.imp, nda2.0.1.edit$abcd_site, summary)
tapply(nda2.0.1.edit$married.imp, nda2.0.1.edit$abcd_site, summary)

nda2.0.1.edit %>% count(nda2.0.1.edit$irma_meqc_2) 
nda2.0.1.edit %>% count(nda2.0.1.edit$irma_meqc_7) 
nda2.0.1.edit %>% count(nda2.0.1.edit$irma_meqc_12)

nda2.0.1.edit$wins_ksads_ptsd_total.imp<-Winsorize(nda2.0.1.edit$ksads_ptsd_total.imp, probs = c(0.025, 0.975), na.rm = TRUE) #winsorize

################
#Begin Analysis#
################

#linear model asks whether ksads associated with PTS Total
lm1<-lm(formula = wins_PTS_Total ~ wins_ksads_ptsd_total.imp + cbcl_scr_syn_anxdep_r + age + female + race_ethnicity + high.educ + married.imp + household.income.imp, data = nda2.0.1.edit)
summary(lm1)
confint.default(lm1, method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

#linear mixed effects model asks whether cbcl anxiety associated with PTS Total
lm2<-lm(formula = wins_PTS_Total ~ cbcl_scr_syn_anxdep_r + age + female + race_ethnicity + high.educ + married.imp + household.income.imp, data = nda2.0.1.edit)
summary(lm2)
confint.default(lm2, method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

####
#nda2.0.1.edit$abcd_site site03 FIU
#nda2.0.1.edit$abcd_site site05 MUSC
#nda2.0.1.edit$abcd_site site10 UCSD
#nda2.0.1.edit$abcd_site site11 UF

nda2.0.1.edit$Site_IrmaState_vs_SanDiego <- as.numeric(ifelse(nda2.0.1.edit$abcd_site == "site03" | nda2.0.1.edit$abcd_site == "site05" | nda2.0.1.edit$abcd_site == "site11", 1, 0))
nda2.0.1.edit$Site_Florida_vs_Others <- as.numeric(ifelse(nda2.0.1.edit$abcd_site == "site03" | nda2.0.1.edit$abcd_site == "site11", 1, 0))
nda2.0.1.edit$Site_FIU_vs_Others <- as.numeric(ifelse(nda2.0.1.edit$abcd_site == "site03", 1, 0))

##### Plot Data for Objective Exposure Figure #######

library(car)
library(ggplot2)
PTS_fiu<-subset(nda2.0.1.edit, nda2.0.1.edit$Site_FIU_vs_Others == 1)

#without anxiety and trauma covariates

lm3 = rlm(formula = wins_PTS_Total ~ ObjExp + age + female + race_ethnicity + high.educ + married + household.income.imp, na.action = na.omit, data = PTS_fiu)
confint.default(lm3,method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)
summary(lm3)
summ(lm3,part.corr=TRUE) #run as lm to double check manual calculation

#scale to get standardized beta
lm3 = rlm(formula = scale(wins_PTS_Total) ~ scale(ObjExp) + age + female + race_ethnicity + high.educ + married + household.income.imp, na.action = na.omit, data = PTS_fiu)
confint.default(lm3,method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

#add anxiety and trauma
lm3 = rlm(formula = wins_PTS_Total ~ ObjExp + cbcl_scr_syn_anxdep_r + wins_ksads_ptsd_total.imp + age + female + race_ethnicity + high.educ + married + household.income.imp, na.action = na.omit, data = PTS_fiu)
confint.default(lm3,method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

# assess all exposed sites
PTS_all_hurricane<-subset(nda2.0.1.edit, nda2.0.1.edit$Site_IrmaState_vs_SanDiego == 1)
lm3 = rlm(formula = wins_PTS_Total ~ ObjExp + age + female + race_ethnicity + high.educ + married + household.income.imp, na.action = na.omit, data = PTS_all_hurricane)
confint.default(lm3, method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

#add anxiety and trauma
lm3 = rlm(formula = wins_PTS_Total ~ ObjExp + cbcl_scr_syn_anxdep_r + wins_ksads_ptsd_total.imp + age + female + race_ethnicity + high.educ + married + household.income.imp, na.action = na.omit, data = PTS_all_hurricane)
confint.default(lm3, method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

#plot data for all exposed sites, without anxiety and trauma

dat<-data.frame(avPlots(lm3)$"ObjExp")
colnames(dat)<-c("Predictor","Outcome")
min.out<-abs(min(dat$Outcome))
dat$Outcome=dat$Outcome+min.out
min.pred<-abs(min(dat$Predictor))
dat$Predictor=dat$Predictor+min.pred

newmod<-lm(dat$Outcome~dat$Predictor)
plot.predict <- cbind(dat, predict(newmod, interval = 'confidence')) #adds confidence interval values and fitted values for plotting line
 
theme_set(theme_light(base_size = 20))
p <- ggplot(plot.predict, aes(x=Predictor, y=Outcome)) +
geom_point(shape=21, fill = "light blue", col = "grey", size=2, stroke = 1) +
geom_line(aes(Predictor, fit), col = "grey", size=1) +
geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.1) + #change alpha to make shading darker or lighter
xlab('Irma Exposure | Covariates') + ylab('PTS Symptoms | Covariates') + #xlim(0,13) +
#ggtitle(expression(paste("The title (", italic(beta), ") formatted in case you need to add interesting text"))) +
ggtitle("Irma Exposure Predicts Post-Irma PTS Symptoms \n Among Hurricane Exposed Youth") +
theme(panel.border = element_blank(),
panel.grid.major = element_blank(),
axis.line = element_line(size = 1, linetype = "solid", colour = "grey"),
axis.text = element_text(size = 20, colour = "black"), plot.title = element_text(hjust = .5),) #change hjust to -.5 if you need to center the title
 
p

tiff("exposure_scaled.tiff", units = 'in', width = 10, height = 6, res = 600, compression = "lzw")
p
dev.off()

####
##### End Plot Data ######


##### Plot Data for Media Exposure #######

#moderator not entered, not controlling for anxiety and trauma

lm4<-rlm(formula = wins_PTS_Total ~ Total_IrmaMedia_Before + Site_IrmaState_vs_SanDiego + age + female + race_ethnicity + high.educ 
+ married.imp + household.income.imp, data = nda2.0.1.edit)
confint.default(lm4, method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

#moderator not entered, controlling for anxiety and trauma

lm4<-rlm(formula = wins_PTS_Total ~ Total_IrmaMedia_Before + Site_IrmaState_vs_SanDiego + cbcl_scr_syn_anxdep_r + wins_ksads_ptsd_total.imp + age + female + race_ethnicity + high.educ 
+ married.imp + household.income.imp, data = nda2.0.1.edit)
confint.default(lm4, method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

##moderator entered, controlling for anxiety and trauma

lm4<-rlm(formula = wins_PTS_Total ~ Total_IrmaMedia_Before*Site_IrmaState_vs_SanDiego 
+ cbcl_scr_syn_anxdep_r + wins_ksads_ptsd_total.imp + age + female + race_ethnicity + high.educ 
+ married.imp + household.income.imp, data = nda2.0.1.edit)
confint.default(lm4, method = "boot", use.u = TRUE, type = "parametric", nsim = 10000, level=0.95)

#plot the original model
plot.frame<-lm4$model

plot.frame.new<-data.frame(plot.frame[1:2], as.factor(plot.frame[,3]))

dat<-data.frame(avPlots(lm4)$"Total_IrmaMedia_Before", as.factor(plot.frame[,3]))
colnames(dat)<-c("Predictor","Outcome", "SiteVariable")
min.out<-abs(min(dat$Outcome))
dat$Outcome=dat$Outcome+min.out
min.pred<-abs(min(dat$Predictor))
dat$Predictor=dat$Predictor+min.pred
newmod<-lm(dat$Outcome~dat$Predictor)
plot.predict <- cbind(dat, predict(newmod, interval = 'confidence')) #adds confidence interval values and fitted values for plotting line
 
theme_set(theme_light(base_size = 20))
p <- ggplot(dat, aes(x = Predictor, y = Outcome, group = SiteVariable)) + 
geom_smooth(aes(color=SiteVariable, linetype = SiteVariable), method = "lm", alpha =  .2, show.legend = FALSE) +
geom_point(aes(fill=SiteVariable), shape = 21, size=2, stroke = .5, col = "grey") +
scale_fill_manual(values=c("medium purple", "light blue"), name = "Location", labels  = c("Southern California", "Irma-Impacted State")) +
scale_color_manual(values=c("medium purple", "light blue")) + 
xlab('Pre-Storm Irma-related Media Exposure | Covariates') + ylab('PTS Symptoms | Covariates') + 
ggtitle("Media Exposure Predicts PTS Symptoms Among Children Near and Far") + 
theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
    axis.line = element_line(size = 1, linetype = "solid", colour = "grey"),
    axis.text = element_text(size = 18, colour = "black"), 
    plot.title = element_text(hjust = 0),) #change hjust to -.5 if you need to center the title

p
tiff("media_scaled.tiff", units = 'in', width = 12, height = 6, res = 600, compression = "lzw")
p
dev.off()

##### End Plot Data for Media #######