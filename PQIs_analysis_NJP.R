#########################
# Title: 210 Final Project
# Purpose: Create Regressions to model difference in health outcomes based on when states expanded
# initially intended to compare data from before expansion to after expansion, however the data does not exist before 2014.
# There is evidence of a strong linear association between the different PQIs and expansion category.
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)
library(VGAM)

setwd("~/Desktop/bst210_final/")

# LOAD DATA ---------------------------------------------------------------

# Load data
quality <- read_csv("data/quality_measures_clean_aggregate.csv")

# CLEAN AND AGG FIELDS ----------------------------------------------------

# PQI 01: Diabetes Short-Term Complications Admission Rate
# Get all fields that have "PQI 01" in them and determine if they can be aggregated.
names(quality)[str_detect(names(quality), "PQI 01")] # in name, these all look the same

# Do they overlap?
count(quality, is.na(`PQI 01: Diabetes Short-Term Complications Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Diabetes Short-Term Complications per 100,000 Enrollee-Months: Ages 18-64 [Lower rates are better]`), 
      is.na(`PQI 01: Diabetes Short-Term Complications Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Diabetes Short-Term Complications per 100,000 Enrollee Months: Ages 18-64`), is.na(`PQI 01: Diabetes Short-Term Complications Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Diabetes Short-Term Complications per 100,000 Beneficiary Months: Ages 18-64`), 
      is.na(`PQI 01: Diabetes Short-Term Complications Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Diabetes Short-Term Complications per 100,000 Beneficiary Months: Ages 18 to 64`))

# # A tibble: 5 x 5
# `is.na(\`PQI 01: Diabetes… `is.na(\`PQI 01: Diabetes… `is.na(\`PQI 01: Diabetes… `is.na(\`PQI 01: Diabete…     n
# <lgl>                      <lgl>                      <lgl>                      <lgl>          <int>
#   1 FALSE                      TRUE                       TRUE                       TRUE         25
# 2 TRUE                       FALSE                      TRUE                       TRUE           29
# 3 TRUE                       TRUE                       FALSE                      TRUE           28
# 4 TRUE                       TRUE                       TRUE                       FALSE          30
# 5 TRUE                       TRUE                       TRUE                       TRUE           146

# They do not overlap, so these fields indicate the same data:
quality$pqi_01 <- pmax(quality$`PQI 01: Diabetes Short-Term Complications Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Diabetes Short-Term Complications per 100,000 Enrollee-Months: Ages 18-64 [Lower rates are better]`, 
                       quality$`PQI 01: Diabetes Short-Term Complications Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Diabetes Short-Term Complications per 100,000 Enrollee Months: Ages 18-64`, 
                       quality$`PQI 01: Diabetes Short-Term Complications Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Diabetes Short-Term Complications per 100,000 Enrollee-Months: Ages 18-64 [Lower rates are better]`, na.rm = T)


# PQI 05: COPD or Asthma in Older Adults Admission Rate
# Get all fields that have "PQI 01" in them and determine if they can be aggregated.
names(quality)[str_detect(names(quality), "PQI 05")] # in name, these all look the same

count(quality, is.na(`PQI 05: Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults Admission Rate: Age 40 and Older__Inpatient Hospital Admissions for Chronic Obstructive Pulmonary Disease (COPD) or Asthma per 100,00 Enrollee Months: Ages 40-64`), 
      is.na(`PQI 05: Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults Admission Rate: Age 40 and Older__Inpatient Hospital Admissions for Chronic Obstructive Pulmonary Disease (COPD) or Asthma per 100,00 Beneficiary Months: Ages 40-64`), 
      is.na(`PQI 05: Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults Admission Rate: Age 40 and Older__Inpatient Hospital Admissions for Chronic Obstructive Pulmonary Disease (COPD) or Asthma per 100,000 Beneficiary Months: Ages 40 to 64`))

# A tibble: 4 x 4
# `is.na(\`PQI 05: Chronic Obstructive Pulmonary Disease… `is.na(\`PQI 05: Chronic Obstructive Pulmonary Diseas… `is.na(\`PQI 05: Chronic Obstructive Pulmonary Diseas…     n
# <lgl>                                                   <lgl>                                                  <lgl>                                                  <int>
# 1 FALSE                                                   TRUE                                                   TRUE                                                   25
# 2 TRUE                                                    FALSE                                                  TRUE                                                   25
# 3 TRUE                                                    TRUE                                                   FALSE                                                  25
# 4 TRUE                                                    TRUE                                                   TRUE                                                   183

# Aggregate
quality$pqi_05 <- pmax(quality$`PQI 05: Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults Admission Rate: Age 40 and Older__Inpatient Hospital Admissions for Chronic Obstructive Pulmonary Disease (COPD) or Asthma per 100,00 Enrollee Months: Ages 40-64`, 
                       quality$`PQI 05: Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults Admission Rate: Age 40 and Older__Inpatient Hospital Admissions for Chronic Obstructive Pulmonary Disease (COPD) or Asthma per 100,00 Beneficiary Months: Ages 40-64`, 
                       quality$`PQI 05: Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults Admission Rate: Age 40 and Older__Inpatient Hospital Admissions for Chronic Obstructive Pulmonary Disease (COPD) or Asthma per 100,000 Beneficiary Months: Ages 40 to 64`, na.rm =T)

# PQI 08: Heart Failure Admission Rate (Ages 18-64) 
names(quality)[str_detect(names(quality), "PQI 08")] # in name, these all look the same

count(quality, is.na(`PQI 08: Heart Failure Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Heart Failure per 100,000 Enrollee-Months: Ages 18-64 [Lower rates are better]`),
      is.na(`PQI 08: Heart Failure Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Heart Failure per 100,000 Enrollee Months: Ages 18-64`),
      is.na(`PQI 08: Heart Failure Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Heart Failure per 100,000 Beneficiary Months: Ages 18-64`),
      is.na(`PQI 08: Heart Failure Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Heart Failure per 100,000 Beneficiary Months: Ages 18 to 64`))

# Aggregate
quality$pqi_08 <- pmax(quality$`PQI 08: Heart Failure Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Heart Failure per 100,000 Beneficiary Months: Ages 18 to 64`, 
                       quality$`PQI 08: Heart Failure Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Heart Failure per 100,000 Enrollee Months: Ages 18-64`,
                       quality$`PQI 08: Heart Failure Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Heart Failure per 100,000 Enrollee-Months: Ages 18-64 [Lower rates are better]`,
                       quality$`PQI 08: Heart Failure Admission Rate: Age 18 and Older__Inpatient Hospital Admissions for Heart Failure per 100,000 Beneficiary Months: Ages 18-64`
                       , na.rm =T)


# PQI 15: Asthma in Younger Adults Admission Rate
count(quality, is.na(quality$`PQI 15: Asthma in Younger Adults Admission Rate: Ages 18-39__Inpatient Hospital Admissions for Asthma per 100,000 Enrollee-Months: Ages 18-39`),
      is.na(quality$`PQI 15: Asthma in Younger Adults Admission Rate: Ages 18-39__Inpatient Hospital Admissions for Asthma per 100,000 Beneficiary Months: Ages 18-39`),
      is.na(quality$`PQI 15: Asthma in Younger Adults Admission Rate: Ages 18 to 39__Inpatient Hospital Admissions for Asthma per 100,000 Beneficiary Months: Ages 18 to 39`))

# Aggregate
quality$pqi_15 <- pmax(quality$`PQI 15: Asthma in Younger Adults Admission Rate: Ages 18-39__Inpatient Hospital Admissions for Asthma per 100,000 Enrollee-Months: Ages 18-39`,
                       quality$`PQI 15: Asthma in Younger Adults Admission Rate: Ages 18-39__Inpatient Hospital Admissions for Asthma per 100,000 Beneficiary Months: Ages 18-39`,
                       quality$`PQI 15: Asthma in Younger Adults Admission Rate: Ages 18 to 39__Inpatient Hospital Admissions for Asthma per 100,000 Beneficiary Months: Ages 18 to 39`, na.rm =T)


# All Cause admissions 18-64
names(quality)[str_detect(names(quality), "admissions")] 

#Check for overlap
count(quality, is.na(quality$`Plan All-Cause Readmissions: Ages 18-64__Ratio of Observed All-Cause Readmissions to Expected Readmissions: Ages 18-64`), is.na(quality$`Plan All-Cause Readmissions: Ages 18 to 64__Ratio of Observed All-Cause Readmissions to Expected Readmissions: Ages 18 to 64`))

quality$all_cause <- pmax(quality$`Plan All-Cause Readmissions: Ages 18-64__Ratio of Observed All-Cause Readmissions to Expected Readmissions: Ages 18-64`,
                          quality$`Plan All-Cause Readmissions: Ages 18 to 64__Ratio of Observed All-Cause Readmissions to Expected Readmissions: Ages 18 to 64`, na.rm = T)

# CREATE LINEAR/ORDINAL REGRESSIONS -----------------------------------

### A. PQI 01

## i. Modeling PQI 01 with expansion status and year (MLR)

mod.lmA <- lm(pqi_01 ~ yr + expansion_status, data = quality)
summary(mod.lmA)
# 1 unit increase in expansion status is a roughly 3 unit increase in PQI 01 (p = 0.00867)
# 1 unit increase in yr results in -2.78 in PQI 01 (not sig, p = 0.236)
plot(mod.lmA) # not looking great in terms of model evaluation

### B. PQI 05

## i. Modeling PQI 05 with expansion status and year (MLR)

mod.lmB <- lm(pqi_05 ~ yr + expansion_status, data = quality)
summary(mod.lmB)
# 1 unit increase in expansion status is a roughly 24.1446 unit increase in PQI 05 (p = 1.08e-06)
# 1 unit increase in yr results in -0.2598 in PQI 05 (not sig, p = 0.964)
plot(mod.lmB)


### C. PQI 08

## i. Modeling PQI 08 with expansion status and year (MLR)

mod.lmC <- lm(pqi_08 ~ yr + expansion_status, data = quality)
summary(mod.lmC)
# 1 unit increase in expansion status is a roughly 8.173 unit increase in PQI 08 (p = 4.93e-08)
# 1 unit increase in yr results in 1.942 in PQI 08 (not sig, p = 0.131)
plot(mod.lmC)
# Definite pattern in residuals

### D. PQI 15

## i. Modeling PQI 15 with expansion status and year (MLR)

mod.lmD <- lm(pqi_15 ~ yr + expansion_status, data = quality)
summary(mod.lmD)
# 1 unit increase in expansion status is a roughly 0.9835 unit increase in PQI 15 (p = 0.0063)
# 1 unit increase in yr results in -0.1840 in PQI 15 (not sig, p =  0.6887)
plot(mod.lmC)
# Definite pattern in residuals

### E. All cause admissions 
mod.lmE <- lm(all_cause ~ yr + expansion_status, data = quality)
summary(mod.lmE)

plot(mod.lmE) # not significant 


## Model all PQIs as a predictor of expansion status
mod.multinomA <- multinom(expansion_status ~  pqi_01 + pqi_05 + pqi_08 + pqi_15, data=quality)

summary(mod.multinom)
# need to look into how to evaluate the strength of this 
exp(-0.01305062) # 0.987
# risk of being in outcome 2  with a certain level of PQI 01 is .98 times the risk of being in outcome 1 for a PQI 01 one unit lower

mod.multinomB <- multinom(expansion_status ~  all_cause, data=quality)
summary(mod.multinomB)
