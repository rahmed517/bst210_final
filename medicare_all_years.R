#########################
# Title: BST 210 Project
# Purpose: Year over year Medicare analysis
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# LOAD DATA ---------------------------------------------------------------

medicare <- readRDS("data/medicare_2007_2018.rds")



# OUTCOMES ----------------------------------------------------------------

# Check outcome variables over time

# emergency_department_visits_per_1000_beneficiaries
medicare %>% 
  ggplot(aes(x = emergency_department_visits_per_1000_beneficiaries)) +
  geom_histogram(color = "black") +
  theme_bw() +
  facet_wrap(year~.)
# about the same every year

# Look at those who expanded in 2014
medicare %>% 
  filter(expansion_status == 1) %>% 
  ggplot(aes(x = emergency_department_visits_per_1000_beneficiaries)) +
  geom_histogram(color = "black") +
  theme_bw() +
  facet_wrap(year~.)

# fqhc/rhc_visits_per_1000_beneficiaries
medicare %>% 
  ggplot(aes(x = `fqhc/rhc_visits_per_1000_beneficiaries`)) +
  geom_histogram(color = "black") +
  theme_bw() +
  facet_wrap(year~.)

# Looks like this decreases over time

# hospital_readmission_rate

medicare %>% 
  ggplot(aes(x = hospital_readmission_rate)) +
  geom_histogram(color = "black") +
  theme_bw() +
  facet_wrap(year~.)
# Looks about the same every year



# BUILD MODELS ------------------------------------------------------------

# Build same models as before but include year as a covariate
# Fit overdispersed 
mod.nb_ed <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                            expansion_status + percent_eligible_for_medicaid + year + 
                            percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
summary(mod.nb_ed)



mod.nb <- MASS::glm.nb(`fqhc/rhc_visits_per_1000_beneficiaries` ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
summary(mod.nb)


# MODELS 2018 -------------------------------------------------------------

mod.nb_ed_2018 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                            expansion_status + percent_eligible_for_medicaid + year + 
                            percent_eligible_for_medicaid_2, data = na.omit(medicare[medicare$year == 2018, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
summary(mod.nb_ed)



mod.nb_2018 <- MASS::glm.nb(`fqhc/rhc_visits_per_1000_beneficiaries` ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[medicare$year == 2018, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
summary(mod.nb)
