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

medicare <- read_csv("data/medicare_2007_2018.csv")

# write_csv(medicare, "data/medicare_2007_2018.csv")

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

# Emergency Department Visits per 1,000 beneficiaries
# Poisson model:
mod.pois_ed <- glm(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                     expansion_status + percent_eligible_for_medicaid + year + 
                     percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2), family = poisson())
summary(mod.pois_ed)

# Check overdispersion:
deviance(mod.pois_ed)/mod.pois_ed$df.residual # 20.99868
pearson.stat_ed <- sum((na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status","emergency_department_visits_per_1000_beneficiaries", "year")])$emergency_department_visits_per_1000_beneficiaries - fitted(mod.pois_ed))^2/fitted(mod.pois_ed))
pearson.stat_ed/mod.pois_ed$df.residual # 20.0565


# Since these are greater than 1, we fit a negative binomial version:
mod.nb_ed <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                            expansion_status + percent_eligible_for_medicaid + year + 
                            percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
summary(mod.nb_ed)

# Check assumptions of negative binomial
# not a lot of 0s, overdispersion
# conditional vs unconditional dispersion (check these, Anjali will get back to us)



# FQHC/RHC Visits per 1,000 beneficiaries

# Poisson model:  

mod.pois_fqhc <-  glm(`fqhc/rhc_visits_per_1000_beneficiaries` ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2), family = poisson())
summary(mod.pois_fqhc)

# Check overdispersion:
deviance(mod.pois_fqhc)/mod.pois_fqhc$df.residual # 1142.826
pearson.stat_fqhc <- sum((na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries")])$`fqhc/rhc_visits_per_1000_beneficiaries` - fitted(mod.pois_fqhc))^2/fitted(mod.pois_fqhc))
pearson.stat_fqhc/mod.pois_fqhc$df.residual # 1239.304


# Since these are greater than 1, we fit a negative binomial version:

mod.nb_fqhc <- MASS::glm.nb(`fqhc/rhc_visits_per_1000_beneficiaries` ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
summary(mod.nb_fqhc)

# Adding outpatient visits per 1000 beneficiaries
mod.pois_op <-  glm(op_visits_per_1000_beneficiaries ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2), family = poisson())
summary(mod.pois_op)

# Check overdispersion
deviance(mod.pois_op)/mod.pois_op$df.residual # 611.44
pearson.stat_op <- sum((na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")])$op_visits_per_1000_beneficiaries - fitted(mod.pois_op))^2/fitted(mod.pois_op))
pearson.stat_op/mod.pois_op$df.residual # 629.5251

# Since these are greater than 1, we fit a negative binomial version:
mod.nb_op <-  MASS::glm.nb(op_visits_per_1000_beneficiaries ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))
summary(mod.nb_op)


# MODELS 2018 (NB only) -------------------------------------------------------------

mod.nb_ed_2018 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                            expansion_status + percent_eligible_for_medicaid + year + 
                            percent_eligible_for_medicaid_2, data = na.omit(medicare[medicare$year == 2018, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
summary(mod.nb_ed)



mod.nb_2018 <- MASS::glm.nb(`fqhc/rhc_visits_per_1000_beneficiaries` ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[medicare$year == 2018, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
summary(mod.nb_2018)



# EVALUATE MISSINGNESS ----------------------------------------------------
medicare %>% map(function(x) sum(is.na(x)))

# Roughly what time of missing data do you think exists? How did you arrive to this? Explain the steps you took to discover whatever you have learned about missing data in your project, and whether these missing data require attention of some sort.

# First checked how many missing values we have in each column
medicare %>% map(function(x) sum(is.na(x)))
# I think we have missing at random - it seems like some sta
medicare$number_na <- apply(medicare, 1, function(x) sum(is.na(x) == T))

medicare %>% 
  group_by(state, county) %>% 
  summarize(sum_na = sum(number_na, na.rm = T))

# Some states have more missing this than others (some have none) So I think we can say some counties report all metrics while others don't

# b.What steps might you take to accommodate missing data in your project?  Please explain the specific steps, and if you cannot, at least explain the thought process –not everyone is able to viably manage missing data in a study in a strictly formal sense, but you should be able to explain what you will do to address what you believe to be the situation, and why you would take the steps you intend to.

# For the variables we are focused on, we have a very small percentage of missingness, so we will do a complete cases analysis.
