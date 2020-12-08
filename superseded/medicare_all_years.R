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

# write_csv

# FUNCTIONS ---------------------------------------------------------------

# Define a function to get coefficient relevant to different expansion statuses
get_coeff <- function(model, status) {
  model %>% 
    summary() %>% # Get the summary of the model
    coef() %>% # Get coefficient estimates
    data.frame() %>% # convert matrix to data frame
    filter(str_detect(row.names(.), paste0("\\(expansion_status\\)", status))) %>%  #extract coefficient pertaining to given status
    return()
}


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


# Emergency Department Visits per 1,000 beneficiaries ---------------------

# Poisson model:
mod.pois_ed <- glm(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                     factor(expansion_status) + percent_eligible_for_medicaid + 
                     percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2), family = poisson(), offset = log(year))
summary(mod.pois_ed)

# Check overdispersion:
deviance(mod.pois_ed)/mod.pois_ed$df.residual # 21.74594
pearson.stat_ed <- sum((na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status","emergency_department_visits_per_1000_beneficiaries", "year")])$emergency_department_visits_per_1000_beneficiaries - fitted(mod.pois_ed))^2/fitted(mod.pois_ed))
pearson.stat_ed/mod.pois_ed$df.residual # 20.80951


# Since these are greater than 1, we fit a negative binomial version:
mod.nb_ed_1 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                            factor(expansion_status) + percent_eligible_for_medicaid  + offset(log(year)), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 

# exp(confint(mod.nb_ed))
# Get interpretation for the relevant covariates
# IRR of ed visits for the expansion for cat 2 vs cat 1 is exp(coef(summary(mod.nb_ed))[3,1]) 1.05 (95\% CI (1.041901, 1.058925)) holding average age, year and percent eligible for medicaid constant
# IRR of ed visits for the expansion for cat 3 vs cat 1 is exp(coef(summary(mod.nb_ed))[4,1]) 1.0346 (95\% CI (1.025334, 1.04397)) holding average age, year and percent eligible for medicaid constant
# IRR of ed visits for the expansion for cat 4 vs cat 1 is exp(coef(summary(mod.nb_ed))[5,1]) 1.0503 (95\% CI (1.042278, 1.058522)) holding average age, year and percent eligible for medicaid constant

# Check assumptions of negative binomial
# not a lot of 0s, overdispersion
# conditional vs unconditional dispersion (check these, Anjali will get back to us)

# Include Quadratic percent eligible - include
mod.nb_ed_2 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                            factor(expansion_status) + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + offset(log(year)), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 

anova(mod.nb_ed_1, mod.nb_ed_2)


# Include quadratic age - include
mod.nb_ed_3 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                              factor(expansion_status) + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + average_age_2+ offset(log(year)), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2)) 

anova(mod.nb_ed_2, mod.nb_ed_3)

## CHECK INTERACTIONS FOR THIS MODEL
# Adding interaction between expansion status and percent eligible - inlucde
mod.nb_ed_4 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                              factor(expansion_status) + percent_eligible_for_medicaid + 
                              percent_eligible_for_medicaid_2 + average_age_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2)) 

anova(mod.nb_ed_3, mod.nb_ed_4)

# Adding interaction between expansion status and quadratic percent eligible - include
mod.nb_ed_5 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                              factor(expansion_status) + percent_eligible_for_medicaid +  
                              percent_eligible_for_medicaid_2 + average_age_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2)) 

anova(mod.nb_ed_4, mod.nb_ed_5)

# look at age and expansion status
mod.nb_ed_6 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                              factor(expansion_status) + percent_eligible_for_medicaid +  
                              percent_eligible_for_medicaid_2 + average_age_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2 + average_age*factor(expansion_status), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2)) 

anova(mod.nb_ed_5, mod.nb_ed_6)

# quadratic age and expanion status
mod.nb_ed_7 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                              factor(expansion_status) + percent_eligible_for_medicaid +  
                              percent_eligible_for_medicaid_2 + average_age_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2 + average_age*factor(expansion_status) + average_age_2*factor(expansion_status), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2)) 

anova(mod.nb_ed_6, mod.nb_ed_7)



# Poisson model: ( with effect modification)
mod.pois_ed <- glm(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
                     factor(expansion_status) + percent_eligible_for_medicaid +  
                     percent_eligible_for_medicaid_2 + average_age_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2 + average_age*factor(expansion_status) + average_age_2*factor(expansion_status), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2), family = poisson())
summary(mod.pois_ed)

# Check overdispersion:
deviance(mod.pois_ed)/mod.pois_ed$df.residual # 20.91501
pearson.stat_ed <- sum((na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status","emergency_department_visits_per_1000_beneficiaries", "year")])$emergency_department_visits_per_1000_beneficiaries - fitted(mod.pois_ed))^2/fitted(mod.pois_ed))
pearson.stat_ed/mod.pois_ed$df.residual #  20.29139



# Outpatient visits per 1000 beneficiaries --------------------------------
# initial Poisson model
mod.pois_op <-  glm(op_visits_per_1000_beneficiaries ~ average_age + factor(expansion_status) +  percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + offset(log(year)), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2), family = poisson())
summary(mod.pois_op)

# Check overdispersion
deviance(mod.pois_op)/mod.pois_op$df.residual # 613.8479
pearson.stat_op <- sum((na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")])$op_visits_per_1000_beneficiaries - fitted(mod.pois_op))^2/fitted(mod.pois_op))
pearson.stat_op/mod.pois_op$df.residual # 642.9544

# Since these are greater than 1, we fit a negative binomial version:
mod.nb_op <-  MASS::glm.nb(op_visits_per_1000_beneficiaries ~ average_age + factor(expansion_status) + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + offset(log(year)), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))
summary(mod.nb_op)

## CHECK INTERACTIONS FOR THIS MODEL

# % eligible and expansion status - include
mod.nb_op_1 <-   MASS::glm.nb(op_visits_per_1000_beneficiaries ~ average_age + factor(expansion_status) + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))


anova(mod.nb_op, mod.nb_op_1)

# quadratic eligible and expansion status - include
mod.nb_op_2 <-  MASS::glm.nb(op_visits_per_1000_beneficiaries ~ average_age + factor(expansion_status) + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))

anova(mod.nb_op_1, mod.nb_op_2)

# quadratic age
mod.nb_op_3 <-  MASS::glm.nb(op_visits_per_1000_beneficiaries ~ average_age + factor(expansion_status) + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2 + average_age_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2))

anova(mod.nb_op_2, mod.nb_op_3)

# Age and expansion status
mod.nb_op_4 <-  MASS::glm.nb(op_visits_per_1000_beneficiaries ~ average_age + factor(expansion_status) + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2 + average_age_2 + average_age*factor(expansion_status), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2))

anova(mod.nb_op_3, mod.nb_op_4)

# Quadratic age and expansion status
mod.nb_op_5 <-  MASS::glm.nb(op_visits_per_1000_beneficiaries ~ average_age + factor(expansion_status) + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + offset(log(year)) + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2 + average_age_2 + average_age*factor(expansion_status) + average_age_2*factor(expansion_status), data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2, average_age_2 = average_age^2))

anova(mod.nb_op_4, mod.nb_op_5)


# Poisson with effect modification
mod.pois_op <-  glm(op_visits_per_1000_beneficiaries ~ average_age + factor(expansion_status) + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2 + average_age*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid + factor(expansion_status)*percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2), family = poisson())
summary(mod.pois_op)

# Check overdispersion
deviance(mod.pois_op)/mod.pois_op$df.residual # 563.101
pearson.stat_op <- sum((na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "op_visits_per_1000_beneficiaries", "year")])$op_visits_per_1000_beneficiaries - fitted(mod.pois_op))^2/fitted(mod.pois_op))
pearson.stat_op/mod.pois_op$df.residual # 583.8776




# INTERPRETATIONS ---------------------------------------------------------
mod_ed_final <- mod.nb_ed_7
mod_op_final <- mod.nb_op_5

summary(mod_ed_final)
exp(confint(mod_ed_final))
# Waiting for profiling to be done...
# 2.5 %       97.5 %
#   (Intercept)                                             6.591260e-03 8.222127e+04
# average_age                                               7.132740e-01 1.129796e+00
# factor(expansion_status)2                                 3.473334e-07 9.568353e+00
# factor(expansion_status)3                                 3.721223e+06 7.472602e+15
# factor(expansion_status)4                                 1.730401e-13 4.995913e-06
# percent_eligible_for_medicaid                             8.656698e+00 2.060193e+01
# percent_eligible_for_medicaid_2                           1.336649e-02 6.313177e-02
# average_age_2                                             9.989776e-01 1.002212e+00
# factor(expansion_status)2:percent_eligible_for_medicaid   1.153462e+00 2.973020e+00
# factor(expansion_status)3:percent_eligible_for_medicaid   8.673147e+01 2.528971e+02
# factor(expansion_status)4:percent_eligible_for_medicaid   5.669099e-01 1.395740e+00
# factor(expansion_status)2:percent_eligible_for_medicaid_2 2.305100e-01 1.267611e+00
# factor(expansion_status)3:percent_eligible_for_medicaid_2 3.219072e-05 2.086269e-04
# factor(expansion_status)4:percent_eligible_for_medicaid_2 8.187962e-01 4.086469e+00
# average_age:factor(expansion_status)2                     9.334267e-01 1.512043e+00
# average_age:factor(expansion_status)3                     3.501008e-01 6.393672e-01
# average_age:factor(expansion_status)4                     1.431277e+00 2.320459e+00
# factor(expansion_status)2:average_age_2                   9.971253e-01 1.000512e+00
# factor(expansion_status)3:average_age_2                   1.003196e+00 1.007447e+00
# factor(expansion_status)4:average_age_2                   9.940066e-01 9.973856e-01

#### ED VISITS
coeffs <- get_coeff(mod_ed_final, "2")
### 2 vs 1
# The IRR of ED visits in cat 2 vs cat 1 is 0.9610278 holding all other covariates constant
exp(coeffs[1,1])  # 0.001870669
# 95% CI (3.473334e-07 9.568353)

### 3 vs 1
# The IRR of ED visits in cat 3 vs cat 1 is 0.6255082 holding all other covariates constant
coeffs <- get_coeff(mod_ed_final, "3")
exp(coeffs[1,1])  # 168327295689
# 95% CI (0.5928375, 0.6599367)

### 4 vs 1
# The IRR of ED visits in cat 3 vs cat 1 is 0.944392 holding all other covariates constant
coeffs <- get_coeff(mod_ed_final, "4")
exp(coeffs[1,1])  # 9.540758e-10
# 95% CI (0.9027462, 0.9878161)

# With effects of % elibilbe
### 2 vs 1
exp(-0.03975199 +  0.63955122*0.01  + -0.80536995*0.01^2) # 0.9671158


### 3 vs 1
coeffs <- get_coeff(mod_ed_final, "3")
exp(coeffs[1,1] +  coeffs[2,1]*0.01  + coeffs[3,1]*0.01^2) # 0.6534316

### 4 vs 1
coeffs <- get_coeff(mod_ed_final, "4")
exp(coeffs[1,1] +  coeffs[2,1]*0.01  + coeffs[3,1]*0.01^2) # 0.9501715


#### OPs VISITS
coeffs <- get_coeff(mod_op_final, "2")
# IRR

## 2v1
exp(coeffs[1,1]) # 298,033,046 ????

# 3v1
coeffs <- get_coeff(mod_op_final, "3")
exp(coeffs[1,1]) # 6.931681e+13

coeffs <- get_coeff(mod_op_final, "4")
exp(coeffs[1,1]) # 341873557969


# MODELS 2018 (NB only) -------------------------------------------------------------
# 
# mod.nb_ed_2018 <- MASS::glm.nb(emergency_department_visits_per_1000_beneficiaries ~ average_age + 
#                             expansion_status + percent_eligible_for_medicaid + year + 
#                             percent_eligible_for_medicaid_2, data = na.omit(medicare[medicare$year == 2018, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "emergency_department_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
# summary(mod.nb_ed)
# 
# 
# 
# mod.nb_2018 <- MASS::glm.nb(`fqhc/rhc_visits_per_1000_beneficiaries` ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[medicare$year == 2018, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
# summary(mod.nb_2018)




# # FQHC/RHC Visits per 1,000 beneficiaries
# 
# # Poisson model:  
# 
# mod.pois_fqhc <-  glm(`fqhc/rhc_visits_per_1000_beneficiaries` ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2), family = poisson())
# summary(mod.pois_fqhc)
# 
# # Check overdispersion:
# deviance(mod.pois_fqhc)/mod.pois_fqhc$df.residual # 1142.826
# pearson.stat_fqhc <- sum((na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries")])$`fqhc/rhc_visits_per_1000_beneficiaries` - fitted(mod.pois_fqhc))^2/fitted(mod.pois_fqhc))
# pearson.stat_fqhc/mod.pois_fqhc$df.residual # 1239.304
# 
# 
# # Since these are greater than 1, we fit a negative binomial version:
# 
# mod.nb_fqhc <- MASS::glm.nb(`fqhc/rhc_visits_per_1000_beneficiaries` ~ average_age + expansion_status + year + percent_eligible_for_medicaid + percent_eligible_for_medicaid_2, data = na.omit(medicare[, c("county", "percent_eligible_for_medicaid", "average_age", "expansion_status", "fqhc/rhc_visits_per_1000_beneficiaries", "year")]) %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2)) 
# summary(mod.nb_fqhc)

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
