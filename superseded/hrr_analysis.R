#########################
# Title: 
# Purpose: 
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)


# FUNCTIONS ---------------------------------------------------------------

# Define a function to get coefficient relevant to different expansion statuses
get_coeff <- function(model, status) {
  model %>% 
    summary() %>% # Get the summary of the model
    coef() %>% # Get coefficient estimates
    data.frame() %>% # convert matrix to data frame
    filter(str_detect(row.names(.), paste0("status", status))) %>%  #extract coefficient pertaining to given status
    return()
}


# LOAD DATA ---------------------------------------------------------------

# Create a dataset with all years in it
sheets <- openxlsx::getSheetNames("data/HRR_pqi.xlsx")

hrr <- sheets %>% 
  map(function(x) {tmp <- readxl::read_excel("data/HRR_pqi.xlsx", x, skip = 1, col_types = "text"); tmp$year <- as.numeric(str_extract(x, "[0-9]{4}")); return(tmp)})


hrr <- map_df(hrr, bind_rows)

# snake case field names
names(hrr) <- tolower(str_replace_all(names(hrr), " ", "_")) 



# CLEAN DATA --------------------------------------------------------------


clean_nums_sing <- function(x) {
  tmp <- x
  # Clean percentages 
  if(!is.na(x)) {
    if(str_detect(x, "\\%")) {
      tmp <- str_replace(tmp, "\\%", "")
      tmp <- as.numeric(tmp)/100.0
      tmp <- as.character(tmp)
    }
    
    if(x == "*") {
      tmp <- str_replace_all(tmp, "\\*", "")
    }
  }
  return(tmp)
}

# function to modify columns
clean_nums <- function(col) {
  return(sapply(col, clean_nums_sing))
}

hrr <- hrr %>% 
  mutate_all(clean_nums)


hrr <- hrr %>% 
  mutate_at(vars(`beneficiaries_with_part_a_and_part_b`:year), as.numeric)

# Remove national line
hrr <- hrr %>% 
  filter(hrr != "National")

# Get state
hrr$state <- str_sub(hrr$hrr, 1,2)

# ADD ON EXPANSION STATUS -------------------------------------------------

exp <- data.frame(state = state.abb, stringsAsFactors = F)

# KY, NV, CO, OR, NM, WV, AR, RI, AZ, MD, MA, ND, OH, IA, IL, VT, HI, NY, DE - 1/2014 Expansion states
exp_1_2014 <- c("KY", "NV", "CO", "OR", "NM", "WV", "AR", "RI", "AZ", "MD", "MA", "ND", "OH", "IA", "IL", "VT", "HI", "NY", "DE") 
# WA, CA, NJ, MN, DC, CT - Early expansion - (2010- 2013)
early_exp <- c("WA", "CA", "NJ", "MN", "DC", "CT" )
# NH, IN, MI, PA,AK,MT,LA - Late Expansion States (after Jan. 2014)
late_exp <- c("NH", "IN", "MI", "PA","AK","MT","LA" )

exp <- exp %>% 
  mutate(
    expansion_status = ifelse(state %in% early_exp, "1", ifelse(state %in% exp_1_2014, "2", ifelse(state %in% late_exp, "3", "4")))
  )


hrr <- hrr %>% 
  left_join(exp, by = "state")


write_csv(hrr, "data/hrr_all_years.csv")

# EXPLORE PQIS ------------------------------------------------------------

# [226] "pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_40-64)"                 
# [227] "pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_65-74)"                 
# [228] "pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_75+)"

hrr %>% 
  filter(!is.na(expansion_status)) %>% 
  select(year, hrr, state, expansion_status, `pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_40-64)`, `pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_65-74)`, `pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_75+)`) %>% 
  gather(age, pqi05, -year, -hrr, -state, -expansion_status) %>% 
  mutate(
    age = str_sub(age, -10)
  ) %>% 
  ggplot(aes(x = pqi05)) +
  geom_histogram(color = "black") +
  facet_grid(age~expansion_status) +
  theme_bw()

#highest rates

# [229] "pqi07_hypertension_admission_rate_(age_<_65)"                                    
# [230] "pqi07_hypertension_admission_rate_(age_65-74)"                                   
# [231] "pqi07_hypertension_admission_rate_(age_75+)" 

hrr %>% 
  filter(!is.na(expansion_status)) %>% 
  select(year, hrr, state, expansion_status, `pqi07_hypertension_admission_rate_(age_<_65)`, `pqi07_hypertension_admission_rate_(age_65-74)`, `pqi07_hypertension_admission_rate_(age_75+)`) %>% 
  gather(age, pqi07, -year, -hrr, -state, -expansion_status) %>% 
  mutate(
    age = str_sub(age, -10)
  ) %>% 
  ggplot(aes(x = pqi07)) +
  geom_histogram(color = "black") +
  facet_grid(age~expansion_status) +
  theme_bw()



# FIT MODELS --------------------------------------------------------------


## A. PQI 05

# Fit a poisson model for PQI05
pqi05_data <- hrr %>% 
  filter(!is.na(expansion_status)) %>% 
  select(year, hrr, state, expansion_status, percent_female, percent_male, percent_african_american, `percent_non-hispanic_white`, percent_eligible_for_medicaid,`pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_40-64)`, `pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_65-74)`, `pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_75+)`) %>% 
  gather(age, pqi05, c("pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_40-64)", "pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_65-74)", "pqi05_copd_or_asthma_in_older_adults_admission_rate_(age_75+)")) %>% 
  mutate(age = str_extract(age, "(?<=age_).+(?=\\))"))

# Look at distribtuion of data
pqi05_data %>% 
  ggplot(aes(x = pqi05)) +
  geom_histogram(color = "black") +
  theme_bw() # slight right skew 

covs <- names(pqi05_data)[!names(pqi05_data) %in% c("pqi05", "hrr", "state")]
covs <- paste0(covs, collapse = "` + `")

mod.pois_pqi05 <- glm(eval(paste0("pqi05 ~ `", covs, "`")), data = pqi05_data, family = poisson())
summary(mod.pois_pqi05)

# Is there overdispersion? - Yes, do we need to fit a negative binomial?
deviance(mod.pois_pqi05)/mod.pois_pqi05$df.residual # 124.7012
pearson.stat_pqi05 <- sum((na.omit(pqi05_data)$pqi05 - fitted(mod.pois_pqi05))^2/fitted(mod.pois_pqi05))
pearson.stat_pqi05/mod.pois_pqi05$df.residual # 127.8269

# Fit a negative bindomial distribution
# Reason for NB - plotted histograms of outcomes and it looks somewhat normal - not a high amount of 0s or anything like that
mod.nb_pqi05_1 <- MASS::glm.nb(pqi05 ~ `expansion_status` + `percent_female` + `percent_male` + `percent_african_american` + `percent_non-hispanic_white` + `percent_eligible_for_medicaid` + `age` + offset(log(year)), data = pqi05_data)

# Add quadratic % elibible - include
mod.nb_pqi05_2 <- MASS::glm.nb(pqi05 ~ `expansion_status` + `percent_female` + `percent_male` + `percent_african_american` + `percent_non-hispanic_white` + `percent_eligible_for_medicaid` + `age` + offset(log(year)) + percent_eligible_for_medicaid_2, data = pqi05_data %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))


anova(mod.nb_pqi05_1, mod.nb_pqi05_2)

# Add percent male and % eligible interaction - include
mod.nb_pqi05_3 <- MASS::glm.nb(pqi05 ~ `expansion_status` + `percent_female` + `percent_male` + `percent_african_american` + `percent_non-hispanic_white` + `percent_eligible_for_medicaid` + `age` + offset(log(year)) + percent_eligible_for_medicaid_2 + percent_male*percent_eligible_for_medicaid, data = pqi05_data %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))

anova(mod.nb_pqi05_2, mod.nb_pqi05_3)

# Add percent male and quadratic % eligible interaction - include
mod.nb_pqi05_4 <- MASS::glm.nb(pqi05 ~ `expansion_status` + `percent_female` + `percent_male` + `percent_african_american` + `percent_non-hispanic_white` + `percent_eligible_for_medicaid` + `age` + offset(log(year)) + percent_eligible_for_medicaid_2 + percent_male*percent_eligible_for_medicaid + percent_male*percent_eligible_for_medicaid_2, data = pqi05_data %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))

anova(mod.nb_pqi05_3, mod.nb_pqi05_4)

# Add expansion_status and % eligible 
mod.nb_pqi05_5 <- MASS::glm.nb(pqi05 ~ `expansion_status` + `percent_female` + `percent_male` + `percent_african_american` + `percent_non-hispanic_white` + `percent_eligible_for_medicaid` + `age` + offset(log(year)) + percent_eligible_for_medicaid_2 + percent_male*percent_eligible_for_medicaid + percent_male*percent_eligible_for_medicaid_2 + expansion_status*percent_eligible_for_medicaid, data = pqi05_data %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))

anova(mod.nb_pqi05_4, mod.nb_pqi05_5)

# Add expansion_status and quadratic % eligible  -- include
mod.nb_pqi05_6 <- MASS::glm.nb(pqi05 ~ `expansion_status` + `percent_female` + `percent_male` + `percent_african_american` + `percent_non-hispanic_white` + `percent_eligible_for_medicaid` + `age` + offset(log(year)) + percent_eligible_for_medicaid_2 + percent_male*percent_eligible_for_medicaid + percent_male*percent_eligible_for_medicaid_2 + expansion_status*percent_eligible_for_medicaid + expansion_status*percent_eligible_for_medicaid_2, data = pqi05_data %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2))  

anova(mod.nb_pqi05_5, mod.nb_pqi05_6)

exp(get_coeff(mod.nb_pqi05_6, "2")[1,1]) # 0.9071021
exp(get_coeff(mod.nb_pqi05_6, "3")[1,1]) # 0.9331008
exp(get_coeff(mod.nb_pqi05_6, "3")[1,1]) # 0.9331008

exp(confint(mod.nb_pqi05_6)) %>% data.frame() %>% filter(str_detect(row.names(.), "expansion_status" ))
# expansion_status2                                  -0.2636503   0.06794573
# expansion_status3                                  -0.2948908   0.15699189
# expansion_status4                                  -0.1126306   0.21327159


mod.pois_pqi05 <- glm(pqi05 ~ `expansion_status` + `percent_female` + `percent_male` + `percent_african_american` + `percent_non-hispanic_white` + `percent_eligible_for_medicaid` + `age` + offset(log(year)) + percent_eligible_for_medicaid_2 + percent_male*percent_eligible_for_medicaid + percent_male*percent_eligible_for_medicaid_2 + expansion_status*percent_eligible_for_medicaid + expansion_status*percent_eligible_for_medicaid_2, data = pqi05_data %>% mutate(percent_eligible_for_medicaid_2 = percent_eligible_for_medicaid^2), family = "poisson")

# Is there overdispersion? - Yes, do we need to fit a negative binomial?
deviance(mod.pois_pqi05)/mod.pois_pqi05$df.residual # 130.9055
pearson.stat_pqi05 <- sum((na.omit(pqi05_data)$pqi05 - fitted(mod.pois_pqi05))^2/fitted(mod.pois_pqi05))
pearson.stat_pqi05/mod.pois_pqi05$df.residual # 135.0458

# interpretation of expansion categories
# 2 v 1: .2579 + .3196e*percent_eligible_for_medicaid
# Interpretation: The incidence rate ratio of COPD/asthma admissions per 100,000 beneficiaries in cat 2 as compared to category 1 for a certain percent eligible for Medicaid is  exp(.2579 + .3196*0.01) times the IRR for cat 2 vs cat 1 for 1 percent lower.

## B. PQI 07

# Fit a poisson model for PQI07
pqi07_data <- hrr %>% 
  filter(!is.na(expansion_status)) %>% 
  select(year, hrr, state, expansion_status, percent_female, percent_male, percent_african_american, `percent_non-hispanic_white`, percent_eligible_for_medicaid,`pqi07_hypertension_admission_rate_(age_<_65)`, `pqi07_hypertension_admission_rate_(age_65-74)`, `pqi07_hypertension_admission_rate_(age_75+)`) %>% 
  gather(age, pqi07, c("pqi07_hypertension_admission_rate_(age_<_65)", "pqi07_hypertension_admission_rate_(age_65-74)", "pqi07_hypertension_admission_rate_(age_75+)")) %>% 
  mutate(age = str_extract(age, "(?<=age_).+(?=\\))"))


covs <- names(pqi07_data)[!names(pqi07_data) %in% c("pqi07", "hrr", "state")]
covs <- paste0(covs, collapse = "` + `")

mod.pois_pqi07 <- glm(eval(paste0("pqi07 ~ `", covs, "`")), data = pqi07_data, family = poisson())
summary(mod.pois_pqi07)

# Is there overdispersion? - Yes, do we need to fit a negative binomial?
deviance(mod.pois_pqi07)/mod.pois_pqi07$df.residual # 24.68065
pearson.stat_pqi07 <- sum((na.omit(pqi07_data)$pqi07 - fitted(mod.pois_pqi07))^2/fitted(mod.pois_pqi07))
pearson.stat_pqi07/mod.pois_pqi07$df.residual # 25.64116


# fit a MLR just in case
mod.mlr_pqi07 <- lm(eval(paste0("sqrt(pqi07) ~ `", covs, "`")), data = pqi07_data)
summary(mod.mlr_pqi07)

