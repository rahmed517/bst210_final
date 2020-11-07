#########################
# Title: BST 210 Project
# Purpose: Clean demographics data (get source link)
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)

setwd("~/Desktop/bst210_final/")
# LOAD DATA ---------------------------------------------------------------


# Pull in 2018 demographic data for states
state_demo <- readxl::read_excel("../../Documents/Medicaid/State County All Table 2018.xlsx", sheet = "State_county 2018", skip = 1)
# snake case field names
names(state_demo) <- tolower(str_replace_all(names(state_demo), " ", "_")) 


# CLEAN ROWS --------------------------------------------------------------

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

state_demo <- state_demo %>% 
  mutate_all(clean_nums)


state_demo <- state_demo %>% 
  mutate_at(vars(`beneficiaries_with_part_a_and_part_b`:`pqi16_lower_extremity_amputation_admission_rate_(age_75+)`), as.numeric)


# SUBSET COLUMNS ----------------------------------------------------------
state_demo <- state_demo[, c("state", "county", "ffs_beneficiaries",  "ma_beneficiaries" , "ma_participation_rate", "average_age" , "percent_female", "percent_male" , "percent_non-hispanic_white", "percent_african_american"  , "percent_hispanic", "percent_other/unknown", "percent_eligible_for_medicaid", "number_of_acute_hospital_readmissions" ,"hospital_readmission_rate", "emergency_department_visits", "emergency_department_visits_per_1000_beneficiaries", "%_of_beneficiaries_using_tests", "%_of_beneficiaries_using_imaging" , "%_of_beneficiaries_using_dme", "fqhc/rhc_visits_per_1000_beneficiaries", "%_of_beneficiaries_using_part_b_drugs")]



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


state_demo <- state_demo %>% 
  left_join(exp, by = "state")




# EXPLORATORY -------------------------------------------------------------

# Is there more medicaid eligibile in medicaid expansion? 
state_demo %>% 
  filter(!county %in% c("NATIONAL TOTAL", "STATE TOTAL") & !is.na(expansion_status)) %>% 
  group_by(expansion_status) %>% 
  summarize(avg_mcd_elig = mean(percent_eligible_for_medicaid, na.rm = T),
            cnt_counties = n())

state_demo$exp <- ifelse(state_demo$expansion_status == "4", 0, 1)
state_demo$exp <- as.factor(state_demo$exp)

# This model is pretty significant
mod <- lm(emergency_department_visits_per_1000_beneficiaries ~ exp + percent_eligible_for_medicaid, data = state_demo)
AIC(mod)  

# This model is pretty significant
mod <- lm(hospital_readmission_rate ~ exp + percent_eligible_for_medicaid, data = state_demo)
AIC(mod)

summary(lm(hospital_readmission_rate ~ exp + percent_eligible_for_medicaid + `percent_non-hispanic_white` + `percent_african_american` + `percent_hispanic`, data = state_demo))
AIC(lm(hospital_readmission_rate ~ exp + percent_eligible_for_medicaid + `percent_non-hispanic_white` + `percent_african_american` + `percent_hispanic`, data = state_demo))


