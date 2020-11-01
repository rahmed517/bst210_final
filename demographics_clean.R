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


# SUBSET DATA -------------------------------------------------------------

state_demo <- state_demo[, c(1:15)]

state_demo <- state_demo %>% 
  filter(county == "STATE TOTAL")


state_demo <- state_demo %>% 
  mutate_at(
    vars(ma_participation_rate, percent_female:percent_eligible_for_medicaid), .funs = function(x) return(as.numeric(str_replace(x, "\\%", ""))/100.0)
  )

state_demo <- state_demo %>% 
  mutate_at(
    vars(beneficiaries_with_part_a_and_part_b:ma_beneficiaries, average_age), .funs = function(x) return(as.numeric(x))
  )


state_demo <- state_demo %>% 
  select(-county, -state_and_county_fips_code)



# OUTPUT DATA -------------------------------------------------------------

write_csv(state_demo, "data/state_demo_clean_2018.csv")
