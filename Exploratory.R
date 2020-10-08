#########################
# Title: Explore Medicaid Health Quality Measures Data 2018
# Purpose: Create exploratory graphics to identify potential covariates
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2) 
library(scales)
library(ggthemes)

setwd("~/Documents/Medicaid")


# LOAD DATA ---------------------------------------------------------------

dat <- read_csv("2018_Adult_Health_Care_Quality_Measures.csv")

#Snake case field names
names(dat) <- tolower(str_replace_all(names(dat), " ", "_"))  

#Convert state rate to numeric
dat <- dat %>% 
  mutate(
    state_rate = ifelse(state_rate == "NR", "", state_rate),
    state_rate = as.numeric(state_rate)
  ) 

# Pull in 2018 demographic data for states
state_demo <- readxl::read_excel("State County All Table 2018.xlsx", sheet = "State_county 2018", skip = 1)
# snake case field names
names(state_demo) <- tolower(str_replace_all(names(state_demo), " ", "_")) 



# CREATE EXPANSION TABLE --------------------------------------------------
# Medicaid expansion dates from https://www.facs.org/-/media/files/quality-programs/cancer/ncdb/puf_data_dictionary.ashx

exp <- data.frame(state = state.name, stringsAsFactors = F)

# KY, NV, CO, OR, NM, WV, AR, RI, AZ, MD, MA, ND, OH, IA, IL, VT, HI, NY, DE - 1/2014 Expansion states
exp_1_2014 <- state.name[match(c("KY", "NV", "CO", "OR", "NM", "WV", "AR", "RI", "AZ", "MD", "MA", "ND", "OH", "IA", "IL", "VT", "HI", "NY", "DE"), state.abb)]
# WA, CA, NJ, MN, DC, CT - Early expansion - (2010- 2013)
early_exp <- state.name[match(c("WA", "CA", "NJ", "MN", "DC", "CT" ), state.abb)]
# NH, IN, MI, PA,AK,MT,LA - Late Expansion States (after Jan. 2014)
late_exp <- state.name[match(c("WA", "CA", "NJ", "MN", "DC", "CT" ), state.abb)]

exp <- exp %>% 
  mutate(
    exp_time = ifelse(state %in% exp_1_2014, "Expanded 1/2014", ifelse(state %in% early_exp, "Expanded 2010-2013", ifelse(state %in% late_exp, "Expanded after 1/2014", "No Expansion")))
  )

# Add expansion info to data 
dat <- dat %>% 
  left_join(
    exp,
    by = "state"
  )


count(dat, measure_abbreviation, rate_definition)
# A tibble: 24 x 2
# measure_name                                                                                                measure_abbreviati…
# <chr>                                                                                                       <chr>              
# 1 Adherence to Antipsychotic Medications for Individuals with Schizophrenia: Ages 19 to 64                    SAA-AD          
# 2 Antidepressant Medication Management: Age 18 and Older                                                      AMM-AD            
# 3 Diabetes Screening for People with Schizophrenia or Bipolar Disorder Who Are Using Antipsychotic Medicatio… SSD-AD            
# 4 Follow-Up After Emergency Department Visit for Alcohol and Other Drug Abuse or Dependence: Age 18 and Older FUA-AD            
# 5 Follow-Up After Emergency Department Visit for Mental Illness: Age 18 and Older                             FUM-AD            
# 6 Follow-Up After Hospitalization for Mental Illness: Age 18 and Older                                        FUH-AD            
# 7 Initiation and Engagement of Alcohol and Other Drug Abuse or Dependence Treatment: Age 18 and Older         IET-AD            
# 8 Use of Opioids at High Dosage in Persons Without Cancer: Age 18 and Older                                   OHD-AD            
# 9 Annual Monitoring for Patients on Persistent Medications: Age 18 and Older                                  MPM-AD            
# 10 Asthma Medication Ratio: Ages 19 to 64                                                                      AMR-AD           
# 11 Comprehensive Diabetes Care: Hemoglobin A1c Testing: Ages 18 to 75                                          HA1C-AD          
# 12 Controlling High Blood Pressure: Ages 18 to 85                                                              CBP-AD           
# 13 PQI 01: Diabetes Short-Term Complications Admission Rate: Age 18 and Older                                  PQI01-AD         
# 14 PQI 05: Chronic Obstructive Pulmonary Disease (COPD) or Asthma in Older Adults Admission Rate: Age 40 and … PQI05-AD         
# 15 PQI 08: Heart Failure Admission Rate: Age 18 and Older                                                      PQI08-AD         
# 16 PQI 15: Asthma in Younger Adults Admission Rate: Ages 18 to 39                                              PQI15-AD         
# 17 Contraceptive Care: Postpartum Women Ages 21 to 44                                                          CCP-AD           
# 18 Prenatal and Postpartum Care: Postpartum Care                                                               PPC-AD           
# 19 Adult Body Mass Index Assessment: Ages 18 to 74                                                             ABA-AD         
# 20 Breast Cancer Screening: Ages 50 to 74                                                                      BCS-AD           
# 21 Cervical Cancer Screening: Ages 21 to 64                                                                    CCS-AD           
# 22 Chlamydia Screening in Women Ages 21 to 24                                                                  CHL-AD           
# 23 Plan All-Cause Readmissions: Ages 18 to 64                                                                  PCR-AD           
# 24 Comprehensive Diabetes Care: Hemoglobin A1c Poor Control (>9.0%): Ages 18 to 75                             HPC-AD  

# Write a function that plots two variables against one another



# PlOT ---------------------------------------------------------------------


# This function flips the data wide and keeps state rates for two different measure
plot_measures_data <- function(dat, m1, m2) {
  d <- dat %>% 
    filter(measure_abbreviation == m1) %>% 
    select(state, measure_abbreviation, state_rate) %>% 
    full_join(
      dat %>% 
        filter(measure_abbreviation == m2) %>% 
        select(state, measure_abbreviation, state_rate),
      by = c("state"),
      suffix = c("_m1", "_m2")
    )
  
  return(d)
  
}

#### SCATTER PLOTS
p1_d <- plot_measures_data(dat, "AMR-AD","PQI05-AD")
ggplot(p1_d, aes(x = state_rate_m1, y = state_rate_m2)) +
  geom_point()+
  geom_smooth()+
  xlab("Asthma Medication Ratio: 19-64")+
  ylab("PQI-05") +
  ggtitle("Assocation between PQI-05 and Asthma Medication Ratio in 2019")+
  theme_bw()


p2_d <- plot_measures_data(dat, "ABA-AD","PQI05-AD")
ggplot(p2_d, aes(x = state_rate_m1, y = state_rate_m2)) +
  geom_point()+
  geom_smooth()+
  xlab("BMI")+
  ylab("PQI-05") +
  ggtitle("Assocation between PQI-05 and BMI in 2019") +
  theme_bw()

p3_d <- plot_measures_data(dat, "PQI15-AD","PQI05-AD")
ggplot(p3_d, aes(x = state_rate_m1, y = state_rate_m2)) +
  geom_point()+
  geom_smooth()+
  xlab("PQI-15")+
  ylab("PQI-05") +
  ggtitle("Assocation between PQI-05 and PQI-15 in 2019") +
  theme_bw()

p4_d <- plot_measures_data(dat, "OHD-AD","PQI08-AD")
ggplot(p4_d, aes(x = state_rate_m1, y = state_rate_m2)) +
  geom_point()+
  geom_smooth()+
  xlab("% with opiod prescription")+
  ylab("PQI-08") +
  ggtitle("Assocation between opiod prescriptions and PQI-08 in 2019")+
  theme_bw()

p5_d <- plot_measures_data(dat, "OHD-AD","PCR-AD")
ggplot(p5_d, aes(x = state_rate_m1, y = state_rate_m2)) +
  geom_point()+
  geom_smooth()+
  xlab("% with opiod prescription")+
  ylab("Ratio of Readmissions") +
  ggtitle("Assocation between opiod prescriptions and readmissions in 2019") +
  theme_bw()


##### HISTOGRAMS

dat %>% 
  filter(str_detect(rate_definition, "^Percentage")) %>% 
  ggplot(aes(x = state_rate/100)) +
  geom_histogram(color = "black")+
  scale_x_continuous(labels = percent) +
  facet_wrap(.~rate_definition)

###### DEMOGRAPHIC DISTRIBUTIONS BY STATE
state1 <- state_demo %>% 
  filter(county == "STATE TOTAL") %>% 
  select(state, percent_eligible_for_medicaid) %>% 
  mutate(
    percent_eligible_for_medicaid = as.numeric(str_replace(percent_eligible_for_medicaid, "[%]", ""))/100
  )

ggplot(state1, aes(x = percent_eligible_for_medicaid)) +
  geom_histogram(color = "black") +
  scale_x_continuous(labels = percent) +
  xlab("Percentage of State Eligible for Medicaid") +
  ylab("Count") +
  ggtitle("Distribution of Medicaid Eligibility in 2018") +
  theme_bw()


dat %>% 
  filter(str_detect(measure_abbreviation, "PQI05")) %>% 
  ggplot(aes(x = state_rate)) +
  geom_histogram(binwidth = 15,color = "black") +
  facet_wrap(.~exp_time)
