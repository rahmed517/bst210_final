#########################
# Title: BST 210 Final: Clean Managed Care data
# Purpose: Re-format managed care data for analysis
#########################



# SETUP -------------------------------------------------------------------

rm(list = ls())

gc(reset = TRUE)

library(tidyverse)
library(ggplot2)

setwd("~/Desktop/bst210_final/")

# LOAD DATA ---------------------------------------------------------------

mc_df_og <- read_csv("data/2018_Managed_Care_Programs_By_State.csv")

### ORIGINAL FIELD NAMES
# [1] "Features"                                                                                                                                         
# [2] "Program type"                                                                                                                                     
# [3] "Statewide or region-specific?"                                                                                                                    
# [4] "Federal operating authority"                                                                                                                      
# [5] "Program start date"                                                                                                                               
# [6] "Waiver expiration date (if applicable)"                                                                                                           
# [7] "If the program ended in 2018, indicate the end date"                                                                                              
# [8] "Populations enrolled: Low-income adults not covered under ACA Section VIII (excludes pregnant women and people with disabilities)"                
# [9] "Populations enrolled: Low-income adults covered under  ACA Section VIII (excludes pregnant women and people with disabilities)"                   
# [10] "Populations enrolled: Aged, Blind or Disabled Children or Adults"                                                                                 
# [11] "Populations enrolled: Non-Disabled Children (excludes children in foster care or receiving adoption assistance)"                                  
# [12] "Populations enrolled: Individuals receiving Limited Benefits (excludes partial duals)"                                                            
# [13] "Populations enrolled: Full Duals"                                                                                                                 
# [14] "Populations enrolled: Partial Duals"                                                                                                              
# [15] "Populations enrolled: Children with Special Health Care Needs"                                                                                    
# [16] "Populations enrolled: Native American/Alaskan Natives"                                                                                            
# [17] "Populations enrolled: Foster Care and Adoption Assistance Children"                                                                               
# [18] "Populations enrolled: Enrollment choice period"                                                                                                   
# [19] "Populations enrolled: Enrollment broker name (if applicable)"                                                                                     
# [20] "Populations enrolled: Notes on enrollment choice period"                                                                                          
# [21] "Benefits covered: Inpatient hospital physical health"                                                                                             
# [22] "Benefits covered: Inpatient hospital behavioral health (MH and/or SUD)"                                                                           
# [23] "Benefits covered: Outpatient hospital physical health"                                                                                            
# [24] "Benefits covered: Outpatient hospital behavioral health (MH and/or SUD)"                                                                          
# [25] "Benefits covered: Partial hospitalization"                                                                                                        
# [26] "Benefits covered: Physician"                                                                                                                      
# [27] "Benefits covered: Nurse practitioner"                                                                                                             
# [28] "Benefits covered: Rural health clinics and FQHCs"                                                                                                 
# [29] "Benefits covered: Clinic services"                                                                                                                
# [30] "Benefits covered: Lab and x-ray"                                                                                                                  
# [31] "Benefits covered: Prescription drugs"                                                                                                             
# [32] "Benefits covered: Prosthetic devices"                                                                                                             
# [33] "Benefits covered: EPSDT"                                                                                                                          
# [34] "Benefits covered: Case management"                                                                                                                
# [35] "Benefits covered: SSA Section 1945-authorized health home"                                                                                        
# [36] "Benefits covered: Health home care (services in home)"                                                                                            
# [37] "Benefits covered: Family planning"                                                                                                                
# [38] "Benefits covered: Dental services (medical/surgical)"                                                                                             
# [39] "Benefits covered: Dental (preventative or corrective)"                                                                                            
# [40] "Benefits covered: Personal care (state plan option)"                                                                                              
# [41] "Benefits covered: HCBS waiver services"                                                                                                           
# [42] "Benefits covered: Private duty nursing"                                                                                                           
# [43] "Benefits covered: ICF-IDD"                                                                                                                        
# [44] "Benefits covered: Nursing facility services"                                                                                                      
# [45] "Benefits covered: Hospice care"                                                                                                                   
# [46] "Benefits covered: Non-Emergency Medical Transportation"                                                                                           
# [47] "Benefits covered: Institution for Mental Disease inpatient treatment for people ages 21-64 defined by 42 CFR §438.6(e) as an 'in lieu of' benefit"
# [48] "Benefits covered: Other (e.g., nurse midwife services, freestanding birth centers, podiatry, etc.)"                                               
# [49] "Quality assurance and improvement: HEDIS data required?"                                                                                          
# [50] "Quality assurance and improvement: CAHPS data required?"                                                                                          
# [51] "Quality assurance and improvement: Accreditation required?"                                                                                       
# [52] "Quality assurance and improvement: Accrediting organization"                                                                                      
# [53] "Quality assurance and improvement: EQRO contractor name (if applicable)"                                                                          
# [54] "Performance incentives: Payment bonuses/differentials to reward plans"                                                                            
# [55] "Performance incentives: Preferential auto-enrollment to reward plans"                                                                             
# [56] "Performance incentives: Public reports comparing plan performance on key metrics"                                                                 
# [57] "Performance incentives: Withholds tied to performance metrics"                                                                                    
# [58] "Performance incentives: MCOs/PHPs required or encouraged to pay providers for value/quality outcomes"                                             
# [59] "Participating plans: Plans in Program"                                                                                                            
# [60] "Notes: Program notes"                                                                                                                             
# [61] "state"    



# SUBSET DATA -------------------------------------------------------------

# Keep only statewide program for ease of comparison:
mc_df1 <- mc_df_og %>% 
  filter(`Statewide or region-specific?` == "Statewide")

# SUBSET FIELDS -----------------------------------------------------------

# Only keeping state, program type, populations enrolled and benefits covered (Excluded "Other" benefits)

mc_df <- mc_df1[, c(2, 61, 5:6, 8:17, 21:47)]


# STANDARDIZE FIELDS ------------------------------------------------------

# Clean start date
mc_df$`Program start date` <- as.Date(mc_df$`Program start date`, format = "%m/%d/%Y")

# Enrollment factors

mc_df %>% 
  select(names(.)[str_detect(names(.), "Populations enrolled")]) %>% 
  as.matrix() %>% as.vector() %>% unique()
# NA          "Mandatory" "Voluntary" "Varies"    "Exempt"  

# These are clean enough, can convert these to factors 
# mc_df %>% 
  # mutate_at(vars(names(.)[str_detect(names(.), "Populations enrolled")]), .funs = as.factor) %>% str()

# Benefits covered
mc_df %>% 
  select(names(.)[str_detect(names(.), "Benefits covered")]) %>% 
  as.matrix() %>% as.vector() %>% unique()
# NA  "X"

# Recode benefits covered as 0s and 1s

mc_df <- mc_df %>% 
  # clean services variables
  mutate_at(
    vars(names(.)[str_detect(names(.), "Benefits covered")]), .funs = function(x) return(ifelse(is.na(x), 0, 1))
  )



# OUTPUT ------------------------------------------------------------------

write_csv(mc_df, "data/managed_care_clean_2018.csv")


