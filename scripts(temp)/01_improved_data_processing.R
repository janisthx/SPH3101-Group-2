## Improved Data Processing Script for BDHS Analysis
## Date: 2025
## Purpose: Clean and process BDHS data with all necessary variables for malnutrition analysis

# Load necessary libraries
library(tidyverse)
library(haven)
library(anthro)  # For Z-score calculations

# Read data
bdhs <- read.csv('data/bdhs.csv')

cat("Original dataset dimensions:", nrow(bdhs), "rows,", ncol(bdhs), "columns\n")

# ========== EXTRACT ALL NECESSARY VARIABLES ==========
bdhs_interest <- bdhs %>%
  select(
    # Survey design variables (CRITICAL!)
    V005,  # Women's individual sample weight
    V021,  # Primary sampling unit  
    V022,  # Sample strata
    
    # Demographic variables
    V102,  # Type of residence (urban/rural)
    V136,  # Number of household members
    V150,  # Relationship to household head
    V151,  # Sex of household head
    V152,  # Age of household head
    V501,  # Marital status
    V012,  # Mother's age
    
    # Socioeconomic variables
    V190,  # Wealth index combined
    V190A, # Wealth index for urban/rural
    V106,  # Mother's highest education level
    V133,  # Mother's education in single years
    V701,  # Father's education level
    V715,  # Father's total years of education
    
    # Child characteristics
    B4,    # Sex of child
    B19,   # Current age of child in months
    HW1,   # Child's age in months (from height/weight roster)
    HW2,   # Child's weight in kg (1 decimal)
    HW3,   # Child's height in cm (1 decimal)
    HW15,  # Height measurement: lying or standing
    HW71,  # Weight/Age standard deviation (WHO)
    
    # WASH variables (Water, Sanitation, and Hygiene)
    V113,  # Source of drinking water
    V116,  # Type of toilet facility
    V160,  # Toilet facilities shared with other households
    
    # Health and nutrition variables
    H11,   # Had diarrhea recently
    H22,   # Had fever in last two weeks
    H31,   # Had cough in last two weeks
    H34,   # Vitamin A in last 6 months
    H42,   # Iron pills/sprinkles/syrup in last 12 months
    H43,   # Drugs for intestinal parasites in last 6 months
    
    # Maternal health
    V437,  # Mother's weight in kilograms
    V438,  # Mother's height in centimeters
    M2A,   # Prenatal care: doctor
    M2B,   # Prenatal care: nurse/midwife
    M3A,   # Birth assistance: doctor
    M3B,   # Birth assistance: nurse/midwife
    M17,   # Delivery by caesarean section
    M4,    # Duration of breastfeeding
    
    # Fertility and birth spacing
    V201,  # Total children ever born
    V208,  # Births in last five years
    V218   # Number of living children
  )

# ========== DATA CLEANING AND RECODING ==========

# 1. Survey weights (divide by 1,000,000 as per DHS standard)
bdhs_interest$survey_weight <- bdhs_interest$V005 / 1000000
bdhs_interest$psu <- bdhs_interest$V021
bdhs_interest$strata <- bdhs_interest$V022

# 2. Residence
bdhs_interest$residence <- factor(bdhs_interest$V102,
                                  levels = c(1, 2),
                                  labels = c("Urban", "Rural"))

# 3. Household size
bdhs_interest$household_members <- bdhs_interest$V136

# 4. Child characteristics
bdhs_interest$child_sex <- factor(bdhs_interest$B4,
                                  levels = c(1, 2),
                                  labels = c("Male", "Female"))
bdhs_interest$child_age_months <- bdhs_interest$B19

# Clean implausible values for anthropometry
bdhs_interest$child_weight_kg <- ifelse(bdhs_interest$HW2 >= 9994, NA, bdhs_interest$HW2 / 10)
bdhs_interest$child_height_cm <- ifelse(bdhs_interest$HW3 >= 9994, NA, bdhs_interest$HW3 / 10)

# 5. Parent education
# Mother's education
bdhs_interest$mother_edu_years <- bdhs_interest$V133
bdhs_interest$mother_edu_years[bdhs_interest$mother_edu_years %in% c(97, 98)] <- NA

bdhs_interest$mother_edu_level <- factor(bdhs_interest$V106,
                                         levels = c(0, 1, 2, 3),
                                         labels = c("No education", "Primary", 
                                                   "Secondary", "Higher"))

# Father's education  
bdhs_interest$father_edu_years <- bdhs_interest$V715
bdhs_interest$father_edu_years[bdhs_interest$father_edu_years %in% c(97, 98)] <- NA

bdhs_interest$father_edu_level <- factor(bdhs_interest$V701,
                                         levels = c(0, 1, 2, 3, 8),
                                         labels = c("No education", "Primary",
                                                   "Secondary", "Higher", "Don't know"))

# Average parent education
bdhs_interest$average_parent_edu <- rowMeans(
  bdhs_interest[, c("mother_edu_years", "father_edu_years")], 
  na.rm = TRUE
)

# 6. Wealth index
bdhs_interest$wealth <- factor(bdhs_interest$V190,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Poorest", "Poorer", "Middle", 
                                       "Richer", "Richest"))

# 7. Number of children
bdhs_interest$children_under5 <- bdhs_interest$V208  # Births in last 5 years
bdhs_interest$total_children <- bdhs_interest$V201   # Total children ever born
bdhs_interest$living_children <- bdhs_interest$V218  # Living children

# 8. WASH variables
# Improved water source (based on WHO/UNICEF JMP definitions)
improved_water_codes <- c(11:14, 21, 31, 41, 51, 71)
bdhs_interest$improved_water <- ifelse(
  bdhs_interest$V113 %in% improved_water_codes, 1, 0
)

# Improved sanitation
improved_sanitation_codes <- c(11:13, 21, 22, 41)
bdhs_interest$improved_sanitation <- ifelse(
  bdhs_interest$V116 %in% improved_sanitation_codes, 1, 0
)

# Shared toilet facilities
bdhs_interest$shared_toilet <- ifelse(bdhs_interest$V160 == 1, 1, 0)

# 9. Child health indicators
bdhs_interest$recent_diarrhea <- ifelse(bdhs_interest$H11 %in% c(1, 2), 1, 0)
bdhs_interest$recent_fever <- ifelse(bdhs_interest$H22 == 1, 1, 0)
bdhs_interest$recent_cough <- ifelse(bdhs_interest$H31 %in% c(1, 2), 1, 0)
bdhs_interest$vitamin_a <- ifelse(bdhs_interest$H34 == 1, 1, 0)
bdhs_interest$iron_supplement <- ifelse(bdhs_interest$H42 == 1, 1, 0)
bdhs_interest$deworming <- ifelse(bdhs_interest$H43 == 1, 1, 0)

# 10. Maternal health
# BMI calculation
bdhs_interest$mother_weight <- ifelse(bdhs_interest$V437 >= 9994, NA, bdhs_interest$V437 / 10)
bdhs_interest$mother_height <- ifelse(bdhs_interest$V438 >= 9994, NA, bdhs_interest$V438 / 10)
bdhs_interest$mother_bmi <- bdhs_interest$mother_weight / ((bdhs_interest$mother_height / 100)^2)

# Prenatal care
bdhs_interest$prenatal_doctor <- ifelse(bdhs_interest$M2A == 1, 1, 0)
bdhs_interest$prenatal_nurse <- ifelse(bdhs_interest$M2B == 1, 1, 0)
bdhs_interest$any_prenatal <- ifelse(
  bdhs_interest$prenatal_doctor == 1 | bdhs_interest$prenatal_nurse == 1, 1, 0
)

# Delivery care
bdhs_interest$delivery_doctor <- ifelse(bdhs_interest$M3A == 1, 1, 0)
bdhs_interest$delivery_nurse <- ifelse(bdhs_interest$M3B == 1, 1, 0)
bdhs_interest$skilled_delivery <- ifelse(
  bdhs_interest$delivery_doctor == 1 | bdhs_interest$delivery_nurse == 1, 1, 0
)
bdhs_interest$caesarean <- ifelse(bdhs_interest$M17 == 1, 1, 0)

# Breastfeeding duration
bdhs_interest$breastfeeding_months <- bdhs_interest$M4
bdhs_interest$breastfeeding_months[bdhs_interest$breastfeeding_months >= 93] <- NA

# Mother's age
bdhs_interest$mother_age <- bdhs_interest$V012

# ========== CALCULATE Z-SCORES ==========

# Filter for valid records
valid_records <- !is.na(bdhs_interest$HW1) & 
  !is.na(bdhs_interest$HW2) & 
  !is.na(bdhs_interest$HW3) & 
  !is.na(bdhs_interest$B4) &
  !is.na(bdhs_interest$HW15) &
  bdhs_interest$HW2 < 9994 & 
  bdhs_interest$HW3 < 9994

# Initialize z-score columns
bdhs_interest$HAZ <- NA
bdhs_interest$WHZ <- NA
bdhs_interest$WAZ <- NA

# Calculate z-scores for valid records
if(sum(valid_records) > 0) {
  cat("Processing", sum(valid_records), "valid records for Z-score calculation...\n")
  
  valid_data <- bdhs_interest[valid_records, ]
  
  z_scores <- anthro_zscores(
    sex = ifelse(valid_data$B4 == 1, "m", "f"),
    age = valid_data$HW1,  # Age in months
    is_age_in_month = TRUE,
    weight = valid_data$HW2 / 10,  # Convert to kg
    lenhei = valid_data$HW3 / 10,  # Convert to cm
    measure = ifelse(valid_data$HW15 == 1, "l", 
                     ifelse(valid_data$HW15 == 2, "h", NA)),
    oedema = "n"
  )
  
  # Add z-scores
  bdhs_interest$HAZ[valid_records] <- z_scores$zlen  # Height-for-age
  bdhs_interest$WHZ[valid_records] <- z_scores$zwfl  # Weight-for-height
  bdhs_interest$WAZ[valid_records] <- z_scores$zwei  # Weight-for-age
  
  # Handle implausible values
  bdhs_interest$HAZ[valid_records][abs(z_scores$zlen) > 6 | is.na(z_scores$zlen)] <- NA
  bdhs_interest$WHZ[valid_records][abs(z_scores$zwfl) > 5 | is.na(z_scores$zwfl)] <- NA
  bdhs_interest$WAZ[valid_records][abs(z_scores$zwei) > 5 | is.na(z_scores$zwei)] <- NA
}

# ========== CREATE MALNUTRITION OUTCOME VARIABLES ==========

# Stunting: HAZ < -2
bdhs_interest$stunting <- ifelse(bdhs_interest$HAZ < -2, 1, 0)
bdhs_interest$severe_stunting <- ifelse(bdhs_interest$HAZ < -3, 1, 0)

# Wasting: WHZ < -2
bdhs_interest$wasting <- ifelse(bdhs_interest$WHZ < -2, 1, 0)
bdhs_interest$severe_wasting <- ifelse(bdhs_interest$WHZ < -3, 1, 0)

# Underweight: WAZ < -2
bdhs_interest$underweight <- ifelse(bdhs_interest$WAZ < -2, 1, 0)
bdhs_interest$severe_underweight <- ifelse(bdhs_interest$WAZ < -3, 1, 0)

# Any malnutrition
bdhs_interest$any_malnutrition <- ifelse(
  bdhs_interest$stunting == 1 | bdhs_interest$wasting == 1 | bdhs_interest$underweight == 1, 
  1, 0
)

# ========== SELECT FINAL VARIABLES FOR ANALYSIS ==========

bdhs_clean <- bdhs_interest %>%
  select(
    # Survey design
    survey_weight, psu, strata,
    
    # Outcomes
    stunting, wasting, underweight, any_malnutrition,
    severe_stunting, severe_wasting, severe_underweight,
    HAZ, WHZ, WAZ,
    
    # Child characteristics
    child_age_months, child_sex, 
    child_weight_kg, child_height_cm,
    
    # Household characteristics  
    residence, household_members,
    children_under5, total_children, living_children,
    
    # Socioeconomic
    wealth,
    mother_edu_years, father_edu_years, average_parent_edu,
    mother_edu_level, father_edu_level,
    
    # WASH
    improved_water, improved_sanitation, shared_toilet,
    
    # Child health
    recent_diarrhea, recent_fever, recent_cough,
    vitamin_a, iron_supplement, deworming,
    
    # Maternal health
    mother_age, mother_bmi,
    any_prenatal, skilled_delivery, caesarean,
    breastfeeding_months
  )

# Filter to keep only children with complete anthropometric data
bdhs_clean <- bdhs_clean %>%
  filter(!is.na(HAZ) & !is.na(WHZ) & !is.na(WAZ))

# Summary statistics
cat("\n========== DATA SUMMARY ==========\n")
cat("Final dataset dimensions:", nrow(bdhs_clean), "rows,", ncol(bdhs_clean), "columns\n")
cat("\nMalnutrition prevalence:\n")
cat("  Stunting:", round(mean(bdhs_clean$stunting, na.rm=T)*100, 1), "%\n")
cat("  Wasting:", round(mean(bdhs_clean$wasting, na.rm=T)*100, 1), "%\n")
cat("  Underweight:", round(mean(bdhs_clean$underweight, na.rm=T)*100, 1), "%\n")
cat("\nMean Z-scores:\n")
cat("  HAZ:", round(mean(bdhs_clean$HAZ, na.rm=T), 2), "\n")
cat("  WHZ:", round(mean(bdhs_clean$WHZ, na.rm=T), 2), "\n")
cat("  WAZ:", round(mean(bdhs_clean$WAZ, na.rm=T), 2), "\n")

# Save cleaned data
write.csv(bdhs_clean, "data/bdhs_clean_improved.csv", row.names = FALSE)
cat("\nData saved as 'bdhs_clean_improved.csv'\n")
