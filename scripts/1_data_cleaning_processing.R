# 1. Data Cleaning and Processing with Survey Weights

# Read data
bdhs <- read.csv('data/bdhs.csv')

# Select Variables of Interest
bdhs_interest <- bdhs %>%
  select(
    # Demographic variables
    V102,  # Type of residence (urban/rural)
    V136,  # Number of household members
    V151,  # Sex of household head
    V150,  # Relationship to household head
    V152,  # Age of household head
    
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
    
    # Children Numbers
    V201,  # Total children ever born
    V208,  # Births in last five years
    V218   # Number of living children
  )

# Clean and Rename Variables
bdhs_clean <- bdhs_interest %>%
  rename(
    
    # Demographic
    residence = V102,
    household_members = V136,
    relationship = V150,
    head_sex = V151,
    head_age = V152,
    
    # Socioeconomic
    wealth_combined = V190,
    wealth_urban_rural = V190A,
    mother_edu_level = V106,
    mother_edu_years = V133,
    father_edu_level = V701,
    father_edu_years = V715,
    
    # Child characteristics
    child_sex = B4,
    child_age_months = B19,
    child_age_months_hw = HW1,
    child_weight_raw = HW2,
    child_height_raw = HW3,
    measurement_position = HW15,
    
    # Children numbers
    total_children_born = V201,
    births_last5y = V208,
    living_children = V218
  ) %>%
  mutate(
    
    # Convert residence to factor
    residence = factor(residence, levels = c(1, 2), labels = c("Urban", "Rural")),
    
    # Convert sex variables to factors
    head_sex = factor(head_sex, levels = c(1, 2), labels = c("Male", "Female")),
    child_sex = factor(child_sex, levels = c(1, 2), labels = c("Male", "Female")),
    
    # Clean education variables (remove missing codes)
    mother_edu_years = ifelse(mother_edu_years %in% c(97, 98), NA, mother_edu_years),
    father_edu_years = ifelse(father_edu_years %in% c(97, 98), NA, father_edu_years),
    
    # Create education categories
    mother_edu_cat = case_when(
      mother_edu_level == 0 ~ "No education",
      mother_edu_level == 1 ~ "Primary",
      mother_edu_level == 2 ~ "Secondary",
      mother_edu_level == 3 ~ "Higher",
      TRUE ~ NA_character_
    ),
    father_edu_cat = case_when(
      father_edu_level == 0 ~ "No education",
      father_edu_level == 1 ~ "Primary",
      father_edu_level == 2 ~ "Secondary",
      father_edu_level == 3 ~ "Higher",
      TRUE ~ NA_character_
    ),
    
    # Calculate average parent education
    average_parent_edu = case_when(
      !is.na(mother_edu_years) & !is.na(father_edu_years) ~ (mother_edu_years + father_edu_years) / 2,
      !is.na(mother_edu_years) & is.na(father_edu_years) ~ mother_edu_years,
      is.na(mother_edu_years) & !is.na(father_edu_years) ~ father_edu_years,
      TRUE ~ NA_real_
    ),
    
    # Wealth quintiles (1=poorest to 5=richest)
    wealth_quintile = factor(wealth_urban_rural, 
                             levels = 1:5,
                             labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    
    # Household size categories
    household_size_cat = case_when(
      household_members <= 4 ~ "Small (≤4)",
      household_members <= 6 ~ "Medium (5-6)",
      household_members > 6 ~ "Large (>6)",
      TRUE ~ NA_character_
    ),
    
    # Number of children categories
    children_cat = case_when(
      births_last5y == 0 ~ "No children",
      births_last5y == 1 ~ "1 child",
      births_last5y == 2 ~ "2 children",
      births_last5y >= 3 ~ "3+ children",
      TRUE ~ NA_character_
    ),
    
    # Convert weight and height to proper units
    child_weight_kg = ifelse(child_weight_raw >= 9994, NA, child_weight_raw / 10),
    child_height_cm = ifelse(child_height_raw >= 9994, NA, child_height_raw / 10),
    
    # Clean the Head Age Variable
    head_age = ifelse(head_age == 98, NA, head_age),

    # Label the relationship variable
    relationship = ifelse(relationship > 12, NA, relationship),
    relationship = factor(relationship, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                                     labels = c("Head","Wife","Daughter","Daughter-in-law","Granddaughter", 
                                                "Mother","Mother-in-law","Sister","Co-spouse","Other relative", 
                                                "Adopted/foster child","Not related")),
    relationship = case_when(
      relationship %in% c("Wife", "Daughter", "Granddaughter", "Daughter-in-law", "Mother", "Mother-in-law", "Co-spouse") ~ "Traditional",
      relationship %in% c("Head", "Sister", "Other relative", "Adopted/foster child", "Not related") ~ "Non_traditional",
      TRUE ~ NA_character_
    ),
    relationship = factor(relationship, 
                               levels = c("Traditional", "Non_traditional")),
    
    # Create age categories for children
    child_age_cat = case_when(
      child_age_months < 6 ~ "0-5 months",
      child_age_months < 12 ~ "6-11 months",
      child_age_months < 24 ~ "12-23 months",
      child_age_months < 36 ~ "24-35 months",
      child_age_months < 48 ~ "36-47 months",
      child_age_months <= 60 ~ "48-60 months",
      TRUE ~ NA_character_
    )

    
  )

# Calculate Z-scores using anthro package

# Filter for valid records
valid_idx <- with(bdhs_clean,
                  !is.na(child_age_months_hw) &
                  !is.na(child_weight_kg) &
                  !is.na(child_height_cm) &
                  !is.na(child_sex) &
                  !is.na(measurement_position)
)
valid_records <- bdhs_clean[valid_idx, ]

# Initialize z-score columns with NA
bdhs_clean$haz <- NA
bdhs_clean$whz <- NA
bdhs_clean$waz <- NA

# Calculate Z-scores
if (nrow(valid_records) > 0) {
  z_scores <- anthro_zscores(
    sex = ifelse(valid_records$child_sex == "Male", "m", "f"),
    age = valid_records$child_age_months_hw,
    is_age_in_month = TRUE,
    weight = valid_records$child_weight_kg,
    lenhei = valid_records$child_height_cm,
    measure = case_when(
      valid_records$measurement_position == 1 ~ "l",  # lying
      valid_records$measurement_position == 2 ~ "h",  # standing
      TRUE ~ NA_character_
    ),
    oedema = "n"
  )
  
  # Add Z-scores to valid_records
  bdhs_clean$haz[valid_idx] <- z_scores$zlen  # Height-for-age Z-score
  bdhs_clean$whz[valid_idx] <- z_scores$zwfl  # Weight-for-height Z-score
  bdhs_clean$waz[valid_idx] <- z_scores$zwei  # Weight-for-age Z-score
  
  # Remove implausible values (WHO criteria)
  bdhs_clean$haz[abs(bdhs_clean$haz) > 6] <- NA
  bdhs_clean$whz[abs(bdhs_clean$whz) > 5] <- NA
  bdhs_clean$waz[abs(bdhs_clean$waz) > 6] <- NA
}

# Create Malnutrition Indicators

bdhs_clean <- bdhs_clean %>%
  mutate(
    # Stunting (HAZ < -2)
    stunted = ifelse(haz < -2, 1, 0),

    # Wasting (WHZ < -2)
    wasted = ifelse(whz < -2, 1, 0),

    # Underweight (WAZ < -2)
    underweight = ifelse(waz < -2, 1, 0),

    # Any malnutrition (composite outcome)
    any_malnutrition = case_when(
      stunted == 1 | wasted == 1 | underweight == 1 ~ 1,
      stunted == 0 & wasted == 0 & underweight == 0 ~ 0,
      TRUE ~ NA_real_
    ),
  )

# Final Dataset Preparation

# Select variables for final analysis
final_vars <- c(
  
  # Main analysis variables
  "residence", "wealth_quintile", "wealth_urban_rural", "wealth_combined",
  "household_members", "household_size_cat",
  "births_last5y", "living_children", "total_children_born", "children_cat",
  "mother_edu_years", "father_edu_years", "average_parent_edu",
  "mother_edu_cat", "father_edu_cat", "mother_edu_level", "father_edu_level",
  
  # Child characteristics
  "child_sex", "child_age_months", "child_age_cat",
  "child_weight_kg", "child_height_cm",
  
  # Z-scores
  "haz", "whz", "waz",
  
  # Malnutrition outcomes
  "stunted", "wasted", "underweight", "any_malnutrition",

  # Other demographics
  "head_sex", "head_age", "relationship"
)

bdhs_final <- bdhs_clean %>%
  select(all_of(final_vars)) %>%
  # Keep only children with at least one malnutrition indicator
  filter(!is.na(haz) | !is.na(whz) | !is.na(waz))

# Summary Statistics and Save Cleaned Data
cat("\nDATA CLEANING SUMMARY\n")
cat(paste("Original dataset:", nrow(bdhs), "rows\n"))
cat(paste("After cleaning:", nrow(bdhs_final), "rows\n"))
cat(paste("Records with complete malnutrition data:", 
          sum(complete.cases(bdhs_final[c("stunted", "wasted", "underweight")])), "\n\n"))

# Save cleaned dataset
write.csv(bdhs_final, "data/bdhs_final.csv", row.names = FALSE)
cat("Cleaned data saved as 'bdhs_final.csv'\n")

