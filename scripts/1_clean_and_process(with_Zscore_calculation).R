# Modified script to add HAZ, WHZ, and BAZ calculations to existing data processing

# Load required packages (anthro is a package published by WHO for child birth related calculations)
if (!require("anthro")) {
  install.packages("anthro")
}
library(anthro)

# Read data

bdhs <- read.csv('data/bdhs.csv')

# Pull variables of interest (adding HW1 and HW15 for z-score calculation)
bdhs_interest <- bdhs[, c("V102", "V136", "V150", "V151", "V152", "V501", 
                          "B4", "B19", "HW1", "HW2", "HW3", "HW15", "HW71")] 
# V102 type of place of residence (rural/urban)
bdhs_interest$residence <- factor(bdhs_interest$V102, levels = c(1,2), labels = c("Urban", "Rural"))
summary(bdhs_interest$residence)

# V136 number of household members
names(bdhs_interest)[names(bdhs_interest) == "V136"] <- "household_members"
summary(bdhs_interest$household_members)

# V150 relationship to household head
bdhs_interest$relationship <- factor(bdhs_interest$V150, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,98), 
                                     labels = c("Head","Wife","Daughter","Daughter-in-law","Granddaughter", 
                                                "Mother","Mother-in-law","Sister","Co-spouse","Other relative", 
                                                "Adopted/foster child","Not related","Don't know"))
summary(bdhs_interest$relationship)

# Categorise into traditional and non-traditional households
bdhs_interest$relationship_trad <- ifelse(bdhs_interest$relationship == "Wife", "Traditional", "Non-traditional")
bdhs_interest$relationship_trad <- factor(bdhs_interest$relationship_trad, levels = c("Traditional", "Non-traditional"))
summary(bdhs_interest$relationship_trad)

# V151 sex of household head
bdhs_interest$head_sex <- factor(bdhs_interest$V151, levels = c(1,2), labels=c("Male", "Female"))
summary(bdhs_interest$head_sex)

# V152 age of household head
names(bdhs_interest)[names(bdhs_interest) == "V152"] <- "head_age"
summary(bdhs_interest$head_age)
hist(bdhs_interest$head_age,breaks = 100,xlab="Age", main="Histogram of Age of Household Head")
boxplot(bdhs_interest$head_age,ylab="Age", main="Boxplot of Age of Household Head",col = 'lavender')

# Remove outliers
boxplot.stats(bdhs_interest$head_age)$out
bdhs_interest$head_age_clean <- bdhs_interest$head_age
bdhs_interest$head_age_clean[bdhs_interest$head_age_clean %in% boxplot.stats(bdhs_interest$head_age)$out] <- NA
boxplot(bdhs_interest$head_age_clean,ylab="Age", main="Boxplot of Age of Household Head",col = 'lavender')

# V501 marital status
bdhs_interest$marital_status <- factor(bdhs_interest$V501, levels = c(0,1,2,3,4,5), 
                                       labels = c("Never in union","Married","Co-living","Widowed","Divorced","Separated"))
summary(bdhs_interest$marital_status)

# B4 sex of child
bdhs_interest$child_sex <- factor(bdhs_interest$B4, levels = c(1,2), labels=c("Male", "Female"))
summary(bdhs_interest$child_sex)

# B19 current age of child in months 
names(bdhs_interest)[names(bdhs_interest) == "B19"] <- "child_age_months"
summary(bdhs_interest$child_age_months)
hist(bdhs_interest$child_age_months,breaks = 100,xlab="Age (months)", main="Histogram of Age of Child")
boxplot(bdhs_interest$child_age_months,ylab="Age (months)", main="Boxplot of Age of Child",col = 'lavender')

# HW2 child's weight in kilograms (1 decimal)
bdhs_interest$child_weight_kg <- bdhs_interest$HW2 / 10 #add decimal point
summary(bdhs_interest$child_weight_kg)
boxplot(bdhs_interest$child_weight_kg,ylab="Weight (kg)", main="Boxplot of Weight of Child",col = 'lavender')

    # Remove outliers
boxplot.stats(bdhs_interest$child_weight_kg)$out
bdhs_interest$child_weight_clean <- bdhs_interest$child_weight_kg
bdhs_interest$child_weight_clean[bdhs_interest$child_weight_clean %in% boxplot.stats(bdhs_interest$child_weight_kg)$out] <- NA
boxplot(bdhs_interest$child_weight_clean,ylab="Weight (kg)", main="Boxplot of Weight of Child",col = 'lavender')

# HW3 child's height in centimeters (1 decimal)
bdhs_interest$child_height_cm <- bdhs_interest$HW3 / 10 #add decimal point
summary(bdhs_interest$child_height_cm)
boxplot(bdhs_interest$child_height_cm,ylab="Height (cm)", main="Boxplot of Height of Child",col = 'lavender')

# Remove outliers
boxplot.stats(bdhs_interest$child_height_cm)$out 
bdhs_interest$child_height_clean <- bdhs_interest$child_height_cm
bdhs_interest$child_height_clean[bdhs_interest$child_height_clean %in% boxplot.stats(bdhs_interest$child_height_cm)$out] <- NA
boxplot(bdhs_interest$child_height_clean,ylab="Height (cm)", main="Boxplot of Height of Child",col = 'lavender')

# HW71 child's weight age standard deviation
bdhs_interest$WAZ_clean <- bdhs_interest$HW71 / 100 #scaled for Z-value
summary(bdhs_interest$WAZ_clean)
bdhs_interest$WAZ_clean[bdhs_interest$WAZ_clean %in% c(99.96, 99.97, 99.98)] <- NA
hist(bdhs_interest$WAZ_clean)

# ========== NEW Z-SCORE CALCULATIONS ==========

# Filter for valid records for z-score calculation
valid_records <- !is.na(bdhs_interest$HW1) & 
  !is.na(bdhs_interest$HW2) & 
  !is.na(bdhs_interest$HW3) & 
  !is.na(bdhs_interest$B4) &
  !is.na(bdhs_interest$HW15) &
  bdhs_interest$HW2 < 9994 & 
  bdhs_interest$HW3 < 9994

# Initialize z-score columns with NA
bdhs_interest$HAZ <- NA
bdhs_interest$WHZ <- NA
bdhs_interest$BAZ <- NA


# Calculate z-scores only for valid records
if(sum(valid_records) > 0) {
  cat("Processing", sum(valid_records), "valid records...\n")
  
  # Prepare data for anthro function
  valid_data <- bdhs_interest[valid_records, ]
  
  
    z_scores <- anthro_zscores(
      sex = ifelse(valid_data$B4 == 1, "m", "f"),
      age = valid_data$HW1,  # Age in months
      is_age_in_month = TRUE,
      weight = valid_data$HW2 / 10,  # Convert to kg
      lenhei = valid_data$HW3 / 10,  # Convert to cm
      measure = ifelse(valid_data$HW15 == 1, "l", 
                       ifelse(valid_data$HW15 == 2, "h", NA)),  # l=lying, h=standing
      oedema = "n"
    )
  
  # Add z-scores (scaled like HW71 - multiplied by 100)
  bdhs_interest$HAZ[valid_records] <- round(z_scores$zlen * 100)  # Height-for-age
  bdhs_interest$WHZ[valid_records] <- round(z_scores$zwfl * 100)  # Weight-for-height
  bdhs_interest$BAZ[valid_records] <- round(z_scores$zbmi * 100)  # BMI-for-age
  
  # Handle implausible values (set to NA like other cleaned variables)
  bdhs_interest$HAZ[valid_records][abs(z_scores$zlen) > 6 | is.na(z_scores$zlen)] <- NA
  bdhs_interest$WHZ[valid_records][abs(z_scores$zwfl) > 5 | is.na(z_scores$zwfl)] <- NA
  bdhs_interest$BAZ[valid_records][abs(z_scores$zbmi) > 5 | is.na(z_scores$zbmi)] <- NA
  
  
# Convert to regular Z-score format for cleaned dataset
bdhs_interest$HAZ_clean <- bdhs_interest$HAZ / 100
bdhs_interest$WHZ_clean <- bdhs_interest$WHZ / 100
bdhs_interest$BAZ_clean <- bdhs_interest$BAZ / 100

# Quick summary
cat("\nZ-scores calculated for", sum(valid_records), "children\n")
cat("HAZ: Mean =", round(mean(bdhs_interest$HAZ_clean, na.rm = TRUE), 2), "\n")
cat("WHZ: Mean =", round(mean(bdhs_interest$WHZ_clean, na.rm = TRUE), 2), "\n")
cat("BAZ: Mean =", round(mean(bdhs_interest$BAZ_clean, na.rm = TRUE), 2), "\n\n")

# ========== SAVE CLEANED DATA ==========

bdhs_clean <- subset(bdhs_interest,
                     select = c(residence, household_members, relationship,
                                head_sex, head_age, marital_status,
                                child_sex, child_age_months,
                                child_weight_clean, child_height_clean, 
                                WAZ_clean, HAZ_clean, WHZ_clean, BAZ_clean))

write.csv(bdhs_clean, "data/bdhs_clean.csv", row.names = FALSE)

cat("Data processing complete. File saved as 'bdhs_clean.csv'\n")
cat("New columns added: HAZ_clean, WHZ_clean, BAZ_clean\n")
