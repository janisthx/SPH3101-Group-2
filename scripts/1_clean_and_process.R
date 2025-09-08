bdhs <- read.csv('data/bdhs.csv')

#pull variables of interest
bdhs_interest <- bdhs[, c("V102", "V136", "V150", "V151", "V152", "V501", "B4", "B19", "HW2", "HW3", "HW71")] 

#rename variables

#names(bdhs_interest)[names(bdhs_interest) == "V102"] <- "residence"
#names(bdhs_interest)[names(bdhs_interest) == "V136"] <- "household_members"
#names(bdhs_interest)[names(bdhs_interest) == "V150"] <- "relationship"
#names(bdhs_interest)[names(bdhs_interest) == "V151"] <- "head_sex"
#names(bdhs_interest)[names(bdhs_interest) == "V152"] <- "head_age"
#names(bdhs_interest)[names(bdhs_interest) == "V501"] <- "marital_status"
#names(bdhs_interest)[names(bdhs_interest) == "B4"] <- "child_sex"
#names(bdhs_interest)[names(bdhs_interest) == "B19"] <- "child_age_months"
#names(bdhs_interest)[names(bdhs_interest) == "HW2"] <- "child_weight"
#names(bdhs_interest)[names(bdhs_interest) == "HW3"] <- "child_height"
#names(bdhs_interest)[names(bdhs_interest) == "HW71"] <- "child_weight_age_SD"

#V102 type of place of residence (rural/urban)

bdhs_interest$residence <- factor(bdhs_interest$V102, levels = c(1,2), labels = c("Urban", "Rural"))
summary(bdhs_interest$residence)

#V136 number of household members
names(bdhs_interest)[names(bdhs_interest) == "V136"] <- "household_members"
summary(bdhs_interest$household_members)

#V150 relationship to household head

bdhs_interest$relationship <- factor(bdhs_interest$V150, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,98), labels = c("Head","Wife","Daughter","Daughter-in-law","Granddaughter", "Mother","Mother-in-law","Sister","Co-spouse","Other relative", "Adopted/foster child","Not related","Don't know"))
summary(bdhs_interest$relationship)

  #categorise into traditional and non-traditional households
bdhs_interest$relationship_trad <- ifelse(bdhs_interest$relationship == "Wife", "Traditional", "Non-traditional")
bdhs_interest$relationship_trad <- factor(bdhs_interest$relationship_trad, levels = c("Traditional", "Non-traditional"))
summary(bdhs_interest$relationship_trad)

#V151 sex of household head

bdhs_interest$head_sex <- factor(bdhs_interest$V151, levels = c(1,2), labels=c("Male", "Female"))
summary(bdhs_interest$head_sex)

#V152 age of household head

names(bdhs_interest)[names(bdhs_interest) == "V152"] <- "head_age"
summary(bdhs_interest$head_age)
hist(bdhs_interest$head_age,breaks = 100,xlab="Age", main="Histogram of Age of Household Head")
boxplot(bdhs_interest$head_age,ylab="Age", main="Boxplot of Age of Household Head",col = 'lavender')

  #remove outliers

boxplot.stats(bdhs_interest$head_age)$out
bdhs_interest$head_age_clean <- bdhs_interest$head_age
bdhs_interest$head_age_clean[bdhs_interest$head_age_clean %in% boxplot.stats(bdhs_interest$head_age)$out] <- NA
boxplot(bdhs_interest$head_age_clean,ylab="Age", main="Boxplot of Age of Household Head",col = 'lavender')

#V501 marital status
bdhs_interest$marital_status <- factor(bdhs_interest$V501, levels = c(0,1,2,3,4,5), labels = c("Never in union","Married","Co-living","Widowed","Divorced","Separated"))
summary(bdhs_interest$marital_status)

#B4 sex of child

bdhs_interest$child_sex <- factor(bdhs_interest$B4, levels = c(1,2), labels=c("Male", "Female"))
summary(bdhs_interest$child_sex)

#B19 current age of child in months 

names(bdhs_interest)[names(bdhs_interest) == "B19"] <- "child_age_months"
summary(bdhs_interest$child_age_months)
hist(bdhs_interest$child_age_months,breaks = 100,xlab="Age (months)", main="Histogram of Age of Child")
boxplot(bdhs_interest$child_age_months,ylab="Age (months)", main="Boxplot of Age of Child",col = 'lavender')

#HW2 child's weight in kilograms (1 decimal)

bdhs_interest$child_weight_kg <- bdhs_interest$HW2 / 10 #add decimal point
summary(bdhs_interest$child_weight_kg)
boxplot(bdhs_interest$child_weight_kg,ylab="Weight (kg)", main="Boxplot of Weight of Child",col = 'lavender')

  # remove outliers

boxplot.stats(bdhs_interest$child_weight_kg)$out
bdhs_interest$child_weight_clean <- bdhs_interest$child_weight_kg
bdhs_interest$child_weight_clean[bdhs_interest$child_weight_clean %in% boxplot.stats(bdhs_interest$child_weight_kg)$out] <- NA
boxplot(bdhs_interest$child_weight_clean,ylab="Weight (kg)", main="Boxplot of Weight of Child",col = 'lavender')

#HW3 child's height in centimeters (1 decimal)

bdhs_interest$child_height_cm <- bdhs_interest$HW3 / 10 #add decimal point
summary(bdhs_interest$child_height_cm)
boxplot(bdhs_interest$child_height_cm,ylab="Height (cm)", main="Boxplot of Height of Child",col = 'lavender')

  #remove outliers

boxplot.stats(bdhs_interest$child_height_cm)$out 
bdhs_interest$child_height_clean <- bdhs_interest$child_height_cm
bdhs_interest$child_height_clean[bdhs_interest$child_height_clean %in% boxplot.stats(bdhs_interest$child_height_cm)$out] <- NA
boxplot(bdhs_interest$child_height_clean,ylab="Height (cm)", main="Boxplot of Height of Child",col = 'lavender')

#HW71 child's weight age standard deviation

bdhs_interest$child_weight_age_SD <- bdhs_interest$HW71 / 100 #scaled for Z-value
summary(bdhs_interest$bdhs_interest$child_weight_age_SD)
bdhs_interest$child_weight_age_SD[bdhs_interest$child_weight_age_SD %in% c(99.96, 99.97, 99.98)] <- NA
hist(bdhs_interest$child_weight_age_SD)

bdhs_clean <- subset(bdhs_interest,
                         select = c(residence, household_members, relationship,
                                    head_sex, head_age, marital_status,
                                    child_sex, child_age_months,
                                    child_weight_clean, child_height_clean, child_weight_age_SD))
write.csv(bdhs_clean, "bdhs_clean.csv", row.names = FALSE)
