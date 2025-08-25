bdhs <- read.csv('data/bdhs.csv')

#pull variables of interest

bdhs_interest <- bdhs[, c("V102", "V150", "V151", "V152", "B4", "B19", "HW2", "HW3")] 

#rename variables

names(bdhs_interest)[names(bdhs_interest) == "V102"] <- "residence"
names(bdhs_interest)[names(bdhs_interest) == "V150"] <- "relationship"
names(bdhs_interest)[names(bdhs_interest) == "V151"] <- "head_sex"
names(bdhs_interest)[names(bdhs_interest) == "V152"] <- "head_age"
names(bdhs_interest)[names(bdhs_interest) == "B4"] <- "child_sex"
names(bdhs_interest)[names(bdhs_interest) == "B19"] <- "child_age_months"
names(bdhs_interest)[names(bdhs_interest) == "HW2"] <- "child_weight"
names(bdhs_interest)[names(bdhs_interest) == "HW3"] <- "child_height"

#V102 type of place of residence (rural/urban)

bdhs_interest$residence <- factor(bdhs_interest$residence, levels = c(1,2), labels = c("Urban", "Rural"))
summary(bdhs_interest$residence)

#V150 relationship to household head

bdhs_interest$relationship <- factor(bdhs_interest$relationship, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,98), labels = c("Head","Wife","Daughter","Daughter-in-law","Granddaughter", "Mother","Mother-in-law","Sister","Co-spouse","Other relative", "Adopted/foster child","Not related","Don't know"))
summary(bdhs_interest$relationship)

  #categorise into traditional and non-traditional households
bdhs_interest$relationship_trad <- ifelse(bdhs_interest$relationship == "Wife", "Traditional", "Non-traditional")
bdhs_interest$relationship_trad <- factor(bdhs_interest$relationship_trad, levels = c("Traditional", "Non-traditional"))
summary(bdhs_interest$relationship_trad)

#V151 sex of household head

bdhs_interest$head_sex <- factor(bdhs_interest$head_sex, levels = c(1,2), labels=c("Male", "Female"))
summary(bdhs_interest$head_sex)

#V152 age of household head

summary(bdhs_interest$head_age)
hist(bdhs_interest$head_age,breaks = 100,xlab="Age", main="Histogram of Age of Household Head")
boxplot(bdhs_interest$head_age,ylab="Age", main="Boxplot of Age of Household Head",col = 'lavender')

  #remove outliers

boxplot.stats(bdhs_interest$head_age)$out
bdhs_interest$head_age_clean <- bdhs_interest$head_age
bdhs_interest$head_age_clean[bdhs_interest$head_age_clean %in% boxplot.stats(bdhs_interest$head_age)$out] <- NA
boxplot(bdhs_interest$head_age_clean,ylab="Age", main="Boxplot of Age of Household Head",col = 'lavender')

#B4 sex of child

bdhs_interest$child_sex <- factor(bdhs_interest$child_sex, levels = c(1,2), labels=c("Male", "Female"))
summary(bdhs_interest$child_sex)

#B19 current age of child in months 

summary(bdhs_interest$child_age_months)
hist(bdhs_interest$child_age_months,breaks = 100,xlab="Age (months)", main="Histogram of Age of Child")
boxplot(bdhs_interest$child_age_months,ylab="Age (months)", main="Boxplot of Age of Child",col = 'lavender')

#HW2 child's weight in kilograms (1 decimal)

bdhs_interest$child_weight <- bdhs_interest$child_weight / 10 #add decimal point
summary(bdhs_interest$child_weight)
boxplot(bdhs_interest$child_weight,ylab="Weight (kg)", main="Boxplot of Weight of Child",col = 'lavender')

  # remove outliers

boxplot.stats(bdhs_interest$child_weight)$out 
bdhs_interest$child_weight_clean <- bdhs_interest$child_weight
bdhs_interest$child_weight_clean[bdhs_interest$child_weight_clean %in% boxplot.stats(bdhs_interest$child_weight)$out] <- NA
boxplot(bdhs_interest$child_weight_clean,ylab="Weight (kg)", main="Boxplot of Weight of Child",col = 'lavender')

#HW3 child's height in centimeters (1 decimal)

bdhs_interest$child_height <- bdhs_interest$child_height / 10 #add decimal point
summary(bdhs_interest$child_height)
boxplot(bdhs_interest$child_height,ylab="Height (cm)", main="Boxplot of Height of Child",col = 'lavender')

  #remove outliers

boxplot.stats(bdhs_interest$child_height)$out 
bdhs_interest$child_height_clean <- bdhs_interest$child_height
bdhs_interest$child_height_clean[bdhs_interest$child_height_clean %in% boxplot.stats(bdhs_interest$child_height)$out] <- NA
boxplot(bdhs_interest$child_height_clean,ylab="Height (cm)", main="Boxplot of Weight of Child",col = 'lavender')
