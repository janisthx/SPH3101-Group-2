bdhs <- read.csv('data/bdhs.csv')

#pull variables of interest

bdhs_interest <- bdhs[, c("V102", "V150", "V151", "V152", "HW2", "HW3", "HW13", "H11", "H22")] 

#rename variables

names(bdhs_interest)[names(bdhs_interest) == "V102"] <- "residence"
names(bdhs_interest)[names(bdhs_interest) == "V150"] <- "relationship"
names(bdhs_interest)[names(bdhs_interest) == "V151"] <- "sex_head"
names(bdhs_interest)[names(bdhs_interest) == "V152"] <- "age_head"
names(bdhs_interest)[names(bdhs_interest) == "HW2"] <- "weight"
names(bdhs_interest)[names(bdhs_interest) == "HW3"] <- "height"

bdhs_interest$bmi = (bdhs_interest$weight/10) / (bdhs_interest$height/1000)^2

plot(bdhs_interest$relationship, bdhs_interest$)
summary(bdhs_interest$bmi[bdhs_interest$residence %in% 2])

#check for NA values

anyNA(bdhs_interest$residence)
anyNA(bdhs_interest$relationship)
anyNA(bdhs_interest$sex_head)
anyNA(bdhs_interest$age_head)

  #no NA values

#V102 type of place of residence (rural/urban)

bdhs_interest$residence <- factor(bdhs_interest$residence, levels = c(1,2), labels = c("Urban", "Rural"))
summary(bdhs_interest$residence)

#V150 relationship to household head

bdhs_interest$relationship <- factor(bdhs_interest$relationship, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,98), labels = c("Head","Wife","Daughter","Daughter-in-law","Granddaughter", "Mother","Mother-in-law","Sister","Co-spouse","Other relative", "Adopted/foster child","Not related","Don't know"))
summary(bdhs_interest$relationship)

#V151 sex of household head

bdhs_interest$sex_head <- factor(bdhs_interest$sex_head, levels = c(1,2), labels=c("Male", "Female"))
summary(bdhs_interest$sex_head)

#V152 age of household head

summary(bdhs_interest$age_head)
hist(bdhs_interest$age_head,breaks = 100,xlab="Age", main="Histogram of Age of Household Head")
boxplot(bdhs_interest$age_head,ylab="Age", main="Boxplot of Age of Household Head",col = 'lavender')

  #remove outliers

boxplot.stats(bdhs_interest$age_head)$out
bdhs_interest$age_head_clean <- bdhs_interest$age_head
bdhs_interest$age_head_clean[bdhs_interest$age_head_clean %in% boxplot.stats(bdhs_interest$age_head)$out] <- NA
boxplot(bdhs_interest$age_head_clean,ylab="Age", main="Boxplot of Age of Household Head",col = 'lavender')

