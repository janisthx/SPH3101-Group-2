par(mfrow = c(1, 3))

#calculate bmi
bdhs_interest$child_bmi <- bdhs_interest$child_weight_clean / 
  ((bdhs_interest$child_height_clean/100)^2)

summary(bdhs_interest$child_bmi[bdhs_interest$residence == "Rural"])
summary(bdhs_interest$child_bmi[bdhs_interest$residence == "Urban"])

#residence
boxplot(child_bmi ~ residence, data = bdhs_interest,
        main = "BMI of Children by Residence",
        xlab = "Residence",
        ylab = "BMI",
        col = c("skyblue", "salmon"))

hist(bdhs_interest$child_bmi[bdhs_interest$residence == "Urban"],
     main = "BMI of Children in Urban Area",
     xlab = "BMI", col = "skyblue", border = "white")

hist(bdhs_interest$child_bmi[bdhs_interest$residence == "Rural"],
     main = "BMI of Children in Rural Area",
     xlab = "BMI", col = "salmon", border = "white")

#family type
boxplot(child_bmi ~ relationship_trad, data = bdhs_interest,
        main = "BMI of Children by Family Type",
        xlab = "Family Type",
        ylab = "BMI",
        col = c("lightgreen","orange"))

hist(bdhs_interest$child_bmi[bdhs_interest$relationship_trad == "Traditional"],
     main = "BMI of Children in Traditional Families",
     xlab = "BMI", col = "lightgreen", border = "white")

hist(bdhs_interest$child_bmi[bdhs_interest$relationship_trad == "Non-traditional"],
     main = "BMI of Children in Non-traditional Families",
     xlab = "BMI", col = "orange", border = "white")
