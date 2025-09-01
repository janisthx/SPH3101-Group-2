par(mfrow = c(1, 2))

#calculate bmi
bdhs_interest$child_bmi <- bdhs_interest$child_weight_clean / 
  ((bdhs_interest$child_height_clean/100)^2)

summary(bdhs_interest$child_bmi[bdhs_interest$residence == "Rural"])
summary(bdhs_interest$child_bmi[bdhs_interest$residence == "Urban"])

# Rural
hist(bdhs_interest$child_bmi[bdhs_interest$residence == "Rural"],
     main = "BMI of Children in Rural Areas",
     xlab = "BMI",
     col = "skyblue", border = "white")

# Urban
hist(bdhs_interest$child_bmi[bdhs_interest$residence == "Urban"],
     main = "BMI of Children in Urban Areas",
     xlab = "BMI",
     col = "salmon", border = "white")

boxplot(child_bmi ~ residence, data = bdhs_interest,
        main = "BMI of Children by Residence",
        xlab = "Residence",
        ylab = "BMI",
        col = c("skyblue", "salmon"))


