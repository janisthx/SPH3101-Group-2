data <- read.csv('data/bdhs.csv')
# Histogram
data$HW71[data$HW71 %in% c(9996, 9997, 9998)] <- NA
WAZ_clean <- data$HW71 / 100
hist(WAZ_clean, main = "Distribution of Weight-for-Age Z-score", xlab = "HW71 Z-score", col = "skyblue", border = "black")

# Define a new variable to indicate underweight status
data$underweight <- ifelse(data$HW71 < -200, 1, 0)

# Calculate underweight rate by wealth index (V190)
wealth_underweight_rate <- aggregate(underweight ~ V190, data = data, FUN = mean, na.rm = TRUE)
print(wealth_underweight_rate)

# Convert V190 to factor type
data$V190 <- factor(data$V190)

# Create a contingency table
contingency_table <- table(data$V190, data$underweight)
print(contingency_table)

# Perform chi-square test
chisq.test(contingency_table)

# Ensure V102 is a factor type and assign meaningful labels for each category
data$V102 <- factor(data$V102, levels = c(1, 2), labels = c("Urban", "Rural"))

# Use t-test to compare the mean HW71 between urban and rural children
t.test(HW71 ~ V102, data = data)

# Ensure mother's education level (V106) is a factor type
data$V106 <- factor(data$V106, levels = c(0, 1, 2, 3), labels = c("No education", "Primary", "Secondary", "Higher"))

boxplot(WAZ_clean ~ V106,
        data = data,
        col = c("lightcoral", "gold", "lightblue", "palegreen"),
        main = "Weight-for-Age Z-score (HW71) by Mother's Education Level",
        xlab = "Mother's Education Level",
        ylab = "HW71 Z-score")


# Run ANOVA to compare the mean HW71 across different mother's education levels
anova_mother_edu <- aov(HW71 ~ V106, data = data)

# View ANOVA results
summary(anova_mother_edu)

# Exploratory linear model with V136 as predictor
explo <- lm(data$HW71 ~ data$V136)
summary(explo)

# Using multi variables for regression
# Convert categorical variables to factor type
data$V190 <- factor(data$V190)
data$V102 <- factor(data$V102)
data$V106 <- factor(data$V106)
data$V701 <- factor(data$V701)

# Build a multiple linear regression model
# Use HW71 as the dependent variable, V136 as the core independent variable
model_household <- lm(HW71 ~ V136 + V190 + V102 + V106 + V701, data = data)

# View model results
summary(model_household)

# calculate the correlation between household numbers and z-score of weight4age
correlation <- cor(data$V136, data$HW71, use = "pairwise.complete.obs")
print(correlation)
