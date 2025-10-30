# 4. Urban-Rural Interaction Analysis

library(tidyverse)

# Load data and models
bdhs_final <- read.csv("data/bdhs_cleaned_final.csv")

# Take stunting as example:

final_main <- glm(stunted ~ wealth_c + children_c + average_parent_edu_c,
                     data = bdhs_final, family = binomial)
# Trying to add residence into consideration:
tryout <- glm(stunted ~ wealth_c + children_c + average_parent_edu_c + residence,
                     data = bdhs_final, family = binomial)
summary(tryout)

# Not significant at all

# Testing Interaction of residence on three different variables
# 1) wealth
m1 <- glm(stunted ~ wealth_c * residence + children_c + average_parent_edu_c,
             data = bdhs_final, family = binomial)
lr1 <- anova(final_main, m1, test = "Chisq")
# 2) children
m2 <- glm(stunted ~ wealth_c + children_c * residence + average_parent_edu_c,
             data = bdhs_final, family = binomial)
lr2 <- anova(final_main, m2, test = "Chisq")
# 3) education
m3 <- glm(stunted ~ wealth_c + children_c + average_parent_edu_c * residence,
             data = bdhs_final, family = binomial)
lr3 <- anova(final_main, m3, test = "Chisq")

print(lr1)
print(lr2)
print(lr3)

# Try the Linear Model
haz_final <- lm(haz ~ wealth_c + children_c + average_parent_edu_c, 
                         data = bdhs_final)
summary(haz_final)
waz_final <- lm(waz ~ wealth_c + children_c + average_parent_edu_c, 
                data = bdhs_final)
summary(waz_final)

# Linear models have small R2, not a good option.
