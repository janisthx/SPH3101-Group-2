# 4. Urban-Rural Interaction Analysis

library(tidyverse)
library(survey)

# Load data and models
bdhs_final <- read.csv("data/bdhs_cleaned_final.csv")
survey_design <- readRDS("data/survey_design.rds")


# Take stunting as example:

final_main <- svyglm(stunted ~ wealth_c + children_c + average_parent_edu_c,
                     design = survey_design, family = binomial)
# Trying to add residence into consideration:
tryout <- svyglm(stunted ~ wealth_c + children_c + average_parent_edu_c + residence,
                     design = survey_design, family = binomial)
summary(tryout)

# Not significant at all

# Testing Interaction of residence on three different variables
# 1) wealth
m1 <- svyglm(stunted ~ wealth_c * residence + children_c + average_parent_edu_c,
             design = survey_design, family = binomial)
lr1 <- anova(final_main, m1, test = "Chisq")
# 2) children
m2 <- svyglm(stunted ~ wealth_c + children_c * residence + average_parent_edu_c,
             design = survey_design, family = binomial)
lr2 <- anova(final_main, m2, test = "Chisq")
# 3) education
m3 <- svyglm(stunted ~ wealth_c + children_c + average_parent_edu_c * residence,
             design = survey_design, family = binomial)
lr3 <- anova(final_main, m3, test = "Chisq")

print(lr1)
print(lr2)
print(lr3)


