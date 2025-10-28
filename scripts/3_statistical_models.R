# 3. Statistical Modeling with Survey Weights

library(tidyverse)
library(survey)

# Load data
bdhs_final <- read.csv("data/bdhs_cleaned_final.csv")

cat("\n========== STATISTICAL MODELING WITH SURVEY WEIGHTS ==========\n")

# PREPARE MODELING DATA

# Center continuous variables for better interpretation
bdhs_final$wealth_c <- scale(bdhs_final$wealth_urban_rural, center = TRUE, scale = FALSE)
bdhs_final$household_members_c <- scale(bdhs_final$household_members, center = TRUE, scale = FALSE)
bdhs_final$children_c <- scale(bdhs_final$total_children_born, center = TRUE, scale = FALSE)
bdhs_final$average_parent_edu_c <- scale(bdhs_final$average_parent_edu, center = TRUE, scale = FALSE)
bdhs_final$head_age_c <- scale(bdhs_final$head_age, center = TRUE, scale = FALSE)

# Update survey design with centered variables
survey_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight_normalized,
  data = bdhs_final,
  nest = TRUE
)

# Save survey design object for later use
saveRDS(survey_design, "data/survey_design.rds")

# PART A: BUILD MODELS BASED ON OBJECTIVES

cat("\n==================== MODEL BUILDING ====================\n")

# =========== STUNTING MODELS ===========

cat("\n--- STUNTING MODELS ---\n")

# Model 1: Wealth only (Objective 1)
stunting_m1 <- svyglm(stunted ~ wealth_c, 
                      design = survey_design, family = binomial)
cat("\nModel 1 - Wealth only:\n")
print(summary(stunting_m1)$coefficients)

# Model 2: Household size only (Objective 2a)
stunting_m2 <- svyglm(stunted ~ household_members_c, 
                      design = survey_design, family = binomial)
cat("\nModel 2 - Household size only:\n")
print(summary(stunting_m2)$coefficients)

# Model 3: Number of children only (Objective 2b)
stunting_m3 <- svyglm(stunted ~ total_children_born, 
                      design = survey_design, family = binomial)
cat("\nModel 3 - Children under 5 only:\n")
print(summary(stunting_m3)$coefficients)

# Model 4: Education only (Objective 4)
stunting_m4 <- svyglm(stunted ~ average_parent_edu_c, 
                      design = survey_design, family = binomial)
cat("\nModel 4 - Parent education only:\n")
print(summary(stunting_m4)$coefficients)

# Model 5: Head Age only (Objective 4)
stunting_m5 <- svyglm(stunted ~ head_age_c, 
                      design = survey_design, family = binomial)
cat("\nModel 5 - Head Age only:\n")
print(summary(stunting_m5)$coefficients)

# Model 6: Wealth + Children (testing association)
stunting_m6 <- svyglm(stunted ~ wealth_c + children_c, 
                      design = survey_design, family = binomial)
cat("\nModel 6 - Wealth + Children:\n")
print(summary(stunting_m6)$coefficients)

# Model 7: Wealth + Education (testing association)
stunting_m7 <- svyglm(stunted ~ wealth_c + average_parent_edu_c, 
                      design = survey_design, family = binomial)
cat("\nModel 7 - Wealth + Education:\n")
print(summary(stunting_m7)$coefficients)

# Model 8: Children + Education (testing association)
stunting_m8 <- svyglm(stunted ~ children_c + average_parent_edu_c, 
                      design = survey_design, family = binomial)
cat("\nModel 8 - Children + Education:\n")
print(summary(stunting_m8)$coefficients)

# Model 7: Full model with all key variables
stunting_full <- svyglm(stunted ~ head_age_c + household_members_c + wealth_c + children_c + average_parent_edu_c, 
                         design = survey_design, family = binomial)
cat("\nFINAL MODEL - All key variables:\n")
print(summary(stunting_full))

# Model 8: Final model with controls
stunting_final <- svyglm(stunted ~ wealth_c + children_c + average_parent_edu_c , 
                        design = survey_design, family = binomial)
cat("\nFULL MODEL - With controls:\n")
print(summary(stunting_final))

# =========== WASTING MODELS ===========
cat("\n--- WASTING MODELS ---\n")

# Model 1: Wealth only
wasting_m1 <- svyglm(wasted ~ wealth_c, design = survey_design, family = binomial)
cat("\nModel 1 - Wealth only:\n")
print(summary(wasting_m1)$coefficients)

# Model 2: Household size only
wasting_m2 <- svyglm(wasted ~ household_members_c, design = survey_design, family = binomial)
cat("\nModel 2 - Household size only:\n")
print(summary(wasting_m2)$coefficients)

# Model 3: Number of children only
wasting_m3 <- svyglm(wasted ~ children_c, design = survey_design, family = binomial)
cat("\nModel 3 - Children under 5 only:\n")
print(summary(wasting_m3)$coefficients)

# Model 4: Education only
wasting_m4 <- svyglm(wasted ~ average_parent_edu_c, design = survey_design, family = binomial)
cat("\nModel 4 - Parent education only:\n")
print(summary(wasting_m4)$coefficients)

# Model 5: Head Age only
wasting_m5 <- svyglm(wasted ~ head_age_c, design = survey_design, family = binomial)
cat("\nModel 5 - Head Age only:\n")
print(summary(wasting_m5)$coefficients)

# Everything not important, no need for further building


# =========== UNDERWEIGHT MODELS ===========
cat("\n--- UNDERWEIGHT MODELS ---\n")

# Model 1: Wealth only
underweight_m1 <- svyglm(underweight ~ wealth_c, design = survey_design, family = binomial)
cat("\nModel 1 - Wealth only:\n")
print(summary(underweight_m1)$coefficients)

# Model 2: Household size only
underweight_m2 <- svyglm(underweight ~ household_members_c, design = survey_design, family = binomial)
cat("\nModel 2 - Household size only:\n")
print(summary(underweight_m2)$coefficients)

# Model 3: Number of children only
underweight_m3 <- svyglm(underweight ~ children_c, design = survey_design, family = binomial)
cat("\nModel 3 - Children under 5 only:\n")
print(summary(underweight_m3)$coefficients)

# Model 4: Education only
underweight_m4 <- svyglm(underweight ~ average_parent_edu_c, design = survey_design, family = binomial)
cat("\nModel 4 - Parent education only:\n")
print(summary(underweight_m4)$coefficients)

# Model 5: Head Age only
underweight_m5 <- svyglm(underweight ~ head_age_c, design = survey_design, family = binomial)
cat("\nModel 5 - Head Age only:\n")
print(summary(underweight_m5)$coefficients)

# Model 6: Wealth + Children
underweight_m6 <- svyglm(underweight ~ wealth_c + children_c, design = survey_design, family = binomial)
cat("\nModel 6 - Wealth + Children:\n")
print(summary(underweight_m6)$coefficients)

# Model 7: Wealth + Head Age
underweight_m7 <- svyglm(underweight ~ wealth_c + head_age_c, design = survey_design, family = binomial)
cat("\nModel 7 - Wealth + Head Age:\n")
print(summary(underweight_m7)$coefficients)

# Model 8: Wealth + Education
underweight_m8 <- svyglm(underweight ~ wealth_c + average_parent_edu_c, design = survey_design, family = binomial)
cat("\nModel 8 - Wealth + Education:\n")
print(summary(underweight_m8)$coefficients)

# Model 9: Children + Education
underweight_m9 <- svyglm(underweight ~ children_c + average_parent_edu_c, design = survey_design, family = binomial)
cat("\nModel 9 - Children + Education:\n")
print(summary(underweight_m9)$coefficients)

# Model 10: Children + Education + Head Age
underweight_m10 <- svyglm(underweight ~ children_c + average_parent_edu_c + head_age_c, design = survey_design, family = binomial)
cat("\nModel 10 - Children + Education + Head Age:\n")
print(summary(underweight_m10)$coefficients)

# Full model with all key variables
underweight_full <- svyglm(underweight ~ head_age_c + household_members_c + wealth_c + children_c + average_parent_edu_c, 
                           design = survey_design, family = binomial)
cat("\nFULL MODEL - All key variables:\n")
print(summary(underweight_full))

# Final model with controls
underweight_final <- svyglm(underweight ~ wealth_c + children_c + average_parent_edu_c, 
                            design = survey_design, family = binomial)
cat("\nFINAL MODEL - With controls:\n")
print(summary(underweight_final))

# PART B: TESTING VARIABLE IMPORTANCE

cat("\n==================== VARIABLE IMPORTANCE ====================\n")

# Likelihood ratio tests for nested models
cat("\nLikelihood Ratio Tests - Adding variables sequentially:\n")

cat("\nFor Stunting Model:")

# Test 1: Does children number improve wealth model?
lr_test1 <- anova(stunting_m1, stunting_m6, test = "Chisq")
cat("\nAdding children to wealth model:\n")
print(lr_test1)

# Test 2: Does education improve wealth model?
lr_test2 <- anova(stunting_m1, stunting_m7, test = "Chisq")
cat("\nAdding education to wealth model:\n")
print(lr_test2)

# Test 3: Does education improve wealth+children model?
lr_test3 <- anova(stunting_m6, stunting_final, test = "Chisq")
cat("\nAdding education to wealth+children model:\n")
print(lr_test3)

# Test 4: Does children number improve wealth+education model?
lr_test4 <- anova(stunting_m7, stunting_final, test = "Chisq")
cat("\nAdding education to wealth+children model:\n")
print(lr_test4)

cat("\nFor Underweight Model:")

# Test 1: Does children number improve wealth model?
lr_test1_uw <- anova(underweight_m1, underweight_m6, test = "Chisq")
cat("\nAdding children to wealth model:\n")
print(lr_test1_uw)

# Test 2: Does head age improve wealth model?
lr_test2_uw <- anova(underweight_m1, underweight_m7, test = "Chisq")
cat("\nAdding head age to wealth model:\n")
print(lr_test2_uw)

# Test 3: Does education improve wealth model?
lr_test3_uw <- anova(underweight_m1, underweight_m8, test = "Chisq")
cat("\nAdding education to wealth model:\n")
print(lr_test3_uw)

# Test 4: Does education improve wealth+children model?
lr_test4_uw <- anova(underweight_m6, underweight_final, test = "Chisq")
cat("\nAdding education to wealth+children model:\n")
print(lr_test4_uw)

# Test 5: Does children number improve wealth+education model?
lr_test5_uw <- anova(underweight_m8, underweight_final, test = "Chisq")
cat("\nAdding children to wealth+education model:\n")
print(lr_test5_uw)

# Test 6: Does head age improve children+education model?
lr_test5_uw <- anova(underweight_m9, underweight_m10, test = "Chisq")
cat("\nAdding head age to children+education model:\n")
print(lr_test5_uw)


# Final models for stunting and underweight is the same: wealth + education + children_numbers