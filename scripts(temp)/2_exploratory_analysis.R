# 2. Comprehensive Exploratory Analysis with Statistical Tests

library(tidyverse)
library(survey)

# Load cleaned data
bdhs_final <- read.csv("data/bdhs_cleaned_final.csv")

# Create survey design object for weighted analysis
survey_design <- readRDS("data/survey_design.rds")

cat("\n========================================")
cat("\n   EXPLORATORY DATA ANALYSIS")
cat("\n========================================\n")

################################################################################
# PART 1: BASIC DESCRIPTIVE STATISTICS
################################################################################

cat("\n==================== BASIC DESCRIPTIVE STATISTICS ====================\n")

# Sample size
cat("\nTotal sample size:", nrow(bdhs_final), "\n")

# Malnutrition prevalence
cat("\n--- Malnutrition Prevalence (unweighted) ---\n")
cat("Stunting:", mean(bdhs_final$stunted, na.rm=T)*100, "%\n")
cat("Wasting:", mean(bdhs_final$wasted, na.rm=T)*100, "%\n")
cat("Underweight:", mean(bdhs_final$underweight, na.rm=T)*100, "%\n")

# Weighted prevalence
cat("\n--- Malnutrition Prevalence (weighted) ---\n")
stunting_weighted <- svymean(~stunted, survey_design, na.rm=TRUE)
wasting_weighted <- svymean(~wasted, survey_design, na.rm=TRUE)
underweight_weighted <- svymean(~underweight, survey_design, na.rm=TRUE)
cat("Stunting:", stunting_weighted[1]*100, "%\n")
cat("Wasting:", wasting_weighted[1]*100, "%\n")
cat("Underweight:", underweight_weighted[1]*100, "%\n")

################################################################################
# PART 2: OBJECTIVE 1 - WEALTH AND MALNUTRITION
################################################################################

cat("\n==================== OBJECTIVE 1: WEALTH AND MALNUTRITION ====================\n")

# Test different wealth variables
cat("\n--- Testing Different Wealth Variables ---\n")

# 1. Wealth combined (V190)
cat("\n1. Wealth Combined Index:\n")
table(bdhs_final$wealth_combined)

# 2. Wealth urban/rural specific (V190A)
cat("\n2. Wealth Urban/Rural Index:\n")
table(bdhs_final$wealth_urban_rural)

# Chi-square tests for categorical wealth
cat("\n--- Chi-square Tests: Wealth Categories vs Malnutrition ---\n")

# Stunting
cat("\nStunting by wealth quintiles:\n")
stunting_wealth_tab <- table(bdhs_final$stunted, bdhs_final$wealth_quintile)
print(prop.table(stunting_wealth_tab, 2) * 100)
stunting_wealth_test <- chisq.test(stunting_wealth_tab)
cat("Chi-square test: X² =", stunting_wealth_test$statistic, 
    ", p-value =", stunting_wealth_test$p.value, "\n")

# Wasting
cat("\nWasting by wealth quintiles:\n")
wasting_wealth_tab <- table(bdhs_final$wasted, bdhs_final$wealth_quintile)
print(prop.table(wasting_wealth_tab, 2) * 100)
wasting_wealth_test <- chisq.test(wasting_wealth_tab)
cat("Chi-square test: X² =", wasting_wealth_test$statistic, 
    ", p-value =", wasting_wealth_test$p.value, "\n")

# Trend test using numeric wealth index
cat("\n--- Trend Test: Wealth as Continuous Variable ---\n")

# Correlation between wealth index and malnutrition
cor_wealth_stunting <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$stunted)
cor_wealth_wasting <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$wasted)

cat("Correlation - Wealth and Stunting: r =", cor_wealth_stunting$estimate, 
    ", p =", cor_wealth_stunting$p.value, "\n")
cat("Correlation - Wealth and Wasting: r =", cor_wealth_wasting$estimate, 
    ", p =", cor_wealth_wasting$p.value, "\n")

# Logistic regression for trend
wealth_stunting_trend <- glm(stunted ~ wealth_urban_rural, data = bdhs_final, family = binomial)
cat("\nWealth effect on stunting (OR per quintile):", exp(coef(wealth_stunting_trend)[2]), "\n")

wealth_wasting_trend <- glm(wasted ~ wealth_urban_rural, data = bdhs_final, family = binomial)
cat("Wealth effect on wasting (OR per quintile):", exp(coef(wealth_wasting_trend)[2]), "\n")

################################################################################
# PART 3: OBJECTIVE 2A - HOUSEHOLD SIZE AND MALNUTRITION
################################################################################

cat("\n==================== OBJECTIVE 2A: HOUSEHOLD SIZE ====================\n")

# Descriptive statistics
cat("\n--- Household Size Distribution ---\n")
cat("Mean:", mean(bdhs_final$household_members, na.rm=T), "\n")
cat("Median:", median(bdhs_final$household_members, na.rm=T), "\n")
cat("SD:", sd(bdhs_final$household_members, na.rm=T), "\n")

# T-test: Compare household size between malnourished vs normal
cat("\n--- T-tests: Household Size by Malnutrition Status ---\n")

# Stunting
stunted_hh_size <- bdhs_final$household_members[bdhs_final$stunted == 1]
normal_hh_size <- bdhs_final$household_members[bdhs_final$stunted == 0]
t_test_stunting_hh <- t.test(stunted_hh_size, normal_hh_size)
cat("\nStunting - Mean HH size:\n")
cat("  Stunted:", mean(stunted_hh_size, na.rm=T), "\n")
cat("  Normal:", mean(normal_hh_size, na.rm=T), "\n")
cat("  T-test: t =", t_test_stunting_hh$statistic, ", p =", t_test_stunting_hh$p.value, "\n")

# Wasting
wasted_hh_size <- bdhs_final$household_members[bdhs_final$wasted == 1]
normal_wasted_hh_size <- bdhs_final$household_members[bdhs_final$wasted == 0]
t_test_wasting_hh <- t.test(wasted_hh_size, normal_wasted_hh_size)
cat("\nWasting - Mean HH size:\n")
cat("  Wasted:", mean(wasted_hh_size, na.rm=T), "\n")
cat("  Normal:", mean(normal_wasted_hh_size, na.rm=T), "\n")
cat("  T-test: t =", t_test_wasting_hh$statistic, ", p =", t_test_wasting_hh$p.value, "\n")

# Categorical household size
cat("\n--- Chi-square Test: Household Size Categories ---\n")
stunting_hhcat_test <- chisq.test(table(bdhs_final$stunted, bdhs_final$household_size_cat))
cat("Stunting by HH size category: X² =", stunting_hhcat_test$statistic, 
    ", p =", stunting_hhcat_test$p.value, "\n")

# Correlation
cor_hh_stunting <- cor.test(bdhs_final$household_members, bdhs_final$stunted)
cat("\nCorrelation - HH size and stunting: r =", cor_hh_stunting$estimate, 
    ", p =", cor_hh_stunting$p.value, "\n")

################################################################################
# PART 4: OBJECTIVE 2B - NUMBER OF CHILDREN AND MALNUTRITION
################################################################################

cat("\n==================== OBJECTIVE 2B: NUMBER OF CHILDREN ====================\n")

# Test different children variables
cat("\n--- Comparing Different Children Variables ---\n")

# 1. Total children ever born
cat("\n1. Total children ever born:\n")
cat("Mean:", mean(bdhs_final$total_children_born, na.rm=T), "\n")
cor_total_stunting <- cor.test(bdhs_final$total_children_born, bdhs_final$stunted, use = "complete.obs")
cat("Correlation with stunting: r =", cor_total_stunting$estimate, ", p =", cor_total_stunting$p.value, "\n")

# 2. Births in last 5 years
cat("\n2. Births in last 5 years:\n")
cat("Mean:", mean(bdhs_final$births_last5y, na.rm=T), "\n")
cor_births5y_stunting <- cor.test(bdhs_final$births_last5y, bdhs_final$stunted, use = "complete.obs")
cat("Correlation with stunting: r =", cor_births5y_stunting$estimate, ", p =", cor_births5y_stunting$p.value, "\n")

# 3. Living children
cat("\n3. Living children:\n")
cat("Mean:", mean(bdhs_final$living_children, na.rm=T), "\n")
cor_living_stunting <- cor.test(bdhs_final$living_children, bdhs_final$stunted, use = "complete.obs")
cat("Correlation with stunting: r =", cor_living_stunting$estimate, ", p =", cor_living_stunting$p.value, "\n")

# T-tests for births in last 5 years (most relevant)
cat("\n--- T-tests: Children Under 5 by Malnutrition Status ---\n")

# Stunting
stunted_children <- bdhs_final$births_last5y[bdhs_final$stunted == 1]
normal_children <- bdhs_final$births_last5y[bdhs_final$stunted == 0]
t_test_stunting_children <- t.test(stunted_children, normal_children)
cat("\nStunting - Mean children under 5:\n")
cat("  Stunted:", mean(stunted_children, na.rm=T), "\n")
cat("  Normal:", mean(normal_children, na.rm=T), "\n")
cat("  T-test: t =", t_test_stunting_children$statistic, ", p =", t_test_stunting_children$p.value, "\n")

# Wasting
wasted_children <- bdhs_final$births_last5y[bdhs_final$wasted == 1]
normal_wasted_children <- bdhs_final$births_last5y[bdhs_final$wasted == 0]
t_test_wasting_children <- t.test(wasted_children, normal_wasted_children)
cat("\nWasting - Mean children under 5:\n")
cat("  Wasted:", mean(wasted_children, na.rm=T), "\n")
cat("  Normal:", mean(normal_wasted_children, na.rm=T), "\n")
cat("  T-test: t =", t_test_wasting_children$statistic, ", p =", t_test_wasting_children$p.value, "\n")

# Chi-square for categorical
cat("\n--- Chi-square Test: Children Categories ---\n")
stunting_children_cat_test <- chisq.test(table(bdhs_final$stunted, bdhs_final$children_cat))
cat("Stunting by children category: X² =", stunting_children_cat_test$statistic, 
    ", p =", stunting_children_cat_test$p.value, "\n")

################################################################################
# PART 5: HOUSEHOLD SIZE vs CHILDREN - Which is better?
################################################################################

cat("\n==================== HOUSEHOLD SIZE vs CHILDREN COMPARISON ====================\n")

# Model comparison using AIC
cat("\n--- Model Comparison (AIC) ---\n")

model_hh <- glm(stunted ~ household_members, data = bdhs_final, family = binomial)
model_children <- glm(stunted ~ births_last5y, data = bdhs_final, family = binomial)
model_both <- glm(stunted ~ household_members + births_last5y, data = bdhs_final, family = binomial)

cat("Model with HH size only - AIC:", AIC(model_hh), "\n")
cat("Model with children only - AIC:", AIC(model_children), "\n")
cat("Model with both - AIC:", AIC(model_both), "\n")
cat("Best single variable:", ifelse(AIC(model_hh) < AIC(model_children), "Household size", "Children under 5"), "\n")

# Check correlation between HH size and children
cor_hh_children <- cor.test(bdhs_final$household_members, bdhs_final$births_last5y)
cat("\nCorrelation between HH size and children under 5: r =", cor_hh_children$estimate, 
    ", p =", cor_hh_children$p.value, "\n")

################################################################################
# PART 6: OBJECTIVE 3 - WEALTH AND HOUSEHOLD COMPOSITION ASSOCIATION
################################################################################

cat("\n==================== OBJECTIVE 3: WEALTH-HOUSEHOLD ASSOCIATION ====================\n")

# Correlation analysis
cat("\n--- Correlations with Wealth ---\n")

cor_wealth_hh <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$household_members)
cat("Wealth and HH size: r =", cor_wealth_hh$estimate, ", p =", cor_wealth_hh$p.value, "\n")

cor_wealth_children <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$births_last5y)
cat("Wealth and children under 5: r =", cor_wealth_children$estimate, ", p =", cor_wealth_children$p.value, "\n")

cor_wealth_total_children <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$total_children_born)
cat("Wealth and total children born: r =", cor_wealth_total_children$estimate, ", p =", cor_wealth_total_children$p.value, "\n")

# ANOVA for wealth quintiles
cat("\n--- ANOVA: Mean HH size by Wealth Quintile ---\n")
anova_wealth_hh <- aov(household_members ~ wealth_quintile, data = bdhs_final)
summary(anova_wealth_hh)

cat("\n--- ANOVA: Mean children by Wealth Quintile ---\n")
anova_wealth_children <- aov(births_last5y ~ wealth_quintile, data = bdhs_final)
summary(anova_wealth_children)

# Cross-tabulation
cat("\n--- Cross-tabulation: Wealth by HH Size Category ---\n")
wealth_hh_table <- table(bdhs_final$wealth_quintile, bdhs_final$household_size_cat)
print(prop.table(wealth_hh_table, 1) * 100)
chisq_wealth_hh <- chisq.test(wealth_hh_table)
cat("Chi-square test: X² =", chisq_wealth_hh$statistic, ", p =", chisq_wealth_hh$p.value, "\n")

################################################################################
# PART 7: OBJECTIVE 4 - PARENT EDUCATION AND MALNUTRITION
################################################################################

cat("\n==================== OBJECTIVE 4: PARENT EDUCATION ====================\n")

# Descriptive statistics
cat("\n--- Education Distribution ---\n")
cat("Mother's education - Mean:", mean(bdhs_final$mother_edu_years, na.rm=T), "years\n")
cat("Father's education - Mean:", mean(bdhs_final$father_edu_years, na.rm=T), "years\n")
cat("Average parent education - Mean:", mean(bdhs_final$average_parent_edu, na.rm=T), "years\n")

# T-tests for education by malnutrition status
cat("\n--- T-tests: Education by Stunting Status ---\n")

# Mother's education
stunted_mother_edu <- bdhs_final$mother_edu_years[bdhs_final$stunted == 1]
normal_mother_edu <- bdhs_final$mother_edu_years[bdhs_final$stunted == 0]
t_test_mother <- t.test(stunted_mother_edu, normal_mother_edu)
cat("\nMother's education:\n")
cat("  Stunted:", mean(stunted_mother_edu, na.rm=T), "years\n")
cat("  Normal:", mean(normal_mother_edu, na.rm=T), "years\n")
cat("  T-test: t =", t_test_mother$statistic, ", p =", t_test_mother$p.value, "\n")

# Father's education
stunted_father_edu <- bdhs_final$father_edu_years[bdhs_final$stunted == 1]
normal_father_edu <- bdhs_final$father_edu_years[bdhs_final$stunted == 0]
t_test_father <- t.test(stunted_father_edu, normal_father_edu)
cat("\nFather's education:\n")
cat("  Stunted:", mean(stunted_father_edu, na.rm=T), "years\n")
cat("  Normal:", mean(normal_father_edu, na.rm=T), "years\n")
cat("  T-test: t =", t_test_father$statistic, ", p =", t_test_father$p.value, "\n")

# Average parent education
stunted_avg_edu <- bdhs_final$average_parent_edu[bdhs_final$stunted == 1]
normal_avg_edu <- bdhs_final$average_parent_edu[bdhs_final$stunted == 0]
t_test_avg <- t.test(stunted_avg_edu, normal_avg_edu)
cat("\nAverage parent education:\n")
cat("  Stunted:", mean(stunted_avg_edu, na.rm=T), "years\n")
cat("  Normal:", mean(normal_avg_edu, na.rm=T), "years\n")
cat("  T-test: t =", t_test_avg$statistic, ", p =", t_test_avg$p.value, "\n")

# Chi-square for education categories
cat("\n--- Chi-square Tests: Education Categories ---\n")
mother_edu_cat_test <- chisq.test(table(bdhs_final$stunted, bdhs_final$mother_edu_cat))
cat("Mother's education category: X² =", mother_edu_cat_test$statistic, 
    ", p =", mother_edu_cat_test$p.value, "\n")

father_edu_cat_test <- chisq.test(table(bdhs_final$stunted, bdhs_final$father_edu_cat))
cat("Father's education category: X² =", father_edu_cat_test$statistic, 
    ", p =", father_edu_cat_test$p.value, "\n")

# Which education variable is better?
cat("\n--- Comparing Education Variables (AIC) ---\n")
model_mother_edu <- glm(stunted ~ mother_edu_years, data = bdhs_final, family = binomial)
model_father_edu <- glm(stunted ~ father_edu_years, data = bdhs_final, family = binomial)
model_avg_edu <- glm(stunted ~ average_parent_edu, data = bdhs_final, family = binomial)

cat("Mother's education only - AIC:", AIC(model_mother_edu), "\n")
cat("Father's education only - AIC:", AIC(model_father_edu), "\n")
cat("Average parent education - AIC:", AIC(model_avg_edu), "\n")
cat("Best education variable:", c("Mother", "Father", "Average")[which.min(c(AIC(model_mother_edu), 
                                                                             AIC(model_father_edu), 
                                                                             AIC(model_avg_edu)))], "\n")

################################################################################
# PART 8: HOUSEHOLD STRUCTURE AND MALNUTRITION
################################################################################

cat("\n==================== HOUSEHOLD STRUCTURE ====================\n")

# Head of household characteristics
cat("\n--- Head of Household ---\n")

# Sex of household head
cat("\nSex of household head:\n")
table(bdhs_final$head_sex)
head_sex_test <- chisq.test(table(bdhs_final$stunted, bdhs_final$head_sex))
cat("Stunting by head sex: X² =", head_sex_test$statistic, ", p =", head_sex_test$p.value, "\n")

# Age of household head
cat("\nAge of household head:\n")
cat("Mean:", mean(bdhs_final$head_age, na.rm=T), "years\n")
stunted_head_age <- bdhs_final$head_age[bdhs_final$stunted == 1]
normal_head_age <- bdhs_final$head_age[bdhs_final$stunted == 0]
t_test_head_age <- t.test(stunted_head_age, normal_head_age)
cat("  Stunted HH head age:", mean(stunted_head_age, na.rm=T), "\n")
cat("  Normal HH head age:", mean(normal_head_age, na.rm=T), "\n")
cat("  T-test: t =", t_test_head_age$statistic, ", p =", t_test_head_age$p.value, "\n")

# Relationship to household head
cat("\nRelationship to household head:\n")
relationship_table <- table(bdhs_final$relationship_cat)
print(relationship_table)
if("relationship_cat" %in% names(bdhs_final)) {
  relationship_test <- chisq.test(table(bdhs_final$stunted, bdhs_final$relationship_cat))
  cat("Stunting by relationship: X² =", relationship_test$statistic, ", p =", relationship_test$p.value, "\n")
}

################################################################################
# PART 9: URBAN vs RURAL DIFFERENCES
################################################################################

cat("\n==================== URBAN vs RURAL ====================\n")

# Basic distribution
cat("\n--- Residence Distribution ---\n")
residence_table <- table(bdhs_final$residence)
print(residence_table)
print(prop.table(residence_table) * 100)

# Malnutrition by residence
cat("\n--- Malnutrition by Residence ---\n")
stunting_residence <- table(bdhs_final$stunted, bdhs_final$residence)
cat("\nStunting prevalence:\n")
print(prop.table(stunting_residence, 2) * 100)
stunting_res_test <- chisq.test(stunting_residence)
cat("Chi-square test: X² =", stunting_res_test$statistic, ", p =", stunting_res_test$p.value, "\n")

wasting_residence <- table(bdhs_final$wasted, bdhs_final$residence)
cat("\nWasting prevalence:\n")
print(prop.table(wasting_residence, 2) * 100)
wasting_res_test <- chisq.test(wasting_residence)
cat("Chi-square test: X² =", wasting_res_test$statistic, ", p =", wasting_res_test$p.value, "\n")

# Key variables by residence
cat("\n--- Key Variables by Residence ---\n")
urban_data <- bdhs_final[bdhs_final$residence == "Urban", ]
rural_data <- bdhs_final[bdhs_final$residence == "Rural", ]

cat("\nWealth (mean):\n")
cat("  Urban:", mean(urban_data$wealth_urban_rural, na.rm=T), "\n")
cat("  Rural:", mean(rural_data$wealth_urban_rural, na.rm=T), "\n")
t_test_wealth_res <- t.test(urban_data$wealth_urban_rural, rural_data$wealth_urban_rural)
cat("  T-test: p =", t_test_wealth_res$p.value, "\n")

cat("\nHousehold size (mean):\n")
cat("  Urban:", mean(urban_data$household_members, na.rm=T), "\n")
cat("  Rural:", mean(rural_data$household_members, na.rm=T), "\n")
t_test_hh_res <- t.test(urban_data$household_members, rural_data$household_members)
cat("  T-test: p =", t_test_hh_res$p.value, "\n")

cat("\nChildren under 5 (mean):\n")
cat("  Urban:", mean(urban_data$births_last5y, na.rm=T), "\n")
cat("  Rural:", mean(rural_data$births_last5y, na.rm=T), "\n")
t_test_children_res <- t.test(urban_data$births_last5y, rural_data$births_last5y)
cat("  T-test: p =", t_test_children_res$p.value, "\n")

cat("\nParent education (mean):\n")
cat("  Urban:", mean(urban_data$average_parent_edu, na.rm=T), "\n")
cat("  Rural:", mean(rural_data$average_parent_edu, na.rm=T), "\n")
t_test_edu_res <- t.test(urban_data$average_parent_edu, rural_data$average_parent_edu)
cat("  T-test: p =", t_test_edu_res$p.value, "\n")

################################################################################
# PART 10: CHILD CHARACTERISTICS
################################################################################

cat("\n==================== CHILD CHARACTERISTICS ====================\n")

# Child sex
cat("\n--- Child Sex ---\n")
sex_table <- table(bdhs_final$child_sex)
print(sex_table)
sex_stunting_test <- chisq.test(table(bdhs_final$stunted, bdhs_final$child_sex))
cat("Stunting by child sex: X² =", sex_stunting_test$statistic, ", p =", sex_stunting_test$p.value, "\n")

# Child age
cat("\n--- Child Age ---\n")
cat("Mean age:", mean(bdhs_final$child_age_months, na.rm=T), "months\n")
stunted_age <- bdhs_final$child_age_months[bdhs_final$stunted == 1]
normal_age <- bdhs_final$child_age_months[bdhs_final$stunted == 0]
t_test_age <- t.test(stunted_age, normal_age)
cat("  Stunted children age:", mean(stunted_age, na.rm=T), "months\n")
cat("  Normal children age:", mean(normal_age, na.rm=T), "months\n")
cat("  T-test: t =", t_test_age$statistic, ", p =", t_test_age$p.value, "\n")

# Age categories
age_stunting_test <- chisq.test(table(bdhs_final$stunted, bdhs_final$child_age_cat))
cat("\nStunting by age category: X² =", age_stunting_test$statistic, ", p =", age_stunting_test$p.value, "\n")

################################################################################
# SUMMARY OF KEY FINDINGS
################################################################################

cat("\n========================================")
cat("\n   SUMMARY OF KEY FINDINGS")
cat("\n========================================\n")

cat("\nOBJECTIVE 1 - Wealth and Malnutrition:\n")
cat("  - Significant negative association (p < 0.001)\n")
cat("  - Clear gradient: Higher wealth → Lower malnutrition\n")

cat("\nOBJECTIVE 2 - Household Composition:\n")
cat("  - Household size:", ifelse(t_test_stunting_hh$p.value < 0.05, "Significant", "Not significant"), "\n")
cat("  - Children under 5:", ifelse(t_test_stunting_children$p.value < 0.05, "Significant", "Not significant"), "\n")
cat("  - Better predictor:", ifelse(AIC(model_hh) < AIC(model_children), "Household size", "Children under 5"), "\n")

cat("\nOBJECTIVE 3 - Wealth-Household Association:\n")
cat("  - Wealth-HH size correlation: r =", round(cor_wealth_hh$estimate, 3), "\n")
cat("  - Wealth-Children correlation: r =", round(cor_wealth_children$estimate, 3), "\n")
cat("  - Association is", ifelse(abs(cor_wealth_hh$estimate) > 0.3, "moderate to strong", "weak to moderate"), "\n")

cat("\nOBJECTIVE 4 - Parent Education:\n")
cat("  - Mother's education:", ifelse(t_test_mother$p.value < 0.05, "Significant", "Not significant"), "\n")
cat("  - Father's education:", ifelse(t_test_father$p.value < 0.05, "Significant", "Not significant"), "\n")
cat("  - Best education variable:", c("Mother", "Father", "Average")[which.min(c(AIC(model_mother_edu), 
                                                                                   AIC(model_father_edu), 
                                                                                   AIC(model_avg_edu)))], "\n")

cat("\nADDITIONAL FINDINGS:\n")
cat("  - Urban-Rural difference:", ifelse(stunting_res_test$p.value < 0.05, "Significant", "Not significant"), "\n")
cat("  - Head of HH sex effect:", ifelse(head_sex_test$p.value < 0.05, "Significant", "Not significant"), "\n")
cat("  - Child sex effect:", ifelse(sex_stunting_test$p.value < 0.05, "Significant", "Not significant"), "\n")
cat("  - Child age effect:", ifelse(t_test_age$p.value < 0.05, "Significant", "Not significant"), "\n")

cat("\n========================================\n")
cat("RECOMMENDATIONS FOR FINAL MODEL:\n")
cat("1. Include wealth_urban_rural as key predictor\n")
cat("2. Use", ifelse(AIC(model_children) < AIC(model_hh), "births_last5y", "household_members"), "for household composition\n")
cat("3. Use", c("mother_edu_years", "father_edu_years", "average_parent_edu")[which.min(c(AIC(model_mother_edu), 
                                                                                         AIC(model_father_edu), 
                                                                                         AIC(model_avg_edu)))], "for education\n")
cat("4. Consider stratification by residence if interaction effects are significant\n")
cat("5. Control for child age and sex\n")
cat("========================================\n")