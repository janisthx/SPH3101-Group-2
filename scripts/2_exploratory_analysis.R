# 2. Comprehensive Exploratory Analysis with Statistical Tests

library(tidyverse)
library(survey)

# Load cleaned data
bdhs_final <- read.csv("data/bdhs_cleaned_final.csv")

# Create survey design object for weighted analysis
survey_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight_normalized,
  data = bdhs_final,
  nest = TRUE
)

# PART 1: BASIC DESCRIPTIVE STATISTICS

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

# PART 2: OBJECTIVE 1 - WEALTH AND MALNUTRITION

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
stunting_wealth_tab
print(prop.table(stunting_wealth_tab, 2) * 100)
ggplot(bdhs_final, aes(wealth_quintile, fill = factor(stunted))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "",
    breaks = c("0","1"),
    labels = c("0" = "No Stunting", "1" = "Stunting"),
    values = c("0" = "chartreuse4",  
               "1" = "brown"), 
    ) +
  scale_x_discrete(limits = c("Poorest","Poorer","Middle","Richer","Richest")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Wealth quintile", y = "Percent within quintile")
chisq.test(stunting_wealth_tab)

# Wasting
cat("\nWasting by wealth quintiles:\n")
wasting_wealth_tab <- table(bdhs_final$wasted, bdhs_final$wealth_quintile)
wasting_wealth_tab
print(prop.table(wasting_wealth_tab, 2) * 100)
ggplot(bdhs_final, aes(wealth_quintile, fill = factor(wasted))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    name = "",
    breaks = c("0","1"),
    labels = c("0" = "No Wasting", "1" = "Wasting"),
    values = c("0" = "chartreuse4",  
               "1" = "brown"), 
  ) +
  scale_x_discrete(limits = c("Poorest","Poorer","Middle","Richer","Richest")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Wealth quintile", y = "Percent within quintile")
chisq.test(wasting_wealth_tab)

# Underweight
cat("\nUnderweight by wealth quintiles:\n")
uw_wealth_tab <- table(bdhs_final$underweight, bdhs_final$wealth_quintile)
uw_wealth_tab
print(prop.table(uw_wealth_tab, 2) * 100)
chisq.test(uw_wealth_tab)

# Trend test using numeric wealth index
cat("\n--- Trend Test: Wealth as Continuous Variable ---\n")

# Correlation between wealth index and malnutrition
cor.test(bdhs_final$wealth_urban_rural, bdhs_final$stunted)
cor.test(bdhs_final$wealth_urban_rural, bdhs_final$wasted)
cor.test(bdhs_final$wealth_urban_rural, bdhs_final$underweight)

# PART 3: OBJECTIVE 2 - HOUSEHOLD CONDITIONS AND MALNUTRITION
cat("\n==================== OBJECTIVE 2A: HOUSEHOLD STRUCTURES (HEAD) ====================\n")

# T-test: Compare head age between malnourished vs normal
cat("\n--- T-tests: Head Age by Malnutrition Status ---\n")

# Stunting
stunted_head_age <- bdhs_final$head_age[bdhs_final$stunted == 1]
normal_head_age <- bdhs_final$head_age[bdhs_final$any_malnutrition == 0]
t.test(stunted_head_age, normal_head_age)

df_stunting <- data.frame(
  group = c(rep("Stunting", length(stunted_head_age)),
            rep("No Stunting",  length(normal_head_age))),
  head_age = c(stunted_head_age, normal_head_age)
)

ggplot(df_stunting, aes(group, head_age, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Stunting" = "chartreuse4", "Stunting" = "brown")) +
  labs(x = NULL, y = "Head age (months)", fill = NULL) +
  theme_minimal() + theme(legend.position = "none")

# Wasting  
wasted_head_age <- bdhs_final$head_age[bdhs_final$wasted == 1]
t.test(wasted_head_age, normal_head_age)

df_wasting <- data.frame(
  group = c(rep("Wasting", length(wasted_head_age)),
            rep("No Wasting",  length(normal_head_age))),
  head_age = c(wasted_head_age, normal_head_age)
)

ggplot(df_wasting, aes(group, head_age, fill = group)) +
  geom_violin(trim = TRUE, width = 0.9, alpha = 0.35, color = NA) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.06) +
  scale_fill_manual(values = c("No Wasting" = "chartreuse4", "Wasting" = "brown")) +
  labs(x = NULL, y = "Head age (months)", fill = NULL) +
  theme_minimal() + theme(legend.position = "none")

# Underweight  
uw_head_age <- bdhs_final$head_age[bdhs_final$underweight == 1]
t.test(uw_head_age, normal_head_age)

# Head Sex Analysis
cat("\n--- Chi-square Test: Head Sex ---\n")
cat("Stunting & Head Sex")
chisq.test(table(bdhs_final$stunted, bdhs_final$head_sex))

cat("Wasting & Head Sex")
chisq.test(table(bdhs_final$wasted, bdhs_final$head_sex))

chisq.test(table(bdhs_final$underweight, bdhs_final$head_sex))

# Relationship to head Analysis
cat("\n--- Chi-square Test: Relationship ---\n")
cat("Stunting & Relationship")
chisq.test(table(bdhs_final$stunted, bdhs_final$relationship))

cat("Wasting & Relationship")
chisq.test(table(bdhs_final$wasted, bdhs_final$relationship))

chisq.test(table(bdhs_final$underweight, bdhs_final$relationship))

cat("\n==================== OBJECTIVE 2B: HOUSEHOLD SIZE ====================\n")

# Descriptive statistics
cat("\n--- Household Size Distribution ---\n")
cat("Mean:", mean(bdhs_final$household_members), "\n")
cat("Median:", median(bdhs_final$household_members), "\n")
cat("SD:", sd(bdhs_final$household_members), "\n")

# T-test: Compare household size between malnourished vs normal
cat("\n--- T-tests: Household Size by Malnutrition Status ---\n")

# Stunting
stunted_hh_size <- bdhs_final$household_members[bdhs_final$stunted == 1]
normal_hh_size <- bdhs_final$household_members[bdhs_final$any_malnutrition == 0]
t.test(stunted_hh_size, normal_hh_size)

# Wasting
wasted_hh_size <- bdhs_final$household_members[bdhs_final$wasted == 1]
t.test(wasted_hh_size, normal_hh_size)

# Underweight
uw_hh_size <- bdhs_final$household_members[bdhs_final$underweight == 1]
t.test(uw_hh_size, normal_hh_size)

# Categorical household size
cat("\n--- Chi-square Test: Household Size Categories ---\n")
cat("Stunting & Household size")
chisq.test(table(bdhs_final$stunted, bdhs_final$household_size_cat))
cat("Wasting & Household size")
chisq.test(table(bdhs_final$wasted, bdhs_final$household_size_cat))

chisq.test(table(bdhs_final$underweight, bdhs_final$household_size_cat))

cat("\n==================== OBJECTIVE 2C: NUMBER OF CHILDREN ====================\n")

# Test different children variables
cat("\n--- Comparing Different Children Variables ---\n")

# 1. Total children ever born
cat("\n1. Total children ever born:\n")
cat("Mean:", mean(bdhs_final$total_children_born), "\n")
cor.test(bdhs_final$total_children_born, bdhs_final$stunted)
cor.test(bdhs_final$total_children_born, bdhs_final$wasted)
cor.test(bdhs_final$total_children_born, bdhs_final$underweight)

# 2. Births in last 5 years
cat("\n2. Births in last 5 years:\n")
cat("Mean:", mean(bdhs_final$births_last5y), "\n")
cor.test(bdhs_final$births_last5y, bdhs_final$stunted)
cor.test(bdhs_final$births_last5y, bdhs_final$wasted)
cor.test(bdhs_final$births_last5y, bdhs_final$underweight)

# 3. Living children
cat("\n3. Living children:\n")
cat("Mean:", mean(bdhs_final$living_children), "\n")
cor.test(bdhs_final$living_children, bdhs_final$stunted)
cor.test(bdhs_final$living_children, bdhs_final$wasted)
cor.test(bdhs_final$living_children, bdhs_final$underweight)


# T-tests for total children ever born (most relevant)
cat("\n--- T-tests: Children by Malnutrition Status ---\n")

# Stunting
stunted_children <- bdhs_final$total_children_born[bdhs_final$stunted == 1]
normal_children <- bdhs_final$total_children_born[bdhs_final$any_malnutrition == 0]
t.test(stunted_children, normal_children)

# Wasting
wasted_children <- bdhs_final$births_last5y[bdhs_final$wasted == 1]
t.test(wasted_children, normal_children)

# Underweight
uw_children <- bdhs_final$births_last5y[bdhs_final$underweight == 1]
t.test(uw_children, normal_children)

# Chi-square for categorical
cat("\n--- Chi-square Test: Children Categories ---\n")
chisq.test(table(bdhs_final$stunted, bdhs_final$children_cat))
chisq.test(table(bdhs_final$wasted, bdhs_final$children_cat))
chisq.test(table(bdhs_final$underweight, bdhs_final$children_cat))


# PART 4: OBJECTIVE 3 - WEALTH AND HOUSEHOLD COMPOSITION ASSOCIATION

cat("\n==================== OBJECTIVE 3: WEALTH-HOUSEHOLD ASSOCIATION ====================\n")

# Correlation analysis
cat("\n--- Correlations with Wealth ---\n")

cor_wealth_hh <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$household_members)
cat("Wealth and HH size: r =", cor_wealth_hh$estimate, ", p =", cor_wealth_hh$p.value, "\n")

cor_wealth_total_children <- cor.test(bdhs_final$wealth_urban_rural, bdhs_final$total_children_born)
cat("Wealth and total children born: r =", cor_wealth_total_children$estimate, ", p =", cor_wealth_total_children$p.value, "\n")

# Cross-tabulation
cat("\n--- Cross-tabulation: Wealth by HH Size Category ---\n")
wealth_hh_table <- table(bdhs_final$wealth_quintile, bdhs_final$household_size_cat)
print(prop.table(wealth_hh_table, 1) * 100)
chisq.test(wealth_hh_table)

cat("\n--- Cross-tabulation: Wealth by Children Number Category ---\n")
wealth_child_table <- table(bdhs_final$wealth_quintile, bdhs_final$children_cat)
print(prop.table(wealth_child_table, 1) * 100)
chisq.test(wealth_child_table)

# PART 5: OBJECTIVE 4 - PARENT EDUCATION AND MALNUTRITION

cat("\n==================== OBJECTIVE 4: PARENT EDUCATION ====================\n")

# Descriptive statistics
cat("\n--- Education Distribution ---\n")
cat("Mother's education - Mean:", mean(bdhs_final$mother_edu_years), "years\n")
cat("Father's education - Mean:", mean(bdhs_final$father_edu_years), "years\n")
cat("Average parent education - Mean:", mean(bdhs_final$average_parent_edu), "years\n")

# T-tests for education by malnutrition status
cat("\n--- T-tests: Education by Stunting Status ---\n")

# Mother's education
stunted_mother_edu <- bdhs_final$mother_edu_years[bdhs_final$stunted == 1]
normal_mother_edu <- bdhs_final$mother_edu_years[bdhs_final$any_malnutrition == 0]
t_test_mother <- t.test(stunted_mother_edu, normal_mother_edu)
cat("\nMother's education:\n")
cat("  Stunted:", mean(stunted_mother_edu, na.rm=T), "years\n")
cat("  Normal:", mean(normal_mother_edu, na.rm=T), "years\n")
cat("  T-test: t =", t_test_mother$statistic, ", p =", t_test_mother$p.value, "\n")

# Father's education
stunted_father_edu <- bdhs_final$father_edu_years[bdhs_final$stunted == 1]
normal_father_edu <- bdhs_final$father_edu_years[bdhs_final$any_malnutrition == 0]
t_test_father <- t.test(stunted_father_edu, normal_father_edu)
cat("\nFather's education:\n")
cat("  Stunted:", mean(stunted_father_edu, na.rm=T), "years\n")
cat("  Normal:", mean(normal_father_edu, na.rm=T), "years\n")
cat("  T-test: t =", t_test_father$statistic, ", p =", t_test_father$p.value, "\n")

# Average parent education
stunted_avg_edu <- bdhs_final$average_parent_edu[bdhs_final$stunted == 1]
normal_avg_edu <- bdhs_final$average_parent_edu[bdhs_final$any_malnutrition == 0]
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

# We will use average parent education years as the final one since parent's impact on malnutrition is basically same

################################################################################
# PART 6: URBAN vs RURAL DIFFERENCES
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

uw_residence <- table(bdhs_final$underweight, bdhs_final$residence)
cat("\nUnderweight prevalence:\n")
print(prop.table(uw_residence, 2) * 100)
uw_res_test <- chisq.test(uw_residence)
cat("Chi-square test: X² =", uw_res_test$statistic, ", p =", uw_res_test$p.value, "\n")

# It is a bit significant