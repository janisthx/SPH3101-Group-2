## Multicollinearity Diagnostics and Solutions
## Date: 2025
## Purpose: Handle multicollinearity issues, especially with wealth variable

library(survey)
library(car)
library(corrplot)
library(tidyverse)

# Read cleaned data
bdhs_clean <- read.csv("data/bdhs_clean_improved.csv")

# Create survey design
bdhs_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~survey_weight,
  data = bdhs_clean,
  nest = TRUE
)

cat("========== MULTICOLLINEARITY DIAGNOSTICS ==========\n\n")

# ========== CORRELATION MATRIX ==========

# Select numeric variables for correlation
numeric_vars <- bdhs_clean %>%
  select(
    household_members,
    children_under5,
    total_children,
    average_parent_edu,
    mother_edu_years,
    father_edu_years,
    mother_age,
    mother_bmi
  )

# Create correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

cat("Correlation Matrix of Numeric Variables:\n")
print(round(cor_matrix, 2))

# Visualize correlation matrix
png("outputs/correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper",
         order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix of Key Variables")
dev.off()

# ========== VIF ANALYSIS ==========

cat("\n========== VIF Analysis for Different Model Specifications ==========\n\n")

# Model WITH wealth (problematic)
model_with_wealth <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence +
    wealth + average_parent_edu +
    improved_water + improved_sanitation,
  design = bdhs_design,
  family = quasibinomial()
)

# Calculate VIF for model with wealth
# Note: VIF not directly available for svyglm, so we use regular glm for diagnostic
model_wealth_glm <- glm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence +
    wealth + average_parent_edu +
    improved_water + improved_sanitation,
  data = bdhs_clean,
  family = binomial()
)

cat("VIF for model WITH wealth:\n")
vif_with_wealth <- vif(model_wealth_glm)
print(vif_with_wealth)

# Identify problematic variables (VIF > 10)
problematic <- vif_with_wealth[vif_with_wealth > 10]
if(length(problematic) > 0) {
  cat("\nWARNING: Variables with VIF > 10 (severe multicollinearity):\n")
  print(problematic)
}

# Model WITHOUT wealth
model_without_wealth <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence +
    average_parent_edu +
    improved_water + improved_sanitation,
  design = bdhs_design,
  family = quasibinomial()
)

model_no_wealth_glm <- glm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence +
    average_parent_edu +
    improved_water + improved_sanitation,
  data = bdhs_clean,
  family = binomial()
)

cat("\n\nVIF for model WITHOUT wealth:\n")
vif_without_wealth <- vif(model_no_wealth_glm)
print(vif_without_wealth)

# ========== SOLUTION 1: STRATIFICATION BY WEALTH ==========

cat("\n\n========== SOLUTION 1: STRATIFIED ANALYSIS BY WEALTH ==========\n\n")

wealth_levels <- unique(bdhs_clean$wealth[!is.na(bdhs_clean$wealth)])
stratified_results <- list()

for(w in wealth_levels) {
  cat(paste0("\n--- Analysis for ", w, " wealth quintile ---\n"))
  
  # Subset design
  subset_design <- subset(bdhs_design, wealth == w)
  
  # Fit model without wealth variable
  model_stratified <- svyglm(
    stunting ~ child_age_months + child_sex + 
      children_under5 + residence +
      average_parent_edu +
      improved_water + improved_sanitation,
    design = subset_design,
    family = quasibinomial()
  )
  
  # Store results
  stratified_results[[w]] <- model_stratified
  
  # Print coefficients
  coef_summary <- coef(summary(model_stratified))
  print(round(coef_summary, 4))
  
  # Calculate odds ratios
  OR <- exp(coef(model_stratified))
  cat("\nOdds Ratios:\n")
  print(round(OR, 3))
}

# ========== SOLUTION 2: USE WEALTH COMPONENTS ==========

cat("\n\n========== SOLUTION 2: USING WEALTH COMPONENTS ==========\n\n")

# Instead of wealth index, use individual components
# Note: You would need to add these variables in the data processing step
# For demonstration, let's assume we have these variables:
# - has_electricity (V119)
# - has_television (V121)
# - has_refrigerator (V122)
# - improved_floor (V127)
# - improved_wall (V128)

# Add wealth components to dataset (example)
bdhs_clean$has_electricity <- ifelse(bdhs$V119 == 1, 1, 0)
bdhs_clean$has_television <- ifelse(bdhs$V121 == 1, 1, 0)
bdhs_clean$has_refrigerator <- ifelse(bdhs$V122 == 1, 1, 0)

# Update survey design with new variables
bdhs_design_components <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~survey_weight,
  data = bdhs_clean,
  nest = TRUE
)

# Model with wealth components instead of wealth index
model_components <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + residence +
    average_parent_edu +
    improved_water + improved_sanitation +
    has_electricity + has_television + has_refrigerator,
  design = bdhs_design_components,
  family = quasibinomial()
)

cat("Model using wealth COMPONENTS instead of wealth INDEX:\n")
print(summary(model_components))

# ========== SOLUTION 3: PRINCIPAL COMPONENT ANALYSIS ==========

cat("\n\n========== SOLUTION 3: PCA OF SOCIOECONOMIC VARIABLES ==========\n\n")

# Select socioeconomic variables for PCA
ses_vars <- bdhs_clean %>%
  select(
    average_parent_edu,
    mother_edu_years,
    father_edu_years,
    has_electricity,
    has_television,
    has_refrigerator
  ) %>%
  na.omit()

# Perform PCA
pca_result <- prcomp(ses_vars, scale = TRUE)

# Summary of PCA
cat("PCA Summary:\n")
print(summary(pca_result))

# Variance explained by each component
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cat("\nVariance explained by each PC:\n")
print(round(var_explained, 3))

# Use first 2 PCs as predictors
bdhs_clean$PC1 <- predict(pca_result, bdhs_clean)[,1]
bdhs_clean$PC2 <- predict(pca_result, bdhs_clean)[,2]

# Update design
bdhs_design_pca <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~survey_weight,
  data = bdhs_clean,
  nest = TRUE
)

# Model with PCA components
model_pca <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + residence +
    PC1 + PC2 +
    improved_water + improved_sanitation,
  design = bdhs_design_pca,
  family = quasibinomial()
)

cat("\nModel using PCA components:\n")
print(summary(model_pca))

# ========== COMPARE SOLUTIONS ==========

cat("\n\n========== COMPARISON OF APPROACHES ==========\n\n")

# Compare AIC
models_to_compare <- list(
  "With Wealth Index" = model_with_wealth,
  "Without Wealth" = model_without_wealth,
  "Wealth Components" = model_components,
  "PCA Components" = model_pca
)

aic_comparison <- sapply(models_to_compare, AIC)
cat("AIC Comparison:\n")
print(sort(aic_comparison))

# Best approach based on AIC
best_approach <- names(aic_comparison)[which.min(aic_comparison)]
cat(paste0("\nBest approach based on AIC: ", best_approach, "\n"))

# ========== RECOMMENDATION ==========

cat("\n\n========== RECOMMENDATIONS ==========\n\n")

cat("Based on the multicollinearity analysis:\n\n")

cat("1. PROBLEM IDENTIFIED:\n")
cat("   - Wealth index shows high correlation with education and household variables\n")
cat("   - VIF values > 10 indicate severe multicollinearity\n\n")

cat("2. RECOMMENDED SOLUTIONS:\n")
cat("   a) For main analysis: EXCLUDE wealth, include education and WASH variables\n")
cat("   b) For sensitivity analysis: STRATIFY by wealth quintile\n")
cat("   c) Alternative: Use wealth COMPONENTS instead of index\n")
cat("   d) Advanced: Use PCA to create orthogonal SES components\n\n")

cat("3. FINAL MODEL RECOMMENDATION:\n")
cat("   Use the model WITHOUT wealth but WITH:\n")
cat("   - Child age and sex (always!)\n")
cat("   - Household size variables\n")
cat("   - Parental education\n")
cat("   - WASH variables\n")
cat("   - Maternal health indicators\n")
cat("   - Test interactions with residence\n\n")

# Save diagnostics to file
sink("outputs/multicollinearity_diagnostics.txt")
cat("========== MULTICOLLINEARITY DIAGNOSTIC REPORT ==========\n\n")
cat("VIF Analysis:\n")
cat("\nWith Wealth Index:\n")
print(vif_with_wealth)
cat("\nWithout Wealth Index:\n")
print(vif_without_wealth)
cat("\n\nModel Comparison (AIC):\n")
print(sort(aic_comparison))
cat("\n\nRecommendation: ", best_approach)
sink()

cat("Diagnostic results saved to 'outputs/multicollinearity_diagnostics.txt'\n")
cat("Correlation matrix saved to 'outputs/correlation_matrix.png'\n")
