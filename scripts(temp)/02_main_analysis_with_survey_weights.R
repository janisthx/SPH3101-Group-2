## Main Analysis Script with Survey Weights
## Date: 2025
## Purpose: Analyze malnutrition outcomes using proper survey-weighted methods

# Load necessary libraries
library(survey)
library(tidyverse)
library(car)      # For VIF
library(pROC)     # For ROC curves
library(ggplot2)
library(margins)  # For marginal effects

# Read cleaned data
bdhs_clean <- read.csv("data/bdhs_clean_improved.csv")

# ========== CRITICAL: CREATE SURVEY DESIGN OBJECT ==========
# This accounts for the complex survey design and weights
bdhs_design <- svydesign(
  ids = ~psu,                    # Primary sampling unit
  strata = ~strata,              # Strata
  weights = ~survey_weight,      # Survey weights
  data = bdhs_clean,
  nest = TRUE                    # PSUs are nested within strata
)

cat("Survey design object created successfully\n")
cat("Total weighted population size:", sum(weights(bdhs_design)), "\n\n")

# ========== DESCRIPTIVE STATISTICS (WEIGHTED) ==========

cat("========== WEIGHTED DESCRIPTIVE STATISTICS ==========\n\n")

# Malnutrition prevalence
cat("Weighted malnutrition prevalence:\n")
print(svymean(~stunting + wasting + underweight, bdhs_design, na.rm = TRUE))

# By residence
cat("\nStunting by residence:\n")
print(svyby(~stunting, ~residence, bdhs_design, svymean, na.rm = TRUE))

# By wealth quintile
cat("\nStunting by wealth quintile:\n")
print(svyby(~stunting, ~wealth, bdhs_design, svymean, na.rm = TRUE))

# By number of children under 5
cat("\nStunting by number of children under 5:\n")
print(svyby(~stunting, ~children_under5, bdhs_design, svymean, na.rm = TRUE))

# ========== MODEL BUILDING STRATEGY ==========

cat("\n========== BUILDING MODELS WITH SURVEY WEIGHTS ==========\n\n")

# Model 1: Base model with child age and sex (ALWAYS include these!)
model_1_stunting <- svyglm(
  stunting ~ child_age_months + child_sex,
  design = bdhs_design,
  family = quasibinomial()
)

cat("Model 1 - Base model (child age + sex):\n")
print(summary(model_1_stunting))

# Model 2: Add household characteristics
model_2_stunting <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence,
  design = bdhs_design,
  family = quasibinomial()
)

cat("\nModel 2 - With household characteristics:\n")
print(summary(model_2_stunting))

# Model 3: Add parental education
model_3_stunting <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence +
    average_parent_edu,
  design = bdhs_design,
  family = quasibinomial()
)

cat("\nModel 3 - With parental education:\n")
print(summary(model_3_stunting))

# Model 4: Add WASH variables (CRITICAL!)
model_4_stunting <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence +
    average_parent_edu +
    improved_water + improved_sanitation,
  design = bdhs_design,
  family = quasibinomial()
)

cat("\nModel 4 - With WASH variables:\n")
print(summary(model_4_stunting))

# Model 5: Add maternal health
model_5_stunting <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence +
    average_parent_edu +
    improved_water + improved_sanitation +
    mother_age + mother_bmi + any_prenatal,
  design = bdhs_design,
  family = quasibinomial()
)

cat("\nModel 5 - With maternal health:\n")
print(summary(model_5_stunting))

# Model 6: Add child health indicators
model_6_stunting <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + household_members + residence +
    average_parent_edu +
    improved_water + improved_sanitation +
    mother_age + mother_bmi + any_prenatal +
    recent_diarrhea + vitamin_a,
  design = bdhs_design,
  family = quasibinomial()
)

cat("\nModel 6 - With child health:\n")
print(summary(model_6_stunting))

# ========== TEST INTERACTIONS (RESEARCH QUESTION!) ==========

cat("\n========== TESTING INTERACTION EFFECTS ==========\n\n")

# Interaction 1: Household size × Residence
model_interaction_1 <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 * residence +  # INTERACTION!
    average_parent_edu +
    improved_water + improved_sanitation +
    mother_bmi,
  design = bdhs_design,
  family = quasibinomial()
)

cat("Interaction Model 1 - Children × Residence:\n")
print(summary(model_interaction_1))

# Test if interaction is significant
anova(model_4_stunting, model_interaction_1)

# Interaction 2: Parental education × Residence
model_interaction_2 <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 + 
    average_parent_edu * residence +  # INTERACTION!
    improved_water + improved_sanitation +
    mother_bmi,
  design = bdhs_design,
  family = quasibinomial()
)

cat("\nInteraction Model 2 - Education × Residence:\n")
print(summary(model_interaction_2))

# Interaction 3: Multiple interactions
model_interaction_3 <- svyglm(
  stunting ~ child_age_months + child_sex + 
    children_under5 * residence +  # INTERACTION 1
    average_parent_edu * residence +  # INTERACTION 2
    improved_water + improved_sanitation +
    mother_bmi,
  design = bdhs_design,
  family = quasibinomial()
)

cat("\nInteraction Model 3 - Multiple interactions:\n")
print(summary(model_interaction_3))

# ========== STRATIFIED ANALYSIS BY WEALTH ==========

cat("\n========== STRATIFIED ANALYSIS BY WEALTH QUINTILE ==========\n\n")

# Create separate models for each wealth quintile
wealth_models <- list()

for(w in unique(bdhs_clean$wealth)) {
  if(!is.na(w)) {
    # Subset the design
    subset_design <- subset(bdhs_design, wealth == w)
    
    # Fit model
    wealth_models[[w]] <- svyglm(
      stunting ~ child_age_months + child_sex + 
        children_under5 + residence + average_parent_edu +
        improved_water + improved_sanitation,
      design = subset_design,
      family = quasibinomial()
    )
    
    cat(paste0("\n--- Model for ", w, " wealth group ---\n"))
    print(coef(summary(wealth_models[[w]])))
  }
}

# ========== MODEL DIAGNOSTICS ==========

cat("\n========== MODEL DIAGNOSTICS ==========\n\n")

# For the best model (let's use model_interaction_1)
best_model <- model_interaction_1

# 1. Extract odds ratios with confidence intervals
cat("Odds Ratios with 95% CI:\n")
OR_table <- exp(cbind(
  OR = coef(best_model),
  confint(best_model)
))
print(round(OR_table, 3))

# 2. Pseudo R-squared (for survey-weighted GLM)
# McFadden's pseudo R-squared
null_model <- svyglm(
  stunting ~ 1,
  design = bdhs_design,
  family = quasibinomial()
)
pseudo_r2 <- 1 - (best_model$deviance / null_model$deviance)
cat("\nMcFadden's Pseudo R-squared:", round(pseudo_r2, 4), "\n")

# 3. Model comparison using AIC
cat("\nModel Comparison (AIC):\n")
model_list <- list(
  "Base" = model_1_stunting,
  "Household" = model_2_stunting,
  "Education" = model_3_stunting,
  "WASH" = model_4_stunting,
  "Maternal" = model_5_stunting,
  "Full" = model_6_stunting,
  "Interaction1" = model_interaction_1
)

aic_values <- sapply(model_list, AIC)
print(sort(aic_values))

# ========== VISUALIZATION OF INTERACTIONS ==========

cat("\n========== CREATING INTERACTION PLOTS ==========\n\n")

# Predict probabilities for interaction effect
# Create prediction data frame
pred_data <- expand.grid(
  children_under5 = 0:4,
  residence = c("Urban", "Rural"),
  child_age_months = mean(bdhs_clean$child_age_months, na.rm = TRUE),
  child_sex = "Male",
  average_parent_edu = mean(bdhs_clean$average_parent_edu, na.rm = TRUE),
  improved_water = 1,
  improved_sanitation = 1,
  mother_bmi = mean(bdhs_clean$mother_bmi, na.rm = TRUE)
)

# Get predictions
predictions <- predict(model_interaction_1, newdata = pred_data, type = "response", se.fit = TRUE)
pred_data$probability <- predictions$fit
pred_data$se <- predictions$se.fit
pred_data$lower <- pred_data$probability - 1.96 * pred_data$se
pred_data$upper <- pred_data$probability + 1.96 * pred_data$se

# Create interaction plot
interaction_plot <- ggplot(pred_data, 
                          aes(x = children_under5, 
                              y = probability,
                              color = residence,
                              group = residence)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = residence), 
              alpha = 0.2, linetype = 0) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Interaction: Number of Children Under 5 × Residence on Stunting",
    subtitle = "Predicted probability of stunting with 95% confidence intervals",
    x = "Number of Children Under 5 in Household",
    y = "Probability of Stunting",
    color = "Residence",
    fill = "Residence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

print(interaction_plot)

# Save plot
ggsave("outputs/interaction_plot_children_residence.png", 
       interaction_plot, 
       width = 10, height = 6, dpi = 300)

# ========== SIMILAR ANALYSIS FOR WASTING ==========

cat("\n========== WASTING ANALYSIS ==========\n\n")

# Main effects model for wasting
model_wasting <- svyglm(
  wasting ~ child_age_months + child_sex + 
    children_under5 + residence +
    average_parent_edu +
    improved_water + improved_sanitation +
    mother_bmi + recent_diarrhea,
  design = bdhs_design,
  family = quasibinomial()
)

cat("Wasting Model:\n")
print(summary(model_wasting))

# Interaction model for wasting
model_wasting_interaction <- svyglm(
  wasting ~ child_age_months + child_sex + 
    children_under5 * residence +
    average_parent_edu +
    improved_water + improved_sanitation +
    mother_bmi + recent_diarrhea,
  design = bdhs_design,
  family = quasibinomial()
)

cat("\nWasting Model with Interaction:\n")
print(summary(model_wasting_interaction))

# ========== SAVE RESULTS ==========

# Save model summaries to file
sink("outputs/model_results_summary.txt")

cat("========================================\n")
cat("BANGLADESH DHS MALNUTRITION ANALYSIS\n")
cat("Survey-Weighted Regression Results\n")
cat("========================================\n\n")

cat("BEST MODEL FOR STUNTING:\n")
print(summary(best_model))

cat("\n\nODDS RATIOS WITH 95% CI:\n")
print(round(OR_table, 3))

cat("\n\nMODEL FIT STATISTICS:\n")
cat("Pseudo R-squared:", round(pseudo_r2, 4), "\n")
cat("AIC:", AIC(best_model), "\n")

sink()

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat("Results saved to 'outputs/model_results_summary.txt'\n")
cat("Interaction plot saved to 'outputs/interaction_plot_children_residence.png'\n")
