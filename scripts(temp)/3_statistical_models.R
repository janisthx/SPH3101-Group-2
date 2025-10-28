################################################################################
# 3. Statistical Modeling with Survey Weights
# Bangladesh DHS Child Malnutrition Analysis
################################################################################

library(tidyverse)
library(survey)
library(lmtest)
library(car)
library(stargazer)
library(pROC)
library(broom)

# Load data and survey design
bdhs_final <- read.csv("bdhs_cleaned_final.csv")
survey_design <- readRDS("survey_design.rds")

# Create results directory
if (!dir.exists("results")) dir.create("results")
if (!dir.exists("tables")) dir.create("tables")

cat("\n========== STATISTICAL MODELING WITH SURVEY WEIGHTS ==========\n")

################################################################################
# PREPARE MODELING DATA
################################################################################

# Center continuous variables for better interpretation
bdhs_final$wealth_c <- scale(bdhs_final$wealth_urban_rural, center = TRUE, scale = FALSE)
bdhs_final$household_members_c <- scale(bdhs_final$household_members, center = TRUE, scale = FALSE)
bdhs_final$births_last5y_c <- scale(bdhs_final$births_last5y, center = TRUE, scale = FALSE)
bdhs_final$average_parent_edu_c <- scale(bdhs_final$average_parent_edu, center = TRUE, scale = FALSE)

# Update survey design with centered variables
survey_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight_normalized,
  data = bdhs_final,
  nest = TRUE
)

################################################################################
# PART A: BUILD MODELS BASED ON OBJECTIVES
################################################################################

cat("\n==================== MODEL BUILDING ====================\n")

# =========== STUNTING MODELS ===========

cat("\n--- STUNTING MODELS ---\n")

# Model 1: Wealth only (Objective 1)
stunting_m1 <- svyglm(stunted ~ wealth_urban_rural, 
                      design = survey_design, family = binomial)
cat("\nModel 1 - Wealth only:\n")
print(summary(stunting_m1)$coefficients)

# Model 2: Household size only (Objective 2a)
stunting_m2 <- svyglm(stunted ~ household_members, 
                      design = survey_design, family = binomial)
cat("\nModel 2 - Household size only:\n")
print(summary(stunting_m2)$coefficients)

# Model 3: Number of children only (Objective 2b)
stunting_m3 <- svyglm(stunted ~ births_last5y, 
                      design = survey_design, family = binomial)
cat("\nModel 3 - Children under 5 only:\n")
print(summary(stunting_m3)$coefficients)

# Model 4: Education only (Objective 4)
stunting_m4 <- svyglm(stunted ~ average_parent_edu, 
                      design = survey_design, family = binomial)
cat("\nModel 4 - Parent education only:\n")
print(summary(stunting_m4)$coefficients)

# Model 5: Wealth + Household size (testing association)
stunting_m5 <- svyglm(stunted ~ wealth_urban_rural + household_members, 
                      design = survey_design, family = binomial)
cat("\nModel 5 - Wealth + Household size:\n")
print(summary(stunting_m5)$coefficients)

# Model 6: Wealth + Children (testing association)
stunting_m6 <- svyglm(stunted ~ wealth_urban_rural + births_last5y, 
                      design = survey_design, family = binomial)
cat("\nModel 6 - Wealth + Children:\n")
print(summary(stunting_m6)$coefficients)

# Model 7: Final model with all key variables
stunting_final <- svyglm(stunted ~ wealth_urban_rural + births_last5y + average_parent_edu, 
                         design = survey_design, family = binomial)
cat("\nFINAL MODEL - All key variables:\n")
print(summary(stunting_final))

# Model 8: Final model with controls
stunting_full <- svyglm(stunted ~ wealth_urban_rural + births_last5y + average_parent_edu + 
                        residence + child_sex + child_age_months, 
                        design = survey_design, family = binomial)
cat("\nFULL MODEL - With controls:\n")
print(summary(stunting_full))

# =========== WASTING MODELS ===========

cat("\n--- WASTING MODELS ---\n")

# Similar models for wasting
wasting_m1 <- svyglm(wasted ~ wealth_urban_rural, design = survey_design, family = binomial)
wasting_m2 <- svyglm(wasted ~ household_members, design = survey_design, family = binomial)
wasting_m3 <- svyglm(wasted ~ births_last5y, design = survey_design, family = binomial)
wasting_m4 <- svyglm(wasted ~ average_parent_edu, design = survey_design, family = binomial)
wasting_final <- svyglm(wasted ~ wealth_urban_rural + births_last5y + average_parent_edu, 
                        design = survey_design, family = binomial)
wasting_full <- svyglm(wasted ~ wealth_urban_rural + births_last5y + average_parent_edu + 
                       residence + child_sex + child_age_months, 
                       design = survey_design, family = binomial)

################################################################################
# PART B: MODEL COMPARISON AND SELECTION
################################################################################

cat("\n==================== MODEL COMPARISON ====================\n")

# AIC comparison for stunting models
stunting_models <- list(
  "Wealth only" = stunting_m1,
  "HH size only" = stunting_m2,
  "Children only" = stunting_m3,
  "Education only" = stunting_m4,
  "Wealth + HH size" = stunting_m5,
  "Wealth + Children" = stunting_m6,
  "Final model" = stunting_final,
  "Full model" = stunting_full
)

aic_stunting <- sapply(stunting_models, AIC)
cat("\nAIC Comparison - Stunting Models:\n")
print(sort(aic_stunting))

# Best model
best_stunting <- names(aic_stunting)[which.min(aic_stunting)]
cat(paste("\nBest stunting model (lowest AIC):", best_stunting, "\n"))

# AIC comparison for wasting models
wasting_models <- list(
  "Wealth only" = wasting_m1,
  "HH size only" = wasting_m2,
  "Children only" = wasting_m3,
  "Education only" = wasting_m4,
  "Final model" = wasting_final,
  "Full model" = wasting_full
)

aic_wasting <- sapply(wasting_models, AIC)
cat("\nAIC Comparison - Wasting Models:\n")
print(sort(aic_wasting))

################################################################################
# PART C: TESTING VARIABLE IMPORTANCE
################################################################################

cat("\n==================== VARIABLE IMPORTANCE ====================\n")

# Likelihood ratio tests for nested models
cat("\nLikelihood Ratio Tests - Adding variables sequentially:\n")

# Test 1: Does household size improve wealth model?
lr_test1 <- anova(stunting_m1, stunting_m5, test = "Chisq")
cat("\nAdding household size to wealth model:\n")
print(lr_test1)

# Test 2: Does children number improve wealth model?
lr_test2 <- anova(stunting_m1, stunting_m6, test = "Chisq")
cat("\nAdding children to wealth model:\n")
print(lr_test2)

# Test 3: Compare household size vs children
cat("\nComparing household size vs children (when added to wealth):\n")
cat("  Wealth + HH size AIC:", AIC(stunting_m5), "\n")
cat("  Wealth + Children AIC:", AIC(stunting_m6), "\n")
cat("  Better variable:", ifelse(AIC(stunting_m5) < AIC(stunting_m6), "Household size", "Children"), "\n")

# Test 4: Does education improve wealth+children model?
lr_test4 <- anova(stunting_m6, stunting_final, test = "Chisq")
cat("\nAdding education to wealth+children model:\n")
print(lr_test4)

################################################################################
# PART D: EXTRACT ODDS RATIOS AND CONFIDENCE INTERVALS
################################################################################

cat("\n==================== ODDS RATIOS ====================\n")

# Function to extract OR and CI
extract_OR <- function(model, model_name) {
  coef_summary <- summary(model)$coefficients
  OR <- exp(coef_summary[, "Estimate"])
  CI_lower <- exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"])
  CI_upper <- exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"])
  p_value <- coef_summary[, "Pr(>|t|)"]
  
  results <- data.frame(
    Model = model_name,
    Variable = names(OR),
    OR = round(OR, 3),
    CI_Lower = round(CI_lower, 3),
    CI_Upper = round(CI_upper, 3),
    P_value = round(p_value, 4),
    Significant = ifelse(p_value < 0.05, "*", "")
  )
  return(results)
}

# Extract OR for key models
stunting_final_OR <- extract_OR(stunting_final, "Stunting - Final")
stunting_full_OR <- extract_OR(stunting_full, "Stunting - Full")
wasting_final_OR <- extract_OR(wasting_final, "Wasting - Final")
wasting_full_OR <- extract_OR(wasting_full, "Wasting - Full")

cat("\nSTUNTING - Final Model Odds Ratios:\n")
print(stunting_final_OR)

cat("\nWASTING - Final Model Odds Ratios:\n")
print(wasting_final_OR)

# Combine all OR results
all_OR <- rbind(stunting_final_OR, stunting_full_OR, wasting_final_OR, wasting_full_OR)
write.csv(all_OR, "results/odds_ratios.csv", row.names = FALSE)

################################################################################
# PART E: MODEL DIAGNOSTICS
################################################################################

cat("\n==================== MODEL DIAGNOSTICS ====================\n")

# Function for model diagnostics
perform_diagnostics <- function(model, model_name, data) {
  cat(paste("\nDiagnostics for", model_name, ":\n"))
  cat("--------------------------------\n")
  
  # Pseudo R-squared (Nagelkerke)
  null_model <- svyglm(as.formula(paste(all.vars(formula(model))[1], "~ 1")), 
                       design = survey_design, family = binomial)
  null_deviance <- deviance(null_model)
  model_deviance <- deviance(model)
  n <- nrow(model$data)
  nagelkerke_r2 <- (1 - exp((model_deviance - null_deviance) / n)) / 
                    (1 - exp(-null_deviance / n))
  cat(paste("Nagelkerke R²:", round(nagelkerke_r2, 4), "\n"))
  
  # AIC and BIC
  cat(paste("AIC:", round(AIC(model), 2), "\n"))
  cat(paste("BIC:", round(BIC(model), 2), "\n"))
  
  # Check for multicollinearity (VIF) - only for models with multiple predictors
  if (length(coef(model)) > 2) {
    # Create a regular glm for VIF calculation
    regular_model <- glm(formula(model), data = data, family = binomial)
    vif_values <- car::vif(regular_model)
    cat("\nVariance Inflation Factors:\n")
    if (is.matrix(vif_values)) {
      print(round(vif_values[,1], 2))
    } else {
      print(round(vif_values, 2))
    }
    
    if (any(vif_values > 5)) {
      cat("WARNING: VIF > 5 detected - potential multicollinearity\n")
    }
  }
  
  return(list(nagelkerke_r2 = nagelkerke_r2, aic = AIC(model), bic = BIC(model)))
}

# Perform diagnostics for final models
diag_stunting <- perform_diagnostics(stunting_final, "Stunting - Final", bdhs_final)
diag_wasting <- perform_diagnostics(wasting_final, "Wasting - Final", bdhs_final)

################################################################################
# PART F: INTERPRETATION OF KEY FINDINGS
################################################################################

cat("\n==================== KEY FINDINGS SUMMARY ====================\n")

# Wealth effect
wealth_OR_stunting <- exp(coef(stunting_final)["wealth_urban_rural"])
wealth_OR_wasting <- exp(coef(wasting_final)["wealth_urban_rural"])

cat("\nOBJECTIVE 1 - Wealth Effect:\n")
cat("-----------------------------\n")
cat(paste("Stunting: Each unit increase in wealth quintile reduces odds by", 
          round((1 - wealth_OR_stunting) * 100, 1), "%\n"))
cat(paste("Wasting: Each unit increase in wealth quintile reduces odds by", 
          round((1 - wealth_OR_wasting) * 100, 1), "%\n"))

# Household composition effect
children_OR_stunting <- exp(coef(stunting_final)["births_last5y"])
children_OR_wasting <- exp(coef(wasting_final)["births_last5y"])

cat("\nOBJECTIVE 2 - Household Composition Effect:\n")
cat("--------------------------------------------\n")
cat(paste("Stunting: Each additional child under 5 increases odds by", 
          round((children_OR_stunting - 1) * 100, 1), "%\n"))
cat(paste("Wasting: Each additional child under 5 changes odds by", 
          round((children_OR_wasting - 1) * 100, 1), "%\n"))

# Education effect
edu_OR_stunting <- exp(coef(stunting_final)["average_parent_edu"])
edu_OR_wasting <- exp(coef(wasting_final)["average_parent_edu"])

cat("\nOBJECTIVE 4 - Education Effect:\n")
cat("--------------------------------\n")
cat(paste("Stunting: Each year of parent education reduces odds by", 
          round((1 - edu_OR_stunting) * 100, 1), "%\n"))
cat(paste("Wasting: Each year of parent education reduces odds by", 
          round((1 - edu_OR_wasting) * 100, 1), "%\n"))

# Check if wealth effect changes when controlling for household composition
cat("\nOBJECTIVE 3 - Wealth-Household Association:\n")
cat("--------------------------------------------\n")
wealth_alone <- coef(stunting_m1)["wealth_urban_rural"]
wealth_with_children <- coef(stunting_m6)["wealth_urban_rural"]
change_pct <- ((wealth_with_children - wealth_alone) / wealth_alone) * 100

cat(paste("Wealth coefficient alone:", round(wealth_alone, 4), "\n"))
cat(paste("Wealth coefficient with children:", round(wealth_with_children, 4), "\n"))
cat(paste("Change in wealth effect:", round(change_pct, 1), "%\n"))
cat("Interpretation: Controlling for number of children", 
    ifelse(abs(change_pct) > 10, "substantially", "slightly"),
    "changes the wealth effect\n")

################################################################################
# SAVE FINAL MODEL RESULTS
################################################################################

# Create summary table
model_summary <- data.frame(
  Model = c("Stunting-Final", "Stunting-Full", "Wasting-Final", "Wasting-Full"),
  AIC = c(AIC(stunting_final), AIC(stunting_full), AIC(wasting_final), AIC(wasting_full)),
  Nagelkerke_R2 = c(diag_stunting$nagelkerke_r2, NA, diag_wasting$nagelkerke_r2, NA),
  Wealth_OR = c(
    exp(coef(stunting_final)["wealth_urban_rural"]),
    exp(coef(stunting_full)["wealth_urban_rural"]),
    exp(coef(wasting_final)["wealth_urban_rural"]),
    exp(coef(wasting_full)["wealth_urban_rural"])
  ),
  Children_OR = c(
    exp(coef(stunting_final)["births_last5y"]),
    exp(coef(stunting_full)["births_last5y"]),
    exp(coef(wasting_final)["births_last5y"]),
    exp(coef(wasting_full)["births_last5y"])
  ),
  Education_OR = c(
    exp(coef(stunting_final)["average_parent_edu"]),
    exp(coef(stunting_full)["average_parent_edu"]),
    exp(coef(wasting_final)["average_parent_edu"]),
    exp(coef(wasting_full)["average_parent_edu"])
  )
)

write.csv(model_summary, "results/model_summary.csv", row.names = FALSE)

# Save model objects for later use
saveRDS(list(
  stunting_final = stunting_final,
  stunting_full = stunting_full,
  wasting_final = wasting_final,
  wasting_full = wasting_full
), "results/final_models.rds")

cat("\n========== STATISTICAL MODELING COMPLETE ==========\n")
cat("Results saved in 'results' directory\n")
