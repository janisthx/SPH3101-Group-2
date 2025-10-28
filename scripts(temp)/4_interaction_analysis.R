################################################################################
# 4. Urban-Rural Interaction Analysis
# Bangladesh DHS Child Malnutrition Analysis
################################################################################

library(tidyverse)
library(survey)
library(ggplot2)
library(ggeffects)
library(emmeans)
library(gridExtra)

# Load data and models
bdhs_final <- read.csv("bdhs_cleaned_final.csv")
survey_design <- readRDS("survey_design.rds")
models <- readRDS("results/final_models.rds")

# Create plots directory
if (!dir.exists("plots")) dir.create("plots")

cat("\n========== URBAN-RURAL INTERACTION ANALYSIS ==========\n")

################################################################################
# PART A: TEST INTERACTIONS WITH RESIDENCE
################################################################################

cat("\n==================== INTERACTION MODELS ====================\n")

# ========== STUNTING MODELS WITH INTERACTIONS ==========

cat("\n--- STUNTING: Testing Interactions with Residence ---\n")

# 1. Wealth × Residence interaction
stunting_wealth_int <- svyglm(stunted ~ wealth_urban_rural * residence + 
                              births_last5y + average_parent_edu,
                              design = survey_design, family = binomial)

cat("\n1. Wealth × Residence Interaction:\n")
cat("------------------------------------\n")
# Test significance of interaction
base_model <- svyglm(stunted ~ wealth_urban_rural + residence + 
                     births_last5y + average_parent_edu,
                     design = survey_design, family = binomial)
lr_test_wealth <- anova(base_model, stunting_wealth_int, test = "Chisq")
print(lr_test_wealth)
cat("Interaction coefficient:\n")
print(summary(stunting_wealth_int)$coefficients["wealth_urban_rural:residenceRural",])

# 2. Household size × Residence interaction
stunting_hhsize_int <- svyglm(stunted ~ household_members * residence + 
                              wealth_urban_rural + average_parent_edu,
                              design = survey_design, family = binomial)

cat("\n2. Household Size × Residence Interaction:\n")
cat("-------------------------------------------\n")
base_model2 <- svyglm(stunted ~ household_members + residence + 
                      wealth_urban_rural + average_parent_edu,
                      design = survey_design, family = binomial)
lr_test_hhsize <- anova(base_model2, stunting_hhsize_int, test = "Chisq")
print(lr_test_hhsize)
cat("Interaction coefficient:\n")
print(summary(stunting_hhsize_int)$coefficients["household_members:residenceRural",])

# 3. Number of children × Residence interaction
stunting_children_int <- svyglm(stunted ~ births_last5y * residence + 
                                wealth_urban_rural + average_parent_edu,
                                design = survey_design, family = binomial)

cat("\n3. Number of Children × Residence Interaction:\n")
cat("-----------------------------------------------\n")
base_model3 <- svyglm(stunted ~ births_last5y + residence + 
                      wealth_urban_rural + average_parent_edu,
                      design = survey_design, family = binomial)
lr_test_children <- anova(base_model3, stunting_children_int, test = "Chisq")
print(lr_test_children)
cat("Interaction coefficient:\n")
print(summary(stunting_children_int)$coefficients["births_last5y:residenceRural",])

# 4. Education × Residence interaction
stunting_edu_int <- svyglm(stunted ~ average_parent_edu * residence + 
                           wealth_urban_rural + births_last5y,
                           design = survey_design, family = binomial)

cat("\n4. Education × Residence Interaction:\n")
cat("--------------------------------------\n")
base_model4 <- svyglm(stunted ~ average_parent_edu + residence + 
                      wealth_urban_rural + births_last5y,
                      design = survey_design, family = binomial)
lr_test_edu <- anova(base_model4, stunting_edu_int, test = "Chisq")
print(lr_test_edu)
cat("Interaction coefficient:\n")
print(summary(stunting_edu_int)$coefficients["average_parent_edu:residenceRural",])

# ========== WASTING MODELS WITH INTERACTIONS ==========

cat("\n--- WASTING: Testing Interactions with Residence ---\n")

# Similar models for wasting
wasting_wealth_int <- svyglm(wasted ~ wealth_urban_rural * residence + 
                             births_last5y + average_parent_edu,
                             design = survey_design, family = binomial)

wasting_children_int <- svyglm(wasted ~ births_last5y * residence + 
                               wealth_urban_rural + average_parent_edu,
                               design = survey_design, family = binomial)

wasting_edu_int <- svyglm(wasted ~ average_parent_edu * residence + 
                         wealth_urban_rural + births_last5y,
                         design = survey_design, family = binomial)

################################################################################
# PART B: SUMMARY OF INTERACTION TESTS
################################################################################

cat("\n==================== INTERACTION TESTS SUMMARY ====================\n")

# Create summary table
interaction_summary <- data.frame(
  Outcome = rep(c("Stunting", "Wasting"), each = 4),
  Interaction = rep(c("Wealth × Residence", "HH Size × Residence", 
                     "Children × Residence", "Education × Residence"), 2),
  P_value = c(
    lr_test_wealth$`Pr(>F)`[2],
    lr_test_hhsize$`Pr(>F)`[2],
    lr_test_children$`Pr(>F)`[2],
    lr_test_edu$`Pr(>F)`[2],
    NA, NA, NA, NA  # Will be filled for wasting
  ),
  Significant = NA
)

# Add significance markers
interaction_summary$Significant <- ifelse(interaction_summary$P_value < 0.05, "Yes", "No")
interaction_summary$Significance <- ifelse(interaction_summary$P_value < 0.001, "***",
                                          ifelse(interaction_summary$P_value < 0.01, "**",
                                                ifelse(interaction_summary$P_value < 0.05, "*", "")))

cat("\nInteraction Test Results:\n")
cat("--------------------------\n")
print(interaction_summary[1:4,])  # Stunting results

# Print significant interactions
sig_interactions <- interaction_summary[interaction_summary$Significant == "Yes" & !is.na(interaction_summary$Significant), ]
if(nrow(sig_interactions) > 0) {
  cat("\nSIGNIFICANT INTERACTIONS DETECTED:\n")
  print(sig_interactions[, c("Outcome", "Interaction", "P_value")])
}

################################################################################
# PART C: STRATIFIED ANALYSIS BY RESIDENCE
################################################################################

cat("\n==================== STRATIFIED ANALYSIS ====================\n")

# Separate data by residence
urban_data <- bdhs_final[bdhs_final$residence == "Urban", ]
rural_data <- bdhs_final[bdhs_final$residence == "Rural", ]

# Create separate survey designs
urban_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight_normalized,
  data = urban_data,
  nest = TRUE
)

rural_design <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~weight_normalized,
  data = rural_data,
  nest = TRUE
)

cat("\n--- URBAN Models ---\n")
urban_stunting <- svyglm(stunted ~ wealth_urban_rural + births_last5y + average_parent_edu,
                        design = urban_design, family = binomial)
cat("Urban Stunting Model:\n")
print(summary(urban_stunting)$coefficients)

cat("\n--- RURAL Models ---\n")
rural_stunting <- svyglm(stunted ~ wealth_urban_rural + births_last5y + average_parent_edu,
                        design = rural_design, family = binomial)
cat("Rural Stunting Model:\n")
print(summary(rural_stunting)$coefficients)

# Compare coefficients
cat("\n==================== COEFFICIENT COMPARISON ====================\n")

coef_comparison <- data.frame(
  Variable = c("Wealth", "Children <5", "Education"),
  Urban_Beta = c(
    coef(urban_stunting)["wealth_urban_rural"],
    coef(urban_stunting)["births_last5y"],
    coef(urban_stunting)["average_parent_edu"]
  ),
  Rural_Beta = c(
    coef(rural_stunting)["wealth_urban_rural"],
    coef(rural_stunting)["births_last5y"],
    coef(rural_stunting)["average_parent_edu"]
  ),
  Urban_OR = exp(c(
    coef(urban_stunting)["wealth_urban_rural"],
    coef(urban_stunting)["births_last5y"],
    coef(urban_stunting)["average_parent_edu"]
  )),
  Rural_OR = exp(c(
    coef(rural_stunting)["wealth_urban_rural"],
    coef(rural_stunting)["births_last5y"],
    coef(rural_stunting)["average_parent_edu"]
  ))
)

coef_comparison$OR_Ratio <- coef_comparison$Rural_OR / coef_comparison$Urban_OR
coef_comparison$Different <- ifelse(abs(coef_comparison$OR_Ratio - 1) > 0.2, "Yes", "No")

cat("\nOdds Ratio Comparison (Urban vs Rural):\n")
cat("-----------------------------------------\n")
print(round(coef_comparison, 3))

################################################################################
# PART D: VISUALIZE SIGNIFICANT INTERACTIONS
################################################################################

cat("\n==================== VISUALIZING INTERACTIONS ====================\n")

# Function to create interaction plot
create_int_plot <- function(model, var_name, outcome) {
  # Create prediction data
  if(var_name == "wealth") {
    pred_data <- expand.grid(
      wealth_urban_rural = 1:5,
      residence = c("Urban", "Rural"),
      births_last5y = mean(bdhs_final$births_last5y, na.rm = TRUE),
      average_parent_edu = mean(bdhs_final$average_parent_edu, na.rm = TRUE)
    )
  } else if(var_name == "children") {
    pred_data <- expand.grid(
      births_last5y = 0:4,
      residence = c("Urban", "Rural"),
      wealth_urban_rural = 3,  # Middle wealth
      average_parent_edu = mean(bdhs_final$average_parent_edu, na.rm = TRUE)
    )
  } else if(var_name == "education") {
    pred_data <- expand.grid(
      average_parent_edu = seq(0, 12, by = 2),
      residence = c("Urban", "Rural"),
      wealth_urban_rural = 3,  # Middle wealth
      births_last5y = mean(bdhs_final$births_last5y, na.rm = TRUE)
    )
  }
  
  # Get predictions
  pred_data$predicted <- predict(model, newdata = pred_data, type = "response")
  
  # Create plot
  if(var_name == "wealth") {
    x_var <- "wealth_urban_rural"
    x_label <- "Wealth Quintile"
  } else if(var_name == "children") {
    x_var <- "births_last5y"
    x_label <- "Number of Children Under 5"
  } else {
    x_var <- "average_parent_edu"
    x_label <- "Average Parent Education (years)"
  }
  
  p <- ggplot(pred_data, aes_string(x = x_var, y = "predicted", 
                                    color = "residence", linetype = "residence")) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    labs(title = paste(outcome, "by", x_label, "and Residence"),
         x = x_label,
         y = paste("Predicted Probability of", outcome),
         color = "Residence",
         linetype = "Residence") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("Urban" = "#2E86AB", "Rural" = "#A23B72"))
  
  return(p)
}

# Create plots for significant interactions
if(lr_test_wealth$`Pr(>F)`[2] < 0.05) {
  p1 <- create_int_plot(stunting_wealth_int, "wealth", "Stunting")
  ggsave("plots/interaction_wealth_residence_stunting.png", p1, width = 8, height = 6)
  cat("Created plot: interaction_wealth_residence_stunting.png\n")
}

if(lr_test_children$`Pr(>F)`[2] < 0.05) {
  p2 <- create_int_plot(stunting_children_int, "children", "Stunting")
  ggsave("plots/interaction_children_residence_stunting.png", p2, width = 8, height = 6)
  cat("Created plot: interaction_children_residence_stunting.png\n")
}

if(lr_test_edu$`Pr(>F)`[2] < 0.05) {
  p3 <- create_int_plot(stunting_edu_int, "education", "Stunting")
  ggsave("plots/interaction_education_residence_stunting.png", p3, width = 8, height = 6)
  cat("Created plot: interaction_education_residence_stunting.png\n")
}

################################################################################
# PART E: FINAL INTERACTION MODEL
################################################################################

cat("\n==================== FINAL INTERACTION MODEL ====================\n")

# Build final model with significant interactions only
# (This will depend on which interactions were significant)

# Example: If wealth × residence is significant
if(lr_test_wealth$`Pr(>F)`[2] < 0.05) {
  final_int_model <- stunting_wealth_int
  cat("\nFinal model includes Wealth × Residence interaction\n")
} else if(lr_test_children$`Pr(>F)`[2] < 0.05) {
  final_int_model <- stunting_children_int
  cat("\nFinal model includes Children × Residence interaction\n")
} else if(lr_test_edu$`Pr(>F)`[2] < 0.05) {
  final_int_model <- stunting_edu_int
  cat("\nFinal model includes Education × Residence interaction\n")
} else {
  final_int_model <- models$stunting_final
  cat("\nNo significant interactions - using main effects model\n")
}

cat("\nFinal Interaction Model Summary:\n")
print(summary(final_int_model))

################################################################################
# SAVE RESULTS
################################################################################

# Save interaction results
interaction_results <- list(
  summary_table = interaction_summary,
  coefficient_comparison = coef_comparison,
  urban_model = urban_stunting,
  rural_model = rural_stunting,
  final_interaction_model = final_int_model
)

saveRDS(interaction_results, "results/interaction_results.rds")
write.csv(interaction_summary, "results/interaction_summary.csv", row.names = FALSE)
write.csv(coef_comparison, "results/urban_rural_comparison.csv", row.names = FALSE)

cat("\n========== INTERACTION ANALYSIS COMPLETE ==========\n")
cat("Results saved in 'results' directory\n")
cat("Plots saved in 'plots' directory\n")

# Print final recommendations
cat("\n========== FINAL RECOMMENDATIONS ==========\n")
if(any(interaction_summary$Significant == "Yes", na.rm = TRUE)) {
  cat("Significant urban-rural interactions detected.\n")
  cat("RECOMMENDATION: Use stratified models or include interaction terms in final analysis.\n")
  cat("Different policies may be needed for urban and rural areas.\n")
} else {
  cat("No significant urban-rural interactions detected.\n")
  cat("RECOMMENDATION: A single model can be used for both urban and rural areas.\n")
  cat("Similar intervention strategies may work in both settings.\n")
}
