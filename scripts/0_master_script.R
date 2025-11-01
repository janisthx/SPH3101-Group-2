# 0. Master Script

# Load all required packages
library(tidyverse)
library(anthro) # For calculation of Z-Scores
library(car) # For VIF Analysis

library(ggplot2) # For plots
library(broom) # For tidy function
library(pROC) # For ROC Curves

# Run the data cleaning and processing
source('scripts/1_data_cleaning_processing.R')

# Run exploratory analysis codes, including all the tests and their results
source('scripts/2_exploratory_analysis.R')

# Run model building codes
source('scripts/3_statistical_models.R')

# Run the interaction analysis of residence vs. all variables in the final model
source('scripts/4_interaction_analysis.R')

# Run the visualization codes
# source('scripts/5_visualization.R')