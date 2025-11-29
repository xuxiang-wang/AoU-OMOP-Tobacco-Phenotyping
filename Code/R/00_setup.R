# ==============================================================================
# 00_setup.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2025-11-27
#
# Description:
# This script loads all required libraries and defines global helper functions
# for the entire project. It is sourced first by the main runner scripts.
# ==============================================================================


# -- Libraries for Data Manipulation and Database Connection --
library(tidyverse)
library(bigrquery)
library(dplyr)
library(lubridate)
library(stringr)


# -- Libraries for Analysis, Tables, and Plotting --
library(data.table)
library(purrr)
library(gtsummary)
library(irr)
library(ggplot2)
library(broom)
library(ggsci)
library(scales)
library(rmarkdown)

# -- Global Helper Functions --

# Function to retrieve data from Google BigQuery
retrieve_data <- function(sql = NULL, int_type = "integer") {
  bq_table_download(
    bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), sql, billing = Sys.getenv("GOOGLE_PROJECT")),
    bigint = int_type
  )
}

# Function to compute Cohen's Kappa
# Source: Analysis_Validation_Pipeline-XW_1Sept2025.R
compute_kappa <- function(data, survey_var, ehr_var, measure_name, weight = "unweighted") {
  kappa_data <- data %>%
    select(!!sym(survey_var), !!sym(ehr_var))
  
  kappa_result <- kappa2(kappa_data, weight = weight)
  
  # Compute standard error and confidence intervals
  se <- kappa_result$value / kappa_result$statistic
  ci_lower <- round(kappa_result$value - 1.96 * se, 3)
  ci_upper <- round(kappa_result$value + 1.96 * se, 3)
  
  tibble(
    Measure = measure_name,
    `Cohen's Kappa` = round(kappa_result$value, 3),
    `95% CI Lower` = ci_lower,
    `95% CI Upper` = ci_upper
  )
}

# Function to calculate diagnostic metrics from a 2x2 confusion matrix
# Source: Analysis_Validation_Pipeline-XW_1Sept2025.R
calculate_metrics <- function(confusion_matrix, measure_name) {
  TP <- confusion_matrix["1", "1"]
  TN <- confusion_matrix["0", "0"]
  FP <- confusion_matrix["0", "1"]
  FN <- confusion_matrix["1", "0"]
  
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  ppv <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
  npv <- ifelse((TN + FN) > 0, TN / (TN + FN), NA)
  
  tibble(
    Measure = measure_name,
    Sensitivity = round(sensitivity, 3),
    Specificity = round(specificity, 3),
    PPV = round(ppv, 3),
    NPV = round(npv, 3)
  )
}

# JAMA color palette
jama_cols <- ggsci::pal_jama("default")(7)

# JAMA-style theme applied to all ggplot objects
theme_jama <- function(base_size = 11, base_family = "sans") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line        = element_line(color = "black", linewidth = 0.4),
      axis.ticks       = element_line(color = "black", linewidth = 0.4),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 1),
      plot.title       = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle    = element_text(size = base_size, hjust = 0),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key       = element_blank(),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text       = element_text(face = "bold"),
      plot.margin      = margin(t = 5.5, r = 5.5, b = 5.5, l = 5.5)
    )
}

theme_set(theme_jama())

