# ==============================================================================
# run_tobacco.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2025-09-26
#
# Description:
# This is the main control script for the Tobacco Phenotyping Validation pipeline.
# Running this script will execute the entire workflow from data loading to
# final output generation.
# ==============================================================================

# Step 0: Install Install once if needed before running
# Restart Rstudio maybe needed before install these package
# install.packages(c("tidyverse","gtsummary","rmarkdown","broom","bigrquery",
#                    "dplyr","lubridate","data.table","purrr","irr","ggplot2",
#                    "ggsci","scales"))


# Step 1: Setup environment, load all libraries and helper functions
source("R/00_setup.R")
print("Step 1: Environment setup complete.")

# Step 2: Load all raw data from BigQuery and local CSVs
source("R/tobacco_phenotype/01_load_data.R")
print("Step 2: Data loading complete.")

# Step 3: Process raw data and generate the final analytic cohort
source("R/tobacco_phenotype/02_process_and_combine.R")
print("Step 3: Data processing and cohort generation complete.")

# Step 4: Create specialized datasets required for validation analyses
source("R/tobacco_phenotype/03_create_analysis_datasets.R")
print("Step 4: Analysis-specific datasets created.")

# Step 5: Execute the statistical validation analyses and save results to a file
source("R/tobacco_phenotype/04_validation_analysis.R")
print("Step 5: Statistical validation analyses complete. Results saved to .RData file.")

# Step 6: Render the R Markdown report into a final HTML file
rmarkdown::render(
  "R/tobacco_phenotype/05_generate_tobacco_report.Rmd",
  output_file = "../tobacco_validation_report.html"
)
print("Step 6: Final HTML report generated successfully.")


print("======================================================================")
print("Tobacco validation pipeline finished successfully.")
print("A report named 'tobacco_validation_report.html' has been created.")
print("======================================================================")
