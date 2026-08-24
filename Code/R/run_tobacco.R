# ==============================================================================
# run_tobacco.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2026-07-02
#
# Description:
# One-shot runner: sources 00-04 in order (each resumable via the bucket
# manifest), then renders the HTML report. Assumes the working directory is
# /home/rstudio; edit CODE_DIR and the paths in 00_setup.R if it differs.
#
# install.packages(c("tidyverse","gtsummary","rmarkdown","broom","bigrquery",
#                    "lubridate","data.table","irr","ggsci","scales","gt"))
# ==============================================================================

CODE_DIR    <- "AoU-OMOP-Tobacco-Phenotyping-main/Code/R"
REPORT_RMD  <- file.path(CODE_DIR, "05_generate_tobacco_report.Rmd")
REPORT_HTML <- "AoU-OMOP-Tobacco-Phenotyping-main/Results/tobacco_validation_report.html"

# -- Step 1: environment, libraries, helpers, parameters ----------------------
source(file.path(CODE_DIR, "00_setup.R"))
log_step("Step 1/6  setup complete.")

# -- Step 2: pull all raw data to the bucket, write the run manifest ----------
source(file.path(CODE_DIR, "01_load_data.R"))
log_step("Step 2/6  data pull complete.")

# -- Step 3: EHR classification -> product long/wide + concept dictionary ------
source(file.path(CODE_DIR, "02_classify_ehr.R"))   # <- file-2 content (EHR classification)
log_step("Step 3/6  EHR classification complete.")

# -- Step 4: survey processing, analytic cohort, matched sets -----------------
source(file.path(CODE_DIR, "03_compare.R"))  # <- file-3 content (comparison)
log_step("Step 4/6  cohort and matched sets built.")

# -- Step 5: all analyses + sensitivity -> Results/ ---------------------------
source(file.path(CODE_DIR, "04_validation_analysis.R"))
log_step("Step 5/6  analyses complete; results written to Results/.")

# -- Step 6: render the HTML report from Results/ -----------------------------
# knit_root_dir keeps the report at /home/rstudio so Results/ resolves.
rmarkdown::render(REPORT_RMD,
                  output_file  = normalizePath(REPORT_HTML, mustWork = FALSE),
                  knit_root_dir = getwd())
log_step("Step 6/6  report rendered -> ", REPORT_HTML)

log_step("Pipeline finished.")