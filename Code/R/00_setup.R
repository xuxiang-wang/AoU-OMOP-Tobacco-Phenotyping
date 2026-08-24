# ==============================================================================
# 00_setup.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2026-07-02
#
# Description:
# Sourced first. Loads libraries, workspace identifiers, analysis parameters,
# the bucket/manifest/Results I/O helpers, analysis helpers, and the plot theme.
# ==============================================================================

library(tidyverse)
library(bigrquery)
library(lubridate)
library(data.table)
library(gtsummary)
library(irr)
library(broom)
library(ggsci)
library(scales)
library(rmarkdown)
library(smd)


# Query tables live in the CDR dataset (AOU_CDR), not the *_index_* dataset, so
# AOU_CDR is the default dataset for every query. Billing is a separate project.
AOU_QUERY_DATASET <- "wb-silky-artichoke-2408.C2025Q4R6_index_061026"
AOU_CDR           <- sub("_index_[0-9]+$", "", AOU_QUERY_DATASET)   # -> ...C2025Q4R6
AOU_BILLING       <- "wb-smart-apple-5753"
AOU_BUCKET        <- Sys.getenv("WORKSPACE_BUCKET",
                                unset = "gs://cloned-rw-migration-aou-rw-c9df43e3-wb-smart-apple-5753")
AOU_USER          <- "xuxiang@researchallofus.org"
EXPORT_ROOT   <- file.path(AOU_BUCKET, "bq_exports", AOU_USER)
RUN_DATE      <- strftime(now(), "%Y%m%d")
MANIFEST_PATH <- file.path(EXPORT_ROOT, "manifest", "manifest.csv")

run_dir <- function(name) file.path(EXPORT_ROOT, RUN_DATE, name)

RESULTS_DIR  <- "AoU-OMOP-Tobacco-Phenotyping-main/Results"
RESULTS_DIRS <- list(tables = "tables", figures = "figures", figure_data = "figure_data")
for (d in RESULTS_DIRS) dir.create(file.path(RESULTS_DIR, d), recursive = TRUE, showWarnings = FALSE)


PARAMS <- list(
  concept_set_dir   = "AoU-OMOP-Tobacco-Phenotyping-main/Concept Sets",
  recency_months    = 12, 
  CUTOFF_DATE       = as.Date("2025-01-01"),
  age_min           = 18,
  age_max           = 99,
  match_window_days = 365,
  tol_cigs          = 5,
  tol_years         = 2,
  recency_grid      = c(3, 6, 12, 24), 
  sens_window_days  = 730, 
  lag_breaks        = c(-1, 90, 180, 365, 730)
)


# Run a query and download the result into memory (small results only).
retrieve_data <- function(sql, int_type = "integer") {
  bq_table_download(
    bq_dataset_query(AOU_CDR, sql, billing = AOU_BILLING),
    bigint = int_type
  )
}

# Destination for a sharded CSV export: <root>/<date>/<name>/<name>_*.csv
bq_export_path <- function(name) file.path(run_dir(name), paste0(name, "_*.csv"))

# Run a query and export the result to the bucket as sharded CSV. Returns the path.
bq_export <- function(sql, name) {
  path <- bq_export_path(name)
  message(str_glue("Exporting {name} -> {path}"))
  bq_table_save(
    bq_dataset_query(AOU_CDR, sql, billing = AOU_BILLING),
    path,
    destination_format = "CSV"
  )
  path
}

# Read a sharded CSV export back into a single data frame. Pass col_types to pin
# column types (recommended for dates, which otherwise round-trip as character).
read_bq_export <- function(export_path, col_types = NULL) {
  shards <- system2("gsutil", c("ls", export_path), stdout = TRUE)
  bind_rows(map(shards, function(csv) {
    message(str_glue("Loading {csv}"))
    read_csv(pipe(str_glue("gsutil cat {csv}")), col_types = col_types, show_col_types = FALSE)
  }))
}


# Derived single-file CSVs (long/wide/cohort/compared_*) land in a by-name
# subfolder of the run's date folder, mirroring the export layout.
save_to_bucket <- function(filename, name = tools::file_path_sans_ext(basename(filename))) {
  dest <- file.path(run_dir(name), filename)
  system2("gsutil", c("cp", filename, dest))
  dest
}
# Read a bucket object back by its full gs:// path (as recorded in the manifest).
read_from_bucket <- function(bucket_path) {
  system2("gsutil", c("cp", bucket_path, "."))
  basename(bucket_path)
}

# Fixed-location run manifest (name, bucket path, CDR, date) for resumability.
save_manifest <- function(manifest_df) {
  write_excel_csv(manifest_df, "manifest.csv")
  system2("gsutil", c("cp", "manifest.csv", MANIFEST_PATH))
  invisible(MANIFEST_PATH)
}
read_manifest <- function() {
  system2("gsutil", c("cp", MANIFEST_PATH, "."))
  # Pin every column to character. The date column ("YYYYMMDD") is all digits,
  # so read_csv would otherwise guess <double> and clash with the <character>
  # RUN_DATE when appending rows via bind_rows().
  read_csv("manifest.csv", show_col_types = FALSE,
           col_types = cols(.default = col_character()))
}


save_table <- function(df, name) {
  write_excel_csv(df, file.path(RESULTS_DIR, RESULTS_DIRS$tables, paste0(name, ".csv")))
}
save_figure <- function(plot, name, data = NULL, width = 7, height = 5) {
  ggsave(file.path(RESULTS_DIR, RESULTS_DIRS$figures, paste0(name, ".pdf")),
         plot, width = width, height = height)                                   # vector
  ggsave(file.path(RESULTS_DIR, RESULTS_DIRS$figures, paste0(name, ".png")),
         plot, width = width, height = height, dpi = 300)                        # preview
  if (!is.null(data)) {
    write_excel_csv(data, file.path(RESULTS_DIR, RESULTS_DIRS$figure_data, paste0(name, ".csv")))
  }
  invisible(name)
}


# Timestamped progress line for the console log.
log_step <- function(...) message(format(Sys.time(), "%H:%M:%S"), "  ", ...)

# Fail fast with a clear message if a data frame is missing expected columns.
assert_cols <- function(df, cols, name = deparse(substitute(df))) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop(str_glue("{name} is missing column(s): {paste(missing, collapse = ', ')}"),
         call. = FALSE)
  }
  invisible(df)
}

# Resolve exactly one bucket path from the manifest; error if 0 or >1 matches.
manifest_path <- function(manifest, nm) {
  hit <- manifest$path[manifest$name == nm]
  if (length(hit) != 1) {
    stop(str_glue("manifest has {length(hit)} row(s) named '{nm}' (expected 1)"),
         call. = FALSE)
  }
  hit
}

# Materialize a data frame locally and copy it to the bucket. Returns a one-row
# manifest tibble (name, path, cdr, date) for append_manifest().
stage_to_bucket <- function(df, name) {
  file <- paste0(name, ".csv")
  write_excel_csv(df, file)
  tibble(name = name, path = save_to_bucket(file), cdr = AOU_CDR, date = RUN_DATE)
}

# Append rows to the manifest, replacing any same-named rows so re-runs stay
# idempotent.
append_manifest <- function(manifest, new_rows) {
  manifest %>%
    filter(!name %in% new_rows$name) %>%
    bind_rows(new_rows)
}


# Privacy display: counts in [1, 19] are shown as "< 20", others comma-formatted.
suppress_count <- function(x) {
  ifelse(!is.na(x) & x > 0 & x < 20, "< 20", scales::comma(x))
}


# Cohen's kappa (optionally weighted) with a normal-approximation 95% CI.
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

# Sensitivity / specificity / PPV / NPV from a 2x2 confusion matrix whose
# dimnames are "0"/"1" on both axes.
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