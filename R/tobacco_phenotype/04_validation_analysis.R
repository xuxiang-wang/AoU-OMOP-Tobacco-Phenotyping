# ==============================================================================
# 04_validation_analysis.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2025-09-26
#
# Description:
# This script performs all statistical validation analyses. It takes the
# analysis-ready datasets and computes agreement (Kappa, ICC), performance
# metrics (sensitivity, specificity, PPV, NPV), and creates plot objects.
# The final result objects are saved for the next script to output.
# ==============================================================================


# -- Part 1. Smoking Status (Cohen’s kappa) + Performance --
# ------------------------------------------------------------------------------

# Create EHR-based smoking indicators and prepare data for analysis
analytic_status_data <- compared_smoking_status_df %>%
  mutate(
    EHR_based_smoker = if_else(replace_na(current_smoker, 0) == 1, 1, 0),
    EHR_based_current_former_smoker = if_else(
      replace_na(current_smoker, 0) == 1 | replace_na(former_smoker, 0) == 1, 1, 0
    )
  ) %>%
  # Create survey-based smoking indicators
  mutate(
    survey_based_smoker = if_else(smoking_status == "Current Smoker", 1, 0),
    survey_based_current_former_smoker = if_else(
      smoking_status %in% c("Current Smoker", "Former Smoker", "Current or Former Smoker"), 1, 0
    )
  ) %>%
  # Agreement categories
  mutate(
    current_smoker_agreement = case_when(
      survey_based_smoker == 1 & EHR_based_smoker == 1 ~ "Both Yes",
      survey_based_smoker == 0 & EHR_based_smoker == 0 ~ "Both No",
      survey_based_smoker == 1 & EHR_based_smoker == 0 ~ "Survey-Only",
      survey_based_smoker == 0 & EHR_based_smoker == 1 ~ "EHR-Only"
    ),
    current_smoker_agreement_binary = if_else(
      current_smoker_agreement %in% c("Both Yes", "Both No"), 1, 0
    ),
    current_former_smoker_agreement = case_when(
      survey_based_current_former_smoker == 1 & EHR_based_current_former_smoker == 1 ~ "Both Yes",
      survey_based_current_former_smoker == 0 & EHR_based_current_former_smoker == 0 ~ "Both No",
      survey_based_current_former_smoker == 1 & EHR_based_current_former_smoker == 0 ~ "Survey-Only",
      survey_based_current_former_smoker == 0 & EHR_based_current_former_smoker == 1 ~ "EHR-Only"
    ),
    current_former_smoker_agreement_binary = if_else(
      current_former_smoker_agreement %in% c("Both Yes", "Both No"), 1, 0
    )
  ) %>%
  # Factor levels and demographics
  mutate(
    age = as.numeric(age),
    age_group = factor(age_group, levels = c("18-44", "45-64", "65+")),
    race_and_ethnicity = factor(race_and_ethnicity, levels = c(
      "Hispanic/Latinx", "Non-Hispanic/Latinx White", "Non-Hispanic/Latinx Black or AA",
      "Non-Hispanic/Latinx Multiple", "Non-Hispanic/Latinx Other", "Other/Unclassified",
      "Missing/Unknown"
    )),
    smoking_status = factor(
      smoking_status,
      levels = c("Current Smoker", "Current or Former Smoker", "Former Smoker", "Non-Smoker")
    ),
    education_level = factor(education_level, levels = c(
      "<Nine", "Nine Through Eleven", "Twelve Or GED", "College One to Three",
      "College Graduate", "Advanced Degree", "Missing/Unknown"
    )),
    marital_status = factor(marital_status, levels = c(
      "Never Married", "Married", "Divorced", "Other", "Missing/Unknown"
    )),
    health_insurance = factor(health_insurance, levels = c("No", "Yes", "Missing/Unknown")),
    employment_status = factor(employment_status, levels = c(
      "Employed", "Homemaker", "Student", "Out of Work",
      "Retired", "Unable To Work", "Missing/Unknown"
    )),
    annual_household_income = factor(annual_household_income, levels = c(
      "Less than $25k", "$25k-$50k", "$50k-$75k", "$75k-$100k",
      "$100k-$200k", "More than $200k", "Missing/Unknown"
    )),
    current_home_own = factor(current_home_own, levels = c(
      "Own", "Rent", "Other Arrangement", "Missing/Unknown"
    ))
  )

# Descriptive Table (by smoking_status)
selected_columns_status <- c(
  "age","age_group","race_and_ethnicity","education_level","marital_status",
  "health_insurance","employment_status","annual_household_income","current_home_own",
  "current_smoker_agreement","current_former_smoker_agreement","smoking_status"
)

descriptive_status <- analytic_status_data %>%
  select(all_of(selected_columns_status)) %>%
  tbl_summary(by = smoking_status) %>%
  add_overall() %>%
  add_p()

# Cohen’s kappa (binary endpoints)
kappa_current_smoker <- compute_kappa(
  analytic_status_data, "survey_based_smoker", "EHR_based_smoker",
  "Current Smoker Agreement", "unweighted"
)
kappa_current_former_smoker <- compute_kappa(
  analytic_status_data, "survey_based_current_former_smoker", "EHR_based_current_former_smoker",
  "Current or Former Smoker Agreement", "unweighted"
)
kappa_table_status <- bind_rows(kappa_current_smoker, kappa_current_former_smoker)

# Sensitivity/Specificity for the two binary endpoints
conf_matrix_current_smoker <- table(
  Survey = analytic_status_data$survey_based_smoker,
  EHR = analytic_status_data$EHR_based_smoker
)
performance_current_smoker <- calculate_metrics(
  conf_matrix_current_smoker, "Current Smoker Agreement"
)

conf_matrix_current_former <- table(
  Survey = analytic_status_data$survey_based_current_former_smoker,
  EHR = analytic_status_data$EHR_based_current_former_smoker
)
performance_current_former <- calculate_metrics(
  conf_matrix_current_former, "Current or Former Smoker Agreement"
)

performance_table_status <- bind_rows(performance_current_smoker, performance_current_former)


# -- Part 2. Intensity (ordinal categories)  — Weighted kappa + Performance --
# ------------------------------------------------------------------------------

analytic_intensity_data <- compared_intensity_df %>%
  mutate(
    EHR_based_intensity    = intensity_daily,
    survey_based_intensity = coalesce(intensity_current_daily, intensity_avg_daily)
  ) %>%
  # Demographics + ordered factors for intensity
  mutate(
    age = as.numeric(age),
    age_group = factor(age_group, levels = c("18-44", "45-64", "65+")),
    race_and_ethnicity = factor(race_and_ethnicity, levels = c(
      "Hispanic/Latinx", "Non-Hispanic/Latinx White", "Non-Hispanic/Latinx Black or AA",
      "Non-Hispanic/Latinx Multiple", "Non-Hispanic/Latinx Other", "Other/Unclassified",
      "Missing/Unknown"
    )),
    education_level = factor(education_level, levels = c(
      "<Nine", "Nine Through Eleven", "Twelve Or GED", "College One to Three",
      "College Graduate", "Advanced Degree", "Missing/Unknown"
    )),
    marital_status = factor(marital_status, levels = c(
      "Never Married", "Married", "Divorced", "Other", "Missing/Unknown"
    )),
    health_insurance = factor(health_insurance, levels = c("No", "Yes", "Missing/Unknown")),
    employment_status = factor(employment_status, levels = c(
      "Employed", "Homemaker", "Student", "Out of Work",
      "Retired", "Unable To Work", "Missing/Unknown"
    )),
    annual_household_income = factor(annual_household_income, levels = c(
      "Less than $25k", "$25k-$50k", "$50k-$75k", "$75k-$100k",
      "$100k-$200k", "More than $200k", "Missing/Unknown"
    )),
    current_home_own = factor(current_home_own, levels = c(
      "Own", "Rent", "Other Arrangement", "Missing/Unknown"
    )),
    # Ordered intensity categories (required for weighted kappa)
    EHR_based_intensity = factor(
      EHR_based_intensity,
      levels = c("Trivial", "Light", "Moderate", "Heavy", "Very heavy"),
      ordered = TRUE
    ),
    survey_based_intensity = factor(
      survey_based_intensity,
      levels = c("Trivial", "Light", "Moderate", "Heavy", "Very heavy"),
      ordered = TRUE
    )
  )

# Descriptive Table (by survey_based_intensity)
selected_columns_intensity <- c(
  "age","age_group","race_and_ethnicity","education_level","marital_status",
  "health_insurance","employment_status","annual_household_income","current_home_own",
  "survey_based_intensity"
)

descriptive_intensity <- analytic_intensity_data %>%
  select(all_of(selected_columns_intensity)) %>%
  tbl_summary(by = survey_based_intensity) %>%
  add_overall() %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test",
      age ~ "kruskal.test"
    )
  )

# Weighted kappa (quadratic weights for ordinal agreement)
kappa_intensity <- compute_kappa(
  analytic_intensity_data, "survey_based_intensity", "EHR_based_intensity",
  "Intensity (ordinal, weighted)", weight = "squared"
)

# Performance
conf_matrix_intensity_full <- table(
  Survey = analytic_intensity_data$survey_based_intensity,
  EHR    = analytic_intensity_data$EHR_based_intensity
)

# small helper to build a 2x2 (EHR vs Survey) for one label, guaranteeing 0/1 dims exist
build_cm_one_vs_all <- function(df, level_label) {
  tmp <- df %>%
    mutate(
      EHR_bin    = if_else(EHR_based_intensity    == level_label, 1, 0),
      Survey_bin = if_else(survey_based_intensity == level_label, 1, 0)
    )
  cm <- table(Survey = tmp$Survey_bin, EHR = tmp$EHR_bin)
  # ensure matrix has both 0 and 1 on rows/cols
  alllv <- c("0","1")
  cm_full <- matrix(0, nrow = 2, ncol = 2,
                    dimnames = list(Survey = alllv, EHR = alllv))
  cm_full[rownames(cm), colnames(cm)] <- cm
  cm_full
}

intensity_levels <- levels(analytic_intensity_data$survey_based_intensity)
per_class_metrics <- purrr::map_dfr(
  intensity_levels,
  ~{
    cm <- build_cm_one_vs_all(analytic_intensity_data, .x)
    calculate_metrics(cm, paste0("Intensity: ", .x))
  }
)

macro_metrics <- per_class_metrics %>%
  summarise(
    Measure     = "Intensity (macro average)",
    Sensitivity = mean(Sensitivity, na.rm = TRUE),
    Specificity = mean(Specificity, na.rm = TRUE),
    PPV         = mean(PPV,         na.rm = TRUE),
    NPV         = mean(NPV,         na.rm = TRUE)
  )

sum_counts <- function(df, level_labels) {
  purrr::reduce(
    level_labels,
    .init = list(TP=0, TN=0, FP=0, FN=0),
    .f = function(acc, lab) {
      cm <- build_cm_one_vs_all(df, lab)
      TP <- as.numeric(cm["1","1"]); TN <- as.numeric(cm["0","0"])
      FP <- as.numeric(cm["0","1"]); FN <- as.numeric(cm["1","0"])
      list(TP = acc$TP + TP, TN = acc$TN + TN, FP = acc$FP + FP, FN = acc$FN + FN)
    }
  )
}
cnt <- sum_counts(analytic_intensity_data, intensity_levels)

micro_cm <- matrix(
  c(cnt$TN, cnt$FN,
    cnt$FP, cnt$TP),
  nrow = 2, byrow = TRUE,
  dimnames = list(Survey = c("0","1"), EHR = c("0","1"))
)

micro_metrics <- calculate_metrics(micro_cm, "Intensity (micro average)")

performance_intensity_all <- dplyr::bind_rows(per_class_metrics, macro_metrics, micro_metrics)


# -- Part 3. Cigarettes per Day (continuous) — ICC + BA --
# ------------------------------------------------------------------------------

tolerance_cigs <- 5

analytic_cigs_daily_data <- compared_cigarettes_daily_df %>%
  mutate(
    EHR_based_cigs_daily    = cigarettes_per_day,
    survey_based_cigs_daily = coalesce(
      smoking_current_cigarettes_per_day, smoking_avg_daily_cigarettes
    ),
    cigs_agreement = if_else(
      abs(EHR_based_cigs_daily - survey_based_cigs_daily) <= tolerance_cigs, "Agree", "Disagree"
    ),
    cigs_agreement_binary = if_else(cigs_agreement == "Agree", 1, 0)
  )

cigs_complete <- analytic_cigs_daily_data %>%
  select(EHR = EHR_based_cigs_daily, Survey = survey_based_cigs_daily) %>%
  filter(!is.na(EHR), !is.na(Survey))

n_pairs_cigs <- nrow(cigs_complete)
agree_rate_cigs <- mean(abs(cigs_complete$EHR - cigs_complete$Survey) <= tolerance_cigs)

icc_out_cigs <- icc(
  cigs_complete[, c("EHR", "Survey")],
  model = "twoway",
  type  = "agreement",
  unit  = "single"
)

icc_tbl_cigs <- tibble(
  Measure       = "Cigarettes/day (numeric)",
  Metric        = "ICC(2,1) absolute agreement",
  N_pairs       = n_pairs_cigs,
  ICC           = round(icc_out_cigs$value, 3),
  `95% CI Lower`= round(icc_out_cigs$lbound, 3),
  `95% CI Upper`= round(icc_out_cigs$ubound, 3),
  AgreeRate_tol = round(agree_rate_cigs, 3),
  Tolerance     = tolerance_cigs
)

diff_vec_cigs <- cigs_complete$EHR - cigs_complete$Survey
err_tbl_cigs <- tibble(
  Measure   = "Cigarettes/day (error metrics)",
  Bias_mean = round(mean(diff_vec_cigs), 3),
  MAE       = round(mean(abs(diff_vec_cigs)), 3),
  RMSE      = round(sqrt(mean(diff_vec_cigs^2)), 3),
  Pearson_r = round(cor(cigs_complete$Survey, cigs_complete$EHR, method = "pearson"), 3),
  Spearman_r= round(cor(cigs_complete$Survey, cigs_complete$EHR, method = "spearman"), 3),
  N_pairs   = n_pairs_cigs
)

ba_df_cigs <- cigs_complete %>%
  mutate(mean_cigs = (EHR + Survey) / 2,
         diff_cigs = EHR - Survey)

bias_cigs     <- mean(ba_df_cigs$diff_cigs)
sd_diff_cigs  <- sd(ba_df_cigs$diff_cigs)
loa_low_cigs  <- bias_cigs - 1.96 * sd_diff_cigs
loa_high_cigs <- bias_cigs + 1.96 * sd_diff_cigs

ba_plot_cigs <- ggplot(ba_df_cigs, aes(x = mean_cigs, y = diff_cigs)) +
  geom_point(alpha = 0.45) +
  geom_hline(yintercept = bias_cigs, linetype = "dashed") +
  geom_hline(yintercept = loa_low_cigs, linetype = "dashed") +
  geom_hline(yintercept = loa_high_cigs, linetype = "dashed") +
  labs(
    title = "Bland–Altman: Cigarettes/day (EHR - Survey)",
    x = "Mean of EHR and Survey (cigs/day)",
    y = "Difference (EHR - Survey)"
  )


# -- Part 4. Smoking Years (continuous) — ICC + BA --
# ------------------------------------------------------------------------------

tolerance_years <- 2

analytic_smoking_years_data <- compared_smoking_years_df %>%
  mutate(
    EHR_based_years    = duration_years,
    survey_based_years = smoking_years,
    years_agreement = if_else(
      abs(EHR_based_years - survey_based_years) <= tolerance_years, "Agree", "Disagree"
    ),
    years_agreement_binary = if_else(years_agreement == "Agree", 1L, 0L)
  )

years_complete <- analytic_smoking_years_data %>%
  select(EHR = EHR_based_years, Survey = survey_based_years) %>%
  filter(!is.na(EHR), !is.na(Survey))

n_pairs_years <- nrow(years_complete)
agree_rate_years <- mean(abs(years_complete$EHR - years_complete$Survey) <= tolerance_years)

icc_out_years <- icc(
  years_complete[, c("EHR", "Survey")],
  model = "twoway",
  type  = "agreement",
  unit  = "single"
)

icc_tbl_years <- tibble(
  Measure       = "Smoking years (numeric)",
  Metric        = "ICC(2,1) absolute agreement",
  N_pairs       = n_pairs_years,
  ICC           = round(icc_out_years$value, 3),
  `95% CI Lower`= round(icc_out_years$lbound, 3),
  `95% CI Upper`= round(icc_out_years$ubound, 3),
  AgreeRate_tol = round(agree_rate_years, 3),
  Tolerance     = tolerance_years
)

diff_vec_years <- years_complete$EHR - years_complete$Survey
err_tbl_years <- tibble(
  Measure   = "Smoking years (error metrics)",
  Bias_mean = round(mean(diff_vec_years), 3),
  MAE       = round(mean(abs(diff_vec_years)), 3),
  RMSE      = round(sqrt(mean(diff_vec_years^2)), 3),
  Pearson_r = round(cor(years_complete$Survey, years_complete$EHR, method = "pearson"), 3),
  Spearman_r= round(cor(years_complete$Survey, years_complete$EHR, method = "spearman"), 3),
  N_pairs   = n_pairs_years
)

ba_df_years <- years_complete %>%
  mutate(mean_years = (EHR + Survey) / 2,
         diff_years = EHR - Survey)

bias_years     <- mean(ba_df_years$diff_years)
sd_diff_years  <- sd(ba_df_years$diff_years)
loa_low_years  <- bias_years - 1.96 * sd_diff_years
loa_high_years <- bias_years + 1.96 * sd_diff_years

ba_plot_years <- ggplot(ba_df_years, aes(x = mean_years, y = diff_years)) +
  geom_point(alpha = 0.45) +
  geom_hline(yintercept = bias_years, linetype = "dashed") +
  geom_hline(yintercept = loa_low_years, linetype = "dashed") +
  geom_hline(yintercept = loa_high_years, linetype = "dashed") +
  labs(
    title = "Bland–Altman: Smoking years (EHR - Survey)",
    x = "Mean of EHR and Survey (years)",
    y = "Difference (EHR - Survey)"
  )

# ==============================================================================
# Save all result objects to a file for the report
# ==============================================================================
save(
  # Part 1 Results
  descriptive_status, kappa_table_status, performance_table_status,
  # Part 2 Results
  descriptive_intensity, kappa_intensity, conf_matrix_intensity_full, performance_intensity_all,
  # Part 3 Results
  icc_tbl_cigs, err_tbl_cigs, ba_plot_cigs,
  # Part 4 Results
  icc_tbl_years, err_tbl_years, ba_plot_years,
  # Data for supplemental tables
  long_tobacco_EHR_data, wide_tobacco_EHR_data,
  # File path for the output
  file = "R/validation_results.RData"
)

# -- Clean up intermediate objects from memory --
rm(
  # Intermediate data frames and helpers from your list
  analytic_cigs_daily_data, analytic_intensity_data, analytic_smoking_years_data,
  analytic_status_data, ba_df_cigs, ba_df_years, cigs_complete, years_complete,
  compared_cessation_attempts_df, compared_cigarettes_daily_df, compared_intensity_df,
  compared_smoking_status_df, compared_smoking_years_df, compared_tobacco_types_df,
  conf_matrix_current_former, conf_matrix_current_smoker, conf_matrix_intensity_full,
  kappa_current_former_smoker, kappa_current_smoker, performance_current_former,
  performance_current_smoker, per_class_metrics, macro_metrics, micro_metrics, micro_cm, cnt,
  agree_rate_cigs, agree_rate_years, bias_cigs, bias_years, diff_vec_cigs, diff_vec_years,
  icc_out_cigs, icc_out_years, intensity_levels, loa_high_cigs, loa_high_years,
  loa_low_cigs, loa_low_years, n_pairs_cigs, n_pairs_years, sd_diff_cigs, sd_diff_years,
  selected_columns_intensity, selected_columns_status, tolerance_cigs, tolerance_years,
  build_cm_one_vs_all, sum_counts
)