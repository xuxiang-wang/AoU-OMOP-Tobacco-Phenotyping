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

# -- Part 1. Smoking Status (Cohen’s kappa) + Performance ----------------------
# This section derives binary smoking indicators from EHR and survey data,
# summarizes participant characteristics, and computes agreement and diagnostic
# performance metrics for smoking status.

analytic_status_data <- compared_smoking_status_df %>%
  mutate(
    EHR_based_smoker = if_else(replace_na(current_smoker, 0) == 1, 1, 0),
    EHR_based_current_former_smoker = if_else(
      replace_na(current_smoker, 0) == 1 | replace_na(former_smoker, 0) == 1, 1, 0
    )
  ) %>%
  mutate(
    survey_based_smoker = if_else(smoking_status == "Current Smoker", 1, 0),
    survey_based_current_former_smoker = if_else(
      smoking_status %in% c("Current Smoker", "Former Smoker", "Current or Former Smoker"),
      1, 0
    )
  ) %>%
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

selected_columns_status <- c(
  "age","age_group","race_and_ethnicity","education_level","marital_status",
  "health_insurance","employment_status","annual_household_income","current_home_own",
  "smoking_status"
)

descriptive_status <- analytic_status_data %>%
  select(all_of(selected_columns_status)) %>%
  tbl_summary(by = smoking_status) %>%
  add_overall() %>%
  add_p()

analytic_status_data <- analytic_status_data %>%
  mutate(
    current_smoker_agreement = factor(
      current_smoker_agreement,
      levels = c("Both Yes", "Both No", "Survey-Only", "EHR-Only")
    ),
    current_former_smoker_agreement = factor(
      current_former_smoker_agreement,
      levels = c("Both Yes", "Both No", "Survey-Only", "EHR-Only")
    )
  )

make_agreement_tbl <- function(data, variable) {
  data %>%
    select(all_of(variable)) %>%
    tbl_summary(
      type = all_categorical() ~ "categorical",
      statistic = all_categorical() ~ "{n} ({p}%)",
      label = list(variable ~ "")
    )
}

tbl_current <- make_agreement_tbl(
  analytic_status_data, 
  "current_smoker_agreement"
)

tbl_ever <- make_agreement_tbl(
  analytic_status_data, 
  "current_former_smoker_agreement"
)

table2_agreement <- tbl_stack(
  list(tbl_current, tbl_ever),
  group_header = c("A. Current Smoker Agreement", "B. Ever Smoker (Current or Former) Agreement")
) %>%
  as_gt() %>%
  gt::tab_options(row_group.font.weight = "bold")

kappa_current_smoker <- compute_kappa(
  analytic_status_data, "survey_based_smoker", "EHR_based_smoker",
  "Current Smoker Agreement", "unweighted"
)
kappa_current_former_smoker <- compute_kappa(
  analytic_status_data, "survey_based_current_former_smoker",
  "EHR_based_current_former_smoker",
  "Current or Former Smoker Agreement", "unweighted"
)
kappa_table_status <- bind_rows(kappa_current_smoker, kappa_current_former_smoker)

conf_matrix_current_smoker <- table(
  Survey = analytic_status_data$survey_based_smoker,
  EHR    = analytic_status_data$EHR_based_smoker
)
performance_current_smoker <- calculate_metrics(
  conf_matrix_current_smoker, "Current Smoker Agreement"
)

conf_matrix_current_former <- table(
  Survey = analytic_status_data$survey_based_current_former_smoker,
  EHR    = analytic_status_data$EHR_based_current_former_smoker
)
performance_current_former <- calculate_metrics(
  conf_matrix_current_former, "Current or Former Smoker Agreement"
)

performance_table_status <- bind_rows(performance_current_smoker, performance_current_former)

# Summary bar plots of EHR-derived tobacco indicators
df_participants <- data.frame(
  variable = c(
    "ever_smoker", "never_smoker",
    "cessation_counseling", "medication_use", "cessation_attempt",
    "latest_current_12_month",
    "latest_former_12_month", "latest_cessation_counseling", "latest_medication_use",
    "latest_cessation_attempt"
  ),
  n_persons = c(
    62913, 57209, 2735, 28055, 21558,
    28306, 13044, 594, 17831, 9981
  )
) %>%
  mutate(
    label = recode(
      variable,
      ever_smoker                 = "Ever Smoker",
      never_smoker                = "Never Smoker",
      cessation_counseling        = "Cessation counseling",
      medication_use              = "Medication use",
      cessation_attempt           = "Quit attempt",
      latest_current_12_month     = "Current smoker (past 12m)",
      latest_former_12_month      = "Former smoker (past 12m)",
      latest_cessation_counseling = "Cessation counseling (past 12m)",
      latest_medication_use       = "Medication use (past 12m)",
      latest_cessation_attempt    = "Quit attempt (past 12m)"
    ),
    label = factor(label, levels = rev(label))
  )

max_persons <- max(df_participants$n_persons)

plot_participants <- ggplot(df_participants, aes(x = n_persons, y = label)) +
  geom_col(fill = jama_cols[2], width = 0.5) +
  geom_text(aes(label = comma(n_persons)), hjust = -0.1, size = 3) +
  scale_x_continuous(
    labels = comma,
    limits = c(0, max_persons * 1.07),
    expand = c(0, 0)
  ) +
  labs(
    x = "Number of Participants",
    y = "Variable"
  )

df_records <- data.frame(
  variable = c(
    "current_smoker","former_smoker","non_smoker",
    "current_12_month","former_12_month",
    "cessation_counseling","medication_use","cessation_attempt",
    "e_cigarette_user","cigar_user","smokeless_user","other_type_user",
    "cigaretee_user","packs_per_day","cigarettes_per_day",
    "intensity_daily","duration_years"
  ),
  n_records = c(
    949330,375898,714698,
    949330,232528,
    6208,273462,150842,
    44,2743,1120,352,
    945421,3579,3579,
    25671,2860
  )
) %>%
  mutate(
    label = recode(
      variable,
      current_smoker      = "Current smoker",
      former_smoker       = "Former smoker",
      non_smoker          = "Never smoker",
      current_12_month    = "Current smoker (12m)",
      former_12_month     = "Former smoker (12m)",
      cessation_counseling= "Cessation counseling",
      medication_use      = "Medication use",
      cessation_attempt   = "Quit attempt",
      e_cigarette_user    = "E-cigarette user",
      cigar_user          = "Cigar user",
      smokeless_user      = "Smokeless tobacco user",
      other_type_user     = "Other product user",
      cigaretee_user      = "Cigarette user",
      packs_per_day       = "Packs per day",
      cigarettes_per_day  = "Cigarettes per day",
      intensity_daily     = "Daily intensity",
      duration_years      = "Duration (years)"
    ),
    label = factor(label, levels = rev(label))
  )

max_records <- max(df_records$n_records)

plot_records <- ggplot(df_records, aes(x = n_records, y = label)) +
  geom_col(fill = jama_cols[2], width = 0.6) +
  geom_text(aes(label = comma(n_records)), hjust = -0.1, size = 3) +
  scale_x_continuous(
    labels = comma,
    limits = c(0, max_records * 1.073),
    expand = c(0, 0)
  ) +
  labs(
    x = "Number of Records",
    y = "Variable"
  )


# -- Part 2. Intensity (ordinal) — Weighted kappa + Performance ----------------
# This section evaluates agreement and performance for ordinal smoking intensity
# categories between EHR and survey.

analytic_intensity_data <- compared_intensity_df %>%
  mutate(
    EHR_based_intensity    = intensity_daily,
    survey_based_intensity = coalesce(intensity_current_daily, intensity_avg_daily)
  ) %>%
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

kappa_intensity <- compute_kappa(
  analytic_intensity_data, "survey_based_intensity", "EHR_based_intensity",
  "Intensity (ordinal, weighted)", weight = "squared"
)

conf_matrix_intensity_full <- table(
  Survey = analytic_intensity_data$survey_based_intensity,
  EHR    = analytic_intensity_data$EHR_based_intensity
)

build_cm_one_vs_all <- function(df, level_label) {
  tmp <- df %>%
    mutate(
      EHR_bin    = if_else(EHR_based_intensity    == level_label, 1, 0),
      Survey_bin = if_else(survey_based_intensity == level_label, 1, 0)
    )
  cm <- table(Survey = tmp$Survey_bin, EHR = tmp$EHR_bin)
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
    .init = list(TP = 0, TN = 0, FP = 0, FN = 0),
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


# -- Part 3. Cigarettes per Day (continuous) — ICC + Bland–Altman -------------
# This section evaluates agreement in cigarettes per day as a continuous
# measure, using ICC and a Bland–Altman plot.

tolerance_cigs <- 5

analytic_cigs_daily_data <- compared_cigarettes_daily_df %>%
  mutate(
    EHR_based_cigs_daily    = cigarettes_per_day,
    survey_based_cigs_daily = coalesce(
      smoking_current_cigarettes_per_day, smoking_avg_daily_cigarettes
    ),
    cigs_agreement = if_else(
      abs(EHR_based_cigs_daily - survey_based_cigs_daily) <= tolerance_cigs,
      "Agree", "Disagree"
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
  Measure        = "Cigarettes/day (numeric)",
  Metric         = "ICC(2,1) absolute agreement",
  N_pairs        = n_pairs_cigs,
  ICC            = round(icc_out_cigs$value, 3),
  `95% CI Lower` = round(icc_out_cigs$lbound, 3),
  `95% CI Upper` = round(icc_out_cigs$ubound, 3),
  AgreeRate_tol  = round(agree_rate_cigs, 3),
  Tolerance      = tolerance_cigs
)

diff_vec_cigs <- cigs_complete$EHR - cigs_complete$Survey
err_tbl_cigs <- tibble(
  Measure    = "Cigarettes/day (error metrics)",
  Bias_mean  = round(mean(diff_vec_cigs), 3),
  MAE        = round(mean(abs(diff_vec_cigs)), 3),
  RMSE       = round(sqrt(mean(diff_vec_cigs^2)), 3),
  Pearson_r  = round(cor(cigs_complete$Survey, cigs_complete$EHR, method = "pearson"), 3),
  Spearman_r = round(cor(cigs_complete$Survey, cigs_complete$EHR, method = "spearman"), 3),
  N_pairs    = n_pairs_cigs
)

ba_df_cigs <- cigs_complete %>%
  mutate(
    mean_cigs = (EHR + Survey) / 2,
    diff_cigs = EHR - Survey
  )

bias_cigs     <- mean(ba_df_cigs$diff_cigs)
sd_diff_cigs  <- sd(ba_df_cigs$diff_cigs)
loa_low_cigs  <- bias_cigs - 1.96 * sd_diff_cigs
loa_high_cigs <- bias_cigs + 1.96 * sd_diff_cigs

x_annot_cigs <- max(ba_df_cigs$mean_cigs, na.rm = TRUE) +
  diff(range(ba_df_cigs$mean_cigs, na.rm = TRUE)) * 0.05

ba_plot_cigs <- ggplot(ba_df_cigs, aes(x = mean_cigs, y = diff_cigs)) +
  annotate(
    "rect",
    xmin  = -Inf, xmax = Inf,
    ymin  = loa_low_cigs, ymax = loa_high_cigs,
    fill  = jama_cols[1],
    alpha = 0.08
  ) +
  geom_hline(yintercept = 0, color = "grey80", linewidth = 0.4) +
  geom_point(color = jama_cols[2], alpha = 0.6, size = 1.8) +
  geom_hline(
    yintercept = bias_cigs,
    color      = jama_cols[3],
    linewidth  = 0.7
  ) +
  geom_hline(
    yintercept = c(loa_low_cigs, loa_high_cigs),
    linetype   = "dashed",
    color      = jama_cols[3],
    linewidth  = 0.6
  ) +
  annotate(
    "text",
    x     = x_annot_cigs,
    y     = bias_cigs,
    label = sprintf("Mean bias = %.1f", bias_cigs),
    hjust = 1, vjust = -0.5,
    size  = 3
  ) +
  annotate(
    "text",
    x     = x_annot_cigs,
    y     = loa_high_cigs,
    label = sprintf("+1.96 SD = %.1f", loa_high_cigs),
    hjust = 1, vjust = -0.5,
    size  = 3
  ) +
  annotate(
    "text",
    x     = x_annot_cigs,
    y     = loa_low_cigs,
    label = sprintf("-1.96 SD = %.1f", loa_low_cigs),
    hjust = 1, vjust = 1.5,
    size  = 3
  ) +
  labs(
    title = "Bland–Altman: Cigarettes/day (EHR minus survey)",
    x     = "Mean of EHR and survey (cigarettes/day)",
    y     = "Difference (EHR minus survey)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))


# -- Part 4. Smoking Years (continuous) — ICC + Bland–Altman -------------------
# This section evaluates agreement in cumulative smoking duration (years) using
# ICC and a Bland–Altman plot.

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
  Measure        = "Smoking years (numeric)",
  Metric         = "ICC(2,1) absolute agreement",
  N_pairs        = n_pairs_years,
  ICC            = round(icc_out_years$value, 3),
  `95% CI Lower` = round(icc_out_years$lbound, 3),
  `95% CI Upper` = round(icc_out_years$ubound, 3),
  AgreeRate_tol  = round(agree_rate_years, 3),
  Tolerance      = tolerance_years
)

diff_vec_years <- years_complete$EHR - years_complete$Survey
err_tbl_years <- tibble(
  Measure    = "Smoking years (error metrics)",
  Bias_mean  = round(mean(diff_vec_years), 3),
  MAE        = round(mean(abs(diff_vec_years)), 3),
  RMSE       = round(sqrt(mean(diff_vec_years^2)), 3),
  Pearson_r  = round(cor(years_complete$Survey, years_complete$EHR, method = "pearson"), 3),
  Spearman_r = round(cor(years_complete$Survey, years_complete$EHR, method = "spearman"), 3),
  N_pairs    = n_pairs_years
)

ba_df_years <- years_complete %>%
  mutate(
    mean_years = (EHR + Survey) / 2,
    diff_years = EHR - Survey
  )

bias_years     <- mean(ba_df_years$diff_years)
sd_diff_years  <- sd(ba_df_years$diff_years)
loa_low_years  <- bias_years - 1.96 * sd_diff_years
loa_high_years <- bias_years + 1.96 * sd_diff_years

x_annot_years <- max(ba_df_years$mean_years, na.rm = TRUE) +
  diff(range(ba_df_years$mean_years, na.rm = TRUE)) * 0.05

ba_plot_years <- ggplot(ba_df_years, aes(x = mean_years, y = diff_years)) +
  annotate(
    "rect",
    xmin  = -Inf, xmax = Inf,
    ymin  = loa_low_years, ymax = loa_high_years,
    fill  = jama_cols[1],
    alpha = 0.08
  ) +
  geom_hline(yintercept = 0, color = "grey80", linewidth = 0.4) +
  geom_point(color = jama_cols[2], alpha = 0.6, size = 1.8) +
  geom_hline(
    yintercept = bias_years,
    color      = jama_cols[3],
    linewidth  = 0.7
  ) +
  geom_hline(
    yintercept = c(loa_low_years, loa_high_years),
    linetype   = "dashed",
    color      = jama_cols[3],
    linewidth  = 0.6
  ) +
  annotate(
    "text",
    x     = x_annot_years,
    y     = bias_years,
    label = sprintf("Mean bias = %.1f", bias_years),
    hjust = 1, vjust = -0.5,
    size  = 3
  ) +
  annotate(
    "text",
    x     = x_annot_years,
    y     = loa_high_years,
    label = sprintf("+1.96 SD = %.1f", loa_high_years),
    hjust = 1, vjust = -0.5,
    size  = 3
  ) +
  annotate(
    "text",
    x     = x_annot_years,
    y     = loa_low_years,
    label = sprintf("-1.96 SD = %.1f", loa_low_years),
    hjust = 1, vjust = 1.5,
    size  = 3
  ) +
  labs(
    title = "Bland–Altman: Smoking years (EHR minus survey)",
    x     = "Mean of EHR and survey (years)",
    y     = "Difference (EHR minus survey)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05)))


# ==============================================================================
# Save all result objects to a file for the reporting script
# ==============================================================================

save(
  # Part 1: smoking status
  descriptive_status, kappa_table_status, performance_table_status,
  plot_participants, plot_records,
  
  # Part 2: intensity
  descriptive_intensity, kappa_intensity, conf_matrix_intensity_full,
  performance_intensity_all,
  
  # Part 3: cigarettes/day
  icc_tbl_cigs, err_tbl_cigs, ba_plot_cigs,
  
  # Part 4: smoking years
  icc_tbl_years, err_tbl_years, ba_plot_years,
  
  # Data for supplemental tables
  long_tobacco_EHR_data, wide_tobacco_EHR_data,
  
  file = "R/validation_results.RData"
)


# -- Clean up intermediate objects from memory ---------------------------------

rm(
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
  build_cm_one_vs_all, sum_counts, x_annot_cigs, x_annot_years,
  df_participants, df_records, max_persons, max_records,
  jama_cols, theme_jama
)