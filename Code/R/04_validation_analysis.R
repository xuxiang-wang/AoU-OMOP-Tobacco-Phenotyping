# ==============================================================================
# 04_validation_analysis.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2026-07-02
#
# Description:
# Runs all analyses (status, intensity, cigarettes/day, smoking-years) and the
# 3A/3B sensitivity analyses, and writes every table and figure to Results/.
# The participant/record counts are computed from wide/long (not hard-coded).
# ==============================================================================


# -- 0. Read the matched sets and EHR tables from the bucket ------------------
# ------------------------------------------------------------------------------
manifest <- read_manifest()

d2 <- cols(person_id = col_double(), event_date = col_date(), survey_date = col_date())

compared_smoking_status_df   <- read_bq_export(manifest_path(manifest, "compared_smoking_status"),   col_types = d2)
compared_intensity_df        <- read_bq_export(manifest_path(manifest, "compared_intensity"),        col_types = d2)
compared_cigarettes_daily_df <- read_bq_export(manifest_path(manifest, "compared_cigarettes_daily"), col_types = d2)
compared_smoking_years_df    <- read_bq_export(manifest_path(manifest, "compared_smoking_years"),    col_types = d2)

# cohort + flow counts (for sensitivity re-pairing and Figure 2)
cohort      <- read_bq_export(manifest_path(manifest, "cohort"),
                              col_types = cols(person_id = col_double(), survey_date = col_date()))
cohort_flow <- read_bq_export(manifest_path(manifest, "cohort_flow"))

bias_comparison_cohort <- read_bq_export(
  manifest_path(manifest, "bias_comparison_cohort"),
  col_types = cols(person_id = col_double()))

long_tobacco_EHR_data <- read_bq_export(
  manifest_path(manifest, "long_tobacco_ehr"),
  col_types = cols(person_id = col_double(), concept_id = col_double(),
                   value_as_concept_id = col_double(), value_as_number = col_double(),
                   event_date = col_date(), survey_date = col_date()))
wide_tobacco_EHR_data <- read_bq_export(
  manifest_path(manifest, "wide_tobacco_ehr"),
  col_types = cols(person_id = col_double(),
                   first_event_date = col_date(), last_event_date = col_date()))
concept_dictionary <- read_bq_export(manifest_path(manifest, "concept_dictionary"),
                                     col_types = cols(concept_id = col_double()))


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

smd_vs_final <- function(data, variable, by, ...) {
  g <- factor(data[[by]], levels = c(
    "Final analytic cohort",
    "No tobacco-related records",
    "Tobacco records, none within 1 year"))
  s <- smd::smd(x = data[[variable]], g = g, gref = 1, na.rm = TRUE)  # ref = final cohort
  tibble::tibble(
    smd_norec = style_number(abs(s$estimate[1]), digits = 2),
    smd_nowin = style_number(abs(s$estimate[2]), digits = 2)
  )
}

descriptive_bias_comparison <- bias_comparison_cohort %>%
  mutate(
    age_group = factor(age_group, levels = c("18-44", "45-64", "65+")),
    race_and_ethnicity = factor(race_and_ethnicity, levels = c(
      "Hispanic/Latinx", "Non-Hispanic/Latinx White", "Non-Hispanic/Latinx Black or AA",
      "Non-Hispanic/Latinx Multiple", "Non-Hispanic/Latinx Other", "Other/Unclassified",
      "Missing/Unknown")),
    education_level = factor(education_level, levels = c(
      "<Nine", "Nine Through Eleven", "Twelve Or GED", "College One to Three",
      "College Graduate", "Advanced Degree", "Missing/Unknown")),
    marital_status = factor(marital_status, levels = c(
      "Never Married", "Married", "Divorced", "Other", "Missing/Unknown")),
    health_insurance = factor(health_insurance, levels = c("No", "Yes", "Missing/Unknown")),
    employment_status = factor(employment_status, levels = c(
      "Employed", "Homemaker", "Student", "Out of Work",
      "Retired", "Unable To Work", "Missing/Unknown")),
    annual_household_income = factor(annual_household_income, levels = c(
      "Less than $25k", "$25k-$50k", "$50k-$75k", "$75k-$100k",
      "$100k-$200k", "More than $200k", "Missing/Unknown")),
    current_home_own = factor(current_home_own, levels = c(
      "Own", "Rent", "Other Arrangement", "Missing/Unknown")),
    smoking_status = factor(smoking_status, levels = c(
      "Current Smoker", "Current or Former Smoker", "Former Smoker", "Non-Smoker"))
  ) %>%
  select(all_of(selected_columns_status), cohort_group) %>%
  tbl_summary(by = cohort_group) %>%
  add_stat(fns = everything() ~ smd_vs_final) %>%
  modify_header(
    smd_norec = "**SMD**<br>No records vs final",
    smd_nowin = "**SMD**<br>Not in window vs final")

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

# Keep the stacked gtsummary object (drop the as_gt() styling) so it can be
# flattened to a tibble for the reproducible CSV in Results/. Formatting is a
# file-5 concern now.
table2_agreement <- tbl_stack(
  list(tbl_current, tbl_ever),
  group_header = c("A. Current Smoker Agreement", "B. Current or Former Smoker Agreement")
)

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

# -- 2. Participant / record counts (computed; formerly hard-coded) -----------
# ------------------------------------------------------------------------------
# Contents of the wide/long product tables: persons per variable (wide) and
# records per variable (long).

long_vars <- c(
  "current_smoker", "former_smoker", "non_smoker",
  "cessation_counseling", "medication_use", "cessation_attempt",
  "cigarette_user", "e_cigarette_user", "cigar_user",
  "hookah_user", "smokeless_user", "other_type_user",
  "packs_per_day", "cigarettes_per_day", "intensity_daily", "duration_years"
)
long_labels <- c(
  current_smoker = "Current smoker", former_smoker = "Former smoker", non_smoker = "Non-smoker",
  cessation_counseling = "Cessation counseling", medication_use = "Medication use",
  cessation_attempt = "Quit attempt",
  cigarette_user = "Cigarette user", e_cigarette_user = "E-cigarette user", cigar_user = "Cigar user",
  hookah_user = "Hookah user", smokeless_user = "Smokeless tobacco user", other_type_user = "Other product user",
  packs_per_day = "Packs per day", cigarettes_per_day = "Cigarettes per day",
  intensity_daily = "Daily intensity", duration_years = "Duration (years)"
)

wide_vars <- c(
  "ever_cigarette", "ever_e_cigarette", "ever_cigar", "ever_hookah",
  "ever_smokeless", "ever_other_product",
  "ever_cessation_counseling", "ever_medication_use", "ever_cessation_attempt"
)
wide_labels <- c(
  ever_cigarette = "Cigarette", ever_e_cigarette = "E-cigarette", ever_cigar = "Cigar",
  ever_hookah = "Hookah", ever_smokeless = "Smokeless tobacco", ever_other_product = "Other product",
  ever_cessation_counseling = "Cessation counseling", ever_medication_use = "Medication use",
  ever_cessation_attempt = "Quit attempt"
)

count_long <- function(df, v) {
  sub <- if (v == "intensity_daily") filter(df, !is.na(.data[[v]]))
  else filter(df, !is.na(.data[[v]]) & .data[[v]] != 0)
  tibble(variable = v, n_records = nrow(sub), n_persons = n_distinct(sub$person_id))
}
count_wide <- function(df, v) {
  sub <- filter(df, !is.na(.data[[v]]) & .data[[v]] != 0)
  tibble(variable = v, n_persons = n_distinct(sub$person_id))
}

eligible_ids  <- unique(bias_comparison_cohort$person_id)
wide_eligible <- wide_tobacco_EHR_data %>% filter(person_id %in% eligible_ids)


sup_table_long <- map_dfr(long_vars, ~ count_long(long_tobacco_EHR_data, .x))
sup_table_wide <- map_dfr(wide_vars, ~ count_wide(wide_eligible, .x))

# Point-in-time status (mutually exclusive; current > former > never)
status_levels <- c("Current smoker", "Former smoker", "Non-smoker",
                   "No smoking-status evidence")
status_counts <- wide_eligible %>%
  transmute(status_display = factor(wide_status, levels = status_levels)) %>%
  count(status_display, name = "n_persons", .drop = FALSE) %>%
  transmute(variable = as.character(status_display), n_persons)

# Figure data: participants (person level, from wide) and records (from long).
# The wide figure/table lead with the three mutually exclusive point-in-time
# statuses, then the (non-exclusive) product and cessation indicators.
wide_display_levels <- c(status_levels, unname(wide_labels))
df_participants <- bind_rows(
  status_counts,
  sup_table_wide %>% mutate(variable = wide_labels[variable]) %>% select(variable, n_persons)
) %>%
  mutate(label = factor(variable, levels = rev(wide_display_levels)))
df_records <- sup_table_long %>%
  mutate(label = long_labels[variable],
         label = factor(label, levels = rev(label)))


# -- Supplemental tables (concept names on 1B/2B; counts <20 suppressed) -------
# 1A: patient-level variable counts (wide)
suppl_1A <- df_participants %>%
  transmute(Variable = as.character(label),
            `Number of participants` = suppress_count(n_persons)) %>%
  arrange(match(Variable, wide_display_levels))

# 2A: event-level variable counts (long, including raw flags)
vars_2A <- c("current_smoker", "former_smoker", "non_smoker",
             "is_current_flag", "is_former_flag", "is_non_flag",
             "cessation_counseling", "medication_use", "cessation_attempt",
             "cigarette_user", "e_cigarette_user", "cigar_user", "hookah_user",
             "smokeless_user", "other_type_user",
             "packs_per_day", "cigarettes_per_day", "intensity_daily", "duration_years")
labels_2A <- c(current_smoker = "Current smoker", former_smoker = "Former smoker",
               non_smoker = "Non-smoker",
               is_current_flag = "Flag: current smoker", is_former_flag = "Flag: former smoker",
               is_non_flag = "Flag: never smoker",
               cessation_counseling = "Cessation counseling", medication_use = "Medication use",
               cessation_attempt = "Quit attempt",
               cigarette_user = "Cigarette user", e_cigarette_user = "E-cigarette user",
               cigar_user = "Cigar user", hookah_user = "Hookah user",
               smokeless_user = "Smokeless tobacco user", other_type_user = "Other product user",
               packs_per_day = "Packs per day", cigarettes_per_day = "Cigarettes per day",
               intensity_daily = "Daily intensity", duration_years = "Duration (years)")
suppl_2A <- map_dfr(vars_2A, ~ count_long(long_tobacco_EHR_data, .x)) %>%
  transmute(Variable = labels_2A[variable],
            `Number of records` = suppress_count(n_records))

# 1B: patient-level concept frequency (distinct persons per concept_id)
suppl_1B <- long_tobacco_EHR_data %>%
  group_by(concept_id) %>%
  summarise(n = n_distinct(person_id), .groups = "drop") %>%
  left_join(concept_dictionary, by = "concept_id") %>%
  arrange(desc(n)) %>%
  transmute(`Concept ID` = concept_id, `Concept name` = concept_name,
            `Number of participants` = suppress_count(n))

# 2B: event-level concept frequency (records per concept_id)
suppl_2B <- long_tobacco_EHR_data %>%
  count(concept_id, name = "n") %>%
  left_join(concept_dictionary, by = "concept_id") %>%
  arrange(desc(n)) %>%
  transmute(`Concept ID` = concept_id, `Concept name` = concept_name,
            `Number of records` = suppress_count(n))

plot_participants <- ggplot(df_participants, aes(x = n_persons, y = label)) +
  geom_col(fill = jama_cols[2], width = 0.5) +
  geom_text(aes(label = suppress_count(n_persons)), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.02)),
                     limits = c(0, max(df_participants$n_persons) * 1.28)) +
  coord_cartesian(clip = "off") +
  labs(x = "Number of Participants", y = "Variable")

plot_records <- ggplot(df_records, aes(x = n_records, y = label)) +
  geom_col(fill = jama_cols[2], width = 0.61) +
  geom_text(aes(label = suppress_count(n_records)), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.18)),
                     limits = c(0, max(df_records$n_records) * 1.025)) +
  coord_cartesian(clip = "off") +
  labs(x = "Number of Records", y = "Variable")


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
  c(cnt$TN, cnt$FP,
    cnt$FN, cnt$TP),
  nrow = 2, byrow = TRUE,
  dimnames = list(Survey = c("0","1"), EHR = c("0","1")))

micro_metrics <- calculate_metrics(micro_cm, "Intensity (micro average)")

performance_intensity_all <- dplyr::bind_rows(per_class_metrics, macro_metrics, micro_metrics)


# -- Part 3. Cigarettes per Day (continuous) — ICC + Bland–Altman -------------
# This section evaluates agreement in cigarettes per day as a continuous
# measure, using ICC and a Bland–Altman plot.

tolerance_cigs <- PARAMS$tol_cigs

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

tolerance_years <- PARAMS$tol_years

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
# Sensitivity analyses (Supplemental Table 3A / 3B) -- status phenotype only
# ==============================================================================
# Reuses classify_status() from file 2. 3A varies the recency window with pairing
# fixed at +/-365d; 3B fixes recency at 12 months, pairs out to +/-730d, and
# stratifies by the survey<->nearest-event time lag. Pairing replicates the
# primary status pairing so the 12-month rows stay consistent with Table 2.

# Nearest paired event is R-independent, and the current/former split for any R
# is fixed by two per-person anchor dates. So: precompute anchors once, pair once
# per window, apply each R by comparison. Equivalent to classify_status() per R.
#   cur_anchor = latest current event on/before survey -> cur_win(R): >= survey - R
#   cur_min    = earliest current event                -> prior(R):  <  survey - R

# (1) per-person anchors (once)
status_anchors <- {
  cur_ev <- long_tobacco_EHR_data %>%
    filter(is_current_flag == 1, !is.na(survey_date)) %>%
    select(person_id, survey_date, event_date)
  a_anchor <- cur_ev %>% filter(event_date <= survey_date) %>%
    group_by(person_id) %>% summarise(cur_anchor = max(event_date), .groups = "drop")
  a_min <- cur_ev %>%
    group_by(person_id) %>% summarise(cur_min = min(event_date), .groups = "drop")
  full_join(a_anchor, a_min, by = "person_id")
}

# (2) nearest paired event within a window (R-independent); keep raw flags only
pair_nearest <- function(cohort_df, window_days) {
  cohort_df %>%
    select(person_id, survey_date, smoking_status) %>%
    inner_join(
      long_tobacco_EHR_data %>%
        select(person_id, event_date, is_current_flag, is_former_flag, is_non_flag),
      by = "person_id"
    ) %>%
    mutate(date_diff = abs(as.numeric(event_date - survey_date))) %>%
    filter(date_diff <= window_days) %>%
    group_by(person_id, survey_date) %>%
    slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# (3) apply a recency window R to an already-paired table (cheap, per person)
status_at_R <- function(paired, R) {
  paired %>%
    left_join(status_anchors, by = "person_id") %>%
    mutate(
      cur_win        = as.integer(!is.na(cur_anchor) & cur_anchor >= (survey_date %m-% months(R))),
      prior          = as.integer(!is.na(cur_min)    & cur_min    <  (survey_date %m-% months(R))),
      former_win     = as.integer(cur_win == 0 & prior == 1),
      current_smoker = as.integer(is_current_flag == 1 | cur_win == 1),
      former_smoker  = as.integer(is_former_flag  == 1 | former_win == 1),
      non_smoker     = as.integer(is_non_flag == 1 & current_smoker == 0 & former_smoker == 0)
    )
}

status_binaries <- function(paired) {
  paired %>%
    mutate(
      ehr_cur  = as.integer(current_smoker == 1),
      ehr_ever = as.integer(current_smoker == 1 | former_smoker == 1),
      sv_cur   = as.integer(smoking_status == "Current Smoker"),
      sv_ever  = as.integer(smoking_status %in%
                              c("Current Smoker", "Former Smoker", "Current or Former Smoker"))
    )
}

# N + sensitivity/specificity/PPV/NPV + kappa(95% CI) from survey/EHR binaries.
metrics_row <- function(sv, ehr, group) {
  cm <- table(factor(sv, c(0, 1)), factor(ehr, c(0, 1)))   # rows = survey, cols = EHR
  TP <- cm["1", "1"]; TN <- cm["0", "0"]; FP <- cm["0", "1"]; FN <- cm["1", "0"]
  k  <- kappa2(data.frame(sv, ehr))
  se <- k$value / k$statistic
  tibble(
    Group       = group,
    N           = suppress_count(length(sv)),
    Sensitivity = round(TP / (TP + FN), 3),
    Specificity = round(TN / (TN + FP), 3),
    PPV         = round(TP / (TP + FP), 3),
    NPV         = round(TN / (TN + FN), 3),
    Kappa       = round(k$value, 3),
    Kappa_low   = round(k$value - 1.96 * se, 3),
    Kappa_high  = round(k$value + 1.96 * se, 3)
  )
}

# 3A: pair once at +/-365 days, then apply each recency window
paired_365 <- pair_nearest(cohort, PARAMS$match_window_days)
sensitivity_3A <- map_dfr(PARAMS$recency_grid, function(R) {
  d <- status_binaries(status_at_R(paired_365, R))
  bind_rows(
    metrics_row(d$sv_cur,  d$ehr_cur,  "Current smoker"),
    metrics_row(d$sv_ever, d$ehr_ever, "Current or former smoker")
  ) %>% mutate(lookback_months = R, .before = 1)
})

# 3B: pair once at +/-730 days, apply the 12-month recency, stratify by time lag
d730 <- status_binaries(status_at_R(pair_nearest(cohort, PARAMS$sens_window_days), PARAMS$recency_months)) %>%
  mutate(lag_bin = cut(date_diff, breaks = PARAMS$lag_breaks,
                       labels = c("0-90", "91-180", "181-365", "366-730")))
sensitivity_3B <- d730 %>%
  group_by(lag_bin) %>%
  group_modify(~ bind_rows(
    metrics_row(.x$sv_cur,  .x$ehr_cur,  "Current smoker"),
    metrics_row(.x$sv_ever, .x$ehr_ever, "Current or former smoker")
  )) %>%
  ungroup()


# ==============================================================================
# Figure 2: cohort selection flow (from the step counts recorded in file 3)
# ==============================================================================
cohort_flow <- cohort_flow %>%
  mutate(n = as.numeric(n), step = factor(step, levels = rev(step)))
plot_cohort_flow <- ggplot(cohort_flow, aes(x = n, y = step)) +
  geom_col(fill = jama_cols[3], width = 0.6) +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.13))) +
  labs(x = "Participants remaining", y = NULL, title = "Cohort selection flow")


# ==============================================================================
# Write every result to Results/ (tables -> CSV, figures -> PDF+PNG+data CSV)
# ==============================================================================

# Part 1: smoking status
save_table(as_tibble(descriptive_status),   "descriptive_status")
save_table(as_tibble(table2_agreement),     "table2_agreement")
save_table(kappa_table_status,              "kappa_status")
save_table(performance_table_status,        "performance_status")

# Part 2: intensity
save_table(as_tibble(descriptive_intensity),          "descriptive_intensity")
save_table(kappa_intensity,                           "kappa_intensity")
save_table(as.data.frame(conf_matrix_intensity_full), "confusion_matrix_intensity")
save_table(performance_intensity_all,                 "performance_intensity")

# Part 3-4: continuous measures
save_table(icc_tbl_cigs,  "icc_cigarettes_daily")
save_table(err_tbl_cigs,  "error_cigarettes_daily")
save_table(icc_tbl_years, "icc_smoking_years")
save_table(err_tbl_years, "error_smoking_years")

# Supplemental EHR summaries (long = records, wide = persons)
save_table(suppl_1A, "supplemental_1A_patient_variables")
save_table(suppl_1B, "supplemental_1B_patient_concepts")
save_table(suppl_2A, "supplemental_2A_event_variables")
save_table(suppl_2B, "supplemental_2B_event_concepts")
save_table(as_tibble(descriptive_bias_comparison), "supplemental_selection_bias_comparison")

# Sensitivity analyses + cohort flow
save_table(sensitivity_3A, "sensitivity_3A_recency_windows")
save_table(sensitivity_3B, "sensitivity_3B_timelag_strata")
save_table(cohort_flow,    "cohort_flow_counts")

# Figures (each written with the data behind it)
save_figure(plot_cohort_flow,  "figure2_cohort_flow",       data = cohort_flow, width = 8, height = 4)
save_figure(plot_participants, "participants_by_indicator", data = df_participants)
save_figure(plot_records,      "records_by_indicator",      data = df_records)
save_figure(ba_plot_cigs,      "bland_altman_cigarettes",   data = ba_df_cigs)
save_figure(ba_plot_years,     "bland_altman_smoking_years", data = ba_df_years)


# -- Clean up intermediate objects from memory --------------------------------
# ------------------------------------------------------------------------------
rm(
  compared_smoking_status_df, compared_intensity_df,
  compared_cigarettes_daily_df, compared_smoking_years_df,
  long_tobacco_EHR_data, wide_tobacco_EHR_data,
  long_vars, long_labels, wide_vars, wide_labels,
  count_long, count_wide, manifest, d2,
  max_persons, max_records
)