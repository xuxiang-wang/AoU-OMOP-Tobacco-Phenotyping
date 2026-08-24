# ==============================================================================
# 03_compare.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2026-07-02
#
# Description:
# Processes the surveys, builds the analytic cohort (with per-step flow counts),
# pairs each member to the nearest EHR event within +/-365 days, and writes the
# status / cigarettes-daily / smoking-years matched sets.
# ==============================================================================


# -- 0. Read inputs from the bucket -------------------------------------------
# ------------------------------------------------------------------------------
manifest <- read_manifest()

long_tobacco_EHR_data <- read_bq_export(
  manifest_path(manifest, "long_tobacco_ehr"),
  col_types = cols(
    person_id           = col_double(),
    concept_id          = col_double(),
    value_as_concept_id = col_double(),
    value_as_number     = col_double(),
    event_date          = col_date(),
    survey_date         = col_date()
  )
)
visit_cohort   <- read_bq_export(manifest_path(manifest, "visit_cohort"),
                                 col_types = cols(person_id = col_double()))
basics_data    <- read_bq_export(manifest_path(manifest, "basics"),
                                 col_types = cols(person_id = col_double(),
                                                  survey_datetime = col_character()))
lifestyle_data <- read_bq_export(manifest_path(manifest, "lifestyle"),
                                 col_types = cols(person_id = col_double(),
                                                  survey_datetime = col_character()))
EHR_linked_data <- read_bq_export(
  manifest_path(manifest, "ehr_linked"),
  col_types = cols(person_id = col_double(), date_of_birth = col_character())
) %>%
  mutate(date_of_birth = as_datetime(date_of_birth))

# Persons with any tobacco event (drives has_tobacco_related_data below).
tobacco_person_ids <- unique(long_tobacco_EHR_data$person_id)


# -- 1. Process the 'Basics' survey data --
# ------------------------------------------------------------------------------

# Define question mapping
basics_question_mapping <- tibble(
  Q_label = paste0("Q_", 1:9),
  question = c(
    "The Basics: Birthplace",
    "Education Level: Highest Grade",
    "Marital Status: Current Marital Status",
    "Insurance: Health Insurance",
    "Health Insurance: Health Insurance Type",
    "Health Insurance: Insurance Type Update",
    "Employment: Employment Status",
    "Income: Annual Income",
    "Home Own: Current Home Own"
  )
)

# Join question mapping and clean data
basics_data <- basics_data %>%
  left_join(basics_question_mapping, by = "question") %>%
  filter(!is.na(Q_label)) %>%
  mutate(
    survey_datetime = as.Date(survey_datetime)
  )

# Pivot to wide format while keeping survey_datetime
basics_data <- basics_data %>%
  select(person_id, survey_datetime, Q_label, question_concept_id, answer) %>%
  pivot_wider(
    id_cols = c(person_id, survey_datetime),
    names_from = Q_label,
    values_from = answer,
    names_glue = "{.value}_{Q_label}",
    values_fn = list(answer = ~ first(.))
  )

# Ensure correct data types
basics_data <- basics_data %>%
  mutate(
    survey_datetime = as.Date(survey_datetime),
    across(starts_with("answer_"), as.character)
  )

# Grab the latest survey data per person
basics_data <- basics_data %>%
  arrange(person_id, desc(survey_datetime)) %>%
  group_by(person_id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(latest_survey_date = survey_datetime) %>%
  select(person_id, latest_survey_date, everything())


# Clean up output of basics data
basics_data <- basics_data %>%
  mutate(
    birthplace = case_when(
      answer_Q_1 == 'Birthplace: USA' ~ "USA",
      answer_Q_1 == 'PMI: Other' ~ "Other",
      TRUE ~ 'Missing/Unknown'
    ),
    education_level = case_when(
      answer_Q_2 == 'Highest Grade: Advanced Degree' ~ "Advanced Degree",
      answer_Q_2 == 'Highest Grade: College Graduate' ~ "College Graduate",
      answer_Q_2 == 'Highest Grade: College One to Three' ~ "College One to Three",
      answer_Q_2 == 'Highest Grade: Twelve Or GED' ~ "Twelve Or GED",
      answer_Q_2 == 'Highest Grade: Nine Through Eleven' ~ "Nine Through Eleven",
      answer_Q_2 %in% c('Highest Grade: Five Through Eight',
                        'Highest Grade: One Through Four',
                        'Highest Grade: Never Attended'
      ) ~ "<Nine",
      TRUE ~ "Missing/Unknown"
    ),
    marital_status = case_when(
      answer_Q_3 == 'Current Marital Status: Married' ~ "Married",
      answer_Q_3 == 'Current Marital Status: Never Married' ~ "Never Married",
      answer_Q_3 == 'Current Marital Status: Divorced' ~ "Divorced",
      answer_Q_3 %in% c('Current Marital Status: Living With Partner',
                        'Current Marital Status: Separated',
                        'Current Marital Status: Widowed'
      ) ~ "Other",
      TRUE ~ "Missing/Unknown"
    ),
    health_insurance = case_when(
      answer_Q_4 == 'Health Insurance: Yes' ~ "Yes",
      answer_Q_4 == 'Health Insurance: No' ~ "No",
      TRUE ~ "Missing/Unknown"
    ),
    employment_status = case_when(
      answer_Q_7 %in% c('Employment Status: Employed For Wages',
                        'Employment Status: Self Employed') ~ "Employed",
      answer_Q_7 == 'Employment Status: Retired' ~ "Retired",
      answer_Q_7 == 'Employment Status: Unable To Work' ~ "Unable To Work",
      answer_Q_7 %in% c('Employment Status: Out Of Work One Or More',
                        'Employment Status: Out Of Work Less Than One') ~ "Out of Work",
      answer_Q_7 == 'Employment Status: Student' ~ "Student",
      answer_Q_7 == 'Employment Status: Homemaker' ~ "Homemaker",
      TRUE ~ "Missing/Unknown"
    ),
    annual_household_income = case_when(
      answer_Q_8 %in% c('Annual Income: less 10k',
                        'Annual Income: 10k 25k') ~ "Less than $25k",
      answer_Q_8 %in% c('Annual Income: 25k 35k',
                        'Annual Income: 35k 50k') ~ "$25k-$50k",
      answer_Q_8 == 'Annual Income: 50k 75k' ~ "$50k-$75k",
      answer_Q_8 == 'Annual Income: 75k 100k' ~ "$75k-$100k",
      answer_Q_8 %in% c('Annual Income: 100k 150k',
                        'Annual Income: 150k 200k') ~ "$100k-$200k",
      answer_Q_8 == 'Annual Income: more 200k' ~ "More than $200k",
      TRUE ~ "Missing/Unknown"
    ),
    current_home_own = case_when(
      answer_Q_9 == 'Current Home Own: Own' ~ "Own",
      answer_Q_9 == 'Current Home Own: Rent' ~ "Rent",
      answer_Q_9 == 'Current Home Own: Other Arrangement' ~ "Other Arrangement",
      TRUE ~ "Missing/Unknown"
    )
  )

basics_data <- subset(basics_data, select = c('person_id', 'birthplace',
                                              "education_level", "marital_status", "health_insurance", "employment_status",
                                              "annual_household_income", "current_home_own"))


# -- 2. Process the 'Lifestyle' survey data --
# ------------------------------------------------------------------------------

# Map survey questions to standardized labels
lifestyle_data <- lifestyle_data %>%
  left_join(
    tibble(
      Q_label = paste0("Q_", 1:31),
      question_text = c(
        "Smoking: 100 Cigs Lifetime", "Smoking: Smoke Frequency", "Smoking: Daily Smoke Starting Age",
        "Smoking: Number Of Years", "Smoking: Serious Quit Attempt", "Attempt Quit Smoking: Completely Quit Age",
        "Smoking: Current Daily Cigarette Number", "Smoking: Average Daily Cigarette Number",
        "Electronic Smoking: Electric Smoke Participant", "Electronic Smoking: Electric Smoke Frequency",
        "Cigar Smoking: Cigar Smoke Participant", "Cigar Smoking: Current Cigar Frequency",
        "Hookah Smoking: Hookah Smoke Participant", "Hookah Smoking: Current Hookah Frequency",
        "Smokeless Tobacco: Smokeless Tobacco Participant", "Smokeless Tobacco: Smokeless Tobacco Frequency",
        "Alcohol: Alcohol Participant", "Alcohol: Drink Frequency Past Year", "Alcohol: Average Daily Drink Count",
        "Alcohol: 6 or More Drinks Occurrence", "Recreational Drug Use: Which Drugs Used",
        "Past 3 Month Use Frequency: Marijuana 3 Month Use", "Past 3 Month Use Frequency: Cocaine 3 Month Use",
        "Past 3 Month Use Frequency: Prescription Stimulant 3 Month Use", "Past 3 Month Use Frequency: Other Stimulant 3 Month Use",
        "Past 3 Month Use Frequency: Inhalant 3 Month Use", "Past 3 Month Use Frequency: Sedative 3 Month Use",
        "Past 3 Month Use Frequency: Hallucinogen 3 Month Use", "Past 3 Month Use Frequency: Street Opioid 3 Month Use",
        "Past 3 Month Use Frequency: Prescription Opioid 3 Month Use", "Past 3 Month Use Frequency: Other 3 Month Use"
      )
    ),
    by = c("question" = "question_text")
  ) %>%
  filter(!is.na(Q_label))

# Reshape the dataset to a wide format
lifestyle_data <- lifestyle_data %>%
  select(person_id, survey_datetime, Q_label, answer) %>%
  pivot_wider(
    id_cols = c(person_id, survey_datetime),
    names_from = Q_label,
    values_from = answer,
    names_glue = "answer_{Q_label}",
    values_fn = list(answer = ~ first(.))
  )

tobacco_use_cols <- c("person_id", "survey_datetime")
for (i in 1:16) {
  tobacco_use_cols <- c(tobacco_use_cols, paste0("answer_Q_", i))
}

lifestyle_data <- lifestyle_data %>%
  select(all_of(tobacco_use_cols))


# Standardize response values for relevant substance-related questions
patterns <- list(
  answer_Q_1 = "(Yes|No|Don't Know|Prefer Not To Answer)$",
  answer_Q_2 = "(Every Day|Some Days|Not At All|Don't Know|Prefer Not To Answer)$",
  answer_Q_5 = "(Attempt Quit Smoking|No Attempt Quit Smoking|Don't Know|Prefer Not To Answer)$",
  answer_Q_9 = "(No|Yes|Don't Know|Prefer Not To Answer)$",
  answer_Q_10 = "(Not At All|Some Days|Every Day|Don't Know|Prefer Not To Answer)$",
  answer_Q_11 = "(No|Yes|Don't Know|Prefer Not To Answer)$",
  answer_Q_12 = "(Not At All|Some Days|Every Day|Don't Know|Prefer Not To Answer)$",
  answer_Q_13 = "(No|Yes|Don't Know|Prefer Not To Answer)$",
  answer_Q_14 = "(Not At All|Some Days|Every Day|Don't Know|Prefer Not To Answer)$",
  answer_Q_15 = "(No|Yes|Don't Know|Prefer Not To Answer)$",
  answer_Q_16 = "(Not At All|Some Days|Every Day|Don't Know|Prefer Not To Answer)$"
)

lifestyle_data <- lifestyle_data %>%
  mutate(
    across(names(patterns), ~ str_extract(.x, patterns[[cur_column()]]))
  )

# Select the most recent survey response, prioritizing those with a valid 100 Cigs Lifetime answer
lifestyle_data <- lifestyle_data %>%
  arrange(person_id, desc(survey_datetime)) %>%
  group_by(person_id) %>%
  mutate(has_valid_100_cigs = !is.na(answer_Q_1) & answer_Q_1 %in% c("Yes", "No")) %>%
  filter(if (any(has_valid_100_cigs)) has_valid_100_cigs else row_number() == 1) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(latest_survey_date = survey_datetime) %>%
  select(person_id, latest_survey_date, everything())

# Create derived smoking status indicators
lifestyle_data <- lifestyle_data %>%
  mutate(
    survey_date = as.Date(survey_datetime),
    
    smoking_100_cigs = case_when(
      answer_Q_1 == "Yes" ~ 1,
      answer_Q_1 == "No" ~ 0,
      TRUE ~ NA_real_
    ),
    
    smoking_frequency = answer_Q_2,
    smoking_start_age = suppressWarnings(as.numeric(answer_Q_3)),
    smoking_years = suppressWarnings(as.numeric(answer_Q_4)),
    smoking_quit_attempt = case_when(
      answer_Q_5 == "Attempt Quit Smoking" ~ 1,
      answer_Q_5 == "No Attempt Quit Smoking" ~ 0,
      TRUE ~ NA_real_
    ),
    smoking_quit_age = suppressWarnings(as.numeric(answer_Q_6)),
    smoking_current_cigarettes_per_day = suppressWarnings(as.numeric(answer_Q_7)),
    smoking_avg_daily_cigarettes = suppressWarnings(as.numeric(answer_Q_8)),
    
    electric_smoking = ifelse(
      answer_Q_9 == 'Yes', 1, 0
    ),
    electric_smoking_frequency = answer_Q_10,
    
    cigar_smoking = ifelse(
      answer_Q_11 == 'Yes', 1, 0
    ),
    cigar_smoking_frequency = answer_Q_12,
    
    hookah_smoking = ifelse(
      answer_Q_13 == 'Yes', 1, 0
    ),
    hookah_smoking_frequency = answer_Q_14,
    
    smokeless_smoking = ifelse(
      answer_Q_15 == 'Yes', 1, 0
    ),
    smokeless_smoking_frequency = answer_Q_16,
    
    intensity_current_daily = case_when(
      smoking_current_cigarettes_per_day <=  1 ~ "Trivial",
      
      (smoking_current_cigarettes_per_day > 1 & smoking_current_cigarettes_per_day <=  9) ~ "Light",
      
      (smoking_current_cigarettes_per_day >= 10 & smoking_current_cigarettes_per_day <= 19) ~ "Moderate",
      
      (smoking_current_cigarettes_per_day >= 20 & smoking_current_cigarettes_per_day <= 39)~ "Heavy",
      
      smoking_current_cigarettes_per_day >= 40 ~ "Very heavy",
      
      TRUE ~ NA_character_
    ),
    
    intensity_avg_daily = case_when(
      smoking_avg_daily_cigarettes <=  1 ~ "Trivial",
      
      (smoking_avg_daily_cigarettes > 1 & smoking_avg_daily_cigarettes <=  9) ~ "Light",
      
      (smoking_avg_daily_cigarettes >= 10 & smoking_avg_daily_cigarettes <= 19) ~ "Moderate",
      
      (smoking_avg_daily_cigarettes >= 20 & smoking_avg_daily_cigarettes <= 39)~ "Heavy",
      
      smoking_avg_daily_cigarettes >= 40 ~ "Very heavy",
      
      TRUE ~ NA_character_
    ),
    
    # Define mutually exclusive current and former smoker indicators
    is_current_smoker = ifelse(
      smoking_100_cigs == 1 & smoking_frequency %in% c("Every Day", "Some Days") & is.na(smoking_quit_age),
      1, 0
    ),
    
    is_former_smoker = ifelse(
      smoking_100_cigs == 1 & (smoking_frequency == "Not At All" | !is.na(smoking_quit_age)),
      1, 0
    ),
    
    # Assign categorical smoking status
    smoking_status = case_when(
      is_current_smoker == 1 ~ "Current Smoker",
      is_former_smoker == 1 ~ "Former Smoker",
      smoking_100_cigs == 1 & (smoking_frequency %in% c("Don't Know", "Prefer Not To Answer") | is.na(smoking_frequency)) ~ "Current or Former Smoker",
      smoking_100_cigs == 0 ~ "Non-Smoker",
      TRUE ~ "Unknown"
    )
  ) %>%
  arrange(person_id, survey_datetime)


# -- 3. Combine data sources and create the final analytic cohort --
# ------------------------------------------------------------------------------

combined_df <- basics_data %>%
  full_join(lifestyle_data, by = "person_id") %>%
  full_join(EHR_linked_data, by = "person_id") %>%
  mutate(smoking_status = replace_na(smoking_status, "Unknown"), 
         age = floor(interval(start = date_of_birth, end = survey_datetime) / years(1)),
         # Age Group
         age_group = case_when(
           age < 18 ~ "<18",
           age < 45 ~ "18-44",
           age < 65 ~ "45-64",
           age >= 65 ~ "65+",
           TRUE ~ "Missing/Unknown"
         ),
         
         # Race Categories
         race = case_when(
           race_concept_id == 8516 ~ "Black or African American",
           race_concept_id == 2100000001 ~ "Hispanic, Latino, or Spanish",
           race_concept_id == 8527 ~ "White",
           race_concept_id == 8515 ~ "Asian",
           race_concept_id == 2000000008 ~ "Multiple",
           race_concept_id == 45882607 ~ "None of these",
           race_concept_id == 38003615 ~ "Middle Eastern or North African",
           race_concept_id == 1177221 ~ "I prefer not to answer",
           race_concept_id == 8557 ~ "Native Hawaiian or Other Pacific Islander",
           TRUE ~ NA_character_
         ),
         
         # Clean Race
         clean_race = case_when(
           race_concept_id == 8527 ~ "White",
           race_concept_id == 8516 ~ "Black or African American",
           race_concept_id == 2000000008 ~ "Multiple",
           race_concept_id %in% c(8515, 8557, 2100000001, 45882607, 38003615) ~ "Other",
           is.na(race_concept_id) ~ "Missing/Unknown",
           TRUE ~ "Missing/Unknown"
         ),
         
         # Ethnicity Categories
         ethnicity = case_when(
           ethnicity_concept_id == 903079 ~ "Prefer Not To Answer",
           ethnicity_concept_id == 903096 ~ "Skip",
           ethnicity_concept_id == 1586148 ~ "None of these fully describe me",
           ethnicity_concept_id == 38003563 ~ "Hispanic or Latino",
           ethnicity_concept_id == 38003564 ~ "Not Hispanic or Latino",
           TRUE ~ NA_character_
         ),
         
         # Clean Ethnicity
         clean_ethnicity = case_when(
           is.na(ethnicity_concept_id) ~ "Missing/Unknown",
           ethnicity_concept_id == 38003563 ~ "Hispanic or Latino",
           ethnicity_concept_id == 38003564 ~ "Not Hispanic or Latino",
           ethnicity_concept_id == 1586148 ~ "Other",
           TRUE ~ "Missing/Unknown"
         ),
         
         # Race and Ethnicity Combination
         race_and_ethnicity = case_when(
           clean_ethnicity == "Hispanic or Latino" ~ 'Hispanic/Latinx',
           clean_race == "White" & clean_ethnicity == "Not Hispanic or Latino" ~ 'Non-Hispanic/Latinx White',
           clean_race == "Black or African American" & clean_ethnicity == "Not Hispanic or Latino" ~ 'Non-Hispanic/Latinx Black or AA',
           clean_race == "Multiple" & clean_ethnicity == "Not Hispanic or Latino" ~ 'Non-Hispanic/Latinx Multiple',
           clean_race == "Other" & clean_ethnicity == "Not Hispanic or Latino" ~ 'Non-Hispanic/Latinx Other',
           clean_race == "Missing/Unknown" & clean_ethnicity == "Missing/Unknown" ~ "Missing/Unknown",
           TRUE ~ "Other/Unclassified"
         ),
         
         has_EHR_data = if_else(person_id %in% EHR_linked_data$person_id, 1, 0),
         has_visit_data = if_else(person_id %in% visit_cohort$person_id, 1, 0),
         has_tobacco_related_data = if_else(person_id %in% tobacco_person_ids, 1, 0)
  )

# Apply cohort inclusion/exclusion criteria sequentially
# Sequential inclusion/exclusion, recording person counts at each step so file 4
# can draw the Figure 2 flow diagram. Age bounds come from PARAMS.
count_step <- function(df, label) tibble(step = label, n = n_distinct(df$person_id))

cohort_flow <- count_step(combined_df, "Survey + demographics available")
combined_df <- combined_df %>% filter(has_EHR_data == 1)
cohort_flow <- bind_rows(cohort_flow, count_step(combined_df, "Has EHR data"))
combined_df <- combined_df %>% filter(!is.na(age) & age >= PARAMS$age_min & age <= PARAMS$age_max)
cohort_flow <- bind_rows(cohort_flow, count_step(combined_df, "Age within eligible range"))
combined_df <- combined_df %>% filter(smoking_status != "Unknown")
cohort_flow <- bind_rows(cohort_flow, count_step(combined_df, "Known survey smoking status"))
combined_df <- combined_df %>% filter(sex_at_birth %in% c("Female", "Male"))
cohort_flow <- bind_rows(cohort_flow, count_step(combined_df, "Female/Male sex at birth"))
combined_df <- combined_df %>% filter(has_visit_data == 1)
cohort_flow <- bind_rows(cohort_flow, count_step(combined_df, "Eligible visit within 1 year"))

eligible_base <- combined_df

combined_df <- combined_df %>% filter(has_tobacco_related_data == 1)
cohort_flow <- bind_rows(cohort_flow, count_step(combined_df, "Has tobacco-related EHR record"))

# -- Snapshot the person-level cohort before the merge overwrites combined_df --
# ------------------------------------------------------------------------------
cohort <- combined_df


# -- 3. Create the final matched datasets for analysis --
# ------------------------------------------------------------------------------
# This section merges the clean cohort with the enriched EHR data and finds the
# closest EHR record within a 1-year window of each person's survey date.

# Convert to data.table for faster merging
dt_survey <- as.data.table(combined_df)
# long now carries its own survey_date (file 1 LEFT JOIN); drop it so the merge
# uses the cohort's survey_date.
dt_ehr <- as.data.table(long_tobacco_EHR_data %>% select(-survey_date))


# Merge by person_id
merged <- dt_ehr[dt_survey, on = .(person_id), allow.cartesian = TRUE]

# Keep only EHR events within ±365 days of survey_date
filtered <- merged[abs(event_date - survey_date) <= 365]

# Overwrite combined_df with this new, event-level matched data frame
combined_df <- as.data.frame(filtered)


# Define variable lists for each comparison
varlist_demographics <- c(
  "age", "age_group", "race_and_ethnicity", "education_level", "marital_status",
  "health_insurance", "employment_status", "annual_household_income", "current_home_own"
)

varlist_smoking_status <- c(
  "person_id", "event_date", "survey_date", "current_smoker", "former_smoker",
  "non_smoker", "smoking_status"
)

varlist_intensity <- c(
  "person_id", "event_date", "survey_date", "intensity_daily",
  "intensity_current_daily", "intensity_avg_daily"
)

varlist_smoking_years <- c(
  "person_id", "event_date", "survey_date", "duration_years", "smoking_years"
)

varlist_cigarettes_daily <- c(
  "person_id", "event_date", "survey_date", "cigarettes_per_day",
  "smoking_current_cigarettes_per_day", "smoking_avg_daily_cigarettes"
)

# Create `compared_smoking_status_df`
compared_smoking_status_df <- combined_df %>%
  select(all_of(c(varlist_smoking_status, varlist_demographics))) %>%
  filter(
    !if_all(c(current_smoker, former_smoker, non_smoker), is.na) |
      !is.na(smoking_status)
  ) %>%
  mutate(date_diff = abs(as.numeric(event_date - survey_date))) %>%
  arrange(person_id, event_date) %>%
  group_by(person_id, survey_date) %>%
  slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-date_diff)

# Create `compared_intensity_df`
compared_intensity_df <- combined_df %>%
  select(all_of(c(varlist_intensity, varlist_demographics))) %>%
  filter(
    !is.na(intensity_daily),
    !is.na(intensity_current_daily) | !is.na(intensity_avg_daily)
  )%>%
  mutate(date_diff = abs(as.numeric(event_date - survey_date))) %>%
  arrange(person_id, event_date) %>%
  group_by(person_id, survey_date) %>%
  slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-(date_diff))

# Create `compared_smoking_years_df`
compared_smoking_years_df <- combined_df %>%
  select(all_of(c(varlist_smoking_years, varlist_demographics))) %>%
  filter(
    !is.na(duration_years),
    !is.na(smoking_years)
  )%>%
  mutate(date_diff = abs(as.numeric(event_date - survey_date))) %>%
  arrange(person_id, event_date) %>%
  group_by(person_id, survey_date) %>%
  slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-(date_diff))

# Create `compared_cigarettes_daily_df`
compared_cigarettes_daily_df <- combined_df %>%
  select(all_of(c(varlist_cigarettes_daily, varlist_demographics))) %>%
  filter(
    !is.na(cigarettes_per_day),
    !is.na(smoking_current_cigarettes_per_day) | !is.na(smoking_avg_daily_cigarettes)
  )%>%
  mutate(date_diff = abs(as.numeric(event_date - survey_date))) %>%
  arrange(person_id, event_date) %>%
  group_by(person_id, survey_date) %>%
  slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-(date_diff))

# -- Selection-bias comparison groups (person level) --------------------------
# Within the eligibility base, split into three mutually exclusive groups so
# reviewers can assess non-random exclusion:
#   1. No tobacco-related records at all
#   2. Tobacco records present, but none within 1 year of the survey date
#   3. Final analytic cohort (a tobacco event within 1 year of the survey date)
final_cohort_ids <- unique(compared_smoking_status_df$person_id)
bias_comparison_cohort <- eligible_base %>%
  mutate(
    cohort_group = case_when(
      person_id %in% final_cohort_ids ~ "Final analytic cohort",
      has_tobacco_related_data == 1   ~ "Tobacco records, none within 1 year",
      TRUE                            ~ "No tobacco-related records"
    ),
    cohort_group = factor(cohort_group, levels = c(
      "No tobacco-related records",
      "Tobacco records, none within 1 year",
      "Final analytic cohort"
    ))
  ) %>%
  select(person_id, cohort_group, all_of(varlist_demographics), smoking_status)

# -- Export cohort and the three matched sets, append the manifest ------------
# ------------------------------------------------------------------------------
# Final analytic-sample count (persons with an EHR event within 365 days).
cohort_flow <- bind_rows(
  cohort_flow,
  count_step(compared_smoking_status_df, "EHR event within 365 days (final sample)")
)

compared_sets <- list(
  cohort                    = cohort,
  cohort_flow               = cohort_flow,
  bias_comparison_cohort    = bias_comparison_cohort,
  compared_smoking_status   = compared_smoking_status_df,
  compared_intensity        = compared_intensity_df,
  compared_cigarettes_daily = compared_cigarettes_daily_df,
  compared_smoking_years    = compared_smoking_years_df
)
new_rows <- imap_dfr(compared_sets, ~ stage_to_bucket(.x, .y))
manifest <- append_manifest(manifest, new_rows)
save_manifest(manifest)


# -- Clean up -----------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(long_tobacco_EHR_data, visit_cohort, basics_data, lifestyle_data,
   EHR_linked_data, tobacco_person_ids, compared_sets, new_rows)