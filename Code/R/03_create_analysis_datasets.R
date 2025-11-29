# ==============================================================================
# 03_create_analysis_datasets.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2025-09-26
#
# Description:
# This script takes the processed cohort and raw EHR data to generate the final,
# analysis-ready datasets. It enriches the EHR data with phenotype definitions,
# matches EHR records to survey dates, and creates the specific 'compared_*_df'
# data frames for validation.
# ==============================================================================


# -- 1. Enrich the LONG format EHR data with phenotype variables --
# ------------------------------------------------------------------------------
long_tobacco_EHR_data <- long_tobacco_EHR_data %>%
  filter(person_id %in% combined_df$person_id) %>%
  mutate(
    current_smoker = if_else(is_current_flag == 1 | current_12_month == 1, 1, 0),
    former_smoker = if_else(is_former_flag == 1 | former_12_month == 1, 1, 0),
    non_smoker = if_else(is_non_flag == 1 & current_smoker == 0 & former_smoker == 0, 1, 0)
  )


long_tobacco_EHR_data <- long_tobacco_EHR_data %>%
  # bring in date_of_birth
  left_join(
    EHR_linked_data %>% select(person_id, date_of_birth),
    by = "person_id"
  ) %>%
  
  mutate(
    birth_year = year(date_of_birth),
    event_year = year(event_date),
    
    # Type Indicators
    e_cigarette_user = if_else(
      concept_id == 903655 |
        (concept_id %in% c(903652, 43054909) &
           value_as_concept_id %in% c(36716478, 37203948)
        ),
      1, 0
    ),
    
    cigar_user = if_else(
      concept_id == 903664 |
        (concept_id %in% c(903652, 903651) &
           value_as_concept_id %in% c(4246415, 4052949)
        ),
      1, 0
    ),
    
    
    hookah_user = if_else(
      concept_id == 903665, 1, 0
    ),
    
    smokeless_user = if_else(
      concept_id %in% c(903666, 903667, 903668, 903669) |
        (concept_id == 903652 &
           value_as_concept_id %in% c(764567, 37017610, 4218741, 4043058,
                                      4269998, 4038738)
        ),
      1, 0
    ),
    
    other_type_user = if_else(
      concept_id == 903663, 1, 0
    ),
    
    cigaretee_user = if_else(
      concept_id %in% c(903657, 903658, 903659, 903660, 903661, 903662) |
        (concept_id == 903652 &
           value_as_concept_id %in% c(4276526, 4044775, 4092281, 4148416, 4042037,
                                      4144273, 4052029, 4052030, 4145798, 4044777,
                                      4141782, 4041511, 4141783, 4141784, 4148415,
                                      4044778
           )
        ) |
        (current_smoker == 1 &
           e_cigarette_user == 0 &
           cigar_user       == 0 &
           hookah_user      == 0 &
           smokeless_user   == 0
        ),
      1, 0
    ),
    
    # Intensity
    packs_per_day = case_when(
      concept_id == 3004518 ~ value_as_number,
      TRUE ~ NA_real_
    ),
    
    # convert to cigarettes per day (assuming 20 cigs/pack)
    cigarettes_per_day = if_else(
      !is.na(packs_per_day),
      packs_per_day * 20,
      NA_real_
    ),
    
    intensity_daily = case_when(
      concept_id == 903658 |
        (concept_id %in% c(903651, 903652) &
           value_as_concept_id %in% c(4148415, 4144273)
        ) | cigarettes_per_day <=  1 ~ "Trivial",
      
      concept_id == 903659 |
        (concept_id %in% c(903651, 903652, 43054909) &
           value_as_concept_id %in% c(762501, 4145798, 762498, 4042037, 4052029,
                                      45878118)
        ) | (cigarettes_per_day > 1 & cigarettes_per_day <=  9) ~ "Light",
      
      concept_id == 903660 |
        (concept_id %in% c(903651, 903652) &
           value_as_concept_id %in% c(4141782, 4209585, 4052030)
        ) | (cigarettes_per_day >= 10 & cigarettes_per_day <= 19) ~ "Moderate",
      
      concept_id == 903661 |
        (concept_id %in% c(903651, 903652, 43054909) &
           value_as_concept_id %in% c(762500, 4141783, 762499, 4041511, 45884038)
        ) | (cigarettes_per_day >= 20 & cigarettes_per_day <= 39)~ "Heavy",
      
      concept_id == 903662 |
        (concept_id %in% c(903651, 903652) &
           value_as_concept_id %in% c(4141784, 4044777, 762499, 4041511, 45884038)
        ) | cigarettes_per_day >= 40 ~ "Very heavy",
      
      TRUE ~ NA_character_
    ),
    
    # valid duration years
    duration_years = case_when(
      concept_id %in% c(36303803, 40770349) &
        ((event_year - value_as_number) > birth_year) ~ value_as_number,
      TRUE ~ NA_real_
    )
  )


# -- 2. Enrich the WIDE format EHR data --
# ------------------------------------------------------------------------------
wide_tobacco_EHR_data <- wide_tobacco_EHR_data %>%
  filter(person_id %in% combined_df$person_id)


# Join demographics and/or survey summary fields
wide_tobacco_EHR_data <- wide_tobacco_EHR_data %>%
  left_join(EHR_linked_data, by = "person_id") %>%
  left_join(
    lifestyle_data %>%
      group_by(person_id) %>%
      arrange(desc(survey_datetime)) %>%
      slice(1),
    by = "person_id"
  )


# -- 3. Create the final matched datasets for analysis --
# ------------------------------------------------------------------------------
# This section merges the clean cohort with the enriched EHR data and finds the
# closest EHR record within a 1-year window of each person's survey date.

# Convert to data.table for faster merging
dt_survey <- as.data.table(combined_df)
dt_ehr <- as.data.table(long_tobacco_EHR_data)


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
  "non_smoker", "is_current_flag", "is_former_flag", "is_non_flag",
  "current_12_month", "former_12_month", "smoking_status"
)

varlist_tobacco_types <- c(
  "person_id", "event_date", "survey_date", "cigaretee_user", "e_cigarette_user",
  "cigar_user", "hookah_user", "smokeless_user", "other_type_user", "smoking_100_cigs",
  "electric_smoking", "cigar_smoking", "hookah_smoking", "smokeless_smoking"
)

varlist_cessation_attempts <- c(
  "person_id", "event_date", "survey_date", "cessation_attempt", "smoking_quit_attempt"
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
  group_by(person_id, survey_date) %>%
  slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-date_diff)

# Create `compared_tobacco_types_df`
compared_tobacco_types_df <- combined_df %>%
  select(all_of(c(varlist_tobacco_types, varlist_demographics))) %>%
  filter(
    !if_all(c(cigaretee_user, e_cigarette_user, cigar_user, hookah_user, smokeless_user, other_type_user), is.na) |
      !if_all(c(smoking_100_cigs, electric_smoking, cigar_smoking, hookah_smoking, smokeless_smoking), is.na)
  ) %>%
  mutate(date_diff = abs(as.numeric(event_date - survey_date))) %>%
  group_by(person_id, survey_date) %>%
  slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-date_diff)

# Create `compared_cessation_attempts_df`
compared_cessation_attempts_df <- combined_df %>%
  select(all_of(c(varlist_cessation_attempts, varlist_demographics))) %>%
  filter(
    !is.na(cessation_attempt),
    !is.na(smoking_quit_attempt)
  )%>%
  mutate(date_diff = abs(as.numeric(event_date - survey_date))) %>%
  group_by(person_id, survey_date) %>%
  slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-(date_diff))

# Create `compared_intensity_df`
compared_intensity_df <- combined_df %>%
  select(all_of(c(varlist_intensity, varlist_demographics))) %>%
  filter(
    !is.na(intensity_daily),
    !is.na(intensity_current_daily) | !is.na(intensity_avg_daily)
  )%>%
  mutate(date_diff = abs(as.numeric(event_date - survey_date))) %>%
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
  group_by(person_id, survey_date) %>%
  slice_min(order_by = date_diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-(date_diff))


# -- 4. Clean up intermediate objects from memory --
# ------------------------------------------------------------------------------
rm(dt_survey, dt_ehr, filtered, merged)
rm(varlist_demographics, varlist_smoking_status, varlist_tobacco_types,
   varlist_cessation_attempts, varlist_intensity, varlist_smoking_years,
   varlist_cigarettes_daily)