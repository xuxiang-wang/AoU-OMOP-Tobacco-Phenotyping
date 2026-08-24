# ==============================================================================
# 02_classify_ehr.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2026-07-02
#
# Description:
# EHR classification for every person: product types from the Smoking Status
# concept crosswalk, intensity/duration, survey-anchored recency status, a
# concept dictionary, and a person-level wide table covering the full person set.
# ==============================================================================


# -- 0. Read inputs -----------------------------------------------------------
# ------------------------------------------------------------------------------
manifest <- read_manifest()

long_raw <- read_bq_export(
  manifest_path(manifest, "long_tobacco_ehr_raw"),
  col_types = cols(
    person_id           = col_double(),
    concept_id          = col_double(),
    value_as_concept_id = col_double(),
    value_as_number     = col_double(),
    event_date          = col_date(),
    survey_date         = col_date()
  )
)
assert_cols(long_raw, c("person_id", "survey_date", "event_date", "concept_id",
                        "value_as_number", "value_as_concept_id",
                        "is_current_flag", "is_former_flag", "is_non_flag",
                        "cessation_counseling", "medication_use", "cessation_attempt"))

# Full person-table spine so wide can cover everyone.
person_all <- read_bq_export(manifest_path(manifest, "person_all"),
                             col_types = cols(person_id = col_double()))

# date_of_birth (for duration). Read as character then coerce; format is
# environment-dependent -- verify duration_years has no unexpected NAs on run 1.
ehr_linked <- read_bq_export(
  manifest_path(manifest, "ehr_linked"),
  col_types = cols(person_id = col_double(), date_of_birth = col_character())
) %>%
  mutate(date_of_birth = as_datetime(date_of_birth))

# Concept crosswalk: (Id = concept_id, value_as_concept_id) -> product flags.
status_map <- read_csv(file.path(PARAMS$concept_set_dir, "Smoking Status.csv"),
                       show_col_types = FALSE)


# -- 1. Product-type flags from the concept crosswalk -------------------------
# ------------------------------------------------------------------------------
# The CSV marks product columns with 1 (NA elsewhere). Join on the (concept_id,
# value_as_concept_id) pair; events whose pair is not in the CSV get 0 (see the
# coverage caveat in the header). other_type_user is a single-concept rule the
# CSV does not carry.
prod_map <- status_map %>%
  transmute(
    concept_id          = as.double(Id),
    value_as_concept_id = as.double(value_as_concept_id),
    cigarette_user      = if_else(coalesce(cigarette, 0)      == 1, 1L, 0L),
    e_cigarette_user    = if_else(coalesce(`e-cigarette`, 0)  == 1, 1L, 0L),
    cigar_user          = if_else(coalesce(cigar, 0)          == 1, 1L, 0L),
    hookah_user         = if_else(coalesce(hookah, 0)         == 1, 1L, 0L),
    smokeless_user      = if_else(coalesce(smokeless, 0)      == 1, 1L, 0L)
  ) %>%
  distinct()


# -- 2. Enrich the LONG (event-level) table -----------------------------------
# ------------------------------------------------------------------------------
long_tobacco_EHR_data <- long_raw %>%
  left_join(prod_map, by = c("concept_id", "value_as_concept_id")) %>%
  # events with no crosswalk match -> product flags 0
  mutate(across(c(cigarette_user, e_cigarette_user, cigar_user,
                  hookah_user, smokeless_user), ~ coalesce(., 0L))) %>%
  # single-concept "other product" rule (not in the CSV)
  mutate(other_type_user = if_else(concept_id == 903663, 1L, 0L)) %>%
  # intensity: packs/day -> cigarettes/day (20/pack) -> ordinal category
  mutate(
    packs_per_day = case_when(
      concept_id == 3004518 ~ value_as_number,
      TRUE ~ NA_real_
    ),
    cigarettes_per_day = if_else(!is.na(packs_per_day), packs_per_day * 20, NA_real_),
    intensity_daily = case_when(
      concept_id == 903658 |
        (concept_id %in% c(903651, 903652) &
           value_as_concept_id %in% c(4148415, 4144273)
        ) | cigarettes_per_day <= 1 ~ "Trivial",
      
      concept_id == 903659 |
        (concept_id %in% c(903651, 903652, 43054909) &
           value_as_concept_id %in% c(762501, 4145798, 762498, 4042037, 4052029,
                                      45878118)
        ) | (cigarettes_per_day > 1 & cigarettes_per_day <= 9) ~ "Light",
      
      concept_id == 903660 |
        (concept_id %in% c(903651, 903652) &
           value_as_concept_id %in% c(4141782, 4209585, 4052030)
        ) | (cigarettes_per_day >= 10 & cigarettes_per_day <= 19) ~ "Moderate",
      
      concept_id == 903661 |
        (concept_id %in% c(903651, 903652, 43054909) &
           value_as_concept_id %in% c(762500, 4141783, 762499, 4041511, 45884038)
        ) | (cigarettes_per_day >= 20 & cigarettes_per_day <= 39) ~ "Heavy",
      
      concept_id == 903662 |
        (concept_id %in% c(903651, 903652) &
           value_as_concept_id %in% c(4141784, 4044777, 762499, 4041511, 45884038)
        ) | cigarettes_per_day >= 40 ~ "Very heavy",
      
      TRUE ~ NA_character_
    )
  ) %>%
  # duration (years): needs birth_year from date_of_birth
  left_join(ehr_linked %>% select(person_id, date_of_birth), by = "person_id") %>%
  mutate(
    birth_year = year(date_of_birth),
    event_year = year(event_date),
    duration_years = case_when(
      concept_id %in% c(36303803, 40770349) &
        ((event_year - value_as_number) > birth_year) ~ value_as_number,
      TRUE ~ NA_real_
    )
  ) %>%
  # keep only product columns; drop the birth/event-year helpers and date_of_birth
  select(
    person_id, survey_date, event_date, source,
    concept_id, value_as_number, value_as_concept_id,
    is_current_flag, is_former_flag, is_non_flag,
    cessation_counseling, medication_use, cessation_attempt,
    cigarette_user, e_cigarette_user, cigar_user,
    hookah_user, smokeless_user, other_type_user,
    packs_per_day, cigarettes_per_day, intensity_daily, duration_years
  )
# (current_smoker/former_smoker/non_smoker are appended next, in section 3)


# -- 3. Recency status (survey-anchored 12-month logic, window configurable) ---
# ------------------------------------------------------------------------------
# Original 12-month split, re-anchored to survey_date; window R configurable.
# current_smoker = is_current_flag OR (current event within R of survey);
# former_smoker  = is_former_flag  OR (current evidence older than R);
# non_smoker     = is_non_flag and neither. Three 0/1 columns, no NA.
classify_status <- function(events, lookback_months = PARAMS$recency_months) {
  events %>%
    group_by(person_id) %>%
    mutate(
      .cur_win = as.integer(any(is_current_flag == 1 & !is.na(survey_date) &
                                  event_date >= (survey_date %m-% months(lookback_months)) &
                                  event_date <= survey_date)),
      .prior   = as.integer(any(is_current_flag == 1 & !is.na(survey_date) &
                                  event_date <  (survey_date %m-% months(lookback_months)))),
      .former_win = as.integer(.cur_win == 0 & .prior == 1)
    ) %>%
    ungroup() %>%
    mutate(
      current_smoker = as.integer(is_current_flag == 1 | .cur_win == 1),
      former_smoker  = as.integer(is_former_flag  == 1 | .former_win == 1),
      non_smoker     = as.integer(is_non_flag == 1 & current_smoker == 0 & former_smoker == 0)
    ) %>%
    select(-.cur_win, -.prior, -.former_win)
}

long_tobacco_EHR_data <- classify_status(long_tobacco_EHR_data, PARAMS$recency_months)


# -- 4. WIDE (person level, covering every person) ----------------------------
# ------------------------------------------------------------------------------
# Per-person aggregates over the (all-person) event table.
person_activity <- long_tobacco_EHR_data %>%
  group_by(person_id) %>%
  summarise(
    n_events                  = n(),
    first_event_date          = min(event_date, na.rm = TRUE),
    last_event_date           = max(event_date, na.rm = TRUE),
    ever_smoker               = as.integer(any(is_current_flag == 1 | is_former_flag == 1)),
    ever_nonsmoker_record     = as.integer(any(is_non_flag == 1)),
    ever_cigarette            = as.integer(any(cigarette_user == 1)),
    ever_e_cigarette          = as.integer(any(e_cigarette_user == 1)),
    ever_cigar                = as.integer(any(cigar_user == 1)),
    ever_hookah               = as.integer(any(hookah_user == 1)),
    ever_smokeless            = as.integer(any(smokeless_user == 1)),
    ever_other_product        = as.integer(any(other_type_user == 1)),
    ever_cessation_counseling = as.integer(any(cessation_counseling == 1)),
    ever_medication_use       = as.integer(any(medication_use == 1)),
    ever_cessation_attempt    = as.integer(any(cessation_attempt == 1)),
    .groups = "drop"
  )

# Reference date per person: latest non-NA survey_date, else cutoff.
# Guard the all-NA case explicitly so summarise always returns length 1.
ref_by_person <- long_tobacco_EHR_data %>%
  group_by(person_id) %>%
  summarise(
    ref_date = {
      sv <- survey_date[!is.na(survey_date)]
      if (length(sv) > 0) max(sv) else PARAMS$CUTOFF_DATE
    },
    .groups = "drop"
  )

# Point-in-time status via classify_status(), feeding ref_date as survey_date,
# then collapsing the (non-exclusive) 0/1 columns to one status per person.
wide_status_tbl <- long_tobacco_EHR_data %>%
  select(person_id, event_date, is_current_flag, is_former_flag, is_non_flag) %>%
  left_join(ref_by_person, by = "person_id") %>%
  group_by(person_id) %>%
  summarise(
    cur_in_win = any(is_current_flag == 1 &
                       event_date >= (ref_date %m-% months(PARAMS$recency_months)) &
                       event_date <= ref_date, na.rm = TRUE),
    cur_before = any(is_current_flag == 1 &
                       event_date <  (ref_date %m-% months(PARAMS$recency_months)), na.rm = TRUE),
    any_former = any(is_former_flag == 1, na.rm = TRUE),
    any_non    = any(is_non_flag    == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    wide_current = as.integer(cur_in_win),
    wide_former  = as.integer(!cur_in_win & (cur_before | any_former)),
    wide_never   = as.integer(!cur_in_win & !(cur_before | any_former) & any_non),
    wide_status  = case_when(
      wide_current == 1 ~ "Current smoker",
      wide_former  == 1 ~ "Former smoker",
      wide_never   == 1 ~ "Non-smoker",
      TRUE              ~ "No smoking-status evidence"
    )
  ) %>%
  select(person_id, wide_current, wide_former, wide_never, wide_status)

# Spine = every person in the person table. wide is a person-level product table
# (ever-flags, counts, and one point-in-time status). People with no tobacco
# events get has_tobacco_record = 0 and 0/NA elsewhere.
wide_tobacco_EHR_data <- person_all %>%
  distinct(person_id) %>%
  left_join(person_activity, by = "person_id") %>%
  left_join(wide_status_tbl, by = "person_id") %>%
  mutate(
    has_tobacco_record = as.integer(!is.na(n_events)),
    n_events           = coalesce(n_events, 0L),
    wide_status        = coalesce(wide_status, "No smoking-status evidence"),
    across(c(ever_smoker, ever_nonsmoker_record,
             ever_cigarette, ever_e_cigarette, ever_cigar, ever_hookah,
             ever_smokeless, ever_other_product,
             ever_cessation_counseling, ever_medication_use, ever_cessation_attempt,
             wide_current, wide_former, wide_never),
           ~ coalesce(., 0L))
  ) %>%
  select(
    person_id, has_tobacco_record,
    wide_status, wide_current, wide_former, wide_never,
    ever_smoker, ever_nonsmoker_record,
    ever_cigarette, ever_e_cigarette, ever_cigar, ever_hookah,
    ever_smokeless, ever_other_product,
    ever_cessation_counseling, ever_medication_use, ever_cessation_attempt,
    n_events, first_event_date, last_event_date
  )


# -- 5. Concept dictionary ----------------------------------------------------
# ------------------------------------------------------------------------------
# One lookup for every code appearing in the events (concept_id and
# value_as_concept_id), with name / domain / vocabulary.
concept_ids <- long_tobacco_EHR_data %>%
  { c(.$concept_id, .$value_as_concept_id) } %>%
  unique()
concept_ids <- concept_ids[!is.na(concept_ids) & concept_ids != 0]

dict_sql <- str_glue("
  SELECT concept_id, concept_name, domain_id, vocabulary_id
  FROM `concept`
  WHERE concept_id IN ({paste(concept_ids, collapse = ', ')})
")
concept_dictionary <- retrieve_data(dict_sql)


# -- 6. Export and append the manifest ----------------------------------------
# ------------------------------------------------------------------------------
new_rows <- bind_rows(
  stage_to_bucket(long_tobacco_EHR_data, "long_tobacco_ehr"),
  stage_to_bucket(wide_tobacco_EHR_data, "wide_tobacco_ehr"),
  stage_to_bucket(concept_dictionary,    "concept_dictionary")
)
manifest <- append_manifest(manifest, new_rows)
save_manifest(manifest)


# -- 7. Clean up --------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(long_raw, person_all, ehr_linked, status_map, prod_map,
   person_activity, concept_ids, dict_sql, new_rows)
