# ==============================================================================
# 01_load_data.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2026-07-02
#
# Description:
# Pulls the widest event superset once (all persons, no cohort filter, unbounded
# in time) plus survey/demographic tables, and exports them to the bucket with a
# fresh run manifest. survey_date is attached per event via LatestSurvey.
# ==============================================================================


# -- 1. Read in local concept sets for cessation --
# ------------------------------------------------------------------------------
nicotine_cessation_concepts_sets <- read_csv(file.path(PARAMS$concept_set_dir, "Nicotine_for_Cessation.csv"))
cess_ids <- paste(nicotine_cessation_concepts_sets$Id, collapse = ", ")

medication_cessation_concepts_sets <- read_csv(file.path(PARAMS$concept_set_dir, "Medication Cessation.csv"))
med_ids <- paste(medication_cessation_concepts_sets$Id, collapse = ", ")


# -- 2. Define and execute SQL queries to retrieve data from BigQuery --
# ------------------------------------------------------------------------------


# Grab visit data to define cohort with recent outpatient/telehealth visits
visit_sql <- paste("
WITH
DescendantConcepts AS (
  SELECT
    ca.descendant_concept_id,
    CASE
      WHEN ca.ancestor_concept_id = 9202 THEN 'Outpatient_Visit'
      WHEN ca.ancestor_concept_id = 9201 THEN 'Inpatient_Visit'
      WHEN ca.ancestor_concept_id = 9203 THEN 'ER_Visit'
      WHEN ca.ancestor_concept_id = 262 THEN 'ER_Inpatient_Visit'
      WHEN ca.ancestor_concept_id = 42898160 THEN 'Non_Hospital_Visit'
      WHEN ca.ancestor_concept_id = 581476 THEN 'Home_Visit'
      WHEN ca.ancestor_concept_id = 722455 THEN 'Telehealth_Visit'
      WHEN ca.ancestor_concept_id = 581458 THEN 'Pharmacy_Visit'
      WHEN ca.ancestor_concept_id = 32036 THEN 'Laboratory_Visit'
      WHEN ca.ancestor_concept_id = 581478 THEN 'Ambulance_Visit'
      WHEN ca.ancestor_concept_id = 38004193 THEN 'Case_Management_Visit'
      ELSE 'Other'
    END AS visit_type
  FROM `concept_ancestor` ca
  WHERE ca.ancestor_concept_id IN (
    9201, 9203, 262, 42898160, 9202, 581476, 722455, 581458, 32036, 581478, 38004193
  )
),

LatestSurvey AS (
  SELECT
    answer.person_id,
    MAX(DATE(answer.survey_datetime)) AS latest_survey_date
  FROM `ds_survey` answer
  WHERE question_concept_id IN (
      SELECT DISTINCT concept_id
      FROM `cb_criteria` c
      JOIN (SELECT CAST(cr.id AS STRING) AS id
            FROM `cb_criteria` cr
            WHERE concept_id IN (1585855) AND domain_id = 'SURVEY') a
      ON c.path LIKE CONCAT('%', a.id, '.%')
      WHERE domain_id = 'SURVEY' AND type = 'PPI' AND subtype = 'QUESTION'
  )
  GROUP BY answer.person_id
),

-- Restrict to Outpatient/Telehealth visits only within ±1 year of the survey date
EligibleVisits AS (
  SELECT
    v.person_id,
    v.visit_start_date,
    s.latest_survey_date
  FROM `visit_occurrence` v
  LEFT JOIN DescendantConcepts dc ON v.visit_concept_id = dc.descendant_concept_id
  JOIN LatestSurvey s ON v.person_id = s.person_id
  WHERE dc.visit_type IN ('Outpatient_Visit', 'Telehealth_Visit')
    AND v.visit_start_date BETWEEN DATE_SUB(s.latest_survey_date, INTERVAL 1 YEAR)
                              AND DATE_ADD(s.latest_survey_date, INTERVAL 1 YEAR)
)

SELECT DISTINCT person_id
FROM EligibleVisits")


# Long event pull: all tobacco events for cohort members (unbounded in time)
long_tobacco_EHR_sql <- paste("
WITH

LatestSurvey AS (
  SELECT
    answer.person_id,
    MAX(DATE(answer.survey_datetime)) AS latest_survey_date
  FROM `ds_survey` answer
  WHERE question_concept_id IN (
      SELECT DISTINCT concept_id
      FROM `cb_criteria` c
      JOIN (SELECT CAST(cr.id AS STRING) AS id
            FROM `cb_criteria` cr
            WHERE concept_id IN (1585855) AND domain_id = 'SURVEY') a
      ON c.path LIKE CONCAT('%', a.id, '.%')
      WHERE domain_id = 'SURVEY' AND type = 'PPI' AND subtype = 'QUESTION'
  )
  GROUP BY answer.person_id
),

-- 2. observation data (all persons in the EHR; no cohort filter)

TobaccoObservations AS (
  SELECT
    o.person_id,
    o.observation_date AS event_date,
    o.observation_concept_id AS concept_id,
    o.value_as_number,
    o.value_as_concept_id,

    -- Current smoker classification
        CASE
            WHEN
                (
                    o.observation_concept_id IN (903652, 43054909, 903654, 903657, 36305168, 903666, 903655)
                    AND o.value_as_concept_id IN (
                        4276526, 42709996, 4005823, 45877994, 4298794, 37395605, 4246415, 4044775,
                        37017610, 4218741, 762498, 4218917, 36716478, 4209585, 4216174, 762499,
                        4215409, 4044778, 4141787, 4034855, 4190573, 4058137, 4209423, 37203948,
                        4042037, 4144273, 4052029, 4038738, 4052030, 4145798, 4044777, 36716991,
                        437264, 36716473, 4041511, 45884037, 45881517, 45878118, 45884038, 4188539,
                        45884084
                    )
                )
                OR (o.observation_concept_id IN (4005823, 903655, 903658, 903659, 903660, 903661, 903662, 903664, 903668, 903665, 903667, 903663, 903669, 903657))
                OR (o.observation_concept_id IN (3004518, 36303803) AND o.value_as_number > 0)
                -- counseling / NRT observations treated as current-smoking evidence,
                -- consistent with the same concepts in TobaccoProcedure and NicotineCessationEvents
                OR (o.observation_concept_id IN (2514534, 2514535, 2617450, 2617852, 40520042, 44813532,
                                                 4285436, 4300768, 4159975, 3526479, 3526480))
            THEN 1
            ELSE 0
        END AS is_current_flag,

        -- Former smoker classification
        CASE
            WHEN
                (
                    o.observation_concept_id IN (903652, 43054909, 903654, 36305168, 903666, 903651)
                    AND o.value_as_concept_id IN (
                        4310250, 4052032, 45765917, 764567, 4052464, 4092281, 4148416, 4052949,
                        4043059, 4145798, 4141782, 4141783, 4197592, 4141784, 46270534, 45883458,
                        36307819
                    )
                )
                OR o.observation_concept_id  IN (44786668)
            THEN 1
            ELSE 0
        END AS is_former_flag,

        -- Non-smoker classification
        CASE
            WHEN
                (
                    o.observation_concept_id IN (903652, 43054909, 903654, 36305168, 903666, 903651)
                    AND o.value_as_concept_id IN (
                        4144272, 45878245, 45765920, 4222303, 45876662, 4184633, 37017812, 4030580,
                        4227889, 46273081, 4038739, 45879404, 45877986, 764151, 36308879
                    )
                )
                OR o.observation_concept_id IN (903656, 903653, 37017812, 4038739)
            THEN 1
            ELSE 0
        END AS is_non_flag,
    'observation'            AS source,
    CASE
      WHEN o.observation_concept_id IN (2514534, 2514535, 2617450, 2617852, 40520042, 44813532)
        THEN 1  ELSE 0
    END AS cessation_counseling,
    0                        AS medication_use,
    CASE
      WHEN o.observation_concept_id IN (4285436, 4300768, 4159975, 3526479, 3526480)
        THEN 1  ELSE 0
    END AS cessation_attempt

  FROM `observation` o
  WHERE o.observation_concept_id IN (
       36305168, 903654, 4005823, 903653, 903661, 43054909, 903667, 903666,
       903664, 903656, 903651, 37017812, 903657, 903662, 3004518, 903660,
       40770349, 903659, 903655, 903652, 903665, 903668, 36303694, 903663,
       44786668, 903658, 4038739, 903669, 21494887, 36303803,
       -- cessation counseling (Observation domain, from Tobacco_Counseling.csv)
       2514534, 2514535, 2617450, 2617852, 40520042, 44813532,
       -- nicotine replacement therapy (Observation domain, from Nicotine_for_Cessation.csv)
       4285436, 4300768, 4159975, 3526479, 3526480
  )
  AND o.observation_source_concept_id NOT IN (
    1585857, 1586162, 1586163, 903064, 1586173, 1586159,
    1586160, 903063, 1585864, 1585865, 1585866, 1586165,
    1586181, 1585873, 1586157, 1586158, 1585867, 1585860,
    1586189
  )
),


-- 3. procedure_occurrence data for eligible individuals

TobaccoProcedure AS (
  SELECT
    p.person_id,
    p.procedure_date        AS event_date,
    p.procedure_concept_id  AS concept_id,
    NULL                    AS value_as_number,
    NULL                    AS value_as_concept_id,
    1                       AS is_current_flag,
    0                       AS is_former_flag,
    0                       AS is_non_flag,
    'procedure_occurrence'  AS source,
    CASE
      WHEN p.procedure_concept_id IN (
         46272634,3274920,42505701,2617959,1435207,1435208,45542453,45514828,
         40664492,40664586,40664474,40664513,2617449
      ) THEN 1 ELSE 0
    END AS cessation_counseling,
    0                      AS medication_use,
    CASE
      WHEN p.procedure_concept_id IN (
         40313661,44509537,44791437,44509538,44793017,4193015,40565028,
         40560167,40378941
      ) THEN 1 ELSE 0
    END AS cessation_attempt

  FROM `procedure_occurrence` p
  WHERE p.procedure_concept_id IN (
      -- cessation counseling
      46272634, 3274920, 42505701, 2617959, 1435207, 1435208, 45542453, 45514828,
      40664492, 40664586, 40664474, 40664513, 2617449,
      -- cessation attempts
      40313661, 44509537, 44791437, 44509538, 44793017, 4193015, 40565028,
      40560167, 40378941)
),


-- 4. drug_exposure data for eligible individuals

TobaccoDrug AS (
  SELECT
    d.person_id,
    d.drug_exposure_start_date    AS event_date,
    d.drug_concept_id             AS concept_id,
    NULL                          AS value_as_number,
    NULL                          AS value_as_concept_id,
    0                             AS is_current_flag,
    0                             AS is_former_flag,
    0                             AS is_non_flag,
    'drug_exposure'               AS source,
    0                             AS cessation_counseling,
    1                             AS medication_use,
    0                             AS cessation_attempt
  FROM `drug_exposure` d
  WHERE d.drug_concept_id IN (", med_ids, ")
),

-- 5. nicotinecessation data for eligible individuals

NicotineCessationEvents AS (
  SELECT
    d.person_id,
    d.drug_exposure_start_date  AS event_date,
    d.drug_concept_id           AS concept_id,
    NULL                        AS value_as_number,
    NULL                        AS value_as_concept_id,
    1                           AS is_current_flag,
    0                           AS is_former_flag,
    0                           AS is_non_flag,
    'drug_exposure'             AS source,
    0                           AS cessation_counseling,
    0                           AS medication_use,
    1                           AS cessation_attempt
  FROM drug_exposure d
  WHERE d.drug_concept_id IN (", cess_ids, ")
),


-- 6. union all data

AllEvents AS (
  SELECT * FROM TobaccoObservations
  UNION ALL
  SELECT * FROM TobaccoProcedure
  UNION ALL
  SELECT * FROM TobaccoDrug
  UNION ALL
  SELECT * FROM NicotineCessationEvents
)

-- =============================================================================
-- Final long pull: one row per distinct tobacco event for EVERY person in the
-- EHR (no cohort filter). survey_date is attached by a LEFT JOIN to
-- LatestSurvey, so people with a smoking-module survey get their reference date
-- and everyone else gets NULL. Recency is NOT applied here; downstream code
-- computes it against survey_date (using it where present, skipping where NULL).
-- =============================================================================

SELECT DISTINCT
  e.person_id,
  s.latest_survey_date AS survey_date,
  e.event_date,
  e.concept_id,
  e.value_as_number,
  e.value_as_concept_id,
  e.is_current_flag,
  e.is_former_flag,
  e.is_non_flag,
  e.source,
  e.cessation_counseling,
  e.medication_use,
  e.cessation_attempt
FROM AllEvents e
LEFT JOIN LatestSurvey s ON e.person_id = s.person_id
ORDER BY e.person_id, e.event_date
")


# Cohort with EHR data (person-level demographics) and lifestyle survey
EHR_linked_sql <- paste("
    SELECT
        person.person_id,
        person.gender_concept_id,
        p_gender_concept.concept_name as gender,
        person.birth_datetime as date_of_birth,
        person.race_concept_id,
        p_race_concept.concept_name as race,
        person.ethnicity_concept_id,
        p_ethnicity_concept.concept_name as ethnicity,
        person.sex_at_birth_concept_id,
        p_sex_at_birth_concept.concept_name as sex_at_birth
    FROM
        `person` person
    LEFT JOIN
        `concept` p_gender_concept
            ON person.gender_concept_id = p_gender_concept.concept_id
    LEFT JOIN
        `concept` p_race_concept
            ON person.race_concept_id = p_race_concept.concept_id
    LEFT JOIN
        `concept` p_ethnicity_concept
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id
    LEFT JOIN
        `concept` p_sex_at_birth_concept
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id
    WHERE
        person.PERSON_ID IN (SELECT
            distinct person_id
        FROM
            `cb_search_person` cb_search_person
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                age_at_consent BETWEEN 18 AND 124 )
            AND cb_search_person.person_id IN (SELECT
                person_id
            FROM
                `cb_search_person` p
            WHERE
                has_ehr_data = 1 )
            AND cb_search_person.person_id IN (SELECT
                criteria.person_id
            FROM
                (SELECT
                    DISTINCT person_id, entry_date, concept_id
                FROM
                    `cb_search_all_events`
                WHERE
                    (concept_id IN(SELECT
                        DISTINCT c.concept_id
                    FROM
                        `cb_criteria` c
                    JOIN
                        (SELECT
                            CAST(cr.id as string) AS id
                        FROM
                            `cb_criteria` cr
                        WHERE
                            concept_id IN (1585855)
                            AND full_text LIKE '%_rank1]%'      ) a
                            ON (c.path LIKE CONCAT('%.', a.id, '.%')
                            OR c.path LIKE CONCAT('%.', a.id)
                            OR c.path LIKE CONCAT(a.id, '.%')
                            OR c.path = a.id)
                    WHERE
                        is_standard = 0
                        AND is_selectable = 1)
                    AND is_standard = 0 )) criteria ) )", sep="")


# Survey basics
basics_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name
    FROM
        `ds_survey` answer
    WHERE
        (
            question_concept_id IN (SELECT
                DISTINCT concept_id
            FROM
                `cb_criteria` c
            JOIN
                (SELECT
                    CAST(cr.id as string) AS id
                FROM
                    `cb_criteria` cr
                WHERE
                    concept_id IN (1586134)
                    AND domain_id = 'SURVEY') a
                    ON (c.path like CONCAT('%', a.id, '.%'))
            WHERE
                domain_id = 'SURVEY'
                AND type = 'PPI'
                AND subtype = 'QUESTION')
        )", sep="")


# Survey lifestyle
lifestyle_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name
    FROM
        `ds_survey` answer
    WHERE
        (
            question_concept_id IN (SELECT
                DISTINCT concept_id
            FROM
                `cb_criteria` c
            JOIN
                (SELECT
                    CAST(cr.id as string) AS id
                FROM
                    `cb_criteria` cr
                WHERE
                    concept_id IN (1585855)
                    AND domain_id = 'SURVEY') a
                    ON (c.path like CONCAT('%', a.id, '.%'))
            WHERE
                domain_id = 'SURVEY'
                AND type = 'PPI'
                AND subtype = 'QUESTION')
        )", sep="")

# Full person list (person-table spine) so downstream wide can cover EVERY
# person, including those with no tobacco events and no survey. One column.
person_all_sql <- "SELECT person_id FROM `person`"


# -- 3. Execute the queries and export each result to the bucket --------------
# ------------------------------------------------------------------------------
# bq_export() runs each query and writes the result straight to the run's date
# folder as sharded CSV (nothing large is pulled into memory), returning the
# gs:// path. save_manifest() then writes a fresh manifest for this run.

manifest <- tibble(
  name = c("person_all", "visit_cohort", "long_tobacco_ehr_raw", "ehr_linked", "basics", "lifestyle"),
  path = c(
    bq_export(person_all_sql,       "person_all"),
    bq_export(visit_sql,            "visit_cohort"),
    bq_export(long_tobacco_EHR_sql, "long_tobacco_ehr_raw"),
    bq_export(EHR_linked_sql,       "ehr_linked"),
    bq_export(basics_sql,           "basics"),
    bq_export(lifestyle_sql,        "lifestyle")
  ),
  cdr  = AOU_CDR,
  date = RUN_DATE
)

save_manifest(manifest)


# -- 4. Clean up intermediate objects from memory ----------------------------
# ------------------------------------------------------------------------------
rm(nicotine_cessation_concepts_sets, cess_ids, medication_cessation_concepts_sets, med_ids)
rm(person_all_sql, visit_sql, long_tobacco_EHR_sql, EHR_linked_sql, basics_sql, lifestyle_sql)