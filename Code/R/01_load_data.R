# ==============================================================================
# 01_load_data.R
#
# Author: Xuxiang Wang, A. Jerrod Anzalone
# Date: 2025-09-26
#
# Description:
# This script loads all raw data required for the tobacco phenotype validation.
# It reads a local CSV for concept sets and executes multiple BigQuery SQL
# queries to pull EHR and survey data from the AoU CDR.
# ==============================================================================


# -- 1. Read in local concept sets for cessation --
# ------------------------------------------------------------------------------
nicotine_cessation_concepts_sets <- read_csv("Concept Sets/Nicotine_for_Cessation.csv")
cess_ids <- paste(nicotine_cessation_concepts_sets$Id, collapse = ", ")


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


# Main SQL for all tobacco-related EHR events
tobacco_EHR_sql <- paste("
WITH
-- 1. visit_occurrence for Outpatient and Telehealth within ±1 year of survey date

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
    v.*,
    ROW_NUMBER() OVER (PARTITION BY v.person_id ORDER BY v.visit_start_date DESC) AS rn
  FROM `visit_occurrence` v
  LEFT JOIN DescendantConcepts dc ON v.visit_concept_id = dc.descendant_concept_id
  JOIN LatestSurvey s ON v.person_id = s.person_id
  WHERE dc.visit_type IN ('Outpatient_Visit', 'Telehealth_Visit')
    AND v.visit_start_date BETWEEN DATE_SUB(s.latest_survey_date, INTERVAL 1 YEAR)
                              AND DATE_ADD(s.latest_survey_date, INTERVAL 1 YEAR)
),


FinalCohort AS (
  SELECT DISTINCT person_id
  FROM EligibleVisits
),

-- 2. observation data for eligible individuals

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
    0                        AS cessation_counseling,
    0                        AS medication_use,
    CASE
      WHEN o.observation_concept_id IN (4285436, 4300768, 4159975, 3526479, 3526480)
        THEN 1  ELSE 0
    END AS cessation_attempt

  FROM `observation` o
  WHERE o.person_id IN (SELECT person_id FROM FinalCohort)
  AND o.observation_concept_id IN (
       36305168, 903654, 4005823, 903653, 903661, 43054909, 903667, 903666,
       903664, 903656, 903651, 37017812, 903657, 903662, 3004518, 903660,
       40770349, 903659, 903655, 903652, 903665, 903668, 36303694, 903663,
       44786668, 903658, 4038739, 903669, 21494887, 36303803
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
  WHERE p.person_id IN (SELECT person_id FROM FinalCohort)
    AND p.procedure_concept_id IN (
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
  WHERE d.person_id IN (SELECT person_id FROM FinalCohort)
    AND d.drug_concept_id IN (
      36790488, 36777125, 36790487, 36777124, 36777015, 42731747, 44136654,
      44136741, 40702803, 36070575, 36814866, 36953736, 36931568, 21177907,
      36284020, 36897812, 782296, 782242, 21177892, 21178002, 40702799,
      21177951, 21177736, 40702800, 36924825, 35898675, 40702801, 36814865,
      35898674, 40702829, 19127968, 36897550, 36897811, 43298440, 19130125,
      40221856, 45774486, 40221877, 40221879, 40221881, 40221871, 40221874,
      42707259, 19130124, 19128025, 44035184, 44115973, 42966741, 42966742,
      44048318, 44077256, 42966746, 21084548, 43293066, 21104250, 36036362,
      43287638, 42966743, 43820782, 43694855, 43604823, 41285790, 41074867,
      41005057, 41043503, 43640912, 41161586, 41192747, 41200295, 41130224,
      40856553, 43677055, 44048317, 44090159, 44103177, 43265838, 43276890,
      43293067, 43820783, 41130225, 41324629, 41168914, 40822565, 41200294,
      40856552, 40981114, 40856554, 40950020, 40856551, 40825683, 41005060,
      41168913, 41168912, 41168917, 40981113, 41200292, 40856550, 40981112,
      1971776, 41200291, 40918737, 41262091, 43640911, 42966747, 43586695,
      43856891, 44048316, 44103176, 42966750, 42966739, 44112765, 44129143,
      42966745, 44112764, 44129142, 43271327, 43287639, 43287640, 44061295,
      44115972, 44061294, 44115971, 42966744, 42966740, 40911434, 41324626,
      40973681, 41168915, 36788628, 41005058, 41043504, 44188980, 36788626,
      44171651, 36788627, 44099949, 44038408, 44125936, 44077255, 42966748,
      42966749, 36956475, 21104252, 36927304, 21104253, 36788624, 41005059,
      41168916, 41293267, 36788625, 36920790, 21172941, 36062567, 36956585,
      21084550, 36925186, 21025652, 36942368, 21133502, 36920939, 21045267,
      36936028, 21064917, 36952462, 40747738, 21163196, 36938996, 21172942,
      2915941, 36062568, 21045265, 21055075, 21045266, 21055074, 21094301,
      40747739, 21084549, 44038407, 21104251, 43200064, 43156108, 43144913,
      43265839, 43265840, 43255063, 40222060, 1525612, 40222092, 36220348,
      36220349, 36249641, 40221861, 40221863, 702089, 702090, 780442, 702082,
      702086, 702092, 702088, 36889159, 43194149, 43183161, 36883330, 43172191,
      43183160, 780443, 21065806, 19089146, 43150055, 35749766, 43216021,
      43205107, 35754316, 35767622, 35759297, 35755195, 35759296, 43205108,
      43183162, 35767623, 35771973, 35759299, 35759298, 44105322, 780444,
      21154281, 21036370, 21114920, 21144362, 21134442, 21026596, 40736438,
      21055998, 21144363, 21036371, 40882419, 21095256, 21055997, 21105118,
      40736439, 21164062, 21114921, 780447, 44098252, 44087549, 40703337,
      40703339, 40703341, 40703342, 40703338, 40703340, 41075685, 41233727,
      41020567, 40927061, 40833736, 41306968, 41182647, 41069369, 41306967,
      44065360, 44105323, 44070964, 41146160, 44124327, 40995009, 40851153,
      44113430, 780445, 21065807, 19089147, 43194150, 40833735, 35774971,
      36785903, 43139117, 43183163, 43194151, 41026090, 40975657, 35745929,
      36785902, 36785901, 35742608, 35771977, 35755197, 35746791, 43183164,
      41100853, 35750951, 41287787, 43161257, 35771976, 35771975, 43194152,
      40851154, 35755196, 44185422, 40975658, 35771974, 41256750, 40944716,
      35746790, 35771978, 35742609, 35750952, 35746793, 35746792, 44040628,
      780446, 40958327, 21046218, 36277419, 21154282, 21154283, 2914465,
      2914466, 2914464, 2914468, 2914469, 2914467, 21085459, 41244929,
      41100851, 41069368, 41225864, 41132180, 21065809, 36275218, 36260938,
      21055999, 21124528, 40736436, 21026597, 21114923, 21026598, 41256751,
      41225865, 21065810, 41038108, 21095257, 40736435, 21065811, 21173867,
      21124529, 41319211, 21065812, 41132181, 41100854, 41100852, 41007032,
      21095258, 41225863, 21075746, 21075747, 21144364, 21065808, 21075745,
      40736437, 21114922, 21173866, 19124311, 44059623, 44074691, 43183159,
      43161256, 702083, 702084, 702085, 702091, 36220537, 40133797, 21075744,
      40133798, 44030454, 36220538)
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
  WHERE d.person_id IN (SELECT person_id FROM FinalCohort)
    AND d.drug_concept_id IN (", cess_ids, ")
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
),


-- 7. aggregate to get 12‐month flags

DedupEvents AS (
  SELECT
    a.person_id,
    a.event_date,
    a.concept_id,
    a.value_as_number,
    a.value_as_concept_id,
    a.is_current_flag,
    a.is_former_flag,
    a.is_non_flag,
    a.source,
    a.cessation_counseling,
    a.medication_use,
    a.cessation_attempt,
    CASE
      WHEN a.is_current_flag = 1 THEN 3
      WHEN a.is_non_flag = 1 THEN 2
      WHEN a.is_former_flag = 1 THEN 1
      ELSE 0
    END AS status_score,
    ROW_NUMBER() OVER (
      PARTITION BY a.person_id, a.event_date
      ORDER BY
        CASE
          WHEN a.is_current_flag = 1 THEN 3
          WHEN a.is_non_flag = 1 THEN 2
          WHEN a.is_former_flag = 1 THEN 1
          ELSE 0
        END DESC
    ) AS rn
  FROM AllEvents a
),
OneEventPerDay AS (
  SELECT
    person_id,
    event_date,
    concept_id,
    value_as_number,
    value_as_concept_id,
    is_current_flag,
    is_former_flag,
    is_non_flag,
    source,
    cessation_counseling,
    medication_use,
    cessation_attempt
  FROM DedupEvents
  WHERE rn = 1
),

AllEventsWithFlags AS (
  SELECT
    a.person_id,
    a.event_date,
    a.concept_id,
    a.value_as_number,
    a.value_as_concept_id,
    a.is_current_flag,
    a.is_former_flag,
    a.is_non_flag,
    a.source,
    a.cessation_counseling,
    a.medication_use,
    a.cessation_attempt,

    -- “current_12_month” = 1 if ANY is_current_flag = 1 in the 12‐month window [event_date−12mo, event_date]
    MAX(IF(
      evt.is_current_flag=1
      AND evt.event_date BETWEEN DATE_SUB(a.event_date, INTERVAL 12 MONTH) AND a.event_date,
      1, 0
    )) AS current_12_month,

    -- if any same‐person event older than 12mo has is_current_flag=1
    MAX(IF(
      evt.is_current_flag=1
      AND evt.event_date < DATE_SUB(a.event_date, INTERVAL 12 MONTH),
      1, 0
    )) AS prior_smoker

  FROM AllEvents a
  LEFT JOIN AllEvents evt
    ON a.person_id = evt.person_id

  GROUP BY
    a.person_id,
    a.event_date,
    a.concept_id,
    a.value_as_number,
    a.value_as_concept_id,
    a.is_current_flag,
    a.is_former_flag,
    a.is_non_flag,
    a.source,
    a.cessation_counseling,
    a.medication_use,
    a.cessation_attempt
)")

long_tobacco_EHR_sql <- paste(tobacco_EHR_sql, "

-- =============================================================================
-- final SQL for long data
-- =============================================================================

SELECT
  person_id,
  event_date,
  concept_id,
  value_as_number,
  value_as_concept_id,
  is_current_flag,
  is_former_flag,
  is_non_flag,
  source,
  cessation_counseling,
  medication_use,
  cessation_attempt,
  current_12_month,
  CASE WHEN current_12_month = 0 AND prior_smoker = 1 THEN 1 ELSE 0 END AS former_12_month
FROM AllEventsWithFlags
ORDER BY person_id, event_date
")

wide_tobacco_EHR_sql <- paste(tobacco_EHR_sql, ",
-- =============================================================================
-- final SQL for wide data
-- =============================================================================

EverFlags AS (
  SELECT
    person_id,
    MAX(is_current_flag)      AS current_flag,
    MAX(is_former_flag)       AS former_flag,
    MAX(is_non_flag)          AS non_flag,
    MAX(cessation_counseling) AS cessation_counseling,
    MAX(medication_use)       AS medication_use,
    MAX(cessation_attempt)    AS cessation_attempt
  FROM AllEventsWithFlags
  GROUP BY person_id
),
LatestEvent AS (
  SELECT
    *
  FROM (
    SELECT *,
      ROW_NUMBER() OVER (PARTITION BY person_id ORDER BY event_date DESC) AS rn
    FROM AllEventsWithFlags
  ) sub
  WHERE rn = 1
)

SELECT
  e.person_id,

  CASE WHEN e.current_flag = 1 OR e.former_flag = 1 THEN 1 ELSE 0 END AS ever_smoker,
  e.non_flag,
  e.cessation_counseling,
  e.medication_use,
  e.cessation_attempt,

  -- Most recent event
  l.event_date           AS latest_event_date,
  l.is_current_flag      AS latest_is_current_flag,
  l.is_former_flag       AS latest_is_former_flag,
  l.is_non_flag          AS latest_is_non_flag,
  l.current_12_month     AS latest_current_12_month,
  CASE WHEN l.current_12_month = 0 AND l.prior_smoker = 1 THEN 1 ELSE 0 END AS latest_former_12_month,
  l.cessation_counseling AS latest_cessation_counseling,
  l.medication_use       AS latest_medication_use,
  l.cessation_attempt    AS latest_cessation_attempt,
  l.value_as_number      AS latest_value_as_number,
  l.value_as_concept_id  AS latest_value_as_concept_id,
  l.concept_id           AS latest_concept_id,
  l.source               AS latest_source

FROM EverFlags e
LEFT JOIN LatestEvent l
  ON e.person_id = l.person_id
ORDER BY e.person_id
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

visit_cohort <- retrieve_data(visit_sql)
long_tobacco_EHR_data <- retrieve_data(long_tobacco_EHR_sql)
wide_tobacco_EHR_data <- retrieve_data(wide_tobacco_EHR_sql)
EHR_linked_data <- retrieve_data(EHR_linked_sql)
basics_data <- retrieve_data(basics_sql)
lifestyle_data <- retrieve_data(lifestyle_sql)

# -- 3. Clean up intermediate objects from memory --
# ------------------------------------------------------------------------------
rm(nicotine_cessation_concepts_sets, cess_ids)
rm(visit_sql, tobacco_EHR_sql, long_tobacco_EHR_sql, wide_tobacco_EHR_sql, EHR_linked_sql, basics_sql, lifestyle_sql)