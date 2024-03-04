-- Uses the visit_occurence to determine the observation period for each person
TRUNCATE @cdm_schema.observation_period;
INSERT INTO @cdm_schema.observation_period
(
    observation_period_id,
    person_id,
    observation_period_start_date,
    observation_period_end_date,
    period_type_concept_id
)
SELECT
    ROW_NUMBER() OVER(ORDER BY v.person_id) AS observation_period_id,

    v.person_id AS person_id,

    MIN(v.visit_start_datetime) AS observation_period_start_date,

    MAX(v.visit_end_datetime) AS observation_period_end_date,

    --EHR discharge record
    32823 AS period_type_concept_id

FROM @cdm_schema.visit_occurrence v
GROUP BY v.person_id
;
