-- DEATH records are part of the AmsterdamUMCdb admissions table, using either
-- the hospital date of death record or based on discharge time on the ICU with
-- destination = 'OVERLEDEN' (deceased)
TRUNCATE @cdm_schema.death;
INSERT INTO @cdm_schema.death
(
    person_id,
    death_date,
    death_datetime,
    death_type_concept_id,
    cause_concept_id,
    cause_source_value,
    cause_source_concept_id
)
SELECT
    a.patientid AS person_id,

    DATE(COALESCE(
      a.admissiondatetime + make_interval(secs =>(a.dateofdeath - a.admittedat)/1000),
      a.admissiondatetime + make_interval(secs =>(a.dischargedat - a.admittedat)/1000)
    )) AS death_date,

    COALESCE(
      a.admissiondatetime + make_interval(secs =>(a.dateofdeath - a.admittedat)/1000),
      a.admissiondatetime + make_interval(secs =>(a.dischargedat - a.admittedat)/1000)
    ) AS death_datetime,

 -- EHR discharge record
    32823 AS death_type_concept_id,

    NULL AS cause_concept_id,

    NULL AS cause_source_value,

    NULL AS cause_source_concept_id

FROM @cdm_schema.admissions_scalar a
WHERE
  (
    NOT a.dateofdeath IS NULL
    OR a.destination = 'Overleden'
  )
  AND a.admissioncount = 1
ORDER BY a.patientid
;
