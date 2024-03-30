TRUNCATE @cdm_schema.visit_occurrence;
INSERT INTO @cdm_schema.visit_occurrence
(
    visit_occurrence_id,
    person_id,
    visit_concept_id,
    visit_start_date,
    visit_start_datetime,
    visit_end_date,
    visit_end_datetime,
    visit_type_concept_id,
    provider_id,
    care_site_id,
    visit_source_value,
    visit_source_concept_id,
    admitted_from_concept_id,
    admitted_from_source_value,
    discharged_to_concept_id,
    discharged_to_source_value,
    preceding_visit_occurrence_id
)
SELECT
    a.admissionid AS visit_occurrence_id,

    a.patientid AS person_id,

    --Intensive Care
    32037 AS visit_concept_id,

    DATE(a.admissiondatetime) as visit_start_date,

    a.admissiondatetime AS visit_start_datetime,

    DATE(a.admissiondatetime + make_interval(secs =>
    (a.dischargedat - a.admittedat)/1000)) AS visit_end_date,

    a.admissiondatetime + make_interval(secs =>
    (a.dischargedat - a.admittedat)/1000) AS visit_end_datetime,

    --EHR encounter record
    32827 AS visit_type_concept_id,

    --the admitting medical specialty
    p.provider_id AS provider_id,

    CASE a.location
      WHEN 'MC' THEN 2
      ELSE 1
    END
    AS care_site_id,

    a.location AS visit_source_value,

    NULL AS visit_source_concept_id,

    stcm_origin.target_concept_id AS admitted_from_concept_id,

    LEFT(a.origin,50) AS admitted_from_source_value,

    stcm_destination.target_concept_id AS discharged_to_concept_id,

    a.destination AS discharged_to_source_value,

    LAG(a.admissionid) OVER(PARTITION BY a.patientid ORDER BY a.admittedat)
    AS preceding_visit_occurrence_id

FROM @cdm_schema.admissions_scalar a
LEFT JOIN @cdm_schema.source_to_concept_map stcm_origin ON

  --uses origin field from admission table mappings
  stcm_origin.source_code = LEFT(a.origin,50) AND
  stcm_origin.source_vocabulary_id = 'AUMC Origin'

LEFT JOIN @cdm_schema.source_to_concept_map stcm_destination ON
  --uses listitems value mappings
  stcm_destination.source_code = CONCAT('10472-', a.destination) AND
  stcm_destination.source_vocabulary_id = 'AUMC Destination'

LEFT JOIN @cdm_schema.provider p ON
  a.specialty = p.provider_source_value
;
