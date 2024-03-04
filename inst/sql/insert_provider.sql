TRUNCATE @cdm_schema.provider;
INSERT INTO @cdm_schema.provider
(
    provider_id,
    provider_name,
    npi,
    dea,
    specialty_concept_id,
    care_site_id,
    year_of_birth,
    gender_concept_id,
    provider_source_value,
    specialty_source_value,
    specialty_source_concept_id,
    gender_source_value,
    gender_source_concept_id
)
SELECT
    ROW_NUMBER() OVER(ORDER BY source_vocabulary_id DESC, source_code ASC) AS provider_id,
    NULL AS provider_name,
    NULL AS npi,
    NULL AS dea,
    target_concept_id AS specialty_concept_id,
    NULL AS care_site_id,
    NULL AS year_of_birth,
    NULL AS gender_concept_id,
    source_code AS provider_source_value,
    source_code AS specialty_source_value,
    0 AS specialty_source_concept_id,
    NULL AS gender_source_value,
    NULL AS gender_source_concept_id

FROM @cdm_schema.source_to_concept_map
WHERE
  (
    source_vocabulary_id = 'AUMC Specialty' OR
    source_vocabulary_id = 'AUMC Provider' )
  AND NOT target_concept_id = 0
ORDER BY
  source_vocabulary_id DESC, source_code ASC
;
