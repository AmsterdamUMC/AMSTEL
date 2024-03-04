-- INSERT records into SPECIMEN from stem_table
-- WHERE domain_id = 36 (Specimen)
TRUNCATE @cdm_schema.specimen;
INSERT INTO @cdm_schema.specimen
(
    specimen_id,
    person_id,
    specimen_concept_id,
    specimen_type_concept_id,
    specimen_date,
    specimen_datetime,
    quantity,
    unit_concept_id,
    anatomic_site_concept_id,
    disease_status_concept_id,
    specimen_source_id,
    specimen_source_value,
    unit_source_value,
    anatomic_site_source_value,
    disease_status_source_value
)
SELECT
    stem_table.id AS specimen_id,
    stem_table.person_id AS person_id,
    stem_table.concept_id AS specimen_concept_id,
    stem_table.type_concept_id AS specimen_type_concept_id,
    stem_table.start_date AS specimen_date,
    stem_table.start_datetime AS specimen_datetime,
    NULL AS quantity,
    NULL AS unit_concept_id,
    NULL AS anatomic_site_concept_id,
    NULL AS disease_status_concept_id,
    NULL AS specimen_source_id,
    stem_table.source_value AS specimen_source_value,
    NULL AS unit_source_value,
    NULL AS anatomic_site_source_value,
    NULL AS disease_status_source_value

FROM @cdm_schema.stem_table
WHERE stem_table.domain_id = 36 -- Specimen
;
