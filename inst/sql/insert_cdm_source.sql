TRUNCATE @cdm_schema.cdm_source;
INSERT INTO @cdm_schema.cdm_source
(
    cdm_source_name,
    cdm_source_abbreviation,
    cdm_holder,
    source_description,
    source_documentation_reference,
    cdm_etl_reference,
    source_release_date,
    cdm_release_date,
    cdm_version,
    cdm_version_concept_id,
    vocabulary_version
)
SELECT
    '@cdm_source_name',
    '@cdm_source_abbreviation',
    '@cdm_holder',
    '@source_description',
    '@source_documentation_reference',
    '@cdm_etl_reference',
    CAST( '@source_release_date' AS DATE),
    CURRENT_DATE,
    '@cdm_version', --without 'CDM'
    756265, --CDM 5.4 from SELECT * FROM @cdm_schema.concept WHERE vocabulary_id = 'CDM' AND concept_class_id = 'CDM'
    vocabulary_version
    FROM @cdm_schema.vocabulary
    WHERE vocabulary_id = 'None';
