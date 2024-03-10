-- inserts freetextitems records into stem_table that
-- Since this table contains laboratory results with free text results,
-- the records should go into the MEASUREMENT table. However, currently
-- OMOP CDM does not support free text (string) values. To allow any meaningful
-- analysis, the most common free text values (e.g. 'Positief', 'Negatief') have
-- been mapped to (categorical) OMOP Standard Concepts.
INSERT INTO @cdm_schema.stem_table
(
    domain_id,
    -- id, -- auto-generated
    person_id,
    concept_id,
    start_date,
    start_datetime,
    end_date,
    end_datetime,
    type_concept_id,
    provider_id,
    visit_occurrence_id,
    visit_detail_id,
    source_value,
    source_concept_id,
    value_as_number,
    value_as_string,
    value_as_concept_id,
    unit_concept_id,
    value_source_value,
    unit_source_concept_id,
    unit_source_value,
    verbatim_end_date,
    days_supply,
    dose_unit_source_value,
    modifier_concept_id,
    modifier_source_value,
    measurement_time,
    operator_concept_id,
    quantity,
    range_low,
    range_high,
    stop_reason,
    refills,
    sig,
    route_concept_id,
    route_source_value,
    lot_number,
    unique_device_id,
    production_id,
    anatomic_site_concept_id,
    disease_status_concept_id,
    specimen_source_id,
    anatomic_site_source_value,
    disease_status_source_value,
    condition_status_concept_id,
    condition_status_source_value,
    qualifier_concept_id,
    qualifier_source_value,
    event_id,
    event_field_concept_id
)
SELECT
    21 AS domain_id, -- Measurement (21)

    -- NULL AS id,

    a.patientid AS person_id,

    COALESCE(stcm_textitem.target_concept_id, 0) AS concept_id,

    DATE(a.admissiondatetime + make_interval(secs =>
    (ft.measuredat - a.admittedat)/1000)) AS start_date,
    a.admissiondatetime + make_interval(secs =>
    (ft.measuredat - a.admittedat)/1000) AS start_datetime,

    NULL AS end_date,
    NULL AS end_datetime,

    32856 AS type_concept_id, -- Lab

    -- Keep most recent provider (updatedby -> registeredby)
    -- With the exception of manual corrections of errors this would be
    -- 'Systeem' -> 38004692 (Clinical Laboratory)
    COALESCE(upd_prov.provider_id, reg_prov.provider_id) AS provider_id,

    ft.admissionid AS visit_occurrence_id,

    NULL AS visit_detail_id,

    LEFT(ft.item, 50) AS source_value,

    NULL AS source_concept_id,

    NULL AS value_as_number,
    NULL AS value_as_string,

    -- Freetextitem values (free text lab results) have been mapped to standard
    -- concepts if possible.
    stcm_textvalue.target_concept_id AS value_as_concept_id,

    NULL AS unit_concept_id,

    LEFT(ft.value, 50) AS value_source_value,

    NULL AS unit_source_concept_id,
    NULL AS unit_source_value,
    NULL AS verbatim_end_date,
    NULL AS days_supply,
    NULL AS dose_unit_source_value,
    NULL AS modifier_concept_id,
    NULL AS modifier_source_value,

    TO_CHAR(a.admissiondatetime + make_interval(secs =>
      (ft.measuredat - a.admittedat)/1000), 'HH24:MI:SS') AS measurement_time,

    NULL AS operator_concept_id,
    NULL AS quantity,
    NULL AS range_low,
    NULL AS range_high,
    NULL AS stop_reason,
    NULL AS refills,
    NULL AS sig,
    NULL AS route_concept_id,
    NULL AS route_source_value,
    NULL AS lot_number,
    NULL AS unique_device_id,
    NULL AS production_id,
    NULL AS anatomic_site_concept_id,
    NULL AS disease_status_concept_id,
    NULL AS specimen_source_id,
    NULL AS anatomic_site_source_value,
    NULL AS disease_status_source_value,
    NULL AS condition_status_concept_id,
    NULL AS condition_status_source_value,
    NULL AS qualifier_concept_id,
    NULL AS qualifier_source_value,
    NULL AS event_id,
    NULL AS event_field_concept_id

FROM @ams_schema.freetextitems ft

LEFT JOIN @cdm_schema.admissions_scalar a ON
    ft.admissionid = a.admissionid

LEFT JOIN @cdm_schema.source_to_concept_map stcm_textitem ON
  stcm_textitem.source_code = CAST(ft.itemid AS VARCHAR) AND
  stcm_textitem.source_vocabulary_id = 'AUMC Text'

LEFT JOIN @cdm_schema.source_to_concept_map stcm_textvalue ON
  stcm_textvalue.source_code = ft.value AND
  stcm_textvalue.source_vocabulary_id = 'AUMC Text Value'

LEFT JOIN @cdm_schema.provider reg_prov ON
  reg_prov.provider_source_value = ft.registeredby

LEFT JOIN @cdm_schema.provider upd_prov ON
  upd_prov.provider_source_value = ft.updatedby

WHERE NOT itemid = 11646 -- exclude specimen source

ORDER BY ft.admissionid, ft.measuredat, ft.itemid

;
