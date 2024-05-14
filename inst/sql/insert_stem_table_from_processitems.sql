-- inserts records of listitems into stem_table. These records are either
-- DEVICE_EXPOSURE or PROCEDURE records
-- Since lines/drains have an anatomical site that currently is not
-- encoded in the concept, an additional record will be created in the
-- OBSERVATION table.
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
    -- Default to 17 (Device) if unmappable
    COALESCE(domain.domain_concept_id, 17) AS domain_id,

    -- id,
    a.patientid AS person_id,

    COALESCE(stcm_proc.target_concept_id, 0) AS concept_id,

    DATE(a.admissiondatetime + make_interval(secs =>
    (p.start - a.admittedat)/1000)) AS start_date,
    a.admissiondatetime + make_interval(secs =>
    (p.start - a.admittedat)/1000) AS start_datetime,

    DATE(a.admissiondatetime + make_interval(secs =>
    (p.stop - a.admittedat)/1000)) AS end_date,
    a.admissiondatetime + make_interval(secs =>
    (p.stop - a.admittedat)/1000) AS end_datetime,

    32817	AS type_concept_id, -- EHR

    NULL AS provider_id,

    p.admissionid AS visit_occurrence_id,

    NULL AS visit_detail_id,

    -- OMOP CDM compliant: LEFT(p.item, 50) AS source_value,
    LEFT(p.item, 255) AS source_value,

    NULL AS source_concept_id,
    NULL AS value_as_number,
    NULL AS value_as_string,

    -- stores the anatomic location in the value_as_concept_id field.
    -- this will be used to create separate OBSERVATION records, since
    -- DEVICE_EXPOSURE does not have a property field for Anatomic site.
    stcm_site.target_concept_id AS value_as_concept_id,

    NULL AS unit_concept_id,
    NULL AS value_source_value,

    NULL AS unit_source_concept_id,
    NULL AS unit_source_value,
    NULL AS verbatim_end_date,
    NULL AS days_supply,
    NULL AS dose_unit_source_value,
    NULL AS modifier_concept_id,
    NULL AS modifier_source_value,
    NULL AS measurement_time,
    NULL AS operator_concept_id,

    -- number of catheter lumina
    stvm_quantity.value AS quantity,

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

FROM @ams_schema.processitems p

LEFT JOIN @cdm_schema.admissions_scalar a ON
    p.admissionid = a.admissionid

LEFT JOIN @cdm_schema.source_to_concept_map stcm_proc ON
  stcm_proc.source_code = CAST(p.itemid AS VARCHAR) AND
  stcm_proc.source_vocabulary_id = 'AUMC Process'

LEFT JOIN @cdm_schema.concept c ON
  stcm_proc.target_concept_id = c.concept_id AND
  NOT stcm_proc.target_concept_id = 0

LEFT JOIN @cdm_schema.domain domain ON
  c.domain_id = domain.domain_id

LEFT JOIN @cdm_schema.source_to_concept_map stcm_site ON
  stcm_site.source_code = CAST(p.itemid AS VARCHAR) AND
  stcm_site.source_vocabulary_id = 'AUMC Anatomic Site'

LEFT JOIN @cdm_schema.source_to_value_map stvm_quantity ON
  stvm_quantity.source_code = CAST(p.itemid AS VARCHAR) AND
  stvm_quantity.source_vocabulary_id = 'AUMC Quantity'

ORDER BY p.admissionid, p.start, p.itemid, p.stop
;

-- create OBSERVATION records containing the anatomic site of the
-- device in DEVICE_EXPOSURE. To easily and efficiently link the records to
-- allow bulk inserts, the anatomic site has been stored in the
-- values_as_concept field.
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
    27 AS domain_id, -- Observation
    -- id,
    s.person_id,

    42539300 AS concept_id, --Anatomic location (property)

    s.start_date,
    s.start_datetime,

    NULL AS end_date,
    NULL AS end_datetime,

    32817	AS type_concept_id, -- EHR

    NULL AS provider_id,
    s.visit_occurrence_id,
    NULL AS visit_detail_id,

    -- OMOP CDM compliant: LEFT(CONCAT('Anatomic location: ', s.source_value), 50) AS source_value,
    LEFT(CONCAT('Anatomic location: ', s.source_value), 255) AS source_value,

    0 AS source_concept_id,
    NULL AS value_as_number,
    NULL AS value_as_string,

    s.value_as_concept_id AS value_as_concept_id,

    NULL AS unit_concept_id,
    NULL AS value_source_value,
    NULL AS unit_source_concept_id,
    NULL AS unit_source_value,
    NULL AS verbatim_end_date,
    NULL AS days_supply,
    NULL AS dose_unit_source_value,
    NULL AS modifier_concept_id,
    NULL AS modifier_source_value,
    NULL AS measurement_time,
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

    -- linking DEVICE_EXPOSURE -> OBSERVATION
    s.id AS event_id,

    CASE s.domain_id
      WHEN 17	THEN 1147115 -- device_exposure.device_exposure_id
      WHEN 10	THEN 1147082 -- procedure_occurrence.procedure_occurrence_id
    END AS event_field_concept_id

FROM @cdm_schema.stem_table s
WHERE
  s.concept_id IN (
    SELECT target_concept_id
    FROM @cdm_schema.source_to_concept_map
    WHERE source_vocabulary_id = 'AUMC Process'
  )
  AND s.value_as_concept_id IN (
    SELECT target_concept_id
    FROM @cdm_schema.source_to_concept_map
    WHERE source_vocabulary_id = 'AUMC Anatomic Site'
  )
ORDER BY s.id
;


