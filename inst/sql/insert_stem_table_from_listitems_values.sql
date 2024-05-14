-- inserts records based on the values of listitems into stem_table
-- Besides containing (concept) values for MEASUREMENT and OBSERVATION,
-- these values also contain concepts relating to DEVICE, PROCEDURE, or
-- CONDITION records
INSERT INTO @cdm_schema.stem_table
(
    domain_id,
    --id, -- auto-generated
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
    domain.domain_concept_id AS domain_id,

    -- id,
    a.patientid AS person_id,

    stcm_value.target_concept_id AS concept_id,

    DATE(a.admissiondatetime + make_interval(secs =>
    (l.measuredat - a.admittedat)/1000)) AS start_date,

    a.admissiondatetime + make_interval(secs =>
    (l.measuredat - a.admittedat)/1000) AS start_datetime,

    NULL AS end_date,
    NULL AS end_datetime,

    32817	AS type_concept_id, -- EHR

    -- Keep most recent provider (updatedby -> registeredby)
    COALESCE(upd_prov.provider_id, reg_prov.provider_id) AS provider_id,

    l.admissionid AS visit_occurrence_id,

    NULL AS visit_detail_id,

    -- OMOP CDM compliant: LEFT(l.value, 50) AS source_value,
    LEFT(l.value, 255) AS source_value,

    NULL AS source_concept_id,
    NULL AS value_as_number,
    NULL AS value_as_string,

    NULL AS value_as_concept_id,
    NULL AS unit_concept_id,
    NULL AS value_source_value,

    NULL AS unit_source_concept_id,
    NULL AS unit_source_value,
    NULL AS verbatim_end_date,
    NULL AS days_supply,
    NULL AS dose_unit_source_value,
    NULL AS modifier_concept_id,
    NULL AS modifier_source_value,

    TO_CHAR(a.admissiondatetime + make_interval(secs =>
      (l.measuredat - a.admittedat)/1000), 'HH24:MI:SS') AS measurement_time,

    NULL AS operator_concept_id,
    stvm_quantity.value as quantity,
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

FROM @ams_schema.listitems l

INNER JOIN @cdm_schema.admissions_scalar a ON
    l.admissionid = a.admissionid

-- INNER JOIN to only insert records that contain mapped objects from the
-- local source vocabularies below
INNER JOIN @cdm_schema.source_to_concept_map stcm_value ON
  stcm_value.source_code = CONCAT(l.itemid, '-', l.valueid) AND
  stcm_value.source_vocabulary_id IN (
      'AUMC Device',
      'AUMC Procedure',
      'AUMC Condition'
  )

INNER JOIN @cdm_schema.concept c ON
  stcm_value.target_concept_id = c.concept_id AND
  NOT stcm_value.target_concept_id = 0

INNER JOIN @cdm_schema.domain domain ON
  c.domain_id = domain.domain_id

LEFT JOIN @cdm_schema.source_to_value_map stvm_quantity ON
  stvm_quantity.source_code = CONCAT(l.itemid, '-', l.valueid) AND
  stvm_quantity.source_vocabulary_id = 'AUMC Quantity'

LEFT JOIN @cdm_schema.provider reg_prov ON
  reg_prov.provider_source_value = l.registeredby

LEFT JOIN @cdm_schema.provider upd_prov ON
  upd_prov.provider_source_value = l.updatedby

ORDER BY l.admissionid, l.measuredat, l.itemid
;
