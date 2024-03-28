-- inserts records of listitems into stem_table
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
    -- Most of the records are either Measurement, Observation or
    -- documentation of (nursing) Procedures
    -- Default to Observation (27) for unmapped concepts since many listitems
    -- are categorical observations.
    COALESCE(domain.domain_concept_id, 27) AS domain_id,

    -- id,
    a.patientid AS person_id,

    COALESCE(stcm_list.target_concept_id, 0) AS concept_id,

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

    LEFT(l.item, 50) AS source_value,

    NULL AS source_concept_id,

    CASE stcm_value.target_concept_id
      WHEN 4035566 -- 'Text Value'
        -- remove non-numeric characters (if applicable)
      THEN CAST(TRIM('abcdefghijklmnopqrstuvwxyz' FROM
          REPLACE(
            REPLACE(LOWER(l.value), ' ', ''),'cmh2o',''
          )
        ) AS NUMERIC)
      ELSE NULL
    END AS value_as_number,
    LEFT(l.value, 60) AS value_as_string,

    CASE stcm_value.target_concept_id
      WHEN 4035566 -- 'Text Value'
      THEN NULL -- Do NOT put 'Text Value' concept in value_as_concept_id field
      ELSE stcm_value.target_concept_id
    END AS value_as_concept_id,
    stcm_unit.target_concept_id  AS unit_concept_id,
    LEFT(l.value, 50) AS value_source_value,

    NULL AS unit_source_concept_id,
    NULL AS unit_source_value,
    NULL AS verbatim_end_date,
    NULL AS days_supply,
    NULL AS dose_unit_source_value,

    -- the 'modifier' fields for PROCEDURE_OCCURENCE are very similar to the
    -- 'qualifier' fields OBSERVATION since these fields give additional
    -- information about the concept. The `value` of the listitem record are
    -- used as a modifier for a number of Procedures
    stcm_value.target_concept_id AS modifier_concept_id,
    LEFT(l.value, 50) AS modifier_source_value,

    TO_CHAR(a.admissiondatetime + make_interval(secs =>
      (l.measuredat - a.admittedat)/1000), 'HH24:MI:SS') AS measurement_time,

    NULL AS operator_concept_id,

    -- store quantity for procedures
    CASE stcm_value.target_concept_id
      WHEN 45884119	-- 'Not performed'
        THEN 0
      WHEN 45877935	-- 'Performed'
        THEN 1
      WHEN 4035566 -- 'Text Value'
        -- remove non-numeric characters (if applicable)
      THEN CAST(TRIM('abcdefghijklmnopqrstuvwxyz' FROM
          REPLACE(
            REPLACE(LOWER(l.value), ' ', ''),'cmh2o',''
          )
        ) AS NUMERIC)
      ELSE NULL
    END AS quantity,

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
    stcm_qualifier.target_concept_id AS qualifier_concept_id,
    stcm_qualifier.source_code_description AS qualifier_source_value,
    NULL AS event_id,
    NULL AS event_field_concept_id

FROM @ams_schema.listitems l

LEFT JOIN @cdm_schema.admissions_scalar a ON
    l.admissionid = a.admissionid

LEFT JOIN @cdm_schema.source_to_concept_map stcm_list ON
  stcm_list.source_code = CAST(l.itemid AS VARCHAR) AND
  stcm_list.source_vocabulary_id = 'AUMC List'

LEFT JOIN @cdm_schema.concept c ON
  stcm_list.target_concept_id = c.concept_id AND
  NOT stcm_list.target_concept_id = 0

LEFT JOIN @cdm_schema.domain domain ON
  c.domain_id = domain.domain_id

LEFT JOIN @cdm_schema.source_to_concept_map stcm_value ON
  stcm_value.source_code = CONCAT(l.itemid, '-', l.valueid) AND
  CASE domain.domain_concept_id
    WHEN 21 --measurement
    --Forces the MEASUREMENT to contain only value_as_concept that are
    --port of the 'Meas Value' domain
    THEN stcm_value.source_vocabulary_id = 'AUMC Meas Value'
    -- OBSERVATION is less stringent
    ELSE stcm_value.source_vocabulary_id IN (
      'AUMC Meas Value',
      'AUMC Obs Value',
      'AUMC Diagnosis',
      'AUMC Destination',
      'AUMC Device',
      'AUMC Procedure',
      'AUMC Condition'
    )
  END
  -- prevent unmappable values as concept (i.e. reviewed but no concept available)
  -- from creating '0' values
  AND NOT stcm_value.target_concept_id = 0

LEFT JOIN @cdm_schema.source_to_concept_map stcm_qualifier ON
  stcm_qualifier.source_code = CAST(l.itemid AS VARCHAR) AND
  stcm_qualifier.source_vocabulary_id = 'AUMC Qualifier'

LEFT JOIN @cdm_schema.source_to_concept_map stcm_unit ON
  stcm_unit.source_code = CAST(l.itemid AS VARCHAR) AND
  stcm_unit.source_vocabulary_id = 'AUMC List Unit'

LEFT JOIN @cdm_schema.provider reg_prov ON
  reg_prov.provider_source_value = l.registeredby

LEFT JOIN @cdm_schema.provider upd_prov ON
  upd_prov.provider_source_value = l.updatedby

ORDER BY l.admissionid, l.measuredat, l.itemid
;
