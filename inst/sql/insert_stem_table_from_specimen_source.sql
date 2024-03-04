-- inserts freetextitems and listitem records into stem_table that represent
-- specimens with a specified source. Specifically it combines the records of
-- listitems itemid=7432 ('Afnameplaats') and freetextitems itemid=11646
-- ('Afname (bloed)') for creating SPECIMEN records.
-- This is specifically useful for determining the source of the blood gas
-- analysis.
WITH specimen_sources AS (
(
	  SELECT admissionid, itemid, item, value, CONCAT(itemid, '-', value) AS source_code, measuredat, registeredby, updatedby
	  FROM @ams_schema.freetextitems
	  WHERE itemid = 11646
  )
  UNION ALL
  (
	  SELECT admissionid, itemid, item, value, CONCAT(itemid, '-', valueid) AS source_code, measuredat, registeredby, updatedby
	  FROM @ams_schema.listitems
  	WHERE itemid = 7432
  )
)
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
    domain.domain_concept_id AS domain_id,

    -- NULL AS id,

    a.patientid AS person_id,

    stcm_ss.target_concept_id AS concept_id,

    DATE(a.admissiondatetime + make_interval(secs =>
    (ss.measuredat - a.admittedat)/1000)) AS start_date,
    a.admissiondatetime + make_interval(secs =>
    (ss.measuredat - a.admittedat)/1000) AS start_datetime,

    NULL AS end_date,
    NULL AS end_datetime,

    CASE COALESCE(ss.updatedby, ss.registeredby)
      -- Lab:
      WHEN 'Systeem' THEN 32856

      -- EHR (either as manual entry or correction of received lab result):
      ELSE 32817

    END AS type_concept_id,

    COALESCE(upd_prov.provider_id, reg_prov.provider_id) AS provider_id,

    ss.admissionid AS visit_occurrence_id,

    NULL AS visit_detail_id,

    LEFT(ss.source_code, 50) AS source_value,

    0 AS source_concept_id,

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
    NULL AS event_id,
    NULL AS event_field_concept_id

FROM specimen_sources ss

LEFT JOIN @cdm_schema.admissions_scalar a ON
    ss.admissionid = a.admissionid

LEFT JOIN @cdm_schema.source_to_concept_map stcm_ss ON
  stcm_ss.source_code = ss.source_code AND
  stcm_ss.source_vocabulary_id = 'AUMC Specimen Source'

LEFT JOIN @cdm_schema.concept c ON
  stcm_ss.target_concept_id = c.concept_id AND
  NOT stcm_ss.target_concept_id = 0

LEFT JOIN @cdm_schema.domain domain ON
  c.domain_id = domain.domain_id    -- 'Systeem' -> 38004692 (Clinical Laboratory)

LEFT JOIN @cdm_schema.provider reg_prov ON
  reg_prov.provider_source_value = ss.registeredby

LEFT JOIN @cdm_schema.provider upd_prov ON
  upd_prov.provider_source_value = ss.updatedby

-- Only keep records that contain a valid (useful) specimen source identifier
WHERE domain.domain_concept_id = 36	--Specimen

ORDER BY ss.admissionid, ss.measuredat, ss.itemid
;
