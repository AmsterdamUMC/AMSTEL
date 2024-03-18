-- inserts numericitems records into stem_table
--
-- Since numericitems is the largest table (almost 1 billion records) any
-- changes should be carefully evaluated to prevent excessive processing time.
-- In PostgreSQL:
INSERT INTO @cdm_schema.stem_table
(
    domain_id,
    --id,
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
    -- Most of the records are either Measurement or Observation.
    -- Default to Observation (27), since this table allows concepts of any type
    -- except those with Concepts in the Condition, Procedure, Drug,
    -- Measurement, or Device domains, that should be inserted into the
    -- corresponding tables
    COALESCE(domain.domain_concept_id, 27) AS domain_id,

    -- id,
    a.patientid AS person_id,

 -- [MAPPING   LOGIC] Missing 'fluid output' concept in CDM VOCABULARIES for ultrafiltration (itemid = 8805 -- CVVH onttrokken) =>concept_id = 3020433 (Fluid Output miscellaneous Measured); LINK to CVVH output:  UPDATE stem_table s SET  event_id = (  SELECT id FROM numericitems n2   WHERE itemid = 20079 --MFT_Filtraatvolume_totaal  AND s.person_id = s2.person_id AND s.start_datetime = s2.start_datetime ), event_field_concept_id = 1147330 --measurement WHERE unit_source_concept_id = 8805 --CVVH ontrokken
    COALESCE(stcm_num.target_concept_id, 0) AS concept_id,

    DATE(a.admissiondatetime + make_interval(secs =>
    (n.measuredat - a.admittedat)/1000)) AS start_date,
    a.admissiondatetime + make_interval(secs =>
    (n.measuredat - a.admittedat)/1000) AS start_datetime,

    NULL AS end_date,
    NULL AS end_datetime,

    CASE
      WHEN n.islabresult = b'1' THEN 32856	-- Lab
      ELSE 32817	-- EHR
    END AS type_concept_id,

    -- Keep most recent provider (updatedby -> registeredby)
    COALESCE(upd_prov.provider_id, reg_prov.provider_id) AS provider_id,

    n.admissionid AS visit_occurrence_id,

    NULL AS visit_detail_id,

    n.item AS source_value,

    NULL AS source_concept_id,

    -- tag: With the exception of '<' and '>' (and 'NUL' or '') the tag denotes
    -- a free text comment result in addition to the numeric value, however
    -- MEASUREMENT does not formally support value_as_string
    -- as a way of storing 'free text' laboratory results. OBSERVATION does have
    -- a value_as_string field.
    --
    -- fluidout: contains fluid output records, however 'fluidout' and 'value'
    -- contain the same values, if fluidout >= 0
    CASE n.tag
      WHEN '-' THEN -n.value
      ELSE n.value
    END AS value_as_number,

    NULL AS value_as_string,
    NULL AS value_as_concept_id,

    -- uses either the manually added unit based on the concept or the default
    -- based on the unit originally specified in the record
    COALESCE(
      stcm_numeric_unit.target_concept_id,
      stcm_unit.target_concept_id
      ) AS unit_concept_id,

    -- comments will not fit varchar(50) limitation:
    -- CONCAT_WS('\ncomment: ', value, comment) AS value_source_value,
    n.value AS value_source_value,

    -- To allow future ETLs to handle conversion to a 'standardized unit',
    -- put the original (mapped) unit here:
    -- https://github.com/OHDSI/CommonDataModel/issues/259
    -- uses either the manually added unit based on the concept or the default
    -- based on the unit originally specified in the record
    COALESCE(
      stcm_numeric_unit.target_concept_id,
      stcm_unit.target_concept_id
      ) AS unit_source_concept_id,
    n.unit AS unit_source_value,

    NULL AS verbatim_end_date,
    NULL AS days_supply,
    NULL AS dose_unit_source_value,

    NULL AS modifier_concept_id,
    NULL AS modifier_source_value,

    TO_CHAR(a.admissiondatetime + make_interval(secs =>
      (n.measuredat - a.admittedat)/1000), 'HH24:MI:SS') AS measurement_time,

    CASE n.tag
      WHEN '<' THEN 4171756	-- <	(less-than)
      WHEN '>' THEN 4172704	-- >	(greater-than)
    END AS operator_concept_id,

    NULL AS quantity,

 -- TO DO [MAPPING COMMENT] from dictionary.csv
    NULL AS range_low,

 -- TO DO [MAPPING COMMENT] from dictionary.csv
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

FROM @ams_schema.numericitems n

LEFT JOIN @cdm_schema.admissions_scalar a ON
    n.admissionid = a.admissionid

LEFT JOIN @cdm_schema.source_to_concept_map stcm_num ON
  stcm_num.source_code = CAST(n.itemid AS VARCHAR) AND
  stcm_num.source_vocabulary_id in ('AUMC Numeric', 'AUMC Laboratory')

LEFT JOIN @cdm_schema.concept c ON
  stcm_num.target_concept_id = c.concept_id AND
  NOT stcm_num.target_concept_id = 0

LEFT JOIN @cdm_schema.domain domain ON
  c.domain_id = domain.domain_id

-- Some concepts lacked a valid unit, that during mapping with Usagi
-- have been added. To force these units in the record, thus ignoring the
-- orignal unit, we will join the current concept with these 'corrected' units
-- and COALESCE the values above.
LEFT JOIN @cdm_schema.source_to_concept_map stcm_numeric_unit ON
  stcm_numeric_unit.source_code = CAST(n.itemid AS VARCHAR) AND
  stcm_numeric_unit.source_vocabulary_id = 'AUMC Numeric Unit'

LEFT JOIN @cdm_schema.source_to_concept_map stcm_unit ON
  stcm_unit.source_code = CAST(n.unitid AS VARCHAR) AND
  stcm_unit.source_vocabulary_id = 'AUMC Unit'

LEFT JOIN @cdm_schema.provider reg_prov ON
  reg_prov.provider_source_value = n.registeredby

LEFT JOIN @cdm_schema.provider upd_prov ON
  upd_prov.provider_source_value = n.updatedby

ORDER BY n.admissionid, n.measuredat, n.itemid
;
