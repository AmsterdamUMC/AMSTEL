-- For every DRUG_EXPOSURE record with fluidin > 0, a MEASUREMENT record will
-- be created to allow calculating fluid input as part of the fluid balance.
--
-- The current implementation creates MEASUREMENT records. However, an
-- alternative approach could just as well create OBSERVATION records, since
-- the distinction between concepts belonging to the two tables have not
-- been well defined.
---
-- Blood tranfusions are part of the drugitems table, but in OMOP CDM
-- blood transfusions are considered devices and are given in mL
-- quantities.
-- See: https://ohdsi.github.io/CommonDataModel/cdm54.html#DEVICE_EXPOSURE
-- For consistency, blood transfusions also have a DRUG_EXPOSURE record,
-- in addition to a MEASUREMENT record (for fluid input).

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
    21 AS domain_id, -- Measurement
    --id,
    person_id,

    -- determines type of fluid intake based on medication route
    CASE s.route_concept_id
      WHEN 4167540 --Enteral route
        THEN  3010494 --Fluid intake enteral tube Measured
      WHEN 4171047 --Intravenous route
        THEN 3037253  --Fluid intake intravascular Measured
      WHEN 4170113 --Intravenous central route
        THEN 3037253  --Fluid intake intravascular Measured
      ELSE 3013308 --Fluid intake Measured
    END AS concept_id,

    start_date,
    start_datetime,

    NULL AS end_date,
    NULL AS end_datetime,

    32818 AS type_concept_id, --EHR administration record
    NULL AS provider_id,

    visit_occurrence_id,
    NULL AS visit_detail_id,

    LEFT(CONCAT('Fluid In: ', s.source_value), 50) AS source_value,
    NULL AS source_concept_id,

    -- stem_table for drug_exposure records (temporarily) stores fluidin in
    -- value_as_number field
    s.value_as_number AS value_as_number,

    NULL AS value_as_string,
    NULL AS value_as_concept_id,
    8587 AS unit_concept_id, --milliliter
    s.value_as_number AS value_source_value,
    NULL AS unit_source_concept_id,
    'ml' AS unit_source_value,
    NULL AS verbatim_end_date,
    NULL AS days_supply,
    NULL AS dose_unit_source_value,
    NULL AS modifier_concept_id,
    NULL AS modifier_source_value,
    NULL AS measurement_time,
    NULL AS operator_concept_id,
    NULL as quantity,
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

    -- linking DRUG_EXPOSURE -> MEASUREMENT
    s.id AS event_id,
    1147094 AS event_field_id_concept_id -- drug_exposure.drug_exposure_id

FROM @cdm_schema.stem_table s
WHERE
  s.concept_id IN (
    SELECT target_concept_id
    FROM @cdm_schema.source_to_concept_map
    WHERE source_vocabulary_id = 'AUMC Drug'
  )
  AND NOT s.value_as_number IS NULL
ORDER BY s.id
;
