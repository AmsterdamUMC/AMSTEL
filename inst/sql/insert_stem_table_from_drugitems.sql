-- Create modified drugitems CTE containing values that are only using
-- base units (mg, ml, unit, international unit)
--
-- Blood tranfusions are part of the drugitems table, but in OMOP CDM
-- blood transfusions are considered devices and are given in mL
-- quantities.
-- See: https://ohdsi.github.io/CommonDataModel/cdm54.html#DEVICE_EXPOSURE
-- For consistency, blood transfusions also have a DRUG_EXPOSURE record,
-- in addition to a MEASUREMENT record (for fluid input)
WITH drugitems_staging AS (
  SELECT
    admissionid,
    orderid,
    ordercategoryid,
    ordercategory,
    itemid,
    item,
    isadditive,
    isconditional,
    rate,
    rateunit,
    rateunitid,
    ratetimeunitid,
    doserateperkg,
    dose,
    doseunit,
    doserateunit,
    doseunitid,
    doserateunitid,
    CASE administeredunitid
      WHEN 6 THEN administered --ml (same unit)
      WHEN 9 THEN administered * 1000 --1 g = 1000 mg
      WHEN 10 THEN administered --mg (same unit)
      WHEN 11 THEN administered / 1000 --1 ug = 1/1000 mg
      WHEN 12 THEN administered * 1000000 --1 kg = 1,000,000 mg
      WHEN 178 THEN administered --E (same unit)
      WHEN 180 THEN administered --IE (same unit)
      WHEN 179 THEN administered * 1000000 --10^6 E = 1,000,000 unit
      WHEN 243 THEN administered / 1000 --mIE = 1/1000 unit
      ELSE administered
    END AS administered_converted,
    administered,
    administeredunit,
    administeredunitid,
    CASE administeredunitid
      WHEN 6 THEN 8587 --ml -> milliliter
      WHEN 9 THEN 8576 --g -> milligram
      WHEN 10 THEN 8576 --mg -> milligram
      WHEN 11 THEN 8576 --ug -> milligram
      WHEN 12 THEN 8576 --kg -> milligram
      WHEN 178 THEN 8510 --E -> unit
      WHEN 179 THEN 8510 --10^6 E -> unit
      WHEN 180 THEN 8718 --IE -> international unit
      WHEN 243 THEN 8510 --mIE 0 -> unit
    END AS administeredunit_concept_id,
    action,
    start,
    stop,
    duration,
    solutionitemid,
    solutionitem,
    solutionadministered,
    solutionadministeredunit,
    fluidin,
    iscontinuous
	FROM @ams_schema.drugitems d
),
drug_strength_numbered AS (
  SELECT
    drug_concept_id,
    ingredient_concept_id,
    amount_value,
    amount_unit_concept_id,
    numerator_value,
    numerator_unit_concept_id,
    denominator_value,
    denominator_unit_concept_id,
    box_size,
    valid_start_date,
    valid_end_date,
    invalid_reason,
    ROW_NUMBER() OVER (
          PARTITION BY drug_concept_id
          ORDER BY amount_value DESC, ingredient_concept_id DESC
    ) AS row_number_strength,
    ROW_NUMBER() OVER (
          PARTITION BY drug_concept_id
          ORDER BY amount_value ASC, ingredient_concept_id ASC
    ) AS count
	FROM @cdm_schema.drug_strength
	WHERE drug_concept_id IN
	  (
	    SELECT target_concept_id
      FROM @cdm_schema.source_to_concept_map
      WHERE source_vocabulary_id = 'AUMC Drug'
    )
),
drug_strength_single AS (
  SELECT *
  FROM drug_strength_numbered
  WHERE row_number_strength = 1
),
cte_stcm AS (
	SELECT ROW_NUMBER() OVER(
		PARTITION BY stcm.source_code
		ORDER BY stcm.valid_start_date) AS row_number,
	stcm.source_code,
	stcm.source_concept_id,
	stcm.source_vocabulary_id,
	stcm.source_code_description,
	stcm.target_concept_id,
	stcm.target_vocabulary_id,
	stcm.valid_start_date,
	stcm.valid_end_date,
	stcm.invalid_reason
	FROM @cdm_schema.source_to_concept_map stcm
	WHERE stcm.source_vocabulary_id = 'AUMC Drug'
),
-- retrieves explicitly defined quantities from the SOURCE_TO_CONCEPT_MAP
-- table for non-standard drugs
cte_quantity AS (
	SELECT ROW_NUMBER() OVER(
		PARTITION BY q.source_code
		ORDER BY q.valid_start_date) AS row_number_quantity,
	q.source_code,
	q.source_concept_id,
	q.source_vocabulary_id,
	q.source_code_description,
	q.target_concept_id,
	q.target_vocabulary_id,
	q.valid_start_date,
	q.valid_end_date,
	q.invalid_reason
	FROM @cdm_schema.source_to_concept_map q
	WHERE q.source_vocabulary_id = 'AUMC Drug Quantity'
),
stcm_drug AS (
  SELECT
    cte_stcm.row_number,
  	cte_stcm.source_code,
  	cte_stcm.source_concept_id,
  	cte_stcm.source_vocabulary_id,
  	cte_stcm.source_code_description,
  	cte_stcm.target_concept_id,
  	c.concept_name AS target_concept_name,
  	cte_stcm.target_vocabulary_id,
  	cte_stcm.valid_start_date,
  	cte_stcm.valid_end_date,
  	cte_stcm.invalid_reason,
  	cte_quantity.target_concept_id AS quantity_concept_id,
  	CAST(c_quant.concept_name AS NUMERIC) AS value

  FROM cte_stcm
  LEFT JOIN cte_quantity ON
  	cte_stcm.source_code = cte_quantity.source_code AND
  	cte_stcm.row_number = cte_quantity.row_number_quantity
  LEFT JOIN cdm_54.concept c ON
  	cte_stcm.target_concept_id = c.concept_id
  LEFT JOIN cdm_54.concept c_quant ON
  	cte_quantity.target_concept_id = c_quant.concept_id
)
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
    --Default to Drug for unmappable concepts
    COALESCE(domain.domain_concept_id, 13) AS domain_id,
    --id,
    a.patientid AS person_id,
    COALESCE(stcm_drug.target_concept_id, 0) AS concept_id,

    DATE(a.admissiondatetime + make_interval(secs =>
    (d.start - a.admittedat)/1000)) AS start_date,
    a.admissiondatetime + make_interval(secs =>
    (d.start - a.admittedat)/1000) AS start_datetime,

    DATE(a.admissiondatetime + make_interval(secs =>
    (d.stop - a.admittedat)/1000)) AS end_date,
    a.admissiondatetime + make_interval(secs =>
    (d.stop - a.admittedat)/1000) AS end_datetime,

    32818 AS type_concept_id, --EHR administration record
    NULL AS provider_id,

    d.admissionid AS visit_occurrence_id,
    NULL AS visit_detail_id,

    LEFT(d.item,50) AS source_value,
    NULL AS source_concept_id,

    -- stores fluidin value in value_as_number field to allow creating fluidin
    -- records in MEASUREMENT (or OBSERVATION) as bulk insert
    -- To prevent duplicate fluidin records, thus leading to wrong fluid
    -- balance calculations, items that have been mapped to multiple target
    -- concepts, will only store a fluidin value when stcm_drug.row_number = 1
    CASE stcm_drug.row_number
      WHEN 1 THEN d.fluidin
    END AS value_as_number,

    NULL AS value_as_string,
    NULL AS value_as_concept_id,
    NULL AS unit_concept_id,
    NULL AS value_source_value,
    NULL AS unit_source_concept_id,
    NULL AS unit_source_value,
    NULL AS verbatim_end_date,
    NULL AS days_supply,
    d.administeredunit AS dose_unit_source_value,
    NULL AS modifier_concept_id,
    NULL AS modifier_source_value,
    NULL AS measurement_time,
    NULL AS operator_concept_id,

    -- See https://ohdsi.github.io/CommonDataModel/drug_dose.html.
    -- DRUG_EXPOSURE does not contain unit of quantity, thus analyses requires
    -- using DRUG_STRENGTH

    -- dose form concept_id
    CASE
      -- when explicit quantity has been set (e.g. for Selective Digestive
      -- tract Decontamination) use that value instead of calculating based
      -- on dose form
      WHEN NOT stcm_drug.value is NULL
        THEN stcm_drug.value

      -- fixed dose forms
      WHEN
           r.concept_id_2 = 19082079	-- Extended Release Oral Tablet
        OR r.concept_id_2 = 19082168	-- Oral Capsule
        OR r.concept_id_2 = 19082229	-- Transdermal System
        OR r.concept_id_2 = 19082573	-- Oral Tablet
        OR r.concept_id_2 = 19093368	-- Vaginal Suppository
        THEN
          CASE
            WHEN ds.amount_value > 0
              THEN d.administered_converted / ds.amount_value -- e.g. number of tables/capsules
            ELSE d.administered_converted
          END
        -- liquids
      WHEN
           r.concept_id_2 = 19082103	-- Injectable Solution
        OR r.concept_id_2 = 19082106	-- Intravenous Solution
        OR r.concept_id_2 = 19082165	-- Nasal Solution
        OR r.concept_id_2 = 19082170	-- Oral Solution
        OR r.concept_id_2 = 19082224	-- Topical Cream
        OR r.concept_id_2 = 19082283	-- Rectal Solution
        OR r.concept_id_2 = 19095898	-- Inhalation Solution
        OR r.concept_id_2 = 19095916	-- Oral Gel
        OR r.concept_id_2 = 19095918	-- Oral Paste
        OR r.concept_id_2 = 19095976	-- Oral Powder
        OR r.concept_id_2 = 19129634	-- Ophthalmic Solution
        OR r.concept_id_2 = 19135925	-- Ophthalmic Ointment
        OR r.concept_id_2 = 46234469	-- Injection
        THEN
          CASE
            WHEN ds.count = 1 --single ingredient drug
              THEN
                CASE
                  WHEN d.administeredunit_concept_id = ds.denominator_unit_concept_id  --e.g. milliliter
                    AND NOT ds.denominator_value = 0
                    THEN d.administered_converted * ds.numerator_value / ds.denominator_value
                    --e.g. 500 (milliliter) * 4500 (milligram) / 500 (milliliter) = 4500 (milligram)
                  ELSE
                    d.administered_converted --already in mass units
              END
            ELSE --ingredient in liquid
              d.administered_converted
          END
    ELSE
      -- for blood transfusions and non-medication records
      d.administered_converted
    END as quantity,
    NULL AS range_low,
    NULL AS range_high,
    NULL AS stop_reason,
    NULL AS refills,

    --DRUG_EXPOSURE does not contain the 'concept' of infusion rate so store additional rate data in sig field
    CASE
      WHEN doserateunitid IS NOT NULL THEN
        CASE
          WHEN doserateperkg = B'1' THEN
            CONCAT(administered, ' ', administeredunit, ' @ ',
              CASE
                WHEN rate > 0 THEN CONCAT_WS(' = ', CONCAT_WS('/kg/', CONCAT_WS(' ', dose, doseunit), doserateunit), CONCAT_WS(' ', rate, rateunit))
                ELSE CONCAT_WS('/kg/', CONCAT_WS(' ', dose, doseunit), doserateunit)
              END
            )
          ELSE
            CONCAT(administered, ' ', administeredunit, ' @ ',
              CASE
                WHEN rate > 0 THEN CONCAT_WS(' = ', CONCAT_WS('/', CONCAT_WS(' ', dose, doseunit), doserateunit), CONCAT_WS(' ', rate, rateunit))
                ELSE CONCAT_WS('/', CONCAT_WS(' ', dose, doseunit), doserateunit)
              END
            )
          END
      ELSE CONCAT(dose, ' ', doseunit)
    END AS sig,

    -- no explicit routes have been defined in source,
    -- during mapping the most plausible routes have been added if different
    -- than what would been be derived based on ordercategory(id)
    COALESCE(
      stcm_route_specific.target_concept_id,
      stcm_route_default.target_concept_id
    ) AS route_concept_id,
    LEFT(d.ordercategory,50) AS route_source_value,
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

FROM drugitems_staging d
LEFT JOIN @cdm_schema.admissions_scalar a ON
    d.admissionid = a.admissionid

LEFT JOIN stcm_drug ON
  stcm_drug.source_code = CONCAT(d.itemid, '-', d.ordercategoryid)

LEFT JOIN @cdm_schema.concept c ON
  stcm_drug.target_concept_id = c.concept_id AND
  NOT stcm_drug.target_concept_id = 0

LEFT JOIN @cdm_schema.domain domain ON
  c.domain_id = domain.domain_id

LEFT JOIN @cdm_schema.source_to_concept_map stcm_route_specific ON

  stcm_route_specific.source_code = CONCAT(d.itemid, '-', d.ordercategoryid) AND
  stcm_route_specific.source_vocabulary_id = 'AUMC Drug Route'

LEFT JOIN @cdm_schema.source_to_concept_map stcm_route_default ON

  stcm_route_default.source_code = CONCAT('ordercategory-', d.ordercategoryid) AND
  stcm_route_default.source_vocabulary_id = 'AUMC Route'

LEFT JOIN @cdm_schema.concept_relationship r ON
  stcm_drug.target_concept_id = r.concept_id_1 AND
  r.relationship_id IN (
    'Has dose form',
	  'RxNorm has dose form'
  )

LEFT JOIN drug_strength_single ds ON
  stcm_drug.target_concept_id = ds.drug_concept_id

ORDER BY d.admissionid, d.start, d.itemid, d.orderid
;
