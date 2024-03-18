-- Reason for ICU admission
-- Combines multiple sources in listitems into stem_table.
-- On the ICU reason for admission is a combination of conditions and
-- admissions following surgery. Please note that during the stay the admission
-- reason might be corrected or modified.
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
    -- All reasons for admissions lisitems are of domain Condition
    19 AS domain_id,

    -- id,
    a.patientid AS person_id,

    COALESCE(stcm_diag.target_concept_id, 0) AS concept_id,

    -- Since the reason for admission is a Condition that applies to the
    -- entire ICU stay, but was often recorded near or after the end of the
    -- ICU stay, for clarity, we set the timestamp of these conditions
    -- to the start of the ICU stay
    DATE(a.admissiondatetime) AS start_date,
    a.admissiondatetime AS start_datetime,

    NULL AS end_date,
    NULL AS end_datetime,

    32817	AS type_concept_id, -- EHR

    -- Keep most recent provider (updatedby -> registeredby)
    COALESCE(upd_prov.provider_id, reg_prov.provider_id) AS provider_id,

    l.admissionid AS visit_occurrence_id,

    NULL AS visit_detail_id,

    LEFT(l.value, 50) AS source_value,

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

    TO_CHAR(a.admissiondatetime, 'HH24:MI:SS') AS measurement_time,

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

    CASE
      WHEN itemid IN (
        18611, -- SEC_Apache II Operatief  Gastr-intenstinaal
        18612, -- SEC_Apache II Operatief Cardiovasculair
        18613, -- SEC_Apache II Operatief Hematologisch
        18614, -- SEC_Apache II Operatief Metabolisme
        18615, -- SEC_Apache II Operatief Neurologisch
        18616, -- SEC_Apache II Operatief Renaal
        18617, -- SEC_Apache II Operatief Respiratoir

        17029, -- SEC_APACHEIV Post-operative cardiovascular
        17030, -- SEC_APACHEIV Post-operative gastro-intestinal
        17031, -- SEC_APACHEIV Post-operative genitourinary
        17032, -- SEC_APACHEIV Post-operative hematology
        17033, -- SEC_APACHEIV Post-operative metabolic
        17034, -- SEC_APACHEIV Post-operative musculoskeletal /skin
        17035, -- SEC_APACHEIV Post-operative neurologic
        17036, -- SEC_APACHEIV Post-operative respiratory
        17037, -- SEC_APACHEIV Post-operative transplant
        17038, -- SEC_APACHEIV Post-operative trauma


        18604, -- SEC_Apache II Non-Operatief Cardiovasculair
        18605, -- SEC_Apache II Non-Operatief Gastro-intestinaal
        18606, -- SEC_Apache II Non-Operatief Hematologisch
        18607, -- SEC_Apache II Non-Operatief Metabolisme
        18608, -- SEC_Apache II Non-Operatief Neurologisch
        18609, -- SEC_Apache II Non-Operatief Renaal
        18610, -- SEC_Apache II Non-Operatief Respiratoir


        17019, -- SEC_APACHE IV Non-operative cardiovascular
        17020, -- SEC_APACHE IV Non-operative Gastro-intestinal
        17021, -- SEC_APACHE IV Non-operative genitourinary
        17022, -- SEC_APACHEIV  Non-operative haematological
        17023, -- SEC_APACHEIV  Non-operative metabolic
        17024, -- SEC_APACHEIV Non-operative musculo-skeletal
        17025, -- SEC_APACHEIV Non-operative neurologic
        17026, -- SEC_APACHEIV Non-operative respiratory
        17027, -- SEC_APACHEIV Non-operative transplant
        17028, -- SEC_APACHEIV Non-operative trauma

        18670, -- NICE SEC APACHEII diagnosen
        18673 -- NICE SEC APACHEIV diagnosen
      )
      THEN 32907 -- Secondary admission diagnosis
      ELSE 32901	--Primary admission diagnosis
    END AS condition_status_concept_id,

    LEFT(l.item, 50) AS condition_status_source_value,

    NULL AS qualifier_concept_id,
    NULL AS qualifier_source_value,
    NULL AS event_id,
    NULL AS event_field_concept_id

FROM @ams_schema.listitems l

LEFT JOIN @cdm_schema.admissions_scalar a ON
    l.admissionid = a.admissionid

-- INNER JOIN to only include mapped concepts (conditions)
INNER JOIN @cdm_schema.source_to_concept_map stcm_diag ON
  stcm_diag.source_code = CONCAT(l.itemid, '-', l.valueid) AND
  stcm_diag.source_vocabulary_id = 'AUMC Diagnosis'

INNER JOIN @cdm_schema.concept c ON
  stcm_diag.target_concept_id = c.concept_id AND
  NOT stcm_diag.target_concept_id = 0

LEFT JOIN @cdm_schema.provider reg_prov ON
  reg_prov.provider_source_value = l.registeredby

LEFT JOIN @cdm_schema.provider upd_prov ON
  upd_prov.provider_source_value = l.updatedby

WHERE
  l.itemid IN (
    -- Diagnosis
    -- SURGICAL
    13116, -- D_Thoraxchirurgie_CABG en Klepchirurgie
    16671, -- DMC_Thoraxchirurgie_CABG en Klepchirurgie
    13117, -- D_Thoraxchirurgie_Cardio anders
    16672, -- DMC_Thoraxchirurgie_Cardio anders
    13118, -- D_Thoraxchirurgie_Aorta chirurgie
    16670, -- DMC_Thoraxchirurgie_Aorta chirurgie
    13119, -- D_Thoraxchirurgie_Pulmonale chirurgie
    16673, -- DMC_Thoraxchirurgie_Pulmonale chirurgie

    13141, -- D_Algemene chirurgie_Algemeen
    16642, -- DMC_Algemene chirurgie_Algemeen
    13121, -- D_Algemene chirurgie_Buikchirurgie
    16643, -- DMC_Algemene chirurgie_Buikchirurgie
    13123, -- D_Algemene chirurgie_Endocrinologische chirurgie
    16644, -- DMC_Algemene chirurgie_Endocrinologische chirurgie
    13145, -- D_Algemene chirurgie_KNO/Overige
    16645, -- DMC_Algemene chirurgie_KNO/Overige
    13125, -- D_Algemene chirurgie_Orthopedische chirurgie
    16646, -- DMC_Algemene chirurgie_Orthopedische chirurgie
    13122, -- D_Algemene chirurgie_Transplantatie chirurgie
    16647, -- DMC_Algemene chirurgie_Transplantatie chirurgie
    13124, -- D_Algemene chirurgie_Trauma
    16648, -- DMC_Algemene chirurgie_Trauma
    13126, -- D_Algemene chirurgie_Urogenitaal
    16649, -- DMC_Algemene chirurgie_Urogenitaal
    13120, -- D_Algemene chirurgie_Vaatchirurgie
    16650, -- DMC_Algemene chirurgie_Vaatchirurgie

    13128, -- D_Neurochirurgie _Vasculair chirurgisch
    16661, -- DMC_Neurochirurgie _Vasculair chirurgisch
    13129, -- D_Neurochirurgie _Tumor chirurgie
    16660, -- DMC_Neurochirurgie _Tumor chirurgie
    13130, -- D_Neurochirurgie_Overige
    16662, -- DMC_Neurochirurgie_Overige

    18596, -- Apache II Operatief  Gastr-intenstinaal
    18597, -- Apache II Operatief Cardiovasculair
    18598, -- Apache II Operatief Hematologisch
    18599, -- Apache II Operatief Metabolisme
    18600, -- Apache II Operatief Neurologisch
    18601, -- Apache II Operatief Renaal
    18602, -- Apache II Operatief Respiratoir

    18611, -- SEC_Apache II Operatief  Gastr-intenstinaal
    18612, -- SEC_Apache II Operatief Cardiovasculair
    18613, -- SEC_Apache II Operatief Hematologisch
    18614, -- SEC_Apache II Operatief Metabolisme
    18615, -- SEC_Apache II Operatief Neurologisch
    18616, -- SEC_Apache II Operatief Renaal
    18617, -- SEC_Apache II Operatief Respiratoir

    17008, -- APACHEIV Post-operative cardiovascular
    17009, -- APACHEIV Post-operative gastro-intestinal
    17010, -- APACHEIV Post-operative genitourinary
    17011, -- APACHEIV Post-operative hematology
    17012, -- APACHEIV Post-operative metabolic
    17013, -- APACHEIV Post-operative musculoskeletal /skin
    17014, -- APACHEIV Post-operative neurologic
    17015, -- APACHEIV Post-operative respiratory
    17016, -- APACHEIV Post-operative transplant
    17017, -- APACHEIV Post-operative trauma

    17029, -- SEC_APACHEIV Post-operative cardiovascular
    17030, -- SEC_APACHEIV Post-operative gastro-intestinal
    17031, -- SEC_APACHEIV Post-operative genitourinary
    17032, -- SEC_APACHEIV Post-operative hematology
    17033, -- SEC_APACHEIV Post-operative metabolic
    17034, -- SEC_APACHEIV Post-operative musculoskeletal /skin
    17035, -- SEC_APACHEIV Post-operative neurologic
    17036, -- SEC_APACHEIV Post-operative respiratory
    17037, -- SEC_APACHEIV Post-operative transplant
    17038, -- SEC_APACHEIV Post-operative trauma

    -- MEDICAL
    13133, -- D_Interne Geneeskunde_Cardiovasculair
    16653, -- DMC_Interne Geneeskunde_Cardiovasculair
    13134, -- D_Interne Geneeskunde_Pulmonaal
    16658, -- DMC_Interne Geneeskunde_Pulmonaal
    13135, -- D_Interne Geneeskunde_Abdominaal
    16652, -- DMC_Interne Geneeskunde_Abdominaal
    13136, -- D_Interne Geneeskunde_Infectieziekten
    16655, -- DMC_Interne Geneeskunde_Infectieziekten
    13137, -- D_Interne Geneeskunde_Metabool
    16656, -- DMC_Interne Geneeskunde_Metabool
    13138, -- D_Interne Geneeskunde_Renaal
    16659, -- DMC_Interne Geneeskunde_Renaal
    13139, -- D_Interne Geneeskunde_Hematologisch
    16654, -- DMC_Interne Geneeskunde_Hematologisch
    13140, -- D_Interne Geneeskunde_Overige
    16657, -- DMC_Interne Geneeskunde_Overige

    13131, -- D_Neurologie_Vasculair neurologisch
    16664, -- DMC_Neurologie_Vasculair neurologisch
    13132, -- D_Neurologie_Overige
    16663, -- DMC_Neurologie_Overige
    13127, -- D_KNO/Overige

    18589, -- Apache II Non-Operatief Cardiovasculair
    18590, -- Apache II Non-Operatief Gastro-intestinaal
    18591, -- Apache II Non-Operatief Hematologisch
    18592, -- Apache II Non-Operatief Metabolisme
    18593, -- Apache II Non-Operatief Neurologisch
    18594, -- Apache II Non-Operatief Renaal
    18595, -- Apache II Non-Operatief Respiratoir

    18604, -- SEC_Apache II Non-Operatief Cardiovasculair
    18605, -- SEC_Apache II Non-Operatief Gastro-intestinaal
    18606, -- SEC_Apache II Non-Operatief Hematologisch
    18607, -- SEC_Apache II Non-Operatief Metabolisme
    18608, -- SEC_Apache II Non-Operatief Neurologisch
    18609, -- SEC_Apache II Non-Operatief Renaal
    18610, -- SEC_Apache II Non-Operatief Respiratoir

    16998, -- APACHE IV Non-operative cardiovascular
    16999, -- APACHE IV Non-operative Gastro-intestinal
    17000, -- APACHE IV Non-operative genitourinary
    17001, -- APACHEIV  Non-operative haematological
    17002, -- APACHEIV  Non-operative metabolic
    17003, -- APACHEIV Non-operative musculo-skeletal
    17004, -- APACHEIV Non-operative neurologic
    17005, -- APACHEIV Non-operative respiratory
    17006, -- APACHEIV Non-operative transplant
    17007, -- APACHEIV Non-operative trauma

    17019, -- SEC_APACHE IV Non-operative cardiovascular
    17020, -- SEC_APACHE IV Non-operative Gastro-intestinal
    17021, -- SEC_APACHE IV Non-operative genitourinary
    17022, -- SEC_APACHEIV  Non-operative haematological
    17023, -- SEC_APACHEIV  Non-operative metabolic
    17024, -- SEC_APACHEIV Non-operative musculo-skeletal
    17025, -- SEC_APACHEIV Non-operative neurologic
    17026, -- SEC_APACHEIV Non-operative respiratory
    17027, -- SEC_APACHEIV Non-operative transplant
    17028, -- SEC_APACHEIV Non-operative trauma

    -- NICE: surgical/medical combined in same parameter
    18669, -- NICE APACHEII diagnosen
    18670, -- NICE SEC APACHEII diagnosen
    18671, -- NICE APACHEIV diagnosen
    18673 -- NICE SEC APACHEIV diagnosen
  )

ORDER BY l.admissionid, l.measuredat, l.itemid
;
