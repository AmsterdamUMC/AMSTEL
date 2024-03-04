/*
Per Dutch legislation: race_concept_id and ethnicity_concept_id are not
routinely documented => set values to 0
*/
TRUNCATE @cdm_schema.person;
INSERT INTO @cdm_schema.person
(
    person_id,
    gender_concept_id,
    year_of_birth,
    month_of_birth,
    day_of_birth,
    birth_datetime,
    race_concept_id,
    ethnicity_concept_id,
    location_id,
    provider_id,
    care_site_id,
    person_source_value,
    gender_source_value,
    gender_source_concept_id,
    race_source_value,
    race_source_concept_id,
    ethnicity_source_value,
    ethnicity_source_concept_id
)
SELECT
    a.patientid AS person_id,

    CASE a.gender
      WHEN 'Vrouw' Then 8532 --FEMALE
      WHEN 'Man' Then 8507 --MALE
      ELSE 0
    END
    AS gender_concept_id,

    a.admissionyear - a.age AS year_of_birth,

    NULL AS month_of_birth,

    NULL AS day_of_birth,

    NULL AS birth_datetime,

    0 AS race_concept_id,

    0 AS ethnicity_concept_id,

    NULL AS location_id,

    NULL AS provider_id,

    NULL AS care_site_id,

    a.patientid AS person_source_value,

    a.gender AS gender_source_value,

    0 AS gender_source_concept_id,

    NULL AS race_source_value,

    NULL AS race_source_concept_id,

    NULL AS ethnicity_source_value,

    NULL AS ethnicity_source_concept_id

FROM @cdm_schema.admissions_scalar a --a modified version of the AmsterdamUMCdb admissions table with scalar values

WHERE admissioncount = 1
--use the first admission for patient demographics since AmsterdamUMCdb does not have a separate patients table

ORDER BY patientid, admittedat;
