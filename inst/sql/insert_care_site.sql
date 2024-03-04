/*
Only contains two care sites: Intensive Care and Medium Care
*/
TRUNCATE @cdm_schema.care_site;
INSERT INTO @cdm_schema.care_site
(
    care_site_id,
    care_site_name,
    place_of_service_concept_id,
    location_id,
    care_site_source_value,
    place_of_service_source_value
)
VALUES
(
    1, --care_site_id
    'Intensive Care', --care_site_name
    581379, --place_of_service_concept_id: Inpatient Critical Care Facility
    1, --location_id
    'IC', --care_site_source_value
    NULL --place_of_service_source_value
),
(
    2, --care_site_id
    'Medium Care', --care_site_name
    581379, --place_of_service_concept_id: Inpatient Critical Care Facility
    1, --location_id
    'MC', --care_site_source_value
    NULL --place_of_service_source_value
)
;
