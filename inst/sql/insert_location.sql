/*
Location of Amsterdam UMC, location VUmc
*/
TRUNCATE @cdm_schema.location;
INSERT INTO @cdm_schema.location
(
    location_id,
    address_1,
    address_2,
    city,
    state,
    zip,
    county,
    location_source_value,
    country_concept_id,
    country_source_value,
    latitude,
    longitude
)
VALUES
(
    1, --location_id
    'De Boelelaan 1117', --address_1
    NULL, --address_2
    'Amsterdam', --city
    'NH', --state
    '1081 HV', --zip
    NULL, --county
    'VU medisch centrum', --location_source_value
    42032174, --country_concept_id,
    'Nederland', --country_source_value,
    52.3345817, --latitude
    4.7774081 --longitude
)
;
