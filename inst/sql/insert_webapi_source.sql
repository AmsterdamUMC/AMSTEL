INSERT INTO webapi.source 
(
    source_id, 
    source_name, 
    source_key, 
    source_connection, 
    source_dialect,
    is_cache_enabled --Not documented as requirement
) 
SELECT 
    nextval('webapi.source_sequence'), 
    '@cdm_source_name', 
    '@cdm_source_abbreviation', 
    '@cdm_connection_string', 
    '@cdm_dbms',
    TRUE
;

INSERT INTO webapi.source_daimon 
(
    source_daimon_id,
    source_id, 
    daimon_type, 
    table_qualifier, 
    priority
) 
SELECT 
    nextval('webapi.source_daimon_sequence'), 
    source_id, 
    0, -- CDM
    '@cdm_schema', 
    0
FROM webapi.source
WHERE source_key = '@cdm_source_abbreviation'
;

INSERT INTO webapi.source_daimon 
(
    source_daimon_id, 
    source_id, 
    daimon_type, 
    table_qualifier, 
    priority
) 
SELECT 
    nextval('webapi.source_daimon_sequence'),
    source_id, 
    1, -- Vocabulary
    '@cdm_schema', 
    1
FROM webapi.source
WHERE source_key = '@cdm_source_abbreviation'
;

INSERT INTO webapi.source_daimon 
(
    source_daimon_id, 
    source_id, 
    daimon_type, 
    table_qualifier, 
    priority
) 
SELECT 
    nextval('webapi.source_daimon_sequence'), 
    source_id, 
    2, -- Results (Achilles output).
    '@cdm_results_schema', 
    1
FROM webapi.source
WHERE source_key = '@cdm_source_abbreviation'
;

INSERT INTO webapi.source_daimon 
(
    source_daimon_id, 
    source_id, 
    daimon_type, 
    table_qualifier, 
    priority
) 
SELECT 
    nextval('webapi.source_daimon_sequence'), 
    source_id, 
    5, -- Temp
    '@cdm_temp_schema', 
    0
FROM webapi.source
WHERE source_key = '@cdm_source_abbreviation'
;
