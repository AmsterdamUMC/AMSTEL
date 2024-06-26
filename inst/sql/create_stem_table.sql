-- Creates a common stem_table
--
-- Since the concepts in AmsterdamUMCdb from the different tables
-- have multiple target tables in CDM, we are using the intermediate stem_table
-- to combine the data and process the data as needed before inserting the
-- records into the final CDM target tables.
DROP TABLE IF EXISTS @cdm_schema.stem_table;
CREATE TABLE IF NOT EXISTS @cdm_schema.stem_table (
    domain_id int,
    id int GENERATED ALWAYS AS IDENTITY,
    person_id int,
    concept_id int,
    start_date date,
    start_datetime timestamp,
    end_date date,
    end_datetime timestamp,
    type_concept_id int,
    provider_id int,
    visit_occurrence_id int,
    visit_detail_id int,
    
    -- OMOP CDM compliant: source_value varchar(50),
    source_value varchar(255),

    source_concept_id int,
    value_as_number numeric,

	-- OMOP CDM compliant: value_as_string varchar(60),
	-- Strictly not OMOP CDM conformant but allows 
	-- storing full source values
    value_as_string varchar(1024),

    value_as_concept_id int,
    unit_concept_id int,
    
    -- OMOP CDM compliant: value_source_value varchar(50),
    -- Strictly not OMOP CDM conformant but allows 
    -- storing full source values including comments:
    value_source_value varchar(1024) NULL,

    unit_source_concept_id int,
    unit_source_value varchar(50),
    verbatim_end_date date,
    days_supply int,
    dose_unit_source_value varchar(50),
    modifier_concept_id int,
    modifier_source_value varchar(50),
    measurement_time varchar(10),
    operator_concept_id int,
    quantity numeric,
    range_low numeric,
    range_high numeric,
    stop_reason varchar(20),
    refills int,
    sig text,
    route_concept_id int,
    route_source_value varchar(50),
    lot_number varchar(50),
    unique_device_id int,
    production_id int,
    anatomic_site_concept_id int,
    disease_status_concept_id int,
    specimen_source_id int,
    anatomic_site_source_value varchar(50),
    disease_status_source_value varchar(50),
    condition_status_concept_id int,
    condition_status_source_value varchar(50),
    qualifier_concept_id int,
    qualifier_source_value varchar(50),
    event_id int,
    event_field_concept_id int,
    PRIMARY KEY (id)
);
