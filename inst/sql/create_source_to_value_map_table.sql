-- Since many numeric value concepts were deprecated, this will create a
-- new intermediate mappping table to allow mapping concepts to
-- values to enrich the source_to_concept_map table
CREATE TABLE IF NOT EXISTS @cdm_schema.source_to_value_map (
		source_code varchar(50) NOT NULL,
		source_concept_id integer NOT NULL,
		source_vocabulary_id varchar(20) NOT NULL,
    source_code_description varchar(255) NULL,
		value numeric NULL,
		row integer NOT NULL
);
