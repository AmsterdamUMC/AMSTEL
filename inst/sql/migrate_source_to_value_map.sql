-- migrates invalid quantity concepts as values in a new source_to_value_map table

CREATE TABLE IF NOT EXISTS @cdm_schema.source_to_value_map (
		source_code varchar(50) NOT NULL,
		source_concept_id integer NOT NULL,
		source_vocabulary_id varchar(20) NOT NULL,
    source_code_description varchar(255) NULL,
		value numeric NULL,
		row integer NOT NULL
);

INSERT INTO @cdm_schema.source_to_value_map
(
    source_code,
		source_concept_id,
		source_vocabulary_id,
		source_code_description,
		value,
		row
)
SELECT
  source_code,
  source_concept_id,
  source_vocabulary_id,
  source_code_description,
  CAST(concept_name AS NUMERIC) AS value,
  ROW_NUMBER() OVER (PARTITION BY source_code ORDER BY stcm.valid_start_date) as row
FROM @cdm_schema.SOURCE_TO_CONCEPT_MAP stcm
JOIN @cdm_schema.concept co
    ON stcm.TARGET_CONCEPT_ID = co.concept_id
WHERE
    source_vocabulary_id IN ('AUMC Quantity', 'AUMC Drug Quantity')
--ordering by valid_start_date since this has been used for allowing
--a single concept mapped to multiple corresponding entries to have multiple corresponding entries
ORDER BY stcm.valid_start_date
;
