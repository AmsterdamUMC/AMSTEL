-- INSERT records into PROCEDURE_OCCURRENCE from stem_table
-- WHERE domain_id = 10 (Procedure)
TRUNCATE @cdm_schema.procedure_occurrence;
INSERT INTO @cdm_schema.procedure_occurrence
(
    procedure_occurrence_id,
    person_id,
    procedure_concept_id,
    procedure_date,
    procedure_datetime,
    procedure_end_date,
    procedure_end_datetime,
    procedure_type_concept_id,
    modifier_concept_id,
    quantity,
    provider_id,
    visit_occurrence_id,
    visit_detail_id,
    procedure_source_value,
    procedure_source_concept_id,
    modifier_source_value
)
SELECT
    stem_table.id AS procedure_occurrence_id,
    stem_table.person_id AS person_id,
    stem_table.concept_id AS procedure_concept_id,
    stem_table.start_date AS procedure_date,
    stem_table.start_datetime AS procedure_datetime,
    stem_table.end_date AS procedure_end_date,
    stem_table.end_datetime AS procedure_end_datetime,
    stem_table.type_concept_id AS procedure_type_concept_id,
    stem_table.modifier_concept_id AS modifier_concept_id,
    stem_table.quantity AS quantity,
    stem_table.provider_id AS provider_id,
    stem_table.visit_occurrence_id AS visit_occurrence_id,
    NULL AS visit_detail_id,
    stem_table.source_value AS procedure_source_value,
    stem_table.source_concept_id AS procedure_source_concept_id,
    stem_table.modifier_source_value AS modifier_source_value

FROM @cdm_schema.stem_table
WHERE
  stem_table.domain_id = 10 -- Procedure
  AND (
    stem_table.quantity > 0 OR stem_table.quantity IS NULL
  )
;
