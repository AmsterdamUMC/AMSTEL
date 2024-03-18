/*
Some data quality issues can be corrected by improving the patient level
(source) data based on other patient records
*/

/*
MISSING gender
Since AmsterdamUMCdb is an ICU dataset, there is actually only one (1) procedure
specific to females: 4168236 (Evaluation of uterine fundal height)

SELECT DISTINCT(PROCEDURE_CONCEPT_ID)
FROM @cdm_schema.PROCEDURE_OCCURRENCE cdmTable
JOIN @cdm_schema.concept_ancestor ca
ON ca.descendant_concept_id = cdmTable.PROCEDURE_CONCEPT_ID
WHERE ca.ancestor_concept_id IN (4041261)

For patients with a missing gender this will update the gender if the
PROCEDURE_OCCURENCE table contains this procedure
*/
UPDATE @cdm_schema.person p
SET gender_concept_id =  8532 -- FEMALE
WHERE
    -- unspecified gender
    p.gender_concept_id = 0

    -- 4041261 Procedure on female genital system
    AND person_id IN (
        SELECT person_id
        FROM @cdm_schema.procedure_occurrence po
        JOIN @cdm_schema.concept_ancestor ca ON
            ca.descendant_concept_id = po.procedure_concept_id
        WHERE
            ca.ancestor_concept_id IN (4041261)
    )
;
