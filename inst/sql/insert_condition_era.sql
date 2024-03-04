-- Creates the Standard Derived CONDITION_ERA table based on records from
-- CONDITION_OCCURRENCE.
--
-- Modified from:
-- https://github.com/OHDSI/ETL-CMS/blob/master/SQL/create_CDMv5_condition_era.sql
--
TRUNCATE @cdm_schema.condition_era;

WITH cteConditionTarget AS (
	SELECT
		co.condition_occurrence_id,
		co.person_id,
		co.condition_concept_id,
		co.condition_start_date,
		COALESCE(NULLIF(co.condition_end_date,NULL),
		condition_start_date + INTERVAL '1 day') AS condition_end_date
	FROM @cdm_schema.condition_occurrence co
	--only select valid mapped concepts
	WHERE condition_concept_id != 0
),
cteEndDates AS (
  SELECT
    person_id,
		condition_concept_id,
		event_date - INTERVAL '30 days' AS end_date -- unpad the end date
	FROM (
		SELECT
			person_id,
			condition_concept_id,
			event_date,
			event_type,

			--pulls the current START down from the prior rows so that the NULLs from
			-- the END DATES will contain a value we can compare with
			MAX(start_ordinal) OVER (
			  PARTITION BY person_id, condition_concept_id
			  ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING
			) AS start_ordinal,

			-- re-number the inner UNION so all rows are numbered ordered by the event date
			ROW_NUMBER() OVER (
			  PARTITION BY person_id, condition_concept_id
			  ORDER BY event_date, event_type
			) AS overall_ord
		FROM (
			-- select the start dates, assigning a row number to each
			SELECT
				person_id,
				condition_concept_id,
				condition_start_date AS event_date,
				-1 AS event_type,
				ROW_NUMBER() OVER (PARTITION BY person_id,
				condition_concept_id ORDER BY condition_start_date) AS start_ordinal
			FROM cteConditionTarget

			UNION ALL

			-- pad the end dates by 30 to allow a grace period for overlapping ranges.
			SELECT
			  person_id,
			  condition_concept_id,
				condition_end_date + INTERVAL '30 days',
				1 AS event_type,
				NULL
			FROM cteConditionTarget
		) RAWDATA
	) e
	WHERE (2 * e.start_ordinal) - e.overall_ord = 0
),

cteConditionEnds AS (
  SELECT
    c.person_id,
	  c.condition_concept_id,
	  c.condition_start_date,
	  MIN(e.end_date) AS era_end_date
  FROM cteConditionTarget c
  JOIN cteEndDates e ON
    c.person_id = e.person_id
    AND c.condition_concept_id = e.condition_concept_id
    AND e.end_date >= c.condition_start_date
  GROUP BY
    c.condition_occurrence_id,
	  c.person_id,
	  c.condition_concept_id,
	  c.condition_start_date
)

INSERT INTO @cdm_schema.condition_era
(
  condition_era_id,
  person_id,
  condition_concept_id,
  condition_era_start_date,
  condition_era_end_date,
  condition_occurrence_count
)
SELECT
  ROW_NUMBER() OVER (
    ORDER BY
      person_id,
      condition_concept_id
  ) AS condition_era_id,
	person_id,
	condition_concept_id,
	MIN(condition_start_date) AS condition_era_start_date,
	era_end_date AS condition_era_end_date,
	COUNT(*) AS condition_occurrence_count
FROM cteConditionEnds
GROUP BY person_id, condition_concept_id, era_end_date
ORDER BY person_id, condition_concept_id
;
