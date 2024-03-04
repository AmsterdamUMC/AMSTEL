-- Creates the Standard Derived DRUG_ERA table from records in the
-- DRUG_EXPOSURE table.
--
-- Modified from:
-- https://github.com/OHDSI/ETL-CMS/blob/master/SQL/create_CDMv5_drug_era_non_stockpile.sql

TRUNCATE @cdm_schema.drug_era;

WITH ctePreDrugTarget AS (
  -- Normalize DRUG_EXPOSURE_END_DATE to either the existing drug exposure end date, or add days supply, or add 1 day to the start date
	SELECT
	  d.drug_exposure_id,
		d.person_id,
		c.concept_id AS ingredient_concept_id,
		d.drug_exposure_start_date AS drug_exposure_start_date,
		d.days_supply AS days_supply,
		COALESCE(
			---NULLIF returns NULL if both values are the same, otherwise it returns the first parameter
			NULLIF(drug_exposure_end_date, NULL),
			---If drug_exposure_end_date != NULL, return drug_exposure_end_date, otherwise go to next case
			NULLIF(drug_exposure_start_date + (INTERVAL '1 day' * days_supply), drug_exposure_start_date),
			---If days_supply != NULL or 0, return drug_exposure_start_date + days_supply, otherwise go to next case
			drug_exposure_start_date + INTERVAL '1 day'
			---Add 1 day to the drug_exposure_start_date since there is no end_date or INTERVAL for the days_supply
		) AS drug_exposure_end_date
	FROM @cdm_schema.drug_exposure d
	JOIN @cdm_schema.concept_ancestor ca ON
	  ca.descendant_concept_id = d.drug_concept_id
	JOIN @cdm_schema.concept c ON
	  ca.ancestor_concept_id = c.concept_id
	WHERE
	  c.vocabulary_id = 'RxNorm' -- selects RxNorm from the vocabulary_id
		AND c.concept_class_id = 'Ingredient'

		-- Ignore unmapped drug_concept_id's
		AND d.drug_concept_id != 0
),

-- A preliminary sorting that groups all of the overlapping exposures into one
-- exposure so that we don't double-count non-gap-days
cteSubExposureEndDates AS (
	SELECT
	  person_id,
	  ingredient_concept_id,
	  event_date AS end_date
	FROM (
	  SELECT
	    person_id,
	    ingredient_concept_id,
	    event_date,
	    event_type,

	    -- this pulls the current START down from the prior rows so that the NULLs
		  -- from the END DATES will contain a value we can compare with
	    MAX(start_ordinal) OVER (
	      PARTITION BY person_id, ingredient_concept_id
	      ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING) AS start_ordinal,

		  -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
		  ROW_NUMBER() OVER (
		    PARTITION BY person_id, ingredient_concept_id
				ORDER BY event_date, event_type
			) AS overall_ord

		FROM (
			-- select the start dates, assigning a row number to each
			SELECT
			  person_id,
			  ingredient_concept_id,
			  drug_exposure_start_date AS event_date,
			  -1 AS event_type,
			  ROW_NUMBER() OVER (
			    PARTITION BY person_id, ingredient_concept_id
			    ORDER BY drug_exposure_start_date
		    ) AS start_ordinal
			FROM ctePreDrugTarget

			UNION ALL

			SELECT
			  person_id,
			  ingredient_concept_id,
			  drug_exposure_end_date,
			  1 AS event_type,
			  NULL
			FROM ctePreDrugTarget) RAWDATA
	) e
	WHERE (2 * e.start_ordinal) - e.overall_ord = 0
),
cteDrugExposureEnds AS (
  SELECT
	  dt.person_id,
	  dt.ingredient_concept_id AS drug_concept_id,
	  dt.drug_exposure_start_date,
	  MIN(e.end_date) AS drug_sub_exposure_end_date
  FROM ctePreDrugTarget dt
  JOIN cteSubExposureEndDates e ON
    dt.person_id = e.person_id
    AND dt.ingredient_concept_id = e.ingredient_concept_id
    AND e.end_date >= dt.drug_exposure_start_date
  GROUP BY
    dt.drug_exposure_id,
    dt.person_id,
	  dt.ingredient_concept_id,
	  dt.drug_exposure_start_date
),

cteSubExposures AS
(
	SELECT
	  ROW_NUMBER() OVER (
	    PARTITION BY person_id, drug_concept_id, drug_sub_exposure_end_date
	 ) AS row_number,
	 person_id,
	 drug_concept_id,
	 MIN(drug_exposure_start_date) AS drug_sub_exposure_start_date,
	 drug_sub_exposure_end_date,
	 COUNT(*) AS drug_exposure_count
	FROM cteDrugExposureEnds
	GROUP BY
	  person_id,
	  drug_concept_id,
	  drug_sub_exposure_end_date
	ORDER BY
	  person_id,
	  drug_concept_id
),

--------------------------------------------------------------------------------------------------------------
/*Everything above grouped exposures into sub_exposures if there was overlap between exposures.
 *So there was no persistence window. Now we can add the persistence window to calculate eras.
 */
--------------------------------------------------------------------------------------------------------------

cteFinalTarget AS (
	SELECT
	  row_number,
	  person_id,
	  drug_concept_id AS ingredient_concept_id,
	  drug_sub_exposure_start_date,
	  drug_sub_exposure_end_date,
	  drug_exposure_count,
		drug_sub_exposure_end_date - drug_sub_exposure_start_date AS days_exposed
	FROM cteSubExposures
),

cteEndDates AS (
	SELECT person_id, ingredient_concept_id, event_date - INTERVAL '30 days' AS end_date -- unpad the end date
	FROM (
		  SELECT
		    person_id,
		    ingredient_concept_id,
		    event_date,
		    event_type,

		    -- this pulls the current START down from the prior rows so that the NULLs
		    -- from the END DATES will contain a value we can compare with
		    MAX(start_ordinal) OVER (
		      PARTITION BY person_id, ingredient_concept_id
		      ORDER BY event_date, event_type ROWS UNBOUNDED PRECEDING
		    ) AS start_ordinal,


		  -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
			ROW_NUMBER() OVER (PARTITION BY person_id, ingredient_concept_id
				ORDER BY event_date, event_type) AS overall_ord

		FROM (

			-- select the start dates, assigning a row number to each
			SELECT
			  person_id,
			  ingredient_concept_id,
			  drug_sub_exposure_start_date AS event_date,
			  -1 AS event_type,
			  ROW_NUMBER() OVER (
			    PARTITION BY person_id, ingredient_concept_id
				  ORDER BY drug_sub_exposure_start_date
				) AS start_ordinal
			FROM cteFinalTarget

			UNION ALL

			-- pad the end dates by 30 to allow a grace period for overlapping ranges.
			SELECT
			  person_id,
			  ingredient_concept_id,
			  drug_sub_exposure_end_date + INTERVAL '30 days',
			  1 AS event_type,
			  NULL
			 FROM cteFinalTarget
			) RAWDATA
	) e
	WHERE (2 * e.start_ordinal) - e.overall_ord = 0
),

cteDrugEraEnds AS (
  SELECT
	  ft.person_id,
	  ft.ingredient_concept_id AS drug_concept_id,
	  ft.drug_sub_exposure_start_date,
	  MIN(e.end_date) AS drug_era_end_date,
	  drug_exposure_count,
	  days_exposed
  FROM cteFinalTarget ft
  JOIN cteEndDates e ON
    ft.person_id = e.person_id
    AND ft.ingredient_concept_id = e.ingredient_concept_id
    AND e.end_date >= ft.drug_sub_exposure_start_date
    GROUP BY
      ft.person_id,
	    ft.ingredient_concept_id,
	    ft.drug_sub_exposure_start_date,
	    drug_exposure_count,
	    days_exposed
	 )

INSERT INTO @cdm_schema.drug_era
(
  drug_era_id,
  person_id,
  drug_concept_id,
  drug_era_start_date,
  drug_era_end_date,
  drug_exposure_count,
  gap_days
)
SELECT
  ROW_NUMBER() OVER (
    ORDER BY
      person_id,
      drug_concept_id
  ) AS drug_era_id,
	person_id,
	drug_concept_id,
	MIN(drug_sub_exposure_start_date) AS drug_era_start_date,
	drug_era_end_date,
	SUM(drug_exposure_count) AS drug_exposure_count,
	EXTRACT(EPOCH FROM drug_era_end_date - MIN(drug_sub_exposure_start_date) - SUM(days_exposed)) / 86400 AS gap_days
FROM cteDrugEraEnds dee
GROUP BY person_id, drug_concept_id, drug_era_end_date
ORDER BY person_id, drug_concept_id
;
