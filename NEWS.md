# amstel 1.0.0
## First release
First release for Milestone 3 reporting.

## ETL Changes
* SET `SOURCE_CONCEPT_ID` TO NULL instead of 0 since this is a not required field and considered `FAIL` in DQD.
* Added mapped unit from source date into UNIT_SOURCE_CONCEPT_ID for future ETL specification based on issue (https://github.com/OHDSI/CommonDataModel/issues/259) for future releases.
* Allows the ETL to process manually added units in Usagi using the `MAP_TO_UNIT` type to fix missing (implausible) units for some concepts that `FAIL` in DQD
* Conditional index creation (IF NOT EXISTS) to allow continuing index creation if there is a failure (i.e. `DISK FULL`)
* Added DRUG_STRENGTH to ignore list of DQD since this table is OHDSI maintained
* Migrated source vocabularies `AUMC Drug Quantity`, `AUMC Quantity` from `source_to_concept_map` to new `source_to_value_map` intermediate table since Vocabulary v5.0 29-FEB-24 deprecated many concepts representing numeric values.
* Improved ETL unit testing by increasing coverage.

## Mappings
* Remapped many concepts due to changes and deprecations of concepts in `Vocabulary v5.0 29-FEB-24`
* Changed mappings from `Volume fraction` (%) to `Pure Volume Fraction` (decimal fraction) for hematocrit
* Improved mappings from `listitems` table  (e.g. all Glasgow Coma Scale, MRC muscle strength)
* Removed the `None` and `Geen` units from mapping to allow NULL value for unit_concept_id to be stored in the MEASUREMENT table for unitless measurements (e.g. Hematocrit = 0.40)


## DataQualityDashboard
### Field level checks
1. `field_plausiblestartbeforeend_cdm_source_cdm_release_date`
The recent DQD version 2.6.0. that added the `plausibleStartBeforeEnd` check, introduced a bug by adding a check that fails if CDM_RELEASE DATE > CDM_SOURCE. This should, of course, be the other way around. This check is equivalent to the soon to be deprecated, but still active `field_plausibletemporalafter_cdm_source_cdm_release_date` check. To allow this check to function as intended the check has to be added to the row for `cdm_source_date` and not `cdm_release_date` (or the source code changed). The control file has been updated to reflect this.

2. `field_plausiblevaluehigh_drug_exposure_quantity`
Failing the default threshold of 1% is entirely expected for this ICU dataset, since most medication was administered intravenously and for most medication in DRUG_STRENGTH this means that the unit of quantity is milligram. As an example, 500 ml of normal saline contains 4500 mg sodium chloride. The threshold has been increased to 10% based on the current database value of ~8% records.

3. `field_sourcevaluecompleteness_drug_exposure_drug_source_value`
For completeness, the clinical database was updated to routinely contain every medication administration entry, thus leading to many drugs that were only administered sporadically. The ~40% of unmapped DRUG_SOURCE_VALUE fields have been used in only 1% of drug administrations. The acceptable threshold was increased to 40%.

4. `field_standardconceptrecordcompleteness_observation_observation_concept_id`
Many ICU specific concepts, mostly device generated measurements, such as Work of Breathing and Shallow Breathing Index, cannot be mapped to corresponding Standard Concepts. Since by specification, the MEASUREMENT table only allows concepts that are part of the Measurement domain, these unmapped concepts are placed in the OBSERVATION table. In addition since these observations are frequent (up to once every minute) this will lead to a very high number of `0` values in the `observation_concept_id` column. While alternatively these concepts could have been omitted, we have decided that these values could be of value in research outside of the standard network analyses. The acceptable threshold was increased to 90%, and hopefully can be decreased by improvements in the LOINC and SNOMED vocabularies.

5. `field_standardconceptrecordcompleteness_observation_unit_concept_id`
Similary, some device generated ICU specific concepts use units that currently do not have a corresponding Standard Concept, e.g. `J/l` (Joule per liter) and `µV` (microvolt). The acceptable threshold was increased to 30%.

### Concept level checks
1. `concept_plausibleunitconceptids_measurement_measurement_concept_id_3026361`
Added UNIT_CONCEPT_ID 8734 (`10*12/L`, `trillion per liter`) to the list of plausible units `"32706,8785,8815,8931"` for MEASUREMENT_CONCEPT_ID 3026361 (`Erythrocytes [#/volume] in Blood`) since this is equivalent to 8815 and 8931. It is common in the Netherlands to express absolute cell counts per liter. 8785 seems a very unlikely unit based on the expected number of significant digits. Is this really used in the wild? For future work it would probably help to define a base unit (i.e. 'L') and derive the equivalent units (and perhaps acceptable values) from the base unit.

List of plausible units:

ID | Code | Name
--- | --- | ---
32706   | 10*4/uL |	ten thousand per microliter
8785 |	/mm3 |	per cubic millimeter
8815 |	10*6/uL	| million per microliter
8931 |	10*6/mm3 |	million per cubic millimeter
NEW: 8734 |	10*12/L	| trillion per liter

2. `concept_plausibleunitconceptids_measurement_measurement_concept_id_3010910`
Similar to `concept_plausibleunitconceptids_measurement_measurement_concept_id_3026361`:
Added UNIT_CONCEPT_ID_ID 8734 (`10*12/L`, `trillion per liter`) to the list of plausible units (8647,8785,8815,8931) for MEASUREMENT_CONCEPT_ID 3010910 (`ERYTHROCYTES [#/VOLUME] IN BODY FLUID`)


3. `concept_plausibleunitconceptids_measurement_measurement_concept_id_3022174`
Added UNIT_CONCEPT_ID 9444 (`10*9/L`, `billion per liter`) to the list of plausible units for MEASUREMENT_CONCEPT_ID 3022174 (`LEUKOCYTES [#/VOLUME] IN BODY FLUID`).

ID | Code | Name
--- | --- | ---
8647 |	/uL |	per microliter
8784 |	{cells}/uL |	cells per microliter
8785 |	/mm3 |	per cubic millimeter
8848 |	10*3/uL	| thousand per microliter
8961 |	10*3/mm3 | thousand per cubic millimeter
NEW: 9444 |	10*9/L	billion per liter

4. Missing `FEMALE` gender for person(s) having procedures with ancestor concept 4041261 (`Procedure on female genital system`)
This has been fixed by adding the gender_concept_id to the record in `person`. The function `amstel::apply_corrections()` has been created to allow future data corrections to be applied during the ETL process.

# amstel 0.9.0

## Milestone 2
* Pre-release version released for EHDEN Milestone 2.

## Improved documentation
* Added pkgdown documentation
* Examples
* Hide some output when used non-interactively (e.g. progress bars)