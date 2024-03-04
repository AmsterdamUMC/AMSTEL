/*postgresql OMOP CDM Indices
  There are no unique indices created because it is assumed that the primary key constraints have been run prior to
  implementing indices.
*/
/************************
Standardized clinical data
************************/
CREATE INDEX idx_person_id  ON @cdmDatabaseSchema.person  (person_id ASC);
CLUSTER @cdmDatabaseSchema.person  USING idx_person_id;
CREATE INDEX idx_gender ON @cdmDatabaseSchema.person (gender_concept_id ASC);
CREATE INDEX idx_observation_period_id_1  ON @cdmDatabaseSchema.observation_period  (person_id ASC);
CLUSTER @cdmDatabaseSchema.observation_period  USING idx_observation_period_id_1;
CREATE INDEX idx_visit_person_id_1  ON @cdmDatabaseSchema.visit_occurrence  (person_id ASC);
CLUSTER @cdmDatabaseSchema.visit_occurrence  USING idx_visit_person_id_1;
CREATE INDEX idx_visit_concept_id_1 ON @cdmDatabaseSchema.visit_occurrence (visit_concept_id ASC);
CREATE INDEX idx_visit_det_person_id_1  ON @cdmDatabaseSchema.visit_detail  (person_id ASC);
CLUSTER @cdmDatabaseSchema.visit_detail  USING idx_visit_det_person_id_1;
CREATE INDEX idx_visit_det_concept_id_1 ON @cdmDatabaseSchema.visit_detail (visit_detail_concept_id ASC);
CREATE INDEX idx_visit_det_occ_id ON @cdmDatabaseSchema.visit_detail (visit_occurrence_id ASC);
CREATE INDEX idx_condition_person_id_1  ON @cdmDatabaseSchema.condition_occurrence  (person_id ASC);
CLUSTER @cdmDatabaseSchema.condition_occurrence  USING idx_condition_person_id_1;
CREATE INDEX idx_condition_concept_id_1 ON @cdmDatabaseSchema.condition_occurrence (condition_concept_id ASC);
CREATE INDEX idx_condition_visit_id_1 ON @cdmDatabaseSchema.condition_occurrence (visit_occurrence_id ASC);
CREATE INDEX idx_drug_person_id_1  ON @cdmDatabaseSchema.drug_exposure  (person_id ASC);
CLUSTER @cdmDatabaseSchema.drug_exposure  USING idx_drug_person_id_1;
CREATE INDEX idx_drug_concept_id_1 ON @cdmDatabaseSchema.drug_exposure (drug_concept_id ASC);
CREATE INDEX idx_drug_visit_id_1 ON @cdmDatabaseSchema.drug_exposure (visit_occurrence_id ASC);
CREATE INDEX idx_procedure_person_id_1  ON @cdmDatabaseSchema.procedure_occurrence  (person_id ASC);
CLUSTER @cdmDatabaseSchema.procedure_occurrence  USING idx_procedure_person_id_1;
CREATE INDEX idx_procedure_concept_id_1 ON @cdmDatabaseSchema.procedure_occurrence (procedure_concept_id ASC);
CREATE INDEX idx_procedure_visit_id_1 ON @cdmDatabaseSchema.procedure_occurrence (visit_occurrence_id ASC);
CREATE INDEX idx_device_person_id_1  ON @cdmDatabaseSchema.device_exposure  (person_id ASC);
CLUSTER @cdmDatabaseSchema.device_exposure  USING idx_device_person_id_1;
CREATE INDEX idx_device_concept_id_1 ON @cdmDatabaseSchema.device_exposure (device_concept_id ASC);
CREATE INDEX idx_device_visit_id_1 ON @cdmDatabaseSchema.device_exposure (visit_occurrence_id ASC);
CREATE INDEX idx_measurement_person_id_1  ON @cdmDatabaseSchema.measurement  (person_id ASC);
CLUSTER @cdmDatabaseSchema.measurement  USING idx_measurement_person_id_1;
CREATE INDEX idx_measurement_concept_id_1 ON @cdmDatabaseSchema.measurement (measurement_concept_id ASC);
CREATE INDEX idx_measurement_visit_id_1 ON @cdmDatabaseSchema.measurement (visit_occurrence_id ASC);
CREATE INDEX idx_observation_person_id_1  ON @cdmDatabaseSchema.observation  (person_id ASC);
CLUSTER @cdmDatabaseSchema.observation  USING idx_observation_person_id_1;
CREATE INDEX idx_observation_concept_id_1 ON @cdmDatabaseSchema.observation (observation_concept_id ASC);
CREATE INDEX idx_observation_visit_id_1 ON @cdmDatabaseSchema.observation (visit_occurrence_id ASC);
CREATE INDEX idx_death_person_id_1  ON @cdmDatabaseSchema.death  (person_id ASC);
CLUSTER @cdmDatabaseSchema.death  USING idx_death_person_id_1;
CREATE INDEX idx_note_person_id_1  ON @cdmDatabaseSchema.note  (person_id ASC);
CLUSTER @cdmDatabaseSchema.note  USING idx_note_person_id_1;
CREATE INDEX idx_note_concept_id_1 ON @cdmDatabaseSchema.note (note_type_concept_id ASC);
CREATE INDEX idx_note_visit_id_1 ON @cdmDatabaseSchema.note (visit_occurrence_id ASC);
CREATE INDEX idx_note_nlp_note_id_1  ON @cdmDatabaseSchema.note_nlp  (note_id ASC);
CLUSTER @cdmDatabaseSchema.note_nlp  USING idx_note_nlp_note_id_1;
CREATE INDEX idx_note_nlp_concept_id_1 ON @cdmDatabaseSchema.note_nlp (note_nlp_concept_id ASC);
CREATE INDEX idx_specimen_person_id_1  ON @cdmDatabaseSchema.specimen  (person_id ASC);
CLUSTER @cdmDatabaseSchema.specimen  USING idx_specimen_person_id_1;
CREATE INDEX idx_specimen_concept_id_1 ON @cdmDatabaseSchema.specimen (specimen_concept_id ASC);
CREATE INDEX idx_fact_relationship_id1 ON @cdmDatabaseSchema.fact_relationship (domain_concept_id_1 ASC);
CREATE INDEX idx_fact_relationship_id2 ON @cdmDatabaseSchema.fact_relationship (domain_concept_id_2 ASC);
CREATE INDEX idx_fact_relationship_id3 ON @cdmDatabaseSchema.fact_relationship (relationship_concept_id ASC);
/************************
Standardized health system data
************************/
CREATE INDEX idx_location_id_1  ON @cdmDatabaseSchema.location  (location_id ASC);
CLUSTER @cdmDatabaseSchema.location  USING idx_location_id_1;
CREATE INDEX idx_care_site_id_1  ON @cdmDatabaseSchema.care_site  (care_site_id ASC);
CLUSTER @cdmDatabaseSchema.care_site  USING idx_care_site_id_1;
CREATE INDEX idx_provider_id_1  ON @cdmDatabaseSchema.provider  (provider_id ASC);
CLUSTER @cdmDatabaseSchema.provider  USING idx_provider_id_1;
/************************
Standardized health economics
************************/
CREATE INDEX idx_period_person_id_1  ON @cdmDatabaseSchema.payer_plan_period  (person_id ASC);
CLUSTER @cdmDatabaseSchema.payer_plan_period  USING idx_period_person_id_1;
CREATE INDEX idx_cost_event_id  ON @cdmDatabaseSchema.cost (cost_event_id ASC);
/************************
Standardized derived elements
************************/
CREATE INDEX idx_drug_era_person_id_1  ON @cdmDatabaseSchema.drug_era  (person_id ASC);
CLUSTER @cdmDatabaseSchema.drug_era  USING idx_drug_era_person_id_1;
CREATE INDEX idx_drug_era_concept_id_1 ON @cdmDatabaseSchema.drug_era (drug_concept_id ASC);
CREATE INDEX idx_dose_era_person_id_1  ON @cdmDatabaseSchema.dose_era  (person_id ASC);
CLUSTER @cdmDatabaseSchema.dose_era  USING idx_dose_era_person_id_1;
CREATE INDEX idx_dose_era_concept_id_1 ON @cdmDatabaseSchema.dose_era (drug_concept_id ASC);
CREATE INDEX idx_condition_era_person_id_1  ON @cdmDatabaseSchema.condition_era  (person_id ASC);
CLUSTER @cdmDatabaseSchema.condition_era  USING idx_condition_era_person_id_1;
CREATE INDEX idx_condition_era_concept_id_1 ON @cdmDatabaseSchema.condition_era (condition_concept_id ASC);
/**************************
Standardized meta-data
***************************/
CREATE INDEX idx_metadata_concept_id_1  ON @cdmDatabaseSchema.metadata  (metadata_concept_id ASC);
CLUSTER @cdmDatabaseSchema.metadata  USING idx_metadata_concept_id_1;

