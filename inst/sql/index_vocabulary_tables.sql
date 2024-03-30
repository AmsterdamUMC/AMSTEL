/**************************
Standardized vocabularies
***************************/
-- Code generated using OHDSI CommonDataModel (https://github.com/OHDSI/CommonDataModel/blob/main/R/writeDDL.R)
-- CommonDataModel::writeIndex(targetDialect="postgresql", cdmVersion="5.4")
CREATE INDEX IF NOT EXISTS idx_concept_concept_id  ON @cdmDatabaseSchema.concept  (concept_id ASC);
CLUSTER @cdmDatabaseSchema.concept  USING idx_concept_concept_id ;
CREATE INDEX IF NOT EXISTS idx_concept_code ON @cdmDatabaseSchema.concept (concept_code ASC);
CREATE INDEX IF NOT EXISTS idx_concept_vocabluary_id ON @cdmDatabaseSchema.concept (vocabulary_id ASC);
CREATE INDEX IF NOT EXISTS idx_concept_domain_id ON @cdmDatabaseSchema.concept (domain_id ASC);
CREATE INDEX IF NOT EXISTS idx_concept_class_id ON @cdmDatabaseSchema.concept (concept_class_id ASC);
CREATE INDEX IF NOT EXISTS idx_vocabulary_vocabulary_id  ON @cdmDatabaseSchema.vocabulary  (vocabulary_id ASC);
CLUSTER @cdmDatabaseSchema.vocabulary  USING idx_vocabulary_vocabulary_id ;
CREATE INDEX IF NOT EXISTS idx_domain_domain_id  ON @cdmDatabaseSchema.domain  (domain_id ASC);
CLUSTER @cdmDatabaseSchema.domain  USING idx_domain_domain_id ;
CREATE INDEX IF NOT EXISTS idx_concept_class_class_id  ON @cdmDatabaseSchema.concept_class  (concept_class_id ASC);
CLUSTER @cdmDatabaseSchema.concept_class  USING idx_concept_class_class_id ;
CREATE INDEX IF NOT EXISTS idx_concept_relationship_id_1  ON @cdmDatabaseSchema.concept_relationship  (concept_id_1 ASC);
CLUSTER @cdmDatabaseSchema.concept_relationship  USING idx_concept_relationship_id_1 ;
CREATE INDEX IF NOT EXISTS idx_concept_relationship_id_2 ON @cdmDatabaseSchema.concept_relationship (concept_id_2 ASC);
CREATE INDEX IF NOT EXISTS idx_concept_relationship_id_3 ON @cdmDatabaseSchema.concept_relationship (relationship_id ASC);
CREATE INDEX IF NOT EXISTS idx_relationship_rel_id  ON @cdmDatabaseSchema.relationship  (relationship_id ASC);
CLUSTER @cdmDatabaseSchema.relationship  USING idx_relationship_rel_id ;
CREATE INDEX IF NOT EXISTS idx_concept_synonym_id  ON @cdmDatabaseSchema.concept_synonym  (concept_id ASC);
CLUSTER @cdmDatabaseSchema.concept_synonym  USING idx_concept_synonym_id ;
CREATE INDEX IF NOT EXISTS idx_concept_ancestor_id_1  ON @cdmDatabaseSchema.concept_ancestor  (ancestor_concept_id ASC);
CLUSTER @cdmDatabaseSchema.concept_ancestor  USING idx_concept_ancestor_id_1 ;
CREATE INDEX IF NOT EXISTS idx_concept_ancestor_id_2 ON @cdmDatabaseSchema.concept_ancestor (descendant_concept_id ASC);
CREATE INDEX IF NOT EXISTS idx_source_to_concept_map_3  ON @cdmDatabaseSchema.source_to_concept_map  (target_concept_id ASC);
CLUSTER @cdmDatabaseSchema.source_to_concept_map  USING idx_source_to_concept_map_3 ;
CREATE INDEX IF NOT EXISTS idx_source_to_concept_map_1 ON @cdmDatabaseSchema.source_to_concept_map (source_vocabulary_id ASC);
CREATE INDEX IF NOT EXISTS idx_source_to_concept_map_2 ON @cdmDatabaseSchema.source_to_concept_map (target_vocabulary_id ASC);
CREATE INDEX IF NOT EXISTS idx_source_to_concept_map_c ON @cdmDatabaseSchema.source_to_concept_map (source_code ASC);
CREATE INDEX IF NOT EXISTS idx_drug_strength_id_1  ON @cdmDatabaseSchema.drug_strength  (drug_concept_id ASC);
CLUSTER @cdmDatabaseSchema.drug_strength  USING idx_drug_strength_id_1 ;
CREATE INDEX IF NOT EXISTS idx_drug_strength_id_2 ON @cdmDatabaseSchema.drug_strength (ingredient_concept_id ASC);
