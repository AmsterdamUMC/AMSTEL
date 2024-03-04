-- After re-running Achilles with new CDM data, we need to refresh the cache by
-- dropping the relevant tables in the (hard-coded) webapi schema.
-- Based on the information in this forum post by Chris_Knoll
-- https://forums.ohdsi.org/t/how-to-force-the-update-of-reports-in-atlas/14282/3

-- achilles_cache: this stores the Data Sources reports (like Condition Occurence reports).
DROP TABLE IF EXISTS webapi.achilles_cache;

-- cdm_cache: this stores the record counts that are displayed when you search
-- the vocabulary or look at the ‘included concepts’ of a concept set.
-- Those columns are RC/DRC and PC/PRC (record count, descendant record count,
-- and person count, descendent person count).
DROP TABLE IF EXISTS webapi.cdm_cache;
