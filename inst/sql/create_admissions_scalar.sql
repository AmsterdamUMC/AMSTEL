DROP TABLE IF EXISTS @cdm_schema.admissions_scalar;
CREATE TABLE IF NOT EXISTS @cdm_schema.admissions_scalar AS TABLE @amsterdamumcdb_schema.admissions;
ALTER TABLE @cdm_schema.admissions_scalar ADD COLUMN IF NOT EXISTS age INT;
ALTER TABLE @cdm_schema.admissions_scalar ADD COLUMN IF NOT EXISTS admissionyear INT;
ALTER TABLE @cdm_schema.admissions_scalar ADD COLUMN IF NOT EXISTS admissiondatetime DATETIME;
ALTER TABLE @cdm_schema.admissions_scalar ADD COLUMN IF NOT EXISTS weight INT;
ALTER TABLE @cdm_schema.admissions_scalar ADD COLUMN IF NOT EXISTS height INT;

UPDATE @cdm_schema.admissions_scalar
SET
  age =
    CASE agegroup
      WHEN '18-39' THEN 30
      WHEN '40-49' THEN 45
      WHEN '50-59' THEN 55
      WHEN '60-69' THEN 65
      WHEN '70-79' THEN 75
      WHEN '80+' THEN 85
    END,
  admissionyear =
    CASE admissionyeargroup
      WHEN '2010-2016' THEN 2013
      WHEN '2003-2009' THEN 2006
    END,
  admissiondatetime =
    CASE admissionyeargroup
      WHEN '2010-2016' THEN DATE('2013-01-01') + make_interval(secs => admittedat/1000)
      WHEN '2003-2009' THEN DATE('2006-01-01') + make_interval(secs => admittedat/1000)
    END,
  weight =
    CASE weightgroup --TO DO fix doc: weightsource
    WHEN '59-' THEN 55
      WHEN '60-69' THEN 65
      WHEN '70-79' THEN 75
      WHEN '80-89' THEN 85
      WHEN '90-99' THEN 95
      WHEN '100-109' THEN 105
      WHEN '110+' THEN 115
    END,
  height =
    CASE heightgroup
      WHEN '159-' THEN 155
      WHEN '160-169' THEN 165
      WHEN '170-179' THEN 175
      WHEN '180-189' THEN 185
      WHEN '190+' THEN 195
    END
