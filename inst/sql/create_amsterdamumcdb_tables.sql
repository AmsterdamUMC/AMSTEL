-- Creates the tables for the AmsterdamUMCdb schema. This is used in the
-- TestingFramework to create an empty set of tables.
DROP TABLE IF EXISTS @amsterdamumcdb_schema.admissions;
CREATE TABLE IF NOT EXISTS @amsterdamumcdb_schema.admissions
(
    patientid INTEGER,
    admissionid serial PRIMARY KEY,
    admissioncount INTEGER,
    location VARCHAR,
    urgency BIT,
    origin VARCHAR,
    admittedat BIGINT,
    admissionyeargroup VARCHAR,
    dischargedat BIGINT,
    lengthofstay SMALLINT,
    destination VARCHAR,
    gender VARCHAR,
    agegroup VARCHAR,
    dateofdeath BIGINT,
    weightgroup VARCHAR,
    weightsource VARCHAR,
    heightgroup VARCHAR,
    heightsource VARCHAR,
    specialty VARCHAR
);

CREATE TABLE IF NOT EXISTS @amsterdamumcdb_schema.drugitems
(
    admissionid INTEGER,
    orderid BIGINT,
    ordercategoryid INTEGER,
    ordercategory VARCHAR,
    itemid INTEGER,
    item VARCHAR,
    isadditive BIT,
    isconditional BIT,
    rate FLOAT,
    rateunit VARCHAR,
    rateunitid INTEGER,
    ratetimeunitid INTEGER,
    doserateperkg BIT,
    dose FLOAT,
    doseunit VARCHAR,
    doserateunit VARCHAR,
    doseunitid INTEGER,
    doserateunitid INTEGER,
    administered FLOAT,
    administeredunit VARCHAR,
    administeredunitid INTEGER,
    action VARCHAR,
    start BIGINT,
    stop BIGINT,
    duration BIGINT,
    solutionitemid INTEGER,
    solutionitem VARCHAR,
    solutionadministered FLOAT,
    solutionadministeredunit VARCHAR,
    fluidin FLOAT,
    iscontinuous BIT
);

CREATE TABLE IF NOT EXISTS @amsterdamumcdb_schema.freetextitems
(
    admissionid INTEGER,
    itemid BIGINT,
    item VARCHAR,
    value VARCHAR,
    comment VARCHAR,
    measuredat BIGINT,
    registeredat BIGINT,
    registeredby VARCHAR,
    updatedat BIGINT,
    updatedby VARCHAR,
    islabresult BIT
);

CREATE TABLE IF NOT EXISTS @amsterdamumcdb_schema.listitems
(
    admissionid INTEGER,
    itemid BIGINT,
    item VARCHAR,
    valueid INT,
    value VARCHAR,
    measuredat BIGINT,
    registeredat BIGINT,
    registeredby VARCHAR,
    updatedat BIGINT,
    updatedby VARCHAR,
    islabresult BIT
);

CREATE TABLE IF NOT EXISTS @amsterdamumcdb_schema.numericitems
(
    admissionid INTEGER,
    itemid BIGINT,
    item VARCHAR,
    tag VARCHAR,
    value FLOAT,
    unitid INT,
    unit VARCHAR,
    comment VARCHAR,
    measuredat BIGINT,
    registeredat BIGINT,
    registeredby VARCHAR,
    updatedat BIGINT,
    updatedby VARCHAR,
    islabresult BIT,
    fluidout FLOAT
);

CREATE TABLE IF NOT EXISTS @amsterdamumcdb_schema.procedureorderitems
(
    admissionid INTEGER,
    orderid BIGINT,
    ordercategoryid INT,
    ordercategoryname VARCHAR,
    itemid INT,
    item VARCHAR,
    registeredat BIGINT,
    registeredby VARCHAR
);

CREATE TABLE IF NOT EXISTS @amsterdamumcdb_schema.processitems
(
    admissionid INTEGER,
    itemid BIGINT,
    item VARCHAR,
    start BIGINT,
    stop BIGINT,
    duration BIGINT
);
