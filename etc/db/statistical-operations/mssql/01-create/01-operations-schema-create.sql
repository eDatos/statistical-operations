-- ###########################################
-- # Create
-- ###########################################

-- Create normal entities
    
CREATE TABLE TB_INTERNATIONAL_STRINGS (
  ID BIGINT NOT NULL,
  VERSION BIGINT NOT NULL
);


CREATE TABLE TB_LIS_COLL_METHODS (
  ID BIGINT NOT NULL,
  IDENTIFIER NVARCHAR(255) NOT NULL,
  UUID NVARCHAR(36) NOT NULL,
  VERSION BIGINT NOT NULL,
  DESCRIPTION BIGINT
);


CREATE TABLE TB_LIS_COSTS (
  ID BIGINT NOT NULL,
  IDENTIFIER NVARCHAR(255) NOT NULL,
  UUID NVARCHAR(36) NOT NULL,
  VERSION BIGINT NOT NULL,
  DESCRIPTION BIGINT
);


CREATE TABLE TB_FAMILIES (
  ID BIGINT NOT NULL,
  CODE NVARCHAR(255) NOT NULL,
  URN NVARCHAR(4000) NOT NULL,
  INTERNAL_INVENTORY_DATE_TZ VARCHAR(50),
  INTERNAL_INVENTORY_DATE DATETIME,
  INVENTORY_DATE_TZ VARCHAR(50),
  INVENTORY_DATE DATETIME,
  UPDATE_DATE_TZ VARCHAR(50),
  UPDATE_DATE DATETIME,
  UUID NVARCHAR(36) NOT NULL,
  CREATED_DATE_TZ VARCHAR(50),
  CREATED_DATE DATETIME,
  CREATED_BY NVARCHAR(50),
  LAST_UPDATED_TZ VARCHAR(50),
  LAST_UPDATED DATETIME,
  LAST_UPDATED_BY NVARCHAR(50),
  VERSION BIGINT NOT NULL,
  TITLE_FK BIGINT NOT NULL,
  ACRONYM_FK BIGINT,
  DESCRIPTION_FK BIGINT,
  PROC_STATUS NVARCHAR(255) NOT NULL
);


CREATE TABLE TB_EXTERNAL_ITEMS (
  ID BIGINT NOT NULL,
  CODE NVARCHAR(255) NOT NULL,
  CODE_NESTED NVARCHAR(255),
  URI NVARCHAR(4000) NOT NULL,
  URN NVARCHAR(4000),
  URN_PROVIDER NVARCHAR(4000),
  MANAGEMENT_APP_URL NVARCHAR(4000),
  VERSION BIGINT NOT NULL,
  TITLE_FK BIGINT,
  TYPE NVARCHAR(255) NOT NULL
);


CREATE TABLE TB_LIS_SURVEY_TYPES (
  ID BIGINT NOT NULL,
  IDENTIFIER NVARCHAR(255) NOT NULL,
  UUID NVARCHAR(36) NOT NULL,
  VERSION BIGINT NOT NULL,
  DESCRIPTION BIGINT
);


CREATE TABLE TB_LIS_OFFICIALITY_TYPES (
  ID BIGINT NOT NULL,
  IDENTIFIER NVARCHAR(255) NOT NULL,
  UUID NVARCHAR(36) NOT NULL,
  VERSION BIGINT NOT NULL,
  DESCRIPTION BIGINT
);


CREATE TABLE TB_OPERATIONS (
  ID BIGINT NOT NULL,
  CODE NVARCHAR(11) NOT NULL,
  URN NVARCHAR(4000) NOT NULL,
  INDICATOR_SYSTEM BIT NOT NULL,
  INTERNAL_INVENTORY_DATE_TZ VARCHAR(50),
  INTERNAL_INVENTORY_DATE DATETIME,
  CURRENTLY_ACTIVE BIT NOT NULL,
  RELEASE_CALENDAR BIT NOT NULL,
  RELEASE_CALENDAR_ACCESS NVARCHAR(4000),
  INVENTORY_DATE_TZ VARCHAR(50),
  INVENTORY_DATE DATETIME,
  UPDATE_DATE_TZ VARCHAR(50),
  UPDATE_DATE DATETIME,
  UUID NVARCHAR(36) NOT NULL,
  CREATED_DATE_TZ VARCHAR(50),
  CREATED_DATE DATETIME,
  CREATED_BY NVARCHAR(50),
  LAST_UPDATED_TZ VARCHAR(50),
  LAST_UPDATED DATETIME,
  LAST_UPDATED_BY NVARCHAR(50),
  VERSION BIGINT NOT NULL,
  COMMON_METADATA_FK BIGINT,
  TITLE_FK BIGINT NOT NULL,
  ACRONYM_FK BIGINT,
  SUBJECT_AREA_FK BIGINT,
  OBJECTIVE_FK BIGINT,
  DESCRIPTION_FK BIGINT,
  SURVEY_TYPE_FK BIGINT,
  OFFICIALITY_TYPE_FK BIGINT,
  REL_POL_US_AC_FK BIGINT,
  REV_POLICY_FK BIGINT,
  REV_PRACTICE_FK BIGINT,
  SPECIFIC_LEGAL_ACTS_FK BIGINT,
  SPECIFIC_DATA_SHARING_FK BIGINT,
  COMMENT_FK BIGINT,
  NOTES_FK BIGINT,
  PROC_STATUS NVARCHAR(255) NOT NULL,
  STATUS NVARCHAR(255) NOT NULL,
  STREAM_MESSAGE_STATUS VARCHAR(255) NOT NULL
);


CREATE TABLE TB_LIS_INSTANCE_TYPES (
  ID BIGINT NOT NULL,
  IDENTIFIER NVARCHAR(255) NOT NULL,
  UUID NVARCHAR(36) NOT NULL,
  VERSION BIGINT NOT NULL,
  DESCRIPTION BIGINT
);


CREATE TABLE TB_LIS_SURVEY_SOURCES (
  ID BIGINT NOT NULL,
  IDENTIFIER NVARCHAR(255) NOT NULL,
  UUID NVARCHAR(36) NOT NULL,
  VERSION BIGINT NOT NULL,
  DESCRIPTION BIGINT
);


CREATE TABLE TB_INSTANCES (
  ID BIGINT NOT NULL,
  ORDER_IDX INT NOT NULL,
  CODE NVARCHAR(255) NOT NULL,
  URN NVARCHAR(4000) NOT NULL,
  BASE_PERIOD NVARCHAR(255),
  INTERNAL_INVENTORY_DATE_TZ VARCHAR(50),
  INTERNAL_INVENTORY_DATE DATETIME,
  INVENTORY_DATE_TZ VARCHAR(50),
  INVENTORY_DATE DATETIME,
  UPDATE_DATE_TZ VARCHAR(50),
  UPDATE_DATE DATETIME,
  UUID NVARCHAR(36) NOT NULL,
  CREATED_DATE_TZ VARCHAR(50),
  CREATED_DATE DATETIME,
  CREATED_BY NVARCHAR(50),
  LAST_UPDATED_TZ VARCHAR(50),
  LAST_UPDATED DATETIME,
  LAST_UPDATED_BY NVARCHAR(50),
  VERSION BIGINT NOT NULL,
  TITLE_FK BIGINT NOT NULL,
  ACRONYM_FK BIGINT,
  OPERATION_FK BIGINT NOT NULL,
  DATA_DESCRIPTION_FK BIGINT,
  STATISTICAL_POPULATION_FK BIGINT,
  GEOGRAPHIC_COMPARABILITY_FK BIGINT,
  TEMPORAL_COMPARABILITY_FK BIGINT,
  STAT_CONC_DEF_FK BIGINT,
  CLASS_SYSTEM_FK BIGINT,
  INSTANCE_TYPE_FK BIGINT,
  DOC_METHOD_FK BIGINT,
  SURVEY_SOURCE_FK BIGINT,
  COLL_METHOD_FK BIGINT,
  DATA_VALIDATION_FK BIGINT,
  DATA_COMPILATION_FK BIGINT,
  ADJUSTMENT_FK BIGINT,
  COST_BURDEN_FK BIGINT,
  QUALITY_DOC_FK BIGINT,
  QUALITY_ASSURE_FK BIGINT,
  QUALITY_ASSMNT_FK BIGINT,
  USER_NEEDS_FK BIGINT,
  USER_SAT_FK BIGINT,
  COMPLETENESS_FK BIGINT,
  TIMELINESS_FK BIGINT,
  PUNCTUALITY_FK BIGINT,
  ACCURACY_OVERALL_FK BIGINT,
  SAMPLING_ERR_FK BIGINT,
  NONSAMPLING_ERR_FK BIGINT,
  COHER_X_DOMAIN_FK BIGINT,
  COHER_INTERNAL_FK BIGINT,
  COMMENT_FK BIGINT,
  NOTES_FK BIGINT,
  PROC_STATUS NVARCHAR(255) NOT NULL
);


CREATE TABLE TB_LOCALISED_STRINGS (
  ID BIGINT NOT NULL,
  LABEL NVARCHAR(4000) NOT NULL,
  LOCALE NVARCHAR(255) NOT NULL,
  IS_UNMODIFIABLE BIT,
  VERSION BIGINT NOT NULL,
  INTERNATIONAL_STRING_FK BIGINT NOT NULL
);



-- Create many to many relations
    
CREATE TABLE TB_EI_CLASS_SYSTEM_LISTS (
  CLASS_SYSTEM_LIST_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);


CREATE TABLE TB_EI_CONC_DEF_LISTS (
  STAT_CONC_DEF_LIST_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);


CREATE TABLE TB_EI_FREQ_COLL (
  FREQ_COLL_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);


CREATE TABLE TB_EI_GEO_GRANULARITIES (
  GEOGRAPHIC_GRANULARITY_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);


CREATE TABLE TB_EI_INF_SUPPLIERS (
  INFORMATION_SUPPLIERS_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);


CREATE TABLE TB_EI_PRODUCERS (
  PRODUCER_FK BIGINT NOT NULL,
  TB_OPERATIONS BIGINT NOT NULL
);


CREATE TABLE TB_EI_PUBLISHERS (
  PUBLISHER_FK BIGINT NOT NULL,
  TB_OPERATIONS BIGINT NOT NULL
);


CREATE TABLE TB_EI_REG_CONTRIBUTORS (
  REGIONAL_CONTRIBUTOR_FK BIGINT NOT NULL,
  TB_OPERATIONS BIGINT NOT NULL
);


CREATE TABLE TB_EI_REG_RESPONSIBLES (
  REGIONAL_RESPONSIBLE_FK BIGINT NOT NULL,
  TB_OPERATIONS BIGINT NOT NULL
);


CREATE TABLE TB_EI_SECONDARY_AREAS (
  SECUN_SUBJECT_AREAS_FK BIGINT NOT NULL,
  TB_OPERATIONS BIGINT NOT NULL
);


CREATE TABLE TB_EI_STATISTICAL_UNITS (
  STATISTICAL_UNIT_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);


CREATE TABLE TB_EI_TIME_GRANULARITIES (
  TEMPORAL_GRANULARITY_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);


CREATE TABLE TB_EI_UNITS_MEASURE (
  UNIT_MEASURE_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);


CREATE TABLE TB_EI_UPDATE_FREQUENCY (
  UPDATE_FREQUENCY_FK BIGINT NOT NULL,
  TB_OPERATIONS BIGINT NOT NULL
);


CREATE TABLE TB_FAMILIES_OPERATIONS (
  OPERATION BIGINT NOT NULL,
  FAMILY BIGINT NOT NULL
);


CREATE TABLE TB_INSTANCES_COSTS (
  COST_FK BIGINT NOT NULL,
  TB_INSTANCES BIGINT NOT NULL
);



-- Primary keys
    
ALTER TABLE TB_INTERNATIONAL_STRINGS ADD CONSTRAINT PK_TB_INTERNATIONAL_STRINGS
    PRIMARY KEY (ID)
;

ALTER TABLE TB_LIS_COLL_METHODS ADD CONSTRAINT PK_TB_LIS_COLL_METHODS
    PRIMARY KEY (ID)
;

ALTER TABLE TB_LIS_COSTS ADD CONSTRAINT PK_TB_LIS_COSTS
    PRIMARY KEY (ID)
;

ALTER TABLE TB_FAMILIES ADD CONSTRAINT PK_TB_FAMILIES
    PRIMARY KEY (ID)
;

ALTER TABLE TB_EXTERNAL_ITEMS ADD CONSTRAINT PK_TB_EXTERNAL_ITEMS
    PRIMARY KEY (ID)
;

ALTER TABLE TB_LIS_SURVEY_TYPES ADD CONSTRAINT PK_TB_LIS_SURVEY_TYPES
    PRIMARY KEY (ID)
;

ALTER TABLE TB_LIS_OFFICIALITY_TYPES ADD CONSTRAINT PK_TB_LIS_OFFICIALITY_TYPES
    PRIMARY KEY (ID)
;

ALTER TABLE TB_OPERATIONS ADD CONSTRAINT PK_TB_OPERATIONS
    PRIMARY KEY (ID)
;

ALTER TABLE TB_LIS_INSTANCE_TYPES ADD CONSTRAINT PK_TB_LIS_INSTANCE_TYPES
    PRIMARY KEY (ID)
;

ALTER TABLE TB_LIS_SURVEY_SOURCES ADD CONSTRAINT PK_TB_LIS_SURVEY_SOURCES
    PRIMARY KEY (ID)
;

ALTER TABLE TB_INSTANCES ADD CONSTRAINT PK_TB_INSTANCES
    PRIMARY KEY (ID)
;

ALTER TABLE TB_LOCALISED_STRINGS ADD CONSTRAINT PK_TB_LOCALISED_STRINGS
    PRIMARY KEY (ID)
;

    
ALTER TABLE TB_EI_CLASS_SYSTEM_LISTS ADD CONSTRAINT PK_TB_EI_CLASS_SYSTEM_LISTS
    PRIMARY KEY (CLASS_SYSTEM_LIST_FK, TB_INSTANCES)
;

ALTER TABLE TB_EI_CONC_DEF_LISTS ADD CONSTRAINT PK_TB_EI_CONC_DEF_LISTS
    PRIMARY KEY (STAT_CONC_DEF_LIST_FK, TB_INSTANCES)
;

ALTER TABLE TB_EI_FREQ_COLL ADD CONSTRAINT PK_TB_EI_FREQ_COLL
    PRIMARY KEY (FREQ_COLL_FK, TB_INSTANCES)
;

ALTER TABLE TB_EI_GEO_GRANULARITIES ADD CONSTRAINT PK_TB_EI_GEO_GRANULARITIES
    PRIMARY KEY (GEOGRAPHIC_GRANULARITY_FK, TB_INSTANCES)
;

ALTER TABLE TB_EI_INF_SUPPLIERS ADD CONSTRAINT PK_TB_EI_INF_SUPPLIERS
    PRIMARY KEY (INFORMATION_SUPPLIERS_FK, TB_INSTANCES)
;

ALTER TABLE TB_EI_PRODUCERS ADD CONSTRAINT PK_TB_EI_PRODUCERS
    PRIMARY KEY (PRODUCER_FK, TB_OPERATIONS)
;

ALTER TABLE TB_EI_PUBLISHERS ADD CONSTRAINT PK_TB_EI_PUBLISHERS
    PRIMARY KEY (PUBLISHER_FK, TB_OPERATIONS)
;

ALTER TABLE TB_EI_REG_CONTRIBUTORS ADD CONSTRAINT PK_TB_EI_REG_CONTRIBUTORS
    PRIMARY KEY (REGIONAL_CONTRIBUTOR_FK, TB_OPERATIONS)
;

ALTER TABLE TB_EI_REG_RESPONSIBLES ADD CONSTRAINT PK_TB_EI_REG_RESPONSIBLES
    PRIMARY KEY (REGIONAL_RESPONSIBLE_FK, TB_OPERATIONS)
;

ALTER TABLE TB_EI_SECONDARY_AREAS ADD CONSTRAINT PK_TB_EI_SECONDARY_AREAS
    PRIMARY KEY (SECUN_SUBJECT_AREAS_FK, TB_OPERATIONS)
;

ALTER TABLE TB_EI_STATISTICAL_UNITS ADD CONSTRAINT PK_TB_EI_STATISTICAL_UNITS
    PRIMARY KEY (STATISTICAL_UNIT_FK, TB_INSTANCES)
;

ALTER TABLE TB_EI_TIME_GRANULARITIES ADD CONSTRAINT PK_TB_EI_TIME_GRANULARITIES
    PRIMARY KEY (TEMPORAL_GRANULARITY_FK, TB_INSTANCES)
;

ALTER TABLE TB_EI_UNITS_MEASURE ADD CONSTRAINT PK_TB_EI_UNITS_MEASURE
    PRIMARY KEY (UNIT_MEASURE_FK, TB_INSTANCES)
;

ALTER TABLE TB_EI_UPDATE_FREQUENCY ADD CONSTRAINT PK_TB_EI_UPDATE_FREQUENCY
    PRIMARY KEY (UPDATE_FREQUENCY_FK, TB_OPERATIONS)
;

ALTER TABLE TB_FAMILIES_OPERATIONS ADD CONSTRAINT PK_TB_FAMILIES_OPERATIONS
    PRIMARY KEY (OPERATION, FAMILY)
;

ALTER TABLE TB_INSTANCES_COSTS ADD CONSTRAINT PK_TB_INSTANCES_COSTS
    PRIMARY KEY (COST_FK, TB_INSTANCES)
;


-- Unique constraints

ALTER TABLE TB_INTERNATIONAL_STRINGS
    ADD CONSTRAINT UQ_TB_INTERNATIONAL_STRINGS UNIQUE (ID)
;


ALTER TABLE TB_LIS_COLL_METHODS
    ADD CONSTRAINT UQ_TB_LIS_COLL_METHODS UNIQUE (UUID)
;


ALTER TABLE TB_LIS_COSTS
    ADD CONSTRAINT UQ_TB_LIS_COSTS UNIQUE (UUID)
;


ALTER TABLE TB_FAMILIES
    ADD CONSTRAINT UQ_TB_FAMILIES UNIQUE (UUID)
;


ALTER TABLE TB_EXTERNAL_ITEMS
    ADD CONSTRAINT UQ_TB_EXTERNAL_ITEMS UNIQUE (ID)
;


ALTER TABLE TB_LIS_SURVEY_TYPES
    ADD CONSTRAINT UQ_TB_LIS_SURVEY_TYPES UNIQUE (UUID)
;


ALTER TABLE TB_LIS_OFFICIALITY_TYPES
    ADD CONSTRAINT UQ_TB_LIS_OFFICIALITY_TYPES UNIQUE (UUID)
;


ALTER TABLE TB_OPERATIONS
    ADD CONSTRAINT UQ_TB_OPERATIONS UNIQUE (UUID)
;


ALTER TABLE TB_LIS_INSTANCE_TYPES
    ADD CONSTRAINT UQ_TB_LIS_INSTANCE_TYPES UNIQUE (UUID)
;


ALTER TABLE TB_LIS_SURVEY_SOURCES
    ADD CONSTRAINT UQ_TB_LIS_SURVEY_SOURCES UNIQUE (UUID)
;


ALTER TABLE TB_INSTANCES
    ADD CONSTRAINT UQ_TB_INSTANCES UNIQUE (UUID)
;


ALTER TABLE TB_LOCALISED_STRINGS
    ADD CONSTRAINT UQ_TB_LOCALISED_STRINGS UNIQUE (ID)
;



-- Foreign key constraints
    

  
  
ALTER TABLE TB_LIS_COLL_METHODS ADD CONSTRAINT FK_TB_LIS_COLL_METHODS_DESCRIPTION
    FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LIS_COSTS ADD CONSTRAINT FK_TB_LIS_COSTS_DESCRIPTION
    FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_FAMILIES ADD CONSTRAINT FK_TB_FAMILIES_TITLE_FK
    FOREIGN KEY (TITLE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_FAMILIES ADD CONSTRAINT FK_TB_FAMILIES_ACRONYM_FK
    FOREIGN KEY (ACRONYM_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_FAMILIES ADD CONSTRAINT FK_TB_FAMILIES_DESCRIPTION_FK
    FOREIGN KEY (DESCRIPTION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_EXTERNAL_ITEMS ADD CONSTRAINT FK_TB_EXTERNAL_ITEMS_TITLE_FK
    FOREIGN KEY (TITLE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LIS_SURVEY_TYPES ADD CONSTRAINT FK_TB_LIS_SURVEY_TYPES_DESCRIPTION
    FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LIS_OFFICIALITY_TYPES ADD CONSTRAINT FK_TB_LIS_OFFICIALITY_TYPES_DESCRIPTION
    FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_COMMON_METADATA_FK
    FOREIGN KEY (COMMON_METADATA_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_TITLE_FK
    FOREIGN KEY (TITLE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_ACRONYM_FK
    FOREIGN KEY (ACRONYM_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_SUBJECT_AREA_FK
    FOREIGN KEY (SUBJECT_AREA_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_OBJECTIVE_FK
    FOREIGN KEY (OBJECTIVE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_DESCRIPTION_FK
    FOREIGN KEY (DESCRIPTION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_SURVEY_TYPE_FK
    FOREIGN KEY (SURVEY_TYPE_FK) REFERENCES TB_LIS_SURVEY_TYPES (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_OFFICIALITY_TYPE_FK
    FOREIGN KEY (OFFICIALITY_TYPE_FK) REFERENCES TB_LIS_OFFICIALITY_TYPES (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_REL_POL_US_AC_FK
    FOREIGN KEY (REL_POL_US_AC_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_REV_POLICY_FK
    FOREIGN KEY (REV_POLICY_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_REV_PRACTICE_FK
    FOREIGN KEY (REV_PRACTICE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_SPECIFIC_LEGAL_ACTS_FK
    FOREIGN KEY (SPECIFIC_LEGAL_ACTS_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_SPECIFIC_DATA_SHARING_FK
    FOREIGN KEY (SPECIFIC_DATA_SHARING_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_COMMENT_FK
    FOREIGN KEY (COMMENT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_NOTES_FK
    FOREIGN KEY (NOTES_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LIS_INSTANCE_TYPES ADD CONSTRAINT FK_TB_LIS_INSTANCE_TYPES_DESCRIPTION
    FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LIS_SURVEY_SOURCES ADD CONSTRAINT FK_TB_LIS_SURVEY_SOURCES_DESCRIPTION
    FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_TITLE_FK
    FOREIGN KEY (TITLE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_ACRONYM_FK
    FOREIGN KEY (ACRONYM_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_OPERATION_FK
    FOREIGN KEY (OPERATION_FK) REFERENCES TB_OPERATIONS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_DATA_DESCRIPTION_FK
    FOREIGN KEY (DATA_DESCRIPTION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_STATISTICAL_POPULATION_FK
    FOREIGN KEY (STATISTICAL_POPULATION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_GEOGRAPHIC_COMPARABILITY_FK
    FOREIGN KEY (GEOGRAPHIC_COMPARABILITY_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_TEMPORAL_COMPARABILITY_FK
    FOREIGN KEY (TEMPORAL_COMPARABILITY_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_STAT_CONC_DEF_FK
    FOREIGN KEY (STAT_CONC_DEF_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_CLASS_SYSTEM_FK
    FOREIGN KEY (CLASS_SYSTEM_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_INSTANCE_TYPE_FK
    FOREIGN KEY (INSTANCE_TYPE_FK) REFERENCES TB_LIS_INSTANCE_TYPES (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_DOC_METHOD_FK
    FOREIGN KEY (DOC_METHOD_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_SURVEY_SOURCE_FK
    FOREIGN KEY (SURVEY_SOURCE_FK) REFERENCES TB_LIS_SURVEY_SOURCES (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COLL_METHOD_FK
    FOREIGN KEY (COLL_METHOD_FK) REFERENCES TB_LIS_COLL_METHODS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_DATA_VALIDATION_FK
    FOREIGN KEY (DATA_VALIDATION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_DATA_COMPILATION_FK
    FOREIGN KEY (DATA_COMPILATION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_ADJUSTMENT_FK
    FOREIGN KEY (ADJUSTMENT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COST_BURDEN_FK
    FOREIGN KEY (COST_BURDEN_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_QUALITY_DOC_FK
    FOREIGN KEY (QUALITY_DOC_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_QUALITY_ASSURE_FK
    FOREIGN KEY (QUALITY_ASSURE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_QUALITY_ASSMNT_FK
    FOREIGN KEY (QUALITY_ASSMNT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_USER_NEEDS_FK
    FOREIGN KEY (USER_NEEDS_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_USER_SAT_FK
    FOREIGN KEY (USER_SAT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COMPLETENESS_FK
    FOREIGN KEY (COMPLETENESS_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_TIMELINESS_FK
    FOREIGN KEY (TIMELINESS_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_PUNCTUALITY_FK
    FOREIGN KEY (PUNCTUALITY_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_ACCURACY_OVERALL_FK
    FOREIGN KEY (ACCURACY_OVERALL_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_SAMPLING_ERR_FK
    FOREIGN KEY (SAMPLING_ERR_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_NONSAMPLING_ERR_FK
    FOREIGN KEY (NONSAMPLING_ERR_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COHER_X_DOMAIN_FK
    FOREIGN KEY (COHER_X_DOMAIN_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COHER_INTERNAL_FK
    FOREIGN KEY (COHER_INTERNAL_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COMMENT_FK
    FOREIGN KEY (COMMENT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_NOTES_FK
    FOREIGN KEY (NOTES_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LOCALISED_STRINGS ADD CONSTRAINT FK_TB_LOCALISED_STRINGS_INTERNATIONAL_STRING_FK
    FOREIGN KEY (INTERNATIONAL_STRING_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  

ALTER TABLE TB_EI_CLASS_SYSTEM_LISTS ADD CONSTRAINT FK_TB_EI_CLASS_SYSTEM_LISTS_CLASS_SYSTEM_LIST_FK
    FOREIGN KEY (CLASS_SYSTEM_LIST_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_CLASS_SYSTEM_LISTS ADD CONSTRAINT FK_TB_EI_CLASS_SYSTEM_LISTS_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_CONC_DEF_LISTS ADD CONSTRAINT FK_TB_EI_CONC_DEF_LISTS_STAT_CONC_DEF_LIST_FK
    FOREIGN KEY (STAT_CONC_DEF_LIST_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_CONC_DEF_LISTS ADD CONSTRAINT FK_TB_EI_CONC_DEF_LISTS_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_FREQ_COLL ADD CONSTRAINT FK_TB_EI_FREQ_COLL_FREQ_COLL_FK
    FOREIGN KEY (FREQ_COLL_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_FREQ_COLL ADD CONSTRAINT FK_TB_EI_FREQ_COLL_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_GEO_GRANULARITIES ADD CONSTRAINT FK_TB_EI_GEO_GRANULARITIES_GEOGRAPHIC_GRANULARITY_FK
    FOREIGN KEY (GEOGRAPHIC_GRANULARITY_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_GEO_GRANULARITIES ADD CONSTRAINT FK_TB_EI_GEO_GRANULARITIES_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_INF_SUPPLIERS ADD CONSTRAINT FK_TB_EI_INF_SUPPLIERS_INFORMATION_SUPPLIERS_FK
    FOREIGN KEY (INFORMATION_SUPPLIERS_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_INF_SUPPLIERS ADD CONSTRAINT FK_TB_EI_INF_SUPPLIERS_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_PRODUCERS ADD CONSTRAINT FK_TB_EI_PRODUCERS_PRODUCER_FK
    FOREIGN KEY (PRODUCER_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_PRODUCERS ADD CONSTRAINT FK_TB_EI_PRODUCERS_TB_OPERATIONS
    FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_PUBLISHERS ADD CONSTRAINT FK_TB_EI_PUBLISHERS_PUBLISHER_FK
    FOREIGN KEY (PUBLISHER_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_PUBLISHERS ADD CONSTRAINT FK_TB_EI_PUBLISHERS_TB_OPERATIONS
    FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_REG_CONTRIBUTORS ADD CONSTRAINT FK_TB_EI_REG_CONTRIBUTORS_REGIONAL_CONTRIBUTOR_FK
    FOREIGN KEY (REGIONAL_CONTRIBUTOR_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_REG_CONTRIBUTORS ADD CONSTRAINT FK_TB_EI_REG_CONTRIBUTORS_TB_OPERATIONS
    FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_REG_RESPONSIBLES ADD CONSTRAINT FK_TB_EI_REG_RESPONSIBLES_REGIONAL_RESPONSIBLE_FK
    FOREIGN KEY (REGIONAL_RESPONSIBLE_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_REG_RESPONSIBLES ADD CONSTRAINT FK_TB_EI_REG_RESPONSIBLES_TB_OPERATIONS
    FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_SECONDARY_AREAS ADD CONSTRAINT FK_TB_EI_SECONDARY_AREAS_SECUN_SUBJECT_AREAS_FK
    FOREIGN KEY (SECUN_SUBJECT_AREAS_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_SECONDARY_AREAS ADD CONSTRAINT FK_TB_EI_SECONDARY_AREAS_TB_OPERATIONS
    FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_STATISTICAL_UNITS ADD CONSTRAINT FK_TB_EI_STATISTICAL_UNITS_STATISTICAL_UNIT_FK
    FOREIGN KEY (STATISTICAL_UNIT_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_STATISTICAL_UNITS ADD CONSTRAINT FK_TB_EI_STATISTICAL_UNITS_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_TIME_GRANULARITIES ADD CONSTRAINT FK_TB_EI_TIME_GRANULARITIES_TEMPORAL_GRANULARITY_FK
    FOREIGN KEY (TEMPORAL_GRANULARITY_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_TIME_GRANULARITIES ADD CONSTRAINT FK_TB_EI_TIME_GRANULARITIES_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_UNITS_MEASURE ADD CONSTRAINT FK_TB_EI_UNITS_MEASURE_UNIT_MEASURE_FK
    FOREIGN KEY (UNIT_MEASURE_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_UNITS_MEASURE ADD CONSTRAINT FK_TB_EI_UNITS_MEASURE_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_UPDATE_FREQUENCY ADD CONSTRAINT FK_TB_EI_UPDATE_FREQUENCY_UPDATE_FREQUENCY_FK
    FOREIGN KEY (UPDATE_FREQUENCY_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_UPDATE_FREQUENCY ADD CONSTRAINT FK_TB_EI_UPDATE_FREQUENCY_TB_OPERATIONS
    FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_FAMILIES_OPERATIONS ADD CONSTRAINT FK_TB_FAMILIES_OPERATIONS_OPERATION
    FOREIGN KEY (OPERATION) REFERENCES TB_OPERATIONS (ID)
;
ALTER TABLE TB_FAMILIES_OPERATIONS ADD CONSTRAINT FK_TB_FAMILIES_OPERATIONS_FAMILY
    FOREIGN KEY (FAMILY) REFERENCES TB_FAMILIES (ID)
;

  
ALTER TABLE TB_INSTANCES_COSTS ADD CONSTRAINT FK_TB_INSTANCES_COSTS_COST_FK
    FOREIGN KEY (COST_FK) REFERENCES TB_LIS_COSTS (ID)
;
ALTER TABLE TB_INSTANCES_COSTS ADD CONSTRAINT FK_TB_INSTANCES_COSTS_TB_INSTANCES
    FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  

    


