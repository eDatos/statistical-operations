-- ###########################################
-- # Create
-- ###########################################
-- Create pk sequence
    


-- Create normal entities
    
CREATE TABLE TB_INTERNATIONAL_STRINGS (
  ID NUMBER(19) NOT NULL,
  VERSION NUMBER(19) NOT NULL
);


CREATE TABLE TB_LIS_COLL_METHODS (
  ID NUMBER(19) NOT NULL,
  IDENTIFIER VARCHAR2(255) NOT NULL,
  UUID VARCHAR2(36) NOT NULL,
  VERSION NUMBER(19) NOT NULL,
  DESCRIPTION NUMBER(19)
);


CREATE TABLE TB_LIS_COSTS (
  ID NUMBER(19) NOT NULL,
  IDENTIFIER VARCHAR2(255) NOT NULL,
  UUID VARCHAR2(36) NOT NULL,
  VERSION NUMBER(19) NOT NULL,
  DESCRIPTION NUMBER(19)
);


CREATE TABLE TB_FAMILIES (
  ID NUMBER(19) NOT NULL,
  CODE VARCHAR2(255) NOT NULL,
  INTERNAL_INVENTORY_DATE_TZ VARCHAR2(50)
  ,
  INTERNAL_INVENTORY_DATE TIMESTAMP ,
  INVENTORY_DATE_TZ VARCHAR2(50)
  ,
  INVENTORY_DATE TIMESTAMP ,
  UUID VARCHAR2(36) NOT NULL,
  CREATED_DATE_TZ VARCHAR2(50)
  ,
  CREATED_DATE TIMESTAMP,
  CREATED_BY VARCHAR2(50),
  LAST_UPDATED_TZ VARCHAR2(50)
  ,
  LAST_UPDATED TIMESTAMP,
  LAST_UPDATED_BY VARCHAR2(50),
  VERSION NUMBER(19) NOT NULL,
  TITLE_FK NUMBER(19) NOT NULL,
  ACRONYM_FK NUMBER(19),
  DESCRIPTION_FK NUMBER(19),
  PROC_STATUS VARCHAR2(40) NOT NULL
);


CREATE TABLE TB_LIS_SURVEY_TYPES (
  ID NUMBER(19) NOT NULL,
  IDENTIFIER VARCHAR2(255) NOT NULL,
  UUID VARCHAR2(36) NOT NULL,
  VERSION NUMBER(19) NOT NULL,
  DESCRIPTION NUMBER(19)
);


CREATE TABLE TB_LIS_OFFICIALITY_TYPES (
  ID NUMBER(19) NOT NULL,
  IDENTIFIER VARCHAR2(255) NOT NULL,
  UUID VARCHAR2(36) NOT NULL,
  VERSION NUMBER(19) NOT NULL,
  DESCRIPTION NUMBER(19)
);


CREATE TABLE TB_OPERATIONS (
  ID NUMBER(19) NOT NULL,
  CODE VARCHAR2(255) NOT NULL,
  INDICATOR_SYSTEM NUMBER(1,0) NOT NULL,
  INTERNAL_INVENTORY_DATE_TZ VARCHAR2(50)
  ,
  INTERNAL_INVENTORY_DATE TIMESTAMP ,
  CURRENTLY_ACTIVE NUMBER(1,0) NOT NULL,
  REL_POL_US_AC_URL VARCHAR2(4000),
  RELEASE_CALENDAR NUMBER(1,0) NOT NULL,
  RELEASE_CALENDAR_ACCESS VARCHAR2(4000),
  INVENTORY_DATE_TZ VARCHAR2(50)
  ,
  INVENTORY_DATE TIMESTAMP ,
  REV_POLICY_URL VARCHAR2(4000),
  REV_PRACTICE_URL VARCHAR2(4000),
  COMMENT_URL VARCHAR2(4000),
  NOTES_URL VARCHAR2(4000),
  UUID VARCHAR2(36) NOT NULL,
  CREATED_DATE_TZ VARCHAR2(50)
  ,
  CREATED_DATE TIMESTAMP,
  CREATED_BY VARCHAR2(50),
  LAST_UPDATED_TZ VARCHAR2(50)
  ,
  LAST_UPDATED TIMESTAMP,
  LAST_UPDATED_BY VARCHAR2(50),
  VERSION NUMBER(19) NOT NULL,
  TITLE_FK NUMBER(19) NOT NULL,
  ACRONYM_FK NUMBER(19),
  OBJECTIVE_FK NUMBER(19),
  DESCRIPTION_FK NUMBER(19),
  SURVEY_TYPE_FK NUMBER(19),
  OFFICIALITY_TYPE_FK NUMBER(19),
  REL_POL_US_AC_FK NUMBER(19),
  REV_POLICY_FK NUMBER(19),
  REV_PRACTICE_FK NUMBER(19),
  COMMENT_FK NUMBER(19),
  NOTES_FK NUMBER(19),
  COMMON_METADATA_URI_INT VARCHAR2(255),
  COMMON_METADATA_CODE_ID VARCHAR2(255),
  COMMON_METADATA_TYPE VARCHAR2(40),
  SUBJECT_AREA_URI_INT VARCHAR2(255),
  SUBJECT_AREA_CODE_ID VARCHAR2(255),
  SUBJECT_AREA_TYPE VARCHAR2(40),
  PROC_STATUS VARCHAR2(40) NOT NULL,
  STATUS VARCHAR2(40) NOT NULL
);


CREATE TABLE TB_LIS_INSTANCE_TYPES (
  ID NUMBER(19) NOT NULL,
  IDENTIFIER VARCHAR2(255) NOT NULL,
  UUID VARCHAR2(36) NOT NULL,
  VERSION NUMBER(19) NOT NULL,
  DESCRIPTION NUMBER(19)
);


CREATE TABLE TB_LIS_SURVEY_SOURCES (
  ID NUMBER(19) NOT NULL,
  IDENTIFIER VARCHAR2(255) NOT NULL,
  UUID VARCHAR2(36) NOT NULL,
  VERSION NUMBER(19) NOT NULL,
  DESCRIPTION NUMBER(19)
);


CREATE TABLE TB_INSTANCES (
  ID NUMBER(19) NOT NULL,
  ORDER_IDX NUMBER(10) NOT NULL,
  CODE VARCHAR2(255) NOT NULL,
  BASE_PERIOD VARCHAR2(255),
  INTERNAL_INVENTORY_DATE_TZ VARCHAR2(50)
  ,
  INTERNAL_INVENTORY_DATE TIMESTAMP ,
  DOC_METHOD_URL VARCHAR2(4000),
  DATA_VALIDATION_URL VARCHAR2(4000),
  DATA_COMPILATION_URL VARCHAR2(4000),
  ADJUSTMENT_URL VARCHAR2(4000),
  COST_BURDEN_URL VARCHAR2(4000),
  INVENTORY_DATE_TZ VARCHAR2(50)
  ,
  INVENTORY_DATE TIMESTAMP ,
  QUALITY_DOC_URL VARCHAR2(4000),
  QUALITY_ASSURE_URL VARCHAR2(4000),
  QUALITY_ASSMNT_URL VARCHAR2(4000),
  USER_NEEDS_URL VARCHAR2(4000),
  USER_SAT_URL VARCHAR2(4000),
  COMPLETENESS_URL VARCHAR2(4000),
  TIMELINESS_URL VARCHAR2(4000),
  PUNCTUALITY_URL VARCHAR2(4000),
  ACCURACY_OVERALL_URL VARCHAR2(4000),
  SAMPLING_ERR_URL VARCHAR2(4000),
  NONSAMPLING_ERR_URL VARCHAR2(4000),
  COHER_X_DOMAIN_URL VARCHAR2(4000),
  COHER_INTERNAL_URL VARCHAR2(4000),
  COMMENT_URL VARCHAR2(4000),
  NOTES_URL VARCHAR2(4000),
  UUID VARCHAR2(36) NOT NULL,
  CREATED_DATE_TZ VARCHAR2(50)
  ,
  CREATED_DATE TIMESTAMP,
  CREATED_BY VARCHAR2(50),
  LAST_UPDATED_TZ VARCHAR2(50)
  ,
  LAST_UPDATED TIMESTAMP,
  LAST_UPDATED_BY VARCHAR2(50),
  VERSION NUMBER(19) NOT NULL,
  TITLE_FK NUMBER(19) NOT NULL,
  ACRONYM_FK NUMBER(19),
  OPERATION_FK NUMBER(19) NOT NULL,
  DATA_DESCRIPTION_FK NUMBER(19),
  STATISTICAL_POPULATION_FK NUMBER(19),
  GEOGRAPHIC_COMPARABILITY_FK NUMBER(19),
  TEMPORAL_COMPARABILITY_FK NUMBER(19),
  STAT_CONC_DEF_FK NUMBER(19),
  CLASS_SYSTEM_FK NUMBER(19),
  INSTANCE_TYPE_FK NUMBER(19),
  DOC_METHOD_FK NUMBER(19),
  SURVEY_SOURCE_FK NUMBER(19),
  COLL_METHOD_FK NUMBER(19),
  DATA_VALIDATION_FK NUMBER(19),
  DATA_COMPILATION_FK NUMBER(19),
  ADJUSTMENT_FK NUMBER(19),
  COST_BURDEN_FK NUMBER(19),
  QUALITY_DOC_FK NUMBER(19),
  QUALITY_ASSURE_FK NUMBER(19),
  QUALITY_ASSMNT_FK NUMBER(19),
  USER_NEEDS_FK NUMBER(19),
  USER_SAT_FK NUMBER(19),
  COMPLETENESS_FK NUMBER(19),
  TIMELINESS_FK NUMBER(19),
  PUNCTUALITY_FK NUMBER(19),
  ACCURACY_OVERALL_FK NUMBER(19),
  SAMPLING_ERR_FK NUMBER(19),
  NONSAMPLING_ERR_FK NUMBER(19),
  COHER_X_DOMAIN_FK NUMBER(19),
  COHER_INTERNAL_FK NUMBER(19),
  COMMENT_FK NUMBER(19),
  NOTES_FK NUMBER(19),
  GEOGRAPHIC_GRANULARITY_URI_INT VARCHAR2(255),
  GEOGRAPHIC_GRANULARITY_CODE_ID VARCHAR2(255),
  GEOGRAPHIC_GRANULARITY_TYPE VARCHAR2(40),
  TEMPORAL_GRANULARITY_URI_INT VARCHAR2(255),
  TEMPORAL_GRANULARITY_CODE_ID VARCHAR2(255),
  TEMPORAL_GRANULARITY_TYPE VARCHAR2(40),
  PROC_STATUS VARCHAR2(40) NOT NULL
);


CREATE TABLE TB_LOCALISED_STRINGS (
  ID NUMBER(19) NOT NULL,
  LABEL VARCHAR2(4000) NOT NULL,
  LOCALE VARCHAR2(255) NOT NULL,
  VERSION NUMBER(19) NOT NULL,
  INTERNATIONAL_STRING_FK NUMBER(19)
);


CREATE TABLE TB_EXTERNAL_ITEMS (
  ID NUMBER(19) NOT NULL,
  UUID VARCHAR2(36) NOT NULL,
  EXT_URI_INT VARCHAR2(255) NOT NULL,
  EXT_CODE_ID VARCHAR2(255) NOT NULL,
  EXT_TYPE VARCHAR2(40) NOT NULL
);



-- Create many to many relations
    
CREATE TABLE TB_EI_CLASS_SYSTEM_LISTS (
  CLASS_SYSTEM_LIST_FK NUMBER(19) NOT NULL,
  TB_INSTANCES NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_CONC_DEF_LISTS (
  STAT_CONC_DEF_LIST_FK NUMBER(19) NOT NULL,
  TB_INSTANCES NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_FREQ_COLL (
  FREQ_COLL_FK NUMBER(19) NOT NULL,
  TB_INSTANCES NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_INF_SUPPLIERS (
  INFORMATION_SUPPLIERS_FK NUMBER(19) NOT NULL,
  TB_INSTANCES NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_PRODUCERS (
  PRODUCER_FK NUMBER(19) NOT NULL,
  TB_OPERATIONS NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_PUBLISHERS (
  PUBLISHER_FK NUMBER(19) NOT NULL,
  TB_OPERATIONS NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_REG_CONTRIBUTORS (
  REGIONAL_CONTRIBUTOR_FK NUMBER(19) NOT NULL,
  TB_OPERATIONS NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_REG_RESPONSIBLES (
  REGIONAL_RESPONSIBLE_FK NUMBER(19) NOT NULL,
  TB_OPERATIONS NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_SECONDARY_AREAS (
  SECUN_SUBJECT_AREAS_FK NUMBER(19) NOT NULL,
  TB_OPERATIONS NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_STATISTICAL_UNITS (
  STATISTICAL_UNIT_FK NUMBER(19) NOT NULL,
  TB_INSTANCES NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_UNITS_MEASURE (
  UNIT_MEASURE_FK NUMBER(19) NOT NULL,
  TB_INSTANCES NUMBER(19) NOT NULL
);


CREATE TABLE TB_EI_UPDATE_FREQUENCY (
  UPDATE_FREQUENCY_FK NUMBER(19) NOT NULL,
  TB_OPERATIONS NUMBER(19) NOT NULL
);


CREATE TABLE TB_FAMILIES_OPERATIONS (
  OPERATION NUMBER(19) NOT NULL,
  FAMILY NUMBER(19) NOT NULL
);


CREATE TABLE TB_INSTANCES_COSTS (
  COST_FK NUMBER(19) NOT NULL,
  TB_INSTANCES NUMBER(19) NOT NULL
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

ALTER TABLE TB_EXTERNAL_ITEMS ADD CONSTRAINT PK_TB_EXTERNAL_ITEMS
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
     
 

ALTER TABLE TB_LIS_COLL_METHODS
    ADD CONSTRAINT UQ_TB_LIS_COLL_METHODS UNIQUE (UUID)
;

 

ALTER TABLE TB_LIS_COSTS
    ADD CONSTRAINT UQ_TB_LIS_COSTS UNIQUE (UUID)
;

 

ALTER TABLE TB_FAMILIES
    ADD CONSTRAINT UQ_TB_FAMILIES UNIQUE (UUID)
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

 
 

ALTER TABLE TB_EXTERNAL_ITEMS
    ADD CONSTRAINT UQ_TB_EXTERNAL_ITEMS UNIQUE (UUID)
;



-- Foreign key constraints
    

  
ALTER TABLE TB_LOCALISED_STRINGS ADD CONSTRAINT FK_TB_LOCALISED_STRINGS_TB_I97
	FOREIGN KEY (INTERNATIONAL_STRING_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

ALTER TABLE TB_LIS_COLL_METHODS ADD CONSTRAINT FK_TB_LIS_COLL_METHODS_DESCR66
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

  
ALTER TABLE TB_LIS_SURVEY_TYPES ADD CONSTRAINT FK_TB_LIS_SURVEY_TYPES_DESCR37
	FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LIS_OFFICIALITY_TYPES ADD CONSTRAINT FK_TB_LIS_OFFICIALITY_TYPES_28
	FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_TITLE_FK
	FOREIGN KEY (TITLE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_ACRONYM_FK
	FOREIGN KEY (ACRONYM_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_OBJECTIVE_FK
	FOREIGN KEY (OBJECTIVE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_DESCRIPTION82
	FOREIGN KEY (DESCRIPTION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_SURVEY_TYPE11
	FOREIGN KEY (SURVEY_TYPE_FK) REFERENCES TB_LIS_SURVEY_TYPES (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_OFFICIALITY28
	FOREIGN KEY (OFFICIALITY_TYPE_FK) REFERENCES TB_LIS_OFFICIALITY_TYPES (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_REL_POL_US_51
	FOREIGN KEY (REL_POL_US_AC_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_REV_POLICY_FK
	FOREIGN KEY (REV_POLICY_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_REV_PRACTIC99
	FOREIGN KEY (REV_PRACTICE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_COMMENT_FK
	FOREIGN KEY (COMMENT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT FK_TB_OPERATIONS_NOTES_FK
	FOREIGN KEY (NOTES_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LIS_INSTANCE_TYPES ADD CONSTRAINT FK_TB_LIS_INSTANCE_TYPES_DES38
	FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_LIS_SURVEY_SOURCES ADD CONSTRAINT FK_TB_LIS_SURVEY_SOURCES_DES46
	FOREIGN KEY (DESCRIPTION) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_TITLE_FK
	FOREIGN KEY (TITLE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_ACRONYM_FK
	FOREIGN KEY (ACRONYM_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_OPERATION_FK
	FOREIGN KEY (OPERATION_FK) REFERENCES TB_OPERATIONS (ID) ON DELETE CASCADE
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_DATA_DESCRIP53
	FOREIGN KEY (DATA_DESCRIPTION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_STATISTICAL_99
	FOREIGN KEY (STATISTICAL_POPULATION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_GEOGRAPHIC_C80
	FOREIGN KEY (GEOGRAPHIC_COMPARABILITY_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_TEMPORAL_COM97
	FOREIGN KEY (TEMPORAL_COMPARABILITY_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_STAT_CONC_DE76
	FOREIGN KEY (STAT_CONC_DEF_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_CLASS_SYSTEM68
	FOREIGN KEY (CLASS_SYSTEM_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_INSTANCE_TYP70
	FOREIGN KEY (INSTANCE_TYPE_FK) REFERENCES TB_LIS_INSTANCE_TYPES (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_DOC_METHOD_FK
	FOREIGN KEY (DOC_METHOD_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_SURVEY_SOURC94
	FOREIGN KEY (SURVEY_SOURCE_FK) REFERENCES TB_LIS_SURVEY_SOURCES (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COLL_METHOD_FK
	FOREIGN KEY (COLL_METHOD_FK) REFERENCES TB_LIS_COLL_METHODS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_DATA_VALIDAT76
	FOREIGN KEY (DATA_VALIDATION_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_DATA_COMPILA00
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
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_QUALITY_ASSU87
	FOREIGN KEY (QUALITY_ASSURE_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_QUALITY_ASSM60
	FOREIGN KEY (QUALITY_ASSMNT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_USER_NEEDS_FK
	FOREIGN KEY (USER_NEEDS_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_USER_SAT_FK
	FOREIGN KEY (USER_SAT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COMPLETENESS74
	FOREIGN KEY (COMPLETENESS_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_TIMELINESS_FK
	FOREIGN KEY (TIMELINESS_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_PUNCTUALITY_FK
	FOREIGN KEY (PUNCTUALITY_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_ACCURACY_OVE59
	FOREIGN KEY (ACCURACY_OVERALL_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_SAMPLING_ERR17
	FOREIGN KEY (SAMPLING_ERR_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_NONSAMPLING_24
	FOREIGN KEY (NONSAMPLING_ERR_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COHER_X_DOMA87
	FOREIGN KEY (COHER_X_DOMAIN_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COHER_INTERN31
	FOREIGN KEY (COHER_INTERNAL_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_COMMENT_FK
	FOREIGN KEY (COMMENT_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;
ALTER TABLE TB_INSTANCES ADD CONSTRAINT FK_TB_INSTANCES_NOTES_FK
	FOREIGN KEY (NOTES_FK) REFERENCES TB_INTERNATIONAL_STRINGS (ID)
;

  
  
  
  
  

ALTER TABLE TB_EI_CLASS_SYSTEM_LISTS ADD CONSTRAINT FK_TB_EI_CLASS_SYSTEM_LISTS_90
	FOREIGN KEY (CLASS_SYSTEM_LIST_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_CLASS_SYSTEM_LISTS ADD CONSTRAINT FK_TB_EI_CLASS_SYSTEM_LISTS_54
	FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_CONC_DEF_LISTS ADD CONSTRAINT FK_TB_EI_CONC_DEF_LISTS_STAT05
	FOREIGN KEY (STAT_CONC_DEF_LIST_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_CONC_DEF_LISTS ADD CONSTRAINT FK_TB_EI_CONC_DEF_LISTS_TB_I87
	FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_FREQ_COLL ADD CONSTRAINT FK_TB_EI_FREQ_COLL_FREQ_COLL47
	FOREIGN KEY (FREQ_COLL_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_FREQ_COLL ADD CONSTRAINT FK_TB_EI_FREQ_COLL_TB_INSTAN69
	FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_INF_SUPPLIERS ADD CONSTRAINT FK_TB_EI_INF_SUPPLIERS_INFOR28
	FOREIGN KEY (INFORMATION_SUPPLIERS_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_INF_SUPPLIERS ADD CONSTRAINT FK_TB_EI_INF_SUPPLIERS_TB_IN63
	FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_PRODUCERS ADD CONSTRAINT FK_TB_EI_PRODUCERS_PRODUCER_FK
	FOREIGN KEY (PRODUCER_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_PRODUCERS ADD CONSTRAINT FK_TB_EI_PRODUCERS_TB_OPERAT43
	FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_PUBLISHERS ADD CONSTRAINT FK_TB_EI_PUBLISHERS_PUBLISHE66
	FOREIGN KEY (PUBLISHER_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_PUBLISHERS ADD CONSTRAINT FK_TB_EI_PUBLISHERS_TB_OPERA01
	FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_REG_CONTRIBUTORS ADD CONSTRAINT FK_TB_EI_REG_CONTRIBUTORS_RE45
	FOREIGN KEY (REGIONAL_CONTRIBUTOR_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_REG_CONTRIBUTORS ADD CONSTRAINT FK_TB_EI_REG_CONTRIBUTORS_TB45
	FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_REG_RESPONSIBLES ADD CONSTRAINT FK_TB_EI_REG_RESPONSIBLES_RE05
	FOREIGN KEY (REGIONAL_RESPONSIBLE_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_REG_RESPONSIBLES ADD CONSTRAINT FK_TB_EI_REG_RESPONSIBLES_TB64
	FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_SECONDARY_AREAS ADD CONSTRAINT FK_TB_EI_SECONDARY_AREAS_SEC28
	FOREIGN KEY (SECUN_SUBJECT_AREAS_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_SECONDARY_AREAS ADD CONSTRAINT FK_TB_EI_SECONDARY_AREAS_TB_11
	FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_EI_STATISTICAL_UNITS ADD CONSTRAINT FK_TB_EI_STATISTICAL_UNITS_S62
	FOREIGN KEY (STATISTICAL_UNIT_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_STATISTICAL_UNITS ADD CONSTRAINT FK_TB_EI_STATISTICAL_UNITS_T73
	FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_UNITS_MEASURE ADD CONSTRAINT FK_TB_EI_UNITS_MEASURE_UNIT_50
	FOREIGN KEY (UNIT_MEASURE_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_UNITS_MEASURE ADD CONSTRAINT FK_TB_EI_UNITS_MEASURE_TB_IN48
	FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  
ALTER TABLE TB_EI_UPDATE_FREQUENCY ADD CONSTRAINT FK_TB_EI_UPDATE_FREQUENCY_UP11
	FOREIGN KEY (UPDATE_FREQUENCY_FK) REFERENCES TB_EXTERNAL_ITEMS (ID)
;
ALTER TABLE TB_EI_UPDATE_FREQUENCY ADD CONSTRAINT FK_TB_EI_UPDATE_FREQUENCY_TB74
	FOREIGN KEY (TB_OPERATIONS) REFERENCES TB_OPERATIONS (ID)
;

  
ALTER TABLE TB_FAMILIES_OPERATIONS ADD CONSTRAINT FK_TB_FAMILIES_OPERATIONS_OP00
	FOREIGN KEY (OPERATION) REFERENCES TB_OPERATIONS (ID)
;
ALTER TABLE TB_FAMILIES_OPERATIONS ADD CONSTRAINT FK_TB_FAMILIES_OPERATIONS_FA19
	FOREIGN KEY (FAMILY) REFERENCES TB_FAMILIES (ID)
;

  
ALTER TABLE TB_INSTANCES_COSTS ADD CONSTRAINT FK_TB_INSTANCES_COSTS_COST_FK
	FOREIGN KEY (COST_FK) REFERENCES TB_LIS_COSTS (ID)
;
ALTER TABLE TB_INSTANCES_COSTS ADD CONSTRAINT FK_TB_INSTANCES_COSTS_TB_INS32
	FOREIGN KEY (TB_INSTANCES) REFERENCES TB_INSTANCES (ID)
;

  


-- Index
  
  
  
  
  


