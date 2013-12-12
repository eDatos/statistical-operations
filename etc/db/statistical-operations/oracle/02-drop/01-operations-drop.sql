-- ###########################################
-- # Drop
-- ###########################################
-- Drop index


-- Drop many to many relations
    
DROP TABLE TB_INSTANCES_COSTS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_FAMILIES_OPERATIONS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_UPDATE_FREQUENCY CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_UNITS_MEASURE CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_TIME_GRANULARITIES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_STATISTICAL_UNITS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_SECONDARY_AREAS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_REG_RESPONSIBLES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_REG_CONTRIBUTORS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_PUBLISHERS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_PRODUCERS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_INF_SUPPLIERS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_GEO_GRANULARITIES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_FREQ_COLL CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_CONC_DEF_LISTS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EI_CLASS_SYSTEM_LISTS CASCADE CONSTRAINTS PURGE;

-- Drop normal entities
    
DROP TABLE TB_LOCALISED_STRINGS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_INSTANCES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_LIS_SURVEY_SOURCES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_LIS_INSTANCE_TYPES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_OPERATIONS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_LIS_OFFICIALITY_TYPES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_LIS_SURVEY_TYPES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_EXTERNAL_ITEMS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_FAMILIES CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_LIS_COSTS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_LIS_COLL_METHODS CASCADE CONSTRAINTS PURGE;

DROP TABLE TB_INTERNATIONAL_STRINGS CASCADE CONSTRAINTS PURGE;


-- Drop pk sequence

-- Drop sequences

drop sequence SEQ_I18NSTRS;
drop sequence SEQ_L10NSTRS;

drop sequence SEQ_EXTERNAL_ITEMS;

drop sequence SEQ_COMMON_METADATA;

drop sequence SEQ_FAMILIES;
drop sequence SEQ_OPERATIONS;
drop sequence SEQ_INSTANCES;

drop sequence SEQ_SURVEY_TYPES;
drop sequence SEQ_INSTANCE_TYPES;
drop sequence SEQ_ACTIVITY_CLASSES;
drop sequence SEQ_SURVEY_SOURCES;
drop sequence SEQ_OFFICIALITY_TYPES;
drop sequence SEQ_COLL_METHODS;
drop sequence SEQ_COSTS;