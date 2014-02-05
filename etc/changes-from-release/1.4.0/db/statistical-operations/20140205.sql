ALTER TABLE TB_LIS_COLL_METHODS MODIFY(
  IDENTIFIER VARCHAR2(255 CHAR),
  UUID VARCHAR2(36 CHAR)
);

alter TABLE TB_LIS_COSTS modify (
  IDENTIFIER VARCHAR2(255 CHAR),
  UUID VARCHAR2(36 CHAR)
);

ALTER TABLE TB_FAMILIES MODIFY (
  CODE VARCHAR2(255 CHAR),
  URN VARCHAR2(4000 CHAR),
  INTERNAL_INVENTORY_DATE_TZ VARCHAR2(50 CHAR),
  INVENTORY_DATE_TZ VARCHAR2(50 CHAR),
  UPDATE_DATE_TZ VARCHAR2(50 CHAR),
  UUID VARCHAR2(36 CHAR),
  CREATED_DATE_TZ VARCHAR2(50 CHAR),
  CREATED_BY VARCHAR2(50 CHAR),
  LAST_UPDATED_TZ VARCHAR2(50 CHAR),
  LAST_UPDATED_BY VARCHAR2(50 CHAR),
  PROC_STATUS VARCHAR2(255 CHAR)
);

ALTER TABLE TB_EXTERNAL_ITEMS MODIFY (
  CODE VARCHAR2(255 CHAR),
  CODE_NESTED VARCHAR2(255 CHAR),
  URI VARCHAR2(4000 CHAR),
  URN VARCHAR2(4000 CHAR),
  MANAGEMENT_APP_URL VARCHAR2(4000 CHAR),
  TYPE VARCHAR2(255 CHAR)
);


ALTER TABLE TB_LIS_SURVEY_TYPES MODIFY (
  IDENTIFIER VARCHAR2(255 CHAR),
  UUID VARCHAR2(36 CHAR)
);

ALTER TABLE TB_LIS_OFFICIALITY_TYPES MODIFY (
  IDENTIFIER VARCHAR2(255 CHAR),
  UUID VARCHAR2(36 CHAR)
);

ALTER TABLE TB_OPERATIONS MODIFY (
  CODE VARCHAR2(255 CHAR),
  URN VARCHAR2(4000 CHAR),
  INTERNAL_INVENTORY_DATE_TZ VARCHAR2(50 CHAR),
  RELEASE_CALENDAR_ACCESS VARCHAR2(4000 CHAR),
  INVENTORY_DATE_TZ VARCHAR2(50 CHAR),
  UPDATE_DATE_TZ VARCHAR2(50 CHAR),
  UUID VARCHAR2(36 CHAR),
  CREATED_DATE_TZ VARCHAR2(50 CHAR),
  CREATED_BY VARCHAR2(50 CHAR),
  LAST_UPDATED_TZ VARCHAR2(50 CHAR),
  LAST_UPDATED_BY VARCHAR2(50 CHAR),
  PROC_STATUS VARCHAR2(255 CHAR),
  STATUS VARCHAR2(255 CHAR)
);



ALTER TABLE TB_LIS_INSTANCE_TYPES MODIFY (
  IDENTIFIER VARCHAR2(255 CHAR),
  UUID VARCHAR2(36 CHAR)
);


ALTER TABLE TB_LIS_SURVEY_SOURCES MODIFY (
  IDENTIFIER VARCHAR2(255 CHAR),
  UUID VARCHAR2(36 CHAR)
);


ALTER TABLE TB_INSTANCES MODIFY (
  CODE VARCHAR2(255 CHAR),
  URN VARCHAR2(4000 CHAR),
  BASE_PERIOD VARCHAR2(255 CHAR),
  INTERNAL_INVENTORY_DATE_TZ VARCHAR2(50 CHAR),
  INVENTORY_DATE_TZ VARCHAR2(50 CHAR),
  UPDATE_DATE_TZ VARCHAR2(50 CHAR),
  UUID VARCHAR2(36 CHAR),
  CREATED_DATE_TZ VARCHAR2(50 CHAR),
  CREATED_BY VARCHAR2(50 CHAR),
  LAST_UPDATED_TZ VARCHAR2(50 CHAR),
  LAST_UPDATED_BY VARCHAR2(50 CHAR),
  PROC_STATUS VARCHAR2(255 CHAR)
);


ALTER TABLE TB_LOCALISED_STRINGS MODIFY (
  LABEL VARCHAR2(4000 CHAR),
  LOCALE VARCHAR2(255 CHAR)
);

alter table TB_SEQUENCES modify (
	SEQUENCE_NAME VARCHAR2(255 CHAR)
);
