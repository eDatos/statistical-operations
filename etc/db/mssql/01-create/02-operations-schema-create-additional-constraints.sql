create table TB_SEQUENCES (
	SEQUENCE_NAME VARCHAR(255) NOT NULL,
	SEQUENCE_NEXT_VALUE BIGINT
);

ALTER TABLE TB_SEQUENCES ADD CONSTRAINT PK_TB_SEQUENCES
	PRIMARY KEY (SEQUENCE_NAME)
;

ALTER TABLE TB_FAMILIES ADD CONSTRAINT FAMILIES_CODE UNIQUE(CODE);
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT OPERATIONS_CODE UNIQUE(CODE);
ALTER TABLE TB_INSTANCES ADD CONSTRAINT INSTANCES_CODE UNIQUE(CODE);
ALTER TABLE TB_LOCALISED_STRINGS ADD CONSTRAINT LOCALE_INTERNATIONAL UNIQUE(LOCALE, INTERNATIONAL_STRING_FK);