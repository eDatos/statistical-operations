ALTER TABLE TB_FAMILIES ADD CONSTRAINT FAMILIES_CODE UNIQUE(CODE);
ALTER TABLE TB_OPERATIONS ADD CONSTRAINT OPERATIONS_CODE UNIQUE(CODE);
ALTER TABLE TB_LOCALISED_STRINGS ADD CONSTRAINT LOCALE_INTERNATIONAL UNIQUE(LOCALE, INTERNATIONAL_STRING_FK);