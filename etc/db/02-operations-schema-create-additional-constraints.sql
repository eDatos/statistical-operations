ALTER TABLE TBL_FAMILIES ADD CONSTRAINT FAMILIES_CODE UNIQUE(CODE) DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE TBL_OPERATIONS ADD CONSTRAINT OPERATIONS_CODE UNIQUE(CODE) DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE TBL_INSTANCES ADD CONSTRAINT INSTANCES_CODE UNIQUE(CODE) DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE TBL_LOCALISED_STRINGS ADD CONSTRAINT LOCALE_INTERNATIONAL UNIQUE(LOCALE, INTERNATIONAL_STRING_FK) DEFERRABLE INITIALLY DEFERRED;