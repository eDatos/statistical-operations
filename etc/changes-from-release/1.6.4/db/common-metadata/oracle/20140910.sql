-- --------------------------------------------------------------------------------------
-- METAMAC-2347 - [OOEE] Las operaciones deben tener un código de como máximo 11 dígitos
-- --------------------------------------------------------------------------------------
ALTER TABLE TB_OPERATIONS MODIFY CODE VARCHAR2(11 CHAR);