-- --------------------------------------------------------------------------------------
-- METAMAC-2347 - [OOEE] Las operaciones deben tener un código de como máximo 11 dígitos
-- --------------------------------------------------------------------------------------
ALTER TABLE TB_OPERATIONS ALTER COLUMN CODE NVARCHAR(11);