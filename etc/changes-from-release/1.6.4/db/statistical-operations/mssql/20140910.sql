-- --------------------------------------------------------------------------------------
-- METAMAC-2347 - [OOEE] Las operaciones deben tener un código de como máximo 11 dígitos
-- --------------------------------------------------------------------------------------

-- Se debe comprobar primero si hay alguna reestricción que haga uso de la columna a modificar 

IF EXISTS (
  SELECT * FROM sys.objects WHERE NAME = 'OPERATIONS_CODE'
)
ALTER TABLE FILL_WITH_SCHEMA_NAME.TB_OPERATIONS DROP CONSTRAINT OPERATIONS_CODE
GO
ALTER TABLE FILL_WITH_SCHEMA_NAME.TB_OPERATIONS ALTER COLUMN CODE NVARCHAR(11)
GO
ALTER TABLE FILL_WITH_SCHEMA_NAME.TB_OPERATIONS
  ADD CONSTRAINT OPERATIONS_CODE
  UNIQUE(CODE)