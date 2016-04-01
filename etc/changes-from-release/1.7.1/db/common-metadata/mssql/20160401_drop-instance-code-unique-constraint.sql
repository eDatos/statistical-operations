-- --------------------------------------------------------------------------------------------------
-- METAMAC-2447 - Las instancias deben comprobar si el código es único dentro de una OOEE
-- --------------------------------------------------------------------------------------------------

-- Eliminar unique constraint para el código de las operaciones estadísticas

ALTER TABLE FILL_WITH_SCHEMA_NAME.TB_OPERATIONS DROP CONSTRAINT OPERATIONS_CODE