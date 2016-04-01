-- --------------------------------------------------------------------------------------------------
-- METAMAC-2447 - Las instancias deben comprobar si el código es único dentro de una OOEE
-- --------------------------------------------------------------------------------------------------

-- Eliminar unique constraint para el código de las operaciones estadísticas

ALTER TABLE TB_INSTANCES
DROP CONSTRAINT INSTANCES_CODE;