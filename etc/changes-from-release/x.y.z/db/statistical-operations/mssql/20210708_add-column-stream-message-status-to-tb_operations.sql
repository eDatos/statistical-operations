-- --------------------------------------------------------------------------------------------------
-- EDATOS-3380 - Enviar mensaje a través de kafka al publicar operaciones
-- --------------------------------------------------------------------------------------------------

-- Añade nueva columna a la tabla de operaciones que determina el estado de transmisión a través de Kafka

ALTER TABLE TB_OPERATIONS
ADD COLUMN STREAM_MESSAGE_STATUS VARCHAR(255);

-- 'Update' statement without 'where' updates all table rows at once
UPDATE TB_OPERATIONS
SET STREAM_MESSAGE_STATUS = 'PENDING';

ALTER TABLE TB_OPERATIONS
ALTER COLUMN STREAM_MESSAGE_STATUS SET NOT NULL;
