-- -------------------------------------------------------------------------------------------------------------------
-- METAMAC-2137 - No funciona correctamente el link de descarga del manual de ayuda
-- -------------------------------------------------------------------------------------------------------------------

insert into TB_DATA_CONFIGURATIONS (ID,VERSION,SYSTEM_PROPERTY,CONF_KEY,CONF_VALUE) values (GET_NEXT_SEQUENCE_VALUE('DATA_CONFIGURATIONS'),1,1,'metamac.data.docs.statistical_operations.path','${metamac.data.path}/statistical-operations/docs');
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'DATA_CONFIGURATIONS';

commit;