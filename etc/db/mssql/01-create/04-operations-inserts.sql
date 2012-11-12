-- SURVEY TYPES

Insert into TB_LIS_SURVEY_TYPES (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('SURVEY_TYPES'),'A','f5895f76-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'SURVEY_TYPES';

Insert into TB_LIS_SURVEY_TYPES (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('SURVEY_TYPES'),'B','aaaaaaaa-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'SURVEY_TYPES';

-- INSTANCE TYPES

Insert into TB_LIS_INSTANCE_TYPES (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('INSTANCE_TYPES'),'SERIE','bbbbbbbb-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'INSTANCE_TYPES';

Insert into TB_LIS_INSTANCE_TYPES (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('INSTANCE_TYPES'),'SECTION','cccccccc-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'INSTANCE_TYPES';

-- COLL METHODS

Insert into TB_LIS_COLL_METHODS (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('COLL_METHODS'),'AAA','dddddddd-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'COLL_METHODS';

Insert into TB_LIS_COLL_METHODS (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('COLL_METHODS'),'BBB','eeeeeeee-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'COLL_METHODS';

-- COSTS

Insert into TB_LIS_COSTS (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('COSTS'),'AA','ffffffff-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'COSTS';

Insert into TB_LIS_COSTS (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('COSTS'),'BB','gggggggg-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'COSTS';

-- SURVEY SOURCES

Insert into TB_LIS_SURVEY_SOURCES (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('SURVEY_SOURCES'),'POLL','hhhhhhhh-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'SURVEY_SOURCES';

Insert into TB_LIS_SURVEY_SOURCES (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('SURVEY_SOURCES'),'ADMINISTRATION','iiiiiiii-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'SURVEY_SOURCES';

-- OFFICIALITY TYPES

Insert into TB_LIS_OFFICIALITY_TYPES (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('OFFICIALITY_TYPES'),'STUDY','jjjjjjjj-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'OFFICIALITY_TYPES';

Insert into TB_LIS_OFFICIALITY_TYPES (ID,IDENTIFIER,UUID,VERSION) values (FILL_WITH_SCHEMA_NAME.GetSequenceNextValue('OFFICIALITY_TYPES'),'OFFICIAL','kkkkkkkk-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'OFFICIALITY_TYPES';

commit;
