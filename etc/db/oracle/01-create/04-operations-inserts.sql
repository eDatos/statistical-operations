-- SURVEY TYPES

Insert into TB_LIS_SURVEY_TYPES (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('SURVEY_TYPES'),'A','f5895f76-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'SURVEY_TYPES';

Insert into TB_LIS_SURVEY_TYPES (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('SURVEY_TYPES'),'B','aaaaaaaa-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'SURVEY_TYPES';

-- INSTANCE TYPES

Insert into TB_LIS_INSTANCE_TYPES (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('INSTANCE_TYPES'),'SERIE','bbbbbbbb-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'INSTANCE_TYPES';

Insert into TB_LIS_INSTANCE_TYPES (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('INSTANCE_TYPES'),'SECTION','cccccccc-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'INSTANCE_TYPES';

-- COLL METHODS

Insert into TB_LIS_COLL_METHODS (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('COLL_METHODS'),'AAA','dddddddd-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'COLL_METHODS';

Insert into TB_LIS_COLL_METHODS (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('COLL_METHODS'),'BBB','eeeeeeee-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'COLL_METHODS';

-- COSTS

Insert into TB_LIS_COSTS (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('COSTS'),'AA','ffffffff-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'COSTS';

Insert into TB_LIS_COSTS (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('COSTS'),'BB','gggggggg-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'COSTS';

-- SURVEY SOURCES

Insert into TB_LIS_SURVEY_SOURCES (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('SURVEY_SOURCES'),'POLL','hhhhhhhh-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'SURVEY_SOURCES';

Insert into TB_LIS_SURVEY_SOURCES (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('SURVEY_SOURCES'),'ADMINISTRATION','iiiiiiii-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'SURVEY_SOURCES';

-- OFFICIALITY TYPES

Insert into TB_LIS_OFFICIALITY_TYPES (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('OFFICIALITY_TYPES'),'STUDY','jjjjjjjj-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'OFFICIALITY_TYPES';

Insert into TB_LIS_OFFICIALITY_TYPES (ID,IDENTIFIER,UUID,VERSION) values (GET_NEXT_SEQUENCE_VALUE('OFFICIALITY_TYPES'),'OFFICIAL','kkkkkkkk-14ae-47d2-b228-f4e977833be7',0);
UPDATE TB_SEQUENCES SET SEQUENCE_NEXT_VALUE = SEQUENCE_NEXT_VALUE + 1 WHERE SEQUENCE_NAME = 'OFFICIALITY_TYPES';

commit;