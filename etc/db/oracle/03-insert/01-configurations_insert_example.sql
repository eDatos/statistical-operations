-- ###########################################
-- # Insert
-- ###########################################

insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.codelist.operation.updatefrequency','FILL_ME_WITH_CODELIST_URN');
insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.codelist.instance.geographicgranularity','FILL_ME_WITH_CODELIST_URN');
insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.codelist.instance.temporalgranularity','FILL_ME_WITH_CODELIST_URN');
insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.codelist.instance.freqcoll','FILL_ME_WITH_CODELIST_URN');
	
-- DATASOURCE: ORACLE
insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.db.url','jdbc:oracle:thin:@FILL_ME_WITH_HOST:FILL_ME_WITH_PORT:XE');
insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.db.username','FILL_ME_WITH_USERNAME');
insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.db.password','FILL_ME_WITH_PASSWORD');
insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.db.driver_name','oracle.jdbc.OracleDriver');
insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.db.dialect','org.siemac.metamac.hibernate.dialect.Oracle10gDialectMetamac');

insert into TB_CONFIGURATIONS (CONF_KEY,CONF_VALUE) values ('metamac.statistical_operations.user_guide.file_name','Gestor_operaciones_estadisticas-Manual_usuario.pdf');