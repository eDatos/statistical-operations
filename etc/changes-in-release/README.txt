Cuando se cree la RELEASE, añadir estos pasos al manual de instalación:

1. Parar Tomcat

2. BBDD
	ALTER TABLE TB_EXTERNAL_ITEMS ADD CODE_NESTED VARCHAR2(255 CHAR);
	
3. DATA
	- Eliminado fichero 'statistical-operations-freq-codelist.xml'. Este fichero contenía los codelist por defecto para las granularidades temporales y geográficas.
	- Los codelist anteriores se especifican ahora en el data común: se han añadido las propiedades 'metamac.default.codelist.temporal_granularity.urn' y 
	'metamac.default.codelist.geographical_granularity.urn' al fichero de configuración común.
	
99. Reiniciar Tomcat