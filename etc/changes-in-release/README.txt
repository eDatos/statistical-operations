Cuando se cree la RELEASE, añadir estos pasos al manual de instalación:

1. Parar Tomcat

NOTA: Los siguientes cambios ya se han añadido al manual de instalación, pero no se ha hecho la instalación en el ISTAC.

	|	2. Base de datos:
	|		Actualizar los scripts de create y sucesivos. (Se pide autorización a Alberto a 06/05/2013)
	|		
	|		DEPRECATED:
	|		- Ejecutar el script 03-updates-in-release/01-update-columns-length.sql, tanto para Oracle como MSSql
	|		- Ejecutar el script 03-updates-in-release/02-update-localised-string.sql, tanto para Oracle como MSSql
	|	
	|	3. Aplicación externa:
	|		- Se ha eliminado el DATA, de forma que la configuración de la aplicación se realice en el WAR.
	|		  a) Explicar el fichero a configurar (metamac-statistical-operations-external-web-configuration.xml).
	|		  b) Explicar cómo configurar la ubicación de logs en el logback
	|		  
	|	4. Aplicación interna:
	|		- Se ha eliminado la propiedad 'metamac.statistical.operations.clients.common.metadata.rest.external' del DATA (se especifica en el data común)
	|		- [DATA]/common: Refactor fichero de configuración internal-endpoints.xml por api-endpoints.xml
	|       - Añadida propiedad 'metamac.statistical.operations.codelist.instance.geographicgranularity' a statistical-operations-freq-codelist.xml

99. Reiniciar Tomcat