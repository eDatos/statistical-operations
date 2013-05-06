Cuando se cree la RELEASE, añadir estos pasos al manual de instalación:

1. Parar Tomcat

2. Base de datos:
	Actualizar los scripts de create y sucesivos. (Se pide autorización a Alberto a 06/05/2013)
	

	DEPRECATED:
	- Ejecutar el script 03-updates-in-release/01-update-columns-length.sql, tanto para Oracle como MSSql
	- Ejecutar el script 03-updates-in-release/02-update-localised-string.sql, tanto para Oracle como MSSql

3. Aplicación externa:
	- Se ha eliminado el DATA, de forma que la configuración de la aplicación se realice en el WAR.
	  a) Explicar el fichero a configurar (metamac-statistical-operations-external-web-configuration.xml).
	  b) Explicar cómo configurar la ubicación de logs en el logback
	  
4. Aplicación interna:
	- Se ha eliminado la propiedad 'metamac.statistical.operations.clients.common.metadata.rest.external' del DATA (se especifica en el data común)

99. Reiniciar Tomcat