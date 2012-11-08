Cuando se cree la RELEASE, añadir estos pasos al manual de instalación:

1. Parar Tomcat

2. Cambios en el data común:
	- Eliminado el directorio de configuración del ISTAC [DATA_ISTAC]
	- Añadida propiedad "metamac.navbar.url" en el fichero [DATA]/common/conf/static/resources.xml
	- Añadida propiedad "metamac.organisation" en el fichero [DATA]/common/conf/static/resources.xml

3. Cambios en el data de statistical-operations:
	- Añadido fichero [DATA]/statistical-operations/docs/Gestor_operaciones_estadísticas-Manual_usuario.pdf
	- Añadido fichero [DATA]/statistical-operations/conf/static/resources.xml con la propiedad "metamac.statistical.operations.user.guide.file.name"

4. Añadido soporte para SQL Server:
	- Se ha creado una nueva tabla que almacena las secuencias (TB_SEQUENCES). Es necesario crear esa tabla con el script
	  /etc/db/oracle/03-updates-in-release/01-create-sequences-table.sql. 
	- Si al realizar la instalación ya existen datos en la base de datos, es necesario rellenar esa tabla con las secuencias correspondientes 
	  (/etc/db/oracle/03-updates-in-release/02-fill-sequences-table.sql).	

99. Reiniciar Tomcat