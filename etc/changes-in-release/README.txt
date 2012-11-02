Cuando se cree la RELEASE, añadir estos pasos al manual de instalación:

1. Parar Tomcat

2. Cambios en el data común:
	- Eliminado el directorio de configuración del ISTAC [DATA_ISTAC]
	- Añadida propiedad "metamac.navbar.url" en el fichero [DATA]/common/conf/static/resources.xml
	- Añadida propiedad "metamac.organisation" en el fichero [DATA]/common/conf/static/resources.xml

3. Cambios en el data de statistical-operations:
	- Añadido fichero [DATA]/statistical-operations/docs/Gestor_operaciones_estadísticas-Manual_usuario.pdf
	- Añadido fichero [DATA]/statistical-operations/conf/static/resources.xml con la propiedad "metamac.statistical.operations.user.guide.file.name"


99. Reiniciar Tomcat