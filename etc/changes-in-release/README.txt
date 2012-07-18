Cuando se cree la RELEASE, añadir estos pasos al manual de instalación:

1. Parar Tomcat

2. Cambios en Base de datos
	- No enviar el script de updates-in-release porque falta migrar inserts posiblemente no tengan datos. Mandar BBDD completa. Si se quejan se manda la actualización. 

3. Cambios en el data
    - Añadido fichero resources.xml en la ruta [DATA]/conf/static

99. Reiniciar Tomcat