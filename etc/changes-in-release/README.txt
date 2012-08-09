Cuando se cree la RELEASE, añadir estos pasos al manual de instalación:

1. Parar Tomcat

2. Cambios en Base de datos
	- No enviar el script de updates-in-release porque falta migrar inserts posiblemente no tengan datos. Mandar BBDD completa. Si se quejan se manda la actualización. 

3. Cambios en el data
    - Añadido fichero resources.xml en la ruta [DATA]/conf/static
    
	- Añadir al fichero [DATA_METAMAC]/common/static/endpoints.xml (crearlo si no existe), las siguientes entradas:
	    <entry key="metamac.endpoints.statistical.operations.rest.internal">http://localhost:8080/metamac-statistical-operations-web</entry>
	    <entry key="metamac.endpoints.statistical.operations.rest.external">http://localhost:8080/metamac-statistical-operations-external-web</entry>
	    <entry key="metamac.endpoints.srm.rest.internal">http://localhost:8080/metamac-srm-web</entry>
	    <entry key="metamac.endpoints.srm.rest.external">http://localhost:8080/metamac-srm-external-web</entry>
    	
99. Reiniciar Tomcat