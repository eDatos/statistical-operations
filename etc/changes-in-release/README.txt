Cuando se cree la RELEASE, añadir estos pasos al manual de instalación:

1. Parar Tomcat

2. Cambios en Base de datos
	- No enviar el script de updates-in-release porque falta migrar inserts posiblemente no tengan datos. Mandar BBDD completa. Si se quejan se manda la actualización. 

3. Cambios en el data
    - Añadido fichero resources.xml en la ruta [DATA]/conf/static
    
	- Añadir al fichero [DATA_METAMAC]/common/static/endpoints.xml (crearlo si no existe), las siguientes entradas:
	    <entry key="metamac.endpoints.statistical.operations.rest.internal">http://localhost:8080/metamac-statistical-operations-web/apis/operations-internal</entry>
	    <entry key="metamac.endpoints.statistical.operations.rest.external">http://localhost:8080/metamac-statistical-operations-external-web/apis/operations</entry>
	    
	    Se requiere que las siguientes propiedades estén ya definidas en dicho fichero:
	     - API Externa de Common Metadata: metamac.endpoints.common.metadata.rest.external
	     - API Interna de SRM: metamac.endpoints.srm.rest.internal
	     - API Externa de SRM: metamac.endpoints.srm.rest.external

4. Indicar que se han sustituido los webservices por APIs Rest. Explicar en qué direcciones se despliega la api interna y la externa.
	- Externa: http://localhost:8080/metamac-statistical-operations-external-web/apis/operations/v1.0

5. Indicar que la ubicación de la documentación de las APIs:
	- Externa: http://localhost:8080/metamac-statistical-operations-external-web/docs/api/index.html
	- Interna: http://localhost:8080/metamac-statistical-operations-web/docs/api/index.html
	
99. Reiniciar Tomcat