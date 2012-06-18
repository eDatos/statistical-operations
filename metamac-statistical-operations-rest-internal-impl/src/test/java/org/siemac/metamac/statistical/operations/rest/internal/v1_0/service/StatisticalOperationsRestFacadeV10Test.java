package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.testutil.common.AbstractBusClientServerTestBase;
import org.junit.BeforeClass;
import org.junit.Test;
import org.siemac.metamac.common.test.rest.ServerResource;

public class StatisticalOperationsRestFacadeV10Test extends AbstractBusClientServerTestBase {

    private static final String PORT = ServerResource.PORT;
    private static String baseAddress = "http://localhost:" + PORT + "/api/internal/v1.0/";
    private static StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacade; 

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {

        // Start server
        assertTrue("server did not launch correctly", launchServer(ServerResource.class, true));
        
        // Rest client
        statisticalOperationsRestFacade = JAXRSClientFactory.create(baseAddress, StatisticalOperationsRestFacadeV10.class);
        WebClient.client(statisticalOperationsRestFacade).accept("application/xml");
    }

    @Test
    public void testRetrieveOperationByCode() throws Exception {
        String code = "NOT_EXISTS";
        org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation operation = statisticalOperationsRestFacade.retrieveOperationByCode(code);
        
        assertNotNull(operation);
        assertEquals(code, operation.getCode());

    }
    
    @Test
    public void testFindOperations() throws Exception {
        org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations operations = statisticalOperationsRestFacade.findOperations();
        assertNotNull(operations);
    }
}
