package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.testutil.common.AbstractBusClientServerTestBase;
import org.junit.BeforeClass;
import org.junit.Test;
import org.siemac.metamac.common.test.rest.ServerResource;

public class StatisticalOperationsRestFacadeTest extends AbstractBusClientServerTestBase {

    private static final String PORT = ServerResource.PORT;

    @BeforeClass
    public static void startServers() throws Exception {
        // JNDI SDMXDS
        // SimpleNamingContextBuilder simpleNamingContextBuilder = OracleJNDIMock.setUp("SDMXDS", "metamac", "metamac", "jdbc:oracle:thin:@localhost:1521:XE", null);

        assertTrue("server did not launch correctly", launchServer(ServerResource.class, true));
    }

    @Test
    public void testRetrieveOperationByCode() throws Exception {

        String baseAddress = "http://localhost:" + PORT + "/api/internal/v1.0/";
        StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacade = JAXRSClientFactory.create(baseAddress, StatisticalOperationsRestFacadeV10.class);
        WebClient.client(statisticalOperationsRestFacade).accept("application/xml");
        org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation operation = statisticalOperationsRestFacade.retrieveOperationByCode("CODE-1");
        assertNotNull(operation);

    }
    
    @Test
    public void testFindOperations() throws Exception {

        String baseAddress = "http://localhost:" + PORT + "/api/internal/v1.0/";
        StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacade = JAXRSClientFactory.create(baseAddress, StatisticalOperationsRestFacadeV10.class);
        WebClient.client(statisticalOperationsRestFacade).accept("application/xml");
        org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations operations = statisticalOperationsRestFacade.findOperations();
        assertNotNull(operations);
    }
}
