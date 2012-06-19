package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.testutil.common.AbstractBusClientServerTestBase;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.siemac.metamac.common.test.rest.ServerResource;
import org.siemac.metamac.core.common.util.ApplicationContextProvider;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsCoreMocks;
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsRestMocks;
import org.springframework.context.ApplicationContext;

public class StatisticalOperationsRestFacadeV10Test extends AbstractBusClientServerTestBase {

    private static final String                       PORT               = ServerResource.PORT;
    private static String                             baseAddress        = "http://localhost:" + PORT + "/api/internal/v1.0/";

    private static ApplicationContext                 applicationContext = null;

    private static StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacadeClient;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {

        // Start server
        assertTrue("server did not launch correctly", launchServer(ServerResource.class, true));

        // Get application context from Jetty
        applicationContext = ApplicationContextProvider.getApplicationContext();

        // Rest client
        statisticalOperationsRestFacadeClient = JAXRSClientFactory.create(baseAddress, StatisticalOperationsRestFacadeV10.class);
        WebClient.client(statisticalOperationsRestFacadeClient).accept("application/xml");
    }

    @Before
    public void setUp() throws Exception {

        // MOCKS
        StatisticalOperationsBaseService statisticalOperationsBaseService = applicationContext.getBean(StatisticalOperationsBaseService.class);

        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq("Code1"))).thenReturn(StatisticalOperationsCoreMocks.mockOperation1());
    }

    @Test
    public void testRetrieveOperationByCode() throws Exception {

        String code = "Code1";
        org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation operation = statisticalOperationsRestFacadeClient.retrieveOperationByCode(code);

        assertNotNull(operation);
        StatisticalOperationsRestAsserts.assertEqualsOperation(StatisticalOperationsRestMocks.mockOperation1(), operation);
    }

    @Test
    public void testFindOperations() throws Exception {
        org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations operations = statisticalOperationsRestFacadeClient.findOperations();
        assertNotNull(operations);
    }
}
