package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.testutil.common.AbstractBusClientServerTestBase;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.siemac.metamac.common.test.rest.ServerResource;
import org.siemac.metamac.core.common.util.ApplicationContextProvider;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.RestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsCoreMocks;
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsRestMocks;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.springframework.context.ApplicationContext;

public class StatisticalOperationsRestFacadeV10Test extends AbstractBusClientServerTestBase {

    private static final String                       PORT               = ServerResource.PORT;
    private static String                             baseApi            = "http://localhost:" + PORT + "/api/internal/v1.0";

    private static String                             APPLICATION_XML    = "application/xml";
    private static String                             APPLICATION_JSON   = "application/json";

    private static ApplicationContext                 applicationContext = null;

    private static StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacadeClient;

    private static String                             NOT_EXISTS         = "NOT_EXISTS";

    // Operations
    private static String                             OPERATION_CODE1    = "Code1";

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {

        // Start server
        assertTrue("server did not launch correctly", launchServer(ServerResource.class, true));

        // Get application context from Jetty
        applicationContext = ApplicationContextProvider.getApplicationContext();

        // Rest client
        statisticalOperationsRestFacadeClient = JAXRSClientFactory.create(baseApi, StatisticalOperationsRestFacadeV10.class);
        WebClient.client(statisticalOperationsRestFacadeClient).accept(APPLICATION_XML);
    }

    @Before
    public void setUp() throws Exception {

        // MOCKS
        StatisticalOperationsBaseService statisticalOperationsBaseService = applicationContext.getBean(StatisticalOperationsBaseService.class);

        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq(OPERATION_CODE1))).thenReturn(StatisticalOperationsCoreMocks.mockOperation1());
        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
    }

    @Test
    public void testRetrieveOperationByCode() throws Exception {

        String code = OPERATION_CODE1;
        String baseRequest = baseApi + "/operations/" + code;
        WebClient webClient = WebClient.create(baseRequest).accept(APPLICATION_XML);
        Operation operation = webClient.get(Operation.class);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsOperation(StatisticalOperationsRestMocks.mockOperation1(baseApi), operation);
    }

    @Test
    public void testRetrieveOperationByCodeReviewXml() throws Exception {

        String code = OPERATION_CODE1;
        String baseRequest = baseApi + "/operations/" + code;

        WebClient webClient = WebClient.create(baseRequest).accept(APPLICATION_XML);
        Response response = webClient.get();

        // Validation
        assertEquals(Status.OK.getStatusCode(), response.getStatus());
        InputStream responseActual = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.code1.xml");
        InputStream responseExpected = (InputStream) response.getEntity();
        RestAsserts.assertEqualsResponse(responseActual, responseExpected);
    }

    @Test
    public void testRetrieveOperationByCodeReviewJson() throws Exception {

        String code = OPERATION_CODE1;
        String baseRequest = baseApi + "/operations/" + code;

        WebClient webClient = WebClient.create(baseRequest).accept(APPLICATION_JSON);
        Response response = webClient.get();

        // Validation
        assertEquals(Status.OK.getStatusCode(), response.getStatus());
        InputStream responseActual = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.code1.json");
        InputStream responseExpected = (InputStream) response.getEntity();
        RestAsserts.assertEqualsResponse(responseActual, responseExpected);
    }

    @Test
    public void testRetrieveOperationByCodeErrorNotExists() throws Exception {

        String code = NOT_EXISTS;
        String baseRequest = baseApi + "/operations/" + code;
        WebClient webClient = WebClient.create(baseRequest).accept(APPLICATION_XML);
        try {
            webClient.get(Operation.class);
        } catch (Exception e) {
            // TODO comprobar excepción
        }
    }
    
    @Test
    public void testRetrieveOperationByCodeErrorNotExistsReviewXml() throws Exception {

        String code = NOT_EXISTS;
        String baseRequest = baseApi + "/operations/" + code;

        WebClient webClient = WebClient.create(baseRequest).accept(APPLICATION_XML);
        Response response = webClient.get();

        // Validation
        assertEquals(Status.NOT_FOUND.getStatusCode(), response.getStatus());
        InputStream responseActual = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.xml");
        InputStream responseExpected = (InputStream) response.getEntity();
        RestAsserts.assertEqualsResponse(responseActual, responseExpected);
    }

    // TODO testear
    @Test
    @Ignore
    public void testFindOperations() throws Exception {
        // org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations operations = statisticalOperationsRestFacadeClient.findOperations();
    }    
}
