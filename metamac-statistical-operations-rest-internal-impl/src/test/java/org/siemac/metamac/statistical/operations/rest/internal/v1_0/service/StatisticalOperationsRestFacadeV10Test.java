package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import java.io.InputStream;

import javax.ws.rs.core.Response;

import org.apache.commons.lang.StringUtils;
import org.apache.cxf.helpers.IOUtils;
import org.apache.cxf.io.CachedOutputStream;
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
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsCoreMocks;
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.StatisticalOperationsRestMocks;
import org.springframework.context.ApplicationContext;

public class StatisticalOperationsRestFacadeV10Test extends AbstractBusClientServerTestBase {

    private static final String                       PORT               = ServerResource.PORT;
    private static String                             baseApi            = "http://localhost:" + PORT + "/api/internal/v1.0";

    private static String                             APPLICATION_XML    = "application/xml";
    private static String                             APPLICATION_JSON   = "application/json";

    private static ApplicationContext                 applicationContext = null;

    private static StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacadeClient;

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

        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq("Code1"))).thenReturn(StatisticalOperationsCoreMocks.mockOperation1());
    }

    @Test
    public void testRetrieveOperationByCode() throws Exception {

        String code = "Code1";
        org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation operation = statisticalOperationsRestFacadeClient.retrieveOperationByCode(code);

        assertNotNull(operation);
        StatisticalOperationsRestAsserts.assertEqualsOperation(StatisticalOperationsRestMocks.mockOperation1(baseApi), operation);
    }

    @Test
    public void testRetrieveOperationByCodeReviewXml() throws Exception {

        String code = "Code1";
        String baseRequest = baseApi + "/operations/" + code;

        WebClient webClient = WebClient.create(baseRequest).accept(APPLICATION_XML);
        Response response = webClient.get();

        // Validation
        assertEquals(200, response.getStatus());
        InputStream responseStream = (InputStream) response.getEntity();
        String actual = getStringFromInputStream(responseStream);
        String expected = getStringFromInputStream(StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.code1.xml"));
        // Only compare lenghts of response, because items in response can be received in different order (example: LocalisedString)
        String actualFormatted = StringUtils.deleteWhitespace(actual);
        String expectedFormatted = StringUtils.deleteWhitespace(expected);
        assertEquals(actualFormatted.length(), expectedFormatted.length());
    }

    @Test
    public void testRetrieveOperationByCodeReviewJson() throws Exception {

        String code = "Code1";
        String baseRequest = baseApi + "/operations/" + code;

        WebClient webClient = WebClient.create(baseRequest).accept(APPLICATION_JSON);
        Response response = webClient.get();

        // Validation
        assertEquals(200, response.getStatus());
        InputStream responseStream = (InputStream) response.getEntity();
        String actual = getStringFromInputStream(responseStream);
        String expected = getStringFromInputStream(StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.code1.json"));
        // Only compare lenghts of response, because items in response can be received in different order (example: LocalisedString)
        String actualFormatted = StringUtils.deleteWhitespace(actual);
        String expectedFormatted = StringUtils.deleteWhitespace(expected);
        assertEquals(actualFormatted.length(), expectedFormatted.length());
    }

    // TODO testear
    @Test
    @Ignore
    public void testFindOperations() throws Exception {
//        org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations operations = statisticalOperationsRestFacadeClient.findOperations();
    }

    private String getStringFromInputStream(InputStream in) throws Exception {
        CachedOutputStream bos = new CachedOutputStream();
        IOUtils.copy(in, bos);
        in.close();
        bos.close();
        // System.out.println(bos.getOut().toString());
        return bos.getOut().toString();
    }
}
