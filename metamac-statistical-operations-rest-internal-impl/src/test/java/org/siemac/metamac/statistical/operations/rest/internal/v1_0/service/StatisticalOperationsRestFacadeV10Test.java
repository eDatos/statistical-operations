package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import java.io.InputStream;

import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.siemac.metamac.core.common.util.ApplicationContextProvider;
import org.siemac.metamac.rest.common.test.MetamacRestBaseTest;
import org.siemac.metamac.rest.common.test.ServerResource;
import org.siemac.metamac.rest.common.test.utils.MetamacRestAsserts;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsCoreMocks;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsRestMocks;
import org.springframework.context.ApplicationContext;

public class StatisticalOperationsRestFacadeV10Test extends MetamacRestBaseTest {

    private static final String       PORT               = ServerResource.PORT;
    private static String             baseApi            = "http://localhost:" + PORT + "/api/internal/v1.0";

    private static ApplicationContext applicationContext = null;

    private static String             NOT_EXISTS         = "NOT_EXISTS";

    // Operations
    private static String             OPERATION_CODE1    = "Operation1";
    private static String             FAMILY_CODE1       = "Family1";
    private static String             INSTANCE_CODE1     = "Instance1";

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {

        // Start server
        assertTrue("server did not launch correctly", launchServer(ServerResource.class, true));

        // Get application context from Jetty
        applicationContext = ApplicationContextProvider.getApplicationContext();
    }

    @Before
    public void setUp() throws Exception {

        // MOCKS
        StatisticalOperationsBaseService statisticalOperationsBaseService = applicationContext.getBean(StatisticalOperationsBaseService.class);
        // Operations
        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq(OPERATION_CODE1))).thenReturn(StatisticalOperationsCoreMocks.mockOperation1());
        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
        // Families
        when(statisticalOperationsBaseService.findFamilyByCode(any(ServiceContext.class), eq(FAMILY_CODE1))).thenReturn(StatisticalOperationsCoreMocks.mockFamily1());
        when(statisticalOperationsBaseService.findFamilyByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
        // Instances
        when(statisticalOperationsBaseService.findInstanceByCode(any(ServiceContext.class), eq(INSTANCE_CODE1))).thenReturn(StatisticalOperationsCoreMocks.mockInstance1());
        when(statisticalOperationsBaseService.findInstanceByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
    }

    @Test
    public void testRetrieveOperationByCode() throws Exception {

        String requestUri = getRequestUriRetrieveOperationByCode(OPERATION_CODE1);
        WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_XML);
        Operation operation = webClient.get(Operation.class);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsOperation(StatisticalOperationsRestMocks.mockOperation1(baseApi), operation);
    }

    @Test
    public void testRetrieveOperationByCodeXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveOperationByCode(OPERATION_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.code1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveOperationByCodeJsonWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveOperationByCode(OPERATION_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.code1.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveOperationByCodeErrorNotExistsXml() throws Exception {

        String requestUri = getRequestUriRetrieveOperationByCode(NOT_EXISTS);
        WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_XML);
        try {
            webClient.get(Operation.class);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.xml");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveOperationByCodeErrorNotExistsJson() throws Exception {

        String requestUri = getRequestUriRetrieveOperationByCode(NOT_EXISTS);
        WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_JSON);
        try {
            webClient.get(Operation.class);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.json");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveOperationByCodeErrorNotExistsJsonWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveOperationByCode(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveOperationByCodeErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveOperationByCode(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    }

    // TODO testear findOperations
    @Test
    @Ignore
    public void testFindOperations() throws Exception {
        // org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations operations = statisticalOperationsRestFacadeClient.findOperations();
    }

    @Test
    public void testRetrieveFamilyByCode() throws Exception {

        String requestUri = getRequestUriRetrieveFamilyByCode(FAMILY_CODE1);
        WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_XML);
        Family family = webClient.get(Family.class);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsFamily(StatisticalOperationsRestMocks.mockFamily1(baseApi), family);
    }

    @Test
    public void testRetrieveFamilyByCodeXml() throws Exception {

        String requestUri = getRequestUriRetrieveFamilyByCode(FAMILY_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.code1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveFamilyByCodeJson() throws Exception {

        String requestUri = getRequestUriRetrieveFamilyByCode(FAMILY_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.code1.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveFamilyByCodeErrorNotExistsXml() throws Exception {

        String requestUri = getRequestUriRetrieveFamilyByCode(NOT_EXISTS);
        WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_XML);
        try {
            webClient.get(Family.class);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.notFound.xml");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveFamilyByCodeErrorNotExistsJson() throws Exception {

        String requestUri = getRequestUriRetrieveFamilyByCode(NOT_EXISTS);
        WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_JSON);
        try {
            webClient.get(Family.class);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.notFound.json");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveFamilyByCodeErrorNotExistsJsonWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveFamilyByCode(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.notFound.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveFamilyByCodeErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveFamilyByCode(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.notFound.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveInstanceByCode() throws Exception {

        String requestUri = getRequestUriRetrieveInstanceByCode(OPERATION_CODE1, INSTANCE_CODE1);
        WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_XML);
        Instance instance = webClient.get(Instance.class);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsInstance(StatisticalOperationsRestMocks.mockInstance1(baseApi), instance);
    }

    // TODO

    @Test
    public void testRetrieveInstanceByCodeXml() throws Exception {

        String requestUri = getRequestUriRetrieveInstanceByCode(OPERATION_CODE1, INSTANCE_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.code1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    // @Test
    // public void testRetrieveInstanceByCodeJson() throws Exception {
    //
    // String requestUri = getRequestUriRetrieveInstanceByCode(INSTANCE_CODE1);
    // InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.code1.json");
    //
    // // Request and validate
    // testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
    // }
    //
    // @Test
    // public void testRetrieveInstanceByCodeErrorNotExistsXml() throws Exception {
    //
    // String requestUri = getRequestUriRetrieveInstanceByCode(NOT_EXISTS);
    // WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_XML);
    // try {
    // webClient.get(Instance.class);
    // } catch (Exception e) {
    // InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.notFound.xml");
    // InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
    // MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
    // }
    // }
    //
    // @Test
    // public void testRetrieveInstanceByCodeErrorNotExistsJson() throws Exception {
    //
    // String requestUri = getRequestUriRetrieveInstanceByCode(NOT_EXISTS);
    // WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_JSON);
    // try {
    // webClient.get(Instance.class);
    // } catch (Exception e) {
    // InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.notFound.json");
    // InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
    // MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
    // }
    // }
    //
    // @Test
    // public void testRetrieveInstanceByCodeErrorNotExistsJsonWithoutJaxbTransformation() throws Exception {
    // String requestUri = getRequestUriRetrieveInstanceByCode(NOT_EXISTS);
    // InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.notFound.json");
    //
    // // Request and validate
    // testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.NOT_FOUND, responseExpected);
    // }
    //
    // @Test
    // public void testRetrieveInstanceByCodeErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
    // String requestUri = getRequestUriRetrieveInstanceByCode(NOT_EXISTS);
    // InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.notFound.xml");
    //
    // // Request and validate
    // testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    // }

    private String getRequestUriRetrieveOperationByCode(String code) {
        return baseApi + "/operations/" + code;
    }

    private String getRequestUriRetrieveFamilyByCode(String code) {
        return baseApi + "/families/" + code;
    }

    private String getRequestUriRetrieveInstanceByCode(String operationCode, String code) {
        return baseApi + "/operations/" + operationCode + "/instances/" + code;
    }
}
