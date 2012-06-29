package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
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
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamiliesNoPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito.FindFamiliesByOperationCode1Matcher;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito.FindFamiliesByOperationCode1PaginatorMatcher;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsCoreMocks;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsRestMocks;
import org.springframework.context.ApplicationContext;

public class StatisticalOperationsRestFacadeV10Test extends MetamacRestBaseTest {

    private static final String                       PORT               = ServerResource.PORT;
    private static String                             baseApi            = "http://localhost:" + PORT + "/internal/v1.0";

    private static StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacadeClientXml;
    private static StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacadeClientJson;

    private static ApplicationContext                 applicationContext = null;

    private static String                             NOT_EXISTS         = "NOT_EXISTS";

    // Operations
    public static String                              OPERATION_CODE1    = "operation1";
    public static String                              FAMILY_CODE1       = "family1";
    public static String                              INSTANCE_CODE1     = "instance1";

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {

        // Start server
        assertTrue("server did not launch correctly", launchServer(ServerResource.class, true));

        // Get application context from Jetty
        applicationContext = ApplicationContextProvider.getApplicationContext();

        // Rest clients
        // xml
        statisticalOperationsRestFacadeClientXml = JAXRSClientFactory.create(baseApi, StatisticalOperationsRestFacadeV10.class);
        WebClient.client(statisticalOperationsRestFacadeClientXml).accept(APPLICATION_XML);
        // json
        statisticalOperationsRestFacadeClientJson = JAXRSClientFactory.create(baseApi, StatisticalOperationsRestFacadeV10.class);
        WebClient.client(statisticalOperationsRestFacadeClientJson).accept(APPLICATION_JSON);

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
        when(
                statisticalOperationsBaseService.findFamilyByCondition(any(ServiceContext.class), argThat(new FindFamiliesByOperationCode1Matcher()),
                        argThat(new FindFamiliesByOperationCode1PaginatorMatcher()))).thenReturn(StatisticalOperationsCoreMocks.mockFamiliesOperation1());
        // Instances
        when(statisticalOperationsBaseService.findInstanceByCode(any(ServiceContext.class), eq(INSTANCE_CODE1))).thenReturn(StatisticalOperationsCoreMocks.mockInstance1());
        when(statisticalOperationsBaseService.findInstanceByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
    }

    @Test
    public void testWithoutMatchError404() throws Exception {

        String requestUri = baseApi + "/nomatch";

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, new ByteArrayInputStream(new byte[0]));
    }

    @Test
    public void testRetrieveOperationByCodeXml() throws Exception {

        // Retrieve
        Operation operation = statisticalOperationsRestFacadeClientXml.retrieveOperationByCode(OPERATION_CODE1);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsOperation(StatisticalOperationsRestMocks.mockOperation1(baseApi), operation);
    }

    @Ignore
    @Test
    public void testRetrieveOperationByCodeJson() throws Exception {
        // NOTE: Throws exception. We dont support calls with jaxb transformation when media type is Json. @see METAMAC-675
        // Operation operation = statisticalOperationsRestFacadeClientJson.retrieveOperationByCode(OPERATION_CODE1);
        // StatisticalOperationsRestAsserts.assertEqualsOperation(StatisticalOperationsRestMocks.mockOperation1(baseApi), operation);
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
        try {
            statisticalOperationsRestFacadeClientXml.retrieveOperationByCode(NOT_EXISTS);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.xml");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveOperationByCodeErrorNotExistsJson() throws Exception {

        try {
            statisticalOperationsRestFacadeClientJson.retrieveOperationByCode(NOT_EXISTS);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.json");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveOperationByCodeErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveOperationByCode(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveOperationByCodeErrorNotExistsJsonWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveOperationByCode(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveFamilyByCodeXml() throws Exception {

        // Retrieve
        Family family = statisticalOperationsRestFacadeClientXml.retrieveFamilyByCode(FAMILY_CODE1);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsFamily(StatisticalOperationsRestMocks.mockFamily1(baseApi), family);
    }

    @Test
    public void testRetrieveFamilyByCodeXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveFamilyByCode(FAMILY_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.code1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Ignore
    @Test
    public void testRetrieveFamilyByCodeJson() throws Exception {
        // NOTE: Throws exception. We dont support calls with jaxb transformation when media type is Json. @see METAMAC-675
        // Family family = statisticalOperationsRestFacadeClientJson.retrieveFamilyByCode(FAMILY_CODE1);
        // StatisticalOperationsRestAsserts.assertEqualsFamily(StatisticalOperationsRestMocks.mockFamily1(baseApi), family);
    }

    @Test
    public void testRetrieveFamilyByCodeJsonWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveFamilyByCode(FAMILY_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.code1.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveFamilyByCodeErrorNotExistsXml() throws Exception {
        try {
            statisticalOperationsRestFacadeClientXml.retrieveFamilyByCode(NOT_EXISTS);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.notFound.xml");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveFamilyByCodeErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveFamilyByCode(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.notFound.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveFamilyByCodeErrorNotExistsJson() throws Exception {

        try {
            statisticalOperationsRestFacadeClientJson.retrieveFamilyByCode(NOT_EXISTS);
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
    public void testRetrieveInstanceByCodeXml() throws Exception {

        // Retrieve
        Instance instance = statisticalOperationsRestFacadeClientXml.retrieveInstanceByCode(OPERATION_CODE1, INSTANCE_CODE1);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsInstance(StatisticalOperationsRestMocks.mockInstance1(baseApi), instance);
    }

    @Test
    public void testRetrieveInstanceByCodeXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveInstanceByCode(OPERATION_CODE1, INSTANCE_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.code1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Ignore
    @Test
    public void testRetrieveInstanceByCodeJson() throws Exception {

        // NOTE: Throws exception. We dont support calls with jaxb transformation when media type is Json. @see METAMAC-675
        // Instance instance = statisticalOperationsRestFacadeClientJson.retrieveInstanceByCode(OPERATION_CODE1, INSTANCE_CODE1);
        // StatisticalOperationsRestAsserts.assertEqualsInstance(StatisticalOperationsRestMocks.mockInstance1(baseApi), instance);
    }

    @Test
    public void testRetrieveInstanceByCodeJsonWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveInstanceByCode(OPERATION_CODE1, INSTANCE_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.code1.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveInstanceByCodeErrorNotExistsXml() throws Exception {
        try {
            statisticalOperationsRestFacadeClientXml.retrieveInstanceByCode(OPERATION_CODE1, NOT_EXISTS);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.notFound.xml");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveInstanceByCodeErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveInstanceByCode(OPERATION_CODE1, NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.notFound.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveInstanceByCodeErrorNotExistsJson() throws Exception {

        try {
            statisticalOperationsRestFacadeClientJson.retrieveInstanceByCode(OPERATION_CODE1, NOT_EXISTS);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.notFound.json");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Test
    public void testRetrieveInstanceByCodeErrorNotExistsJsonWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveInstanceByCode(OPERATION_CODE1, NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceByCode.notFound.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveFamiliesByOperationXml() throws Exception {

        // Retrieve
        FamiliesNoPagedResult familiesNoPagedResult = statisticalOperationsRestFacadeClientXml.retrieveFamiliesByOperation(OPERATION_CODE1);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsFamiliesNoPagedResult(StatisticalOperationsRestMocks.mockFamiliesNoPagedResultByOperation1(baseApi), familiesNoPagedResult);
    }

    @Test
    public void testRetrieveFamiliesByOperationXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveFamiliesByOperationByCode(OPERATION_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamiliesByOperation.code1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveFamiliesByOperationJsonWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveFamiliesByOperationByCode(OPERATION_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamiliesByOperation.code1.json");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveFamiliesByOperationErrorNotExistsXml() throws Exception {
        try {
            statisticalOperationsRestFacadeClientXml.retrieveFamiliesByOperation(NOT_EXISTS);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.xml");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    private String getRequestUriRetrieveOperationByCode(String code) {
        return baseApi + "/operations/" + code;
    }

    private String getRequestUriRetrieveFamilyByCode(String code) {
        return baseApi + "/families/" + code;
    }

    private String getRequestUriRetrieveInstanceByCode(String operationCode, String code) {
        return baseApi + "/operations/" + operationCode + "/instances/" + code;
    }

    private String getRequestUriRetrieveFamiliesByOperationByCode(String code) {
        return baseApi + "/operations/" + code + "/families";
    }
}
