package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.List;

import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.util.ApplicationContextProvider;
import org.siemac.metamac.rest.RestConstants;
import org.siemac.metamac.rest.common.test.MetamacRestBaseTest;
import org.siemac.metamac.rest.common.test.ServerResource;
import org.siemac.metamac.rest.common.test.mockito.PagingParameterMatcher;
import org.siemac.metamac.rest.common.test.utils.MetamacRestAsserts;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesPagedResult;
import org.siemac.metamac.rest.utils.RestUtils;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito.FindFamiliesByOperationMatcher;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito.FindFamiliesMatcher;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito.FindInstancesByOperationMatcher;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito.FindOperationsByFamilyMatcher;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mockito.FindOperationsMatcher;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsCoreMocks;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsRestAsserts;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.utils.StatisticalOperationsRestMocks;
import org.springframework.context.ApplicationContext;
import org.springframework.web.util.UriUtils;

public class StatisticalOperationsRestFacadeV10Test extends MetamacRestBaseTest {

    private static final String                       PORT                                              = ServerResource.PORT;
    private static String                             baseApi                                           = "http://localhost:" + PORT + "/internal/v1.0";

    private static StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacadeClientXml;
    private static StatisticalOperationsRestFacadeV10 statisticalOperationsRestFacadeClientJson;

    private static ApplicationContext                 applicationContext                                = null;

    private static String                             NOT_EXISTS                                        = "NOT_EXISTS";

    // Operations
    public static String                              OPERATION_CODE1                                   = "operation1";
    public static String                              FAMILY_CODE1                                      = "family1";
    public static String                              FAMILY_CODE2                                      = "family2";
    public static String                              INSTANCE_CODE1                                    = "instance1";
    public static String                              QUERY_OPERATION_CODE_LIKE_1                       = OperationCriteriaPropertyRestriction.CODE + " LIKE \"1\"";
    public static String                              QUERY_OPERATION_CODE_LIKE_1_AND_INDICATORS_SYSTEM = OperationCriteriaPropertyRestriction.CODE + " LIKE \"1\" AND "
                                                                                                                + OperationCriteriaPropertyRestriction.IS_INDICATORS_SYSTEM + " EQ \"true\"";

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

        // Mockito
        setUpMockito();
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
    public void testFindOperationsPaginationXml() throws Exception {

        {
            // without limits
            String limit = null;
            String offset = null;
            String query = null;
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // without limits, first page
            String limit = "10000";
            String offset = null;
            String query = null;
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // without limits, first page
            String limit = null;
            String offset = "0";
            String query = null;
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // first page with pagination
            String limit = "2";
            String offset = "0";
            String query = null;
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String query = null;
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // last page with pagination
            String limit = "2";
            String offset = "8";
            String query = null;
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // no results
            String limit = "2";
            String offset = "9";
            String query = null;
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset), pagedResult);
        }

        // Queries
        {
            // query by code, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_OPERATION_CODE_LIKE_1; // operation1 and operation10
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset, query), pagedResult);
        }
        {
            // query by code and indicators system, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_OPERATION_CODE_LIKE_1_AND_INDICATORS_SYSTEM; // operation1
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset, query), pagedResult);
        }
        {
            // query by code, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_OPERATION_CODE_LIKE_1; // operation1 and operation10
            String orderBy = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findOperations(query, orderBy, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResult(baseApi, limit, offset, query), pagedResult);
        }
    }

    @Test
    public void testFindOperationsXmlWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindOperations(null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findOperations.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindOperations(null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findOperations.limit2-offset2.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }

        // Queries
        {
            // query by code, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_OPERATION_CODE_LIKE_1; // operation1 and operation10
            String requestUri = getRequestUriFindOperations(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findOperations.query1.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // query by code, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_OPERATION_CODE_LIKE_1; // operation1 and operation10
            String requestUri = getRequestUriFindOperations(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findOperations.query1.limit1-offset0.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
    }

    @Test
    public void testFindOperationsJsonWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindOperations(null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findOperations.nolimits.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindOperations(null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findOperations.limit2-offset2.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
        // Queries
        {
            // query by code, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_OPERATION_CODE_LIKE_1; // operation1 and operation10
            String requestUri = getRequestUriFindOperations(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findOperations.query1.nolimits.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
        {
            // query by code, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_OPERATION_CODE_LIKE_1; // operation1 and operation10
            String requestUri = getRequestUriFindOperations(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findOperations.query1.limit1-offset0.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
    }

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testRetrieveOperationsByFamilyXml() throws Exception {

        {
            // without limits, family 1
            String limit = null;
            String offset = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResultByFamily1(baseApi, limit, offset), pagedResult);
        }
        {
            // without limits, first page
            String limit = "10000";
            String offset = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResultByFamily1(baseApi, limit, offset), pagedResult);
        }
        {
            // without limits, first page
            String limit = null;
            String offset = "0";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResultByFamily1(baseApi, limit, offset), pagedResult);
        }
        {
            // first page with pagination
            String limit = "2";
            String offset = "0";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResultByFamily1(baseApi, limit, offset), pagedResult);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResultByFamily1(baseApi, limit, offset), pagedResult);
        }
        {
            // second page with pagination, family 2
            String limit = "1";
            String offset = "2";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(FAMILY_CODE2, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResultByFamily2(baseApi, limit, offset), pagedResult);
        }
        {
            // last page, with pagination
            String limit = "2";
            String offset = "4";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResultByFamily1(baseApi, limit, offset), pagedResult);
        }
        {
            // no results
            String limit = "2";
            String offset = "7";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockOperationsPagedResultByFamily1(baseApi, limit, offset), pagedResult);
        }
    }

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testRetrieveOperationsByFamilyXmlWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriRetrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationsByFamily.code1.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // With limit = 2, offset = 4
            String limit = "2";
            String offset = "4";
            String requestUri = getRequestUriRetrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationsByFamily.code1.limit2-offset4.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
    }

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testRetrieveOperationsByFamilyJsonWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriRetrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationsByFamily.code1.nolimits.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
        {
            // With limit = 2, offset = 4
            String limit = "2";
            String offset = "4";
            String requestUri = getRequestUriRetrieveOperationsByFamily(FAMILY_CODE1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationsByFamily.code1.limit2-offset4.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
    }

    @Test
    public void testRetrieveOperationsByFamilyErrorNotExistsXml() throws Exception {
        try {
            statisticalOperationsRestFacadeClientXml.retrieveOperationsByFamily(NOT_EXISTS, null, null);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyByCode.notFound.xml");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
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

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testFindInstancesXml() throws Exception {

        {
            // without limits
            String limit = null;
            String offset = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findInstances(OPERATION_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockInstancesPagedResultByOperation1(baseApi, limit, offset), pagedResult);
        }
        {
            // without limits, first page
            String limit = "10000";
            String offset = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findInstances(OPERATION_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockInstancesPagedResultByOperation1(baseApi, limit, offset), pagedResult);
        }
        {
            // without limits, first page
            String limit = null;
            String offset = "0";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findInstances(OPERATION_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockInstancesPagedResultByOperation1(baseApi, limit, offset), pagedResult);
        }
        {
            // first page with pagination
            String limit = "2";
            String offset = "0";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findInstances(OPERATION_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockInstancesPagedResultByOperation1(baseApi, limit, offset), pagedResult);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findInstances(OPERATION_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockInstancesPagedResultByOperation1(baseApi, limit, offset), pagedResult);
        }
        {
            // last page with pagination
            String limit = "2";
            String offset = "4";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findInstances(OPERATION_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockInstancesPagedResultByOperation1(baseApi, limit, offset), pagedResult);
        }
        {
            // no results
            String limit = "2";
            String offset = "9";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findInstances(OPERATION_CODE1, limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockInstancesPagedResultByOperation1(baseApi, limit, offset), pagedResult);
        }
    }

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testFindInstancesXmlWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindInstances(OPERATION_CODE1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findInstances.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindInstances(OPERATION_CODE1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findInstances.limit2-offset2.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
    }

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testFindInstancesJsonWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindInstances(OPERATION_CODE1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findInstances.nolimits.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindInstances(OPERATION_CODE1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findInstances.limit2-offset2.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
    }

    @Test
    public void testFindInstancesErrorOperationNotExistsXml() throws Exception {
        try {
            statisticalOperationsRestFacadeClientXml.findInstances(NOT_EXISTS, null, null);
        } catch (Exception e) {
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationByCode.notFound.xml");
            InputStream responseActual = (InputStream) ((ServerWebApplicationException) e).getResponse().getEntity();
            MetamacRestAsserts.assertEqualsResponse(responseExpected, responseActual);
        }
    }

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testFindFamiliesXml() throws Exception {

        {
            // without limits
            String limit = null;
            String offset = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findFamilies(limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockFamiliesPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // without limits, first page
            String limit = "10000";
            String offset = null;
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findFamilies(limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockFamiliesPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // without limits, first page
            String limit = null;
            String offset = "0";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findFamilies(limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockFamiliesPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // first page with pagination
            String limit = "2";
            String offset = "0";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findFamilies(limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockFamiliesPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findFamilies(limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockFamiliesPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // last page with pagination
            String limit = "2";
            String offset = "4";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findFamilies(limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockFamiliesPagedResult(baseApi, limit, offset), pagedResult);
        }
        {
            // no results
            String limit = "2";
            String offset = "9";
            ResourcesPagedResult pagedResult = statisticalOperationsRestFacadeClientXml.findFamilies(limit, offset);
            MetamacRestAsserts.assertEqualsResourcesPagedResult(StatisticalOperationsRestMocks.mockFamiliesPagedResult(baseApi, limit, offset), pagedResult);
        }
    }

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testFindFamiliesXmlWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindFamilies(limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findFamilies.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindFamilies(limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findFamilies.limit2-offset2.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
    }

    @Ignore // TODO pendiente METAMAC-753
    @Test
    public void testFindFamiliesJsonWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindFamilies(limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findFamilies.nolimits.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindFamilies(limit, offset);
            InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/findFamilies.limit2-offset2.json");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_JSON, Status.OK, responseExpected);
        }
    }

    @Test
    public void testRetrieveFamiliesByOperationXml() throws Exception {

        // Retrieve
        ResourcesNoPagedResult familiesNoPagedResult = statisticalOperationsRestFacadeClientXml.retrieveFamiliesByOperation(OPERATION_CODE1);

        // Validation
        MetamacRestAsserts.assertEqualsResourcesNoPagedResult(StatisticalOperationsRestMocks.mockFamiliesNoPagedResultByOperation1(baseApi), familiesNoPagedResult);
    }

    @Test
    public void testRetrieveFamiliesByOperationXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveFamiliesByOperation(OPERATION_CODE1);
        InputStream responseExpected = StatisticalOperationsRestFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamiliesByOperation.code1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveFamiliesByOperationJsonWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveFamiliesByOperation(OPERATION_CODE1);
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

    private String getRequestUriFindOperations(String query, String limit, String offset) throws Exception {
        String url = baseApi + "/operations";
        if (query != null) {
            query = UriUtils.encodePath(query, "UTF-8");
            url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_QUERY, query);
        }
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_LIMIT, limit);
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_OFFSET, offset);
        return url;
    }

    private String getRequestUriRetrieveFamilyByCode(String code) {
        return baseApi + "/families/" + code;
    }

    private String getRequestUriFindFamilies(String limit, String offset) {
        String url = baseApi + "/families";
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_LIMIT, limit);
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_OFFSET, offset);
        return url;
    }

    private String getRequestUriRetrieveInstanceByCode(String operationCode, String code) {
        return baseApi + "/operations/" + operationCode + "/instances/" + code;
    }

    private String getRequestUriFindInstances(String operationCode, String limit, String offset) {
        String url = baseApi + "/operations/" + operationCode + "/instances";
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_LIMIT, limit);
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_OFFSET, offset);
        return url;
    }

    private String getRequestUriRetrieveFamiliesByOperation(String code) {
        return baseApi + "/operations/" + code + "/families";
    }

    private String getRequestUriRetrieveOperationsByFamily(String code, String limit, String offset) {
        String url = baseApi + "/families/" + code + "/operations";
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_LIMIT, limit);
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_OFFSET, offset);
        return url;
    }

    private static void setUpMockito() throws MetamacException {
        // MOCKS
        StatisticalOperationsBaseService statisticalOperationsBaseService = applicationContext.getBean(StatisticalOperationsBaseService.class);

        // Retrieve operations
        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq(OPERATION_CODE1))).thenReturn(StatisticalOperationsCoreMocks.mockOperation1());
        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
        // Retrieve operations by family
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_CODE1, 25, 0);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_CODE1, 1000, 0);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_CODE1, 2, 0);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_CODE1, 2, 2);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_CODE1, 2, 4);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_CODE1, 2, 7);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_CODE2, 1, 2);
        // Find operations
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 25, 0, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 1000, 0, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 2, 0, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 2, 2, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 2, 8, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 2, 9, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 25, 0, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 25, 0, QUERY_OPERATION_CODE_LIKE_1);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 1, 0, QUERY_OPERATION_CODE_LIKE_1);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 25, 0, QUERY_OPERATION_CODE_LIKE_1_AND_INDICATORS_SYSTEM);
        // Retrieve family
        when(statisticalOperationsBaseService.findFamilyByCode(any(ServiceContext.class), eq(FAMILY_CODE1))).thenReturn(StatisticalOperationsCoreMocks.mockFamily1());
        when(statisticalOperationsBaseService.findFamilyByCode(any(ServiceContext.class), eq(FAMILY_CODE2))).thenReturn(StatisticalOperationsCoreMocks.mockFamily2());
        when(statisticalOperationsBaseService.findFamilyByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
        mockitoFindFamilyByConditionByOperation(statisticalOperationsBaseService, OPERATION_CODE1);
        // Find families
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 25, 0);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 1000, 0);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 2, 0);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 2, 2);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 2, 4);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 2, 9);
        // Retrieve instance
        when(statisticalOperationsBaseService.findInstanceByCode(any(ServiceContext.class), eq(INSTANCE_CODE1))).thenReturn(StatisticalOperationsCoreMocks.mockInstance1());
        when(statisticalOperationsBaseService.findInstanceByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
        // Find instances
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_CODE1, 25, 0);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_CODE1, 1000, 0);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_CODE1, 2, 0);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_CODE1, 2, 2);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_CODE1, 2, 4);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_CODE1, 2, 9);
    }

    private static void mockitoFindOperationByConditionByFamily(StatisticalOperationsBaseService statisticalOperationsBaseService, String family, int limit, int offset) throws MetamacException {
        PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> pagedResultOperations = null;
        if (FAMILY_CODE1.equals(family)) {
            pagedResultOperations = StatisticalOperationsCoreMocks.mockOperationsPagedResultByFamily1(String.valueOf(limit), String.valueOf(offset));
        } else if (FAMILY_CODE2.equals(family)) {
            pagedResultOperations = StatisticalOperationsCoreMocks.mockOperationsPagedResultByFamily2(String.valueOf(limit), String.valueOf(offset));
        } else {
            fail("family non supported. Family = " + family);
        }
        // Mock
        when(
                statisticalOperationsBaseService.findOperationByCondition(any(ServiceContext.class), argThat(new FindOperationsByFamilyMatcher(family)), argThat(new PagingParameterMatcher(
                        PagingParameter.rowAccess(offset, offset + limit, Boolean.TRUE))))).thenReturn(pagedResultOperations);

    }

    private static void mockitoFindOperationByCondition(StatisticalOperationsBaseService statisticalOperationsBaseService, int limit, int offset, String query) throws MetamacException {

        List<ConditionalCriteria> conditionalCriterias = null;
        String querySupported1 = QUERY_OPERATION_CODE_LIKE_1;
        String querySupported2 = QUERY_OPERATION_CODE_LIKE_1_AND_INDICATORS_SYSTEM;

        if (querySupported1.equals(query)) {
            conditionalCriterias = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class).withProperty(OperationProperties.code()).like("%1%")
                    .build();
        } else if (querySupported2.equals(query)) {
            conditionalCriterias = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class).withProperty(OperationProperties.code()).like("%1%")
                    .and().withProperty(OperationProperties.indicatorSystem()).eq(Boolean.TRUE).build();
        }
        when(
                statisticalOperationsBaseService.findOperationByCondition(any(ServiceContext.class), argThat(new FindOperationsMatcher(conditionalCriterias, null)),
                        argThat(new PagingParameterMatcher(PagingParameter.rowAccess(offset, offset + limit, Boolean.TRUE))))).thenReturn(
                StatisticalOperationsCoreMocks.mockOperationsPagedResult(String.valueOf(limit), String.valueOf(offset), query));
    }

    private static void mockitoFindFamilyByConditionByOperation(StatisticalOperationsBaseService statisticalOperationsBaseService, String operation) throws MetamacException {
        PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> pagedResult = null;
        if (OPERATION_CODE1.equals(operation)) {
            pagedResult = StatisticalOperationsCoreMocks.mockFamiliesNoPagedResultByOperation1();
        } else {
            fail("Operation non supported. Operation = " + operation);
        }
        // Mock
        when(
                statisticalOperationsBaseService.findFamilyByCondition(any(ServiceContext.class), argThat(new FindFamiliesByOperationMatcher(operation)), argThat(new PagingParameterMatcher(
                        PagingParameter.noLimits())))).thenReturn(pagedResult);
    }

    private static void mockitoFindFamilyByCondition(StatisticalOperationsBaseService statisticalOperationsBaseService, int limit, int offset) throws MetamacException {
        when(
                statisticalOperationsBaseService.findFamilyByCondition(any(ServiceContext.class), argThat(new FindFamiliesMatcher()),
                        argThat(new PagingParameterMatcher(PagingParameter.rowAccess(offset, offset + limit, Boolean.TRUE))))).thenReturn(
                StatisticalOperationsCoreMocks.mockFamiliesPagedResult(String.valueOf(limit), String.valueOf(offset)));
    }

    private static void mockitoFindInstanceByConditionByOperation(StatisticalOperationsBaseService statisticalOperationsBaseService, String operation, int limit, int offset) throws MetamacException {
        PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> pagedResult = null;
        if (OPERATION_CODE1.equals(operation)) {
            pagedResult = StatisticalOperationsCoreMocks.mockInstancesPagedResultByOperation1(String.valueOf(limit), String.valueOf(offset));
        } else {
            fail("Operation non supported. Operation = " + operation);
        }
        // Mock
        when(
                statisticalOperationsBaseService.findInstanceByCondition(any(ServiceContext.class), argThat(new FindInstancesByOperationMatcher(operation)), argThat(new PagingParameterMatcher(
                        PagingParameter.rowAccess(offset, offset + limit, Boolean.TRUE))))).thenReturn(pagedResult);
    }
}
