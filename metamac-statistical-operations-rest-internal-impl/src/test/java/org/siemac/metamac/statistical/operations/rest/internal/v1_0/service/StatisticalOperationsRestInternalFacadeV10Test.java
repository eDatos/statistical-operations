package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.argThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.client.JAXRSClientFactory;
import org.apache.cxf.jaxrs.client.ServerWebApplicationException;
import org.apache.cxf.jaxrs.client.WebClient;
import org.apache.cxf.jaxrs.provider.JAXBElementProvider;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.junit.BeforeClass;
import org.junit.Test;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.util.ApplicationContextProvider;
import org.siemac.metamac.rest.common.test.MetamacRestBaseTest;
import org.siemac.metamac.rest.common.test.ServerResource;
import org.siemac.metamac.rest.common.test.mockito.PagingParameterMatcher;
import org.siemac.metamac.rest.common.v1_0.domain.ComparisonOperator;
import org.siemac.metamac.rest.common.v1_0.domain.LogicalOperator;
import org.siemac.metamac.rest.constants.RestConstants;
import org.siemac.metamac.rest.utils.RestUtils;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.core.domain.InstanceProperties;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsListsService;
import org.siemac.metamac.statistical.operations.rest.internal.exception.RestServiceExceptionType;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.CollMethods;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Costs;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Families;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamilyCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InstanceCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InstanceTypes;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instances;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationCriteriaPropertyRestriction;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.SurveySources;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.SurveyTypes;
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

public class StatisticalOperationsRestInternalFacadeV10Test extends MetamacRestBaseTest {

    private static final String                               PORT                                            = ServerResource.PORT;
    private static String                                     baseApi                                         = "http://localhost:" + PORT + "/internal/v1.0";

    private static StatisticalOperationsRestInternalFacadeV10 statisticalOperationsRestInternalFacadeClientXml;

    private static ApplicationContext                         applicationContext                              = null;

    private static String                                     NOT_EXISTS                                      = "NOT_EXISTS";

    // Operations
    public static String                                      OPERATION_1                                     = "operation1";
    public static String                                      FAMILY_1                                        = "family1";
    public static String                                      FAMILY_2                                        = "family2";
    public static String                                      INSTANCE_1                                      = "instance1";
    public static String                                      COMMON_METADATA_1                               = "commonMetadata1";
    public static String                                      QUERY_OPERATION_ID_LIKE_1                       = OperationCriteriaPropertyRestriction.ID + " " + ComparisonOperator.LIKE + " \"1\"";
    public static String                                      QUERY_OPERATION_ID_LIKE_1_AND_INDICATORS_SYSTEM = OperationCriteriaPropertyRestriction.ID + " " + ComparisonOperator.LIKE + " \"1\" "
                                                                                                                      + LogicalOperator.AND + " "
                                                                                                                      + OperationCriteriaPropertyRestriction.IS_INDICATORS_SYSTEM + " "
                                                                                                                      + ComparisonOperator.EQ + " \"true\"";
    public static String                                      QUERY_FAMILY_ID_LIKE_1                          = FamilyCriteriaPropertyRestriction.ID + " " + ComparisonOperator.LIKE + " \"1\"";
    public static String                                      QUERY_INSTANCE_ID_LIKE_1                        = InstanceCriteriaPropertyRestriction.ID + " " + ComparisonOperator.LIKE + " \"1\"";

    @SuppressWarnings({"unchecked", "rawtypes"})
    @BeforeClass
    public static void setUpBeforeClass() throws Exception {

        // Start server
        assertTrue("server did not launch correctly", launchServer(ServerResource.class, true));

        // Get application context from Jetty
        applicationContext = ApplicationContextProvider.getApplicationContext();

        // Rest clients
        // xml
        {
            List providers = new ArrayList();
            providers.add(applicationContext.getBean(JAXBElementProvider.class));
            statisticalOperationsRestInternalFacadeClientXml = JAXRSClientFactory.create(baseApi, StatisticalOperationsRestInternalFacadeV10.class, providers, Boolean.TRUE);
        }
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
    public void testRetrieveOperationByIdXml() throws Exception {

        // Retrieve
        Operation operation = getStatisticalOperationsRestInternalFacadeClientXml().retrieveOperationById(OPERATION_1);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsOperation(StatisticalOperationsRestMocks.mockOperation1(baseApi), operation);
    }

    @Test
    public void testRetrieveOperationByIdXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveOperationById(OPERATION_1);
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationById.id1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveOperationByIdXmlWithoutJaxbTransformationWithXmlSufix() throws Exception {

        String requestUri = getRequestUriRetrieveOperationById(OPERATION_1) + ".xml";
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationById.id1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveOperationByIdXmlWithoutJaxbTransformationWithTypeParameter() throws Exception {

        String requestUri = getRequestUriRetrieveOperationById(OPERATION_1) + "?_type=xml";
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationById.id1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveOperationByIdErrorNotExistsXml() throws Exception {
        try {
            getStatisticalOperationsRestInternalFacadeClientXml().retrieveOperationById(NOT_EXISTS);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Error error = extractErrorFromException(statisticalOperationsRestInternalFacadeClientXml, e);

            assertEquals(1, error.getErrorItems().getErrorItems().size());
            assertEquals(RestServiceExceptionType.OPERATION_NOT_FOUND.getCode(), error.getErrorItems().getErrorItems().get(0).getCode());
            assertEquals("Operation not found with id {0}", error.getErrorItems().getErrorItems().get(0).getMessage());
            assertEquals(1, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().size());
            assertEquals(NOT_EXISTS, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().get(0));
        } catch (Exception e) {
            fail("Incorrect exception");
        }
    }

    @Test
    public void testRetrieveOperationByIdErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveOperationById(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveOperationById.notFound.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveOperationByIdJsonNonAcceptable() throws Exception {

        String requestUri = getRequestUriRetrieveOperationById(OPERATION_1);

        // Request and validate
        WebClient webClient = WebClient.create(requestUri).accept(APPLICATION_JSON);
        Response response = webClient.get();

        assertEquals(Status.NOT_ACCEPTABLE.getStatusCode(), response.getStatus());
    }

    @Test
    public void testFindOperationsXml() throws Exception {

        {
            // without limits
            String limit = null;
            String offset = null;
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset), operations);
        }
        {
            // without limits, first page
            String limit = "10000";
            String offset = null;
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset), operations);
        }
        {
            // without limits, first page
            String limit = null;
            String offset = "0";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset), operations);
        }
        {
            // first page with pagination
            String limit = "2";
            String offset = "0";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset), operations);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset), operations);
        }
        {
            // last page with pagination
            String limit = "2";
            String offset = "8";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset), operations);
        }
        {
            // no results
            String limit = "2";
            String offset = "9";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset), operations);
        }

        // Queries
        {
            // query by id, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_OPERATION_ID_LIKE_1; // operation1 and operation10
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset, query), operations);
        }
        {
            // query by id and indicators system, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_OPERATION_ID_LIKE_1_AND_INDICATORS_SYSTEM; // operation1
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset, query), operations);
        }
        {
            // query by id, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_OPERATION_ID_LIKE_1; // operation1 and operation10
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperations(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperations(baseApi, limit, offset, query), operations);
        }
    }

    @Test
    public void testFindOperationsXmlWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindOperations(null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findOperations.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindOperations(null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findOperations.limit2-offset2.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }

        // Queries
        {
            // query by id, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_OPERATION_ID_LIKE_1; // operation1 and operation10
            String requestUri = getRequestUriFindOperations(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findOperations.query1.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // query by id, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_OPERATION_ID_LIKE_1; // operation1 and operation10
            String requestUri = getRequestUriFindOperations(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findOperations.query1.limit1-offset0.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
    }

    @Test
    public void testFindOperationsXmlErrorParameterIncorrectWithoutJaxbTransformation() throws Exception {

        // Metadata not supported to search
        {
            String limit = "1";
            String offset = "0";
            String query = "METADATA_INCORRECT LIKE \"1\"";
            String requestUri = getRequestUriFindOperations(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findOperations.parameterIncorrect.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.INTERNAL_SERVER_ERROR, responseExpected);
        }
        // Query incorrect
        {
            String limit = "1";
            String offset = "0";
            String query = OperationCriteriaPropertyRestriction.ID + " OP_INCORRECT \"1\"";
            String requestUri = getRequestUriFindOperations(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findOperations.queryIncorrect.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.INTERNAL_SERVER_ERROR, responseExpected);
        }
    }

    @Test
    public void testFindOperationsByFamilyXml() throws Exception {

        {
            // without limits, family 1
            String limit = null;
            String offset = null;
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(FAMILY_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperationsByFamily1(baseApi, limit, offset), operations);
        }
        {
            // without limits, first page
            String limit = "10000";
            String offset = null;
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(FAMILY_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperationsByFamily1(baseApi, limit, offset), operations);
        }
        {
            // without limits, first page
            String limit = null;
            String offset = "0";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(FAMILY_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperationsByFamily1(baseApi, limit, offset), operations);
        }
        {
            // first page with pagination
            String limit = "2";
            String offset = "0";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(FAMILY_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperationsByFamily1(baseApi, limit, offset), operations);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(FAMILY_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperationsByFamily1(baseApi, limit, offset), operations);
        }
        {
            // second page with pagination, family 2
            String limit = "1";
            String offset = "2";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(FAMILY_2, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperationsByFamily2(baseApi, limit, offset), operations);
        }
        {
            // last page, with pagination
            String limit = "2";
            String offset = "4";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(FAMILY_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperationsByFamily1(baseApi, limit, offset), operations);
        }
        {
            // no results
            String limit = "2";
            String offset = "7";
            String query = null;
            String orderBy = null;
            Operations operations = getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(FAMILY_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsOperations(StatisticalOperationsRestMocks.mockOperationsByFamily1(baseApi, limit, offset), operations);
        }
    }

    @Test
    public void testFindOperationsByFamilyXmlWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindOperationsByFamily(FAMILY_1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findOperationsByFamily.id1.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // With limit = 2, offset = 4
            String limit = "2";
            String offset = "4";
            String requestUri = getRequestUriFindOperationsByFamily(FAMILY_1, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findOperationsByFamily.id1.limit2-offset4.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
    }

    @Test
    public void testFindOperationsByFamilyErrorNotExistsXml() throws Exception {
        try {
            getStatisticalOperationsRestInternalFacadeClientXml().findOperationsByFamily(NOT_EXISTS, null, null, null, null);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Error error = extractErrorFromException(statisticalOperationsRestInternalFacadeClientXml, e);

            assertEquals(1, error.getErrorItems().getErrorItems().size());
            assertEquals(RestServiceExceptionType.FAMILY_NOT_FOUND.getCode(), error.getErrorItems().getErrorItems().get(0).getCode());
            assertEquals("Family not found with id {0}", error.getErrorItems().getErrorItems().get(0).getMessage());
            assertEquals(1, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().size());
            assertEquals(NOT_EXISTS, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().get(0));
        } catch (Exception e) {
            fail("Incorrect exception");
        }
    }

    @Test
    public void testRetrieveFamilyByIdXml() throws Exception {

        // Retrieve
        Family family = getStatisticalOperationsRestInternalFacadeClientXml().retrieveFamilyById(FAMILY_1);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsFamily(StatisticalOperationsRestMocks.mockFamily1(baseApi), family);
    }

    @Test
    public void testRetrieveFamilyByIdXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveFamilyById(FAMILY_1);
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyById.id1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveFamilyByIdErrorNotExistsXml() throws Exception {
        try {
            getStatisticalOperationsRestInternalFacadeClientXml().retrieveFamilyById(NOT_EXISTS);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Error error = extractErrorFromException(statisticalOperationsRestInternalFacadeClientXml, e);

            assertEquals(1, error.getErrorItems().getErrorItems().size());
            assertEquals(RestServiceExceptionType.FAMILY_NOT_FOUND.getCode(), error.getErrorItems().getErrorItems().get(0).getCode());
            assertEquals("Family not found with id {0}", error.getErrorItems().getErrorItems().get(0).getMessage());
            assertEquals(1, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().size());
            assertEquals(NOT_EXISTS, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().get(0));
        } catch (Exception e) {
            fail("Incorrect exception");
        }
    }

    @Test
    public void testRetrieveFamilyByIdErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveFamilyById(NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamilyById.notFound.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testRetrieveInstanceByIdXml() throws Exception {

        // Retrieve
        Instance instance = getStatisticalOperationsRestInternalFacadeClientXml().retrieveInstanceById(OPERATION_1, INSTANCE_1);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsInstance(StatisticalOperationsRestMocks.mockInstance1(baseApi), instance);
    }

    @Test
    public void testRetrieveInstanceByIdXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveInstanceById(OPERATION_1, INSTANCE_1);
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceById.id1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveInstanceByIdErrorNotExistsXml() throws Exception {
        try {
            getStatisticalOperationsRestInternalFacadeClientXml().retrieveInstanceById(OPERATION_1, NOT_EXISTS);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Error error = extractErrorFromException(statisticalOperationsRestInternalFacadeClientXml, e);

            assertEquals(1, error.getErrorItems().getErrorItems().size());
            assertEquals(RestServiceExceptionType.INSTANCE_NOT_FOUND.getCode(), error.getErrorItems().getErrorItems().get(0).getCode());
            assertEquals("Instance not found with id {0}", error.getErrorItems().getErrorItems().get(0).getMessage());
            assertEquals(1, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().size());
            assertEquals(NOT_EXISTS, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().get(0));
        } catch (Exception e) {
            fail("Incorrect exception");
        }
    }

    @Test
    public void testRetrieveInstanceByIdErrorNotExistsXmlWithoutJaxbTransformation() throws Exception {
        String requestUri = getRequestUriRetrieveInstanceById(OPERATION_1, NOT_EXISTS);
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceById.notFound.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.NOT_FOUND, responseExpected);
    }

    @Test
    public void testFindInstancesXml() throws Exception {

        {
            // without limits
            String limit = null;
            String offset = null;
            String query = null;
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset), instances);
        }
        {
            // without limits, first page
            String limit = "10000";
            String offset = null;
            String query = null;
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset), instances);
        }
        {
            // without limits, first page
            String limit = null;
            String offset = "0";
            String query = null;
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset), instances);
        }
        {
            // first page with pagination
            String limit = "2";
            String offset = "0";
            String query = null;
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset), instances);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String query = null;
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset), instances);
        }
        {
            // last page with pagination
            String limit = "2";
            String offset = "4";
            String query = null;
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset), instances);
        }
        {
            // no results
            String limit = "2";
            String offset = "9";
            String query = null;
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset), instances);
        }

        // Queries
        {
            // query by id, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_INSTANCE_ID_LIKE_1; // instance1 and instance15
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset, query), instances);
        }
        {
            // query by id, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_INSTANCE_ID_LIKE_1; // instance1 and instance15
            String orderBy = null;
            Instances instances = getStatisticalOperationsRestInternalFacadeClientXml().findInstances(OPERATION_1, query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsInstances(StatisticalOperationsRestMocks.mockInstancesByOperation1(baseApi, limit, offset, query), instances);
        }
    }

    @Test
    public void testFindInstancesXmlWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindInstances(OPERATION_1, null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findInstances.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindInstances(OPERATION_1, null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findInstances.limit2-offset2.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }

        // Queries
        {
            // query by id, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_INSTANCE_ID_LIKE_1;
            String requestUri = getRequestUriFindInstances(OPERATION_1, query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findInstances.query1.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // query by id, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_INSTANCE_ID_LIKE_1; // instance1 and instance15
            String requestUri = getRequestUriFindInstances(OPERATION_1, query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findInstances.query1.limit1-offset0.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
    }

    @Test
    public void testFindInstancesErrorOperationNotExistsXml() throws Exception {
        try {
            getStatisticalOperationsRestInternalFacadeClientXml().findInstances(NOT_EXISTS, null, null, null, null);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Error error = extractErrorFromException(statisticalOperationsRestInternalFacadeClientXml, e);

            assertEquals(1, error.getErrorItems().getErrorItems().size());
            assertEquals(RestServiceExceptionType.OPERATION_NOT_FOUND.getCode(), error.getErrorItems().getErrorItems().get(0).getCode());
            assertEquals("Operation not found with id {0}", error.getErrorItems().getErrorItems().get(0).getMessage());
            assertEquals(1, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().size());
            assertEquals(NOT_EXISTS, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().get(0));
        } catch (Exception e) {
            fail("Incorrect exception");
        }
    }

    @Test
    public void testFindFamiliesXml() throws Exception {

        {
            // without limits
            String limit = null;
            String offset = null;
            String query = null;
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset), families);
        }
        {
            // without limits, first page
            String limit = "10000";
            String offset = null;
            String query = null;
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset), families);
        }
        {
            // without limits, first page
            String limit = null;
            String offset = "0";
            String query = null;
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset), families);
        }
        {
            // first page with pagination
            String limit = "2";
            String offset = "0";
            String query = null;
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset), families);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String query = null;
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset), families);
        }
        {
            // last page with pagination
            String limit = "2";
            String offset = "4";
            String query = null;
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset), families);
        }
        {
            // no results
            String limit = "2";
            String offset = "9";
            String query = null;
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset), families);
        }
        // Queries
        {
            // query by id, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_FAMILY_ID_LIKE_1; // family1 and family15
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset, query), families);
        }
        {
            // query by id, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_FAMILY_ID_LIKE_1; // family1 and family15
            String orderBy = null;
            Families families = getStatisticalOperationsRestInternalFacadeClientXml().findFamilies(query, orderBy, limit, offset);
            StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamilies(baseApi, limit, offset, query), families);
        }
    }

    @Test
    public void testFindFamiliesXmlWithoutJaxbTransformation() throws Exception {
        {
            // without limits
            String limit = null;
            String offset = null;
            String requestUri = getRequestUriFindFamilies(null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findFamilies.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // second page with pagination
            String limit = "2";
            String offset = "2";
            String requestUri = getRequestUriFindFamilies(null, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findFamilies.limit2-offset2.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }

        // Queries
        {
            // query by id, without limits
            String limit = null;
            String offset = null;
            String query = QUERY_FAMILY_ID_LIKE_1;
            String requestUri = getRequestUriFindFamilies(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findFamilies.query1.nolimits.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
        {
            // query by id, first page
            String limit = "1";
            String offset = "0";
            String query = QUERY_FAMILY_ID_LIKE_1; // family1 and family15
            String requestUri = getRequestUriFindFamilies(query, limit, offset);
            InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/findFamilies.query1.limit1-offset0.xml");

            // Request and validate
            testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
        }
    }

    @Test
    public void testRetrieveFamiliesByOperationXml() throws Exception {

        // Retrieve
        Families families = getStatisticalOperationsRestInternalFacadeClientXml().retrieveFamiliesByOperation(OPERATION_1);

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsFamilies(StatisticalOperationsRestMocks.mockFamiliesByOperation1(baseApi), families);
    }

    @Test
    public void testRetrieveFamiliesByOperationXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveFamiliesByOperation(OPERATION_1);
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveFamiliesByOperation.id1.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveFamiliesByOperationErrorNotExistsXml() throws Exception {
        try {
            getStatisticalOperationsRestInternalFacadeClientXml().retrieveFamiliesByOperation(NOT_EXISTS);
        } catch (ServerWebApplicationException e) {
            org.siemac.metamac.rest.common.v1_0.domain.Error error = extractErrorFromException(statisticalOperationsRestInternalFacadeClientXml, e);

            assertEquals(1, error.getErrorItems().getErrorItems().size());
            assertEquals(RestServiceExceptionType.OPERATION_NOT_FOUND.getCode(), error.getErrorItems().getErrorItems().get(0).getCode());
            assertEquals("Operation not found with id {0}", error.getErrorItems().getErrorItems().get(0).getMessage());
            assertEquals(1, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().size());
            assertEquals(NOT_EXISTS, error.getErrorItems().getErrorItems().get(0).getParameters().getParameters().get(0));
        } catch (Exception e) {
            fail("Incorrect exception");
        }
    }

    @Test
    public void testRetrieveSurveyTypesXml() throws Exception {

        // Retrieve
        SurveyTypes surveyTypes = getStatisticalOperationsRestInternalFacadeClientXml().retrieveSurveyTypes();

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsSurveyTypes(StatisticalOperationsRestMocks.mockSurveyTypes(), surveyTypes);
    }

    @Test
    public void testRetrieveSurveyTypesXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveSurveyTypes();
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveSurveyTypes.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveOfficialityTypesXml() throws Exception {

        // Retrieve
        OfficialityTypes officialityTypes = getStatisticalOperationsRestInternalFacadeClientXml().retrieveOfficialityTypes();

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsOfficialityTypes(StatisticalOperationsRestMocks.mockOfficialityTypes(), officialityTypes);
    }

    @Test
    public void testRetrieveOfficialityTypesXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveOfficialityTypes();
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveOfficialityTypes.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveInstanceTypesXml() throws Exception {

        // Retrieve
        InstanceTypes instanceTypes = getStatisticalOperationsRestInternalFacadeClientXml().retrieveInstanceTypes();

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsInstanceTypes(StatisticalOperationsRestMocks.mockInstanceTypes(), instanceTypes);
    }

    @Test
    public void testRetrieveInstanceTypesXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveInstanceTypes();
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveInstanceTypes.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveSurveySourcesXml() throws Exception {

        // Retrieve
        SurveySources surveySources = getStatisticalOperationsRestInternalFacadeClientXml().retrieveSurveySources();

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsSurveySources(StatisticalOperationsRestMocks.mockSurveySources(), surveySources);
    }

    @Test
    public void testRetrieveSurveySourcesXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveSurveySources();
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveSurveySources.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveCollMethodsXml() throws Exception {

        // Retrieve
        CollMethods collMethods = getStatisticalOperationsRestInternalFacadeClientXml().retrieveCollMethods();

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsCollMethods(StatisticalOperationsRestMocks.mockCollMethods(), collMethods);
    }

    @Test
    public void testRetrieveCollMethodsXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveCollMethods();
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveCollMethods.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    @Test
    public void testRetrieveCostsXml() throws Exception {

        // Retrieve
        Costs costs = getStatisticalOperationsRestInternalFacadeClientXml().retrieveCosts();

        // Validation
        StatisticalOperationsRestAsserts.assertEqualsCosts(StatisticalOperationsRestMocks.mockCosts(), costs);
    }

    @Test
    public void testRetrieveCostsXmlWithoutJaxbTransformation() throws Exception {

        String requestUri = getRequestUriRetrieveCosts();
        InputStream responseExpected = StatisticalOperationsRestInternalFacadeV10Test.class.getResourceAsStream("/responses/retrieveCosts.xml");

        // Request and validate
        testRequestWithoutJaxbTransformation(requestUri, APPLICATION_XML, Status.OK, responseExpected);
    }

    private String getRequestUriRetrieveOperationById(String operationId) {
        return baseApi + "/operations/" + operationId;
    }

    private String encodeParameter(String parameter) throws Exception {
        if (parameter == null) {
            return null;
        }
        parameter = UriUtils.encodePath(parameter, "UTF-8");
        return parameter;
    }

    private String getRequestUriFindOperations(String query, String limit, String offset) throws Exception {
        String url = baseApi + "/operations";
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_QUERY, encodeParameter(query));
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_LIMIT, limit);
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_OFFSET, offset);
        return url;
    }

    private String getRequestUriRetrieveFamilyById(String familyId) {
        return baseApi + "/families/" + familyId;
    }

    private String getRequestUriFindFamilies(String query, String limit, String offset) throws Exception {
        String url = baseApi + "/families";
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_QUERY, encodeParameter(query));
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_LIMIT, limit);
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_OFFSET, offset);
        return url;
    }

    private String getRequestUriRetrieveInstanceById(String operationId, String instanceId) {
        return baseApi + "/operations/" + operationId + "/instances/" + instanceId;
    }

    private String getRequestUriFindInstances(String operationId, String query, String limit, String offset) throws Exception {
        String url = baseApi + "/operations/" + operationId + "/instances";
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_QUERY, encodeParameter(query));
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_LIMIT, limit);
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_OFFSET, offset);
        return url;
    }

    private String getRequestUriRetrieveFamiliesByOperation(String operationId) {
        return baseApi + "/operations/" + operationId + "/families";
    }

    private String getRequestUriFindOperationsByFamily(String familyId, String limit, String offset) {
        String url = baseApi + "/families/" + familyId + "/operations";
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_LIMIT, limit);
        url = RestUtils.createLinkWithQueryParam(url, RestConstants.PARAMETER_OFFSET, offset);
        return url;
    }

    private String getRequestUriRetrieveSurveyTypes() {
        return baseApi + "/surveyTypes";
    }

    private String getRequestUriRetrieveOfficialityTypes() {
        return baseApi + "/officialityTypes";
    }

    private String getRequestUriRetrieveInstanceTypes() {
        return baseApi + "/instanceTypes";
    }

    private String getRequestUriRetrieveSurveySources() {
        return baseApi + "/surveySources";
    }

    private String getRequestUriRetrieveCollMethods() {
        return baseApi + "/collMethods";
    }

    private String getRequestUriRetrieveCosts() {
        return baseApi + "/costs";
    }

    private static void setUpMockito() throws MetamacException {
        // MOCKS
        StatisticalOperationsBaseService statisticalOperationsBaseService = applicationContext.getBean(StatisticalOperationsBaseService.class);
        StatisticalOperationsListsService statisticalOperationsListsService = applicationContext.getBean(StatisticalOperationsListsService.class);
        // CommonMetadataRestInternalFacade commonMetadataRestInternalFacade = applicationContext.getBean(CommonMetadataRestInternalFacade.class); // TODO

        // Retrieve operations
        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq(OPERATION_1))).thenReturn(StatisticalOperationsCoreMocks.mockOperation1());
        when(statisticalOperationsBaseService.findOperationByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
        // Retrieve operations by family
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_1, 25, 0);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_1, 1000, 0);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_1, 2, 0);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_1, 2, 2);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_1, 2, 4);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_1, 2, 7);
        mockitoFindOperationByConditionByFamily(statisticalOperationsBaseService, FAMILY_2, 1, 2);
        // Find operations
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 25, 0, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 1000, 0, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 2, 0, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 2, 2, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 2, 8, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 2, 9, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 25, 0, null);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 25, 0, QUERY_OPERATION_ID_LIKE_1);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 1, 0, QUERY_OPERATION_ID_LIKE_1);
        mockitoFindOperationByCondition(statisticalOperationsBaseService, 25, 0, QUERY_OPERATION_ID_LIKE_1_AND_INDICATORS_SYSTEM);
        // Retrieve family
        when(statisticalOperationsBaseService.findFamilyByCode(any(ServiceContext.class), eq(FAMILY_1))).thenReturn(StatisticalOperationsCoreMocks.mockFamily1());
        when(statisticalOperationsBaseService.findFamilyByCode(any(ServiceContext.class), eq(FAMILY_2))).thenReturn(StatisticalOperationsCoreMocks.mockFamily2());
        when(statisticalOperationsBaseService.findFamilyByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
        mockitoFindFamilyByConditionByOperation(statisticalOperationsBaseService, OPERATION_1);
        // Find families
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 25, 0, null);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 1000, 0, null);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 2, 0, null);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 2, 2, null);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 2, 4, null);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 2, 9, null);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 25, 0, QUERY_FAMILY_ID_LIKE_1);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 1, 0, QUERY_FAMILY_ID_LIKE_1);
        mockitoFindFamilyByCondition(statisticalOperationsBaseService, 1, 1, QUERY_FAMILY_ID_LIKE_1);
        // Retrieve instance
        when(statisticalOperationsBaseService.findInstanceByCode(any(ServiceContext.class), eq(INSTANCE_1))).thenReturn(StatisticalOperationsCoreMocks.mockInstance1());
        when(statisticalOperationsBaseService.findInstanceByCode(any(ServiceContext.class), eq(NOT_EXISTS))).thenReturn(null);
        // Find instances
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 25, 0, null);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 1000, 0, null);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 2, 0, null);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 2, 2, null);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 2, 4, null);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 2, 9, null);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 25, 0, QUERY_FAMILY_ID_LIKE_1);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 1, 0, QUERY_FAMILY_ID_LIKE_1);
        mockitoFindInstanceByConditionByOperation(statisticalOperationsBaseService, OPERATION_1, 1, 1, QUERY_FAMILY_ID_LIKE_1);
        // Lists
        when(statisticalOperationsListsService.findAllSurveyTypes(any(ServiceContext.class))).thenReturn(StatisticalOperationsCoreMocks.mockFindAllSurveyTypes());
        when(statisticalOperationsListsService.findAllOfficialityTypes(any(ServiceContext.class))).thenReturn(StatisticalOperationsCoreMocks.mockFindAllOfficialityTypes());
        when(statisticalOperationsListsService.findAllInstanceTypes(any(ServiceContext.class))).thenReturn(StatisticalOperationsCoreMocks.mockFindAllInstanceTypes());
        when(statisticalOperationsListsService.findAllSurveySources(any(ServiceContext.class))).thenReturn(StatisticalOperationsCoreMocks.mockFindAllSurveySources());
        when(statisticalOperationsListsService.findAllCollMethods(any(ServiceContext.class))).thenReturn(StatisticalOperationsCoreMocks.mockFindAllCollMethods());
        when(statisticalOperationsListsService.findAllCosts(any(ServiceContext.class))).thenReturn(StatisticalOperationsCoreMocks.mockFindAllCosts());

        // External APIS TODO
        // when(commonMetadataRestInternalFacade.retrieveConfigurationById(COMMON_METADATA_1)).thenReturn(StatisticalOperationsRestMocks.mockExternalApiCommonMetadataRetrieveConfiguration1ById());
    }
    private static void mockitoFindOperationByConditionByFamily(StatisticalOperationsBaseService statisticalOperationsBaseService, String family, int limit, int offset) throws MetamacException {
        PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operations = null;
        if (FAMILY_1.equals(family)) {
            operations = StatisticalOperationsCoreMocks.mockOperationsPagedResultByFamily1(String.valueOf(limit), String.valueOf(offset));
        } else if (FAMILY_2.equals(family)) {
            operations = StatisticalOperationsCoreMocks.mockOperationsPagedResultByFamily2(String.valueOf(limit), String.valueOf(offset));
        } else {
            fail("family non supported. Family = " + family);
        }
        // Mock
        when(
                statisticalOperationsBaseService.findOperationByCondition(any(ServiceContext.class), argThat(new FindOperationsByFamilyMatcher(family, null, null)),
                        argThat(new PagingParameterMatcher(PagingParameter.rowAccess(offset, offset + limit, Boolean.TRUE))))).thenReturn(operations);

    }

    private static void mockitoFindOperationByCondition(StatisticalOperationsBaseService statisticalOperationsBaseService, int limit, int offset, String query) throws MetamacException {

        List<ConditionalCriteria> conditionalCriterias = null;
        String querySupported1 = QUERY_OPERATION_ID_LIKE_1;
        String querySupported2 = QUERY_OPERATION_ID_LIKE_1_AND_INDICATORS_SYSTEM;

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
        if (OPERATION_1.equals(operation)) {
            pagedResult = StatisticalOperationsCoreMocks.mockFamiliesNoPagedResultByOperation1();
        } else {
            fail("Operation non supported. Operation = " + operation);
        }
        // Mock
        when(
                statisticalOperationsBaseService.findFamilyByCondition(any(ServiceContext.class), argThat(new FindFamiliesByOperationMatcher(operation)), argThat(new PagingParameterMatcher(
                        PagingParameter.noLimits())))).thenReturn(pagedResult);
    }

    private static void mockitoFindFamilyByCondition(StatisticalOperationsBaseService statisticalOperationsBaseService, int limit, int offset, String query) throws MetamacException {
        List<ConditionalCriteria> conditionalCriterias = null;
        String querySupported1 = QUERY_FAMILY_ID_LIKE_1;

        if (querySupported1.equals(query)) {
            conditionalCriterias = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class).withProperty(FamilyProperties.code()).like("%1%").build();
        }
        when(
                statisticalOperationsBaseService.findFamilyByCondition(any(ServiceContext.class), argThat(new FindFamiliesMatcher(conditionalCriterias, null)), argThat(new PagingParameterMatcher(
                        PagingParameter.rowAccess(offset, offset + limit, Boolean.TRUE))))).thenReturn(
                StatisticalOperationsCoreMocks.mockFamiliesPagedResult(String.valueOf(limit), String.valueOf(offset), query));
    }

    private static void mockitoFindInstanceByConditionByOperation(StatisticalOperationsBaseService statisticalOperationsBaseService, String operation, int limit, int offset, String query)
            throws MetamacException {
        List<ConditionalCriteria> conditionalCriterias = null;
        String querySupported1 = QUERY_INSTANCE_ID_LIKE_1;

        if (querySupported1.equals(query)) {
            conditionalCriterias = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Instance.class).withProperty(InstanceProperties.code()).like("%1%")
                    .build();
        }

        PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> pagedResult = null;
        if (OPERATION_1.equals(operation)) {
            pagedResult = StatisticalOperationsCoreMocks.mockInstancesPagedResultByOperation1(String.valueOf(limit), String.valueOf(offset), query);
        } else {
            fail("Operation non supported. Operation = " + operation);
        }
        // Mock
        when(
                statisticalOperationsBaseService.findInstanceByCondition(any(ServiceContext.class), argThat(new FindInstancesByOperationMatcher(operation, conditionalCriterias, null)),
                        argThat(new PagingParameterMatcher(PagingParameter.rowAccess(offset, offset + limit, Boolean.TRUE))))).thenReturn(pagedResult);
    }

    private StatisticalOperationsRestInternalFacadeV10 getStatisticalOperationsRestInternalFacadeClientXml() {
        WebClient.client(statisticalOperationsRestInternalFacadeClientXml).reset();
        WebClient.client(statisticalOperationsRestInternalFacadeClientXml).accept(APPLICATION_XML);
        return statisticalOperationsRestInternalFacadeClientXml;
    }
}
