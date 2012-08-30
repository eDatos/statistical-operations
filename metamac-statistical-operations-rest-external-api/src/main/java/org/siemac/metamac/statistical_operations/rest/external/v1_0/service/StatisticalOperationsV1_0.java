package org.siemac.metamac.statistical_operations.rest.external.v1_0.service;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;

import org.siemac.metamac.rest.statistical_operations.v1_0.domain.CollMethods;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Costs;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Families;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Family;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Instance;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.InstanceTypes;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Instances;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Operation;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Operations;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.SurveySources;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.SurveyTypes;

@Path("v1.0")
public interface StatisticalOperationsV1_0 {

    /**
     * Find operations
     * 
     * @param query Clause to filter results by metadata <br/>
     *            - Logical operators: AND, OR <br/>
     *            - Comparison operators: EQ, IEQ, LIKE, ILIKE, NE, LT, LE, GT, GE, IS_NULL, IS_NOT_NULL, IN <br/>
     *            - Metadata to filter: ID, URN, TITLE, ACRONYM, SUBJECT_AREA_URN, SECONDARY_SUBJECT_AREA_URN, DESCRIPTION, SURVEY_TYPE_ID, OFFICIALITY_TYPE_ID, IS_INDICATORS_SYSTEM, PRODUCER_URN,
     *            CURRENTLY_ACTIVE, STATUS, PROC_STATUS, PUBLISHER_URN, INVENTORY_DATE <br/>
     *            - Example: (ID LIKE "Operation1" AND OFFICIALITY_TYPE_ID EQ "OfficialityType1") OR (ACRONYM EQ "Op1") OR (INVENTORY_DATE IS_NULL)
     * @param orderBy Clause to order the results by metadata <br/>
     *            - Order operators: ASC, DESC<br/>
     *            - Metadata to order: ID<br/>
     *            - Example: ID ASC<br/>
     * @param limit Maximum number of results per page
     * @param offset Position of first result
     * @return List of operations
     */
    @GET
    @Produces("application/xml")
    @Path("operations")
    Operations findOperations(@QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, @QueryParam("offset") String offset);

    /**
     * Retrieve operation by id
     * 
     * @param id Id
     * @return Operation
     */
    @GET
    @Produces("application/xml")
    @Path("operations/{id}")
    Operation retrieveOperationById(@PathParam("id") String id);

    /**
     * Retrieve families by operation
     * 
     * @param id Id operation
     * @return List of families
     */
    @GET
    @Produces("application/xml")
    @Path("operations/{id}/families")
    Families retrieveFamiliesByOperation(@PathParam("id") String id);

    /**
     * Find instances by operation
     * 
     * @param operationId Operation id
     * @param query Clause to filter results by metadata <br/>
     *            - Logical operators: AND, OR <br/>
     *            - Comparison operators: EQ, IEQ, LIKE, ILIKE, NE, LT, LE, GT, GE, IS_NULL, IS_NOT_NULL, IN <br/>
     *            - Metadata to filter: ID, URN, TITLE, ACRONYM, DATA_DESCRIPTION, GEOGRAPHIC_GRANULARITY_URN, TEMPORAL_GRANULARITY_URN, INVENTORY_DATE <br/>
     *            - Example: (ID LIKE "Instance1" AND DATA_DESCRIPTION EQ "DataDescription1") OR (ACRONYM EQ "Instance1")
     * @param orderBy Clause to order the results by metadata <br/>
     *            - Order operators: ASC, DESC<br/>
     *            - Metadata to order: ID<br/>
     *            - Example: ID ASC<br/>
     * @param limit Maximum number of results per page
     * @param offset Position of first result
     * @return List of instances
     */
    @GET
    @Produces("application/xml")
    @Path("operations/{operationId}/instances")
    Instances findInstances(@PathParam("operationId") String operationId, @QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit,
            @QueryParam("offset") String offset);

    /**
     * Retrieve instance by id
     * 
     * @param operationId Operation id
     * @param id Instance id
     * @return Instance
     */
    @GET
    @Produces("application/xml")
    @Path("operations/{operationId}/instances/{id}")
    Instance retrieveInstanceById(@PathParam("operationId") String operationId, @PathParam("id") String id);

    /**
     * Find families
     * 
     * @param query Clause to filter results by metadata <br/>
     *            - Logical operators: AND, OR <br/>
     *            - Comparison operators: EQ, IEQ, LIKE, ILIKE, NE, LT, LE, GT, GE, IS_NULL, IS_NOT_NULL, IN <br/>
     *            - Metadata to filter: ID, URN, TITLE, ACRONYM, DESCRIPTION, INVENTORY_DATE <br/>
     *            - Example: (ID LIKE "Family1" AND ACRONYM EQ "Family1") OR (DESCRIPTION IS_NOT_NULL)
     * @param orderBy Clause to order the results by metadata <br/>
     *            - Order operators: ASC, DESC<br/>
     *            - Metadata to order: ID<br/>
     *            - Example: ID ASC<br/>
     * @param limit Maximum number of results per page
     * @param offset Position of first result
     * @return List of families
     */
    @GET
    @Produces("application/xml")
    @Path("families")
    Families findFamilies(@QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, @QueryParam("offset") String offset);

    /**
     * Retrieve family by id
     * 
     * @param id Family id
     * @return Family
     */
    @GET
    @Produces("application/xml")
    @Path("families/{id}")
    Family retrieveFamilyById(@PathParam("id") String id);

    /**
     * Find operations by family
     * 
     * @param id Family id
     * @param query Clause to filter results by metadata <br/>
     *            - Logical operators: AND, OR <br/>
     *            - Comparison operators: EQ, IEQ, LIKE, ILIKE, NE, LT, LE, GT, GE, IS_NULL, IS_NOT_NULL, IN <br/>
     *            - Metadata to filter: ID, URN, TITLE, ACRONYM, SUBJECT_AREA_URN, SECONDARY_SUBJECT_AREA_URN, DESCRIPTION, SURVEY_TYPE_ID, OFFICIALITY_TYPE_ID, IS_INDICATORS_SYSTEM, PRODUCER_URN,
     *            CURRENTLY_ACTIVE, STATUS, PROC_STATUS, PUBLISHER_URN, INVENTORY_DATE <br/>
     *            - Example: (ID LIKE "Operation1" AND OFFICIALITY_TYPE_ID EQ "OfficialityType1") OR (ACRONYM EQ "Op1") OR (INVENTORY_DATE IS_NULL)
     * @param orderBy Clause to order the results by metadata <br/>
     *            - Order operators: ASC, DESC<br/>
     *            - Metadata to order: ID<br/>
     *            - Example: ID ASC<br/>
     * @param limit Maximum number of results per page
     * @param offset Position of first result
     * @return List of operations of family
     */
    @GET
    @Produces("application/xml")
    @Path("families/{id}/operations")
    Operations findOperationsByFamily(@PathParam("id") String id, @QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit,
            @QueryParam("offset") String offset);

    /**
     * Retrieve all survey types
     * 
     * @return List of survey types
     */
    @GET
    @Produces("application/xml")
    @Path("surveyTypes")
    SurveyTypes retrieveSurveyTypes();

    /**
     * Retrieve all officiality types
     * 
     * @return List of officiality types
     */
    @GET
    @Produces("application/xml")
    @Path("officialityTypes")
    OfficialityTypes retrieveOfficialityTypes();

    /**
     * Retrieve all instance types
     * 
     * @return List of instance types
     */
    @GET
    @Produces("application/xml")
    @Path("instanceTypes")
    InstanceTypes retrieveInstanceTypes();

    /**
     * Retrieve all survey sources
     * 
     * @return List of survey sources
     */
    @GET
    @Produces("application/xml")
    @Path("surveySources")
    SurveySources retrieveSurveySources();

    /**
     * Retrieve all coll methods
     * 
     * @return List of coll methods
     */
    @GET
    @Produces("application/xml")
    @Path("collMethods")
    CollMethods retrieveCollMethods();

    /**
     * Retrieve all costs
     * 
     * @return List of costs
     */
    @GET
    @Produces("application/xml")
    @Path("costs")
    Costs retrieveCosts();
}