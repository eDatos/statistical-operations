package org.siemac.metamac.statistical_operations.rest.internal.v1_0.service;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;

import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.CollMethods;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Costs;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Families;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.InstanceTypes;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Instances;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.SurveySources;
import org.siemac.metamac.statistical_operations.rest.internal.v1_0.domain.SurveyTypes;

@Path("v1.0")
public interface StatisticalOperationsRestInternalFacadeV10 {

    /**
     * Find operations
     * 
     * @param query Clause to filter results by metadata. Accepts AND/OR clauses (see @LogicalOperator) and operators as eq, like... (see @ComparisonOperator)
     * @param orderBy Clause to order the results by metadata
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
     * @param query Clause to filter results by metadata. Accepts AND/OR clauses (see @LogicalOperator) and operators as eq, like... (see @ComparisonOperator)
     * @param orderBy Clause to order the results by metadata
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
     * @param query Clause to filter results by metadata. Accepts AND/OR clauses (see @LogicalOperator) and operators as eq, like... (see @ComparisonOperator)
     * @param orderBy Clause to order the results by metadata
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
     * @param query Clause to filter results by metadata. Accepts AND/OR clauses (see @LogicalOperator) and operators as eq, like... (see @ComparisonOperator)
     * @param orderBy Clause to order the results by metadata
     * @param limit Maximum number of results per page
     * @param offset Position of first result
     * @return List of operations
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
     * @return List of coll methods
     */
    @GET
    @Produces("application/xml")
    @Path("collMethods")
    CollMethods retrieveCollMethods();

    /**
     * Retrieve all costs
     * @return List of costs
     */
    @GET
    @Produces("application/xml")
    @Path("costs")
    Costs retrieveCosts();
}