package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.SimpleItemsNoPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public interface StatisticalOperationsRestInternalFacadeV10 {

    @GET
    @Produces("application/xml")
    @Path("operations")
    ResourcesPagedResult findOperations(@QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, @QueryParam("offset") String offset);

    @GET
    @Produces("application/xml")
    @Path("operations/{id}")
    Operation retrieveOperationById(@PathParam("id") String id);

    @GET
    @Produces("application/xml")
    @Path("operations/{id}/families")
    ResourcesNoPagedResult retrieveFamiliesByOperation(@PathParam("id") String id);

    @GET
    @Produces("application/xml")
    @Path("operations/{operationId}/instances")
    ResourcesPagedResult findInstances(@PathParam("operationId") String operationId, @QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, 
                @QueryParam("offset") String offset);

    @GET
    @Produces("application/xml")
    @Path("operations/{operationId}/instances/{id}")
    Instance retrieveInstanceById(@PathParam("operationId") String operationId, @PathParam("id") String id);

    @GET
    @Produces("application/xml")
    @Path("families")
    ResourcesPagedResult findFamilies(@QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, @QueryParam("offset") String offset);

    @GET
    @Produces("application/xml")
    @Path("families/{id}")
    Family retrieveFamilyById(@PathParam("id") String id);

    @GET
    @Produces("application/xml")
    @Path("families/{id}/operations")
    ResourcesPagedResult findOperationsByFamily(@PathParam("id") String id, @QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, 
                @QueryParam("offset") String offset);

    @GET
    @Produces("application/xml")
    @Path("surveyTypes")
    SimpleItemsNoPagedResult retrieveSurveyTypes();

    @GET
    @Produces("application/xml")
    @Path("officialityTypes")
    SimpleItemsNoPagedResult retrieveOfficialityTypes();

    @GET
    @Produces("application/xml")
    @Path("instanceTypes")
    SimpleItemsNoPagedResult retrieveInstanceTypes();

    @GET
    @Produces("application/xml")
    @Path("surveySources")
    SimpleItemsNoPagedResult retrieveSurveySources();

    @GET
    @Produces("application/xml")
    @Path("collMethods")
    SimpleItemsNoPagedResult retrieveCollMethods();

    @GET
    @Produces("application/xml")
    @Path("costs")
    SimpleItemsNoPagedResult retrieveCosts();

}