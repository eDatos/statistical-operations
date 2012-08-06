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

public interface StatisticalOperationsRestInternalFacadeV10 {

    @GET
    @Produces("application/xml")
    @Path("operations")
    Operations findOperations(@QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, @QueryParam("offset") String offset);

    @GET
    @Produces("application/xml")
    @Path("operations/{id}")
    Operation retrieveOperationById(@PathParam("id") String id);

    @GET
    @Produces("application/xml")
    @Path("operations/{id}/families")
    Families retrieveFamiliesByOperation(@PathParam("id") String id);

    @GET
    @Produces("application/xml")
    @Path("operations/{operationId}/instances")
    Instances findInstances(@PathParam("operationId") String operationId, @QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, 
                @QueryParam("offset") String offset);

    @GET
    @Produces("application/xml")
    @Path("operations/{operationId}/instances/{id}")
    Instance retrieveInstanceById(@PathParam("operationId") String operationId, @PathParam("id") String id);

    @GET
    @Produces("application/xml")
    @Path("families")
    Families findFamilies(@QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, @QueryParam("offset") String offset);

    @GET
    @Produces("application/xml")
    @Path("families/{id}")
    Family retrieveFamilyById(@PathParam("id") String id);

    @GET
    @Produces("application/xml")
    @Path("families/{id}/operations")
    Operations findOperationsByFamily(@PathParam("id") String id, @QueryParam("query") String query, @QueryParam("orderBy") String orderBy, @QueryParam("limit") String limit, 
                @QueryParam("offset") String offset);

    @GET
    @Produces("application/xml")
    @Path("surveyTypes")
    SurveyTypes retrieveSurveyTypes();

    @GET
    @Produces("application/xml")
    @Path("officialityTypes")
    OfficialityTypes retrieveOfficialityTypes();

    @GET
    @Produces("application/xml")
    @Path("instanceTypes")
    InstanceTypes retrieveInstanceTypes();

    @GET
    @Produces("application/xml")
    @Path("surveySources")
    SurveySources retrieveSurveySources();

    @GET
    @Produces("application/xml")
    @Path("collMethods")
    CollMethods retrieveCollMethods();

    @GET
    @Produces("application/xml")
    @Path("costs")
    Costs retrieveCosts();
}