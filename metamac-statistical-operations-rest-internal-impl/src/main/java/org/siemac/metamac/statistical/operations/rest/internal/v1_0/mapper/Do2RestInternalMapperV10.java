package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.CollMethods;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Costs;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Families;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.InstanceTypes;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instances;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.SurveySources;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.SurveyTypes;

public interface Do2RestInternalMapperV10 {

    // Operations
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl);
    public Operations toOperations(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sources, String query, String orderBy, Integer limit, String apiUrl);
    public Operations toOperationsByFamily(org.siemac.metamac.statistical.operations.core.domain.Family family,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sources, String query, String orderBy, Integer limit, String apiUrl);

    // Families
    public Family toFamily(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl);
    public Families toFamilies(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String query, String orderBy, Integer limit, String apiUrl);
    public Families toFamiliesByOperation(List<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl);

    // Instances
    public Instance toInstance(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl);
    public Instances toInstances(org.siemac.metamac.statistical.operations.core.domain.Operation operation,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> sources, String query, String orderBy, Integer limit, String apiUrl);

    // List of values
    public SurveyTypes toSurveyTypes(List<SurveyType> sources);
    public OfficialityTypes toOfficialityTypes(List<OfficialityType> entitiesResult);
    public InstanceTypes toInstanceTypes(List<InstanceType> entitiesResult);
    public SurveySources toSurveySources(List<SurveySource> entitiesResult);
    public CollMethods toCollMethods(List<CollMethod> entitiesResult);
    public Costs toCosts(List<Cost> entitiesResult);

    // Other
    public Error toError(Exception exception);
}
