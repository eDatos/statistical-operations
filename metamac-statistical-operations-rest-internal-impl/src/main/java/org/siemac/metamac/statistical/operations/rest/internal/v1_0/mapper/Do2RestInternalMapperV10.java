package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.SimpleItemsNoPagedResult;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public interface Do2RestInternalMapperV10 {

    // Operations
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl);
    public ResourcesPagedResult toOperationsPagedResult(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sources, String query, String orderBy, Integer limit, String apiUrl);
    public ResourcesPagedResult toOperationsByFamilyPagedResult(org.siemac.metamac.statistical.operations.core.domain.Family family,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sources, String query, String orderBy, Integer limit, String apiUrl);

    // Families
    public Family toFamily(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl);
    public ResourcesPagedResult toFamiliesPagedResult(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String query, String orderBy, Integer limit, String apiUrl);
    public ResourcesNoPagedResult toFamiliesByOperationNoPagedResult(List<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl);

    // Instances
    public Instance toInstance(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl);
    public ResourcesPagedResult toInstancesPagedResult(org.siemac.metamac.statistical.operations.core.domain.Operation operation,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> sources, String query, String orderBy, Integer limit, String apiUrl);

    // List of values
    public SimpleItemsNoPagedResult toSurveyTypesNoPagedResult(List<SurveyType> sources, String apiUrl);

    // Other
    public Error toError(Exception exception);
}
