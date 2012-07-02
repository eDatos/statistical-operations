package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.RelatedResourcesPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;

public interface Do2RestInternalMapperV10 {

    // Operations
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl);
    public RelatedResourcesPagedResult toOperationsPagedResult(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sources, Integer limit, String apiUrl);
    public RelatedResourcesPagedResult toOperationsByFamilyPagedResult(org.siemac.metamac.statistical.operations.core.domain.Family family,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sources, Integer limit, String apiUrl);

    // Families
    public Family toFamily(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl);
    public RelatedResourcesPagedResult toFamiliesPagedResult(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> sources, Integer limit, String apiUrl);
    public RelatedResourcesNoPagedResult toFamiliesByOperationNoPagedResult(List<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl);

    // Instances
    public Instance toInstance(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl);
    public RelatedResourcesPagedResult toInstancesPagedResult(org.siemac.metamac.statistical.operations.core.domain.Operation operation,
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> sources, Integer limit, String apiUrl);

    // Other
    public Error toError(Exception exception);
}
