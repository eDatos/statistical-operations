package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import java.util.List;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.FamiliesNoPagedResult;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationsPagedResult;

public interface Do2RestInternalMapperV10 {

    // Entities
    public Operation toOperation(org.siemac.metamac.statistical.operations.core.domain.Operation source, String apiUrl);
    public OperationsPagedResult toOperationsPagedResult(PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sources, Integer limit, String apiUrl);
    public OperationsPagedResult toOperationsByFamilyPagedResult(org.siemac.metamac.statistical.operations.core.domain.Family family, PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> sources, Integer limit, String apiUrl);
    public Family toFamily(org.siemac.metamac.statistical.operations.core.domain.Family source, String apiUrl);
    public FamiliesNoPagedResult toFamiliesByOperationNoPagedResult(List<org.siemac.metamac.statistical.operations.core.domain.Family> sources, String apiUrl);
    public Instance toInstance(org.siemac.metamac.statistical.operations.core.domain.Instance source, String apiUrl);
    
    // Other
    public Error toError(Exception exception);
}
