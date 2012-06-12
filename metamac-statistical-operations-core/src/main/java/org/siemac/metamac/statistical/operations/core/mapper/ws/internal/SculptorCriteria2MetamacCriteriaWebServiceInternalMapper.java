package org.siemac.metamac.statistical.operations.core.mapper.ws.internal;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.FindOperationsResult;

public interface SculptorCriteria2MetamacCriteriaWebServiceInternalMapper {

    public FindOperationsResult pageResultToFindOperationsResult(PagedResult<Operation> source, Integer pageSize) throws MetamacException;
}
