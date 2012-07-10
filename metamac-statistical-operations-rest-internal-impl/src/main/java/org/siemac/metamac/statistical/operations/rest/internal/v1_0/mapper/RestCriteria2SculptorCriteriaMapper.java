package org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper;

import org.siemac.metamac.rest.search.criteria.mapper.RestCriteria2SculptorCriteria;
import org.siemac.metamac.statistical.operations.core.domain.Operation;

public interface RestCriteria2SculptorCriteriaMapper {

    public RestCriteria2SculptorCriteria<Operation> getOperationCriteriaMapper();
}
