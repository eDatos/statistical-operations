package org.siemac.metamac.statistical_operations.rest.external.v1_0.mapper;

import org.siemac.metamac.rest.search.criteria.mapper.RestCriteria2SculptorCriteria;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;

public interface RestCriteria2SculptorCriteriaMapper {

    public RestCriteria2SculptorCriteria<Operation> getOperationCriteriaMapper();
    public RestCriteria2SculptorCriteria<Instance> getInstanceCriteriaMapper();
    public RestCriteria2SculptorCriteria<Family> getFamilyCriteriaMapper();
}
