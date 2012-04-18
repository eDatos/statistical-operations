package org.siemac.metamac.statistical.operations.core.mapper;

import org.siemac.metamac.core.common.criteria.mapper.MetamacCriteria2SculptorCriteria;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;

public interface MetamacCriteria2SculptorCriteriaMapper {

    public MetamacCriteria2SculptorCriteria<Family> getFamilyCriteriaMapper();
    public MetamacCriteria2SculptorCriteria<Operation> getOperationCriteriaMapper();
    public MetamacCriteria2SculptorCriteria<Instance> getInstanceCriteriaMapper();
}
