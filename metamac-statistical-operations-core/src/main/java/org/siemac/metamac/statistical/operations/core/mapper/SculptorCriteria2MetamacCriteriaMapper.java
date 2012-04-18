package org.siemac.metamac.statistical.operations.core.mapper;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaResult;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;

public interface SculptorCriteria2MetamacCriteriaMapper {

    MetamacCriteriaResult<FamilyBaseDto> pageResultToMetamacCriteriaResultFamily(PagedResult<Family> source, Integer pageSize);
    MetamacCriteriaResult<OperationBaseDto> pageResultToMetamacCriteriaResultOperation(PagedResult<Operation> source, Integer pageSize);
    MetamacCriteriaResult<InstanceBaseDto> pageResultToMetamacCriteriaResultInstance(PagedResult<Instance> source, Integer pageSize);
}
