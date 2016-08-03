package org.siemac.metamac.statistical.operations.core.mapper;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaResult;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.dto.OperationBaseDto;

public interface SculptorCriteria2MetamacCriteriaMapper {

    MetamacCriteriaResult<FamilyBaseDto> pageResultToMetamacCriteriaResultFamily(PagedResult<Family> source, Integer pageSize) throws MetamacException;
    MetamacCriteriaResult<OperationBaseDto> pageResultToMetamacCriteriaResultOperation(PagedResult<Operation> source, Integer pageSize) throws MetamacException;
    MetamacCriteriaResult<InstanceBaseDto> pageResultToMetamacCriteriaResultInstance(PagedResult<Instance> source, Integer pageSize) throws MetamacException;
}
