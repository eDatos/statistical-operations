package org.siemac.metamac.statistical.operations.core.mapper.ws.internal;

import java.math.BigInteger;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.FindOperationsResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SculptorCriteria2MetamacCriteriaWebServiceInternalMapperImpl implements SculptorCriteria2MetamacCriteriaWebServiceInternalMapper {

    @Autowired
    private Do2WebServiceInternalMapper do2WebServiceInternalMapper;

    @Override
    public FindOperationsResult pageResultToFindOperationsResult(PagedResult<Operation> source, Integer pageSize) throws MetamacException {
        FindOperationsResult target = new FindOperationsResult();
        target.setOperations(do2WebServiceInternalMapper.operationsToOperationBaseList(source.getValues()));

        target.setFirstResult(BigInteger.valueOf(source.getStartRow()));
        target.setMaximumResultSize(BigInteger.valueOf(pageSize)); // when PagingParameter is build as rowAccess pageSize is unknown
        if (source.isTotalCounted()) {
            target.setTotalResults(BigInteger.valueOf(source.getTotalRows()));
        }
        return target;
    }
}