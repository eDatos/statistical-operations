package org.siemac.metamac.statistical.operations.core.mapper;

import java.math.BigInteger;

import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.FindOperationsResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SculptorCriteria2MetamacCriteriaWebServiceMapperImpl implements SculptorCriteria2MetamacCriteriaWebServiceMapper {

    @Autowired
    private Do2WebServiceMapper do2WebServiceMapper;

    @Override
    public FindOperationsResult pageResultToFindOperationsResult(PagedResult<Operation> source, Integer pageSize) throws MetamacException {
        FindOperationsResult target = new FindOperationsResult();
        target.setOperations(do2WebServiceMapper.operationsToOperationBaseList(source.getValues()));

        // TODO pasar a librería común cuando se haga el resto de servicios web, y se decida si son SOAP o REST 
        target.setFirstResult(BigInteger.valueOf(source.getStartRow()));
        target.setMaximumResultSize(BigInteger.valueOf(pageSize)); // when PagingParameter is build as rowAccess pageSize is unknown
        if (source.isTotalCounted()) {
            target.setTotalResults(BigInteger.valueOf(source.getTotalRows()));
        }
        return target;
    }
}