package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import java.math.BigInteger;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper.Do2RestInternalMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

// TODO uri
@Service("statisticalOperationsRestFacadeV10")
public class StatisticalOperationsRestFacadeV10Impl implements StatisticalOperationsRestFacadeV10 {

    @Autowired
    private StatisticalOperationsBaseService statisticalOperationsBaseService;

    @Autowired
    private Do2RestInternalMapper            do2RestInternalMapper;

    private ServiceContext                   serviceContextRestInternal = new ServiceContext("restInternal", "restInternal", "restInternal"); // TODO

    @Override
    public Operations findOperations() {
        Operations operations = new Operations();
        operations.setSize(BigInteger.valueOf(2));
        {
            org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation operation = new org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation();
            operation.setCode("1aaa1");
            operations.getItems().add(operation);
        }
        {
            org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation operation = new org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation();
            operation.setCode("2aaa1");
            operations.getItems().add(operation);
        }
        return operations;
    }

    @Override
    public Operation retrieveOperationByCode(String code) {
        try {
            // TODO Validation of parameters

            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = statisticalOperationsBaseService.findOperationByCode(serviceContextRestInternal, code);
            if (operationEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationEntity.getProcStatus()))) {
                // TODO
                Operation operation = new org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation();
                operation.setCode("NOT_EXISTS");
                return operation;
//                throw new MetamacException(ServiceExceptionType.OPERATION_NOT_FOUND, code);
            }
            Operation operation = do2RestInternalMapper.toOperation(operationEntity);
            return operation;
        } catch (MetamacException e) {
            // TODO error, con código y título
            // return Response.status(errorStatus).entity(sdmx21ErrorResponse).build();
            return null;
        }
    }
}