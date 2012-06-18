package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import java.math.BigInteger;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder.ConditionRoot;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operations;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper.Do2RestInternalMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
    public org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation retrieveOperationByCode(String code) {
        try {
            // TODO Validation of parameters

            // Conditions (by code and publish internally or externally)
            ConditionRoot<Operation> criteria = ConditionalCriteriaBuilder.criteriaFor(Operation.class);
            criteria.withProperty(OperationProperties.code()).eq(code);
            criteria.and();
            criteria.lbrace();
            criteria.withProperty(OperationProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_INTERNALLY);
            criteria.or();
            criteria.withProperty(OperationProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_EXTERNALLY);
            criteria.rbrace();
            criteria.distinctRoot();
            List<ConditionalCriteria> conditions = criteria.build();
            // Only one result
            PagingParameter pagingParameter = PagingParameter.pageAccess(1, 1);

            // Find operations
            PagedResult<Operation> result = statisticalOperationsBaseService.findOperationByCondition(serviceContextRestInternal, conditions, pagingParameter);
            if (result != null && result.getValues() != null && result.getValues().size() == 1) {
                Operation operation = result.getValues().get(0);
                org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation operationBase = do2RestInternalMapper.operationToOperationBase(operation);
                return operationBase;
                
            } else {
                // TODO
                org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation operationBase = new org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation();
                operationBase.setCode("NOT_EXISTS");
                return operationBase;
//                throw new MetamacException(ServiceExceptionType.OPERATION_NOT_FOUND, code);
            }
        } catch (MetamacException e) {
            // TODO error, con código y título
            // return Response.status(errorStatus).entity(sdmx21ErrorResponse).build();
            return null;
        }
    }
}