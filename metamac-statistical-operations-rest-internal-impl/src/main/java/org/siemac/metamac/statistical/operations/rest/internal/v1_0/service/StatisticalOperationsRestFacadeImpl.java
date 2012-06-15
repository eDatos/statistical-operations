package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import java.util.List;

import javax.ws.rs.core.Response;

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
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationBase;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.OperationBaseType;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper.Do2RestInternalMapper;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.service.StatisticalOperationsRestFacade;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service("statisticalOperationsRestFacade")
public class StatisticalOperationsRestFacadeImpl implements StatisticalOperationsRestFacade {

    @Autowired
    private StatisticalOperationsBaseService statisticalOperationsBaseService;

    @Autowired
    private Do2RestInternalMapper            do2RestInternalMapper;

    private ServiceContext                   serviceContextRestInternal = new ServiceContext("restInternal", "restInternal", "restInternal"); // TODO

    @Override
    public Response findOperations() {
        // TODO
        return null;
    }

    @Override
    public OperationBase retrieveOperationByCode(String code) {
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
                OperationBase operationBase = do2RestInternalMapper.operationToOperationBase(operation);
                return operationBase;
            } else {
                // TODO
                OperationBase operationBase = new OperationBase();
                operationBase.setOperationBase(new OperationBaseType());
                operationBase.getOperationBase().setCode("NOT_EXISTS");
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