package org.siemac.metamac.statistical.operations.core.serviceimpl;

import java.util.List;

import javax.jws.WebService;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder.ConditionRoot;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.criteria.SculptorCriteria;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacCriteria;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.mapper.ws.external.Do2WebServiceExternalMapper;
import org.siemac.metamac.statistical.operations.core.mapper.ws.external.MetamacCriteriaWebServiceExternal2SculptorCriteriaMapper;
import org.siemac.metamac.statistical.operations.core.mapper.ws.external.SculptorCriteria2MetamacCriteriaWebServiceExternalMapper;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.core.serviceimpl.utils.WebServiceFacadeValidationUtil;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.MetamacExceptionFault;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.MetamacStatisticalOperationsExternalInterfaceV10;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.FindOperationsResult;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.OperationBase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Statistical operations external web service
 */
@WebService(endpointInterface = "org.siemac.metamac.statistical.operations.external.ws.v1_0.MetamacStatisticalOperationsExternalInterfaceV10", targetNamespace = "http://www.siemac.org/metamac/statistical/operations/external/ws/v1.0", serviceName = "MetamacStatisticalOperationsExternal_v1.0", portName = "MetamacStatisticalOperationsExternalSOAP_v1.0")
@Service("metamacStatisticalOperationsExternalInterfaceV10")
public class StatisticalOperationsExternalWebServiceFacadeV10Impl implements MetamacStatisticalOperationsExternalInterfaceV10 {

    @Autowired
    private StatisticalOperationsBaseService                         statisticalOperationsBaseService;

    @Autowired
    private Do2WebServiceExternalMapper                              do2WebServiceExternalMapper;

    @Autowired
    private MetamacCriteriaWebServiceExternal2SculptorCriteriaMapper metamacCriteriaWebServiceExternal2SculptorCriteriaMapper;

    @Autowired
    private SculptorCriteria2MetamacCriteriaWebServiceExternalMapper sculptorCriteria2MetamacCriteriaWebServiceExternalMapper;

    public StatisticalOperationsExternalWebServiceFacadeV10Impl() {
    }

    private ServiceContext getServiceContextWs() {
        return new ServiceContext("adminWsExternal", "adminWsExternal", "wsExternal");
    }

    /**
     * Find operations by criteria.
     * Only find externally published.
     */
    @Override
    public FindOperationsResult findOperations(MetamacCriteria criteria) throws MetamacExceptionFault {
        try {
            // Validation of parameters
            WebServiceFacadeValidationUtil.validateFindOperations(criteria);

            // Transform
            SculptorCriteria sculptorCriteria = metamacCriteriaWebServiceExternal2SculptorCriteriaMapper.getOperationCriteriaMapper().metamacCriteria2SculptorCriteria(criteria);

            // Add condition by proc status to avoid searching not published externally
            ConditionalCriteria procStatusCriteria = ConditionalCriteria.equal(OperationProperties.procStatus(), ProcStatusEnum.PUBLISH_EXTERNALLY);
            sculptorCriteria.getConditions().add(procStatusCriteria);

            // Find
            PagedResult<Operation> result = statisticalOperationsBaseService.findOperationByCondition(getServiceContextWs(), sculptorCriteria.getConditions(), sculptorCriteria.getPagingParameter());

            // Transform
            FindOperationsResult findOperationsResult = sculptorCriteria2MetamacCriteriaWebServiceExternalMapper.pageResultToFindOperationsResult(result, sculptorCriteria.getPageSize());
            return findOperationsResult;
        } catch (MetamacException e) {
            throw do2WebServiceExternalMapper.metamacExceptionToMetamacExceptionFault(e);
        }
    }

    @Override
    public OperationBase retrieveOperation(String code) throws MetamacExceptionFault {
        try {
            // Validation of parameters
            WebServiceFacadeValidationUtil.validateRetrieveOperation(code);

            // Conditions (by code and publish externally)
            ConditionRoot<Operation> criteria = ConditionalCriteriaBuilder.criteriaFor(Operation.class);
            criteria.withProperty(OperationProperties.code()).eq(code);
            criteria.and();
            criteria.withProperty(OperationProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_EXTERNALLY);
            criteria.distinctRoot();
            List<ConditionalCriteria> conditions = criteria.build();
            // Only one result
            PagingParameter pagingParameter = PagingParameter.pageAccess(1, 1);

            // Find operations
            PagedResult<Operation> result = statisticalOperationsBaseService.findOperationByCondition(getServiceContextWs(), conditions, pagingParameter);
            if (result.getValues() != null && result.getValues().size() == 1) {
                Operation operation = result.getValues().get(0);
                OperationBase operationBase = do2WebServiceExternalMapper.operationToOperationBase(operation);
                return operationBase;
            } else {
                throw new MetamacException(ServiceExceptionType.OPERATION_NOT_FOUND, code);
            }
        } catch (MetamacException e) {
            throw do2WebServiceExternalMapper.metamacExceptionToMetamacExceptionFault(e);
        }
    }

    protected StatisticalOperationsBaseService getStatisticalOperationsBaseService() {
        return statisticalOperationsBaseService;
    }
}