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
import org.siemac.metamac.schema.common.v1_0.domain.MetamacVersion;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.mapper.ws.internal.Do2WebServiceInternalMapper;
import org.siemac.metamac.statistical.operations.core.mapper.ws.internal.MetamacCriteriaWebServiceInternal2SculptorCriteriaMapper;
import org.siemac.metamac.statistical.operations.core.mapper.ws.internal.SculptorCriteria2MetamacCriteriaWebServiceInternalMapper;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.core.serviceimpl.utils.WebServiceFacadeValidationUtil;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacExceptionFault;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacStatisticalOperationsInternalInterfaceV10;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.FindOperationsResult;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBase;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Statistical operations internal web service
 */
@WebService(endpointInterface = "org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacStatisticalOperationsInternalInterfaceV10", targetNamespace = "http://www.siemac.org/metamac/statistical/operations/internal/ws/v1.0", serviceName = "MetamacStatisticalOperationsInternal_v1.0", portName = "MetamacStatisticalOperationsInternalSOAP_v1.0")
@Service("metamacStatisticalOperationsInternalInterfaceV10")
public class StatisticalOperationsInternalWebServiceFacadeV10Impl implements MetamacStatisticalOperationsInternalInterfaceV10 {

    @Autowired
    private StatisticalOperationsBaseService                         statisticalOperationsBaseService;

    @Autowired
    private Do2WebServiceInternalMapper                              do2WebServiceMapper;

    @Autowired
    private MetamacCriteriaWebServiceInternal2SculptorCriteriaMapper metamacCriteriaWebServiceInternal2SculptorCriteriaMapper;

    @Autowired
    private SculptorCriteria2MetamacCriteriaWebServiceInternalMapper sculptorCriteria2MetamacCriteriaWebServiceInternalMapper;

    private static String                                            STATISTICAL_OPERATIONS_WS_VERSION = "v1_0";

    public StatisticalOperationsInternalWebServiceFacadeV10Impl() {
    }

    private ServiceContext getServiceContextWs() {
        return new ServiceContext("adminWsInternal", "adminWsInternal", "wsInternal");
    }

    /**
     * Find operations by criteria.
     * If procStatus is not provided, find internally and externally published. Otherwise, must be internally or externally
     */
    @Override
    public FindOperationsResult findOperations(MetamacCriteria criteria) throws MetamacExceptionFault {
        try {
            // Validation of parameters
            WebServiceFacadeValidationUtil.validateFindOperations(criteria);

            // Transform
            SculptorCriteria sculptorCriteria = metamacCriteriaWebServiceInternal2SculptorCriteriaMapper.getOperationCriteriaMapper().metamacCriteria2SculptorCriteria(criteria);

            // Add condition by proc status to avoid searching not published
            ConditionalCriteria procStatusCriteria = ConditionalCriteria.or(ConditionalCriteria.equal(OperationProperties.procStatus(), ProcStatusEnum.PUBLISH_INTERNALLY),
                    ConditionalCriteria.equal(OperationProperties.procStatus(), ProcStatusEnum.PUBLISH_EXTERNALLY));
            sculptorCriteria.getConditions().add(procStatusCriteria);

            // Find
            PagedResult<Operation> result = statisticalOperationsBaseService.findOperationByCondition(getServiceContextWs(), sculptorCriteria.getConditions(), sculptorCriteria.getPagingParameter());

            // Transform
            FindOperationsResult findOperationsResult = sculptorCriteria2MetamacCriteriaWebServiceInternalMapper.pageResultToFindOperationsResult(result, sculptorCriteria.getPageSize());
            return findOperationsResult;
        } catch (MetamacException e) {
            throw do2WebServiceMapper.metamacExceptionToMetamacExceptionFault(e);
        }
    }

    @Override
    public OperationBase retrieveOperation(String code) throws MetamacExceptionFault {
        try {
            // Validation of parameters
            WebServiceFacadeValidationUtil.validateRetrieveOperation(code);

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
            PagedResult<Operation> result = statisticalOperationsBaseService.findOperationByCondition(getServiceContextWs(), conditions, pagingParameter);
            if (result.getValues() != null && result.getValues().size() == 1) {
                Operation operation = result.getValues().get(0);
                OperationBase operationBase = do2WebServiceMapper.operationToOperationBase(operation);
                return operationBase;
            } else {
                throw new MetamacException(ServiceExceptionType.OPERATION_NOT_FOUND, code);
            }
        } catch (MetamacException e) {
            throw do2WebServiceMapper.metamacExceptionToMetamacExceptionFault(e);
        }
    }

    @Override
    public MetamacVersion retrieveVersion() throws MetamacExceptionFault {

        try {
            // Validation of parameters
            WebServiceFacadeValidationUtil.validateRetrieveVersion();

            // Retrieve version
            MetamacVersion metamacVersion = new MetamacVersion();
            metamacVersion.setServiceVersion(STATISTICAL_OPERATIONS_WS_VERSION);
            return metamacVersion;
        } catch (MetamacException e) {
            throw do2WebServiceMapper.metamacExceptionToMetamacExceptionFault(e);
        }
    }

    protected StatisticalOperationsBaseService getStatisticalOperationsBaseService() {
        return statisticalOperationsBaseService;
    }
}
