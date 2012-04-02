package org.siemac.metamac.statistical.operations.core.serviceimpl;

import static org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder.criteriaFor;

import java.util.List;

import javax.jws.WebService;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacVersion;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.mapper.Do2WebServiceMapper;
import org.siemac.metamac.statistical.operations.core.mapper.WebService2DoMapper;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.core.serviceimpl.utils.WebServiceFacadeValidationUtil;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacExceptionFault;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacStatisticalOperationsInternalInterfaceV10;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBase;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBaseList;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationCriteria;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * Statistical operations internal web service
 */
@WebService(endpointInterface = "org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacStatisticalOperationsInternalInterfaceV10", targetNamespace = "http://www.siemac.org/metamac/statistical/operations/internal/ws/v1.0", serviceName = "MetamacStatisticalOperationsInternal_v1.0", portName = "MetamacStatisticalOperationsInternalSOAP_v1.0")
@Service("metamacStatisticalOperationsInternalInterfaceV10")
public class StatisticalOperationsInternalWebServiceFacadeV10Impl implements MetamacStatisticalOperationsInternalInterfaceV10 {

    @Autowired
    private StatisticalOperationsBaseService statisticalOperationsBaseService;

    @Autowired
    private Do2WebServiceMapper              do2WebServiceMapper;

    @Autowired
    private WebService2DoMapper              webService2DoMapper;

    private static String                    STATISTICAL_OPERATIONS_WS_VERSION = "v1_0";

    public StatisticalOperationsInternalWebServiceFacadeV10Impl() {
    }

    private ServiceContext getServiceContextWs() {
        ServiceContext ctx = new ServiceContext("webService", "webService", "webService"); // TODO ServiceContext en web services
        return ctx;
    }

    /**
     * Find operations by criteria.
     * If procStatus is not provided, find internally and externally published. Otherwise, must be internally or externally
     */
    @Override
    public OperationBaseList findOperations(OperationCriteria criteria) throws MetamacExceptionFault {
        try {
            // Validation of parameters
            WebServiceFacadeValidationUtil.validateFindOperations(criteria);

            // TODO criteria tipo Indicadores

            // Condition proc status
            ConditionalCriteria procStatusCriteria = null;
            if (criteria.getProcStatus() != null) {
                // TODO validar posibles valores?
                ProcStatusEnum procStatusEnum = webService2DoMapper.procStatusTypeToProcStatusEnum(criteria.getProcStatus());
                procStatusCriteria = ConditionalCriteria.equal(OperationProperties.procStatus(), procStatusEnum);
            } else {
                procStatusCriteria = ConditionalCriteria.or(ConditionalCriteria.equal(OperationProperties.procStatus(), ProcStatusEnum.PUBLISH_INTERNALLY),
                        ConditionalCriteria.equal(OperationProperties.procStatus(), ProcStatusEnum.PUBLISH_EXTERNALLY));
            }

            // Condition is indicator system
            ConditionalCriteria isIndicatorSystemCriteria = ConditionalCriteria.equal(OperationProperties.indicatorSystem(), criteria.isIsIndicatorsSystem());

            // Conditions AND
            List<ConditionalCriteria> conditions = criteriaFor(Operation.class).build();
            conditions.add(ConditionalCriteria.and(procStatusCriteria, isIndicatorSystemCriteria));

            // Find
            List<Operation> operations = statisticalOperationsBaseService.findOperationByCondition(getServiceContextWs(), conditions);
            OperationBaseList operationBaseList = do2WebServiceMapper.operationsToOperationBaseList(operations);
            return operationBaseList;
        } catch (MetamacException e) {
            throw do2WebServiceMapper.metamacExceptionToMetamacExceptionFault(e);
        }
    }

    @Override
    public OperationBase retrieveOperation(String code) throws MetamacExceptionFault {
        try {
            // Validation of parameters
            WebServiceFacadeValidationUtil.validateRetrieveOperation(code);

            // Condition code
            ConditionalCriteria codeConditional = ConditionalCriteria.equal(OperationProperties.code(), code);

            // Publish internally or externally
            ConditionalCriteria procStatusCriteria = ConditionalCriteria.or(ConditionalCriteria.equal(OperationProperties.procStatus(), ProcStatusEnum.PUBLISH_INTERNALLY),
                    ConditionalCriteria.equal(OperationProperties.procStatus(), ProcStatusEnum.PUBLISH_EXTERNALLY));

            // Conditions AND
            List<ConditionalCriteria> conditions = criteriaFor(Operation.class).build();
            conditions.add(ConditionalCriteria.and(procStatusCriteria, codeConditional));

            // Find operation with code and published
            List<Operation> operations = statisticalOperationsBaseService.findOperationByCondition(getServiceContextWs(), conditions);
            if (operations != null && operations.size() == 1) {
                Operation operation = operations.get(0);
                OperationBase operationBase = do2WebServiceMapper.operationToOperationBase(operation);
                return operationBase;
            } else if (operations.size() >= 1) {
                throw new MetamacException(ServiceExceptionType.UNKNOWN, "Found more than one operation with code " + code);
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
