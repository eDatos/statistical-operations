package org.siemac.metamac.statistical.operations.core.mapper.ws.external;

import java.math.BigInteger;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.enume.domain.ProcStatusEnum;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacExceptionItem;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacExceptionItemList;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.core.mapper.ws.Do2WebServiceCommonMapper;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.MetamacExceptionFault;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.OperationBase;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.OperationBaseList;
import org.siemac.metamac.statistical.operations.external.ws.v1_0.domain.ProcStatusType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class Do2WebServiceExternalMapperImpl implements Do2WebServiceExternalMapper {

    @Autowired
    private Do2WebServiceCommonMapper do2WebServiceCommonMapper;

    @Override
    public OperationBase operationToOperationBase(Operation source) throws MetamacException {
        if (source == null) {
            return null;
        }
        OperationBase operationBase = new OperationBase();
        operationBase.setCode(source.getCode());
        operationBase.setTitle(do2WebServiceCommonMapper.internationalStringToWebService(source.getTitle()));
        operationBase.setAcronym(do2WebServiceCommonMapper.internationalStringToWebService(source.getAcronym()));
        operationBase.setDescription(do2WebServiceCommonMapper.internationalStringToWebService(source.getDescription()));
        operationBase.setObjective(do2WebServiceCommonMapper.internationalStringToWebService(source.getObjective()));
        operationBase.setProcStatus(procStatusToProcStatusType(source.getProcStatus()));

        return operationBase;
    }

    @Override
    public OperationBaseList operationsToOperationBaseList(List<Operation> sources) throws MetamacException {
        if (sources == null || sources.size() == 0) {
            return null;
        }
        OperationBaseList targets = new OperationBaseList();
        targets.setTotal(BigInteger.valueOf(sources.size()));
        for (Operation source : sources) {
            OperationBase target = operationToOperationBase(source);
            targets.getOperation().add(target);
        }
        return targets;
    }

    @Override
    public MetamacExceptionFault metamacExceptionToMetamacExceptionFault(MetamacException source) {

        org.siemac.metamac.schema.common.v1_0.domain.MetamacException metamacException = new org.siemac.metamac.schema.common.v1_0.domain.MetamacException();
        metamacException.setExceptionItems(new MetamacExceptionItemList());
        if (source.getExceptionItems() == null || source.getExceptionItems().size() == 0) {
            metamacException.getExceptionItems().setTotal(BigInteger.ZERO);
        } else {
            metamacException.getExceptionItems().setTotal(BigInteger.valueOf(source.getExceptionItems().size()));
            for (org.siemac.metamac.core.common.exception.MetamacExceptionItem sourceItem : source.getExceptionItems()) {
                MetamacExceptionItem metamacExceptionItem = new MetamacExceptionItem();
                metamacExceptionItem.setCode(sourceItem.getCode());
                metamacExceptionItem.setMessage(sourceItem.getMessage());
                if (sourceItem.getMessageParameters() != null) {
                    CollectionUtils.addAll(metamacExceptionItem.getMessageParameters(), sourceItem.getMessageParameters());
                }
                metamacException.getExceptionItems().getMetamacExceptionItem().add(metamacExceptionItem);
            }
        }
        return new MetamacExceptionFault(source.getMessage(), metamacException);
    }

    /**************************************************************************
     * PRIVATE
     **************************************************************************/

    private ProcStatusType procStatusToProcStatusType(ProcStatusEnum source) throws MetamacException {
        if (source == null) {
            return null;
        }
        switch (source) {
            case PUBLISH_EXTERNALLY:
                return ProcStatusType.PUBLISH_EXTERNALLY;
            default:
                throw new MetamacException(ServiceExceptionType.UNKNOWN, "ProcStatusEnum non supported in web services: " + source);
        }
    }
}
