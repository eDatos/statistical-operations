package org.siemac.metamac.statistical.operations.core.mapper;

import java.math.BigInteger;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.schema.common.v1_0.domain.InternationalString;
import org.siemac.metamac.schema.common.v1_0.domain.LocalisedString;
import org.siemac.metamac.schema.common.v1_0.domain.LocalisedStringList;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacExceptionItem;
import org.siemac.metamac.schema.common.v1_0.domain.MetamacExceptionItemList;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.MetamacExceptionFault;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBase;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.OperationBaseList;
import org.siemac.metamac.statistical.operations.internal.ws.v1_0.domain.ProcStatusType;
import org.springframework.stereotype.Component;

@Component
public class Do2WebServiceMapperImpl implements Do2WebServiceMapper {

    @Override
    public OperationBase operationToOperationBase(Operation source) throws MetamacException {
        if (source == null) {
            return null;
        }
        OperationBase operationBase = new OperationBase();
        operationBase.setUri(source.getUri());
        operationBase.setCode(source.getCode());
        operationBase.setTitle(internationalStringToWebservice(source.getTitle()));
        operationBase.setAcronym(internationalStringToWebservice(source.getAcronym()));
        operationBase.setDescription(internationalStringToWebservice(source.getDescription()));
        operationBase.setObjetive(internationalStringToWebservice(source.getObjective()));
        operationBase.setProcStatus(procStatusToProcStatusType(source.getProcStatus()));

        return operationBase;
    }

    // TODO qué devolver si no hay resultados?
    @Override
    public OperationBaseList operationsToOperationBaseList(List<Operation> sources) throws MetamacException {
        OperationBaseList targets = new OperationBaseList();
        if (sources == null || sources.size() == 0) {
            targets.setTotal(BigInteger.ZERO);
        } else {
            targets.setTotal(BigInteger.valueOf(sources.size()));
            for (Operation source : sources) {
                OperationBase target = operationToOperationBase(source);
                targets.getOperation().add(target);
            }
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
    private InternationalString internationalStringToWebservice(org.siemac.metamac.core.common.ent.domain.InternationalString source) {
        if (source == null) {
            return null;
        }
        // TODO qué devolver si no hay resultados de Localised?

        // InternationalString to InternationalString Ws
        InternationalString internationalString = new InternationalString();
        internationalString.setLocalisedStrings(new LocalisedStringList());
        if (source.getTexts() == null || source.getTexts().size() == 0) {
            internationalString.getLocalisedStrings().setTotal(BigInteger.ZERO);
        } else {
            internationalString.getLocalisedStrings().setTotal(BigInteger.valueOf(source.getTexts().size()));
            // LocalisedString to LocalisedString Ws
            for (org.siemac.metamac.core.common.ent.domain.LocalisedString item : source.getTexts()) {
                LocalisedString localisedString = localisedStringToLocalisedStringWebservice(item);
                internationalString.getLocalisedStrings().getLocalisedString().add(localisedString);
            }
        }

        return internationalString;
    }

    private LocalisedString localisedStringToLocalisedStringWebservice(org.siemac.metamac.core.common.ent.domain.LocalisedString source) {
        LocalisedString localisedString = new LocalisedString();
        localisedString.setLocale(source.getLocale());
        localisedString.setLabel(source.getLabel());
        return localisedString;
    }

    private ProcStatusType procStatusToProcStatusType(ProcStatusEnum source) throws MetamacException {
        if (source == null) {
            return null;
        }
        switch (source) {
            case PUBLISH_EXTERNALLY:
                return ProcStatusType.PUBLISH_EXTERNALLY;
            case PUBLISH_INTERNALLY:
                return ProcStatusType.PUBLISH_INTERNALLY;
            default:
                throw new MetamacException(ServiceExceptionType.UNKNOWN, "ProcStatusEnum non supported in web services: " + source);
        }
    }
}
