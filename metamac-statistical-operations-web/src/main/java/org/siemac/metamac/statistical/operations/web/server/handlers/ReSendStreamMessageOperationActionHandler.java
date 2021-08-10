package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.ArrayList;
import java.util.List;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItem;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.core.serviceimpl.result.ReSendStreamMessageOperationServiceResult;
import org.siemac.metamac.statistical.operations.web.server.rest.NoticesRestInternalFacade;
import org.siemac.metamac.statistical.operations.web.server.rest.serviceapi.ExternalItemValidator;
import org.siemac.metamac.statistical.operations.web.shared.ReSendStreamMessageOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.ReSendStreamMessageOperationResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class ReSendStreamMessageOperationActionHandler extends SecurityActionHandler<ReSendStreamMessageOperationAction, ReSendStreamMessageOperationResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    @Autowired
    private ExternalItemValidator externalItemValidator;

    @Autowired
    private NoticesRestInternalFacade noticesRestInternalFacade;

    public ReSendStreamMessageOperationActionHandler() {
        super(ReSendStreamMessageOperationAction.class);
    }

    @Override
    public ReSendStreamMessageOperationResult executeSecurityAction(ReSendStreamMessageOperationAction action) throws ActionException {
        ServiceContext serviceContext = ServiceContextHolder.getCurrentServiceContext();
        ReSendStreamMessageOperationServiceResult result;

        try {
            OperationDto operationToPublish = statisticalOperationsServiceFacade.findOperationById(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            checkExternalItemsAreExternallyPublished(serviceContext, operationToPublish);
            result = statisticalOperationsServiceFacade.republishExternallyOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }

        MetamacWebException operationException = null;
        if (!result.isOk()) {
            MetamacException e = result.getMainException();
            List<MetamacExceptionItem> list = new ArrayList<>();
            for (MetamacException exception : result.getSecondaryExceptions()) {
                list.addAll(exception.getExceptionItems());
            }
            e.getExceptionItems().addAll(list);
            operationException = WebExceptionUtils.createMetamacWebException(e);
            try {
                noticesRestInternalFacade.createNotificationForStreamError(serviceContext, result.getContent());
            } catch (MetamacWebException noticeException) {
                operationException.getWebExceptionItems().addAll(noticeException.getWebExceptionItems());
            }
        }

        try {
            noticesRestInternalFacade.createNotificationForPublishExternallyOperation(serviceContext, result.getContent());
        } catch (MetamacWebException e) {
            if (operationException == null) {
                operationException = e;
            } else {
                operationException.getWebExceptionItems().addAll(e.getWebExceptionItems());
            }
        }

        return new ReSendStreamMessageOperationResult(result.getContent(), operationException);
    }

    /**
     * Check that all the external items of an {@link OperationDto} are externally published
     *
     * @param operationDto
     * @throws MetamacWebException
     */
    private void checkExternalItemsAreExternallyPublished(ServiceContext serviceContext, OperationDto operationDto) throws MetamacWebException {

        MetamacWebException metamacWebException = new MetamacWebException();

        // CommonMetadata should be always be externally published, so there is no need to check it
        externalItemValidator.checkExternalItemIsExternallyPublished(serviceContext, ServiceExceptionParameters.OPERATION_SUBJECT_AREA, operationDto.getSubjectArea(), metamacWebException);
        externalItemValidator.checkExternalItemsAreExternallyPublished(serviceContext, ServiceExceptionParameters.OPERATION_SECONDARY_SUBJECT_AREAS, operationDto.getSecondarySubjectAreas(),
                metamacWebException);
        externalItemValidator.checkExternalItemsAreExternallyPublished(serviceContext, ServiceExceptionParameters.OPERATION_PRODUCER, operationDto.getProducer(), metamacWebException);
        externalItemValidator.checkExternalItemsAreExternallyPublished(serviceContext, ServiceExceptionParameters.OPERATION_REGIONAL_RESPONSIBLE, operationDto.getRegionalResponsible(),
                metamacWebException);
        externalItemValidator.checkExternalItemsAreExternallyPublished(serviceContext, ServiceExceptionParameters.OPERATION_REGIONAL_CONTRIBUTOR, operationDto.getRegionalContributor(),
                metamacWebException);
        externalItemValidator.checkExternalItemsAreExternallyPublished(serviceContext, ServiceExceptionParameters.OPERATION_PUBLISHER, operationDto.getPublisher(), metamacWebException);
        externalItemValidator.checkExternalItemsAreExternallyPublished(serviceContext, ServiceExceptionParameters.OPERATION_UPDATE_FREQUENCY, operationDto.getUpdateFrequency(), metamacWebException);

        if (metamacWebException.getWebExceptionItems() != null && !metamacWebException.getWebExceptionItems().isEmpty()) {
            throw metamacWebException;
        }
    }
}
