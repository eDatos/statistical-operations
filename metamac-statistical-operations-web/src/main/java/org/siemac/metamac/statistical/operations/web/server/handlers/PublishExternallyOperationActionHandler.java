package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.CommonServiceExceptionType;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.core.common.exception.MetamacExceptionItemBuilder;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.rest.NoticesRestInternalService;
import org.siemac.metamac.statistical.operations.web.server.rest.serviceapi.ExternalItemValidator;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishExternallyOperationActionHandler extends SecurityActionHandler<PublishExternallyOperationAction, PublishExternallyOperationResult> {
    private static final Logger LOGGER = LoggerFactory.getLogger(PublishExternallyOperationActionHandler.class);

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    @Autowired
    private ExternalItemValidator              externalItemValidator;

    @Autowired
    private NoticesRestInternalService         noticesRestInternalService;

    public PublishExternallyOperationActionHandler() {
        super(PublishExternallyOperationAction.class);
    }

    @Override
    public PublishExternallyOperationResult executeSecurityAction(PublishExternallyOperationAction action) throws ActionException {
        ServiceContext serviceContext = ServiceContextHolder.getCurrentServiceContext();
        OperationDto operationToPublish;
        OperationDto operationPublished;

        try {
            operationToPublish = statisticalOperationsServiceFacade.findOperationById(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            checkExternalItemsAreExternallyPublished(serviceContext, operationToPublish);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }

        try {
            operationPublished = statisticalOperationsServiceFacade.publishExternallyOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
        } catch (MetamacException metamacException) {
            try {
                noticesRestInternalService.createErrorOnStreamMessagingService(ServiceContextHolder.getCurrentServiceContext(), operationToPublish);
            } catch (Exception exception) {
                LOGGER.error("Could not send notice about external publication error", exception);
                metamacException.getExceptionItems().add(MetamacExceptionItemBuilder.metamacExceptionItem().withCommonServiceExceptionType(CommonServiceExceptionType.REST_API_NOTICES_ERROR_SENDING_NOTIFICATION).withMessageParameters(exception.getMessage()).build());
            }
            throw WebExceptionUtils.createMetamacWebException(metamacException);
        }

        try {
            noticesRestInternalService.createNotificationForPublishExternallyOperation(serviceContext, operationPublished);
        } catch (MetamacWebException e) {
            return new PublishExternallyOperationResult(operationPublished, e);
        }

        return new PublishExternallyOperationResult(operationPublished, null);
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
