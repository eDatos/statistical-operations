package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionParameters;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.rest.serviceapi.ExternalItemValidator;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishExternallyOperationActionHandler extends SecurityActionHandler<PublishExternallyOperationAction, PublishExternallyOperationResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    @Autowired
    private ExternalItemValidator              externalItemValidator;

    public PublishExternallyOperationActionHandler() {
        super(PublishExternallyOperationAction.class);
    }

    @Override
    public PublishExternallyOperationResult executeSecurityAction(PublishExternallyOperationAction action) throws ActionException {
        try {
            OperationDto operationToPublish = statisticalOperationsServiceFacade.findOperationById(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            checkExternalItemsAreExternallyPublished(operationToPublish);

            OperationDto operationPublished = statisticalOperationsServiceFacade.publishExternallyOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            return new PublishExternallyOperationResult(operationPublished);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    /**
     * Check that all the external items of an {@link OperationDto} are externally published
     * 
     * @param operationDto
     * @throws MetamacWebException
     */
    private void checkExternalItemsAreExternallyPublished(OperationDto operationDto) throws MetamacWebException {
        // CommonMetadata should be always be externally published, so there is no need to check it
        externalItemValidator.checkExternalItemIsExternallyPublished(ServiceExceptionParameters.OPERATION_SUBJECT_AREA, operationDto.getSubjectArea());
        // TODO
    }
}
