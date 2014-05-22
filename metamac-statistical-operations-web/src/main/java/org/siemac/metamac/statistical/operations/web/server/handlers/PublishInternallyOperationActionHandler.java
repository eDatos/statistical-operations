package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.rest.NoticesRestInternalService;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishInternallyOperationActionHandler extends SecurityActionHandler<PublishInternallyOperationAction, PublishInternallyOperationResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    @Autowired
    private NoticesRestInternalService         noticesRestInternalService;

    public PublishInternallyOperationActionHandler() {
        super(PublishInternallyOperationAction.class);
    }

    @Override
    public PublishInternallyOperationResult executeSecurityAction(PublishInternallyOperationAction action) throws ActionException {
        ServiceContext serviceContext = ServiceContextHolder.getCurrentServiceContext();
        OperationDto operationPublished = null;

        try {
            operationPublished = statisticalOperationsServiceFacade.publishInternallyOperation(serviceContext, action.getOperationId());
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }

        try {
            noticesRestInternalService.createNotificationForPublishInternallyOperation(serviceContext, operationPublished);
        } catch (MetamacWebException e) {
            return new PublishInternallyOperationResult(operationPublished, e);
        }

        return new PublishInternallyOperationResult(operationPublished, null);
    }

    @Override
    public void undo(PublishInternallyOperationAction action, PublishInternallyOperationResult result, ExecutionContext context) throws ActionException {
    }

}
