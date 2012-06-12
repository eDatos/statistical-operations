package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyOperationResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishExternallyOperationActionHandler extends SecurityActionHandler<PublishExternallyOperationAction, PublishExternallyOperationResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishExternallyOperationActionHandler() {
        super(PublishExternallyOperationAction.class);
    }

    @Override
    public PublishExternallyOperationResult executeSecurityAction(PublishExternallyOperationAction action) throws ActionException {
        try {
            OperationDto operationDto = statisticalOperationsServiceFacade.publishExternallyOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            return new PublishExternallyOperationResult(operationDto);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(PublishExternallyOperationAction action, PublishExternallyOperationResult result, ExecutionContext context) throws ActionException {

    }

}
