package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishInternallyOperationActionHandler extends SecurityActionHandler<PublishInternallyOperationAction, PublishInternallyOperationResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishInternallyOperationActionHandler() {
        super(PublishInternallyOperationAction.class);
    }

    @Override
    public PublishInternallyOperationResult executeSecurityAction(PublishInternallyOperationAction action) throws ActionException {
        try {
            OperationDto operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            return new PublishInternallyOperationResult(operationDto);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(PublishInternallyOperationAction action, PublishInternallyOperationResult result, ExecutionContext context) throws ActionException {

    }

}
