package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyOperationResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class PublishInternallyOperationActionHandler extends AbstractActionHandler<PublishInternallyOperationAction, PublishInternallyOperationResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishInternallyOperationActionHandler() {
        super(PublishInternallyOperationAction.class);
    }

    @Override
    public PublishInternallyOperationResult execute(PublishInternallyOperationAction action, ExecutionContext context) throws ActionException {
        try {
            OperationDto operationDto = statisticalOperationsServiceFacade.publishInternallyOperation(ServiceContextHelper.getServiceContext(), action.getOperationId());
            return new PublishInternallyOperationResult(operationDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(PublishInternallyOperationAction action, PublishInternallyOperationResult result, ExecutionContext context) throws ActionException {

    }

}
