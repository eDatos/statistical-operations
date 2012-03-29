package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.PublishExternallyOperationAction;
import org.siemac.metamac.gopestat.web.shared.PublishExternallyOperationResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class PublishExternallyOperationActionHandler extends AbstractActionHandler<PublishExternallyOperationAction, PublishExternallyOperationResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public PublishExternallyOperationActionHandler() {
        super(PublishExternallyOperationAction.class);
    }

    @Override
    public PublishExternallyOperationResult execute(PublishExternallyOperationAction action, ExecutionContext context) throws ActionException {
        try {
            OperationDto operationDto = gopestatServiceFacade.publishExternallyOperation(ServiceContextHelper.getServiceContext(), action.getOperationId());
            return new PublishExternallyOperationResult(operationDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(PublishExternallyOperationAction action, PublishExternallyOperationResult result, ExecutionContext context) throws ActionException {

    }

}
