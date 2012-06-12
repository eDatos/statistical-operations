package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class DeleteOperationListActionHandler extends SecurityActionHandler<DeleteOperationListAction, DeleteOperationListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public DeleteOperationListActionHandler() {
        super(DeleteOperationListAction.class);
    }

    @Override
    public DeleteOperationListResult executeSecurityAction(DeleteOperationListAction action) throws ActionException {
        for (Long id : action.getOperationIds()) {
            try {
                statisticalOperationsServiceFacade.deleteOperation(ServiceContextHolder.getCurrentServiceContext(), id);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        }
        return new DeleteOperationListResult();
    }

    @Override
    public void undo(DeleteOperationListAction action, DeleteOperationListResult result, ExecutionContext context) throws ActionException {

    }

}
