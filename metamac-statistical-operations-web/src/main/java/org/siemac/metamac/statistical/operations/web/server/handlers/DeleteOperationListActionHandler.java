package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteOperationListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class DeleteOperationListActionHandler extends AbstractActionHandler<DeleteOperationListAction, DeleteOperationListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public DeleteOperationListActionHandler() {
        super(DeleteOperationListAction.class);
    }

    @Override
    public DeleteOperationListResult execute(DeleteOperationListAction action, ExecutionContext context) throws ActionException {
        for (Long id : action.getOperationIds()) {
            try {
                statisticalOperationsServiceFacade.deleteOperation(ServiceContextHelper.getServiceContext(), id);
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
