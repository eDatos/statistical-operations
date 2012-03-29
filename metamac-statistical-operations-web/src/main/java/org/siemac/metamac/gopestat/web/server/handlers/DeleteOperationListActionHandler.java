package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.DeleteOperationListAction;
import org.siemac.metamac.gopestat.web.shared.DeleteOperationListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class DeleteOperationListActionHandler extends AbstractActionHandler<DeleteOperationListAction, DeleteOperationListResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public DeleteOperationListActionHandler() {
        super(DeleteOperationListAction.class);
    }

    @Override
    public DeleteOperationListResult execute(DeleteOperationListAction action, ExecutionContext context) throws ActionException {
        for (Long id : action.getOperationIds()) {
            try {
                gopestatServiceFacade.deleteOperation(ServiceContextHelper.getServiceContext(), id);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
        return new DeleteOperationListResult();
    }

    @Override
    public void undo(DeleteOperationListAction action, DeleteOperationListResult result, ExecutionContext context) throws ActionException {

    }

}
