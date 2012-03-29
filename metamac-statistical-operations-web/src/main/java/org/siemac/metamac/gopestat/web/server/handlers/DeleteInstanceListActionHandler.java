package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.DeleteInstanceListAction;
import org.siemac.metamac.gopestat.web.shared.DeleteInstanceListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class DeleteInstanceListActionHandler extends AbstractActionHandler<DeleteInstanceListAction, DeleteInstanceListResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public DeleteInstanceListActionHandler() {
        super(DeleteInstanceListAction.class);
    }

    @Override
    public DeleteInstanceListResult execute(DeleteInstanceListAction action, ExecutionContext context) throws ActionException {
        for (Long id : action.getInstanceIds()) {
            try {
                gopestatServiceFacade.deleteInstance(ServiceContextHelper.getServiceContext(), id);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
        return new DeleteInstanceListResult();
    }

    @Override
    public void undo(DeleteInstanceListAction action, DeleteInstanceListResult result, ExecutionContext context) throws ActionException {

    }

}
