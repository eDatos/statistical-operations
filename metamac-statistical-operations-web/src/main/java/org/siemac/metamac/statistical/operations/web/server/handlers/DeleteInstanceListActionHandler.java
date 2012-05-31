package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteInstanceListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class DeleteInstanceListActionHandler extends AbstractActionHandler<DeleteInstanceListAction, DeleteInstanceListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public DeleteInstanceListActionHandler() {
        super(DeleteInstanceListAction.class);
    }

    @Override
    public DeleteInstanceListResult execute(DeleteInstanceListAction action, ExecutionContext context) throws ActionException {
        for (Long id : action.getInstanceIds()) {
            try {
                statisticalOperationsServiceFacade.deleteInstance(ServiceContextHolder.getCurrentServiceContext(), id);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        }
        return new DeleteInstanceListResult();
    }

    @Override
    public void undo(DeleteInstanceListAction action, DeleteInstanceListResult result, ExecutionContext context) throws ActionException {

    }

}
