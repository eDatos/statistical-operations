package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.DeleteFamilyListAction;
import org.siemac.metamac.statistical.operations.web.shared.DeleteFamilyListResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class DeleteFamilyListActionHandler extends SecurityActionHandler<DeleteFamilyListAction, DeleteFamilyListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public DeleteFamilyListActionHandler() {
        super(DeleteFamilyListAction.class);
    }

    @Override
    public DeleteFamilyListResult executeSecurityAction(DeleteFamilyListAction action) throws ActionException {
        for (Long id : action.getFamilyIds()) {
            try {
                statisticalOperationsServiceFacade.deleteFamily(ServiceContextHolder.getCurrentServiceContext(), id);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        }
        return new DeleteFamilyListResult();
    }

    @Override
    public void undo(DeleteFamilyListAction action, DeleteFamilyListResult result, ExecutionContext context) throws ActionException {

    }

}
