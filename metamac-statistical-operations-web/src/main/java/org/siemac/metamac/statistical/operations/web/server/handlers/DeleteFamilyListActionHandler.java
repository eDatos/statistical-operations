package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.DeleteFamilyListAction;
import org.siemac.metamac.gopestat.web.shared.DeleteFamilyListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class DeleteFamilyListActionHandler extends AbstractActionHandler<DeleteFamilyListAction, DeleteFamilyListResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public DeleteFamilyListActionHandler() {
        super(DeleteFamilyListAction.class);
    }

    @Override
    public DeleteFamilyListResult execute(DeleteFamilyListAction action, ExecutionContext context) throws ActionException {
        for (Long id : action.getFamilyIds()) {
            try {
                gopestatServiceFacade.deleteFamily(ServiceContextHelper.getServiceContext(), id);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
        return new DeleteFamilyListResult();
    }

    @Override
    public void undo(DeleteFamilyListAction action, DeleteFamilyListResult result, ExecutionContext context) throws ActionException {

    }

}
