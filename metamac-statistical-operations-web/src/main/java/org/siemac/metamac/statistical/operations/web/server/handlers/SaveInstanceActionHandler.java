package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.SaveInstanceAction;
import org.siemac.metamac.gopestat.web.shared.SaveInstanceResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class SaveInstanceActionHandler extends AbstractActionHandler<SaveInstanceAction, SaveInstanceResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public SaveInstanceActionHandler() {
        super(SaveInstanceAction.class);
    }

    @Override
    public SaveInstanceResult execute(SaveInstanceAction action, ExecutionContext context) throws ActionException {
        InstanceDto instanceToSave = action.getInstanceDto();
        if (instanceToSave.getId() == null) {
            // Create instance
            try {
                InstanceDto instanceDto = gopestatServiceFacade.createInstance(ServiceContextHelper.getServiceContext(), action.getOperationId(), instanceToSave);
                return new SaveInstanceResult(instanceDto);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        } else {
            // Update instance
            try {
                InstanceDto instanceDto = gopestatServiceFacade.updateInstance(ServiceContextHelper.getServiceContext(), instanceToSave);
                return new SaveInstanceResult(instanceDto);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
    }

    @Override
    public void undo(SaveInstanceAction action, SaveInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
