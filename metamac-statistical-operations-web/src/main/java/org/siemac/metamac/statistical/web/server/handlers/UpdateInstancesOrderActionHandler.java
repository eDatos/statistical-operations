package org.siemac.metamac.gopestat.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceBaseDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.UpdateInstancesOrderAction;
import org.siemac.metamac.gopestat.web.shared.UpdateInstancesOrderResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class UpdateInstancesOrderActionHandler extends AbstractActionHandler<UpdateInstancesOrderAction, UpdateInstancesOrderResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public UpdateInstancesOrderActionHandler() {
        super(UpdateInstancesOrderAction.class);
    }

    @Override
    public UpdateInstancesOrderResult execute(UpdateInstancesOrderAction action, ExecutionContext context) throws ActionException {
        try {
            List<InstanceBaseDto> instances = gopestatServiceFacade.updateInstancesOrder(ServiceContextHelper.getServiceContext(), action.getOperationId(), action.getInstancesIds());
            return new UpdateInstancesOrderResult(instances);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(UpdateInstancesOrderAction action, UpdateInstancesOrderResult result, ExecutionContext context) throws ActionException {

    }

}
