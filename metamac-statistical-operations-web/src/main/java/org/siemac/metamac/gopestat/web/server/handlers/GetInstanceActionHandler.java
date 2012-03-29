package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.GetInstanceAction;
import org.siemac.metamac.gopestat.web.shared.GetInstanceResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetInstanceActionHandler extends AbstractActionHandler<GetInstanceAction, GetInstanceResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public GetInstanceActionHandler() {
        super(GetInstanceAction.class);
    }

    @Override
    public GetInstanceResult execute(GetInstanceAction action, ExecutionContext context) throws ActionException {
        InstanceDto instanceDto = null;
        try {
            instanceDto = gopestatServiceFacade.findInstanceById(ServiceContextHelper.getServiceContext(), action.getInstanceId());
            return new GetInstanceResult(instanceDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(GetInstanceAction action, GetInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
