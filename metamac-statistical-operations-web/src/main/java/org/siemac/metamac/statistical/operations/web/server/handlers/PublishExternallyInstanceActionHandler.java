package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.PublishExternallyInstanceAction;
import org.siemac.metamac.gopestat.web.shared.PublishExternallyInstanceResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class PublishExternallyInstanceActionHandler extends AbstractActionHandler<PublishExternallyInstanceAction, PublishExternallyInstanceResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public PublishExternallyInstanceActionHandler() {
        super(PublishExternallyInstanceAction.class);
    }

    @Override
    public PublishExternallyInstanceResult execute(PublishExternallyInstanceAction action, ExecutionContext context) throws ActionException {
        try {
            InstanceDto instanceDto = gopestatServiceFacade.publishExternallyInstance(ServiceContextHelper.getServiceContext(), action.getInstanceId());
            return new PublishExternallyInstanceResult(instanceDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(PublishExternallyInstanceAction action, PublishExternallyInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
