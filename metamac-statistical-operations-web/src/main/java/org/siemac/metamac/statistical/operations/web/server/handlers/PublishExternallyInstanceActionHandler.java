package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishExternallyInstanceActionHandler extends SecurityActionHandler<PublishExternallyInstanceAction, PublishExternallyInstanceResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishExternallyInstanceActionHandler() {
        super(PublishExternallyInstanceAction.class);
    }

    @Override
    public PublishExternallyInstanceResult executeSecurityAction(PublishExternallyInstanceAction action) throws ActionException {
        try {
            InstanceDto instanceDto = statisticalOperationsServiceFacade.publishExternallyInstance(ServiceContextHolder.getCurrentServiceContext(), action.getInstanceId());
            return new PublishExternallyInstanceResult(instanceDto);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(PublishExternallyInstanceAction action, PublishExternallyInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
