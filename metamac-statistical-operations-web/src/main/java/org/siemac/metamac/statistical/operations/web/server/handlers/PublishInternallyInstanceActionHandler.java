package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishInternallyInstanceActionHandler extends SecurityActionHandler<PublishInternallyInstanceAction, PublishInternallyInstanceResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishInternallyInstanceActionHandler() {
        super(PublishInternallyInstanceAction.class);
    }

    @Override
    public PublishInternallyInstanceResult executeSecurityAction(PublishInternallyInstanceAction action) throws ActionException {
        try {
            InstanceDto instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(ServiceContextHolder.getCurrentServiceContext(), action.getInstanceId());
            return new PublishInternallyInstanceResult(instanceDto);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(PublishInternallyInstanceAction action, PublishInternallyInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
