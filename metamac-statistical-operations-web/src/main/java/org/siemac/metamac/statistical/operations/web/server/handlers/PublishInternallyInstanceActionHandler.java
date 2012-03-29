package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyInstanceResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class PublishInternallyInstanceActionHandler extends AbstractActionHandler<PublishInternallyInstanceAction, PublishInternallyInstanceResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishInternallyInstanceActionHandler() {
        super(PublishInternallyInstanceAction.class);
    }

    @Override
    public PublishInternallyInstanceResult execute(PublishInternallyInstanceAction action, ExecutionContext context) throws ActionException {
        try {
            InstanceDto instanceDto = statisticalOperationsServiceFacade.publishInternallyInstance(ServiceContextHelper.getServiceContext(), action.getInstanceId());
            return new PublishInternallyInstanceResult(instanceDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(PublishInternallyInstanceAction action, PublishInternallyInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
