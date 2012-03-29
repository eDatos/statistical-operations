package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyInstanceResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class PublishExternallyInstanceActionHandler extends AbstractActionHandler<PublishExternallyInstanceAction, PublishExternallyInstanceResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishExternallyInstanceActionHandler() {
        super(PublishExternallyInstanceAction.class);
    }

    @Override
    public PublishExternallyInstanceResult execute(PublishExternallyInstanceAction action, ExecutionContext context) throws ActionException {
        try {
            InstanceDto instanceDto = statisticalOperationsServiceFacade.publishExternallyInstance(ServiceContextHelper.getServiceContext(), action.getInstanceId());
            return new PublishExternallyInstanceResult(instanceDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(PublishExternallyInstanceAction action, PublishExternallyInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
