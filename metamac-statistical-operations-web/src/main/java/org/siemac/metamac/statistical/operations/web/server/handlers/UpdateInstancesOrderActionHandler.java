package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.UpdateInstancesOrderAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateInstancesOrderResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class UpdateInstancesOrderActionHandler extends SecurityActionHandler<UpdateInstancesOrderAction, UpdateInstancesOrderResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public UpdateInstancesOrderActionHandler() {
        super(UpdateInstancesOrderAction.class);
    }

    @Override
    public UpdateInstancesOrderResult executeSecurityAction(UpdateInstancesOrderAction action) throws ActionException {
        try {
            List<InstanceBaseDto> instances = statisticalOperationsServiceFacade.updateInstancesOrder(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId(),
                    action.getInstancesIds());
            return new UpdateInstancesOrderResult(instances);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(UpdateInstancesOrderAction action, UpdateInstancesOrderResult result, ExecutionContext context) throws ActionException {

    }

}
