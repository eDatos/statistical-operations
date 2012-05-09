package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveInstanceResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class SaveInstanceActionHandler extends AbstractActionHandler<SaveInstanceAction, SaveInstanceResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public SaveInstanceActionHandler() {
        super(SaveInstanceAction.class);
    }

    @Override
    public SaveInstanceResult execute(SaveInstanceAction action, ExecutionContext context) throws ActionException {
        InstanceDto instanceToSave = action.getInstanceDto();
        if (instanceToSave.getId() == null) {
            // Create instance
            try {
                InstanceDto instanceDto = statisticalOperationsServiceFacade.createInstance(ServiceContextHelper.getServiceContext(), action.getOperationId(), instanceToSave);
                return new SaveInstanceResult(instanceDto);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        } else {
            // Update instance
            try {
                InstanceDto instanceDto = statisticalOperationsServiceFacade.updateInstance(ServiceContextHelper.getServiceContext(), instanceToSave);
                return new SaveInstanceResult(instanceDto);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        }
    }

    @Override
    public void undo(SaveInstanceAction action, SaveInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
