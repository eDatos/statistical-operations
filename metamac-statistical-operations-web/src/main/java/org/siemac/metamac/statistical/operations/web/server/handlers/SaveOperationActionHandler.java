package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class SaveOperationActionHandler extends AbstractActionHandler<SaveOperationAction, SaveOperationResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public SaveOperationActionHandler() {
        super(SaveOperationAction.class);
    }

    @Override
    public SaveOperationResult execute(SaveOperationAction action, ExecutionContext context) throws ActionException {
        OperationDto operationToSave = action.getOperationDto();
        if (operationToSave.getId() == null) {
            // Create operation
            try {
                OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(ServiceContextHolder.getCurrentServiceContext(), operationToSave);
                return new SaveOperationResult(operationDto);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        } else {
            // Update operation
            try {
                OperationDto operationDto = statisticalOperationsServiceFacade.updateOperation(ServiceContextHolder.getCurrentServiceContext(), operationToSave);
                return new SaveOperationResult(operationDto);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        }
    }

    @Override
    public void undo(SaveOperationAction action, SaveOperationResult result, ExecutionContext context) throws ActionException {

    }

}
