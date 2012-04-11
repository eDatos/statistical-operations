package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveOperationResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
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
                OperationDto operationDto = statisticalOperationsServiceFacade.createOperation(ServiceContextHelper.getServiceContext(), operationToSave);
                return new SaveOperationResult(operationDto);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        } else {
            // Update operation
            try {
                OperationDto operationDto = statisticalOperationsServiceFacade.updateOperation(ServiceContextHelper.getServiceContext(), operationToSave);
                return new SaveOperationResult(operationDto);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
    }

    @Override
    public void undo(SaveOperationAction action, SaveOperationResult result, ExecutionContext context) throws ActionException {

    }

}
