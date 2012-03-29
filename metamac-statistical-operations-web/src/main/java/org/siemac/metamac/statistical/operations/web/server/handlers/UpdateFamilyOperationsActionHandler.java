package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.UpdateFamilyOperationsAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateFamilyOperationsResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class UpdateFamilyOperationsActionHandler extends AbstractActionHandler<UpdateFamilyOperationsAction, UpdateFamilyOperationsResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public UpdateFamilyOperationsActionHandler() {
        super(UpdateFamilyOperationsAction.class);
    }

    @Override
    public UpdateFamilyOperationsResult execute(UpdateFamilyOperationsAction action, ExecutionContext context) throws ActionException {
        List<OperationBaseDto> operationDtos = new ArrayList<OperationBaseDto>();
        for (Long operationId : action.getOperationsToAdd()) {
            try {
                operationDtos = statisticalOperationsServiceFacade.addOperationForFamily(ServiceContextHelper.getServiceContext(), action.getFamilyId(), operationId);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
        for (Long operationId : action.getOperationsToRemove()) {
            try {
                operationDtos = statisticalOperationsServiceFacade.removeOperationForFamily(ServiceContextHelper.getServiceContext(), action.getFamilyId(), operationId);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
        return new UpdateFamilyOperationsResult(operationDtos);
    }

    @Override
    public void undo(UpdateFamilyOperationsAction action, UpdateFamilyOperationsResult result, ExecutionContext context) throws ActionException {

    }

}
