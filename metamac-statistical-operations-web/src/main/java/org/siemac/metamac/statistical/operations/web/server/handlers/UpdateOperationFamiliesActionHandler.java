package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.UpdateOperationFamiliesAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateOperationFamiliesResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class UpdateOperationFamiliesActionHandler extends AbstractActionHandler<UpdateOperationFamiliesAction, UpdateOperationFamiliesResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public UpdateOperationFamiliesActionHandler() {
        super(UpdateOperationFamiliesAction.class);
    }

    @Override
    public UpdateOperationFamiliesResult execute(UpdateOperationFamiliesAction action, ExecutionContext context) throws ActionException {
        List<FamilyBaseDto> familyDtos = new ArrayList<FamilyBaseDto>();
        for (Long familyId : action.getFamiliesToAdd()) {
            try {
                familyDtos = statisticalOperationsServiceFacade.addFamilyForOperation(ServiceContextHelper.getServiceContext(), action.getOperationId(), familyId);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
        for (Long familyId : action.getFamiliesToRemove()) {
            try {
                familyDtos = statisticalOperationsServiceFacade.removeFamilyForOperation(ServiceContextHelper.getServiceContext(), action.getOperationId(), familyId);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
        return new UpdateOperationFamiliesResult(familyDtos);
    }

    @Override
    public void undo(UpdateOperationFamiliesAction action, UpdateOperationFamiliesResult result, ExecutionContext context) throws ActionException {

    }

}
