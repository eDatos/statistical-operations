package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.UpdateOperationFamiliesAction;
import org.siemac.metamac.statistical.operations.web.shared.UpdateOperationFamiliesResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class UpdateOperationFamiliesActionHandler extends SecurityActionHandler<UpdateOperationFamiliesAction, UpdateOperationFamiliesResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public UpdateOperationFamiliesActionHandler() {
        super(UpdateOperationFamiliesAction.class);
    }

    @Override
    public UpdateOperationFamiliesResult executeSecurityAction(UpdateOperationFamiliesAction action) throws ActionException {
        List<FamilyBaseDto> familyDtos = new ArrayList<FamilyBaseDto>();
        for (Long familyId : action.getFamiliesToAdd()) {
            try {
                familyDtos = statisticalOperationsServiceFacade.addFamilyForOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId(), familyId);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        }
        for (Long familyId : action.getFamiliesToRemove()) {
            try {
                familyDtos = statisticalOperationsServiceFacade.removeFamilyForOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId(), familyId);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        }
        return new UpdateOperationFamiliesResult(familyDtos);
    }

    @Override
    public void undo(UpdateOperationFamiliesAction action, UpdateOperationFamiliesResult result, ExecutionContext context) throws ActionException {

    }

}
