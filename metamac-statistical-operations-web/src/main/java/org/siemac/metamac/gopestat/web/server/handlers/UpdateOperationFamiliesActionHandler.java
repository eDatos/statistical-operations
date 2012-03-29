package org.siemac.metamac.gopestat.web.server.handlers;

import java.util.ArrayList;
import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.UpdateOperationFamiliesAction;
import org.siemac.metamac.gopestat.web.shared.UpdateOperationFamiliesResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class UpdateOperationFamiliesActionHandler extends AbstractActionHandler<UpdateOperationFamiliesAction, UpdateOperationFamiliesResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public UpdateOperationFamiliesActionHandler() {
        super(UpdateOperationFamiliesAction.class);
    }

    @Override
    public UpdateOperationFamiliesResult execute(UpdateOperationFamiliesAction action, ExecutionContext context) throws ActionException {
        List<FamilyBaseDto> familyDtos = new ArrayList<FamilyBaseDto>();
        for (Long familyId : action.getFamiliesToAdd()) {
            try {
                familyDtos = gopestatServiceFacade.addFamilyForOperation(ServiceContextHelper.getServiceContext(), action.getOperationId(), familyId);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
        for (Long familyId : action.getFamiliesToRemove()) {
            try {
                familyDtos = gopestatServiceFacade.removeFamilyForOperation(ServiceContextHelper.getServiceContext(), action.getOperationId(), familyId);
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
