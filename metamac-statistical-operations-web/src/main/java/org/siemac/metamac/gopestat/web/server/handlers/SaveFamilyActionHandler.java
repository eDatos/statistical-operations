package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.SaveFamilyAction;
import org.siemac.metamac.gopestat.web.shared.SaveFamilyResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class SaveFamilyActionHandler extends AbstractActionHandler<SaveFamilyAction, SaveFamilyResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public SaveFamilyActionHandler() {
        super(SaveFamilyAction.class);
    }

    @Override
    public SaveFamilyResult execute(SaveFamilyAction action, ExecutionContext context) throws ActionException {
        FamilyDto familyToSave = action.getFamily();
        if (familyToSave.getId() == null) {
            // Create family
            try {
                FamilyDto familyDto = gopestatServiceFacade.createFamily(ServiceContextHelper.getServiceContext(), familyToSave);
                return new SaveFamilyResult(familyDto);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }

        } else {
            // Update family
            try {
                FamilyDto familyDto = gopestatServiceFacade.updateFamily(ServiceContextHelper.getServiceContext(), familyToSave);
                return new SaveFamilyResult(familyDto);
            } catch (MetamacException e) {
                throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
            }
        }
    }

    @Override
    public void undo(SaveFamilyAction action, SaveFamilyResult result, ExecutionContext context) throws ActionException {

    }

}
