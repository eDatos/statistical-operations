package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.SaveFamilyResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class SaveFamilyActionHandler extends AbstractActionHandler<SaveFamilyAction, SaveFamilyResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public SaveFamilyActionHandler() {
        super(SaveFamilyAction.class);
    }

    @Override
    public SaveFamilyResult execute(SaveFamilyAction action, ExecutionContext context) throws ActionException {
        FamilyDto familyToSave = action.getFamily();
        if (familyToSave.getId() == null) {
            // Create family
            try {
                FamilyDto familyDto = statisticalOperationsServiceFacade.createFamily(ServiceContextHelper.getServiceContext(), familyToSave);
                return new SaveFamilyResult(familyDto);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }

        } else {
            // Update family
            try {
                FamilyDto familyDto = statisticalOperationsServiceFacade.updateFamily(ServiceContextHelper.getServiceContext(), familyToSave);
                return new SaveFamilyResult(familyDto);
            } catch (MetamacException e) {
                throw WebExceptionUtils.createMetamacWebException(e);
            }
        }
    }

    @Override
    public void undo(SaveFamilyAction action, SaveFamilyResult result, ExecutionContext context) throws ActionException {

    }

}
