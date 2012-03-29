package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.GetFamilyAction;
import org.siemac.metamac.gopestat.web.shared.GetFamilyResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetFamilyActionHandler extends AbstractActionHandler<GetFamilyAction, GetFamilyResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public GetFamilyActionHandler() {
        super(GetFamilyAction.class);
    }

    @Override
    public GetFamilyResult execute(GetFamilyAction action, ExecutionContext context) throws ActionException {
        try {
            FamilyDto familyDto = gopestatServiceFacade.findFamilyById(ServiceContextHelper.getServiceContext(), action.getFamilyId());
            return new GetFamilyResult(familyDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(GetFamilyAction action, GetFamilyResult result, ExecutionContext context) throws ActionException {

    }

}
