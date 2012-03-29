package org.siemac.metamac.gopestat.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.PublishInternallyFamilyAction;
import org.siemac.metamac.gopestat.web.shared.PublishInternallyFamilyResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class PublishInternallyFamilyActionHandler extends AbstractActionHandler<PublishInternallyFamilyAction, PublishInternallyFamilyResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public PublishInternallyFamilyActionHandler() {
        super(PublishInternallyFamilyAction.class);
    }

    @Override
    public PublishInternallyFamilyResult execute(PublishInternallyFamilyAction action, ExecutionContext context) throws ActionException {
        try {
            FamilyDto familyDto = gopestatServiceFacade.publishInternallyFamily(ServiceContextHelper.getServiceContext(), action.getFamilyId());
            return new PublishInternallyFamilyResult(familyDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(PublishInternallyFamilyAction action, PublishInternallyFamilyResult result, ExecutionContext context) throws ActionException {

    }

}
