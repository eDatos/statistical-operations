package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.GetFamilyResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetFamilyActionHandler extends SecurityActionHandler<GetFamilyAction, GetFamilyResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetFamilyActionHandler() {
        super(GetFamilyAction.class);
    }

    @Override
    public GetFamilyResult executeSecurityAction(GetFamilyAction action) throws ActionException {
        try {
            FamilyDto familyDto = statisticalOperationsServiceFacade.findFamilyById(ServiceContextHolder.getCurrentServiceContext(), action.getFamilyId());
            return new GetFamilyResult(familyDto);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(GetFamilyAction action, GetFamilyResult result, ExecutionContext context) throws ActionException {

    }

}
