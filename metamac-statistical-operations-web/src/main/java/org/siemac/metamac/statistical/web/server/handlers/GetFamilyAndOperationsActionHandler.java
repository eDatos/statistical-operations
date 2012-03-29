package org.siemac.metamac.gopestat.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationBaseDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.GetFamilyAndOperationsAction;
import org.siemac.metamac.gopestat.web.shared.GetFamilyAndOperationsResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetFamilyAndOperationsActionHandler extends AbstractActionHandler<GetFamilyAndOperationsAction, GetFamilyAndOperationsResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public GetFamilyAndOperationsActionHandler() {
        super(GetFamilyAndOperationsAction.class);
    }

    @Override
    public GetFamilyAndOperationsResult execute(GetFamilyAndOperationsAction action, ExecutionContext context) throws ActionException {
        try {
            FamilyDto familyDto = gopestatServiceFacade.findFamilyById(ServiceContextHelper.getServiceContext(), action.getFamilyId());
            List<OperationBaseDto> operationDtos = gopestatServiceFacade.findOperationsForFamily(ServiceContextHelper.getServiceContext(), action.getFamilyId());
            return new GetFamilyAndOperationsResult(familyDto, operationDtos);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(GetFamilyAndOperationsAction action, GetFamilyAndOperationsResult result, ExecutionContext context) throws ActionException {

    }

}
