package org.siemac.metamac.gopestat.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceBaseDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.GetOperationAndInstancesAction;
import org.siemac.metamac.gopestat.web.shared.GetOperationAndInstancesResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetOperationAndInstancesActionHandler extends AbstractActionHandler<GetOperationAndInstancesAction, GetOperationAndInstancesResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public GetOperationAndInstancesActionHandler() {
        super(GetOperationAndInstancesAction.class);
    }

    @Override
    public GetOperationAndInstancesResult execute(GetOperationAndInstancesAction action, ExecutionContext context) throws ActionException {
        try {
            OperationDto operationDto = gopestatServiceFacade.findOperationById(ServiceContextHelper.getServiceContext(), action.getOperationId());
            List<FamilyBaseDto> familyBaseDtos = gopestatServiceFacade.findFamiliesForOperation(ServiceContextHelper.getServiceContext(), action.getOperationId());
            List<InstanceBaseDto> instanceDtos = gopestatServiceFacade.findInstancesForOperation(ServiceContextHelper.getServiceContext(), action.getOperationId());
            return new GetOperationAndInstancesResult(operationDto, instanceDtos, familyBaseDtos);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(GetOperationAndInstancesAction action, GetOperationAndInstancesResult result, ExecutionContext context) throws ActionException {

    }

}
