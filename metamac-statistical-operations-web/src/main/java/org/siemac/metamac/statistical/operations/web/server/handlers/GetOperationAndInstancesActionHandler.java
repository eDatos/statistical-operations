package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceBaseDto;
import org.siemac.metamac.domain.statistical.operations.dto.OperationDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetOperationAndInstancesActionHandler extends AbstractActionHandler<GetOperationAndInstancesAction, GetOperationAndInstancesResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetOperationAndInstancesActionHandler() {
        super(GetOperationAndInstancesAction.class);
    }

    @Override
    public GetOperationAndInstancesResult execute(GetOperationAndInstancesAction action, ExecutionContext context) throws ActionException {
        try {
            OperationDto operationDto = statisticalOperationsServiceFacade.findOperationById(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            List<FamilyBaseDto> familyBaseDtos = statisticalOperationsServiceFacade.findFamiliesForOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            List<InstanceBaseDto> instanceDtos = statisticalOperationsServiceFacade.findInstancesForOperation(ServiceContextHolder.getCurrentServiceContext(), action.getOperationId());
            return new GetOperationAndInstancesResult(operationDto, instanceDtos, familyBaseDtos);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(GetOperationAndInstancesAction action, GetOperationAndInstancesResult result, ExecutionContext context) throws ActionException {

    }

}
