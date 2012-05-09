package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetOperationListActionHandler extends AbstractActionHandler<GetOperationListAction, GetOperationListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetOperationListActionHandler() {
        super(GetOperationListAction.class);
    }

    @Override
    public GetOperationListResult execute(GetOperationListAction action, ExecutionContext context) throws ActionException {
        try {
            List<OperationBaseDto> operationBaseDtos = statisticalOperationsServiceFacade.findAllOperations(ServiceContextHelper.getServiceContext());
            return new GetOperationListResult(operationBaseDtos);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(GetOperationListAction action, GetOperationListResult result, ExecutionContext context) throws ActionException {

    }

}
