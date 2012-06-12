package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.OperationBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationListResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetOperationListActionHandler extends SecurityActionHandler<GetOperationListAction, GetOperationListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetOperationListActionHandler() {
        super(GetOperationListAction.class);
    }

    @Override
    public GetOperationListResult executeSecurityAction(GetOperationListAction action) throws ActionException {
        try {
            List<OperationBaseDto> operationBaseDtos = statisticalOperationsServiceFacade.findAllOperations(ServiceContextHolder.getCurrentServiceContext());
            return new GetOperationListResult(operationBaseDtos);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(GetOperationListAction action, GetOperationListResult result, ExecutionContext context) throws ActionException {

    }

}
