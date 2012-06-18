package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesByCodeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetOperationAndInstancesByCodeResult;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetOperationAndInstancesByCodeActionHandler extends SecurityActionHandler<GetOperationAndInstancesByCodeAction, GetOperationAndInstancesByCodeResult> {

    // @Autowired
    // private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetOperationAndInstancesByCodeActionHandler() {
        super(GetOperationAndInstancesByCodeAction.class);
    }

    @Override
    public GetOperationAndInstancesByCodeResult executeSecurityAction(GetOperationAndInstancesByCodeAction action) throws ActionException {
        return null;
    }

    @Override
    public void undo(GetOperationAndInstancesByCodeAction action, GetOperationAndInstancesByCodeResult result, ExecutionContext context) throws ActionException {

    }

}
