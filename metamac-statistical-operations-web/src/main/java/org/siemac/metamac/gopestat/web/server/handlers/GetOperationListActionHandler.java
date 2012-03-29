package org.siemac.metamac.gopestat.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationBaseDto;
import org.siemac.metamac.gopestat.core.serviceapi.GopestatServiceFacade;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.GetOperationListAction;
import org.siemac.metamac.gopestat.web.shared.GetOperationListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetOperationListActionHandler extends AbstractActionHandler<GetOperationListAction, GetOperationListResult> {

    @Autowired
    private GopestatServiceFacade gopestatServiceFacade;

    public GetOperationListActionHandler() {
        super(GetOperationListAction.class);
    }

    @Override
    public GetOperationListResult execute(GetOperationListAction action, ExecutionContext context) throws ActionException {
        try {
            List<OperationBaseDto> operationBaseDtos = gopestatServiceFacade.findAllOperations(ServiceContextHelper.getServiceContext());
            return new GetOperationListResult(operationBaseDtos);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(GetOperationListAction action, GetOperationListResult result, ExecutionContext context) throws ActionException {

    }

}
