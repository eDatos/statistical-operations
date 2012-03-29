package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.InstanceBaseDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceListResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetInstanceListActionHandler extends AbstractActionHandler<GetInstanceListAction, GetInstanceListResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetInstanceListActionHandler() {
        super(GetInstanceListAction.class);
    }

    @Override
    public GetInstanceListResult execute(GetInstanceListAction action, ExecutionContext context) throws ActionException {
        List<InstanceBaseDto> instanceBaseDtos = null;
        try {
            instanceBaseDtos = statisticalOperationsServiceFacade.findInstancesForOperation(ServiceContextHelper.getServiceContext(), action.getOperationId());
            return new GetInstanceListResult(instanceBaseDtos);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(GetInstanceListAction action, GetInstanceListResult result, ExecutionContext context) throws ActionException {

    }

}
