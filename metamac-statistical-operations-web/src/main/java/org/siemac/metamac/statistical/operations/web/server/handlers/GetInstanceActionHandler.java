package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceAction;
import org.siemac.metamac.statistical.operations.web.shared.GetInstanceResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetInstanceActionHandler extends AbstractActionHandler<GetInstanceAction, GetInstanceResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public GetInstanceActionHandler() {
        super(GetInstanceAction.class);
    }

    @Override
    public GetInstanceResult execute(GetInstanceAction action, ExecutionContext context) throws ActionException {
        InstanceDto instanceDto = null;
        try {
            instanceDto = statisticalOperationsServiceFacade.findInstanceById(ServiceContextHelper.getServiceContext(), action.getInstanceId());
            return new GetInstanceResult(instanceDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(GetInstanceAction action, GetInstanceResult result, ExecutionContext context) throws ActionException {

    }

}
