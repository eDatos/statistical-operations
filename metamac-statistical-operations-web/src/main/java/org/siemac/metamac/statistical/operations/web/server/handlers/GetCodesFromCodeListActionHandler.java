package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.GetCodesFromCodeListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCodesFromCodeListResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetCodesFromCodeListActionHandler extends AbstractActionHandler<GetCodesFromCodeListAction, GetCodesFromCodeListResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public GetCodesFromCodeListActionHandler() {
        super(GetCodesFromCodeListAction.class);
    }

    @Override
    public GetCodesFromCodeListResult execute(GetCodesFromCodeListAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> concepts = metamacCoreCommonService.retrieveCodelist(ServiceContextHolder.getCurrentServiceContext(), action.getCodeListUri());
        return new GetCodesFromCodeListResult(concepts);
    }

    @Override
    public void undo(GetCodesFromCodeListAction action, GetCodesFromCodeListResult result, ExecutionContext context) throws ActionException {

    }

}
