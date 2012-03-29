package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.GetCodesFromCodeListAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCodesFromCodeListResult;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetCodesFromCodeListActionHandler extends AbstractActionHandler<GetCodesFromCodeListAction, GetCodesFromCodeListResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public GetCodesFromCodeListActionHandler() {
        super(GetCodesFromCodeListAction.class);
    }

    @Override
    public GetCodesFromCodeListResult execute(GetCodesFromCodeListAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> concepts = metamacCoreCommonService.retrieveCodelist(ServiceContextHelper.getServiceContext(), action.getCodeListUri());
        return new GetCodesFromCodeListResult(concepts);
    }

    @Override
    public void undo(GetCodesFromCodeListAction action, GetCodesFromCodeListResult result, ExecutionContext context) throws ActionException {

    }

}
