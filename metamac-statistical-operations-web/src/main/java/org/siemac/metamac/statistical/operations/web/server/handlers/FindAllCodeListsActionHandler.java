package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCodeListsAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCodeListsResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class FindAllCodeListsActionHandler extends AbstractActionHandler<FindAllCodeListsAction, FindAllCodeListsResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllCodeListsActionHandler() {
        super(FindAllCodeListsAction.class);
    }

    @Override
    public FindAllCodeListsResult execute(FindAllCodeListsAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> codeLists = metamacCoreCommonService.findAllCodelists(ServiceContextHolder.getCurrentServiceContext());
        return new FindAllCodeListsResult(codeLists);
    }

    @Override
    public void undo(FindAllCodeListsAction action, FindAllCodeListsResult result, ExecutionContext context) throws ActionException {

    }

}
