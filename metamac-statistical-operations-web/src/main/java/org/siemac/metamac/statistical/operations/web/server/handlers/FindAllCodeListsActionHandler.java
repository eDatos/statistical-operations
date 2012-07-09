package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCodeListsAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCodeListsResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class FindAllCodeListsActionHandler extends SecurityActionHandler<FindAllCodeListsAction, FindAllCodeListsResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllCodeListsActionHandler() {
        super(FindAllCodeListsAction.class);
    }

    @Override
    public FindAllCodeListsResult executeSecurityAction(FindAllCodeListsAction action) throws ActionException {
        List<ExternalItemDto> codeLists = metamacCoreCommonService.findAllCodelists(ServiceContextHolder.getCurrentServiceContext());
        return new FindAllCodeListsResult(codeLists);
    }

    @Override
    public void undo(FindAllCodeListsAction action, FindAllCodeListsResult result, ExecutionContext context) throws ActionException {

    }

}
