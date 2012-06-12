package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class FindAllCategorySchemesActionHandler extends SecurityActionHandler<FindAllCategorySchemesAction, FindAllCategorySchemesResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllCategorySchemesActionHandler() {
        super(FindAllCategorySchemesAction.class);
    }

    @Override
    public FindAllCategorySchemesResult executeSecurityAction(FindAllCategorySchemesAction action) throws ActionException {
        List<ExternalItemBtDto> categorySchemes = metamacCoreCommonService.findAllCategorySchemes(ServiceContextHolder.getCurrentServiceContext());
        return new FindAllCategorySchemesResult(categorySchemes);
    }

    @Override
    public void undo(FindAllCategorySchemesAction action, FindAllCategorySchemesResult result, ExecutionContext context) throws ActionException {

    }

}
