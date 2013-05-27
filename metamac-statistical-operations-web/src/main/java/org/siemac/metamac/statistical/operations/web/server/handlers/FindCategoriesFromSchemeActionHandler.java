package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.shared.FindCategoriesFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.FindCategoriesFromSchemeResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class FindCategoriesFromSchemeActionHandler extends SecurityActionHandler<FindCategoriesFromSchemeAction, FindCategoriesFromSchemeResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindCategoriesFromSchemeActionHandler() {
        super(FindCategoriesFromSchemeAction.class);
    }

    @Override
    public FindCategoriesFromSchemeResult executeSecurityAction(FindCategoriesFromSchemeAction action) throws ActionException {
        List<ExternalItemDto> categories = metamacCoreCommonService.retrieveCategoryScheme(ServiceContextHolder.getCurrentServiceContext(), action.getCategorySchemeUri());
        return new FindCategoriesFromSchemeResult(categories);
    }

    @Override
    public void undo(FindCategoriesFromSchemeAction action, FindCategoriesFromSchemeResult result, ExecutionContext context) throws ActionException {

    }

}
