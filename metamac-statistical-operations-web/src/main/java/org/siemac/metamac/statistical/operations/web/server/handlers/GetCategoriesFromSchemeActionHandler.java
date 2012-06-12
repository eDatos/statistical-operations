package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetCategoriesFromSchemeResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetCategoriesFromSchemeActionHandler extends SecurityActionHandler<GetCategoriesFromSchemeAction, GetCategoriesFromSchemeResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public GetCategoriesFromSchemeActionHandler() {
        super(GetCategoriesFromSchemeAction.class);
    }

    @Override
    public GetCategoriesFromSchemeResult executeSecurityAction(GetCategoriesFromSchemeAction action) throws ActionException {
        List<ExternalItemBtDto> categories = metamacCoreCommonService.retrieveCategoryScheme(ServiceContextHolder.getCurrentServiceContext(), action.getCategorySchemeUri());
        return new GetCategoriesFromSchemeResult(categories);
    }

    @Override
    public void undo(GetCategoriesFromSchemeAction action, GetCategoriesFromSchemeResult result, ExecutionContext context) throws ActionException {

    }

}
