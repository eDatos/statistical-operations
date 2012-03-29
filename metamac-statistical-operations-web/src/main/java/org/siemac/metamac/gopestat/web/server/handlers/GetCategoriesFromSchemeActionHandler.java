package org.siemac.metamac.gopestat.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.gopestat.web.server.ServiceContextHelper;
import org.siemac.metamac.gopestat.web.shared.GetCategoriesFromSchemeAction;
import org.siemac.metamac.gopestat.web.shared.GetCategoriesFromSchemeResult;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class GetCategoriesFromSchemeActionHandler extends AbstractActionHandler<GetCategoriesFromSchemeAction, GetCategoriesFromSchemeResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public GetCategoriesFromSchemeActionHandler() {
        super(GetCategoriesFromSchemeAction.class);
    }

    @Override
    public GetCategoriesFromSchemeResult execute(GetCategoriesFromSchemeAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> categories = metamacCoreCommonService.retrieveCategoryScheme(ServiceContextHelper.getServiceContext(), action.getCategorySchemeUri());
        return new GetCategoriesFromSchemeResult(categories);
    }

    @Override
    public void undo(GetCategoriesFromSchemeAction action, GetCategoriesFromSchemeResult result, ExecutionContext context) throws ActionException {

    }

}
