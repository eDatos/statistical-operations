package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllCategorySchemesResult;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class FindAllCategorySchemesActionHandler extends AbstractActionHandler<FindAllCategorySchemesAction, FindAllCategorySchemesResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllCategorySchemesActionHandler() {
        super(FindAllCategorySchemesAction.class);
    }

    @Override
    public FindAllCategorySchemesResult execute(FindAllCategorySchemesAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> categorySchemes = metamacCoreCommonService.findAllCategorySchemes(ServiceContextHolder.getCurrentServiceContext());
        return new FindAllCategorySchemesResult(categorySchemes);
    }

    @Override
    public void undo(FindAllCategorySchemesAction action, FindAllCategorySchemesResult result, ExecutionContext context) throws ActionException {

    }

}
