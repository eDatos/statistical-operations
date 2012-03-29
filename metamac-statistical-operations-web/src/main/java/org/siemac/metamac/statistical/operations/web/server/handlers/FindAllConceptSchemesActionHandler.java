package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.FindAllConceptSchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllConceptSchemesResult;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class FindAllConceptSchemesActionHandler extends AbstractActionHandler<FindAllConceptSchemesAction, FindAllConceptSchemesResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllConceptSchemesActionHandler() {
        super(FindAllConceptSchemesAction.class);
    }

    @Override
    public FindAllConceptSchemesResult execute(FindAllConceptSchemesAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> conceptSchemes = metamacCoreCommonService.findAllConceptSchemes(ServiceContextHelper.getServiceContext());
        return new FindAllConceptSchemesResult(conceptSchemes);
    }

    @Override
    public void undo(FindAllConceptSchemesAction action, FindAllConceptSchemesResult result, ExecutionContext context) throws ActionException {

    }

}
