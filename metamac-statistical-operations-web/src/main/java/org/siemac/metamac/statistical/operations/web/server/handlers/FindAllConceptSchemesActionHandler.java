package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.shared.FindAllConceptSchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllConceptSchemesResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class FindAllConceptSchemesActionHandler extends SecurityActionHandler<FindAllConceptSchemesAction, FindAllConceptSchemesResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllConceptSchemesActionHandler() {
        super(FindAllConceptSchemesAction.class);
    }

    @Override
    public FindAllConceptSchemesResult executeSecurityAction(FindAllConceptSchemesAction action) throws ActionException {
        List<ExternalItemDto> conceptSchemes = metamacCoreCommonService.findAllConceptSchemes(ServiceContextHolder.getCurrentServiceContext());
        return new FindAllConceptSchemesResult(conceptSchemes);
    }

    @Override
    public void undo(FindAllConceptSchemesAction action, FindAllConceptSchemesResult result, ExecutionContext context) throws ActionException {

    }

}
