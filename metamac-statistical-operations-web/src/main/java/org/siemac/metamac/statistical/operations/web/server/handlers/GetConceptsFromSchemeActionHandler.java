package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.GetConceptsFromSchemeAction;
import org.siemac.metamac.statistical.operations.web.shared.GetConceptsFromSchemeResult;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class GetConceptsFromSchemeActionHandler extends AbstractActionHandler<GetConceptsFromSchemeAction, GetConceptsFromSchemeResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public GetConceptsFromSchemeActionHandler() {
        super(GetConceptsFromSchemeAction.class);
    }

    @Override
    public GetConceptsFromSchemeResult execute(GetConceptsFromSchemeAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> concepts = metamacCoreCommonService.retrieveConceptScheme(ServiceContextHolder.getCurrentServiceContext(), action.getConceptSchemeUri());
        return new GetConceptsFromSchemeResult(concepts);
    }

    @Override
    public void undo(GetConceptsFromSchemeAction action, GetConceptsFromSchemeResult result, ExecutionContext context) throws ActionException {

    }

}
