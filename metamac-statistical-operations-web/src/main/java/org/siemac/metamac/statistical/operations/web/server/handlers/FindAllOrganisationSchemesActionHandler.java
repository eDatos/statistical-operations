package org.siemac.metamac.statistical.operations.web.server.handlers;

import java.util.List;

import org.siemac.metamac.core.common.dto.serviceapi.ExternalItemBtDto;
import org.siemac.metamac.core.common.serviceapi.MetamacCoreCommonService;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.FindAllOrganisationSchemesAction;
import org.siemac.metamac.statistical.operations.web.shared.FindAllOrganisationSchemesResult;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class FindAllOrganisationSchemesActionHandler extends AbstractActionHandler<FindAllOrganisationSchemesAction, FindAllOrganisationSchemesResult> {

    @Autowired
    private MetamacCoreCommonService metamacCoreCommonService;

    public FindAllOrganisationSchemesActionHandler() {
        super(FindAllOrganisationSchemesAction.class);
    }

    @Override
    public FindAllOrganisationSchemesResult execute(FindAllOrganisationSchemesAction action, ExecutionContext context) throws ActionException {
        List<ExternalItemBtDto> organisationSchemes = metamacCoreCommonService.findAllOrganisationSchemes(ServiceContextHelper.getServiceContext());
        return new FindAllOrganisationSchemesResult(organisationSchemes);
    }

    @Override
    public void undo(FindAllOrganisationSchemesAction action, FindAllOrganisationSchemesResult result, ExecutionContext context) throws ActionException {

    }

}
