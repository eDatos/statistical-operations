package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyFamilyResult;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.web.common.server.handlers.SecurityActionHandler;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishExternallyFamilyActionHandler extends SecurityActionHandler<PublishExternallyFamilyAction, PublishExternallyFamilyResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishExternallyFamilyActionHandler() {
        super(PublishExternallyFamilyAction.class);
    }

    @Override
    public PublishExternallyFamilyResult executeSecurityAction(PublishExternallyFamilyAction action) throws ActionException {
        try {
            FamilyDto familyDto = statisticalOperationsServiceFacade.publishExternallyFamily(ServiceContextHolder.getCurrentServiceContext(), action.getFamilyId());
            return new PublishExternallyFamilyResult(familyDto);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(PublishExternallyFamilyAction action, PublishExternallyFamilyResult result, ExecutionContext context) throws ActionException {

    }

}
