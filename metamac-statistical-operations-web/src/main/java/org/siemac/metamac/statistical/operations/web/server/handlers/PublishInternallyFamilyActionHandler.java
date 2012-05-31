package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.domain.statistical.operations.dto.FamilyDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.web.common.server.ServiceContextHolder;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishInternallyFamilyResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

@Component
public class PublishInternallyFamilyActionHandler extends AbstractActionHandler<PublishInternallyFamilyAction, PublishInternallyFamilyResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishInternallyFamilyActionHandler() {
        super(PublishInternallyFamilyAction.class);
    }

    @Override
    public PublishInternallyFamilyResult execute(PublishInternallyFamilyAction action, ExecutionContext context) throws ActionException {
        try {
            FamilyDto familyDto = statisticalOperationsServiceFacade.publishInternallyFamily(ServiceContextHolder.getCurrentServiceContext(), action.getFamilyId());
            return new PublishInternallyFamilyResult(familyDto);
        } catch (MetamacException e) {
            throw WebExceptionUtils.createMetamacWebException(e);
        }
    }

    @Override
    public void undo(PublishInternallyFamilyAction action, PublishInternallyFamilyResult result, ExecutionContext context) throws ActionException {

    }

}
