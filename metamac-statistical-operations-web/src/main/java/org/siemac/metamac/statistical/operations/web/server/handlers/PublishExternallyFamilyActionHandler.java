package org.siemac.metamac.statistical.operations.web.server.handlers;

import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsServiceFacade;
import org.siemac.metamac.statistical.operations.web.server.ServiceContextHelper;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyFamilyAction;
import org.siemac.metamac.statistical.operations.web.shared.PublishExternallyFamilyResult;
import org.siemac.metamac.web.common.server.utils.WebExceptionUtils;
import org.siemac.metamac.web.common.shared.exception.MetamacWebException;
import org.springframework.beans.factory.annotation.Autowired;

import com.gwtplatform.dispatch.server.ExecutionContext;
import com.gwtplatform.dispatch.server.actionhandler.AbstractActionHandler;
import com.gwtplatform.dispatch.shared.ActionException;

public class PublishExternallyFamilyActionHandler extends AbstractActionHandler<PublishExternallyFamilyAction, PublishExternallyFamilyResult> {

    @Autowired
    private StatisticalOperationsServiceFacade statisticalOperationsServiceFacade;

    public PublishExternallyFamilyActionHandler() {
        super(PublishExternallyFamilyAction.class);
    }

    @Override
    public PublishExternallyFamilyResult execute(PublishExternallyFamilyAction action, ExecutionContext context) throws ActionException {
        try {
            FamilyDto familyDto = statisticalOperationsServiceFacade.publishExternallyFamily(ServiceContextHelper.getServiceContext(), action.getFamilyId());
            return new PublishExternallyFamilyResult(familyDto);
        } catch (MetamacException e) {
            throw new MetamacWebException(WebExceptionUtils.getMetamacWebExceptionItem(e.getExceptionItems()));
        }
    }

    @Override
    public void undo(PublishExternallyFamilyAction action, PublishExternallyFamilyResult result, ExecutionContext context) throws ActionException {

    }

}
